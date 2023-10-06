
(* this assumes the list length is even *)
let deinterleave list =
  let rec loop acc = function
    | x :: y :: tail -> loop ((x, y) :: acc) tail
    | [] -> List.rev acc
    | _ -> raise (Invalid_argument "List length must be even")
  in
  loop [] list

module Common(IO: S.IO) = struct
  module IO = IO

  let (>>=) = IO.(>>=)

  type redirection = {
    slot: int;
    host: string;
    port: int;
  }

  (* reply from server *)
  type reply = [
    | `Status of string
    | `Error of string
    | `Int of int
    | `Int64 of Int64.t
    | `Bulk of string option
    | `Multibulk of reply list
    | `Ask of redirection
    | `Moved of redirection
  ]

  let rec string_of_reply = function
    | `Status s -> Printf.sprintf "(Status %s)" s
    | `Moved {slot; host; port} -> Printf.sprintf "MOVED %d %s:%i" slot host port
    | `Ask {slot; host; port} -> Printf.sprintf "ASK %d %s:%i" slot host port
    | `Error  s -> Printf.sprintf "(Error %s)" s
    | `Int i -> Printf.sprintf "(Int %i)" i
    | `Int64 i -> Printf.sprintf "(Int64 %Li)" i
    | `Bulk None -> "(Bulk None)"
    | `Bulk (Some s) -> Printf.sprintf "(Bulk (Some %s))" s
    | `Multibulk replies ->
      let x = List.map string_of_reply replies |> String.concat "; " in
      Printf.sprintf "Multibulk [ %s; ]" x

  (* server connection info *)
  type connection_spec = {
    host : string;
    port : int;
  }

  let connection_spec ?(port=6379) host = {host; port}

  module SlotMap = Map.Make(struct
      type t = int
      let compare = Stdlib.compare
    end)

  module ConnectionSpecMap = Map.Make(struct
      type t = connection_spec
      let compare a b =
        let compare_host = compare a.host b.host in
        let compare_port = compare a.port b.port in
        if compare_host = 0 then compare_port else compare_host
    end)

  type cluster_connections = {
    mutable connections_spec : connection_spec SlotMap.t;
    mutable connections : connection ConnectionSpecMap.t;
  }
  and connection = {
    fd      : IO.fd;
    in_ch   : IO.in_channel;
    out_ch  : IO.out_channel;
    stream  : reply list IO.stream;
    cluster : cluster_connections;
  }

  let empty_cluster = {
    connections_spec = SlotMap.empty;
    connections = ConnectionSpecMap.empty;
  }

  (* error responses from server *)
  exception Redis_error of string

  (* these signal protocol errors *)
  exception Unexpected of reply
  exception Unrecognized of string * string (* explanation, data *)

  type bit_operation = AND | OR | XOR | NOT

  module StringBound = struct
    type t = NegInfinity | PosInfinity | Exclusive of string | Inclusive of string

    let to_cmd ~with_bracket = function
      | NegInfinity -> "-"
      | PosInfinity -> "+"
      | Exclusive bound -> "(" ^ bound
      | Inclusive bound -> if with_bracket then "[" ^ bound else bound

    let to_string = to_cmd ~with_bracket:true
  end

  module FloatBound = struct
    type t = NegInfinity | PosInfinity | Exclusive of float | Inclusive of float

    let to_string = function
      | NegInfinity -> "-inf"
      | PosInfinity -> "+inf"
      | Exclusive bound -> String.concat "" ["("; string_of_float bound]
      | Inclusive bound -> String.concat "" [""; string_of_float bound]
  end

  let write out_ch args =
    let num_args = List.length args in
    IO.output_string out_ch (Printf.sprintf "*%d" num_args) >>= fun () ->
    IO.output_string out_ch "\r\n" >>= fun () ->
    IO.iter_serial
      (fun arg ->
         let length = String.length arg in
         IO.output_string out_ch (Printf.sprintf "$%d" length) >>= fun () ->
         IO.output_string out_ch "\r\n" >>= fun () ->
         IO.output_string out_ch arg >>= fun () ->
         IO.output_string out_ch "\r\n"
      )
      args >>= fun () ->
    IO.flush out_ch

  let read_fixed_line length in_ch =
    let line = Bytes.create length in
    IO.really_input in_ch line 0 length >>= fun () ->
    IO.input_char in_ch >>= fun c1 ->
    IO.input_char in_ch >>= fun c2 ->
    let line = Bytes.to_string line in
    match c1, c2 with
    | '\r', '\n' -> IO.return line
    | _          -> IO.fail (Unrecognized ("Expected terminator", line))

  let read_line in_ch =
    let buf = Buffer.create 32 in
    let rec loop () =
      IO.input_char in_ch >>= function
      | '\r' ->
        IO.input_char in_ch >>= (function
            | '\n' ->
              IO.return (Buffer.contents buf)
            | c ->
              Buffer.add_char buf '\r';
              Buffer.add_char buf c;
              loop ()
          )
      | c ->
        Buffer.add_char buf c;
        loop ()
    in
    loop ()

  let classify_error s =
    try
      let command = Utils.String.nsplit s " " in
      match command with
      | "ASK" :: slot :: host :: [] ->
        (match Utils.String.split host ":" with
         | Some (host, port) ->
           let slot = int_of_string slot in
           let port = int_of_string port in
           IO.return (`Ask {slot; host; port})
         | None ->
           IO.return (`Error s)
        )
      | "MOVED" :: slot :: host :: [] ->
        (match Utils.String.split host ":" with
         | Some (host, port) ->
           let slot = int_of_string slot in
           let port = int_of_string port in
           IO.return (`Moved {slot; host; port})
         | None ->
           IO.return (`Error s)
        )
      | _ ->
        IO.return (`Error s)
    with Invalid_argument _ ->
      IO.return (`Error s)

  (* this expects the initial ':' to have already been consumed *)
  let read_integer in_ch =
    read_line in_ch >>= fun line ->
    IO.return
      (try `Int (int_of_string line)
       with _ -> `Int64 (Int64.of_string line))

  (* this expects the initial '$' to have already been consumed *)
  let read_bulk in_ch =
    read_line in_ch >>= fun line ->
    match int_of_string line with
    | -1 -> IO.return (`Bulk None)
    | n when n >= 0 ->
      read_fixed_line n in_ch >>= fun data ->
      IO.return (`Bulk (Some data))
    | n ->
      IO.fail (Unrecognized ("Invalid bulk length", string_of_int n))

  (* this expects the initial '*' to have already been consumed *)
  let rec read_multibulk in_ch =
    let rec loop acc n =
      if n <= 0 then
        IO.return (`Multibulk (List.rev acc))
      else
        read_reply in_ch >>= fun data -> loop (data :: acc) (n - 1)
    in
    read_line in_ch >>= fun line ->
    let num_bulk = int_of_string line in
    loop [] num_bulk

  and read_reply in_ch =
    IO.atomic (fun in_ch ->
        IO.input_char in_ch >>= function
        | '+' ->
          read_line in_ch >>= fun s -> IO.return (`Status s)
        | '-' ->
          read_line in_ch >>= classify_error
        | ':' ->
          read_integer in_ch
        | '$' ->
          read_bulk in_ch
        | '*' ->
          read_multibulk in_ch
        | c ->
          IO.fail (Unrecognized ("Unexpected char in reply", Char.escaped c)))
      in_ch

  let read_reply_exn in_ch =
    read_reply in_ch >>= function
    | `Status _
    | `Int _
    | `Int64 _
    | `Bulk _
    | `Multibulk _ as reply ->
      IO.return reply
    | `Moved {slot; host; port} ->
      let msg = Printf.sprintf "MOVED %d %s:%i" slot host port in
      IO.fail (Redis_error msg)
    | `Ask {slot; host; port} ->
      let msg = Printf.sprintf "ASK %d %s:%i" slot host port in
      IO.fail (Redis_error msg)
    | `Error msg ->
      IO.fail (Redis_error msg)

  let interleave list =
    let rec loop acc = function
      | (x, y) :: tail -> loop (y :: x :: acc ) tail
      | [] -> List.rev acc
    in
    loop [] list

  let return_bulk = function
    | `Bulk b -> IO.return b
    | x       -> IO.fail (Unexpected x)

  let return_no_nil_bulk = function
    | `Bulk (Some b) -> IO.return b
    | x              -> IO.fail (Unexpected x)

  let return_bool = function
    | `Int 0 -> IO.return false
    | `Int 1 -> IO.return true
    | x      -> IO.fail (Unexpected x)

  let return_status = function
    | `Status _ as s -> IO.return s
    | x              -> IO.fail (Unexpected x)

  let return_expected_status expected = function
    | `Status s when expected = s -> IO.return ()
    | x                           -> IO.fail (Unexpected x)

  let return_ok_or_nil = function
    | `Status "OK" -> IO.return true
    | `Bulk None   -> IO.return false
    | x            -> IO.fail (Unexpected x)

  let return_ok_status =
    return_expected_status "OK"

  let return_queued_status =
    return_expected_status "QUEUED"

  let return_int = function
    | `Int n -> IO.return n
    | x      -> IO.fail (Unexpected x)

  let return_float = function
    | `Int n   -> IO.return (float_of_int n)
    | `Int64 n -> IO.return (Int64.to_float n)
    | `Bulk (Some str) -> IO.return (float_of_string str)
    | x        -> IO.fail (Unexpected x)

  let return_int_option = function
    | `Bulk None -> IO.return None
    | `Int n     -> IO.return (Some n)
    | x          -> IO.fail (Unexpected x)

  let return_float_option = function
    | `Bulk None -> IO.return None
    | x -> return_float x >>= fun f -> IO.return (Some f)

  let return_multibulk = function
    | `Multibulk m -> IO.return m
    | x            -> IO.fail (Unexpected x)

  let return_bulk_multibulk reply =
    try
      return_multibulk reply >>= fun list ->
      IO.map
        (function
          | `Bulk b -> IO.return b
          | x -> IO.fail (Unexpected x))
        list
    with e -> IO.fail e

  (* multibulks all of whose entries are not nil *)
  let return_no_nil_multibulk reply =
    return_bulk_multibulk reply >>= fun list ->
    IO.return (Utils.List.filter_map (fun x -> x) list)

  let return_key_value_multibulk reply =
    return_bulk_multibulk reply >>= fun list ->
    try
      IO.return (Utils.List.filter_map (function
          | (Some k, Some v) -> Some (k, v)
          | _ -> None
        ) (deinterleave list))
    with e -> IO.fail e

  let return_opt_pair_multibulk reply =
    return_bulk_multibulk reply >>= function
    | []               -> IO.return None
    | [Some x; Some y] -> IO.return (Some (x, y))
    | _                -> IO.fail (Invalid_argument "Expected nil or two-element multi-bulk")

  let return_info_bulk reply =
    return_bulk reply >>= function
    | Some b ->
      let fields = Utils.String.nsplit b "\r\n" in
      let fields = List.filter (fun x -> x <> "" && not (String.get x 0 = '#')) fields in
      IO.return (Utils.List.filter_map (fun f -> Utils.String.split f ":") fields)
    | None   -> IO.return []

  let connect spec =
    let {host=host; port=port} = spec in
    IO.connect host port >>= fun fd ->
    let in_ch = IO.in_channel_of_descr fd in
    IO.return
      { fd = fd;
        in_ch = in_ch;
        out_ch = IO.out_channel_of_descr fd;
        cluster = empty_cluster;
        stream =
          let f _ =
            read_reply_exn in_ch >>= fun resp ->
            return_multibulk resp >>= fun b ->
            IO.return (Some b) in
          IO.stream_from f;
      }

  let disconnect connection =
    (* both channels are bound to the same file descriptor so we only need
       to close one of them *)
    IO.close connection.fd

  let with_connection spec f =
    connect spec >>= fun c ->
    IO.catch
      (fun () ->
         f c >>= fun r ->
         disconnect c >>= fun () ->
         IO.return r)
      (fun e ->
         disconnect c >>= fun () ->
         IO.fail e)

  let send_request connection command =
    write connection.out_ch command >>= fun () ->
    read_reply_exn connection.in_ch >>= function
    | `Status _
    | `Int _
    | `Int64 _
    | `Bulk _
    | `Multibulk _ as reply ->
      IO.return reply

  (* generate command for SORT *)
  let sort_command
      ?by
      ?limit (* offset, limit *)
      ?get:(_=[])
      ?order
      ?(alpha=false)
      ?store
      key =
    let command = ref [ key; "SORT" ] in (* we'll reverse this later *)
    let append x = command := x :: !command in
    (match by with
     | Some by ->
       append "BY";
       append by
     | None ->
       ()
    );
    (match limit with
     | Some (offset, limit) ->
       append "LIMIT";
       append (string_of_int offset);
       append (string_of_int limit);
     | None ->
       ()
    );
    (match order with
     | Some `Asc -> append "ASC"
     | Some `Desc -> append "DESC"
     | None -> ()
    );
    if alpha then append "ALPHA";
    (match store with
     | Some dest ->
       append "STORE";
       append dest
     | None ->
       ()
    );
    List.rev !command

  let stream connection = connection.stream

  module CommonMassInsert = struct
    type command = string list

    let empty = []

    type action =
      | Ask of connection * command
      | Reply of reply
      | Moved of connection * command
      | Command of connection * command

    let send_request connection command =
      write connection.out_ch command
      >>= fun () ->
      IO.return connection
  end
end


module type Mode = sig

  module IO : S.IO

  val ( >>= ) : 'a IO.t -> ('a -> 'b IO.t) -> 'b IO.t

  type redirection = {
    slot : int;
    host : string;
    port : int;
  }

  type reply = [
    | `Ask of redirection
    | `Bulk of string option
    | `Error of string
    | `Int of int
    | `Int64 of Int64.t
    | `Moved of redirection
    | `Multibulk of reply list
    | `Status of string
  ]

  val string_of_reply : reply -> string
  (** For debugging purpose *)

  type connection_spec = {
    host : string;
    port : int;
  }

  val connection_spec : ?port:int -> string -> connection_spec

  module SlotMap : Map.S with type key = int
  module ConnectionSpecMap : Map.S with type key = connection_spec

  type cluster_connections = {
    mutable connections_spec : connection_spec SlotMap.t;
    mutable connections : connection ConnectionSpecMap.t;
  }
  and connection = {
    fd      : IO.fd;
    in_ch   : IO.in_channel;
    out_ch  : IO.out_channel;
    stream  : reply list IO.stream;
    cluster : cluster_connections;
  }

  exception Redis_error of string
  exception Unexpected of reply
  exception Unrecognized of string * string

  type bit_operation =  AND | OR | XOR | NOT

  module StringBound : sig
    type t =
      | NegInfinity
      | PosInfinity
      | Exclusive of string
      | Inclusive of string

    val to_string : t -> string
    val to_cmd : with_bracket:bool -> t -> string
  end

  module FloatBound : sig
    type t =
      | NegInfinity
      | PosInfinity
      | Exclusive of float
      | Inclusive of float

    val to_string : t -> string
  end

  val write : IO.out_channel -> string list -> unit IO.t
  val interleave : ('a * 'a) list -> 'a list
  val return_bulk : reply -> string option IO.t
  val return_no_nil_bulk : reply -> string IO.t
  val return_bool : reply -> bool IO.t
  val return_status : reply -> [> `Status of string ] IO.t
  val return_ok_or_nil : reply -> bool IO.t
  val return_ok_status : reply -> unit IO.t
  val return_queued_status : reply -> unit IO.t
  val return_int : reply -> int IO.t
  val return_float : reply -> float IO.t
  val return_int_option : reply -> int option IO.t
  val return_float_option : reply -> float option IO.t
  val return_multibulk : reply -> reply list IO.t
  val return_bulk_multibulk : reply -> string option list IO.t
  val return_no_nil_multibulk : reply -> string list IO.t
  val return_key_value_multibulk : reply -> (string * string) list IO.t
  val return_opt_pair_multibulk : reply -> (string * string) option IO.t
  val return_info_bulk : reply -> (string * string) list IO.t
  val connect : connection_spec -> connection IO.t
  val disconnect : connection -> unit IO.t
  val with_connection :
    connection_spec -> (connection -> 'a IO.t) -> 'a IO.t
  val sort_command :
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] ->
    ?alpha:bool -> ?store:string -> string -> string list
  val stream : connection -> reply list IO.stream

  val read_reply_exn :
    IO.in_channel ->
    [> `Ask of redirection
    | `Bulk of string option
    | `Int of int
    | `Int64 of int64
    | `Moved of redirection
    | `Multibulk of
         [ `Ask of redirection
         | `Bulk of string option
         | `Error of string
         | `Int of int
         | `Int64 of int64
         | `Moved of redirection
         | `Multibulk of 'a
         | `Status of string ] list as 'a
    | `Status of string
    ] IO.t

  val send_request :
    connection ->
    string list ->
    [> `Bulk of string option
    | `Int of int
    | `Int64 of int64
    | `Multibulk of
         [ `Ask of redirection
         | `Bulk of string option
         | `Error of string
         | `Int of int
         | `Int64 of int64
         | `Moved of redirection
         | `Multibulk of 'a
         | `Status of string ] list as 'a
    | `Status of string
    ] IO.t

  module ModeMassInsert : sig
    type command = string list

    val empty : 'a list

    type action =
      | Ask of connection * command
      | Reply of reply
      | Moved of connection * command
      | Command of connection * command

    val send_request : connection -> string list -> connection IO.t
  end
end

module SimpleMode(IO : S.IO) = struct
  include Common(IO)

  module ModeMassInsert = struct
    include CommonMassInsert
  end
end

(** The redis cluster mode is only available with redis >= 3.0.

    In cluster mode, redirection given by the server as [MOVED] or
    [ASK] will be followed.
*)
module ClusterMode(IO : S.IO) = struct
  include Common(IO)

  let tag_re = Re.Str.regexp {|[^{]*{\([^}]+\)}.*|}

  let get_tag s =
    if Re.Str.string_match tag_re s 0 then
      Re.Str.matched_group 1 s
    else
      s

  let get_connection connection slot =
    try
      let spec = SlotMap.find slot connection.cluster.connections_spec in
      let connection = ConnectionSpecMap.find spec connection.cluster.connections in
      Some (connection)
    with Not_found ->
      None

  let get_slot key =
    let tag = get_tag key in
    Crc16.crc16 tag mod 16384

  let get_connection_for_key connection key =
    let slot = get_slot key in
    get_connection connection slot

  let read_reply_exn in_ch =
    read_reply in_ch >>= function
    | `Moved _
    | `Ask _
    | `Status _
    | `Int _
    | `Int64 _
    | `Bulk _
    | `Multibulk _ as reply ->
      IO.return reply
    | `Error msg ->
      IO.fail (Redis_error msg)

  let send_request' main_connection command =
    let rec loop connection =
      write connection.out_ch command >>= fun () ->
      read_reply_exn connection.in_ch >>= function
      | `Ask {slot=_; host; port} ->
        connect {host; port} >>= fun connection_moved ->
        let res = loop connection_moved in
        disconnect connection_moved >>= fun () ->
        res
      | `Moved {slot; host; port} ->
        begin
          try
            IO.return (ConnectionSpecMap.find {host; port} main_connection.cluster.connections)
          with Not_found ->
            connect {host; port} >>= fun connection_moved ->
            main_connection.cluster.connections <- ConnectionSpecMap.add {host; port} connection_moved main_connection.cluster.connections;
            IO.return connection_moved
        end
        >>= fun connection_moved ->
        main_connection.cluster.connections_spec <- SlotMap.add slot {host; port} main_connection.cluster.connections_spec;
        loop connection_moved
      | `Status _
      | `Int _
      | `Int64 _
      | `Bulk _
      | `Multibulk _ as reply ->
        IO.return reply
    in
    loop main_connection

  let send_request connection command =
    let key =
      match command with
      | _command_name :: key :: _ -> Some key
      | "info" :: _
      | "multi" :: _
      | "exec" :: _
      | "slaveof" :: _
      | "config" :: _
      | "shutdown" :: _
      | _ ->
        None
    in
    let connection =
      match key with
      | None -> connection
      | Some key ->
        match get_connection_for_key connection key with
        | None -> connection
        | Some connection -> connection
    in
    send_request' connection command

  let disconnect connection =
    let connection_list = ConnectionSpecMap.bindings connection.cluster.connections in
    connection.cluster.connections <- ConnectionSpecMap.empty;
    IO.iter (fun (_cinfo, connection) ->
        (* Printf.printf "disconnecting %s:%d\n%!" _cinfo.host _cinfo.port; *)
        disconnect connection
      ) connection_list
    >>= fun () -> disconnect connection

  module ModeMassInsert = struct
    include CommonMassInsert

    let send_request connection command =
      let key =
        match command with
        | _command_name :: key :: _ -> Some key
        | "info" :: _
        | "multi" :: _
        | "exec" :: _
        | "slaveof" :: _
        | "config" :: _
        | "shutdown" :: _
        | _ ->
          None
      in
      let connection =
        match key with
        | None -> connection
        | Some key ->
          match get_connection_for_key connection key with
          | None -> connection
          | Some connection -> connection
      in
      write connection.out_ch command
      >>= fun () ->
      IO.return connection
  end
end

(** Bindings for redis.

    This has only been tested with Redis 2.2, but will probably work for >= 2.0
 **)
module MakeClient(Mode: Mode) = struct
  include Mode

  (* Raises Error if password is invalid. *)
  let auth connection password =
    let command = [ "AUTH"; password ] in
    send_request connection command >>= return_ok_status

  let auth_acl connection username password =
    let command = [ "AUTH"; username; password ] in
    send_request connection command >>= return_ok_status

  let echo connection message =
    let command = [ "ECHO"; message ] in
    send_request connection command >>= return_bulk

  let ping connection =
    let command = [ "PING" ] in
    IO.try_bind
      (fun () -> send_request connection command)
      (function `Status "PONG" -> IO.return true
              | _ -> IO.return false)

      (fun e -> IO.fail e)

  let quit connection =
    let command = [ "QUIT" ] in
    send_request connection command >>= return_ok_status

  (* Switch to a different db; raises Error if index is invalid. *)
  let select connection index =
    let index = string_of_int index in
    let command = [ "SELECT"; index ] in
    send_request connection command >>= return_ok_status

  let send_custom_request = send_request

  (** SENTINEL commands *)
  let sentinel_masters connection =
    let command = [ "SENTINEL"; "masters"] in
    send_request connection command
    >>= return_multibulk
    >>= IO.map_serial return_key_value_multibulk

  let sentinel_get_master_addr_by_name connection name =
    let command = [ "SENTINEL"; "GET-MASTER-ADDR-BY-NAME"; name] in
    send_request connection command >>= return_opt_pair_multibulk

  (** Generic key commands *)

  (* Returns the number of keys removed. *)
  let del connection keys =
    let command = "DEL" :: keys in
    send_request connection command >>= return_int

  let exists connection key =
    let command = [ "EXISTS"; key ] in
    send_request connection command >>= return_bool

  (* Returns true if timeout (in seconds) was set, false otherwise. *)
  let expire connection key seconds =
    let seconds = string_of_int seconds in
    let command = [ "EXPIRE"; key; seconds ] in
    send_request connection command >>= return_bool

  (* Returns true if timeout (in milliseconds) was set, false otherwise. *)
  let pexpire connection key milliseconds =
    let milliseconds = string_of_int milliseconds in
    let command = [ "PEXPIRE"; key; milliseconds ] in
    send_request connection command >>= return_bool

  (* Like "expire" but with absolute (Unix) time; the time is truncated to the nearest second. *)
  let expireat connection key unix_time =
    let unix_time = Printf.sprintf "%.0f" unix_time in
    let command = [ "EXPIREAT"; key; unix_time ] in
    send_request connection command >>= return_bool

  (* Like "pexpire" but with absolute (Unix) time in milliseconds. *)
  let pexpireat connection key unix_time_ms =
    let unix_time_ms = Printf.sprintf "%d" unix_time_ms in
    let command = [ "PEXPIREAT"; key; unix_time_ms ] in
    send_request connection command >>= return_bool

  (* Probably not a good idea to use this in production; see Redis documentation. *)
  let keys connection pattern =
    let command = [ "KEYS"; pattern ] in
    send_request connection command >>= return_no_nil_multibulk

  (* Cursor based iteration through all keys in database. *)
  let scan ?(pattern="*") ?(count=10) connection cursor =
    let cursor = string_of_int cursor in
    let count = string_of_int count in
    let command = ["SCAN"; cursor; "MATCH"; pattern; "COUNT"; count] in
    send_request connection command >>= return_multibulk >>=
    function
    | `Bulk Some next_cursor :: `Multibulk keys :: [] ->
      let next_cursor = int_of_string next_cursor in
      IO.map_serial (function
          | `Bulk (Some s) -> IO.return s
          | x -> IO.fail (Unexpected x) >>= fun () -> IO.return "") keys
      >>= fun keys ->
      IO.return (next_cursor, keys)
    | _ -> IO.fail (Redis_error "SCAN returned unexpected result")

  (* Move key to a different db; returns true if key was moved, false otherwise. *)
  let move connection key index =
    let index = string_of_int index in
    let command = [ "MOVE"; key; index ] in
    send_request connection command >>= return_bool

  (* Remove timeout on key; returns true if timeout was removed, false otherwise. *)
  let persist connection key =
    let command = [ "PERSIST"; key ] in
    send_request connection command >>= return_bool

  (* returns none if db is empty. *)
  let randomkey connection =
    let command = [ "randomkey" ] in
    send_request connection command >>= return_bulk

  (* Raises Error if key doesn't exist. *)
  let rename connection key newkey =
    let command = [ "RENAME"; key; newkey ] in
    send_request connection command >>= return_ok_status

  (* Raises Error if key doesn't exist; returns true if key was renamed, false if newkey already exists. *)
  let renamenx connection key newkey =
    let command = [ "RENAMENX"; key; newkey ] in
    send_request connection command >>= return_bool

  let sort
      connection
      ?by
      ?limit (* offset, limit *)
      ?get
      ?order
      ?alpha
      key =
    let command =
      sort_command
        ?by
        ?limit
        ?get
        ?order
        ?alpha
        key
    in
    send_request connection command >>= return_no_nil_multibulk

  let sort_and_store
      connection
      ?by
      ?limit (* offset, limit *)
      ?get
      ?order
      ?alpha
      key
      destination =
    let command =
      sort_command
        ?by
        ?limit
        ?get
        ?order
        ?alpha
        ~store:destination
        key
    in
    send_request connection command >>= return_int

  (* Returns None if key doesn't exist or doesn't have a timeout.
     Otherwise function returns Some seconds. *)
  let ttl connection key =
    let command = [ "TTL"; key ] in
    send_request connection command >>= return_int
    >>= function
    | -1 -> IO.return None
    | t  -> IO.return (Some t)

  (* Returns None if key doesn't exist or doesn't have a timeout.
     Otherwise function returns Some milliseconds. *)
  let pttl connection key =
    let command = [ "PTTL"; key ] in
    send_request connection command >>= return_int
    >>= function
    | -1 -> IO.return None
    | t  -> IO.return (Some t)

  (* TYPE is a reserved word in ocaml *)
  let type_of connection key =
    let command = [ "TYPE"; key ] in
    send_request connection command >>= return_status >>= function
    | `Status "string" -> IO.return `String
    | `Status "list"   -> IO.return `List
    | `Status "zset"   -> IO.return `Zset
    | `Status "hash"   -> IO.return `Hash
    | `Status "none"   -> IO.return `None (* key doesn't exist *)
    | `Status x        -> IO.fail (Unrecognized ("Unexpected TYPE result", x))
    | x                -> IO.fail (Unexpected x)

  let dump connection key =
    let command = ["DUMP"; key] in
    send_request connection command >>= return_bulk

  let restore connection key ttl serialized_value =
    let ttl = string_of_int ttl in
    let command = ["RESTORE"; key; ttl; serialized_value] in
    send_request connection command >>= return_ok_status

  let migrate connection
      ?(copy=false) ?(replace=false)
      host port key destination_db timeout =
    let port = string_of_int port in
    let destination_db = string_of_int destination_db in
    let timeout = string_of_int timeout in
    let copy = match copy with
      | true -> "COPY"
      | false -> "" in
    let replace = match replace with
      | true -> "REPLACE"
      | false -> "" in
    let base_command = ["MIGRATE"; host; port; key; destination_db; timeout] in
    let args = List.filter (fun x -> String.length x > 0) [copy; replace] in
    let command = List.concat [base_command; args] in
    send_request connection command >>= return_ok_status

  let object_refcount connection key =
    let command = ["OBJECT"; "REFCOUNT"; key] in
    send_request connection command >>= function
    | `Int x -> IO.return (Some x)
    | _ -> IO.return None

  let object_encoding connection key =
    let command = ["OBJECT"; "ENCODING"; key] in
    send_request connection command >>= function
    | `Bulk x -> IO.return x
    | _ -> IO.return None

  let object_idletime connection key =
    let command = ["OBJECT"; "IDLETIME"; key] in
    send_request connection command >>= function
    | `Int x -> IO.return (Some x)
    | _ -> IO.return None

  (** String commands *)

  (* Returns length of string after append. *)
  let append connection key value =
    let command = [ "APPEND"; key; value ] in
    send_request connection command >>= return_int

  let decr connection key =
    let command = [ "DECR"; key ] in
    send_request connection command >>= return_int

  let decrby connection key decrement =
    let decrement = string_of_int decrement in
    let command = [ "DECRBY"; key; decrement ] in
    send_request connection command >>= return_int

  let get connection key =
    let command = [ "GET"; key ] in
    send_request connection command >>= return_bulk

  (* Out of range arguments are handled by limiting to valid range. *)
  let getrange connection key start stop =
    let start = string_of_int start in
    let stop = string_of_int stop in
    let command = [ "GETRANGE"; key; start; stop ] in
    send_request connection command >>= return_bulk

  (* Set value and return old value. Raises Error when key exists but isn't a string. *)
  let getset connection key value =
    let command = [ "GETSET"; key; value ] in
    send_request connection command >>= return_bulk

  let incr connection key =
    let command = [ "INCR"; key ] in
    send_request connection command >>= return_int

  let incrby connection key increment =
    let increment = string_of_int increment in
    let command = [ "INCRBY"; key; increment ] in
    send_request connection command >>= return_int

  let incrbyfloat connection key increment =
    let increment = string_of_float increment in
    let command = [ "INCRBYFLOAT"; key; increment ] in
    send_request connection command >>= return_float

  let mget connection keys =
    let command = "MGET" :: keys in
    send_request connection command >>= return_bulk_multibulk

  (* This is atomic: either all keys are set or none are. *)
  let mset connection items =
    let command = "MSET" :: (interleave items) in
    send_request connection command >>= return_ok_status

  (* Like MSET, this is atomic. If even a single key exists, no operations will be performed.
     Returns true if all keys were set, false otherwise. *)
  let msetnx connection items =
    let command = "MSETNX" :: (interleave items) in
    send_request connection command >>= return_bool

  let set connection ?ex:(ex=0) ?px:(px=0) ?nx:(nx=false) ?xx:(xx=false) key value =
    match (nx, xx) with
    | (true, true) ->
      IO.fail (Invalid_argument "SET command can contain only one of NX or XX options.")
    | _ ->
      let ex = match ex with
        | 0 -> []
        | _ -> ["EX"; string_of_int ex] in
      let px = match px with
        | 0 -> []
        | _ -> ["PX"; string_of_int px] in
      let nx = match nx with
        | false -> []
        | true -> ["NX"] in
      let xx = match xx with
        | false -> []
        | true -> ["XX"] in
      let base_command = [ "SET"; key; value; ] in
      let args = List.concat [ex; px; nx; xx] in
      let command = List.concat [base_command; args] in
      send_request connection command >>= return_ok_or_nil

  let setex connection key seconds value =
    let seconds = string_of_int seconds in
    let command = [ "SETEX"; key; seconds; value ] in
    send_request connection command >>= return_ok_status

  let psetex connection key milliseconds value =
    let milliseconds = string_of_int milliseconds in
    let command = [ "PSETEX"; key; milliseconds; value ] in
    send_request connection command >>= return_ok_status

  (* Returns true if key was set, false otherwise. *)
  let setnx connection key value =
    let command = [ "SETNX"; key; value ] in
    send_request connection command >>= return_bool

  (* If offset > length, string will be padded with 0-bytes. Returns length of string after modification. *)
  let setrange connection key offset value =
    let offset = string_of_int offset in
    let command = [ "SETRANGE"; key; offset; value ] in
    send_request connection command >>= return_int

  let strlen connection key =
    let command = [ "STRLEN"; key ] in
    send_request connection command >>= return_int

  (** Bitwise commands *)

  let setbit connection key offset value =
    let offset = string_of_int offset in
    let value = string_of_int value in
    let command = [ "SETBIT"; key; offset; value ] in
    send_request connection command >>= return_int

  let getbit connection key offset =
    let offset = string_of_int offset in
    let command = [ "GETBIT"; key; offset ] in
    send_request connection command >>= return_int

  let bitop connection op dest args =
    let op = (match op with
        | NOT -> "NOT"
        | AND -> "AND"
        | OR -> "OR"
        | XOR -> "XOR") in
    let command = List.concat [["BITOP"; op; dest]; args] in
    send_request connection command >>= return_int

  let bitcount ?(first=0) ?(last=(- 1)) connection key =
    let first = string_of_int first in
    let last = string_of_int last in
    let command = ["BITCOUNT"; key; first; last] in
    send_request connection command >>= return_int

  let bitpos ?(first=0) ?(last=(- 1)) connection key bit =
    let bit = string_of_int bit in
    let first = string_of_int first in
    let last = string_of_int last in
    let command = ["BITPOS"; key; bit; first; last] in
    send_request connection command >>= return_int

  (** Hash commands *)

  let hdel connection key field =
    let command = [ "HDEL"; key; field ] in
    send_request connection command >>= return_bool

  let hexists connection key field =
    let command = [ "HEXISTS"; key; field ] in
    send_request connection command >>= return_bool

  let hget connection key field =
    let command = [ "HGET"; key; field ] in
    send_request connection command >>= return_bulk

  let hgetall connection key =
    let command = [ "HGETALL"; key ] in
    send_request connection command >>= return_key_value_multibulk

  let hincrby connection key field increment =
    let increment = string_of_int increment in
    let command = [ "HINCRBY"; key; field; increment ] in
    send_request connection command >>= return_int

  let hincrbyfloat connection key field increment =
    let increment = string_of_float increment in
    let command = [ "HINCRBYFLOAT"; key; field; increment ] in
    send_request connection command >>= return_float

  let hkeys connection key =
    let command = [ "HKEYS"; key ] in
    send_request connection command >>= return_no_nil_multibulk

  let hlen connection key =
    let command = [ "HLEN"; key ] in
    send_request connection command >>= return_int

  let hmget connection key fields =
    let command = "HMGET" :: key :: fields in
    send_request connection command >>= return_bulk_multibulk

  let hmset connection key items =
    let command = "HMSET" :: key :: (interleave items) in
    send_request connection command >>= return_ok_status

  let hset connection key field value =
    let command = [ "HSET"; key; field; value ] in
    send_request connection command >>= return_bool

  let hsetnx connection key field value =
    let command = [ "HSETNX"; key; field; value ] in
    send_request connection command >>= return_bool

  let hstrlen connection key field =
    let command = [ "HSTRLEN"; key; field ] in
    send_request connection command >>= return_int

  let hscan ?(pattern="*") ?(count=10) connection key cursor =
    let cursor = string_of_int cursor in
    let count = string_of_int count in
    let command = ["HSCAN"; key; cursor; "MATCH"; pattern; "COUNT"; count] in
    send_request connection command >>= return_multibulk >>=
    function
    | `Bulk Some next_cursor :: `Multibulk keys :: [] ->
      let next_cursor = int_of_string next_cursor in
      IO.map_serial (function
          | `Bulk (Some s) -> IO.return s
          | x -> IO.fail (Unexpected x) >>= fun () -> IO.return "") keys
      >>= fun entries ->
      let pairs = Utils.List.pairs_of_list entries |> Utils.Option.default [] in
      IO.return (next_cursor, pairs)
    | _ -> IO.fail (Redis_error "HSCAN returned unexpected result")

  let hvals connection key =
    let command = [ "HVALS"; key ] in
    send_request connection command >>= return_no_nil_multibulk

  (** List commands *)

  (* Blocks while all of the lists are empty. Set timeout to number of seconds OR 0 to block indefinitely. *)
  let blpop connection keys timeout =
    let timeout = string_of_int timeout in
    let command = "BLPOP" :: (keys @ [timeout]) in
    send_request connection command >>= return_opt_pair_multibulk

  (* Same as BLPOP except pulling the last instead of first element. *)
  let brpop connection keys timeout =
    let timeout = string_of_int timeout in
    let command = "BRPOP" :: (keys @ [timeout]) in
    send_request connection command >>= return_opt_pair_multibulk

  (* Blocking RPOPLPUSH.  Returns None on timeout. *)
  let brpoplpush connection source destination timeout =
    let timeout = string_of_int timeout in
    let command = [ "BRPOPLPUSH"; source; destination; timeout ] in
    send_request connection command >>= function
    | `Multibulk []        -> IO.return None
    | `Bulk (Some element) -> IO.return (Some element)
    | x                    -> IO.fail (Unexpected x)

  (* Out of range or nonexistent key will return None. *)
  let lindex connection key index =
    let index = string_of_int index in
    let command = [ "LINDEX"; key; index ] in
    send_request connection command >>= return_bulk

  (* Returns None if pivot isn't found, otherwise returns length of list after insert. *)
  let linsert connection key where pivot value =
    let where =
      match where with
      | `Before -> "BEFORE"
      | `After -> "AFTER"
    in
    let command = [ "LINSERT"; key; where; pivot; value ] in
    send_request connection command >>= return_int
    >>= function
    | -1 -> IO.return None
    | n  -> IO.return (Some n)

  let llen connection key =
    let command = [ "LLEN"; key ] in
    send_request connection command >>= return_int

  let lpop connection key =
    let command = [ "LPOP"; key ] in
    send_request connection command >>= return_bulk

  (* Returns length of list after operation. *)
  let lpush connection key values =
    let command = "LPUSH" :: key :: values in
    send_request connection command >>= return_int

  (* Only push when list exists. Return length of list after operation. *)
  let lpushx connection key values =
    let command = "LPUSHX" :: key :: values in
    send_request connection command >>= return_int

  (* Out of range arguments are handled by limiting to valid range. *)
  let lrange connection key start stop =
    let start = string_of_int start in
    let stop = string_of_int stop in
    let command = [ "LRANGE"; key; start; stop ] in
    send_request connection command >>= return_no_nil_multibulk

  (* Returns number of elements removed. *)
  let lrem connection key count value =
    let count = string_of_int count in
    let command = [ "LREM"; key; count; value ] in
    send_request connection command >>= return_int

  (* Raises Error if out of range. *)
  let lset connection key index value =
    let index = string_of_int index in
    let command = [ "LSET"; key; index; value ] in
    send_request connection command >>= return_ok_status

  (* Removes all but the specified range. Out of range arguments are handled by limiting to valid range. *)
  let ltrim connection key start stop =
    let start = string_of_int start in
    let stop = string_of_int stop in
    let command = [ "LTRIM"; key; start; stop ] in
    send_request connection command >>= return_ok_status

  let rpop connection key =
    let command = [ "RPOP"; key ] in
    send_request connection command >>= return_bulk

  (* Remove last element of source and insert as first element of destination. Returns the element moved
     or None if source is empty. *)
  let rpoplpush connection source destination =
    let command = [ "RPOPLPUSH"; source; destination ] in
    send_request connection command >>= return_bulk

  (* Returns length of list after operation. *)
  let rpush connection key values =
    let command = "RPUSH" :: key :: values in
    send_request connection command >>= return_int

  let rpushx connection key values =
    let command = "RPUSHX" :: key :: values in
    send_request connection command >>= return_int

  let conv_side_ = function
    | `Left -> "LEFT"
    | `Right -> "RIGHT"

  let lmove connection a b sidea sideb =
    let request = ["LMOVE"; a; b; conv_side_ sidea; conv_side_ sideb] in
    send_request connection request >>= return_bulk

  let blmove connection a b sidea sideb ~timeout =
    let request = [
      "BLMOVE"; a; b; conv_side_ sidea; conv_side_ sideb;
      string_of_int timeout] in
    send_request connection request >>= return_bulk

  (** HyperLogLog commands *)

  let pfadd connection key values =
    let command = [ "PFADD"; key ] @ values in
    send_request connection command >>= return_bool

  let pfcount connection keys =
    let command = [ "PFCOUNT" ] @ keys in
    send_request connection command >>= return_int

  let pfmerge connection keys =
    let command = [ "PFMERGE" ] @ keys in
    send_request connection command >>= return_ok_status

  (** Set commands *)

  (* Returns true if member was added, false otherwise. *)
  let sadd connection key member =
    let command = [ "SADD"; key; member ] in
    send_request connection command >>= return_bool

  let scard connection key =
    let command = [ "SCARD"; key ] in
    send_request connection command >>= return_int

  (* Difference between first and all successive sets. *)
  let sdiff connection keys =
    let command = "SDIFF" :: keys in
    send_request connection command >>= return_no_nil_multibulk

  (* like sdiff, but store result in destination. returns size of result. *)
  let sdiffstore connection destination keys =
    let command = "sdiffstore" :: destination :: keys in
    send_request connection command >>= return_int

  let sinter connection keys =
    let command = "SINTER" :: keys in
    send_request connection command >>= return_no_nil_multibulk

  (* Like SINTER, but store result in destination. Returns size of result. *)
  let sinterstore connection destination keys =
    let command = "SINTERSTORE" :: destination :: keys in
    send_request connection command >>= return_int

  let sismember connection key member =
    let command = [ "SISMEMBER"; key; member ] in
    send_request connection command >>= return_bool

  let smembers connection key =
    let command = [ "SMEMBERS"; key ] in
    send_request connection command >>= return_no_nil_multibulk

  (* Returns true if an element was moved, false otherwise. *)
  let smove connection source destination member =
    let command = [ "SMOVE"; source; destination; member ] in
    send_request connection command >>= return_bool

  (* Remove random element from set. *)
  let spop connection key =
    let command = [ "SPOP"; key ] in
    send_request connection command >>= return_bulk

  (* Like SPOP, but doesn't remove chosen element. *)
  let srandmember connection key =
    let command = [ "SRANDMEMBER"; key ] in
    send_request connection command >>= return_bulk

  (* Returns true if element was removed. *)
  let srem connection key member =
    let command = [ "SREM"; key; member ] in
    send_request connection command >>= return_bool

  let sunion connection keys =
    let command = "SUNION" :: keys in
    send_request connection command >>= return_no_nil_multibulk

  (* Like SUNION, but store result in destination. Returns size of result. *)
  let sunionstore connection destination keys =
    let command = "SUNIONSTORE" :: destination :: keys in
    send_request connection command >>= return_int


  (** Sorted set commands *)


  (** Pub/sub commands *)

  (* Post a message to a channel. Returns number of clients that received the message. *)
  let publish connection channel message =
    let command = [ "PUBLISH" ; channel ; message ] in
    send_request connection command >>= return_int

  (* Lists the currently active channels. If no pattern is specified, all channels are listed. *)
  let pubsub_channels connection channels =
    let message = Utils.Option.default "*" channels in
    let command = ["PUBSUB"; "CHANNELS"; message ] in
    send_request connection command >>= return_multibulk

  (* Returns the number of subscribers (not counting clients subscribed to patterns) for the specified channels. *)
  let pubsub_numsub connection channels =
    let command = "PUBSUB" :: "NUMSUB":: channels in
    send_request connection command >>= return_multibulk

  (* Subscribes the client to the specified channels. *)
  let subscribe connection channels =
    let command = "SUBSCRIBE" :: channels in
    write connection.out_ch command >>= fun () -> IO.return ()

  (* Unsubscribes the client from the given channels, or from all of them if an empty list is given *)
  let unsubscribe connection channels =
    let command = "UNSUBSCRIBE" :: channels in
    write connection.out_ch command

  (* Subscribes the client to the given patterns. *)
  let psubscribe connection patterns =
    let command = "PSUBSCRIBE" :: patterns in
    write connection.out_ch command >>= fun () -> IO.return ()

  (* Unsubscribes the client from the given patterns. *)
  let punsubscribe connection patterns =
    let command = "PUNSUBSCRIBE" :: patterns in
    write connection.out_ch command

  (** Sorted Set commands *)

  (* Add one or more members to a sorted set, or update its score if it already exists. *)
  let zadd connection ?x ?(ch = false) key values =
    let f acc (s, v) = (string_of_float s) :: v :: acc in
    let values = List.fold_left f [] values in
    let command =
      let cmd =
        if ch
        then "CH" :: values
        else values
      in
      let cmd =
        match x with
        | None -> cmd
        | Some x ->
          let flag =
            match x with
            | `XX -> "XX"
            | `NX -> "NX"
          in
          flag :: cmd
      in
      "ZADD" :: key :: cmd
    in
    send_request connection command >>= return_int

  let zincrby connection key score member =
    let command = [ "ZINCRBY"; key; string_of_float score; member ] in
    send_request connection command >>= return_float

  (* Returns the score of member in the sorted set. *)
  let zscore connection key member =
    let command = "ZSCORE" :: key :: member :: [] in
    send_request connection command >>= return_float_option

  (* Return a range of members in a sorted set, by index. *)
  let zrange connection ?(withscores=false) key start stop =
    let istart = string_of_int start in
    let istop = string_of_int stop in
    let scores = if withscores then ["withscores"] else [] in
    let command = "ZRANGE" :: key :: istart :: istop :: scores in
    send_request connection command >>= return_multibulk

  (* Return a range of members in a sorted set, by index. *)
  let zrevrange connection ?(withscores=false) key start stop =
    let istart = string_of_int start in
    let istop = string_of_int stop in
    let scores = if withscores then ["withscores"] else [] in
    let command = "ZREVRANGE" :: key :: istart :: istop :: scores in
    send_request connection command >>= return_multibulk

  (* Return a range of members in a sorted set, by score. *)
  let zrangebyscore connection ?(withscores=false) ?limit key min_bound max_bound =
    let min = FloatBound.to_string min_bound in
    let max = FloatBound.to_string max_bound in
    let limit = match limit with
      | None -> []
      | Some (offset, count) -> [ "LIMIT";
                                  string_of_int offset;
                                  string_of_int count; ]
    in
    let scores = if withscores then "WITHSCORES" :: limit else limit in
    let command = "ZRANGEBYSCORE" :: key :: min :: max :: scores in
    send_request connection command >>= return_multibulk

  (* Return a range of members in a sorted set, by lexicographical range. *)
  let zrangebylex connection ?limit key min_bound max_bound =
    let min = StringBound.to_cmd ~with_bracket:true min_bound in
    let max = StringBound.to_cmd ~with_bracket:true max_bound in
    let limit = match limit with
      | None -> []
      | Some (offset, count) ->
        [ "LIMIT"; string_of_int offset; string_of_int count; ]
    in
    let command = "ZRANGEBYLEX" :: key :: min :: max :: limit in
    send_request connection command >>= return_multibulk

  (* Return a range of members in a sorted set, by score. *)
  let zrevrangebyscore connection ?(withscores=false) ?limit key min_bound max_bound =
    let min = FloatBound.to_string min_bound in
    let max = FloatBound.to_string max_bound in
    let limit = match limit with
      | None -> []
      | Some (offset, count) -> [ "LIMIT";
                                  string_of_int offset;
                                  string_of_int count; ]
    in
    let scores = if withscores then "WITHSCORES" :: limit else limit in
    let command = "ZREVRANGEBYSCORE" :: key :: min :: max :: scores in
    send_request connection command >>= return_multibulk

  (* Return a range of members in a sorted set, by lexicographical range. *)
  let zrevrangebylex connection ?limit key min_bound max_bound =
    let min = StringBound.to_cmd ~with_bracket:true min_bound in
    let max = StringBound.to_cmd ~with_bracket:true max_bound in
    let limit = match limit with
      | None -> []
      | Some (offset, count) ->
        [ "LIMIT"; string_of_int offset; string_of_int count; ]
    in
    let command = "ZREVRANGEBYLEX" :: key :: min :: max :: limit in
    send_request connection command >>= return_multibulk

  (* Remove one or more members from a sorted set. *)
  let zrem connection key members =
    let command = "ZREM" :: key :: members in
    send_request connection command >>= return_int

  (* Remove all members in a sorted set between the given lexicographical range. *)
  let zremrangebylex connection key min_bound max_bound =
    let min = StringBound.to_cmd ~with_bracket:true min_bound in
    let max = StringBound.to_cmd ~with_bracket:true max_bound in
    let command = ["ZREMRANGEBYLEX"; key; min; max] in
    send_request connection command >>= return_int

  (* Remove all members in a sorted set between the given score range. *)
  let zremrangebyscore connection key min_bound max_bound =
    let min = FloatBound.to_string min_bound in
    let max = FloatBound.to_string max_bound in
    let command = ["ZREMRANGEBYSCORE"; key; min; max] in
    send_request connection command >>= return_int

  (* Remove all members in a sorted set between the given rank range. *)
  let zremrangebyrank connection key min_bound max_bound =
    let min = string_of_int min_bound in
    let max = string_of_int max_bound in
    let command = ["ZREMRANGEBYRANK"; key; min; max] in
    send_request connection command >>= return_int

  (* Remove one or more members from a sorted set. *)
  let zcard connection key =
    let command = ["ZCARD"; key] in
    send_request connection command >>= return_int

  (* Returns the number of elements in the sorted set at key with a score between min and max. *)
  let zcount connection key lower_bound upper_bound =
    let command = ["ZCOUNT"; key;
                   FloatBound.to_string lower_bound;
                   FloatBound.to_string upper_bound;] in
    send_request connection command >>= return_int

  (* Returns the number of members in a sorted set between a given lexicographical range. *)
  let zlexcount connection key lower_bound upper_bound =
    let command = ["ZLEXCOUNT"; key;
                   StringBound.to_cmd ~with_bracket:true lower_bound;
                   StringBound.to_cmd ~with_bracket:true upper_bound;] in
    send_request connection command >>= return_int

  (* Returns the rank of member in the sorted set stored at key. *)
  let zrank connection key member =
    let command = ["ZRANK"; key; member] in
    send_request connection command >>= return_int_option

  (* Returns the reversed rank of member in the sorted set stored at key. *)
  let zrevrank connection key member =
    let command = ["ZREVRANK"; key; member] in
    send_request connection command >>= return_int_option

  (** Remove and return one or more members with the lowest/highest scores in a sorted set. *)
  let (zpopmin, zpopmax) =
    let zpop op connection key count =
      let command = [op; key; string_of_int count] in
      send_request connection command >>=
      return_key_value_multibulk >>= fun list ->
      try
        IO.return (List.map (fun (value, score) -> (value, float_of_string score)) list)
      with e -> IO.fail e
    in
    zpop "ZPOPMIN", zpop "ZPOPMAX"

  (** Remove and return the member with the lowest/highest scores in a sorted set, or block until one is available. *)
  let (bzpopmin, bzpopmax) =
    let bzpop op connection keys timeout =
      let command = List.concat [[op]; keys; [string_of_float timeout]] in
      send_request connection command >>= fun reply ->
      return_bulk_multibulk reply >>= function
      | [] -> IO.return None
      | (Some key) :: (Some value) :: (Some score) :: [] -> IO.return (Some (key, value, float_of_string score))
      | _ -> IO.fail (Unexpected reply)
    in
    bzpop "BZPOPMIN", bzpop "BZPOPMAX"

  (** Stream commands *)

  let xdel connection stream ids : int IO.t =
    let command = ("XDEL" :: stream :: ids) in
    send_request connection command >>= return_int

  let xadd connection stream ?maxlen ?id pairs : _ IO.t =
    let command =
      List.fold_left (fun acc (k,v) -> v :: k :: acc) [] pairs |> List.rev
    in
    let command = match id with
      | None -> "*" :: command
      | Some id -> id :: command
    in
    let command = match maxlen with
      | None -> command
      | Some (`Exact i) -> "maxcount" :: string_of_int i :: command
      | Some (`Approximate i) -> "maxcount" :: "~" :: string_of_int i :: command
    in
    let command = "XADD" :: stream :: command in
    send_request connection command >>= function
    | `Bulk (Some s) -> IO.return s
    | `Bulk None -> IO.fail (Redis_error "xadd failed")
    | reply -> IO.fail (Unexpected reply)

  let xlen connection stream =
    let command = ["XLEN"; stream] in
    send_request connection command >>= return_int

  let xtrim connection stream ~maxlen () =
    let command = match maxlen with
      | `Exact i -> "maxcount" :: string_of_int i :: []
      | `Approximate i -> "maxcount" :: "~" :: string_of_int i :: []
    in
    let command = "XTRIM" :: stream :: command in
    send_request connection command >>= return_int

  type stream_event = string * (string * string) list

  let decode_evs_ reply =
    match reply with
    | `Multibulk l ->
      l |> List.map
        (function
          | `Multibulk [`Bulk (Some s); `Multibulk pairs] ->
            let pairs = Utils.List.filter_map (function
                | `Bulk (Some k), `Bulk (Some v) -> Some (k, v)
                | _ -> None
              ) (deinterleave pairs)
            in
            s, pairs
          | _ -> raise (Unexpected reply))
    | _ -> raise (Unexpected reply)

  let decode_evs_io_ reply =
    try IO.return (decode_evs_ reply)
    with e -> IO.fail e

  let xrange connection stream ~start ~end_ ?count () : stream_event list IO.t =
    let command = match count with
      | None -> []
      | Some c -> ["COUNT"; string_of_int c]
    in
    let command =
      StringBound.to_cmd ~with_bracket:false start ::
      StringBound.to_cmd ~with_bracket:false end_ :: command in
    let command = "XRANGE" :: stream :: command in
    send_request connection command >>= decode_evs_io_

  let xrevrange connection stream ~start ~end_ ?count () : _ list IO.t =
    let command = match count with
      | None -> []
      | Some c -> ["COUNT"; string_of_int c]
    in
    let command =
      StringBound.to_cmd ~with_bracket:false start ::
      StringBound.to_cmd ~with_bracket:false end_ :: command in
    let command = "XREVRANGE" :: stream :: command in
    send_request connection command >>= decode_evs_io_

  let xread connection ?count ?block_ms pairs : _ list IO.t =
    let command = List.fold_left (fun acc (str, _) -> str :: acc) [] pairs in
    let command = List.fold_left
        (fun acc (_, cnt) ->
           match cnt with
           | `Last -> "$" :: acc
           | `After i -> i :: acc)
        command pairs
    in
    let command = "STREAMS" :: List.rev command in
    let command = match block_ms with
      | None -> command
      | Some s -> "BLOCK" :: string_of_int s :: command
    in
    let command = match count with
      | None -> command
      | Some n -> "COUNT" :: string_of_int n :: command
    in
    let command = "XREAD" :: command in

    let decode_ reply =
      try
        match reply with
        | `Bulk None -> IO.return []
        | `Multibulk l ->
          l |> List.map (function
              | `Multibulk [`Bulk (Some str); evs] ->
                let evs = decode_evs_ evs in
                str, evs
              | _ -> raise (Unexpected reply))
          |> IO.return
        | _ -> raise (Unexpected reply)
      with e -> IO.fail e
    in

    send_request connection command >>= decode_

  (** Transaction commands *)

  (* Marks the start of a transaction block. Subsequent commands will be queued for atomic execution using EXEC. *)
  let multi connection =
    let command = [ "MULTI" ] in
    send_request connection command >>= return_ok_status

  (* Executes all previously queued commands in a transaction and restores the connection state to normal. *)
  let exec connection =
    let command = [ "EXEC" ] in
    send_request connection command >>= return_multibulk

  (* Flushes all previously queued commands in a transaction and restores the connection state to normal. *)
  let discard connection =
    let command = [ "DISCARD" ] in
    send_request connection command >>= return_ok_status

  (* Marks the given keys to be watched for conditional execution of a transaction. *)
  let watch connection keys =
    let command = "WATCH" :: keys in
    send_request connection command >>= return_ok_status

  (* Flushes all the previously watched keys for a transaction. *)
  let unwatch connection =
    let command = [ "UNWATCH" ] in
    send_request connection command >>= return_ok_status

  let queue fn =
    IO.try_bind fn (fun _ -> IO.return ())
      (function
        | Unexpected x -> return_queued_status x
        | e -> IO.fail e)

  (** Scripting commands *)

  (* Load the specified Lua script into the script cache. Returns the SHA1 digest of the script for use with EVALSHA. *)
  let script_load connection script =
    let command = [ "SCRIPT"; "LOAD"; script ] in
    send_request connection command >>= return_no_nil_bulk

  (* Evaluates a script using the built-in Lua interpreter. *)
  let eval connection script keys args =
    let nb_keys = string_of_int (List.length keys) in
    let params = List.flatten [ keys; args ] in
    let command = "EVAL" :: script :: nb_keys :: params in
    send_request connection command


  (* Evaluates a script cached on the server side by its SHA1 digest. *)
  let evalsha connection sha keys args =
    let nb_keys = string_of_int (List.length keys) in
    let params = List.flatten [ keys; args ] in
    let command = "EVALSHA" :: sha :: nb_keys :: params in
    send_request connection command

  (** Server *)

  let bgrewriteaof connection =
    let command = [ "BGREWRITEAOF" ] in
    send_request connection command >>= return_ok_status

  let bgsave connection =
    let command = [ "BGSAVE" ] in
    send_request connection command >>= return_ok_status

  let config_resetstat connection =
    let command = [ "CONFIG"; "RESETSTAT" ] in
    send_request connection command >>= return_ok_status

  let dbsize connection =
    let command = [ "DBSIZE" ] in
    send_request connection command >>= return_int

  (* clear all databases *)
  let flushall connection =
    let command = [ "FLUSHALL" ] in
    send_request connection command >>= return_ok_status

  (* clear current database *)
  let flushdb connection =
    let command = [ "FLUSHDB" ] in
    send_request connection command >>= return_ok_status

  let info connection =
    let command = [ "INFO" ] in
    send_request connection command >>= return_info_bulk

  (* last successful save as Unix timestamp *)
  let lastsave connection =
    let command = [ "LASTSAVE" ] in
    send_request connection command >>= return_float

  (* role in context of replication *)
  let role connection =
    let command = [ "ROLE" ] in
    send_request connection command >>= return_multibulk

  (* synchronous save *)
  let save connection =
    let command = [ "SAVE" ] in
    send_request connection command >>= return_ok_status

  (* save and shutdown server *)
  let shutdown connection =
    let command = [ "SHUTDOWN" ] in
    IO.try_bind
      (fun () -> send_request connection command)
      (fun (_ : reply) -> IO.return ())
      (function
        | End_of_file -> IO.return ()
        | e           -> IO.fail e)

  module MassInsert = struct
    include ModeMassInsert

    let reply main_connection command = function
      | `Ask {slot=_; host; port} ->
        (* Printf.eprintf "create action ASK. new connection for slot %d %s:%d\n%!" slot host port; *)
        connect {host; port} >>= fun connection_ask ->
        send_request connection_ask command >>= fun connection_ask ->
        IO.return (Ask (connection_ask, command))
      | `Moved {slot; host; port} ->
        (* Printf.eprintf "create action MOVED. new connection for slot %d %s:%d\n%!" slot host port; *)
        begin
          try
            IO.return (ConnectionSpecMap.find {host; port} main_connection.cluster.connections)
          with Not_found ->
            (* Printf.eprintf "create action MOVED. connection is not existing. opening a new one.\n%!"; *)
            connect {host; port} >>= fun connection_moved ->
            main_connection.cluster.connections <- ConnectionSpecMap.add {host; port} connection_moved main_connection.cluster.connections;
            IO.return connection_moved
        end
        (* TODO: why is this ignored? *)
        >>= fun _connection_moved ->
        main_connection.cluster.connections_spec <- SlotMap.add slot {host; port} main_connection.cluster.connections_spec;
        send_request main_connection command >>= fun connection_moved ->
        IO.return (Moved (connection_moved, command))
      | `Status _
      | `Int _
      | `Int64 _
      | `Bulk _
      | `Multibulk _ as reply ->
        (* Printf.eprintf "create action REPLY.\n%!"; *)
        IO.return (Reply reply)

    let read_and_reply main_connection stop command =
      match command with
      | Reply r ->
        stop := !stop && true;
        IO.return (Reply r)
      | Command (connection, command) ->
        (* Printf.eprintf "ocaml-redis: next_action COMMAND %s\n%!" (String.concat " " command); *)
        stop := !stop && false;
        read_reply_exn connection.in_ch >>=
        reply main_connection command
      | Ask (connection, command) ->
        (* Printf.eprintf "ocaml-redis: next_action ASK %s\n%!" (String.concat " " command); *)
        stop := !stop && false;
        read_reply_exn connection.in_ch >>=
        reply main_connection command >>= fun action ->
        disconnect connection >>= fun () ->
        IO.return action
      | Moved (connection, command) ->
        (* Printf.eprintf "ocaml-redis: next_action MOVED %s\n%!" (String.concat " " command); *)
        stop := !stop && false;
        read_reply_exn connection.in_ch >>=
        reply main_connection command

    let read_loop connection commands =
      let rec loop commands =
        let stop = ref true in
        (* Printf.eprintf "ocaml-redis: read_loop -----------------------------\n%!"; *)
        IO.map_serial (read_and_reply connection stop) commands >>= fun commands ->
        if not !stop then
          loop commands
        else
          IO.return commands
      in
      loop commands

    let write connection commands =
      IO.map_serial (fun command ->
          (* Printf.eprintf "ocaml-redis: write command %s\n%!" (String.concat " " command); *)
          send_request connection command >>= fun connection ->
          IO.return (Command (connection, command))
        ) commands
      >>= fun commands ->
      read_loop connection commands
      >>= fun responses ->
      IO.map_serial (function
          | Reply r -> IO.return r
          | _ -> assert false
        ) responses

    let set ?ex:(ex=0) ?px:(px=0) ?nx:(nx=false) ?xx:(xx=false) key value =
      match (nx, xx) with
      | (true, true) ->
        raise (Invalid_argument "SET command can contain only one of NX or XX options.")
      | _ ->
        let ex = match ex with
          | 0 -> []
          | _ -> ["EX"; string_of_int ex] in
        let px = match px with
          | 0 -> []
          | _ -> ["PX"; string_of_int px] in
        let nx = match nx with
          | false -> []
          | true -> ["NX"] in
        let xx = match xx with
          | false -> []
          | true -> ["XX"] in
        let base_command = [ "SET"; key; value; ] in
        let args = List.concat [ex; px; nx; xx] in
        let command = List.concat [base_command; args] in
        command

    let del keys =
      "DEL" :: keys

    let expire key seconds =
      let seconds = string_of_int seconds in
      [ "EXPIRE"; key; seconds ]

    let hset key field value =
      [ "HSET"; key; field; value ]

    let hdel key field =
      [ "HDEL"; key; field ]

    let hget key field =
      [ "HGET"; key; field ]

    let hincrby key field increment =
      let increment = string_of_int increment in
      [ "HINCRBY"; key; field; increment ]

    let incr key =
      [ "INCR"; key ]

    let decr key =
      [ "DECR"; key ]
  end

end

module Make(IO : S.IO) = MakeClient(SimpleMode(IO))
module MakeCluster(IO : S.IO) = MakeClient(ClusterMode(IO))
