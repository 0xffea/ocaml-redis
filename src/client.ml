(** Bindings for redis.

    This has only been tested with Redis 2.2, but will probably work for >= 2.0
 **)
module Make(IO : S.IO) = struct
  module IO = IO

  let (>>=) = IO.(>>=)

  (* reply from server *)
  type reply = [
    | `Status of string
    | `Error of string
    | `Int of int
    | `Int64 of Int64.t
    | `Bulk of string option
    | `Multibulk of reply list
  ]

  type connection = {
    fd     : IO.fd;
    in_ch  : IO.in_channel;
    out_ch : IO.out_channel;
    stream : reply list IO.stream;
  }

  (* error responses from server *)
  exception Error of string

  (* these signal protocol errors *)
  exception Unexpected of reply
  exception Unrecognized of string * string (* explanation, data *)

  (* server connection info *)
  type connection_spec = {
    host : string;
    port : int;
  }

  type bit_operation = AND | OR | XOR | NOT

  let write out_ch args =
    let num_args = List.length args in
    IO.output_string out_ch (Printf.sprintf "*%d" num_args) >>= fun () ->
    IO.output_string out_ch "\r\n" >>= fun () ->
    IO.iter
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
    IO.input_char in_ch >>= function
      | '+' ->
          read_line in_ch >>= fun s -> IO.return (`Status s)
      | '-' ->
          read_line in_ch >>= fun s -> IO.return (`Error s)
      | ':' ->
          read_integer in_ch
      | '$' ->
          read_bulk in_ch
      | '*' ->
          read_multibulk in_ch
      | c ->
          IO.fail (Unrecognized ("Unexpected char in reply", Char.escaped c))

  let read_reply_exn in_ch =
    read_reply in_ch >>= function
      | `Status _
      | `Int _
      | `Int64 _
      | `Bulk _
      | `Multibulk _ as reply ->
          IO.return reply
      | `Error msg ->
          IO.fail (Error msg)

  let send_request connection command =
    write connection.out_ch command >>= fun () ->
    read_reply_exn connection.in_ch

  let interleave list =
    let rec loop acc = function
      | (x, y) :: tail -> loop (y :: x :: acc ) tail
      | [] -> List.rev acc
    in
    loop [] list

  (* this assumes the list length is even *)
  let deinterleave list =
    let rec loop acc = function
      | x :: y :: tail -> loop ((x, y) :: acc) tail
      | [] -> List.rev acc
      | _ -> raise (Invalid_argument "List length must be even")
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

  let return_int64 = function
    | `Int n   -> IO.return (Int64.of_int n)
    | `Int64 n -> IO.return n
    | x        -> IO.fail (Unexpected x)

  let return_float = function
    | `Int n   -> IO.return (float_of_int n)
    | `Int64 n -> IO.return (Int64.to_float n)
    | `Bulk (Some str) -> IO.return (float_of_string str)
    | x        -> IO.fail (Unexpected x)

  let return_multibulk = function
    | `Multibulk m -> IO.return m
    | x            -> IO.fail (Unexpected x)

  let return_bulk_multibulk reply =
    try
      return_multibulk reply >>= fun list ->
      IO.return (List.map (function
        | `Bulk b -> b
        | x -> raise (Unexpected x)
      ) list)
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
      | x                -> IO.fail (Invalid_argument "Expected nil or two-element multi-bulk")

  let return_info_bulk reply =
    return_bulk reply >>= function
      | Some b ->
          let fields = Utils.String.nsplit b "\r\n" in
          let fields = List.filter (fun x -> x <> "" && not (String.get x 0 = '#')) fields in
          IO.return (Utils.List.filter_map (fun f -> Utils.String.split f ":") fields)
      | None   -> IO.return []

  (* generate command for SORT *)
  let sort_command
      ?by
      ?limit (* offset, limit *)
      ?(get=[])
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

  let connect spec =
    let {host=host; port=port} = spec in
    IO.connect host port >>= fun fd ->
    let in_ch = IO.in_channel_of_descr fd in
    IO.return
      { fd = fd;
        in_ch = in_ch;
        out_ch = IO.out_channel_of_descr fd;
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
    try
      f c >>= fun r ->
      disconnect c >>= fun () ->
      IO.return r
    with e ->
      disconnect c >>= fun () ->
      IO.fail e

  let stream connection = connection.stream

  (* Raises Error if password is invalid. *)
  let auth connection password =
    let command = [ "AUTH"; password ] in
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
      | _ -> IO.fail (Error "SCAN returned unexpected result")

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
      raise (Invalid_argument "SET command can contain only one of NX or XX options.")
    | _ ->
      let ex = match ex with
        | 0 -> ""
        | _ -> String.concat " " ["EX"; string_of_int ex] in
      let px = match px with
        | 0 -> ""
        | _ -> String.concat " " ["PX"; string_of_int px] in
      let nx = match nx with
        | false -> ""
        | true -> "NX" in
      let xx = match xx with
        | false -> ""
        | true -> "XX" in
      let base_command = [ "SET"; key; value; ] in
      let args = List.filter (fun x -> String.length x > 0) [ex; px; nx; xx] in
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
      | _ -> IO.fail (Error "HSCAN returned unexpected result")

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
  let lpush connection key value =
    let command = [ "LPUSH"; key; value ] in
    send_request connection command >>= return_int

  (* Only push when list exists. Return length of list after operation. *)
  let lpushx connection key value =
    let command = [ "LPUSHX"; key; value ] in
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
  let rpush connection key value =
    let command = [ "RPUSH"; key; value ] in
    send_request connection command >>= return_int

  let rpushx connection key value =
    let command = [ "RPUSHX"; key; value ] in
    send_request connection command >>= return_int

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
  let zadd connection key values =
    let f acc (s, v) = (string_of_int s) :: v :: acc in
    let values = List.fold_left f [] values in
    let command = "ZADD" :: key :: values in
    send_request connection command >>= return_int

  (* Returns the score of member in the sorted set. *)
  let zscore connection key member =
    let command = "ZSCORE" :: key :: member in
    send_request connection command >>= return_bulk

  (* Return a range of members in a sorted set, by index. *)
  let zrange connection ?(withscores=false) key start stop =
    let istart = string_of_int start in
    let istop = string_of_int stop in
    let scores = if withscores then ["withscores"] else [] in
    let command = "ZRANGE" :: key :: istart :: istop :: scores in
    send_request connection command >>= return_multibulk

  (* Return a range of members in a sorted set, by score. *)
  let zrangebyscore connection ?(withscores=false) key min max =
    let imin = string_of_int min in
    let imax = string_of_int max in
    let scores = if withscores then ["withscores"] else [] in
    let command = "ZRANGEBYSCORE" :: key :: imin :: imax :: scores in
    send_request connection command >>= return_multibulk

  (* Remove one or more members from a sorted set. *)
  let zrem connection members =
    let command = "ZREM" :: members in
    send_request connection command >>= return_int

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
end
