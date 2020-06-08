module IO = struct
  type 'a t = 'a

  type fd = Unix.file_descr
  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel

  type 'a stream = 'a Stream.t
  type stream_count = int

  let (>>=) a f = f a
  let (>|=) a f = f a
  let catch f exn_handler = try f () with e -> exn_handler e
  let try_bind f bind_handler exn_handler = try f () >>= bind_handler with e -> exn_handler e
  let ignore_result = ignore
  let return a = a
  let fail e = raise e
  let run a = a
  let atomic f ch = f ch

  let connect host port =
    let port = string_of_int port in
    let addr_info =
      let open Unix in
      match getaddrinfo host port [AI_FAMILY PF_INET] with
      | ai::_ -> ai
      | [] ->
        match getaddrinfo host port [AI_FAMILY PF_INET6] with
        | ai::_ -> ai
        | []    -> failwith "Could not resolve redis host!"
    in
    let fd = Unix.socket addr_info.Unix.ai_family Unix.SOCK_STREAM 0 in
    try
      Unix.connect fd addr_info.Unix.ai_addr; fd
    with
      exn -> Unix.close fd; raise exn

  let close = Unix.close
  let sleep a = ignore (Unix.select [] [] [] a)

  let in_channel_of_descr = Unix.in_channel_of_descr
  let out_channel_of_descr = Unix.out_channel_of_descr
  let input_char = Pervasives.input_char
  let really_input = Pervasives.really_input
  let output_string = output_string
  let flush = Pervasives.flush

  let iter = List.iter
  let iter_serial = List.iter
  let map = List.map
  let map_serial = List.map
  let fold_left = List.fold_left

  let stream_from = Stream.from
  let stream_next = Stream.next

  type mutex = Mutex.t
  let mutex_create = Mutex.create
  let mutex_with m f =
    Mutex.lock m;
    try
      let x = f() in
      Mutex.unlock m;
      x
    with e ->
      Mutex.unlock m;
      raise e

  type condition = Condition.t
  let condition_create () = Condition.create ()
  let condition_wait c m = Condition.wait c m
  let condition_signal = Condition.signal
  let condition_broadcast = Condition.broadcast
end

module Client = Redis.Client.Make(IO)
module Cache = Redis.Cache.Make(IO)(Client)
module Mutex = Redis.Mutex.Make(IO)(Client)

module ClusterClient = Redis.Client.MakeCluster(IO)
module ClusterCache = Redis.Cache.Make(IO)(ClusterClient)
module ClusterMutex = Redis.Mutex.Make(IO)(ClusterClient)
