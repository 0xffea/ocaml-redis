module IO = struct
  type 'a t = 'a Lwt.t

  type fd = Lwt_unix.file_descr
  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  type 'a stream = 'a Lwt_stream.t
  type stream_count = unit

  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let catch = Lwt.catch
  let try_bind = Lwt.try_bind
  let ignore_result = Lwt.ignore_result
  let return = Lwt.return
  let fail = Lwt.fail
  let run = Lwt_main.run
  let atomic = Lwt_io.atomic

  let connect host port =
    let port = string_of_int port in
    let addr_info =
      let open Lwt_unix in
      getaddrinfo host port [AI_FAMILY PF_INET] >>= function
      | ai::_ -> return ai
      | [] ->
        getaddrinfo host port [AI_FAMILY PF_INET6] >>= function
        | ai::_ -> return ai
        | [] -> Lwt.fail_with "Could not resolve redis host!"
    in
    addr_info >>= fun addr_info ->
    let fd = Lwt_unix.socket addr_info.Lwt_unix.ai_family Lwt_unix.SOCK_STREAM 0 in
    let do_connect () =
      Lwt_unix.connect fd addr_info.Lwt_unix.ai_addr >>= fun () ->
      return fd
    in
    catch do_connect (fun exn -> Lwt_unix.close fd >>= fun () -> fail exn)

  let close = Lwt_unix.close
  let sleep = Lwt_unix.sleep

  let in_channel_of_descr fd = Lwt_io.of_fd ~mode:Lwt_io.input fd
  let out_channel_of_descr fd = Lwt_io.of_fd ~mode:Lwt_io.output fd
  let input_char = Lwt_io.read_char
  let really_input = Lwt_io.read_into_exactly
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush

  let iter = Lwt_list.iter_p
  let iter_serial = Lwt_list.iter_s
  let map = Lwt_list.map_p
  let map_serial = Lwt_list.map_s
  let fold_left = Lwt_list.fold_left_s

  let stream_from = Lwt_stream.from
  let stream_next = Lwt_stream.next

  type mutex = Lwt_mutex.t
  let mutex_create = Lwt_mutex.create
  let mutex_with m f = Lwt_mutex.with_lock m f

  type condition = unit Lwt_condition.t
  let condition_create () = Lwt_condition.create ()
  let condition_wait c m = Lwt_condition.wait ~mutex:m c
  let condition_signal c = Lwt_condition.signal c ()
  let condition_broadcast c = Lwt_condition.broadcast c ()
end

module Client = Redis.Client.Make(IO)
module Cache = Redis.Cache.Make(IO)(Client)
module Mutex = Redis.Mutex.Make(IO)(Client)

module ClusterClient = Redis.Client.MakeCluster(IO)
module ClusterCache = Redis.Cache.Make(IO)(ClusterClient)
module ClusterMutex = Redis.Mutex.Make(IO)(ClusterClient)

module Pool = Redis.Pool.Make(IO)(Client)
