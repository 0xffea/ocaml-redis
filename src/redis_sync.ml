module IO = struct
  type 'a t = 'a

  type fd = Unix.file_descr
  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel

  type socket_domain = Unix.socket_domain
  type socket_type = Unix.socket_type
  type socket_addr = Unix.sockaddr

  type 'a stream = 'a Stream.t
  type stream_count = int

  let (>>=) a f = f a
  let catch f exn_handler = try f () with e -> exn_handler e
  let try_bind f bind_handler exn_handler = try f () >>= bind_handler with e -> exn_handler e
  let ignore_result = ignore
  let return a = a
  let fail e = raise e
  let run a = a

  let connect host port =
    let fd = Unix.socket (Unix.PF_INET) Unix.SOCK_STREAM 0 in
    let sock_addr =
      let port = string_of_int port in
      match Unix.getaddrinfo host port [] with
      | [] -> failwith "Could not resolve redis host!"
        | addrinfo::_ -> addrinfo.Unix.ai_addr
    in
    Unix.connect fd sock_addr;
    return fd
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
end

module Client = Client.Make(IO)
module Cache = Cache.Make(IO)(Client)
module Mutex = Redis_mutex.Make(IO)(Client)
