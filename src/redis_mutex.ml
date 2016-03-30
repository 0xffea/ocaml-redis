module Make(IO : S.IO)(Client : S.Client with module IO = IO) = struct
  module IO = IO
  module Client = Client

  exception Error of string

  let (>>=) = IO.(>>=)

  let acquire conn ?(atime=10.) ?(ltime=10) mutex id =
    let etime = Unix.time() +. atime in

    let update_ttl () =
      Client.ttl conn mutex >>= function
        | None -> Client.expire conn mutex ltime >>= fun _ -> IO.return ()
        | _ -> IO.return () in

    let rec loop () =
      Client.setnx conn mutex id >>= function
      | true -> Client.expire conn mutex ltime >>= fun _ -> IO.return ()
      | _ -> update_ttl () >>= fun _ ->
        if Unix.time() < etime then IO.sleep(0.1) >>= loop
        else IO.fail (Error ("could not acquire lock " ^ mutex))
    in
    loop ()

  let release conn mutex id =
    Client.watch conn [mutex] >>= fun _ ->
    Client.get conn mutex >>= function
    | Some x when x = id ->
      Client.multi conn >>= fun _ ->
      Client.queue (fun () -> Client.del conn [mutex]) >>= fun _ ->
      Client.exec conn >>= fun _ ->
      IO.return ()
    | _ ->
      Client.unwatch conn >>= fun _ ->
      IO.fail (Error ("lock was lost: " ^ mutex))

  let with_mutex conn ?atime ?ltime mutex fn =
    let id = Uuidm.(to_string (create `V4)) in
    acquire conn ?atime ?ltime mutex id >>= fun _ ->
    IO.catch
    (* try *) (fun () ->
      fn () >>= fun res ->
      release conn mutex id >>= fun _ ->
      IO.return res)
    (* catch *) (function e ->
      release conn mutex id >>= fun _ ->
      IO.fail e)
end
