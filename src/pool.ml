

module Make(IO : S.IO)(Client : S.Client with module IO=IO)
  : S.POOL with module IO = IO and module Client = Client
= struct
  module IO = IO
  module Client = Client

  open IO

  type t = {
    mutex: IO.mutex;
    condition: IO.condition; (* for threads waiting for a connection *)
    pool: Client.connection Queue.t; (* connections available *)
    spec: Client.connection_spec;
    size: int;
    mutable closed: bool; (* once true, no query accepted *)
  }

  let size self = self.size

  (* initialize [i] connections *)
  let rec init_conns (self:t) i : unit IO.t =
    if i<=0 then IO.return ()
    else (
      Client.connect self.spec >>= fun c ->
      Queue.push c self.pool;
      init_conns self (i-1)
    )

  let create ~size spec : t IO.t =
    if size < 1 then invalid_arg "pool.create: size >= 1 required";
    let self = {
      mutex=IO.mutex_create ();
      condition=IO.condition_create();
      pool=Queue.create ();
      spec;
      size;
      closed = false;
    } in
    init_conns self size >>= fun () ->
    Format.printf "queue: %d@." (Queue.length self.pool);
    IO.return self

  let close (self:t) : unit IO.t =
    self.closed <- true; (* should always be atomic *)
    (* wake up waiters eagerly, to have them die earlier *)
    IO.condition_broadcast self.condition;
    (* close remaining connections *)
    let rec close_conns_in_pool_ () =
      if Queue.is_empty self.pool then IO.return ()
      else (
        let c = Queue.pop self.pool in
        Client.disconnect c >>= close_conns_in_pool_
      )
    in
    close_conns_in_pool_ ()

  let with_pool ~size spec f : _ IO.t =
    create ~size spec >>= fun pool ->
    IO.try_bind
      (fun () -> f pool)
      (fun x -> close pool >|= fun () -> x)
      (fun e -> close pool >>= fun () -> IO.fail e)

  (* release a connection back into the pool, or close it if the
     pool is closed. *)
  let release_conn_ (self:t) (c:Client.connection) : unit IO.t =
    IO.mutex_with self.mutex
      (fun () ->
         if self.closed then (
           (* close connection *)
           Client.disconnect c
         ) else (
           (* release connection, and potentially wake up a waiter to grab it *)
           Queue.push c self.pool;
           IO.condition_signal self.condition;
           IO.return ()
         )
      )

  (* open a new connection and put it into the pool *)
  let reopen_conn_ (self:t) : unit IO.t =
    Client.connect self.spec >>= release_conn_ self

  let rec with_connection (self:t) (f: _ -> 'a IO.t) : 'a IO.t =
    if self.closed then IO.fail (Failure "pool closed")
    else (
      (* try to acquire a connection *)
      IO.mutex_with self.mutex
        (fun () ->
           if Queue.is_empty self.pool then (
             IO.condition_wait self.condition self.mutex >|= fun () ->
             None
           ) else (
             let c = Queue.pop self.pool in
             IO.return (Some c)
           ))
      >>= function
      | None -> with_connection self f (* try again *)
      | Some c ->
        (* run [f c], and be sure to cleanup afterwards *)
        IO.try_bind
          (fun () -> f c)
          (fun x -> release_conn_ self c >|= fun () -> x)
          (fun e ->
             (* close [c] and reopen a new one instead;
                could have been interrupted during a transfer! *)
             let fut1 = reopen_conn_ self in
             let fut2 = Client.disconnect c in
             fut1 >>= fun () ->
             fut2 >>= fun () ->
             IO.fail e)
    )

end
