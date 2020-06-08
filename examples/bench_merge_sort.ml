open Lwt.Infix
module C = Redis_lwt.Client
module P = Redis_lwt.Pool

type t = {
  pool: P.t;
  l: int list;
  n: int;
}

(* generate a random list, but always the same *)
let mk_list n : int list =
  let st = Random.State.make [| 42 |] in
  CCList.init n (fun _ -> Random.State.int st 5_000)

(* make a fresh index *)
let mk_id (self:t) (pre:string) : string Lwt.t =
  P.with_connection self.pool (fun c -> C.incr c "bms:cur_id") >|= fun i ->
  Printf.sprintf "bms:id:%s:%d" pre i

let ignore_int (_x:int Lwt.t) = _x >|= fun _ -> ()

let str_of_list (self:t) (id:string) : (int * string) Lwt.t =
  P.with_connection self.pool (fun c -> C.lrange c id 0 self.n) >|= fun l ->
  List.length l, Printf.sprintf "[%s]" (String.concat "," l)

let run (self:t) : unit Lwt.t =
  mk_id self "list" >>= fun id_list ->
  (* insert the whole list *)
  P.with_connection self.pool
    (fun c -> C.rpush c id_list (List.rev_map string_of_int self.l))
  >>= fun _n ->
  assert (_n = self.n);
  str_of_list self id_list >>= fun (len,s_list) ->
  Printf.printf "initial (len %d): %s\n%!" len s_list;
  (* merge [id1] and [id2] into [into] *)
  let merge (id1:string) (id2:string) ~into : unit Lwt.t =
    (*Lwt.async (fun () ->
        str_of_list self id1 >>= fun (_,s1) ->
        str_of_list self id2 >>= fun (_,s2) ->
        str_of_list self into >|= fun (_,sinto) ->
        Printf.printf "merge %s=%s and %s=%s into %s=%s\n%!"
          id1 s1 id2 s2 into sinto);*)
    assert (id1 <> id2);
    let rec loop () : unit Lwt.t =
      let len1 = P.with_connection self.pool (fun c -> C.llen c id1) in
      let len2 = P.with_connection self.pool (fun c -> C.llen c id2) in
      len1 >>= fun len1 ->
      len2 >>= fun len2 ->
      (* Printf.printf "  len1=%d, len2=%d\n%!" len1 len2; *)
      if len1=0 && len2=0 then Lwt.return ()
      else if len1=0 then (
        P.with_connection self.pool
          (fun c -> C.lrange c id2 0 len2 >>= C.rpush c into) |> ignore_int
      ) else if len2=0 then (
        P.with_connection self.pool
          (fun c -> C.lrange c id1 0 len1 >>= C.rpush c into) |> ignore_int
      ) else (
        let x =
          P.with_connection self.pool
            (fun c -> C.lpop c id1 >|= function
               | None -> assert false | Some x -> int_of_string x)
        in
        let y =
          P.with_connection self.pool
            (fun c -> C.lpop c id2 >|= function
               | None -> assert false | Some x -> int_of_string x)
        in
        x >>= fun x ->
        y >>= fun y ->
        (* Printf.printf "  x=%d, y=%d\n%!" x y; *)
        if x<y then (
          P.with_connection self.pool (fun c ->
              C.lpush c id2 [string_of_int y] >>= fun _ ->
              C.rpush c into [string_of_int x] |> ignore_int)
          >>= loop
        ) else (
          P.with_connection self.pool (fun c ->
              C.lpush c id1 [string_of_int x] >>= fun _ ->
              C.rpush c into [string_of_int y] |> ignore_int)
          >>= loop
        )
      )
    in
    (* str_of_list self into >>= fun (_,s) -> Printf.printf "  -> [%s]=%s\n%!" into s; *)
    loop ()
  in
  (* now recursively do merge sort *)
  let rec sort (id_list:string) : unit Lwt.t =
    P.with_connection self.pool (fun c -> C.llen c id_list)
    >>= fun len ->
    if len >= 2 then (
      let mid = len/2 in
      let l1 = mk_id self "list_tmp" in
      let l2 = mk_id self "list_tmp" in
      l1 >>= fun l1 ->
      l2 >>= fun l2 ->
      let fut1 =
        P.with_connection self.pool
          (fun c -> C.lrange c id_list 0 (mid-1) >>= C.rpush c l1)
      and fut2 =
        P.with_connection self.pool
          (fun c -> C.lrange c id_list mid len >>= C.rpush c l2)
      in
      fut1 >>= fun len1 ->
      fut2 >>= fun len2 ->
      assert (len1 + len2 = len);
      P.with_connection self.pool
        (fun c -> C.del c [id_list] |> ignore_int) >>= fun () ->
      (* sort sublists in parallel *)
      let fut1 = sort l1 in
      let fut2 = sort l2 in
      fut1 >>= fun () ->
      fut2 >>= fun () ->
      merge l1 l2 ~into:id_list >>= fun () ->
      (* cleanup tmp clauses *)
      P.with_connection self.pool (fun c -> C.del c [l1; l2]) >|= fun _ -> ()
    ) else Lwt.return ()
  in
  sort id_list >>= fun () ->
  str_of_list self id_list >>= fun (len,s_res) ->
  Printf.printf "result (len %d): %s\n%!" len s_res;
  P.with_connection self.pool (fun c ->
      (C.lrange c id_list 0 self.n >|= List.map int_of_string) >>= fun l ->
      C.del c [id_list] >>= fun _ ->
      C.del c ["bms:cur_id"] >|= fun _ -> l)
  >>= fun l ->
  (* must be sorted *)
  assert (CCList.is_sorted ~cmp:CCInt.compare l);
  (* same length *)
  assert (List.length l = List.length self.l);
  (* same elements *)
  assert (
    let module IS = CCSet.Make(CCInt) in
    IS.equal (IS.of_list l) (IS.of_list self.l));
  Lwt.return ()

let run ?(n=100_000) host port : unit =
  let spec = {C.host; port} in
  let start = Unix.gettimeofday () in
  Lwt_main.run
    (P.with_pool ~size:32 spec
       (fun pool ->
          let st = {n; pool; l=mk_list n} in
          run st));
  let stop = Unix.gettimeofday () in
  Printf.printf "time: %.3fs\n%!" (stop -. start);
  ()

