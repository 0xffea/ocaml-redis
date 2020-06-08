module Test_lwt = Test.Make(Redis_lwt.Client)
module Test_lwt_cluster = Test.Make(Redis_lwt.ClusterClient)

open OUnit
open Lwt.Infix

(* Compute fibonacci function using Redis as a memoization cache *)
module Test_lwt_fib = struct
  module C = Redis_lwt.Client
  module P = Redis_lwt.Pool
  module Cache = Redis_lwt.Cache(struct
      type key = int
      type data = int
      let cache_key i = Printf.sprintf "test_fib_%d" i
      let cache_expiration = Some 500
      let data_of_string = int_of_string
      let string_of_data = string_of_int
    end)

  let fib_ref n =
    let prev = ref 1 in
    let cur = ref 1 in
    for _i = 2 to n do
      let n = !cur in
      cur := !cur + !prev;
      prev := n
    done;
    !cur

  let check_fib n (pool:P.t) =
    let rec fib n =
      if n <= 1 then
        Lwt.return 1
      else (
        P.with_connection pool (fun c -> Cache.get c n) >>= function
        | Some n -> Lwt.return n
        | None ->
          let n1 = fib (n-1) in
          let n2 = fib (n-2) in
          n1 >>= fun n1 ->
          n2 >>= fun n2 ->
          let res = n1 + n2 in
          P.with_connection pool (fun c -> Cache.set c n res) >|= fun _ -> res
      )
    in
    let start = Unix.gettimeofday () in
    fib n >>= fun res ->
    let stop = Unix.gettimeofday () in
    let ref = fib_ref n in
    Printf.eprintf "fib %d = %d (expected: %d) in %.3fs\n" n res ref (stop -. start);
    OUnit.assert_equal ~printer:string_of_int ref res;
    Lwt.return ()

  let bracket test_case () =
    try
      Lwt_main.run @@ P.with_pool ~size:32 Test_lwt.redis_spec test_case
    with C.Unexpected reply as exn ->
      Printf.eprintf "Got unexpected reply: %s\n" (C.string_of_reply reply);
      raise exn

    let test_fib n = bracket (check_fib n)

    let suite =
      "fib" >::: [
        "10" >:: test_fib 10;
        "20" >:: test_fib 20;
        "30" >:: test_fib 30;
        "40" >:: test_fib 40;
        "60" >:: test_fib 60;
      ]

   let teardown =
     Test_lwt.bracket
       (fun conn ->
         C.keys conn "test_fib_*" >>= fun keys ->
         if keys <> [] then C.del conn keys >>= fun _ -> Lwt.return ()
         else Lwt.return ())
end

let suite =
  "lwt" >::: [
    Test_lwt.suite "simple";
    Test_lwt_cluster.suite "cluster";
    Test_lwt_fib.suite;
  ]

let () =
  Random.self_init ();
  let res = run_test_tt suite in
  Test_lwt.teardown ();
  Test_lwt_fib.teardown();
  exit @@ Test.test_exit_code res
