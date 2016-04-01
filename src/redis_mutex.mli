(**
   Mutex manager
*)
module Make(IO : S.IO)(Client : S.Client with module IO = IO) : S.Mutex
  with module IO = IO
  with module Client = Client
