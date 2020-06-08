
module Make(IO : S.IO)(Client : S.Client with module IO=IO)
  : S.POOL with module IO = IO and module Client = Client
