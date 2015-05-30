module IO : S.IO

module Client : S.Client with module IO = IO

module Cache (Params : S.Cache_params) : S.Cache
  with module IO = IO
  with module Client = Client

module Mutex : S.Mutex
  with module IO = IO
  with module Client = Client
