module type IO = sig
  type 'a t

  type fd
  type in_channel
  type out_channel

  type 'a stream
  type stream_count

  val getaddrinfo : string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list t
  val connect : Unix.socket_domain -> Unix.sockaddr -> fd t
  val close : fd -> unit t
  val sleep : float -> unit t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
  val ignore_result : 'a t -> unit
  val return : 'a -> 'a t
  val fail : exn -> 'a t
  val run : 'a t -> 'a
  val atomic : (in_channel -> 'a t) -> in_channel -> 'a t

  val in_channel_of_descr : fd -> in_channel
  val out_channel_of_descr : fd -> out_channel
  val input_char : in_channel -> char t
  val really_input : in_channel -> bytes -> int -> int -> unit t
  val output_string : out_channel -> string -> unit t
  val flush : out_channel -> unit t

  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter_serial : ('a -> unit t) -> 'a list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val map_serial : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t

  val stream_from : (stream_count -> 'b option t) -> 'b stream
  val stream_next: 'a stream -> 'a t

  type mutex
  val mutex_create : unit -> mutex
  val mutex_with : mutex -> (unit -> 'a t) -> 'a t

  type condition
  val condition_create : unit -> condition
  val condition_wait : condition -> mutex -> unit t
  val condition_signal : condition -> unit
  val condition_broadcast: condition -> unit
end

module type Client = sig
  module IO : IO

  module StringBound : sig
    type t = NegInfinity | PosInfinity | Exclusive of string | Inclusive of string

    val to_string : t -> string
  end

  module FloatBound : sig
    type t = NegInfinity | PosInfinity | Exclusive of float | Inclusive of float

    val to_string : t -> string
  end

  (** {2 Types and exceptions } *)

  type redirection = {
    slot: int;
    host: string;
    port: int;
  }

  type reply = [
    | `Status of string
    | `Error of string
    | `Int of int
    | `Int64 of Int64.t
    | `Bulk of string option
    | `Multibulk of reply list
    | `Ask of redirection
    | `Moved of redirection
  ]

  val string_of_reply : reply -> string
  (** For debugging purpose.
      @since 0.4 *)

  (** Server connection info *)
  type connection_spec = {
    host : string;
    port : int;
  }

  val string_of_connection_spec : connection_spec -> string
  (** Print the spec
      @since 0.8 *)

  val pp_connection_spec : Format.formatter -> connection_spec -> unit
  (** Print the spec
      @since 0.8 *)

  val connection_spec : ?port:int -> string -> connection_spec
  (** Create a connection spec with the given host.
      @param port port to connect to (default [6379])
      @since 0.5 *)

  val connection_spec_unix_socket : string -> connection_spec
  (** Create a connection spec to use the given Unix socket.
      @since 0.7 *)

  module SlotMap : Map.S with type key = int
  module ConnectionSpecMap : Map.S with type key = connection_spec

  type cluster_connections = private {
    mutable connections_spec : connection_spec SlotMap.t;
    mutable connections : connection ConnectionSpecMap.t;
  }
  and connection = private {
    fd      : IO.fd;
    in_ch   : IO.in_channel;
    out_ch  : IO.out_channel;
    stream  : reply list IO.stream;
    cluster : cluster_connections;
  }

  (** Error responses from server *)
  exception Redis_error of string

  (** Protocol errors *)
  exception Unexpected of reply
  exception Unrecognized of string * string (* explanation, data *)

  (** Possible BITOP operations *)
  type bit_operation = AND | OR | XOR | NOT

  (** {2 Connection handling } *)

  val connect : connection_spec -> connection IO.t
  val disconnect : connection -> unit IO.t
  val with_connection : connection_spec -> (connection -> 'a IO.t) -> 'a IO.t
  val stream : connection -> reply list IO.stream

  (** {2 Connection commands } *)

  (** Authenticate to server. *)
  val auth : connection -> string -> unit IO.t

  (** Sends a custom request to the Redis server. Example: [ send_request connection ["set"; "foo"; "bar"] ] @since 0.6*)
  val send_custom_request : connection -> string list -> reply IO.t

  val send_pipelined_custom_requests : connection -> string list list -> reply list IO.t
  (** Send a list of custom requests in a pipelined fashion (all are written, then all
      replies are read)
      @since 0.8 *)

  (** Authenticate to server with username and password. *)
  val auth_acl : connection -> string -> string -> unit IO.t

  (** Echo given string. *)
  val echo : connection -> string -> string option IO.t

  (** Ping connection; returns [ true ] if ping was successfull. *)
  val ping : connection -> bool IO.t

  (** Close connection. *)
  val quit : connection -> unit IO.t

  (** Switch to a different db; raises {!Error} if index is invalid. *)
  val select : connection -> int -> unit IO.t

  (** {2 SENTINEL commands } *)
  val sentinel_masters : connection -> (string * string) list list IO.t
  val sentinel_get_master_addr_by_name : connection -> string -> (string * string) option IO.t

  (** {2 Keys commands} *)

  (** Delete a key; returns the number of keys removed. *)
  val del : connection -> string list -> int IO.t

  (** Determine if a key exists. *)
  val exists : connection -> string -> bool IO.t

  (** Set a key's time to live in seconds; returns [ true ] if timeout was set, false otherwise. *)
  val expire : connection -> string -> int -> bool IO.t

  (** Set a key's time to live in milliseconds; returns [ true ] if timeout was set, false otherwise. *)
  val pexpire : connection -> string -> int -> bool IO.t

  (** Set the expiration for a key as a UNIX timestamp, the time is truncated to the nearest second; returns [ true ] if timeout was set, [ false ] otherwise. *)
  val expireat : connection -> string -> float -> bool IO.t

  (** Set the expiration for a key as a UNIX timestamp in milliseconds; returns [ true ] if timeout was set, [ false ] otherwise. *)
  val pexpireat : connection -> string -> int -> bool IO.t

  (** Find all keys matching the given pattern. *)
  val keys : connection -> string -> string list IO.t

  (** Incrementally iterate the keys space; see tests for usage example. *)
  val scan : ?pattern:string -> ?count:int -> connection -> int -> (int * string list) IO.t

  (** Move key to a different db; returns [ true ] if key was moved, [ false ] otherwise. *)
  val move : connection -> string -> int -> bool IO.t

  (** Remove timeout on key; returns [ true ] if timeout was removed, [ false ] otherwise. *)
  val persist : connection -> string -> bool IO.t

  (** Return a random key from the keyspace; returns [ None ] if db is empty. *)
  val randomkey : connection -> string option IO.t

  (** Rename a key; raises {!Error} if key doesn't exist. *)
  val rename : connection -> string -> string -> unit IO.t

  (** Rename a key, only if the new key does not exist; returns [ true ] if key was renamed, [ false ] if newkey already exists. *)
  val renamenx : connection -> string -> string -> bool IO.t

  (** Sort elements in a list, set or sorted set; return sorted list of items. *)
  val sort :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] -> ?alpha:bool -> string -> string list IO.t

  (** Sort and store elements in a list, set or sorted set; returns length of sorted items list which was stored. *)
  val sort_and_store :
    connection ->
    ?by:string ->
    ?limit:int * int ->
    ?get:'a list ->
    ?order:[< `Asc | `Desc ] ->
    ?alpha:bool -> string -> string -> int IO.t

  (** Time to live for a key in seconds; returns [ None ] if key doesn't exist or doesn't have a timeout. *)
  val ttl : connection -> string -> int option IO.t

  (** Time to live for a key in milliseconds; returns [ None ] if key doesn't exist or doesn't have a timeout. *)
  val pttl : connection -> string -> int option IO.t

  (** Determine the type stored as key. *)
  val type_of : connection -> string -> [> `Hash | `List | `None | `String | `Zset ] IO.t

  (** Return a serialized version of the value stored at the specified key; returns [ None ] if key doesn't exist. *)
  val dump: connection -> string -> string option IO.t

  (** Create a key with serialized value (obtained via DUMP). *)
  val restore: connection -> string -> int -> string -> unit IO.t

  (** Atomically transfer a key from a source Redis instance to a destination Redis instance. *)
  val migrate : connection ->
    ?copy:bool -> ?replace:bool ->
    string -> int -> string -> int -> int ->
    unit IO.t

  (** Inspect the internals of Redis objects; returns the number of references of the value associated with the specified key. *)
  val object_refcount: connection -> string -> int option IO.t

  (** Inspect the internals of Redis objects; returns the kind of internal representation used in order to store the value associated with a key. *)
  val object_encoding: connection -> string -> string option IO.t

  (** Inspect the internals of Redis objects; returns the number of seconds since the object stored at the specified key is idle. *)
  val object_idletime: connection -> string -> int option IO.t

  (** {2 String commands} *)

  (** Append a value to a key; returns length of string after append. *)
  val append : connection -> string -> string -> int IO.t

  (** Sets or clears the bit at offset in the string value stored at key. *)
  val setbit : connection -> string -> int -> int -> int IO.t

  (** Returns the bit value at offset in the string value stored at key. *)
  val getbit : connection -> string -> int -> int IO.t

  (** Perform a bitwise operation between multiple keys (containing string values) and store the result in the destination key.
      See {!bit_operation} type for available operations. *)
  val bitop : connection -> bit_operation -> string -> string list -> int IO.t

  (** Count the number of set bits (population counting) in a string. *)
  val bitcount : ?first:int -> ?last:int -> connection -> string -> int IO.t

  (** Return the position of the first bit set to 1 or 0 in a string. *)
  val bitpos : ?first:int -> ?last:int -> connection -> string -> int -> int IO.t

  (** Decrements the number stored at key by one. If the key does not exist, it is set to 0 before performing the operation. *)
  val decr : connection -> string -> int IO.t

  (** Decrements the number stored at key by decrement. If the key does not exist, it is set to 0 before performing the operation. *)
  val decrby : connection -> string -> int -> int IO.t

  (** Get the value of key. *)
  val get : connection -> string -> string option IO.t

  (** Returns the substring of the string value stored at key, determined by the offsets start and end (both are inclusive). *)
  val getrange : connection -> string -> int -> int -> string option IO.t

  (** Atomically sets key to value and returns the old value stored at key. Returns [ None ] when key exists but does not hold a string value. *)
  val getset : connection -> string -> string -> string option IO.t

  (** Increments the number stored at key by one. If the key does not exist, it is set to 0 before performing the operation. *)
  val incr : connection -> string -> int IO.t

  (** Increments the number stored at key by increment. If the key does not exist, it is set to 0 before performing the operation. *)
  val incrby : connection -> string -> int -> int IO.t

  (** Increment the string representing a floating point number stored at key by the specified increment. If the key does not exist, it is set to 0 before performing the operation. *)
  val incrbyfloat : connection -> string -> float -> float IO.t

  (** Returns the values of all specified keys. *)
  val mget : connection -> string list -> string option list IO.t

  (** Sets the given keys to their respective values. *)
  val mset : connection -> (string * string) list -> unit IO.t

  (** Sets the given keys to their respective values. MSETNX will not perform any operation at all even if just a single key already exists. *)
  val msetnx : connection -> (string * string) list -> bool IO.t

  (** Set key to hold the string value. *)
  val set :
    connection ->
    ?ex:int -> ?px:int -> ?nx:bool -> ?xx:bool ->
    string -> string -> bool IO.t

  (** Set key to hold the string value and set key to timeout after a given number of seconds. *)
  val setex : connection -> string -> int -> string -> unit IO.t

  (** PSETEX works exactly like SETEX with the sole difference that the expire time is specified in milliseconds instead of seconds. *)
  val psetex : connection -> string -> int -> string -> unit IO.t

  (** Set key to hold string value if key does not exist. *)
  val setnx : connection -> string -> string -> bool IO.t

  (** Overwrites part of the string stored at key, starting at the specified offset, for the entire length of value. *)
  val setrange : connection -> string -> int -> string -> int IO.t

  (** Returns the length of the string value stored at key. An error is returned when key holds a non-string value. *)
  val strlen : connection -> string -> int IO.t

  (** {2 Hash commands} *)

  (** Removes the specified fields from the hash stored at key. Specified fields that do not exist within this hash are ignored. *)
  val hdel : connection -> string -> string -> bool IO.t

  (** Returns if field is an existing field in the hash stored at key. *)
  val hexists : connection -> string -> string -> bool IO.t

  (** Returns the value associated with field in the hash stored at key. *)
  val hget : connection -> string -> string -> string option IO.t

  (** Returns all fields and values of the hash stored at key. *)
  val hgetall : connection -> string -> (string * string) list IO.t

  (** Increments the number stored at field in the hash stored at key by increment. *)
  val hincrby : connection -> string -> string -> int -> int IO.t

  (** Increments the number stored at field in the hash stored at key by increment. *)
  val hincrbyfloat : connection -> string -> string -> float -> float IO.t

  (** Returns all field names in the hash stored at key. *)
  val hkeys : connection -> string -> string list IO.t

  (** Returns the number of fields contained in the hash stored at key. *)
  val hlen : connection -> string -> int IO.t

  (** Returns the values associated with the specified fields in the hash stored at key. *)
  val hmget : connection -> string -> string list -> string option list IO.t

  (** Sets the specified fields to their respective values in the hash stored at key. *)
  val hmset : connection -> string -> (string * string) list -> unit IO.t

  (** Sets field in the hash stored at key to value. *)
  val hset : connection -> string -> string -> string -> bool IO.t

  (** Sets field in the hash stored at key to value, only if field does not yet exist. *)
  val hsetnx : connection -> string -> string -> string -> bool IO.t

  (** Get the length of the value of a hash field *)
  val hstrlen : connection -> string -> string -> int IO.t

  (** Incrementally iterate hash fields and associated values *)
  val hscan : ?pattern:string -> ?count:int -> connection -> string -> int -> (int * (string * string) list) IO.t

  (** Returns all values in the hash stored at key. *)
  val hvals : connection -> string -> string list IO.t

  (** {2 List commands} *)

  (** Remove and get the first element in a list, or block until one is available *)
  val blpop : connection -> string list -> int -> (string * string) option IO.t

  (** Remove and get the last element in a list, or block until one is available *)
  val brpop : connection -> string list -> int -> (string * string) option IO.t

  (** Pop a value from a list, push it to another list and return it; or block until one is available *)
  val brpoplpush : connection -> string -> string -> int -> string option IO.t

  (** Get an element from a list by its index *)
  val lindex : connection -> string -> int -> string option IO.t

  (** Insert an element before or after another element in a list *)
  val linsert : connection -> string -> [< `After | `Before ] -> string -> string -> int option IO.t

  (** Get the length of a list *)
  val llen : connection -> string -> int IO.t

  (** Remove and get the first element in a list *)
  val lpop : connection -> string -> string option IO.t

  (** Prepend one or multiple values to a list *)
  val lpush : connection -> string -> string list -> int IO.t

  (** Prepend a value to a list, only if the list exists *)
  val lpushx : connection -> string -> string list -> int IO.t

  (** Get a range of elements from a list *)
  val lrange : connection -> string -> int -> int -> string list IO.t

  (** Remove elements from a list *)
  val lrem : connection -> string -> int -> string -> int IO.t

  (** Set the value of an element in a list by its index *)
  val lset : connection -> string -> int -> string -> unit IO.t

  (** Trim a list to the specified range *)
  val ltrim : connection -> string -> int -> int -> unit IO.t

  (** Remove and get the last element in a list *)
  val rpop : connection -> string -> string option IO.t

  (** Remove the last element in a list, prepend it to another list and return it *)
  val rpoplpush : connection -> string -> string -> string option IO.t

  (** Append one or multiple values to a list *)
  val rpush : connection -> string -> string list -> int IO.t

  (** Append a value to a list, only if the list exists *)
  val rpushx : connection -> string -> string list -> int IO.t

  val lmove : connection ->
    string -> string ->
    [`Left | `Right] -> [`Left | `Right] ->
      string option IO.t
  (** [lmove from into sidefrom sideinto] moves an element from [from]
      into [into], picking which side to pop/push based on the last arguments,
      and returns the element.
      @since 0.6
      since redis 6.2 *)

  val blmove : connection ->
    string -> string ->
    [`Left | `Right] -> [`Left | `Right] ->
    timeout:int ->
    string option IO.t
  (** same as {!lmove} but blocks for up to [timeout] seconds. *)

  (** {2 HyperLogLog commands} *)

  (** Adds values to the HyperLogLog data structure. *)
  val pfadd : connection -> string -> string list -> bool IO.t

  (** Returns the approximated cardinality of the union of the HyperLogLogs passed. *)
  val pfcount : connection -> string list -> int IO.t

  (** Merge multiple HyperLogLog values into an unique value that will approximate the cardinality of the union of the observed Sets of the source HyperLogLog structures. *)
  val pfmerge : connection -> string list -> unit IO.t

  (** {2 Set commands} *)

  (** Returns true if member was added, false otherwise. *)
  val sadd : connection -> string -> string -> bool IO.t

  val scard : connection -> string -> int IO.t

  (** Difference between first and all successive sets. *)
  val sdiff : connection -> string list -> string list IO.t

  (** like sdiff, but store result in destination. returns size of result. *)
  val sdiffstore : connection -> string -> string list -> int IO.t

  val sinter : connection -> string list -> string list IO.t

  (** Like SINTER, but store result in destination. Returns size of result. *)
  val sinterstore : connection -> string -> string list -> int IO.t

  val sismember : connection -> string -> string -> bool IO.t

  val smembers : connection -> string -> string list IO.t

  (** Returns true if an element was moved, false otherwise. *)
  val smove : connection -> string -> string -> string -> bool IO.t

  (** Remove random element from set. *)
  val spop : connection -> string -> string option IO.t

  (** Like SPOP, but doesn't remove chosen element. *)
  val srandmember : connection -> string -> string option IO.t

  (** Returns true if element was removed. *)
  val srem : connection -> string -> string -> bool IO.t

  val sunion : connection -> string list -> string list IO.t

  (** Like SUNION, but store result in destination. Returns size of result. *)
  val sunionstore : connection -> string -> string list -> int IO.t

  (** {2 Pub/sub commands} *)

  (** Post a message to a channel. Returns number of clients that received the message. *)
  val publish : connection -> string -> string -> int IO.t

  (** Lists the currently active channels. If no pattern is specified, all channels are listed. *)
  val pubsub_channels : connection -> string option -> reply list IO.t

  (** Returns the number of subscribers (not counting clients subscribed to patterns) for the specified channels. *)
  val pubsub_numsub : connection -> string list -> reply list IO.t

  (** Subscribes the client to the specified channels.

      From now on only pubsub related commands are accepted on the connection.
      Use {!stream} to read the stream the notifications (possibly-concurrently).
        Each notification in the form of [ ["message"; "<channel name>"; "<message>"] ] for
        subscribed messages, or [ ["subscribe"; <list of channel>] ] or [ ["unsubscribe"; <list of channels] ].
      Use {!reset} to exit the subscription state and issue other types of commands.
      See https://redis.io/docs/latest/develop/interact/pubsub/ for more details.
      *)
  val subscribe : connection -> string list -> unit IO.t

  (** Unsubscribes the client from the given channels, or from all of them if an empty list is given *)
  val unsubscribe : connection -> string list -> unit IO.t

  (** Subscribes the client to the given patterns. See {!subscribe} for more details. *)
  val psubscribe : connection -> string list -> unit IO.t

  (** Unsubscribes the client from the given patterns. *)
  val punsubscribe : connection -> string list -> unit IO.t

  (** {2 Sorted set commands} *)

  (** Add one or more members to a sorted set, or update its score if it already exists. *)
  val zadd : connection ->
    ?x:[< `NX | `XX ] -> ?ch:bool ->
    string -> (float * string) list -> int IO.t

  (** Return a range of members in a sorted set, by index. *)
  val zrange : connection -> ?withscores:bool -> string -> int -> int -> reply list IO.t

  (** Return a reversed range of members in a sorted set, by index. *)
  val zrevrange : connection -> ?withscores:bool -> string -> int -> int -> reply list IO.t

  (** Return a range of members in a sorted set, by score. *)
  val zrangebyscore : connection -> ?withscores:bool -> ?limit:(int * int) -> string -> FloatBound.t -> FloatBound.t -> reply list IO.t

  (** Return a range of members in a sorted set, by lexicographical range. *)
  val zrangebylex : connection -> ?limit:(int * int) -> string -> StringBound.t -> StringBound.t -> reply list IO.t

  (** Return a range of members in a sorted set, by score. *)
  val zrevrangebyscore : connection -> ?withscores:bool -> ?limit:(int * int) -> string -> FloatBound.t -> FloatBound.t -> reply list IO.t

  (** Return a range of members in a sorted set, by lexicographical range. *)
  val zrevrangebylex : connection -> ?limit:(int * int) -> string -> StringBound.t -> StringBound.t -> reply list IO.t

  (** Remove one or more members from a sorted set. *)
  val zrem : connection -> string -> string list -> int IO.t

  (** Remove all members in a sorted set between the given lexicographical range. *)
  val zremrangebylex : connection -> string -> StringBound.t -> StringBound.t -> int IO.t

  (** Remove all members in a sorted set between the given score range. *)
  val zremrangebyscore : connection -> string -> FloatBound.t -> FloatBound.t -> int IO.t

  (** Remove all members in a sorted set between the given rank range. *)
  val zremrangebyrank : connection -> string -> int -> int -> int IO.t

  (** Returns the sorted set cardinality (number of elements) of the sorted set stored at key. *)
  val zcard : connection -> string  -> int IO.t

  (** Increment the score of a member in the sorted set *)
  val zincrby : connection -> string -> float -> string -> float IO.t

  (** Returns the score of a member in the sorted set. *)
  val zscore : connection -> string -> string -> float option IO.t

  (** Returns the number of elements in the sorted set at key with a score between min and max. *)
  val zcount : connection -> string -> FloatBound.t -> FloatBound.t -> int IO.t

  (** Returns the number of members in a sorted set between a given lexicographical range. *)
  val zlexcount : connection -> string -> StringBound.t -> StringBound.t -> int IO.t

  (** Returns the rank of member in the sorted set stored at key. *)
  val zrank : connection -> string -> string -> int option IO.t

  (** Returns the reversed rank of member in the sorted set stored at key. *)
  val zrevrank : connection -> string -> string -> int option IO.t

  (** Removes and returns one or more members with the lowest scores in a sorted set. *)
  val zpopmin : connection -> string -> int -> (string * float) list IO.t

  (** Remove and return one or more members with the highest scores in a sorted set. *)
  val zpopmax : connection -> string -> int -> (string * float) list IO.t

  (** Remove and return the member with the lowest score in a sorted set, or block until one is available. *)
  val bzpopmin : connection -> string list -> float -> (string * string * float) option IO.t

  (** Remove and return the member with the highest score in a sorted set, or block until one is available. *)
  val bzpopmax : connection -> string list -> float -> (string * string * float) option IO.t

  (** {2 Stream commands}

      For redis >= 5. We only support a subset of the commands for now. *)

  (** Add a stream event, as a list of key-value pairs, to the given stream.
      @return the ID of the new event
      @param maxlen can be used to trim the stream.
      @param id specify a custom ID. Most of the of time you don't want to
        set this.

      @see {{: https://redis.io/commands/xadd } the official doc}
      @since 0.5 *)
  val xadd :
    connection ->
    string ->
    ?maxlen:[`Exact of int | `Approximate of int] ->
    ?id:string ->
    (string * string) list ->
    string IO.t

  (** Delete specific stream events. Should be rarely useful.
      @return the number of deleted events.

      @see {{: https://redis.io/commands/xdel } the official doc}
      @since 0.5 *)
  val xdel :
    connection ->
    string ->
    string list ->
    int IO.t

  (** Length of a stream.
      @see https://redis.io/commands/xlen .
      @since 0.5 *)
  val xlen : connection -> string -> int IO.t

  (** Trim stream to the given maximum length.
      @param maxlen the maximum number of entries to preserve, prioritizing
        the most recent ones. [`Approximate n] is faster, and should be preferred.
      @return number of deleted entries

      @see {{: https://redis.io/commands/xtrim } the official doc}
      @since 0.5 *)
  val xtrim :
    connection -> string ->
    maxlen:[`Exact of int | `Approximate of int] ->
    unit -> int IO.t

  (** A stream event as returned by Redis.
      It is composed of a stream ID (timestamp + counter),
      and a list of key/value pairs.
      @since 0.5 *)
  type stream_event = string * (string * string) list

  (** [xrange connection stream ~start ~end_ ()] returns a range of
      events in the stream.

      @param start beginning of the range. It can be one of:

        - [StringBound.NegInfinity] ("-" in the doc) to indicate the earliest possible time
        - [StringBound.Inclusive "timestamp"] or [StringBound.Inclusive "timestamp-number"]
          for a left-inclusive bound
        - [StringBound.Exclusive "timestamp"] or [StringBound.Exclusive "timestamp-number"]
          for a left-exclusive bound ("(" in the doc)
          only since Redis 6.2

      @param end_ same as start but for the right bound
      @param count maximum number of events returned

      @return a lits of events (at most [count] if specified). Each event is
        a pair [(id, pairs)] where [id] is the unique ID of the event,
        of the form "<timestamp>-<counter>", and [pairs] is a list of
        key-value pairs associated with the event.

      @see {{: https://redis.io/commands/xrange} the official doc}
      @since 0.5 *)
  val xrange :
    connection ->
    string ->
    start:StringBound.t ->
    end_:StringBound.t ->
    ?count:int ->
    unit ->
    stream_event list IO.t

  (** Like {!xrange} but in reverse order.
      @see {{: https://redis.io/commands/xrevrange } the official doc}
      @since 0.5 *)
  val xrevrange :
    connection ->
    string ->
    start:StringBound.t ->
    end_:StringBound.t ->
    ?count:int ->
    unit ->
    stream_event list IO.t

  (** [xread connection pairs] reads data from the multiple streams
      specified in [pairs].

      Each item of [pairs] is a pair [("stream-name", <after>)] where
      [<after>] is either:

        - [`Last] ("$" in the doc) to get events coming after
          the last current event (so, new events);
        - or [`After i] to get events coming after the given ID [i],
          excluding [i] itself.

      @return a list of [("stream-name", <events>)].
      Each pair contains the name of a stream (that was among the
      input [pairs]), along with events of that stream coming after the
      corresponding position.

      @param count max number of events returned {b per stream}
      @param block_ms if provided, [xread] blocks at most [block_ms] milliseconds
        for new events. Otherwise [xread] is synchronous and returns immediately.

      @see {{: https://redis.io/commands/xread} the official doc}
      @since 0.5 *)
  val xread :
    connection ->
    ?count:int ->
    ?block_ms:int ->
    (string * [`Last | `After of string]) list ->
    (string * stream_event list) list IO.t

  (** {2 Transaction commands} *)

  (** Marks the start of a transaction block. Subsequent commands will be queued for atomic execution using EXEC. *)
  val multi : connection -> unit IO.t

  (** Executes all previously queued commands in a transaction and restores the connection state to normal. *)
  val exec : connection -> reply list IO.t

  (** Flushes all previously queued commands in a transaction and restores the connection state to normal. *)
  val discard : connection -> unit IO.t

  (** Marks the given keys to be watched for conditional execution of a transaction. *)
  val watch : connection -> string list -> unit IO.t

  (** Flushes all the previously watched keys for a transaction. *)
  val unwatch : connection -> unit IO.t

  val queue : (unit -> 'a IO.t) -> unit IO.t
  (** Within a transaction (see {!multi}, {!exec}, and {!discard}),
      commands will not return their normal value. It is necessary to
      wrap each of them in their individual [Client.queue (fun () -> the_command)]
      to avoid getting an exception [Unexpected (Status "QUEUED")].
  *)

  (** {2 Scripting commands} *)

  (** Load the specified Lua script into the script cache. Returns the SHA1 digest of the script for use with EVALSHA. *)
  val script_load : connection -> string -> string IO.t

  (** Evaluates a script using the built-in Lua interpreter. *)
  val eval : connection -> string -> string list -> string list -> reply IO.t

  (** Evaluates a script cached on the server side by its SHA1 digest. *)
  val evalsha : connection -> string -> string list -> string list -> reply IO.t

  (** {2 Server} *)

  val bgrewriteaof : connection -> unit IO.t

  val bgsave : connection -> unit IO.t

  val config_resetstat : connection -> unit IO.t

  val dbsize : connection -> int IO.t

  (** clear all databases *)
  val flushall : connection -> unit IO.t

  (** clear current database *)
  val flushdb : connection -> unit IO.t

  val info : connection -> (string * string) list IO.t

  (** last successful save as Unix timestamp *)
  val lastsave : connection -> float IO.t

  (** role in context of replication *)
  val role : connection -> reply list IO.t

  (** synchronous save *)
  val save : connection -> unit IO.t

  (** save and shutdown server *)
  val shutdown : connection -> unit IO.t

  (** Batch commands for mass insertion *)
  module MassInsert : sig
    type command

    val empty : command

    val set : ?ex:int -> ?px:int -> ?nx:bool -> ?xx:bool -> string -> string -> command

    (** Delete a key; returns the number of keys removed. *)
    val del : string list -> command

    val expire : string -> int -> command

    val hset : string -> string -> string -> command

    (** Removes the specified fields from the hash stored at key. Specified fields that do not exist within this hash are ignored. *)
    val hdel : string -> string -> command

    val hget : string -> string -> command

    val hincrby : string -> string -> int -> command

    val write :
      connection ->
      command list ->
      reply list IO.t

    val incr : string -> command

    val decr : string -> command
  end
end

module type Cache_params = sig
  type key
  type data

  val cache_key : key -> string
  val cache_expiration : int option

  val data_of_string : string -> data
  val string_of_data : data -> string
end

module type Cache = sig
  module IO : IO
  module Client : Client
  module Params : Cache_params

  val set : Client.connection -> Params.key -> Params.data -> unit IO.t
  val get : Client.connection -> Params.key -> Params.data option IO.t
  val delete : Client.connection -> Params.key -> unit
end

module type Mutex = sig
  module IO : IO
  module Client : Client

  exception Error of string

  val acquire : Client.connection -> ?atime:float -> ?ltime:int -> string -> string -> unit IO.t
  val release : Client.connection -> string -> string -> unit IO.t
  val with_mutex : Client.connection -> ?atime:float -> ?ltime:int -> string -> (unit -> 'a IO.t) -> 'a IO.t
end

(** {2 Connection pool} *)
module type POOL = sig
  module IO : IO
  module Client : Client

  type t

  val size : t -> int

  val create : size:int -> Client.connection_spec -> t IO.t
  (** Create a pool of [size] connections, using the given spec. *)

  val close : t -> unit IO.t
  (** Close all connections *)

  val with_pool : size:int -> Client.connection_spec -> (t -> 'a IO.t) -> 'a IO.t
  (** Create a pool of [size] connections, using the given spec,
      pass it to the callback, and then destroy it. *)

  val with_connection : t -> (Client.connection -> 'a IO.t) -> 'a IO.t
  (** Temporarily require a connection to perform some operation.
      The connection must not escape the scope of the callback *)
end
