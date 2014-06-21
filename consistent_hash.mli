module type DIGEST =
sig
  type t = string
  val string: string -> t
end

module Make: functor (Digest: DIGEST) ->
sig
  type +'a t
  val make: ?interleave_count:int -> unit -> 'a t
  val add: ?weight:int -> string -> 'a -> 'a t -> 'a t
  val find: string -> 'a t -> 'a
  val iter: (int64 -> string -> 'a -> unit) -> 'a t -> unit
  val hash: string -> int64
end
