val project : int -> int @@ total logical [@@vox.spec_only]

module Let_only : sig
  val ( let* ) : 'a -> ('a -> 'b) -> 'b [@@vox.spec_only]
end

module And_only : sig
  val ( let* ) : 'a -> ('a -> 'b) -> 'b
  val ( and* ) : 'a -> 'b -> 'a * 'b [@@vox.spec_only]
end

module Ordinary : sig
  val ( let* ) : 'a -> ('a -> 'b) -> 'b
  val ( and* ) : 'a -> 'b -> 'a * 'b
end
