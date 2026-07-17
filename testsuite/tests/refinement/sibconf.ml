module type SIG = sig
  val base : int
  val f : int{ _ = base } -> int
  val g : unit -> int{ _ = base }
end

(* X.f requires arg = X.base; Y.g () delivers Y.base.  X.base and Y.base are
   independent values, so this is well-typed only if the sibling "base" from X's
   and Y's signatures conflate.  Must be rejected. *)
module F (X : SIG) (Y : SIG) = struct
  let bad = X.f (Y.g ())
end
