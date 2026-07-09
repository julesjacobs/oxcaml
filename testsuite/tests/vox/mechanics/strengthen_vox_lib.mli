(* A module whose signature carries a vox-refined abstract type (int-sorted)
   and a refined value contract.  A [module A = M] alias STRENGTHENS this
   (mtype.ml), which must preserve both the sort (in the jkind) and the value
   contract (Sig_value is passed through untouched). *)
module M : sig
  type t [@@vox.sort int]
  val mk : (n : int) -> t
  val get : (x : t) -> int{ _ = 0 -> true }
end
