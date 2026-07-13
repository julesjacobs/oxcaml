(* One-word tagged representation for {!Rational}. See rational_word.mli.

   [t] is [Obj.t]: physically either an immediate tagged [int] (an integer value with den
   = 1 that fits int63) or a pointer to a {!block} record. The two are told apart by the
   tag bit ([Obj.is_int]) — the same immediate-or-pointer discipline OCaml uses for a
   variant with constant and non-constant constructors, so the GC scans it correctly.

   Soundness of the cast (user hard constraint 1): the conversions are the identity at the
   representation level ([Obj.repr]/[Obj.obj] compile to no code). They are sound because
   every value we ever [to_int_unchecked]/[to_block] was built by the matching
   [of_int_unchecked]/[of_block] and is gated on [is_immediate] at the one client
   ({!Rational}); the physical form therefore always matches the projection. We keep [t]
   abstract as [Obj.t] rather than a concrete int|block union so no other module — and no
   flambda2 approximation crossing the module boundary — can assume a uniform layout and
   compile the tag discrimination away (build-oxcaml is the standing check). The functions
   are one-instruction and left inlinable on purpose: the whole point is zero-alloc,
   zero-call integer arithmetic in the client. *)

type t = Obj.t

type block =
  { num : Oxsmt_core.Bigint.t
  ; den : Oxsmt_core.Bigint.t
  }

let is_immediate (x : t) = Obj.is_int x
let of_int_unchecked (n : int) : t = Obj.repr n
let to_int_unchecked (x : t) : int = (Obj.obj x : int)
let of_block (b : block) : t = Obj.repr b
let to_block (x : t) : block = (Obj.obj x : block)
