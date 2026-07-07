(* Wave-3 cross-module client: Voption + Vresult composition (integrator-owned).
   Each function's goal is discharged only by BOTH modules' shipped algebra
   firing cross-unit, verified against voption.cmi + VoxSig_Voption.olean and
   vresult.cmi + VoxSig_Vresult.olean (no sources). Constructor arguments to
   dependent ops are let-bound first (the C1 workaround the module notes flag).

   - ok_then_some CHAINS the two algebras through a value: unwrap a Vresult.Ok
     (vr_get_ok_ok makes v = x), rewrap as a Voption.Some, unwrap
     (vo_get_or_some makes the result = v). The goal `_ = x` needs BOTH laws:
     drop vr_get_ok_ok and v is opaque; drop vo_get_or_some and the result
     is not v.
   - ok_and_some CONJOINS the two predicate laws: is_ok (.Vok x) is true by
     vr_is_ok_ok and is_some (.Vsome x) is true by vo_is_some_some; `&&` forces
     both. *)

let ok_then_some (d : int) (x : int) : int{ _ = x } =
  (* boundary (C1 KEPT + tagged): this whole chain must stay fully let-bound.
     Every inline variant DISPROVES on the #53 compiler -- even inlining just
     [Vok x] into get_ok_or (which verifies standalone in smoke_vresult) makes
     the goal DISPROVED once that result is let-bound and fed downstream into
     [Vsome v] -> get_or. The constructor-wrapping-a-tier-2-call boundary
     (team-lead's f (Vsome (add k m)) shape) is real here. See LANGUAGE_NEEDS. *)
  let r = Vresult.Vok x in
  let v = Vresult.get_ok_or d r in
  let o = Voption.Vsome v in
  Voption.get_or 0 o

let ok_and_some (x : int) : bool{ _ = true } =
  (* C1 REMOVED: the Vok x / Vsome x constructor args now inline into
     is_ok / is_some (post-#53). But an && OPERAND that is a dependent-arg call
     must still be let-bound to a bool first: inlining both calls directly as
     the two && operands is DISPROVED (a #53 boundary distinct from C1 -- an
     inline dependent-arg call feeding &&; see LANGUAGE_NEEDS). *)
  let ok = Vresult.is_ok (Vresult.Vok x) in
  let some = Voption.is_some (Voption.Vsome x) in
  ok && some
