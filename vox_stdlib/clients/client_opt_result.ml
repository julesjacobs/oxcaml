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
  let r = Vresult.Vok x in
  let v = Vresult.get_ok_or d r in
  let o = Voption.Vsome v in
  Voption.get_or 0 o

let ok_and_some (x : int) : bool{ _ = true } =
  let r = Vresult.Vok x in
  let ok = Vresult.is_ok r in
  let o = Voption.Vsome x in
  let some = Voption.is_some o in
  ok && some
