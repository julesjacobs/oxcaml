(* Wave-3 cross-module client: Voption + Vresult composition (integrator-owned).
   Each function's goal is discharged only by BOTH modules' shipped algebra
   firing cross-unit, verified against voption.cmi + VoxSig_Voption.olean and
   vresult.cmi + VoxSig_Vresult.olean (no sources). Constructor arguments flow
   directly into the dependent ops (nested refined expressions -- no let-bind).

   - ok_then_some CHAINS the two algebras through a value: unwrap a Vresult.Ok
     (vr_get_ok_ok makes v = x), rewrap as a Voption.Some, unwrap
     (vo_get_or_some makes the result = v). The goal `_ = x` needs BOTH laws:
     drop vr_get_ok_ok and v is opaque; drop vo_get_or_some and the result
     is not v.
   - ok_and_some CONJOINS the two predicate laws: is_ok (.Vok x) is true by
     vr_is_ok_ok and is_some (.Vsome x) is true by vo_is_some_some; `&&` forces
     both. *)

let ok_then_some (d : int) (x : int) : int{ _ = x } =
  (* the whole chain is one nested expression -- the two algebras compose
     through the un-named intermediate values (the constructor-wrapping-a-call
     shape that previously had to be fully let-bound; nested refined
     expressions thread each stage's fact out). *)
  Voption.get_or 0 (Voption.Vsome (Vresult.get_ok_or d (Vresult.Vok x)))

let ok_and_some (x : int) : bool{ _ = true } =
  (* both && operands are dependent-arg calls on inline constructors; each
     operand's spec fact is exposed at its name (decompose_bool) so the
     conjunction closes with no let-bind. *)
  Vresult.is_ok (Vresult.Vok x) && Voption.is_some (Voption.Vsome x)
