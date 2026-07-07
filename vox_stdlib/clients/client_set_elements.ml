(* Wave-3 F-2 ACCEPTANCE client (integrator-owned): demonstrates the
   eliminator that F-2 said was impossible — enumerating a via-abstract set
   back to concrete data. roundtrip forces BOTH Vset's vs_addspec (x is in the
   set after add) AND Vset's vs_elements_spec (elements agrees with membership)
   AND Vlist's ll_mem, cross-unit: after add x, x is found in the enumerated
   Vlist. This is the capability the reviewer's dedup task needs; verified
   against Vset + Vlist + Vset_bst artifacts, sources deleted. All dependent
   args are let-bound (C1). *)

let roundtrip (x : int) (s : Vset.t) : bool{ _ = true } =
  let s' = Vset.add x s in          (* vs_addspec s' x s : vs_mem x s' = true *)
  let es = Vset.elements s' in      (* vs_elements_spec es s' : ll_mem x es = vs_mem x s' *)
  Vlist.mem x es                    (* = ll_mem x es = vs_mem x s' = true *)
