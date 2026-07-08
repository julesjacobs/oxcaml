(* Wave-3 F-2 ACCEPTANCE client (integrator-owned): demonstrates the
   eliminator that F-2 said was impossible — enumerating a via-abstract set
   back to concrete data. roundtrip forces BOTH Vset's vs_addspec (x is in the
   set after add) AND Vset's vs_elements_spec (elements agrees with membership)
   AND Vlist's ll_mem, cross-unit: after add x, x is found in the enumerated
   Vlist. This is the capability the reviewer's dedup task needs; verified
   against Vset + Vlist + Vset_bst artifacts, sources deleted. The whole
   pipeline is one nested expression -- no let-bind (a depth-3 chain: the
   [Vset.add] fact threads out from two levels inside the enumeration). *)

let roundtrip (x : int) (s : Vset.t) : bool{ _ = true } =
  Vlist.mem x (Vset.elements (Vset.add x s))
