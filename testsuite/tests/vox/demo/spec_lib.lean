-- Spec functions for lean_spec.ml: measures and predicates over the
-- module's datatypes (named Vox_<Unit>_<type>), unfolded by grind via
-- the equation lemmas that @[grind] registers.
@[grind] def len : Vox_Lean_spec_ilist -> Int
  | .Nil => 0
  | .Cons _ t => 1 + len t

@[grind] def mem (x : Int) : Vox_Lean_spec_ilist -> Prop
  | .Nil => False
  | .Cons h t => x = h ∨ mem x t
