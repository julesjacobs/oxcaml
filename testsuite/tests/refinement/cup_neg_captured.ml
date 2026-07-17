(* (b)-arity-1 witness (captured local): [Cup_provider.capf 0] returns the
   captured [cap] = 7, not its argument.  Same failure mode as the sibling case;
   must be REJECTED. *)
let bad : int{ _ = 0 } = Cup_provider.capf 0
