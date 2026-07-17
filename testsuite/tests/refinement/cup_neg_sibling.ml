(* (b)-arity-1 witness (sibling): [Cup_provider.fk 0] returns the sibling [k] = 5,
   not its argument.  A naive "sole free local = the parameter" substitution would
   replace [k] by the argument [0] and PROVE the false [_ = 0].  Must be
   REJECTED: the sibling reference stays opaque, so the claim cannot be proved. *)
let bad : int{ _ = 0 } = Cup_provider.fk 0
