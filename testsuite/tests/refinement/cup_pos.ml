(* Cross-unit dependent results that SHOULD verify: the imported parameter is
   substituted by the actual argument (arity-1). *)
let good3 : int{ _ = 3 } = Cup_provider.identity 3
let echo (x : int) : int{ _ = x } = Cup_provider.identity x
let six : int{ _ = 6 } = Cup_provider.add1 5
