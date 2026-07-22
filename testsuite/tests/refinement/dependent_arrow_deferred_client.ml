let () = Dependent_arrow_deferred_provider.partial ~x:1
let returning : int{ _ = 1 } =
  Dependent_arrow_deferred_provider.returning_partial ~x:1
