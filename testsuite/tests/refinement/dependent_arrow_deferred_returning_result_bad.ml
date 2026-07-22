let value =
  (Dependent_arrow_deferred_provider.returning_partial ~x:1
    : int{ _ = 2 })
