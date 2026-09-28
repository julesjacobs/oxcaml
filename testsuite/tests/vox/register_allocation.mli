open Register_allocation_spec

val allocate : program -> int -> allocation option @@ total

val preserves :
  (program : program) -> (physical : int) ->
  (args : int list) -> (fuel : fuel) ->
  {u : unit |
    match allocate program physical with
    | None -> true
    | Some allocation ->
      not (same_shape program.inputs args)
      || observable_equal
           (advance program.code fuel (source_initial program args))
           (advance allocation.code fuel (initial_of_allocation allocation args))}
  @ ghost @@ total

val allocation_domain :
  (program : program) -> (physical : int) ->
  {u : unit |
    match allocate program physical with
    | None -> true
    | Some allocation ->
      valid program && 0 < physical && physical <= 32
      && allocation.physical = physical
      && allocation.source_registers = program.registers
      && allocation.source_inputs === program.inputs}
  @ ghost @@ total
