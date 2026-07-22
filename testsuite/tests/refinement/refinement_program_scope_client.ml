let imported =
  Refinement_program_scope_provider.consume
    Refinement_program_scope_provider.stable_value

let dependent = Refinement_program_scope_provider.dependent 4

let nested =
  Refinement_program_scope_provider.Inner.consume
    Refinement_program_scope_provider.Inner.stable_value

module First = Refinement_program_scope_provider.Make ()
module Second = Refinement_program_scope_provider.Make ()

let first = First.consume First.stable_value
let second = Second.consume Second.stable_value
