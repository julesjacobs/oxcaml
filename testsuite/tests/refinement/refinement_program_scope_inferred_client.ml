let imported =
  Refinement_program_scope_inferred_provider.consume
    Refinement_program_scope_inferred_provider.witness

let hierarchical =
  Refinement_program_scope_inferred_provider.Inner.consume_outer
    Refinement_program_scope_inferred_provider.Inner.outer_witness

module First = Refinement_program_scope_inferred_provider.Make ()
module Second = Refinement_program_scope_inferred_provider.Make ()

let first = First.consume First.witness
let second = Second.consume Second.witness

let first_nested =
  First.Nested.consume_outer First.Nested.outer_witness

let second_nested =
  Second.Nested.consume_outer Second.Nested.outer_witness
