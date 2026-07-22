module Provider = Refinement_program_scope_inferred_provider

let rejected = Provider.Left.consume Provider.Right.witness
