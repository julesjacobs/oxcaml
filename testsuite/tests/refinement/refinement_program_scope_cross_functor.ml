module Provider = Refinement_program_scope_inferred_provider
module First = Provider.Make ()
module Second = Provider.Make ()

let rejected = First.consume Second.witness
