module Provider = Refinement_program_scope_seal_provider

module Rejected : sig
  val field : int{ _ = Provider.A.field }
end = struct
  let field : int{ _ = Provider.B.field } = Provider.B.field
end
