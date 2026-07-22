module Provider = Refinement_program_scope_qualified_provider

let imported : int{ _ = Provider.Global.x } = Provider.value

let inherited : int{ _ = Provider.outer } = Provider.Nested.inherited

let sibling : int{ _ = Provider.Nested.base } = Provider.Nested.sibling
