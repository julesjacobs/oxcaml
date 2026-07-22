(* Direct and re-exported copies have independently freshened arrow binders but
   remain alpha-equivalent, so the branch join succeeds. *)
let pick flag = if flag then Cup_provider.identity else Cup_reexport.rident
