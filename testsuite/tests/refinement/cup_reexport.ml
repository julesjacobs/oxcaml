(* Re-exports the provider's refined function.  Loading the provider here
   freshens its parameter reference with THIS unit's fresh stamp, distinct from
   the stamp a direct importer of the provider gets. *)
let rident = Cup_provider.identity
