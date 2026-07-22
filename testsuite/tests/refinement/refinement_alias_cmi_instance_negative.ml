module Provider = Refinement_alias_cmi_provider
module A = Provider.Make (struct end)
module B = Provider.Make (struct end)

(* Functor-result declaration UIDs may be shared, but sibling facts from two
   applications are not interchangeable. *)
let use_wrong_instance () : unit{ B.p = true } =
  A.proof;
  ()

