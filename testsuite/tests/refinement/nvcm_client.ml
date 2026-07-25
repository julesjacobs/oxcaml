(* Without the import check this compiles under full verification, proves
   [_ = 0] from the provider's unchecked claim, and links to a program that
   prints 1. *)
let check : int{ _ = 0 } = Nvcm_provider.value
