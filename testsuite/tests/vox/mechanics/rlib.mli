(* The refines kind must survive the .cmi: refines_cmi.ml is compiled
   against this interface's cmi. *)
type t : value refines int

val zero : t{ _ = 0 }
