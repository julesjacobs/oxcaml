(** Reading the Vox records of compiled units, and [-vox-audit]. *)

(** Print what the verification of the unit just compiled trusts, and what
    the units it depends on trust, found through the load path. *)
val print_unit :
  source_file:string -> current:string -> record:Cmi_format.vox_unit option
  -> unit
