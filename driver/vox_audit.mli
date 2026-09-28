(** Reading the Vox records of compiled units, and [-vox-audit]. *)

(** The records that earlier compilations left in the .cmo and .cmi files of
    this unit (see [Vox_trust.counterparts]). *)
val counterparts : Unit_info.t -> unit -> Cmi_format.vox_unit list

(** Print what the verification of the unit just compiled trusts, and what
    the units it depends on trust, found through the load path. *)
val print_unit :
  source_file:string -> current:string -> record:Cmi_format.vox_unit option
  -> unit
