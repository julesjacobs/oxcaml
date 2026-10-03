(** What the verification of a Vox unit trusts rather than checks: trusted
    external declarations, totality casts, unsafe features and skipped
    verification. The compiler records these in each unit's .cmi, .cmo and
    .cmx files ([Cmi_format.vox_unit]), and [-vox-audit] prints them for a
    unit and the units it depends on. *)

(** {1 Trusted externals} *)

(** [-vox-library]: the unit is part of the verified Vox library or the
    standard library, whose trusted declarations are reviewed as part of the
    trusted base and are not warned about. *)
val library : bool ref

(** The command-line flags of this module. Registered once. *)
val add_arguments : unit -> unit

type external_kind =
  | Refined  (** its type mentions a refinement *)
  | Total  (** it provides a value declared total *)
  | Total_cast  (** a [%identity] cast whose result is total *)

(** Why an [external] is trusted by the verifier, if it is. *)
val trusted_external :
  Env.t -> Typedtree.value_description -> external_kind option

(** Warn about an [external] that is trusted by the verifier, unless the unit
    is compiled with [-vox-library]. *)
val check_external : Env.t -> Typedtree.value_description -> unit

(** {1 The record of a unit} *)

(** [-vox-audit] *)
val audit : bool ref

(** [-smt-assume-verified]: the verifier skips this unit. *)
val assume_verified : bool ref

(** Set by the verifier to the version of the solver it used when that is not
    the expected one ([-smt-solver-any-version]). *)
val unexpected_solver : string ref

(** Record the unit just type-checked and verified, and warn if it imports an
    interface whose verification was skipped. Does nothing without
    [-extension refinement_types]. *)
val record_implementation :
  source_file:string -> ast:Parsetree.structure -> Typedtree.structure -> unit

(** The settings that change how a unit is typed or what verification may
    rely on: [-noassert], [-unsafe], [-nopervasives], [-rectypes], [-open]
    and the language extensions. *)
val config : unit -> string

(** The record of the last implementation, for its .cmi, .cmo and .cmx. *)
val implementation_record : Cmi_format.vox_unit option ref

(** The record of an interface, for its .cmi. *)
val interface_record :
  source_file:string -> Typedtree.signature -> Cmi_format.vox_unit option

(** The record in a compiled interface file, if any. *)
val interface_file_record : string -> Cmi_format.vox_unit option

(** The record of a pack, from its members' names and records. *)
val pack_record :
  (string * Cmi_format.vox_unit option) list -> Cmi_format.vox_unit option

(** {1 Skipped verification} *)

(** Set by the driver: the records of this unit that earlier compilations
    left in its output files. A compilation with [-smt-assume-verified] is
    recorded as verified if one of them is a verified compilation of the same
    program, with the same flags, against the same interfaces, as the native
    half of the library build has in the bytecode half. *)
val counterparts : (unit -> Cmi_format.vox_unit list) ref

(** Forget the previous unit's record, solver and counterparts. *)
val reset : unit -> unit
