(** External declarations whose refinement or totality the verifier assumes
    rather than checks. *)

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
