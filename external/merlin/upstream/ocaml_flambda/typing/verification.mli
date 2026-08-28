val install : (Typedtree.structure -> unit) -> unit

(** Run after delayed typing checks and before emitting artifacts.
    Without a verifier, reject static refinement introductions. *)
val run : Typedtree.structure -> unit
