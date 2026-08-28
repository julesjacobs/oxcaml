type t = Int | Bool

(** Expand transparent aliases, but leave polymorphic and refinement wrappers
    to the caller's acceptance policy. *)
val classify : Env.t -> Types.type_expr -> t option

(** Also unwrap empty polymorphic and refinement wrappers. *)
val classify_payload : Env.t -> Types.type_expr -> t option
