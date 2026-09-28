type t = Int | Bool | Bigint

val bigint_path : string -> Path.t

(** Expand transparent aliases, but leave polymorphic and refinement wrappers
    to the caller's acceptance policy. *)
val classify : Env.t -> Types.type_expr -> t option

(** Also unwrap empty polymorphic and refinement wrappers. *)
val classify_payload : Env.t -> Types.type_expr -> t option

(** Whether a primitive is a C primitive to which the checker gives a
    built-in meaning. *)
val is_builtin_c_primitive : Primitive.description -> bool

(** Whether the declaration with this UID of this primitive carries the
    primitive's built-in meaning, if it has one: only the library's own
    declarations do, with the native name the library gives. Always true for
    other primitives. *)
val carries_builtin_meaning : Shape.Uid.t -> Primitive.description -> bool
