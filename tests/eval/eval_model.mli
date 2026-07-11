open Oxsmt_core

(** Reader for the model sidecar (task #74, format documented in tests/README "SAT models"
    and the gate's [.model] shape). A model assigns:

    - each nullary symbol a value: [(const x 3)], [(const p true)], [(const a 0)] (the
      last an element index of an uninterpreted sort);
    - each function symbol a finite table with a mandatory default:
      [(fun f (default 0) (case (0) 0) (case (1) 0))];
    - optionally, uninterpreted-sort cardinalities: [(sort S 2)] (recorded for range
      validation; a total evaluator does not otherwise need it).

    Tokens are typed against each symbol's {b declared} sort/rank (from {!Reader.Decls}),
    so a bare numeral becomes an [Int], an element index, or is rejected, exactly as the
    declaration dictates. Anything malformed is a loud {!Malformed}. *)

type fun_table =
  { default : Value.t
  ; cases : (Value.t list * Value.t) list (* arg-tuple ↦ result; first match wins *)
  }

type t

(** Ill-formed model syntax, a value that does not match its symbol's declared sort, or an
    out-of-range element index. *)
exception Malformed of string

val of_string : Reader.Decls.t -> string -> t
val of_file : Reader.Decls.t -> string -> t

(** Value bound to a nullary symbol, if the model defines it. *)
val lookup_const : t -> Symbol.t -> Value.t option

(** Table bound to a function symbol, if the model defines it. *)
val lookup_fun : t -> Symbol.t -> fun_table option
