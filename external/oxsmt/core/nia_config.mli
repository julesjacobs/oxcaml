(** Dark process-wide gate for nonlinear integer arithmetic (QF_NIA). Reads [OXSMT_NIA] at
    most once; defaults OFF so an unset environment is byte-identical to trunk. *)
val enabled : unit -> bool

(** The reserved function-symbol name abstracting a nonlinear integer product
    [(.oxsmt.nia.mul a b) : Int]. See the [.ml] for the abstraction's soundness argument. *)
val mul_name : string

(** [is_mul_name n] is [n = mul_name] — the marker predicate used by the parse-time minter
    admit gate, the reserved-symbol exemption, and {!Model_check}'s real-multiplication
    re-evaluation. *)
val is_mul_name : string -> bool
