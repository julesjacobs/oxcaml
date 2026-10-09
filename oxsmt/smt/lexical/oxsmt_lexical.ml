(** The shared SMT-LIB 2.6 lexer (ADR-0008). Stdlib-only; linkable by both the shipped
    {!Oxsmt_smtlib} printer and — after task/gate3 — the gate reader. Scope is strictly
    lexical: it emits {!Lexer.token}s and preserves token kind exactly. *)

module Lexer = Lexer
