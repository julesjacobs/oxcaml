(** SMT-LIB2 parser — TEST-ONLY, never linked into the shipped compiler (DESIGN.md §3;
    AGENTS.md). It ingests the public benchmark corpora and round-trips the
    {!Oxsmt_smtlib} printer's dumps back into frozen-API terms.

    This is a {e separate} library from the shipped printer precisely so the parser cannot
    end up in a shipped artifact: shipped code depends on [oxsmt_smtlib], never on this
    library. *)

module Sexp = Sexp
module Parser = Parser
