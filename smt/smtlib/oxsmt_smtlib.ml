(** SMT-LIB2 interchange — the SHIPPED printer.

    Responsibility: serialize every session to SMT-LIB2 ([set-logic QF_UFLIA],
    declarations, assertions, [check-sat]) as the stable interchange format for the oracle
    and public benchmark corpora (DESIGN.md §3). Stdlib-only, over {!Oxsmt_core}.

    The reverse direction — a parser from SMT-LIB2 text into frozen-API terms — is
    {b test-only} and lives in a {e separate} library ([oxsmt_smtlib_parser]) so that no
    parser code can ever be linked into the shipped compiler artifact (DESIGN.md §3;
    AGENTS.md). This library exposes only the printer.

    Owning task: TASKS.md M0-smtlib. *)

module Status = Status
module Printer = Printer
