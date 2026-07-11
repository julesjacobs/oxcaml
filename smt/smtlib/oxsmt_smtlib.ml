(** SMT-LIB2 interchange: printer (shipped) and parser (test-only).

    Responsibility: serialize every session to SMT-LIB2 ([set-logic QF_UFLIA],
    declarations, assertions, [check-sat]) as the stable interchange format for
    the oracle and public benchmark corpora. The parser exists only to ingest
    benchmarks and round-trip our own dumps — it is NEVER linked into the
    compiler (DESIGN.md §3).

    Status: skeleton. Owning task: TASKS.md M0-smtlib. *)
