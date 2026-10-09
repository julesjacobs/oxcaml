# Goals

## E-graph architecture

Theories talk to each other through the e-graph, not through the SAT solver.
Done when:

- A theory can add an equality to the e-graph directly, reason attached;
  merges notify the other theories — no clause, no fresh boolean variable, no
  restart.
- An e-class can carry theory data (an arithmetic value, a constructor tag),
  so theories read state off the class.
- The families that stalled on equality-via-clauses (trichotomy blowups, wisa)
  solve natively.
- Guesses still enter as SAT decisions; only entailed equalities ride the
  e-graph; every propagated equality replays in the certificate checker.
- Datatypes gets built as an e-graph client with no change to the SAT core —
  that build is this goal's acceptance test.

## Datatypes

The solver reasons about OCaml-style variants natively. Done when:

- Mutually recursive datatypes declare with constructors, selectors, and
  testers, and all four rules work: different constructors conflict; same
  constructor propagates field equalities; selectors evaluate once the
  constructor is known; cycles refute (x = cons(h, x) is unsat).
- Case splits fire when a term's constructor matters, including enums — four
  pairwise-distinct values of a three-constructor type is unsat.
- Integer fields flow into arithmetic and back (head(x) < len(y) works).
- Every sat answer includes a model with actual constructor trees that the
  evaluator checks; every unsat replays in the certificate checker.
- Results on the public QF_DT and QF_UFDT benchmark sets, with their time
  limit stated.

## Solve rate and speed

Competitive on the benchmark corpus, instant on small queries. Done when:

- Within 10 points of Z3 on the 21,468-file corpus at the same 2-second
  limit, with zero disagreements against the benchmark labels.
- Integer literals of any size work — the overflow ceiling and the last 76
  unparseable files are gone.
- A small query — a dozen assertions in the quantifier-free fragment —
  answers in under 100 microseconds, in-process.
- A session of 1,000 small queries with push/pop between them completes in
  well under a second.

## Lemmas

Quantified lemmas can be asserted and used. Done when:

- A goal that needs one or two instantiations of a user lemma (e.g. the
  defining equations of len) solves, fast.
- With quantifiers present, sat degrades to unknown — never a model that
  ignores a quantifier.
- Each instantiation records which lemma and which substitution produced it,
  and replays in the certificate checker.
- A matching-loop lemma (associativity) hits its budget and returns unknown;
  it never hangs.
- Results on the public quantified benchmark sets (UF, UFLIA), with their
  time limit stated.
