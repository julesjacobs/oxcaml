# Refinement-checking slowness

Observed while building `avl_sets.ml`:

- A complete, well-typed file spent several minutes after type checking, while
  variants stopped by a deliberate type error finished in five to ten seconds.
  The latter never reached VC generation, so they measured typing only. The
  full normal-mode compilation did not finish within 41 minutes. By contrast,
  the core without proof modules passed normal and principal modes in 20
  seconds. The bottleneck is therefore in post-typing refinement checking.
- Re-wrapping a subtree in a refinement to transport one equation, or
  specializing a conditional theorem into an unconditional witness, made the
  slowdown especially visible.
- Local modules, empty result signatures, nested sealed proof modules, and a
  compiled test with an empty explicit interface did not eliminate the delay.
  This makes module-signature inference and `.cmi` serialization unlikely to be
  the sole cause.
- The validity proofs remained slow when the insertion proof was excluded, so
  the cost is already present in the rotation and balance proof chain.
- Splitting the proof chain into compilation units, placing each rotation in a
  separate unit, and sealing helper units with explicit interfaces did not fix
  the problem. One isolated rotation still exceeded 15 minutes.
- A two-second process sample during one isolated rotation spent most samples
  waiting in `select` and repeatedly spawning solver processes.
- Interrupting `make test-one-no-rebuild` left orphaned `expect` processes.
  Concurrent orphaned runs made later measurements misleading. The `dev` test
  runner already tracks process groups; the older make path should do the same
  or terminate its full child process group.

## Cause

Predicate translation was path-sensitive. `short_circuit` forked the symbolic
state for `&&` and `||`, and `Rexp_ifthenelse` forked it for `if`. `expose`
returned every resulting state when a refined witness was eliminated. Sequential
`let refine_` eliminations therefore multiply the state count, and refinement
introduction invokes the solver once for every state.

A minimal function eliminating eight witnesses whose predicates contain one
`if` generated exactly 256 queries at its final `refine_`. The isolated
right-rotation proof generated at least 70,199 distinct queries at its final
`refine_` before the trace was stopped. In a separate sample, 4,157 queries
contained 11.8 MB of SMT-LIB in total; the average query was 2.9 KB and the
average solver subprocess took 6.2 ms. The aggregate cost is query and process
multiplicity, not difficult individual queries.

## Resolution

Predicate translation now merges deterministic `if`, `&&`, and `||` outcomes.
It guards branch-local facts with their path conditions and produces `ite`,
`and`, and `or` terms. Normal and principal AVL verification together complete
in under one minute on the development machine. A persistent solver could
still reduce constant process overhead.
