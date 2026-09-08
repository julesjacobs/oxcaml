# Verified borrow library

From a configured Vox checkout with Z3 on `PATH`:

```sh
make vox-library
```

This builds and installs the final compiler, verifies the library with
`-principal` in bytecode and native modes, and installs `Vox_sequence`, `Vox_int_sequence`, `Borrow`,
and `vox_borrow` under the configured prefix's `lib/ocaml/vox`.

With the worktree-local prefix from the agent guide, compile a client with:

```sh
_install/bin/ocamlc -extension refinement_types \
  -I _install/lib/ocaml/vox vox_borrow.cma client.ml -o client.byte
_install/bin/ocamlopt -extension refinement_types \
  -I _install/lib/ocaml/vox vox_borrow.cmxa client.ml -o client.exe
```

The library needs this compiler's storage primitives. Configure the compiler
with `--enable-poll-insertion --enable-multidomain` for actual parallel domains.
`Slice.parallel false` is also available with the single-domain runtime.

[The design](../../design-docs/borrows-and-slices.md) explains ownership,
current/final models, callback postconditions, the storage boundary, and the
normal-return correctness guarantee. Complete verified clients live in
`testsuite/tests/vox`: start with `borrow_demo.ml`, then `borrow_ranges.ml`,
`borrow_validation.ml`, and `quicksort_client.ml`.

Proof-only observations and lemma calls must be enclosed in `ghost_`. A
`Slice.snapshot` is a real copy suitable for executable `assume_` checks.

`Vox_sequence` contains polymorphic sequence operations and decomposition laws.
`Vox_int_sequence` adds integer bounds, sortedness, and permutation laws. Its
abstract `multiset` model exposes `bag`, `multiplicity`, and sequence `count`.
`permutation_count` derives equal counts from permutation; `count_extensional`
proves permutation from a total proof function establishing equal counts at
any integer. Clients do not depend on canonical insertion or its ordering.

`collection_theory.ml` uses those laws to verify rotation, while quicksort
uses them for slice swaps and recombination. Only the three partition lemmas
remain in `quicksort_model.ml`. Integer ordering and counting remain specialized
to integers; the sequence decomposition laws apply to any `immutable_data`.
