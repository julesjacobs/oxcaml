title: What every demo trusts
blurb: The checker, the built-in meanings it assumes, and the unverified runtime code shared by all demos.
status: reviewed
date: 27 September 2026
---
A Vox proof is a compile-time check: the compiler type-checks the program, generates verification conditions from its refinements, and asks Z3 to prove them. A demo's theorems therefore hold only if the components below are correct. None of them is verified. Each demo page lists, in addition, what that demo alone trusts.

## The checker

- The OxCaml type checker, including the mode, uniqueness and ghost checks that make tokens affine and keep ghost code free of runtime effects.
- Verification-condition generation in `verification/vox_vc.ml` and its translation to SMT-LIB in `verification/vox_smt.ml`.
- Z3 4.16.0. A goal counts as proved only when Z3 reports `unsat` for its negation within the resource limit.
- The totality check: a function declared `@@ total` or `@ total` must terminate without raising or touching mutable state. Only such functions may appear in refinements.
- Ghost erasure: ghost arguments, fields and code are removed before code generation.
- The rest of the OxCaml compiler and runtime, which compile and run the erased program.

## Built-in meanings

The checker gives these operations a fixed meaning instead of deriving it from code:

- `int` is a signed 63-bit integer with wrapping arithmetic. `Bigint.t` is an unbounded integer; its operations are the mathematical ones (`stdlib/bigint.mli`, implemented in C in `runtime/bigint.c`).
- `=`, `<>`, `<`, `<=`, `>`, `>=` and `compare` are total and stateless at `int`, `bool` and `Bigint.t`, so they may appear in refinements.
- `===` is logical equality: equality of complete values in the solver's model. It has no runtime counterpart.
- `Vox_sequence.length` is the length of a list; the checker states this directly.
- Ghost heaps. `Pref.Heap` is a finite map from locations to values. The checker encodes `empty`, `mem`, `at`, `put`, `union`, `restrict` and `exclude` by their pointwise meaning; `verification/library/vox_pref_semantics.ml` checks that this encoding gives the expected pointwise equations. The laws declared `external` in `verification/library/pref.mli` (`put_law`, `union_law`, `split_law` and others) are stated, not proved.
- Tokens and cells. `Pref.own`, the cell operations of `Pref` (`empty`, `alloc`, `read`, `write`, `equal`, `split`, `join`) and their erased counterparts in `Ghost_pref` are implemented by C primitives (`verification/library/pref.mli`, `ghost_pref.ml`). Their contracts, together with the affine use of tokens, are the ownership model. `Ghost_pref.alloc` is declared with `val` in the interface but is an `external` in the implementation.
- Borrowed slices. The `borrow_` construct and the operations of `verification/library/borrow.mli` and `borrow_iarray.mli` (reading, writing and splitting a borrowed array slice) have contracts the checker assumes.

## Runtime code

These C and compiler files implement the primitives above. They are ordinary unverified code, assumed to meet the stated contracts:

- `runtime/pref.c`: mutable cells and typed storage behind `Pref`.
- `runtime/borrow.c`: borrowed slices.
- `runtime/bigint.c`: `Bigint`.
- `backend/cmm_builtins.ml`: native lowering of these primitives. Native slot and slice accesses have no bounds checks; the refinements are what keep them in bounds.

## Concurrency

The channel and lock demos also trust `verification/library/verified_atomic.mli`, whose atomic load and compare-and-set are `external` with contracts that open an invariant at the atomic step, and `verification/library/unique_cell.mli`, whose operations (`location`, `create`, `take`, `put`, `replace` and the `Slot` functions) are C primitives; only `location` is declared `external` in the interface, the others are `external`s in `unique_cell.ml`. `verification/library/concurrency-boundary.md` gives the reading order. Their theorems are about ownership transfer and refinements on normal return, not linearizability or progress.

## How the library is built

`verification/library/build.sh` checks every library module when compiling it to bytecode. The native compile of the same source then passes `-smt-assume-verified` and does not check it again. Both builds use a verification cache keyed by the compiler executable, the source, the interfaces it imports, the flags and the solver; a changed input misses the cache. Set `VOX_VERIFY_CACHE=` to disable it.

## What is not claimed

The guarantees assume that the rest of the program does not use unsafe primitives (`Obj.magic`, unchecked array access), does not `Marshal` abstract values, and does not rely on a known OxCaml mode-soundness bug; any of these can forge or duplicate ownership.

Unless a page says otherwise, a contract describes normal return. A function that is not `total` may fail to terminate or may raise, and a function that raises may leave ownership consumed. Running time, memory use and stack depth are not bounded.
