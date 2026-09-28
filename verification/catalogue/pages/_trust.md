title: What every demo trusts
blurb: The checker, the built-in meanings it assumes, the declared contracts and runtime code of the library, the standard library's totality casts and the toolchain.
status: owner-review
date: 28 September 2026
---
A Vox proof is a compile-time check: the compiler type-checks the program, generates verification conditions from its refinements, and asks Z3 to prove them. A demo's theorems hold only if the components below are correct. None of them is verified. Each demo page lists, in addition, what that demo alone trusts.

To get this list for a given program, compile a unit with `-vox-audit`. It prints what that unit and every unit it depends on trust, as recorded in their compiled files: trusted externals, totality casts, uses of unsafe features, and units whose verification was skipped. Outside the library, an external whose type states a refinement or totality also draws warning 228 (`trusted-external`).

## The checker

- The OxCaml type checker, including the mode, uniqueness and ghost checks that make tokens affine and keep ghost code free of run-time effects, and the Vox additions to it (`typing/typecore.ml`).
- Verification-condition generation and the meaning of built-in operations (`verification/vox_vc.ml`, `verification/vox_encoding.ml`), their translation to SMT-LIB (`verification/vox_smt.ml`), and the solver driver and the reading of its answers (`verification/vox_smt_solver.ml`, `verification/vox_smt_response.ml`, `verification/runtime/vox_verify.enabled.ml`).
- The totality check: a function declared `@@ total` or `@ total` must terminate without raising. Totality does not forbid writes: a total function may write to storage it owns uniquely, as `Quicksort.sort_array` does through a uniquely owned array. Creating a loan (`Owned_array.with_mut`, `Slice.split_at`, `split3`, `with_range`) and ending one (`Slice.finish`) are not total: creating a loan chooses its final contents, and ending it assumes that they equal its current contents. Functions that appear in refinements must also be stateless, so that their result depends only on their arguments. The checker treats every total, stateless function as a function of its arguments, including functions of units it never verified, so each primitive declared `@@ total` must give equal results for equal arguments wherever it is compiled.
- Ghost erasure (`lambda/translcore.ml`). Ghost code and `void` values are removed before code generation; a ghost argument of any other layout is passed as a placeholder constant. Ghost record fields are removed in native code; bytecode keeps an empty slot for each. A lemma is an ordinary compiled function; only its calls inside `ghost_` are erased.
- `[@def]` lemmas. For a function marked `[@def]`, the type checker generates a lemma stating that the function equals its body. The verifier assumes it; it holds by construction.
- The rest of the OxCaml compiler, runtime and standard library, which compile and run the erased program.

## Built-in meanings

The checker gives these operations a fixed meaning instead of deriving it from code. A meaning that belongs to a C primitive applies only to the library's own declaration of it, not to another `external` that names the same symbol.

- `int` is a signed 63-bit integer with wrapping arithmetic; `max_int`, `min_int` and `abs` have their exact values. `Bigint.t` is an unbounded integer with the mathematical operations; `Bigint.div` and `Bigint.modulo` are Euclidean, with `div x 0 = 0` and `modulo x 0 = x` (`stdlib/bigint.mli`). The trailing-zero count of `Vox_table_bits` has its exact value.
- `=`, `<>`, `<`, `<=`, `>`, `>=` and `compare` are total, stateless and portable at `int`, `bool` and `Bigint.t`, so they may appear in refinements.
- `lsl`, `lsr` and `asr` are 63-bit shifts for a count in [0, 63]. OCaml leaves other counts unspecified, and native code really differs between evaluations (constant folding against the hardware's masking of the count), so every shift the program performs must have its count proved in range, and the standard shifts are partial. `Int.Refined` has total shifts whose count is refined to that range, and the checker rejects an `external` that declares a shift total without such a refinement.
- `===` is logical equality: equality of complete values in the solver's model. Inside `assume_` it is checked at run time: exactly at `int`, `bool` and `Bigint.t`, and elsewhere by physical equality, raising `Invalid_argument` when that cannot decide.
- `Iarray.length`, `get`, `sub`, `append` and `init` have their meaning, and every immutable array has at most 2^60 − 1 elements.
- `Vox_sequence.length` is the length of a list.
- The operations of `Set.MakeTotal` and `Map.MakeTotal` are the finite-set and finite-map operations, with elements identified up to the ordering's equivalence.
- Ghost heaps. `Pref.Heap` is a finite map from locations to values: `empty`, `mem`, `at`, `put`, `union`, `restrict` and `exclude` have their pointwise meaning, locations are distinct, and two heaps that agree everywhere are equal. `verification/library/vox_pref_semantics.ml` checks the pointwise equations. Of the heap laws in `verification/library/pref.mli`, `put_union_law`, `commute_law`, `exclude_put_law` and `exclude_union_law` are proved; `put_law`, `union_law`, `domain_law`, `union_domain_law`, `partition_law` and `split_law` are stated, not proved.
- Borrowed slices. Opening, restoring, splitting, recombining, transferring and finishing a borrow relate the current and final contents of the slices involved; `finish` sets the final contents to the current ones.
- Distinct string literals are distinct values, and `raise` does not return.

## Declared contracts

An `external` has no body to check, so its type is an assumption: its refinements, its modes and, if it is declared total, its totality. The library's externals are in `verification/library/`: the token, cell and heap operations of `Pref` and `Ghost_pref`; borrowed slices and owned arrays (`borrow.mli`, `borrow_iarray.mli`, `vox_string_view.mli`), including `Owned_array.split_at` and `append`, whose results share storage with their arguments, and the bound on an owner's length in `Owned_array.length` (at most `Borrow.max_length ()`, which is `%max_wosize`); `Raw_memory`; the flat hash table's storage (`vox_table_storage.mli`); `Vox_iarray`, `Vox_sequence` and `Vox_control`; all seven atomic operations of `verified_atomic.mli`; and the operations of `Unique_cell`. The standard library's are the operations of `Bigint`, the division and shift operators of `Int.Refined` (nonzero divisor, count in [0, 63]), `Iarray.length` and `Iarray.Refined.get`, and `find` in the `Refined` modules of `Set.MakeTotal` and `Map.MakeTotal`. An interface can hide an external: `Pref.empty`, `Pref.alloc`, `Ghost_pref.alloc`, `Borrow.max_length`, `Borrow.Owned_array.get`, `set`, `split_at` and `append` are `val` in their `.mli` and `external` in their `.ml`, as are the operations of `Unique_cell` other than `location`. `-vox-audit` lists them all.

## Runtime code

These C and compiler files implement the externals. They are ordinary unverified code, assumed to meet the stated contracts:

- `runtime/bigint.c`: `Bigint`.
- `runtime/pref.c`: `Pref` cells and tokens, the atomics, unique cells, raw memory and the flat hash table's storage.
- `runtime/borrow.c`: borrowed slices, owned arrays (split and appended in place), `Vox_sequence.length`, and `Vox_iarray.sub` and `set`.
- `runtime/vox_control.c`: the trailing-zero count and the 16-byte control-group scans.
- `backend/cmm_builtins.ml`: native lowering of these primitives.

Where a refinement guarantees that an index is in bounds, the primitive does not check it: `Owned_array.get_int` and `set_int` in native code, and the reads and writes of `Raw_memory` and of the table storage in native code and bytecode. The generic `get` and `set` of borrowed slices and owned arrays check bounds in both.

## Totality casts in the standard library

A few standard-library functions are made total by a cast, `external trust_total : 'a -> 'a @ total = "%identity"`, instead of a checked definition. Each is trusted to terminate without raising when its function arguments do:

- `List` (`stdlib/list.ml`): `concat_map`, `merge`, `stable_sort`, `sort`, `fast_sort`, `sort_uniq`, `to_seq`, and `Refined.hd` and `Refined.tl` (which apply the partial `hd` and `tl` to a list whose refinement says it is not empty). The other `List` functions are checked total.
- `Iarray` (`stdlib/iarray.ml`): `iter`, `iteri`, `to_list`, `fold_left`, `fold_right`, `exists`, `for_all`, `equal`, `compare`, `find_opt`, `find_index`, `find_map`, `find_mapi`, `sort`, `stable_sort`, `fast_sort`, `to_seq` and `to_seqi`.
- `Set.MakeTotal` and `Map.MakeTotal` and their `Refined` modules (`stdlib/set.ml`, `stdlib/map.ml`): every function, through `Set.Make` and `Map.Make`. That they implement finite sets and maps also rests on the laws of the ordering (`Set.TotalOrderedType`).

## The toolchain

- Z3 4.16.0. A goal counts as proved only when Z3 reports `unsat` for its negation within the resource limit. The compiler refuses a solver that reports another version unless it is given `-smt-solver-any-version`; the executable named by `-smt-solver` is trusted to be Z3.
- `-smt-assume-verified` skips the verification of a unit, including its `[@@decreases]` termination proofs. The compiled unit records this, `-vox-audit` lists it, and a verified unit that imports such a unit's interface draws warning 229 (`unverified-import`). `verification/library/build.sh` verifies each unit in its module list when compiling it to bytecode (RSA, diff, the e-graphs, `Vox_machine_semantics` and `Vox_traversal` are not in that list; the tests that use them compile and verify them), then compiles the same source natively with `-smt-assume-verified`; the native compilation is recorded as verified because a verified compilation of the same program, with the same flags and interfaces, produced the unit's `.cmo`.
- The verification caches. With `VOX_VERIFY_CACHE` set, a unit is not verified again when the compiler executable, the solver's version, the source, the imported interfaces and the flags are unchanged, and a solver query is not sent again when its text and the solver's version are unchanged. Anyone who can write to the cache directory can mark units and queries as proved; the compiler ignores a directory that another user owns or that other users can write to. `build.sh` and `./dev` use `_build/vox-verify-cache`; `VOX_VERIFY_CACHE=` disables the caches.

## Excluded features

The guarantees assume that no linked code outside the standard library, which is trusted with the compiler, uses the following; each can make a refinement false at run time or duplicate ownership. `-vox-audit` lists their uses, including the standard library's.

- `Obj`, the reading functions of `Marshal`, and `input_value`: each can return a value of any type, and the verifier assumes the refinement of the type it is given.
- Unsafe primitives such as `Array.unsafe_get`, which skip the checks that a refinement would otherwise rely on.
- Other `external` declarations: like the library's, their types are assumptions.
- A known OxCaml mode-soundness bug: first-class modules do not track the portability and contention of the exception constructors defined inside them, so an exception can carry an uncontended `ref` across capsules (`jane/doc/extensions/_05-modes/reference.md`). Whether it can duplicate a Vox token has not been determined.

The library itself uses two of these, and trusts them. `Vox_lz4_checked_api` returns its output buffer with `Bytes.unsafe_to_string` after its last write. `Vox_parallel.fork_join`, used by the parallel quicksort, runs one branch in a new domain (`Domain.Safe.spawn`), joins it before returning or raising, and applies `Obj.magic_unique` to the joined result, which has no other reference because the domain handle is private and joined once; its interface is plain polymorphism with no refinements.

## What is not claimed

Unless a page says otherwise, a contract describes normal return. A function that is not `total` may fail to terminate or may raise, and after a raise the program may have consumed ownership it cannot recover. A total function may still stop by exhausting resources, with `Out_of_memory` or `Stack_overflow`, for example in `Bigint` arithmetic or in a non-tail-recursive list function on a long list. Running time and memory use are not bounded. For the channel and lock demos, the theorems concern ownership transfer and refinements on normal return, not linearizability or progress (`verification/library/concurrency-boundary.md`).
