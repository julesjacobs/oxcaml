title: Flat hash table
blurb: A mutable hash table proved to act as a finite map: lookups return the map's answer, updates are map updates and `length` is its size.
status: review-pending
date: 27 September 2026
sources:
  - verification/library/vox_verified_flat_hashtbl.mli — Public interface
  - verification/library/vox_verified_flat_hashtbl.ml — Implementation: the functor and the abstract map
  - verification/library/vox_table_storage.mli — Trusted storage and SIMD contracts
  - verification/library/vox_table_model.ml — Model of storage and of the SIMD masks
  - verification/library/vox_table_implementation.ml — Probing, insertion, deletion and rebuilding
  - verification/library/vox_flat_hashtbl_review.md — Review notes: reading order and trusted base
  - testsuite/tests/vox/flat_hashtbl_public.ml — Public-only client
  - testsuite/tests/vox/flat_hashtbl_boundary.ml — The public check: public-only compile, rejected clients and erasure
---
`Vox_verified_flat_hashtbl.Make` is a mutable open-addressing hash table whose operations are proved to act on a finite map: lookups return the map's answer, `replace` and `remove` return exactly `put` and `erase` of the previous map, and `length` is the number of bindings. The proof covers probing, replacement, deletion and rebuilding. The SIMD mask routines, the storage primitives and the checker itself are trusted.

Keys and values must be `immutable_data`. Every operation except `create` takes two extra arguments that the compiler erases: a view of the table and a permission token. `create` takes only a token. Only normal return is specified; termination, running time and concurrent use are not.

## Client example

From the public-only client, inside a functor over any `Key`, with `V = Vox_verified_flat_hashtbl.Make (Key)` and `P = Ghost_pref`. `{v : t | p}` is the type `t` refined by the predicate `p`, `===` is logical equality, and `ghost_ (...)` is proof code, checked and then erased. `c.#view` and `u.#view` are erased snapshots of the table. `c.#token` is the erased permission to use it, which `replace` consumes and returns anew and `borrow_` lends to a read.

@code testsuite/tests/vox/flat_hashtbl_public.ml "(* Reading a key back" "V.find_opt c.#table u.#view key"

## A rejected program

Claiming the wrong contents is a type error. This client stores 84 and states that `find` returns 85. The test compiles it after `module V = Flat_hashtbl_public.V`, against the public interfaces and the client.

@code testsuite/tests/vox/flat_hashtbl_boundary.ml "(* A false claim about a lookup" "|}]"

The test requires 13 such programs to be rejected, each with its exact error. Six are ownership or refinement errors: this one, a stale view, a reused token, a token that does not own the table, and two false key laws. Seven are abstraction checks, such as reaching hidden internals or building a `Map.t` from a list. The stale-view, reused-token and missing-ownership programs are also compiled with both compilers without `-extension refinement_types` and are still rejected.

## Native code

`ocamlopt -O3 -dcmm` output for the same example written at top level with integer keys (`find_after_replace_int` in the same client). Three calls remain: `create`, called directly, and `replace` and `find_opt`, through the closures of the table's functor instance. Views and tokens are passed as `[]`, the empty void argument, and the proof leaves only the trivial `catch` at the top. The check asserts that neither this nor the default build calls an ownership primitive or lemma.

```
(function{flat_hashtbl_public.ml:145,27-339}
 camlFlat_hashtbl_public__find_after_replace_int_18_80_code
     (key/8431: int value/8432: int) : val
 (catch (exit 134 (seq 1 [])) with(134)
   (let
     c_2670_unboxed0/8434
       (app{flat_hashtbl_public.ml:147,26-47}
         G:"camlFlat_hashtbl_public__create_15_63_code" val)
     (catch
       (exit 135
         (app{flat_hashtbl_public.ml:148,10-55;vox_verified_flat_hashtbl.ml:171,12-63}
           G:"caml_applyV__V_V__R" c_2670_unboxed0/8434 [] key/8431
           value/8432 []
           (load val
             (+a
               (load val (+a G:"camlFlat_hashtbl_public__replace_20_80" 24))
               80))
           unit))
     with(135)
       (app{flat_hashtbl_public.ml:150,2-52;vox_verified_flat_hashtbl.ml:144,4-48}
         G:"caml_applyV__V_" c_2670_unboxed0/8434 [] key/8431 []
         (load val
           (+a (load val (+a G:"camlFlat_hashtbl_public__replace_20_80" 24))
             16))
         val)))) )
```

Erased values that the optimizer cannot drop are passed as a placeholder constant (48059); the `-O3` assembly of this client contains none.

## Interface

@code verification/library/vox_verified_flat_hashtbl.mli

`Pref.Heap` is a finite map from locations to values, `P.own token` is the heap a token owns, and `Ghost_pref` provides erased tokens: `empty` makes one that owns nothing, and `split` and `join` divide and recombine ownership. `Bigint` is unbounded integers, used for sizes.

## Trusted base

- SIMD group matching: the NEON and SSE2 routines in `runtime/vox_control.c` (or its scalar fallback, used on other targets) and their native lowering in `backend/cmm_builtins.ml` are assumed to return the sixteen-lane masks specified by `vox_table_model.ml`, which is checked OCaml.
- Storage: allocation, typed slot access, bulk clearing and backing replacement are assumed to meet `vox_table_storage.mli` (implemented in `runtime/pref.c`).
- Count-trailing-zeros is assumed to return the index of the lowest set bit of a nonzero 63-bit integer, and 63 for zero.

## Scope

- Operations: `create`, `length`, `find_opt`, `find`, `mem`, `replace`, `remove` and `clear`. There is no iteration, fold, copy or presized `create`.
- Capacity is 16 to 2^30 slots. `find` raises `Not_found` for an absent key; `replace` raises `Invalid_argument` beyond 2^30 slots; any operation can raise `Out_of_memory` or `Stack_overflow`.
- A mutation that raises loses the token it consumed, so the table cannot be used afterwards. `find` only borrows its token, so the table stays usable after `Not_found`.
- Keys and values are `immutable_data`, so mutable records and closures cannot be stored.
- `Key.equal` and `Key.hash` must be Vox-checked `total` functions supplied with proofs of four laws. Hash functions from existing libraries, such as those derived by ppx_hash, are not declared `total` and cannot be passed as they are.
- Callers need not enable `-extension refinement_types` unless they write refinements or proofs.
- No termination, running-time, concurrency or memory-reclamation theorem.

## Performance

Nanoseconds per operation, median of five runs, with integer keys and values. `build` inserts every key into a table that starts at capacity 16; `hit` is `find` on present keys and `miss` is `mem` on absent keys, both in shuffled order. All three tables use the same `Key.hash`.

Against `Base.Hashtbl`, hits are 1.0–1.2× faster up to 1,024 entries and 1.8–2.6× faster from 57,344 up. Misses are 1.1–1.5× slower up to 57,344 entries, level at 917,504 and 3.1× faster in the just-doubled 1,048,576-entry table. Builds are 2–4× slower up to 16 entries and 1.8–2.5× faster from 57,344 up. `Stdlib.Hashtbl` is the fastest of the three up to 1,024 entries.

@performance flat-hash-table.performance.json

57,344 and 917,504 entries fill the verified table to its maximum load of 7/8; at 1,048,576 it has just doubled and is half full. Measured on an Apple M4 Max (arm64, so the NEON path; the SSE2 path used on x86-64 was not measured), `-O3`, `Base` v0.17.3, CPU time from `Sys.time`. Other jobs were running during the measurement; the implementations were interleaved, in alternating order. The measurement predates small interface changes (`create`'s result record, names) that do not touch the probing code.

## Reproduce

Checking happens during ordinary compilation; there is no separate verification tool. After `autoconf && ./configure --prefix=$PWD/_install`, `make install` and `./dev init`:

```
./dev test vox/flat_hashtbl_boundary.ml vox/table_model.ml vox/table_ownership_rejected.ml
```

`flat_hashtbl_boundary.ml` compiles, and so checks, the table's 32 modules with both compilers; compiles the public-only client against the `Pref`, `Ghost_pref` and `Vox_verified_flat_hashtbl` interfaces only, links and runs it; checks the rejected programs; and checks the client's Lambda and native Cmm, and the Cmm of the vacancy scan, for proof code and ownership primitives. The SMT solver is Z3 4.16.0.
