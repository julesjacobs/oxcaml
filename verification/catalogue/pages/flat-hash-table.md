title: Flat hash table
blurb: A mutable hash table proved to act as a finite map: lookups return the map's answer, updates are map updates and `length` is its size.
status: owner-review
date: 4 October 2026
sources:
  - verification/library/vox_verified_flat_hashtbl.mli — Public interface
  - verification/library/vox_verified_flat_hashtbl.ml — Implementation and storage-to-map bridge
  - verification/library/vox_table_storage.mli — Trusted storage and SIMD contracts
  - verification/library/vox_table_model.ml — Model of storage and of the SIMD masks
  - verification/library/vox_table_implementation.ml — Probing, insertion, deletion and rebuilding
  - verification/library/vox_flat_hashtbl_review.md — Review notes: reading order and trusted base
  - testsuite/tests/vox/flat_hashtbl_public.ml — Public-only client
  - testsuite/tests/vox/flat_hashtbl_boundary.ml — The public check: public-only compile, rejected clients and erasure
---
`Vox_verified_flat_hashtbl.Make` is a mutable open-addressing hash table whose operations are proved to act on a finite map: lookups return the map's answer, `replace` and `remove` return `Model.add` and `Model.remove` of the previous map, and `length` is the number of bindings. The proof covers probing, replacement, deletion and rebuilding. The SIMD mask routines, the storage primitives and the checker itself are trusted.

## Interface

A table has an ordinary, aliasable handle and an erased permission. The
permission carries its current bindings: `owner p` identifies the table, and
`bindings p` is its finite map. Reads borrow the permission; mutations consume
it and return its replacement. A saved pure map remains usable as a historical
snapshot. Storage layout and capacity are private.

`Model = Map.MakeLogical(Key)` supplies the usual map operations. `Key.equal`
compares keys; `===` means identical bindings, independent of insertion order
and which equal key was stored. `cardinal` uses mathematical integers. These
operations have built-in verifier support; clients need no map-law calls.
The complete model API is in [Map.MakeLogical](src:stdlib/map.mli).

@code verification/library/vox_verified_flat_hashtbl.mli

## Trusted base

- SIMD group matching: the NEON and SSE2 routines in `runtime/vox_control.c` (or its scalar fallback, used on other targets) and their native lowering in `backend/cmm_builtins.ml` are assumed to return the sixteen-lane masks specified by `vox_table_model.ml`, which is checked OCaml.
- Logical maps: the semantics of `Map.MakeLogical` in the checker and the distinguishing-key contract of `Proof.difference`.
- Storage: allocation, typed slot access, bulk clearing and backing replacement are assumed to meet `vox_table_storage.mli` (implemented in `runtime/pref.c`).
- Count-trailing-zeros (`caml_vox_int_ctz` in `runtime/vox_control.c`, and its native lowering) is assumed to return the index of the lowest set bit of a nonzero 63-bit integer, and 63 for zero. The checker gives the primitive this meaning directly (`verification/vox_encoding.ml`).

## Scope

- Operations: `create`, `length`, `find_opt`, `find`, `mem`, `replace`, `remove` and `clear`. There is no iteration, fold, copy or presized `create`.
- `find` raises `Not_found` for an absent key. Contracts describe normal return.
- A mutation that raises loses the permission it consumed, so the table cannot be used afterwards. `find` only borrows its permission, so the table stays usable after `Not_found`.
- Keys have kind `logical_data` and values have kind `immutable_data`, as stated by the interface.
- `Key.equal` and `Key.hash` must be Vox-checked `total` functions supplied with proofs of four laws. Hash functions from existing libraries, such as those derived by ppx_hash, are not declared `total` and cannot be passed as they are.
- Callers need not enable `-extension refinement_types` unless they write refinements or proofs.
- No termination, running-time, concurrency or memory-reclamation theorem.

## Client example

From the public-only client, with `V = Vox_verified_flat_hashtbl.Make(Key)`.
The result's refinement states the expected value; `borrow_` lends the permission
to the lookup.

@code testsuite/tests/vox/flat_hashtbl_public.ml "  let find_after_replace" "V.find_opt c.#table key (borrow_ p)"

## A rejected program

The boundary test stores 84 under key 1 and checks a client claiming that `find`
returns 85. The verifier rejects it. The corresponding claim of 84 is accepted,
without an explicit proof block: map lookup after `add` is automatic.

The test also rejects consumed permissions, permissions for another table, false key
laws and attempts to access private storage or fabricate the abstract model.
Ownership checks still apply to callers that do not write refinements.

## Native code

An earlier `ocamlopt -O3 -dcmm` excerpt on x86-64 for the example written at top level (`find_after_replace_int` in the same client). The library's table modules are also compiled at `-O3`, so the table's functor instance is specialized in the client: `create` becomes the storage allocation primitive, `replace` is a direct call, and `find_opt` is inlined. A table of 16 slots is a single group, probed in place with SSE2: one byte comparison of the 16 control bytes gives a mask of candidate slots, whose keys are compared in turn. A larger table goes to a direct call of `Vox_table_search.groups`. Proof data are gone, and the proof leaves only the trivial `catch` at the top. The key module compares integers and hashes a key with an exclusive or of its upper half and a multiplication (`int_mul`, computed on tagged integers). The fingerprint is the hash's low seven bits (`byte`, untagged by `(>>s byte/9517 1)`), which the multiplication by 72340172838076673 copies into each byte of a word; the group index is the hash shifted right by 7; and 33 is the capacity 16 as a tagged integer. `...` marks omitted lines, and `{...}` shortens debug locations that list five inlined calls:

```
(function{flat_hashtbl_public.ml:146,27-339}
 camlFlat_hashtbl_public__find_after_replace_int_18_128_code
     (key/9478: int value/9479: int) : val
 (catch (exit 191 (seq 1 [])) with(191)
   (let
     (allocated/9482
        (extcall "caml_vox_table_create"{flat_hashtbl_public.ml:148,26-47;vox_verified_flat_hashtbl.ml:125,12-29;vox_table_mutation.ml:55,20-37}
          33 int->val)
      Pmixedfield/9486 (load val allocated/9482))
     (catch
       (exit 192
         (app{flat_hashtbl_public.ml:149,10-55;vox_verified_flat_hashtbl.ml:174,12-63}
           G:"camlFlat_hashtbl_public__replace_428_92_code" Pmixedfield/9486
           key/9478 value/9479 unit))
     with(192)
       (catch
         (let
           (int_mul/9507
              (+
                (* (or (xor key/9478 (>>u key/9478 32)) 1)
                  712544676207699905)
                -712544676207699904)
 ...
            group/9514 (and (or (>>u int_mul/9507 7) 1) int_sub/9512))
           (if (== capacity/9511 33)
             (let
               mask/9521
                 (let
                   (byte/9517 (and int_mul/9507 255)
 ...
                           (let
                             low/9519
                               (scalar->int64x2
                                 (* (>>s byte/9517 1) 72340172838076673))
 ...
                       (if (== stored/9537 key/9478) (exit 194 index/9534)
 ...
             (exit 193
               (app{...}
                 G:"camlVox_table_search__groups_358_717_code"
 ...
```

The test checks that this client makes no call through a closure of the functor instance (`caml_apply`), that it calls `Vox_table_search` directly, and that neither this nor the default build calls an ownership primitive or lemma. Erased values that the optimizer cannot drop are passed as a placeholder constant (48059), for example to the function that rebuilds the table.

## Performance

Nanoseconds per operation, median of five runs, with integer keys and values. `build` inserts every key into a table that starts at capacity 16; `hit` is `find` on present keys and `miss` is `mem` on absent keys, both in shuffled order. All three tables use the same `Key.hash`.

Against `Base.Hashtbl`, hits are 1.0–1.2× faster up to 1,024 entries and 1.8–2.6× faster from 57,344 up. Misses are 1.1–1.5× slower up to 57,344 entries, level at 917,504 and 3.1× faster in the just-doubled 1,048,576-entry table. Builds are 2–4× slower up to 16 entries and 1.8–2.5× faster from 57,344 up. `Stdlib.Hashtbl` is the fastest of the three up to 1,024 entries.

@performance flat-hash-table.performance.json

57,344 and 917,504 entries fill the verified table to its maximum load of 7/8; at 1,048,576 it has just doubled and is half full. Measured on an Apple M4 Max (arm64, so the NEON path), `-O3`, `Base` v0.17.3, CPU time from `Sys.time`. Other jobs were running during the measurement; the implementations were interleaved, in alternating order. The measurement predates small interface changes (`create`'s result record, names) that do not touch the probing code.

The SSE2 path used on x86-64 was measured separately, against `Stdlib.Hashtbl` and `Base.Hashtbl`: an AMD Ryzen 9 7950X3D with one core pinned, `-O3` with flambda2, integer keys and values, median of seven runs. The machine had low load but was not idle; one other single-core job ran on the other core complex throughout. From 57,344 to 8,388,608 entries the verified table was faster than `Stdlib.Hashtbl`: successful lookups 1.4–1.7×, inserts 1.2–2.1× and insert/delete mixes 1.5–2.4×. Unsuccessful lookups ranged from 0.9× to 4.4×; the 0.9× case is a table at its maximum load that still fits in cache. Up to 4,096 entries the verified table was slower: a successful lookup took about 7 ns against 2.4–3.6 ns for `Stdlib.Hashtbl`, 2–3× slower. At these sizes it was about as fast as `Base.Hashtbl`. Two known causes on small tables are that `Key.equal` is not inlined into the lookup and that each slot write is a C call. There is no unverified table of the same design to compare against, so these numbers compare two designs and do not measure the cost of verification.

## Reproduce

Checking happens during ordinary compilation; there is no separate verification tool. After `autoconf && ./configure --prefix=$PWD/_install`, `make install` and `./dev init`:

```
./dev test vox/flat_hashtbl_boundary.ml vox/table_model.ml vox/table_ownership_rejected.ml
```

`flat_hashtbl_boundary.ml` compiles, and so checks, the table's 32 modules with both compilers; compiles the public-only client against the `Vox_verified_flat_hashtbl` interface only, links and runs it; checks the rejected programs; and checks the client's Lambda and native Cmm, and the Cmm of the vacancy scan, for proof code and ownership primitives. The SMT solver is Z3 4.16.0.
