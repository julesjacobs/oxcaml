title: LZ4 block compression
blurb: An LZ4 block compressor and decoder whose outputs are proved equal to a specification written as a scanner model and a decoder model, with a round-trip theorem.
status: owner-review
date: 27 September 2026
sources:
  - verification/library/vox_lz4.mli — Public interface
  - verification/library/vox_lz4_spec.ml — The two relations the contracts use
  - verification/library/vox_lz4_spec_decode_bytes.ml — Decoder model
  - verification/library/vox_lz4_spec_scan.ml — Compressor model: positions visited and hash-table updates
  - verification/library/vox_lz4_spec_match.ml — Hash and match selection used by the compressor model
  - verification/library/vox_lz4_spec_wire.ml — Wire layout of a plan
  - verification/library/vox_lz4.ml — Implementation of the public functions
  - verification/library/vox_lz4_streaming_codec.ml — Compressor over raw memory
  - verification/library/vox_lz4_string_decode.ml — Decoder over raw memory
  - verification/library/raw_memory.mli — Trusted raw byte buffers
  - verification/library/vox_lz4_string_copy.mli — Trusted copy from raw memory to a string
  - verification/library/vox_lz4_review_boundary.md — Reading order and trusted base
  - testsuite/tests/vox/vox_lz4_public_client.ml — Public-only client
  - testsuite/tests/vox/lz4_boundary.ml — Public-only compile, erasure check, rejected clients and finalizer checks
---
`Vox_lz4` compresses and decompresses independent raw LZ4 blocks of up to 4 MiB (4,194,304 bytes), with no frame header, checksum or dictionary. Its specification is two models written as total functions in `vox_lz4_spec_*.ml`: a compressor model that fixes which positions are hashed, which earlier position is tried as a match and how the matches are laid out on the wire, and a decoder model that fixes the status and output bytes for every input. `compress source` is proved to return exactly the wire bytes of the compressor model, not merely some valid encoding. `decompress_verified wire capacity` is proved to agree with the decoder model: the same status (success, malformed or output limit) and, on success, the same length and every byte. The ordinary entry point `decompress ?capacity wire` is proved to agree with the same model at the capacity it is given, or at 4,194,304 without one, and to return `Invalid_capacity` for a capacity outside 0 to 4,194,304. `roundtrip`, an erased theorem, proves that decoding the compressor model's output with a capacity from the source length to 4,194,304 gives back the source, and `compress_decompress source`, which runs both functions, is proved to return the source.

The reason and position carried by `Malformed` are not specified. The models are the definition of LZ4 here; their agreement with liblz4 is tested, not proved. Only normal return is specified; the codec functions may raise `Out_of_memory`, and `compress` raises `Invalid_argument` above 4 MiB.

## Client example

From the public-only client. `(source : {s : string | p})` names the argument so that the result type can refer to it, and `{v : t | p}` is the type `t` refined by the predicate `p`. `V.contents s` is an erased view of a string as a `char iarray`, and `===` is logical equality. `ghost_ (...)` is proof code, checked and then erased; here it calls the `roundtrip` theorem, whose result type states that `decoded` is `Ok` with the source's contents. `unreachable_ ()` asks the checker to prove that its branch cannot be reached (unlike `assert false`, which the checker treats as a failure that may happen). The theorem gives both that and, in the `Ok` branch, the equality of contents; without the `ghost_` line the function is rejected.

@code testsuite/tests/vox/vox_lz4_public_client.ml "let roundtrip" "| Error _ -> unreachable_ ()"

`lz4_boundary.ml` compiles this client against only the `.cmi` files of `Vox_lz4`, the ten specification modules and three sequence and string modules, runs it with both compilers, and checks that its `-dlambda` output contains exactly two calls into `Vox_lz4` and no reference to `Vox_lz4_spec`.

## A rejected program

The contract does not let a client claim more than the decoder model says. This program claims that decoding with capacity 0 never reports a malformed block. That is false, since the empty string is a malformed block, and the checker rejects it. The test `lz4_boundary.ml` compiles it against the public interfaces only and requires this error.

@code testsuite/tests/vox/lz4_boundary.ml "let f (wire : string) :" "|}]"

The same test checks three uses of `decompress`'s contract (an invalid capacity, the default capacity and a given one) and requires rejection of a claim that the default capacity is 0, of a claim that `compress` returns its input, a runtime use of the erased `contents`, and two references to implementation modules that the client cannot see, each with its exact error.

## Interface

@code verification/library/vox_lz4.mli

In `decompress`, `?capacity:(c : int)` names the optional argument as the caller passed it: `c` is an `int option`, `None` when the argument is left out. `@ ghost` marks `roundtrip`'s result as erased and `@@ total` says it terminates without effects. The two relations are defined in `vox_lz4_spec.ml`. `let[@def]` also generates a lemma, such as `matches_model_def`, that states the definition's equation for proofs to invoke:

@code verification/library/vox_lz4_spec.ml "let[@def] (matches_model @ total)" "from_source model))"

`decode_model` (in `vox_lz4_spec_decode_bytes.ml`) returns a status, a byte count and the decoded bytes in reverse order; `matches_bytes` compares them with the output string. `from_source` (in `vox_lz4_spec_scan.ml`) returns the list of matches the compressor chooses, and `wire_matches_plan` fixes their byte layout. Together with the parser, token, plan, hash and match modules, the specification is ten files and 522 lines.

## Trusted base

- Raw memory: the `external` functions of `raw_memory.mli` (`malloc`, `read`, `write`, `free`, `length`, `location`, and `equal`, which is `%eq`) and the axiom `location_law`, which states that locations of distinct buffers or indices differ. All the functions but `equal` are implemented by the `caml_raw_memory_*` functions in `runtime/pref.c`, which do not check bounds, and `read`, `write` and `length` are lowered to unchecked loads and stores in `backend/cmm_builtins.ml`. The same C file attaches a finalizer that frees a buffer once it is unreachable; it reclaims buffers whose ownership an exception discarded.
- The final copy: `Vox_lz4_string_copy.copy_prefix` is assumed to return a string holding the first `count` bytes of a buffer. It is an allocation followed by `memcpy` in `runtime/pref.c`.
- Strings and immutable arrays: `Vox_string_view.contents`, `length` and `get` (`%string_unsafe_get`), `Vox_sequence.iarray_get`, and `Vox_iarray.get`, `set`, `sub` and `extensional` (two arrays with equal lengths and elements are equal).
- Bytes: `byte_of_char` and `char_of_byte` in `vox_lz4_spec_parse.ml` and `same_char` in `vox_lz4_spec_bytes.ml` are primitives with stated contracts. A second `char_of_byte` in `vox_lz4_snapshot.ml` states `byte_of_char (char_of_byte b) = b`, which the first pair does not imply.
- The six `int32` primitives of the hash in `vox_lz4_spec_match.ml` are declared total with no contract, so the proofs hold whatever values they compute.

## Scope

- Operations: `compress`, `decompress_verified`, `decompress`, `compress_decompress` and `roundtrip`, and the constant `max_block_size`. There is no frame format, streaming across blocks or dictionary.
- Sizes: sources above 4,194,304 bytes make `compress` and `compress_decompress` raise `Invalid_argument`. `decompress_verified` requires a capacity from 0 to 4,194,304 as a precondition; `decompress` returns `Invalid_capacity` outside that range, as its contract states. The limit is on uncompressed bytes: `compress` output can be longer, and the wire model allows up to 4,210,768 bytes.
- Each decoding call with a valid capacity allocates a raw buffer of `capacity` bytes, so `decompress` without `~capacity` allocates 4 MiB whatever the input. Allocation failure raises `Out_of_memory`.
- `compress`'s contract fixes the output bytes. A compressor that chose different matches would not meet it, and nothing requires the output to be shorter than the input.
- The decoder model rejects a final token whose low four bits are nonzero, and, in a block with a match, requires the last five decoded bytes to be literals and the last match to start at least 12 bytes before the end.
- Errors: only the kind of error is specified (`Malformed`, `Output_limit`, never `Invalid_capacity` from `decompress_verified`, and from `decompress` exactly when the capacity is out of range); the `malformed` reason and position are not. `lz4_fast_decoder_reference.ml` compares them with an unverified reference decoder, `vox_lz4_baseline.ml`, by testing.
- An exception while scanning or decoding consumes the ownership of the raw buffer, which the finalizer then frees at some later collection; an exception from the final copy releases the buffer before it is re-raised. There is no exception-safety or prompt-cleanup theorem.
- The ghost `roundtrip` is total; the codec functions are specified for normal return only.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/lz4_boundary.ml vox/lz4_codec.ml vox/lz4_fast_decoder_reference.ml
```

`lz4_boundary.ml` compiles, and so checks, the LZ4 modules with both compilers (natively at `-O3`, as `verification/library/build.sh` does), compiles the public-only client and the rejected programs, and checks explicit release and reclamation by the finalizer after simulated `Out_of_memory` exits, raised after the codec has run rather than inside the public calls, with both compilers. `lz4_codec.ml` compiles every module `Vox_lz4` depends on and tests the codec on fixed, malformed, random and 4 MiB inputs. Separately, and outside the test suite because it needs liblz4, `python3 verification/benchmarks/lz4_interop.py _install/bin/ocamlopt` cross-checks blocks in both directions against it.
