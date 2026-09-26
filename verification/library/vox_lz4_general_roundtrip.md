# Verified mutable LZ4

The public `Vox_lz4` codec uses the checked streaming compressor and checked
string decoder. Both mutate owned storage. The compressor emits sequences as
it scans; its reference match plan exists only in ghost code.

The exact transitive review surface is listed in
[`vox_lz4_review_boundary.md`](vox_lz4_review_boundary.md).

## Specification

The API is partial: allocation can fail, and unsupported source sizes raise
`Invalid_argument`. Its specification concerns normal returns.

- `Vox_lz4.compress` returns a string satisfying
  `Vox_lz4_spec.compresses source wire`. This states that its bytes
  are the wire format of `Vox_lz4_forward_model.from_source (contents source)`.
- `Vox_lz4.decompress_verified` returns a decoded result satisfying
  `Vox_lz4_spec.matches_model wire capacity decoded`. Its string/error result
  classifies success, malformed input, or output limit exactly as the total
  decoder does. On success, its length and every output
  byte equal the allocation-independent total decoder's result. The public
  result has no heap witness. `Vox_lz4.decompress` adds an optional capacity
  argument and checks unsupported capacities.
- `Vox_lz4.roundtrip` is a total ghost theorem: if the wire
  satisfies `compresses`, the decoded result satisfies `matches_model`, and
  capacity is at least the source length, decoding succeeds and the output
  contents equal the source contents.
- `Vox_lz4.compress_decompress` composes the public compressor and verified
  decoder. Its checked return refinement states that equality directly.

Compression sources and decoded capacities are bounded by 4,194,304 bytes. The theorem
covers empty input, arbitrary byte values, multiple matches, overlapping
matches, and every supported capacity sufficient for the source.

## Proof structure

`Vox_lz4_forward_model` is a total model of the scanner's visited hash positions
and match jumps. It returns a valid multi-match plan. `Vox_lz4_streaming.scan`
relates the mutable hash table to its immutable association-list model and
proves that emitted bytes equal `Vox_lz4_general_encode.encode_model` for that
plan. No plan is built at runtime. Its offset splitter uses integer division
and proves equality with the total offset model.

`Vox_lz4_string_decode` proves its parser, literal copies, and overlapping match
copies agree with `Vox_lz4_packed.decode_model`, including the final status,
byte count, and buffer state. `Vox_lz4_decode_bytes_proof` proves that this
heap model and the pure decoded-byte model have identical statuses, counts
and output-byte observations in both directions. The independent pure
plan/wire proof in `Vox_lz4_decode_bytes_roundtrip` reconstructs the source
and proves extensional byte equality without an allocation witness.

The model distinguishes malformed input from output-limit failures. Detailed
error reasons and input positions are checked against the original decoder
by differential tests.

## Runtime boundary

The trusted boundary contains the owned-array and raw-memory primitive
contracts, standard string/character primitive semantics, and the copy from
initialized raw bytes into a new GC string. String contents are a ghost view;
no input iarray is allocated. Character conversion records the usual
`Char.chr (Char.code c) = c` law.

The output is copied once. `caml_raw_memory_copy_string` roots the source
handle across GC allocation, reacquires its raw pointer, and copies the
initialized prefix. The raw buffer is then freed. Malloc storage is never
reinterpreted as a GC array or string.

Allocation failure during scanning or decoding consumes ownership authority.
The unreachable raw carrier now has a GC finalizer that releases abandoned
storage; handlers do not restore the input token. Normal returns and final-copy
failures still release storage explicitly. Explicit release clears the pointer,
so later finalization does not free it twice. External byte counts are registered
with the collector, and the carrier is allocated in the major heap so explicit
release cannot leave stale young-generation accounting.

Reclamation after an exceptional exit occurs when the carrier is collected;
there is no prompt-cleanup guarantee. This runtime lifetime mechanism is trusted,
and the normal-return theorem still makes no exception-safety or termination
claim about the mutable entrypoints.

Native lowering turns integer-table access and raw byte access into loads and
stores. Bytecode uses the C primitives. Both implement the same contracts;
the native lowering is part of the compiler's trusted implementation.

## Checks and benchmark

`lz4_fast_reference.ml` compares the public compressor against the original
codec, two scan references, and the checked encoder on 1,004
inputs, including source code. It also exercises a 65,536-byte incompressible
input across a major GC. `lz4_fast_decoder_reference.ml` compares output and
exact diagnostics on 6,000 generated blocks. `lz4_codec.ml` covers malformed
blocks, overlapping matches, dense matches, and maximum-size inputs. The
interoperability script checks both directions against liblz4.

The original implementation is retained only as
`testsuite/tests/vox/vox_lz4_baseline.ml`. Build the native library with
`make -s vox-library`, then benchmark using the installed compiler:

```sh
_install/bin/ocamlopt -O3 -nostdlib -I _install/lib/ocaml \
  -I _build/vox-library -I testsuite/tests/vox \
  -extension refinement_types -o /tmp/lz4_throughput \
  _build/vox-library/vox_borrow.cmxa \
  testsuite/tests/vox/vox_lz4_baseline.ml \
  verification/benchmarks/lz4_throughput.ml
/tmp/lz4_throughput typing/typecore.ml 500
```

Compression ratios and throughput are measured properties; round-trip
correctness is proved. In particular, the specification does not claim every
input becomes smaller.

A paired native run on 2026-09-25 measured the following (decimal MB/s,
CPU time; 1,000 iterations for `typecore.ml`, 5,000 for `vox_lz4_roundtrip.ml`):

| Source | Input bytes | Encoded bytes | Baseline compression | Verified public compression | Verified decompression |
| --- | ---: | ---: | ---: | ---: | ---: |
| `typing/typecore.ml` | 647,023 | 275,179 | 250.7 MB/s | 263.6 MB/s | 413.2 MB/s |
| `verification/library/vox_lz4_roundtrip.ml` | 47,023 | 13,097 | 464.8 MB/s | 286.5 MB/s | 608.1 MB/s |

Encoded bytes matched the baseline exactly. The verified path remains slower
on the smaller input; these measurements do not claim universal performance
parity. Decoder throughput counts decompressed bytes.
