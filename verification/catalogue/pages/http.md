title: Incremental HTTP request parser
blurb: A total, chunk-by-chunk HTTP/1.1 request parser proved sound and complete for a specified request grammar, with exact byte accounting and chunking invariance.
status: review-pending
date: 27 September 2026
sources:
  - verification/library/vox_http.mli — Public interface
  - verification/library/vox_http_spec.ml — Request grammar, framing, serialization and `well_formed`
  - verification/library/vox_http_spec.mli — The same definitions as checked equations
  - verification/library/vox_http.ml — Implementation and proofs; the executable parser is `Driver`
  - verification/library/vox_http.md — Supported subset, error conventions and reading order
  - verification/library/vox_sequence.mli — List operations used by the contracts
  - testsuite/tests/vox/http_parser.ml — Client proofs and runtime tests
  - testsuite/tests/vox/http_body_rejected.ml — Rejected client
  - testsuite/tests/vox/http_suffix_rejected.ml — Rejected client
  - verification/demos/http_stream.ml — Streaming example
---
`Vox_http` parses one HTTP/1.1 request at a time from input delivered in chunks. A parser state is abstract; `feed state chunk` returns the next state and the unconsumed rest of the chunk, and `status` reports `Incomplete`, `Complete request`, `Malformed reason` or `Limit resource`. Input is a list of integers, one per byte. `Vox_http_spec` defines the requests the parser must complete: `well_formed request` requires a valid request line and header lines, supported framing, a body of the framed length, and a serialized size of at most 16,384 bytes. The interface proves, for every state and input:

- Soundness: a `Complete request` is `well_formed`, and its serialization is exactly the bytes consumed since `initial ()`.
- Completeness: feeding `serialize request` followed by any suffix to `initial ()` gives `Complete request` and returns exactly the suffix, for every `well_formed request`.
- Chunking: feeding `a @ b` gives the same state and rest as feeding `a` and then the rest of `a` followed by `b`.
- Accounting: each call consumes a prefix of its input, a state never consumes more than 16,384 bytes, an `Incomplete` result consumes the whole chunk, and a terminal state consumes nothing.
- Framing errors: feeding a valid request line and valid header lines that fit the budget gives the outcome `framing` prescribes, for example `Malformed Transfer_encoding_content_length`.

`feed` and `parse` are `total`: they terminate without raising. Nothing requires a malformed request line or header line to be rejected, or says which error it gets: a parser that reports `Incomplete` on such input until it has consumed 16,384 bytes, and then reports any error, satisfies every law. For such input the error reason and when it is reported are implementation behavior, tested but not specified. The parser calls the specification's own `valid_request_line`, `valid_header` and `framing` at run time, so the line grammar is correct because it is the specification; the proofs cover the byte-level machine around it (CRLF handling, accumulation, the budget, chunking and serialization). There is no cost theorem.

## Client example

From the positive client, which sees only the interfaces. `(f @ total)` declares that `f` terminates without effects, and `{result : result | p}` is the type `result` refined by the predicate `p`. `===` is logical equality. `ghost_ (...)` is proof code, checked and then erased; here it calls the laws `roundtrip` and `body_agreement`, whose result types carry the facts. Laws are stated as `if premise then conclusion else true`, and `S` is `Vox_sequence`.

@code testsuite/tests/vox/http_parser.ml "let (decode_serialized @ total)" "| _ -> result"

## A rejected program

The contracts do not let a client prove false properties of completed requests. This client claims that every complete request has an empty body:

@code testsuite/tests/vox/http_body_rejected.ml "let (discard_body @ total)" "  ()"

@text testsuite/tests/vox/http_body_rejected.compilers.reference

`http_suffix_rejected.ml` similarly fails to prove that parsing discards a pipelined suffix, and `http_private_rejected.ml` cannot reach the implementation module `Vox_http.Internal`.

## Interface

@code verification/library/vox_http.mli

`bytes` is `int list`. `processed` is an erased observation: the bytes consumed so far for the current request, or `[]` after an error. `total_consumed` counts them at run time, and `consumed before after` is the difference. `Vox_sequence.length` returns a `Bigint.t`, hence the `Bigint.of_int` conversions. `transition_def` states the meaning of the erased relation `transition` that `feed` satisfies. `vox_http_spec.mli` states each definition of the grammar as an equation (`token_def` and so on), checked against `vox_http_spec.ml`, which is easier to read. `well_formed` is:

@code verification/library/vox_http_spec.ml "let[@def] well_formed request" "fits 16384 (serialize request)"

The specification writes protocol strings as byte lists: `[72; 84; 84; 80; 47; 49; 46; 49]` is `HTTP/1.1`, and the two lists in `framing` are `transfer-encoding` and `content-length`.

## Trusted base

Nothing beyond the shared base.

## Scope

- Operations: `initial`, `feed`, `parse`, the observations `status`, `total_consumed`, `consumed` and `processed`, and the laws. `request_separation` follows from `chunking_invariance` and `terminal_preservation`; `framing_rejection` is a fact about `framing` alone.
- Supported requests: a request line `method SP target SP HTTP/1.1` and header lines, each ending in CRLF, exactly one nonempty Host field, and a body framed by Content-Length (absent means empty). A Content-Length value is decimal digits, leading zeros allowed; repeated Content-Length fields must agree. Every Transfer-Encoding is rejected. There is no chunked encoding, URI or Host syntax, response parsing or transport.
- Limits: 16,384 bytes per request, including the body; a Content-Length above 8,192 gives `Limit Body_bytes` unless `framing` finds an earlier error.
- Rejection of syntax errors (bytes outside 0–255, bare LF, invalid request or header lines) and the choice between error reasons are not specified; only the framing errors above are.
- Input and accumulated requests are OCaml lists of integers. The laws return their results `@ ghost`, so their bodies are erased; each still compiles to a small function that returns a placeholder.
- An empty chunk does not signal end of input; a caller that reaches end of input with an `Incomplete` state must report truncation itself.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/http_parser.ml vox/http_body_rejected.ml vox/http_suffix_rejected.ml vox/http_private_rejected.ml
verification/demos/http_stream.sh
```

`http_parser.ml` checks and compiles the specification, the implementation and the client proofs, then runs the client's tests as bytecode: every two-chunk split of a pipeline, byte-at-a-time input, malformed lines, framing errors and the exact message and body limits. The three rejected clients are compiled against the `.mli` files only. `http_stream.sh` builds and runs a streaming example.
