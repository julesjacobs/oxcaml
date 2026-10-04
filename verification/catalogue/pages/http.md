title: Incremental HTTP request parser
blurb: A total, chunk-by-chunk HTTP/1.1 request parser proved sound and complete for a specified request grammar, with exact byte accounting and chunking invariance.
status: owner-review
date: 4 October 2026
sources:
  - verification/library/vox_http.mli — Public interface
  - verification/library/vox_http_spec.ml — Request grammar, framing, serialization and `well_formed`
  - verification/library/vox_http_spec.mli — The same definitions as checked equations
  - verification/library/vox_http_model.ml — Complete pure incremental semantics
  - verification/library/vox_http.ml — Implementation and proofs; the executable parser is `Driver`
  - verification/library/vox_http.md — Supported subset, error conventions and reading order
  - verification/library/vox_sequence.mli — List operations used by the contracts
  - testsuite/tests/vox/http_parser.ml — Client proofs and runtime tests
  - testsuite/tests/vox/http_body_rejected.ml — Rejected client
  - testsuite/tests/vox/http_suffix_rejected.ml — Rejected client
  - testsuite/tests/vox/http_private_rejected.ml — Rejected reference to the implementation module
  - testsuite/tests/vox/http_rejection_rejected.ml — Rejected claims about malformed input
  - verification/demos/http_stream.ml — Streaming example
---
`Vox_http` parses one HTTP/1.1 request at a time from input delivered in chunks. A parser state is abstract; `feed state chunk` returns the next state and the unconsumed rest of the chunk, and `status` reports `Incomplete`, `Complete request`, `Malformed reason` or `Limit resource`. Input is a list of integers, one per byte. `Vox_http_spec` defines the requests the parser must complete: `well_formed request` requires a valid request line and header lines, supported framing, a body of the framed length, and a serialized size of at most 16,384 bytes. The interface proves, for every state and input:

- Soundness: a `Complete request` is `well_formed`, and its serialization is exactly the bytes consumed since `initial ()`.
- Completeness: feeding `serialize request` followed by any suffix to `initial ()` gives `Complete request` and returns exactly the suffix, for every `well_formed request`.
- Chunking: feeding `a @ b` gives the same state and rest as feeding `a` and then the rest of `a` followed by `b`.
- Accounting: each call consumes a prefix of its input, a state never consumes more than 16,384 bytes, an `Incomplete` result consumes the whole chunk, and a terminal state consumes nothing.
- Framing errors: feeding a valid request line and valid header lines that fit the budget gives the outcome `framing` prescribes, for example `Malformed Transfer_encoding_content_length`.
- Line errors: a request line that fails `valid_request_line`, followed by CRLF, gives `Malformed Invalid_request_line` as soon as its CRLF is consumed, and the rest returned is exactly the input after that CRLF. After a valid request line and valid header lines, the first nonempty header line that fails `valid_header` gives `Malformed Invalid_header` in the same way. Both laws cover lines whose bytes are in 0–255 and include no CR or LF, and need the input up to that CRLF to fit the 16,384-byte budget. An `Incomplete` state with budget left that is fed a value outside 0–255 gives `Malformed Invalid_byte` without consuming anything after it.

`feed` and `parse` are `total`: they terminate without raising. The public
pure model fixes every byte transition, including `Invalid_crlf` for a bare
LF or a CR followed by a byte other than LF, and `Limit Message_bytes` when
an incomplete parser has no budget left and receives another byte. The
exhausted-budget byte is returned unconsumed; an empty chunk leaves the
state unchanged. A terminal state also returns the entire chunk unchanged.
The executable parser keeps lists reversed and proves agreement with this
forward-order model. There is no cost theorem.

## Interface

The grammar, framing and serialization definitions come first:

@code verification/library/vox_http_spec.ml "let[@def] well_formed request =" "  && fits 16384 (serialize request)"

The [complete grammar definitions](src:verification/library/vox_http_spec.ml) define request lines, headers, framing and serialization.

The pure incremental model specifies accumulation, CRLF transitions, error
priority, the budget and the exact unconsumed suffix. It uses those grammar
functions directly; it contains no reachability or storage invariant:

@code verification/library/vox_http_model.ml

The parser exposes this semantic state through the erased observation
`model`. `initial` and `feed` state exact model equations. The observation
equations connect `status`, `total_consumed` and `processed` to the model;
the derived laws follow the operations in the full interface.

@code verification/library/vox_http.mli "open Vox_http_spec" "(** {2 Derived observation laws} *)"

[The full interface](src:verification/library/vox_http.mli) contains the derived observation, chunking, grammar and rejection laws.

`bytes` is `int list`. `processed` is the bytes consumed for the current
request, or `[]` after an error. `total_consumed` counts consumed bytes even
after an error, and `consumed before after` is their difference. `S` is
`Vox_sequence`, whose `length` is a `Bigint.t`. `vox_http_spec.mli` exports checked equations
for the grammar definitions. The protocol names appear as byte lists;
`framing` states the rejection priority directly.

## Trusted base

Nothing beyond the shared base.

## Scope

- Operations: `initial`, `feed`, `parse`, the observations `status`, `total_consumed`, `consumed` and `processed`, and the laws. `request_separation` follows from `chunking_invariance` and `terminal_preservation`; `framing_rejection` is a fact about `framing` alone. `header_prefix headers tail`, used by `header_rejection`, is the header lines, each followed by CRLF, then `tail`.
- Supported requests: a request line `method SP target SP HTTP/1.1` and header lines, each ending in CRLF, exactly one nonempty Host field, and a body framed by Content-Length (absent means empty). A Content-Length value is decimal digits, leading zeros allowed; repeated Content-Length fields must agree. Every Transfer-Encoding is rejected. There is no chunked encoding, URI or Host syntax, response parsing or transport.
- Limits: 16,384 bytes per request, including the body; a Content-Length above 8,192 gives `Limit Body_bytes` unless `framing` finds an earlier error.
- Every status and returned suffix is specified by the pure model. Budget exhaustion takes priority over inspecting another byte; otherwise an invalid byte takes priority over CRLF handling.
- Input and accumulated requests are OCaml lists of integers. The laws return their results `@ ghost`, so their bodies are erased; each still compiles to a small function that returns a placeholder.
- An empty chunk does not signal end of input; a caller that reaches end of input with an `Incomplete` state must report truncation itself.

## Client example

From the positive client, which sees only the interfaces. `(f @ total)` declares that `f` terminates without effects, and `{result : result | p}` is the type `result` refined by the predicate `p`. `===` is logical equality. `ghost_ (...)` is proof code, checked and then erased; here it calls the laws `roundtrip` and `body_agreement`, whose result types carry the facts. Laws are stated as `if premise then conclusion else true`, and `S` is `Vox_sequence`.

@code testsuite/tests/vox/http_parser.ml "let (decode_serialized @ total)" "| _ -> result"

The line-error laws are used the same way. This client proves that an invalid request line is rejected with `Invalid_request_line`, leaving exactly the bytes after it:

@code testsuite/tests/vox/http_parser.ml "let (reject_request_line @ total)" "  result"

## A rejected program

The contracts do not let a client prove false properties of completed requests. This client claims that every complete request has an empty body:

@code testsuite/tests/vox/http_body_rejected.ml "let (discard_body @ total)" "  ()"

@text testsuite/tests/vox/http_body_rejected.compilers.reference

`http_suffix_rejected.ml` similarly fails to prove that parsing discards a pipelined suffix, and `http_private_rejected.ml` cannot reach the implementation module `Vox_http.Internal`. `http_rejection_rejected.ml` also rejects a bare LF classified as `Invalid_header`, using the exact model contract. Its other phrases check that the line-error laws prove nothing false: it fails to prove that an invalid request line gets `Invalid_header`, that a valid one is rejected, that the error comes before the line feed, that an invalid line leaves the parser `Incomplete`, that a header error is reported where stated when an earlier header line may be invalid, or that a byte in 0–255 is rejected.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/http_parser.ml vox/http_body_rejected.ml vox/http_suffix_rejected.ml vox/http_private_rejected.ml vox/http_rejection_rejected.ml vox/http_stream_demo.ml
```

`http_parser.ml` checks and compiles the specification, the implementation and the client proofs, then runs the client's tests as bytecode: every two-chunk split of a pipeline, byte-at-a-time input, malformed lines, framing errors, the exact message and body limits, and the budget running out inside a header line and in the body. The three rejected clients are compiled against the grammar and parser interfaces plus the pure model; `http_rejection_rejected.ml` builds the library from source, checks its seven claims against the interfaces and compares the errors with those recorded in the file. `http_stream_demo.ml` builds and runs the streaming example and compares its output with `http_stream_demo.reference`.
