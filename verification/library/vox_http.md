# Incremental HTTP/1.1 request demo

`Vox_http.feed` executes the byte state machine. `parse` executes the same
machine from its initial state and exposes exact byte accounting in its result
refinement, together with equality to `feed (initial ()) input`. Proofs call that implementation; there is no runtime certificate,
trace, or final result checker. All correctness-only work in `parse` is inside
`ghost_`.

## Supported wire language

Bytes are integers from 0 through 255. A request contains a request line,
zero or more header lines, an empty line, and a Content-Length body. Lines end
in exactly CRLF. The request line is `method SP target SP HTTP/1.1`: method is a
nonempty HTTP token, and target is a nonempty sequence of ASCII bytes 33–126.
The target is opaque: URI interpretation, routing, and method semantics belong
to the application.

Header names are nonempty HTTP tokens immediately followed by a colon. Values
may contain HTAB, SP, visible ASCII, or bytes 128–255. Folding, bare LF, embedded
CR, whitespace before the colon, and other controls are rejected. Names are
case insensitive for framing. Leading/trailing SP and HTAB are ignored when
interpreting framing values. Exactly one nonempty Host value is required;
Host authority syntax is outside this demo's scope.

Absent Content-Length means an empty body. Content-Length is a nonempty decimal
sequence, with leading zeros allowed. Repeated fields are accepted only when
their numeric values agree. Comma-separated lengths are rejected. All
Transfer-Encoding fields are unsupported, including `identity`; the presence
of both Transfer-Encoding and Content-Length has its own ambiguity error.
The body is opaque and may contain any byte, including CRLF and NUL.

The public request representation preserves the original request-line and
header bytes, field order, repeated identical lengths, casing, and whitespace.
`request_parts` exposes method and target; `header_field` exposes a validated
lowercase name and trimmed value. `serialize` inserts CRLF delimiters and
appends the body. It does not invent or correct headers. `well_formed` independently requires request-line grammar,
header grammar, consistent supported framing, matching body length, valid body
bytes, and sufficient message budget. Thus roundtrip is exact record equality;
no normalization is needed. Callers construct headers consistent with their
body before serializing.

## Streaming and errors

`initial ()` starts one request with 16,384 bytes of budget. `feed state chunk`
returns a new state and the exact unconsumed suffix of that chunk. `Line` and
`Body` mean incomplete input; retain the state and feed another chunk.
`Complete request`, `Malformed reason`, and `Limit resource` are terminal.
Feeding a terminal state consumes nothing. Start another initial state on the
returned suffix to parse a pipeline. An empty chunk never declares EOF; an
application that reaches EOF while incomplete must report truncation.

Each consumed byte costs one budget unit, including a byte that exposes a
syntax error. The byte after the final body byte (or final header CRLF for an
empty body) is never examined. A request completing on byte 16,384 succeeds.
A still-incomplete state with zero budget returns `Limit Message_bytes` when
another byte arrives, leaving that byte unconsumed. A Content-Length over
8,192 returns `Limit Body_bytes`. Decimal accumulation checks the bound before
multiplication, so arbitrarily long decimal input cannot overflow it.
Resource failures are separate constructors from malformed input.

Error precedence is deterministic: the first encountered line syntax error
wins; at the header terminator TE/CL ambiguity precedes unsupported TE, then
fields are checked in order for Host and Content-Length errors. Once a decimal
prefix exceeds the body limit, its remaining characters are not interpreted.
Limits therefore bound accepted input; they do not certify the syntax of input
beyond the bound.

## Direct laws

- `roundtrip request suffix`: for every `well_formed request` and arbitrary
  suffix, feeding `serialize request @ suffix` returns exactly `request` and
  exactly `suffix`. Consumed bytes equal the serialized length. Instantiating
  the suffix with another request proves request separation.
- `request_separation state prefix suffix`: once a prefix completes a request,
  appending any suffix preserves that completed state and appends the suffix
  to the unconsumed bytes. This holds for arbitrary accepted input.
- `chunking_invariance state a b`: feeding `a @ b` equals feeding `a`, then
  feeding the first result's unconsumed suffix followed by `b`. This applies
  to incomplete input, completion, malformed input, and limits. Induction
  extends it to any finite chunk partition.
- `accounting state input`: for budgets between zero and 16,384, the budget
  difference equals the input length minus the returned suffix length;
  dropping exactly that many bytes yields the returned suffix.
- `suffix_preservation`: concatenating the consumed prefix and returned suffix
  reconstructs the original input.

The serializer proof uses induction over actual byte transitions, lines,
headers, and body. Its unbudgeted `drain` helper is connected to `feed` by
`feed_matches_drain` under a proved sufficient-budget condition. Neither helper
is invoked by the runtime parser. There are no added axioms or trusted HTTP
primitives. The trust boundary is Vox, its existing sequence contracts, and Z3.
The public proof scope is the laws above; a converse theorem that every
completion on arbitrary input yields a `well_formed` request is not provided.
Malformed-input classification is covered by the executable validators and
focused tests.

## Running

From the configured worktree:

```sh
./dev test vox/http_parser.ml
./dev test vox/http_suffix_rejected.ml
verification/demos/http_stream.sh
```

This verifies and executes the focused fixture in bytecode and native modes.
The rejected fixture prevents a client from claiming that parsing discards a
pipelined suffix. The streaming script builds with the installed compiler and
prints each incomplete/completed state. The positive fixture includes a
two-request stream split inside CRLF, a header name, and the body;
every two-chunk split of a pipeline; byte-at-a-time input; every incomplete
prefix; malformed request lines and headers; framing ambiguity; and exact
message/body limits.

This is a bounded functional demonstration, not a production HTTP server.
It uses persistent forward lists, so appending individual bytes has quadratic
allocation/work within a line or body. Its bounds do not cover OCaml stack or
heap exhaustion. It supplies no socket transport, response parser, chunked
encoding, URI/Host semantic validation, or application request policy.
