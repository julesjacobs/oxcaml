# Incremental HTTP/1.1 request demo

`Vox_http.feed` executes the byte state machine. `parse` executes the same
machine from its initial state and exposes exact byte accounting, together
with equality to `feed (initial ()) input`. Completion also guarantees
`well_formed`, serialization of the exact consumed prefix, and reconstruction
of the input from serialization and the remaining suffix. Proofs call that
implementation; there is no runtime certificate, trace, or final result
checker. All correctness-only work in `parse` is inside `ghost_`.

## Human review boundary

Read these files in order; this is the complete semantic review surface:

1. [`vox_http_spec.mli`](vox_http_spec.mli): request and outcome types and
   complete checked definition equations for every character, token, line,
   header and decimal operation; framing and its error precedence;
   serialization; byte/body/message bounds; `well_formed`; and the complete
   recursive meaning of `content_lengths_match`. Every semantic helper has a
   defining equation here. [`vox_http_spec.ml`](vox_http_spec.ml) implements
   those equations and is checked against this signature; no additional
   semantic definition is hidden in its body. Neither file contains parser
   states, invariants, induction helpers, or implementation aliases.
2. [`vox_http.mli`](vox_http.mli): the sealed executable API, state observations,
   the complete checked `transition_def` equation shared by `feed` and `parse`,
   and all public laws. Every predicate refers to definitions in the first
   file or to the observation laws here. `state` is abstract. Its only
   constructors are `initial` and `feed`; callers supply ordinary input bytes,
   never an invariant or proof object.
3. [`vox_sequence.mli`](vox_sequence.mli): the `t` alias and the complete
   `length_def`, `append_def`, `take_def`, and `drop_def` characterizations,
   together with their operation signatures. These are the only sequence
   operations used by public HTTP claims and semantic definitions. Sequence
   induction lemmas and their implementations do not change those meanings.
4. [`stdlib/bigint.mli`](../../stdlib/bigint.mli): unbounded signed integers,
   signed `of_int`, addition, subtraction and numeric comparison, used by
   sequence characterizations and consumed-byte equations. Ordinary byte and
   budget arithmetic uses Vox's signed 63-bit machine-integer semantics.
5. This document: supported subset, error and resource conventions, totality
   convention, and the trusted boundary described below.

`total_consumed` counts bytes spent on the current request, starting at zero;
`consumed before after` is the difference. The ghost observation `processed`
means the exact bytes spent while a state is incomplete or complete. `initial`
fixes it to the empty sequence, and `feed` fixes each live successor to the
old sequence followed by the newly consumed prefix. `state_sound` relates its
length to the counter and a completed request's serialization. For malformed
or resource-limit states it is explicitly the empty sequence; per-call byte
accounting and the returned suffix remain exact. These equations characterize
the observations over all constructible states without exposing a parser
representation or storing a runtime history.

[`vox_http.ml`](vox_http.ml) is behind the checked interface: concrete phases,
accumulators, the reachable-state invariant, its transition proofs, and proof
helpers are private. `Vox_http.Internal` is inaccessible to clients. Both the
ordinary parser state and the sealed state type contain no proof certificate;
the latter is an erased refinement of the former. Public wrappers erase their
proof calls and invoke the same byte driver. Its line, body, and pending-header
accumulators use cons; lines, headers, and bodies are reversed once when each
is complete. Headers remain in wire order while receiving a body.

A private forward-state model retains the grammar and wire proofs. `Driver`
proves each byte transition and complete feed simulate that model, including
all errors, budget outcomes and suffixes. Reversal is involutive, so model
state equality implies driver state equality for the chunking law. Model
conversion and simulation calls occur only in ghost blocks on the parsing
path. No model aliases expose the implementation through the interface.

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
returns a new state and the exact unconsumed suffix of that chunk.
`status state = Incomplete` means incomplete input; its returned suffix is
empty, and the state can be fed the next chunk.
`Complete request`, `Malformed reason`, and `Limit resource` are terminal.
Feeding a terminal state consumes nothing. Start another initial state on the
returned suffix to parse a pipeline. An empty chunk never declares EOF; an
application that reaches EOF while incomplete must report truncation.

Each consumed byte costs one budget unit, including a byte that exposes a
syntax error. The byte after the final body byte (or final header CRLF for an
empty body) is never examined. A request completing on byte 16,384 succeeds.
An incomplete state that has consumed 16,384 bytes returns
`Limit Message_bytes` when another byte arrives, leaving that byte unconsumed. A Content-Length over
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

- `parse input`: for arbitrary input, a completed request satisfies
  `well_formed`. Its serialization is exactly the consumed prefix, and that
  serialization followed by the remaining suffix reconstructs the input.
- `feed state input`: preserves exact per-call accounting and the `processed`
  observation; completion guarantees `well_formed` and serialization of all
  consumed chunks. The implementation carries its invariant automatically
  behind the abstract state type. No public premise refers to that invariant.
- `body_agreement`: from `well_formed`, derive absence of Transfer-Encoding
  and a body length at most 8,192 matching every decoded Content-Length field.
  `content_lengths_match` in the semantic module defines the pointwise
  header condition.
  `no_content_length_body` proves that absent Content-Length means an empty
  body.
- `header_outcome`: for a syntactically valid header block within the message
  limit, feeding its bytes produces exactly the outcome prescribed by
  `framing`. `framing_rejection` specifies TE/CL ambiguity and unsupported TE
  outcomes. A separate compiled client composes these to prove rejection by
  the actual byte parser.
- `roundtrip request suffix`: for every `well_formed request` and arbitrary
  suffix, feeding `serialize request @ suffix` returns exactly `request` and
  exactly `suffix`. Consumed bytes equal the serialized length. Instantiating
  the suffix with another request proves request separation.
- `terminal_preservation`: feeding any terminal state preserves that state
  and returns all input unconsumed.
- `request_separation state prefix suffix`: once a prefix completes a request,
  appending any suffix preserves that completed state and appends the suffix
  to the unconsumed bytes. This holds for arbitrary accepted input.
- `chunking_invariance state a b`: feeding `a @ b` equals feeding `a`, then
  feeding the first result's unconsumed suffix followed by `b`. This applies
  to incomplete input, completion, malformed input, and limits. Induction
  extends it to any finite chunk partition.
- The complete public `transition_def` equation, shared by `feed` and `parse`,
  equates the consumed count with the input length minus
  the returned suffix length, identifies the suffix by `drop`, and reconstructs
  the input by concatenating its consumed prefix with that suffix.

The serializer proof uses induction over forward-model byte transitions, lines,
headers, and body, connected to the executable driver by simulation. Its unbudgeted `drain` helper is connected to `feed` by
`feed_matches_drain` under a proved sufficient-budget condition. Neither helper
is invoked by the runtime parser. There are no added axioms or trusted HTTP
primitives. The trusted boundary is Vox and Z3, including the existing sequence-length
primitive, mathematical-integer primitives, arithmetic encoding, refinement
checking, and ghost erasure. All new semantic definitions, the byte driver,
and their proofs are checked; no new axiom or primitive is introduced.
Accepted-input soundness uses a forward invariant and the driver simulation;
it does not assume the input came from `serialize` or check a final result.
The serializer roundtrip proof remains the complementary completeness law
for the supported well-formed requests.

## Running

From the configured worktree:

```sh
./dev test vox/http_parser.ml
./dev test vox/http_suffix_rejected.ml
./dev test vox/http_body_rejected.ml
./dev test vox/http_private_rejected.ml
verification/demos/http_stream.sh
```

This verifies and executes the focused fixture in bytecode and native modes.
The rejected fixtures prevent a client from claiming that parsing discards a
pipelined suffix or that every accepted body is empty, and reject access to
private implementation helpers. The separately compiled positive clients
import only `Vox_http_spec` and `Vox_http` (plus the public sequence interface).
They derive every former explicit feed/parse clause from `transition_def` and
prove soundness for arbitrary input, two-chunk reconstruction without
invariant arguments, serializer roundtrip, and byte-parser TE rejection. The
streaming script builds with the installed compiler and
prints each incomplete/completed state. The positive fixture includes a
two-request stream split inside CRLF, a header name, and the body;
every two-chunk split of a pipeline; byte-at-a-time input; every incomplete
prefix; malformed request lines and headers; framing ambiguity; and exact
message/body limits.

This is a bounded functional demonstration, not a production HTTP server.
Accumulator work and allocation are linear in the accumulated bytes and
headers: each is consed once and reversed once at completion. This is a source
complexity observation, not a verified cost theorem. Its bounds do not cover OCaml stack or
heap exhaustion. It supplies no socket transport, response parser, chunked
encoding, URI/Host semantic validation, or application request policy.
