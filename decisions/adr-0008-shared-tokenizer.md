# ADR-0008: Shared SMT-LIB lexer (`smt/lexical`)

Status: Accepted 2026-07-11. Adopted as a draft on `task/tokenizer` (board #123)
with the author directive "fix the token-boundary bug *family*, not the
instances"; formally accepted once the gate's `sexp.ml` migration onto the shared
lexer completed (the accepting condition below is satisfied at trunk
`0c4c1744d8`).

## Context

Two independent, verified token-boundary bugs shipped in serialization code that
should have agreed:

- the oracle **cache-collision** exploit — a `|quoted symbol|` whose bytes could
  forge token boundaries in the canonical serialization (fixed by the netstring
  encoding in `tests/gate/canonical.ml`);
- the gate reader's **`|0|` confusion** (codex G1) — a quoted symbol whose text is
  a numeral (`|0|`) was indistinguishable from the numeral `0`, because both the
  gate's `sexp.ml` and `smt/smtlib`'s `sexp.ml` collapse every atom to
  `Atom of string`, discarding whether it was pipe-quoted.

Both are the same class: **a lexer that loses token kind at the boundary**. We had
*three* hand-rolled lexers (smtlib parser, gate reader, and the canonicalizer's
implicit token handling); two of the three had a verified boundary bug. Three
divergent lexers is the root cause.

## Decision

One shared lexer module, `smt/lexical` (library `oxsmt_lexical`), implementing the
SMT-LIB 2.6 §3.1 lexicon exactly, with a token type whose **headline invariant is
"token kind is never lost"**: a quoted `|0|` is `Symbol {text="0"; quoted=true}`,
distinct from `Numeral "0"`; a quoted `|let|` is a `Symbol`, distinct from the
`Reserved` word `let`. The printer's quoting decision and every reader's tokenizing
go through it.

**Scope is strictly lexical** — tokens only. Paren nesting (s-expressions),
command grammar, and term/sort semantics stay in each consumer. The lexer has zero
dependencies beyond the OCaml stdlib.

## Topology change (stated honestly)

`smt/lexical` is a new top-level library that BOTH `smt/smtlib` (printer + parser)
and `tests/gate` may link. This **breaks the gate's previous zero-`smt/`-deps
posture** (DESIGN.md §10 N-version isolation). The trade is deliberate: one
boundary-correct lexer beats three divergent ones when two of three had verified
boundary bugs. Lexing is not where N-version isolation earns its keep — the
oracle's independence lives in the *encoder* and the Lean *kernel*, which stay
untouched.

## Correlation risk and its mitigation (the honest part)

A shared lexer bug now hits the printer and every reader **symmetrically**, so a
print→parse round-trip cannot catch it (both sides share the flaw). That is
precisely the correlated-blind-spot failure DESIGN.md §10 warns about. It is
backstopped by three *uncorrelated* checks, none of which round-trips through the
shared lexer:

1. **The standing fuzzer's cross-implementation differential** — the shared lexer's
   tokenization is compared against a *second, independent* tokenizer. A
   disagreement is a finding, not a pass. (Status at acceptance: a from-spec
   differential is a deferred obligation, tracked as review finding F4 in the
   dual-review arc; the deferral is documented in `tests/gate/sexp.ml` rather than
   claimed as live.)
2. **Lean elaboration** — the gate encodes to Lean, whose kernel rejects an
   ill-formed goal regardless of how our lexer tokenized it.
3. **Pre-labeled public benchmarks** — QF_UF/QF_LIA/QF_UFLIA carry upstream
   verdicts; a lexer that silently mis-tokenizes shifts a label mismatch, an
   uncorrelated soundness signal.

## Sequencing

The gate's `sexp.ml` migration landed AFTER `task/gate3`'s minimal G1 fix and kept
every gate3 honeypot green; the lexer + `smt/smtlib` migration + fuzzer landed
first, and the gate migration rebased over gate3.

## Acceptance

Formal acceptance was conditioned on the gate `sexp.ml` migration completing — the
migration is what makes the shared lexer the single lexing authority the ADR
promises, so the ADR is not "accepted" until the last hand-rolled lexer is retired.
That condition is satisfied at trunk `0c4c1744d8` ("tokenizer-gate done: gate
sexp.ml onto shared lexer + cache-integrity hardening").

The migration was gated by the ADR-0007 dual review (same-model + blocking
cross-model codex), because `tests/gate/**` is a TCB path:

- Same-model adversarial arc: `logs/tokenizer-gate-review.md` (original review of
  the migration + the R1 cache-lookup regression it caught, and the re-verify of
  the fix rounds).
- Cross-model (codex) arc: `logs/codex-review/tokenizer-gate.md` (the full
  branch-delta TCB passes — the reader migration itself confirmed canonical-form-
  and verdict-routing-preserving and fail-closed; the residual cache false-Hit
  CRITICALs were bound into cache-entry integrity in a later fix round before the
  merge landed).

The reader migration is confined to the lexical layer; `reader.ml`, `canonical.ml`,
and `encoder.ml` are byte-identical across the migration, so the oracle's
encoder/kernel independence is preserved as §10 requires.

## Alternatives rejected

- *Keep three lexers, fix each bug locally* — the instance-not-family approach the
  directive explicitly overrides; leaves the next boundary bug latent in whichever
  lexer wasn't audited.
- *Share the whole s-expr reader too* — larger blast radius into the gate; the
  boundary bugs were all lexical, so the minimal shared surface is the lexer.
