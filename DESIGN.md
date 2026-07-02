# OxCaml Refinement Verification Extension — Design Document

Status: prototype design, decisions below are SETTLED unless marked OPEN.
The implementing model must not re-litigate settled decisions. If a settled
decision proves unimplementable, STOP and report; do not silently substitute
an alternative.

## 1. Overview

A verification extension for OxCaml based on:
- **Refinement types**: `{v:τ | p}` where `p` is a predicate over the
  *verifier type* (SMT sort) corresponding to the OxCaml type `τ`.
- **A pi type**: `Π(x:τ).σ` — a dependent arrow where the *refinements* in
  σ may mention the logical reflection of `x`. σ's structure may NOT depend
  on `x` (refinement-level dependency only). This keeps checking decidable
  and erasure trivial.
- **SMT discharge**: every subtyping check between refined types with the
  same skeleton becomes an implication query sent to Z3.

Architecture is Liquid Types (Rondon/Kawaguchi/Jhala): variable-only
dependent application in the core, ANF elaboration at the surface, so
terms never appear inside types — only logic variables.

## 2. Backwards compatibility (hard requirement)

- Unannotated programs must typecheck and compile exactly as before.
  The full existing OxCaml testsuite must pass at EVERY step of the plan.
- Pi types never arise unless written by the user.
- Refinement payloads are invisible to unification and all legacy code.

## 3. Surface syntax

- Refinement: `{v:int | p}`. The bound value variable is always written
  `v` in surface syntax.
- Logical reflection of a program variable `x` is written `x#` INSIDE
  refinement braces only. OPEN: `#` clashes with OxCaml's unboxed-type
  lexing (`float#`, `#(a,b)`). Step C1 resolves this; acceptable outcomes
  are (a) `x#` works inside `{...}` via lexer state, or (b) a different
  sigil, chosen at C1 and used consistently after.
- Pi arrow surface syntax: `(x : τ) -> σ` (dependent arrow, OCaml-ish),
  printed the same way. `Π` is used in this document as notation only.
- Kind annotation: `type t : value with verifier Int` (rides the existing
  jkind annotation syntax).
- `assume e` — unchecked ascription (VC skipped). Ordinary ascription
  `(e : {v:τ|p})` is the checked introduction form.
- Measures: `measure len : 'a list -> int` — a signature-level declaration
  of a logical function symbol. Trusted (axiomatized), not checked, in the
  prototype.

## 4. Logic layer

- **Predicate AST**: immutable tree. Boolean connectives, comparisons,
  linear integer arithmetic, applications of measure symbols, logic
  variables. The refinement's value variable `v` is a `Bound` de Bruijn
  index WITHIN the predicate tree. All other variables are free and
  identified by **Ident stamp** (see §6).
- **Sort AST**: `Int`, `Bool`, uninterpreted sorts (globally unique ids),
  sort variables (for type parameters), and constructor sorts as needed
  later (e.g. `ListSort(s)`).
- **Well-sortedness** is a real judgment: a predicate is checked against a
  sort environment at PARSE/elaboration time, not at VC time. Example:
  `{v:t | v + 1 > 0}` is an error when `t`'s sort is uninterpreted.
- **VC module**: interface is `check : hypotheses:pred list -> goal:pred
  -> [Valid | Invalid | Unknown of reason]`. Serializes to SMT-LIB2,
  drives a Z3 subprocess, parses sat/unsat/timeout. Timeouts are
  `Unknown`, reported as verification failures with a distinct message.
- `--dump-vc` flag prints every generated VC (hypotheses, goal, source
  location) — must exist from the first VC onward.

## 5. Verifier sorts as a kind component

- Add a **verifier-sort field to jkinds**. Do not build a parallel kind
  system; reuse jkind inference/defaulting, signature inclusion, .cmi
  serialization, and annotation syntax.
- **Defaults**: builtins get structural sorts (`int` → SMT `Int` for the
  prototype — the unbounded-integer choice, overflow-unsound, per-module
  soundness knob deferred; `bool` → `Bool`). Every other type declaration
  gets a FRESH uninterpreted sort. Consequence: every immutable binder is
  reflectable, worst case with equality/congruence reasoning only.
- **Signature matching direction**:
  - Sig `type t : value` (no sort annotation) over impl `type t = int`
    ⇒ the sort is SEALED: clients see a fresh uninterpreted sort.
    Clients must NOT be able to prove arithmetic facts about `t`.
  - Sig `type t : value with verifier Int` ⇒ inclusion requires the
    implementation's sort to EQUAL Int.
- **Type parameters** map to sort variables; `Ctype.copy`/instantiation
  instantiates sort variables inside refinements.
- Measures are NOT part of kinds; they are value-namespace-like logical
  declarations scoped by signatures.

## 6. Representation (settled — do not deviate)

- **Refinements and pi binders ride `Tarrow` and type payloads, not a new
  type constructor.** Concretely: an optional payload
  `(Ident.t option * refinement option)` on arrows (binder present iff
  the arrow is dependent), and a refinement payload slot on base-type
  nodes. Unification IGNORES payloads (merge policy: at unification of
  two arrows, payload handling is not unification's job; the refinement
  checker consults payloads separately). Rationale: `typecore`/`ctype`
  pattern-match on `Tarrow` everywhere; a sibling constructor would
  require auditing every site. Precedent: modes and jkinds.
- **Pi binders are stamped `Ident.t`s**, freshened during `Ctype.copy`.
  Never de Bruijn at the type-graph level: `type_expr` is a mutable
  shared graph, so binder depth is ill-defined.
- **The refinement value variable `v` is a Bound index** inside the
  immutable predicate tree (no freshening on copy needed for it).
- **Hash variables are keyed by Ident stamp, not name.** `x#` is surface
  syntax resolving through the normal environment to a stamped ident;
  its logical counterpart is a function of the stamp. Shadowing must be
  handled by stamps.
- **Reflectability**: a binder gets a logical counterpart iff it is
  immutable, not `contended`, and its type has a verifier sort (which,
  given the uninterpreted default, is nearly always). `mutable`/`ref`
  bindings have NO counterpart in the prototype (soundness for state;
  strong update under `unique` is future work).

## 7. Checking algorithm

Bidirectional, prototype-grade:

- **Synthesis** for variables, applications, annotated terms.
  Variable occurrences are **selfified**: occurrence of `y` synthesizes
  `{v | v = y#}` conjoined with (or in place of) the declared refinement.
- **Checking** for lambdas (against pi: bind the binder's stamp into the
  logical environment, check the body) and for branches of `if`/`match`
  (each branch checked against the expected type under its path
  condition; synthesizing a type FOR a conditional requires an
  annotation — that is acceptable for the prototype).
- **Logical environment**: every in-scope reflectable binder
  `x : {v|p}` contributes hypothesis `p[v := x#]` to all VCs in scope.
  Elimination is implicit; there is no unpack construct.
- **Path conditions**: `if c then e1 else e2` checks `e1` under the
  reflection of `c` and `e2` under its negation (requires an
  expression→predicate reflection mini-translator for the supported
  fragment: variables, literals, comparisons, boolean ops). `match`
  branches add tag facts and pattern-binder equations; `option` and
  `list` (with a hardcoded `len` measure) are built in for the prototype.
- **Subtyping**: same skeleton required; refinements discharge via VC.
  `Π(x:τ).σ ≤ τ' → σ'` holds by forgetting the dependency (contravariant
  arg check, weaken result). Plain arrow lifts to trivial pi for free.
- **Application default**: if the function's type is not manifestly a pi,
  assume a plain arrow. No dependency inference.
- **Dependent application** in the core is VARIABLE-ONLY:
  `(Π(x:τ).σ) y ⇒ σ[x# := y#]`, a capture-avoiding stamp-for-stamp
  rename. No term substitution machinery exists anywhere.
- **ANF elaboration**: the elaborator rewrites `f e` (f manifestly
  pi-typed, e not a variable) to `let tmp = e in f tmp`. Surface language
  stays unrestricted.
- **Escape check**: at generalization, walk refinements of the
  generalized type; if any free stamp's binder is out of scope, ERROR
  (never silently weaken to `true` — unsound in argument positions).

## 8. Module boundaries

- Signatures carry refinements and sort-annotated kinds; both round-trip
  through .cmi.
- Calls into unannotated modules are implicitly `assume`d at the
  boundary (their declared OxCaml types taken with trivial refinements).
- Lemma functions are ordinary functions with unit-refined results;
  calling one introduces its postcondition — no new syntax.

## 9. Invariants the implementation must never break

1. Full upstream testsuite green after every step.
2. No term ever appears inside a type; only stamped logic variables.
3. Unification never inspects refinement payloads.
4. Stamps, never names, identify logical variables.
5. Ill-sorted refinements are parse/elaboration-time errors.
6. Escape ⇒ error, never silent weakening.
7. `Unknown`/timeout from the solver ⇒ verification failure, never pass.

## 10. Explicitly deferred (do not implement)

Refinement inference (Horn solving), overflow-sound bitvector ints,
strong update via uniqueness, measure realizability checking, floats and
other unboxed sorts, first-class function reflection, GADTs/existentials
beyond what falls out, error-message polish beyond locations.
