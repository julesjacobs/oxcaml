# Vox VC generation

Refinement-flow made refined programs typecheck and left every obligation
recorded and accepted. Solver-interface built a term language, an obligation
type and two backends, with nothing producing obligations. This piece is the
translation both of them point at: a pass that walks the typed tree after
typechecking, assembles facts, collects the recorded obligations, lowers both
into `Vox_logic` terms, discharges them through a `Vox_backend`, and reports
failures. After this piece, `let v : int{ _ > 0 } = 0` can be *refuted*, not
just recorded.

The pass is a pure consumer. It adds no typing rules, no new `ctype` arms, no
side tables and no changes to what refinement-flow records: its inputs are
the durable records refinement-flow keeps — facts on `pat_type`, value
descriptions, apply arrows, and `Texp_field`'s retained label description;
obligations as `Texp_refinement_obligation` markers plus apply arrow domains
— and its output is exactly `Vox_logic.Obligation.t` handed to
`Vox_backend.discharge`. One further durable record the tree keeps,
`Texp_letop.bop_op_type`, is knowingly deferred (see the fact sources
below). Where this doc and those two docs disagree, those docs win.

vox2's VC subsystem (mapped in the research lane's report; spot-verified
against `~/oxcamls/vox2/main` at the lines cited below) is the evidence that
each mechanism here is implementable and is the source of several deliberate
lessons — the two-tier subject fallback, unconditional binder facts,
continue-past-failure — but no code is copied and its choices are treated as
evidence, not authority.

## Where the pass sits

Between typechecking and everything else, in the driver, not in `Typemod`.

The batch hook is `Compile_common.typecheck_impl`, beside the probe
(`driver/compile_common.ml:183`); the toplevel hook is
`Topcommon.typecheck_phrase`, again beside the probe
(`toplevel/topcommon.ml:259`). Those two call sites cover the bytecode and
native compilers (both drive `Compile_common`) and both toplevels
(`Topcommon` is shared). vox2 instead hooks inside
`Typemod.type_implementation` (vox2 `typing/typemod.ml:4514`); the driver
placement keeps `Typemod` free of solver concerns, and it costs nothing this
piece wants — `typecheck_impl` runs even under `-stop-after typing`, so
verification still covers type-only builds. `.mli`-only units have no
expressions and no obligations; the interface path gets no hook (sealing
obligations are a later piece, see out of scope).

The entry point:

    (* typing/vox_verify.mli *)
    val implementation :
      backend:(module Vox_backend.BACKEND) ->
      dump_only:bool ->
      config:Vox_backend.Config.t ->
      Typedtree.structure -> unit

It raises a located `Error` if the unit is refused (the protocol below). The
caller builds the arguments from three new flags, wired to
`Vox_backend.plan`, whose driver policy solver-interface specified and
deliberately left for this piece to expose:

- `-vox-backend NAME`, default `none`. `none` short-circuits before the walk:
  the pass does not run at all, obligations stay recorded-and-accepted, and
  the compile is byte-identical to today. This is the type-only escape, and
  it is the *default* — see the decision below.
- `-vox-z3 CMD`, explicit override. When absent, the driver resolves the
  command exactly as the solver piece's test gate does
  (`testsuite/tests/vox-solver/has_z3.sh:5-8`): `$VOX_TEST_Z3` if set, else
  `z3` on `PATH`, else the pinned install the gate names — same checks
  with the gate's executable-aware semantics (`command -v` / `test -x`:
  a directory or non-executable file named z3 is never selected), same
  order, so a gate skip decision and a driver run decision can never
  disagree. The result fills `Config.z3_command`; availability is checked
  once at selection (`Vox_backend.plan` already does this) and the failure
  message names `-vox-backend none`.
- `-vox-timeout SECONDS`, default 10, filling `Config.timeout_seconds`.

All three are `Core_options`-registered like `-drefinements`, so the batch
compilers and both toplevels accept them.

The printing backend needs one refinement of driver policy. Its contract
(`typing/vox_backend.mli:76`) is to print the `Prove` query and discharge
nothing — every outcome is `Unknown`. Under the failure protocol below,
`Unknown` refuses the unit, so a dump run would fail every unit that has an
obligation, and an expect fixture could never show its queries *and* its
successful `val` line in one block. So `Vox_backend.plan` (whose flag wiring
belongs to this piece) grows a third arm:

    type plan =
      | No_discharge
      | Dump of (module BACKEND)       (* printing: emit; only its
                                          expected Unknown is suppressed *)
      | Discharge of (module BACKEND)  (* z3 and future backends: verify *)

Dump mode runs the whole pass — collection, facts, lowering, and
`discharge`, whose printing implementation emits the bytes — and suppresses
exactly the non-verdict the printing contract promises: `Ok (Unknown ...)`.
A `discharge` that returns `Error` — the shared renderer refusing an
ill-formed obligation (`typing/vox_backend.ml:98-103`) — is a defect in
this pass's output, not an expected non-verdict; it is reported and refuses
the unit in Dump mode exactly as in Discharge mode. This is a driver-policy
distinction, not a backend capability, which is why it lives in `plan` and
not in `BACKEND`.

## The walk

One traversal of the typed structure, in the shape of
`Refinement_probe.implementation` (`typing/refinement_probe.ml`): a
`Tast_iterator` override for expressions, cases and structure items — except
that unlike the probe it must thread state (the fact environment) down and
choose branch scopes, so it is a recursive function over the tree rather than
a stateless iterator. It visits every expression, including unapplied
function bodies: obligations inside a `fun` that is never called are still
obligations of the unit.

The walk produces, in source order, a list of *pending obligations*:

    type pending =
      { subject : Typedtree.expression   (* the value under obligation *)
      ; imposed : Types.type_expr        (* refined; head = the predicate *)
      ; facts   : Vox_fact.t             (* in scope at the subject *)
      ; loc     : Location.t }

and each becomes one `Vox_logic.Obligation.t` through the lowering below.
Determinism matters for the expect-test story: the obligation order, symbol
names and hypothesis ids must be functions of the source text alone.

## Obligation collection, normalised

Refinement-flow records obligations in two shapes (its synthesis report flags
the split, and the corpus pins both):

- **Markers.** `Texp_refinement_obligation ty` in `exp_extra`
  (`typing/typedtree.mli:445`), attached by the `type_expect` funnel
  (`typing/typecore.ml:6890`). These cover annotations, codomains,
  constructor/record/array arguments, assignment right-hand sides — and also
  the two argument shapes that bypass the apply arrow: optional-supplied
  arguments (corpus: `testsuite/tests/vox/refinement-flow.ml:144`) and
  letop-bound expressions (`refinement-flow.ml:487`), where the expectation
  reaches the argument through `type_argument`'s interior or through
  `Texp_letop` checking rather than through `Known_arg`.
- **Apply arrow domains.** Plain application arguments have no marker:
  `Known_arg` pre-strips both expectation copies
  (`typing/typecore.ml:10788-10793`) and the funct's arrow keeps the refined
  domain as the durable record.

The walker normalises the two shapes into the single `pending` stream:

- Every marker on a node yields one pending obligation with that node as
  subject and the marker's type as imposed. Refinement-flow's dedup rule (one
  record per (node, type up to `Ctype.is_equal`), `typing/typecore.ml:6916`)
  guarantees no duplicate *markers*; duplicates across sources are handled
  below.
- Every `Texp_apply` yields one pending obligation per supplied argument
  whose domain is refined: walk the funct's `exp_type` arrow spine
  (`expand_head` at each step — the alias gate) in parallel with the args
  list, which the typechecker already stores in arrow order with `Omitted`
  holes for unsupplied parameters (`typing/typedtree.mli:525-541`). An
  `Arg e` paired with a refined-headed domain obligates `e` against that
  domain. An `Omitted` paired with a refined domain contributes nothing
  here: the apply node's own type retains that arrow (the apply-result strip
  is head-only, `typing/typecore.ml:7692`), so the later application that
  supplies the argument is itself a `Texp_apply` whose funct arrow carries
  the contract, and the obligation fires there.

The two sources overlap, so the walker deduplicates. Disjointness fails in
at least two compiled shapes: an annotated argument — `f1 (5 : int{ _ > 0 })`
— gets a funnel marker from the annotation *and* an arrow-domain pairing
from the apply; and `%ignore`'s special application path types its argument
against the unstripped arrow domain directly
(`typing/typecore.ml:10861-10889`), so
`external drop : int{ _ > 0 } -> unit = "%ignore"` applied to `0` yields a
marker *and* a retained refined domain. The rule is refinement-flow's own
dedup rule extended across sources at the consumer: one pending obligation
per (subject node, imposed type up to `Ctype.is_equal`); a later production
for a pair already in the stream is dropped. Both shapes are fixtures
(`dedup-annotated-arg`, `dedup-ignore`).

Reading the *solved* arrow makes the walker strictly more complete than the
funnel was at typechecking time. In `let app f x = f x in app f1 0` with
`f1 : int{ _ > 0 } -> int`, `f1` solves `app`'s type variable to the refined
domain before `0` is typed, so `0` is a `Known_arg` whose expectation was
pre-stripped — no marker anywhere — and the only durable record is the
refined domain on the apply's instantiated arrow, which the walk above
collects. (Argument order matters: `let app x f = f x in app 0 f1` is
*rejected* by the typechecker — arguments are typed in arrow order,
`typing/typecore.ml:10932-10937`, so `0` fixes the shared domain to bare
`int` and `f1` then clashes with it.) The genuinely late-solved shape keeps
the variable open past the argument: in
`let h y = let app x f = f x in app y f1`, `y` is checked against a still
undetermined variable, `f1` solves it afterwards, and `y`'s occurrence is
left carrying refined-head residue on `exp_type` — which the fact rules
below deliberately ignore, while the arrow walk still collects the
obligation. Both are fixtures (`late-solved-arrow`, `late-solved-residue`).
That is the tree-reading direction of refinement-flow's "nothing is
discharged silently".

**Fail-closed check.** vox2 hard-errors on refinement-typed applications
missing their `rap` metadata (vox2 `vox_verify.ml:4661-4677`). We have no
side metadata to go missing, so the analogous defect is a pairing failure:
more `Arg`s than the funct's visible arrow spine, or a domain the walker
cannot classify after expansion. Both are internal errors
(`Misc.fatal_error` with the location), never a silent skip — a walker
defect must not become a dropped obligation.

**Dependent arrows can reach the walker, from valid source.** The upstream
rejection (`Unsupported_dependent_arrow`, pinned at
`refinement-flow.ml:364-372`) fires only where the binder is visible at the
application; higher-order solving gets past it. With the corpus's
`external d : m:int{ m > 0 } -> int = "%identity"` (`refinement-flow.ml:359`),
`let app f x = f x in app d 5` compiles: typing the unknown call mints a
binderless arrow (`typing/typecore.ml:5127-5145`) and unification never
reconciles a one-sided arrow binder, so the arrow this pass reads off the
instantiated apply carries a refined domain whose `Rexp_var` binder lives
inside the first argument's type, not in scope over the domain. That is a
program the user wrote, not a broken invariant, so meeting a dependent
arrow — in the spine walk here or as an `Rexp_var` at instantiation below —
is a located rejection in tier 2's shape ("this application involves a
dependent function type that cannot yet be verified"), never a fatal error.
Fixture: `dependent-arrow-escape`.

**Result positions.** An obligation whose subject is a control expression is
pushed into its result positions before lowering, as vox2 does with
`result_marks` (vox2 `vox_verify.ml:2233-2241`): `if`/`match`/`try` arms,
`let`/`letmodule`/`open` bodies, sequence tails each receive the obligation
recursively; the arms are checked in their own fact scopes (an arm's path
condition and pattern facts are in force for its copy of the obligation).
The base case — any other expression — lowers the subject. This is what
makes `let k c : int{ _ > 0 } = if c then 1 else 2` (marker on the whole
`if`, `refinement-flow.ml:157-161`) discharge as two trivial goals `1 > 0`
and `2 > 0` rather than one `ite` goal, and it is load-bearing for `match`:
a match result does not lower as a term at all, so without pushing, every
refined match result would be an opaque constant and every such obligation
`Unknown`.

## The fact environment

    (* typing/vox_fact.mli *)
    type t
    val empty : t
    val add : t -> Vox_lower.Ir.t -> label:string -> loc:Location.t -> t
    val hypotheses :
      t -> Vox_logic.Obligation.hypothesis list   (* ids in add order *)

Facts are stored as terms of the sorted IR below and cross the one emitter
when an obligation snapshots them. Beside the fact environment the walk
threads one more piece of state: the symbol table — every symbol the
lowering mints or resolves, with its sort and any function or datatype
declaration it pulls in — which is what signature assembly reads when it
closes an obligation over the symbols its terms (facts included) mention.
The table is bookkeeping, not soundness: it never decides what is true,
only what must be declared.

Persistent, not mutable. vox2 threads one mutable `Fact_env` with explicit
`restrict` at scope exits and `intersect` at joins (vox2
`vox_vc.ml:141-408`); a persistent value passed down the walk gets scoping
for free — enter a branch with the current environment, and whatever the
branch added vanishes when the walk returns to the parent's value. Facts are
boolean terms with an origin, nothing else: provenance bookkeeping beyond
the one label stays out of the soundness-bearing structure (vox2 lesson).

Hypothesis ids are assigned at snapshot time, in insertion order, so they are
stable across runs and usable as unsat-core currency.

The asymmetry that governs everything here: **obligations fail closed, facts
fail open.** A dropped obligation is a soundness hole; a dropped fact can
only make a true goal unprovable. So collection above never skips, while
every fact source below is free to decline the cases it cannot lower — each
such decline is a completeness gap, not a defect, and the corpus pins where
the gaps bite.

### Binder facts — unconditional

Every pattern whose `pat_type` has a refined head (after expansion)
contributes its instantiated predicate as a fact about the bound value:
`fun (y : int{ _ > 0 }) -> ...` puts `y > 0` in scope of the body. No
stability gate, per vox2's SHOULD-1 rationale (vox2
`vox_verify.ml:2253-2262`): a binder's predicate is a *proven property of
the bound value* — proven by the obligation at whatever site produced the
value — whereas a branch condition is a claim about re-evaluating an
expression. The two kinds of fact have different epistemic status and get
different gates.

Where binder facts are read is forced by refinement-flow's binder strip and
must be stated precisely, because the obvious place is wrong:

- **Patterns, via `pat_type`** — never the environment entry. A local
  immutable binder's environment entry is payload-typed (the binder strip,
  `typing/typecore.ml:1816-1830`); the pattern keeps the refined type as the
  designated fact record. This covers `let`, function parameters and match
  cases (`let mutable` has its own rule below); for destructuring patterns
  the refined `pat_type` sits on the sub-pattern the annotation reached,
  and a fact is deposited only when that sub-pattern is a variable or alias
  pattern — the name is the fact's subject. A refined head landing on a
  wildcard, constant, constructor or or-pattern binds no single name and
  deposits nothing day one: fail-open, a recorded completeness gap.
- **Module-level bindings** also keep refined `pat_type` on their patterns
  (`Tstr_value`), so the same rule covers them; their environment entries
  are *also* refined (the exemption), which the next source relies on.
- **Occurrences of module-level and imported values, via the value
  description.** `use_v = v + 1` sees `v : int` at the occurrence (the
  occurrence strip, `typing/typecore.ml:7372`), but `v`'s value description
  still declares `int{ _ > 0 }`. Whenever the lowering resolves a free
  program identifier whose declared `val_type` has a refined head, it adds
  the instantiated predicate as a fact about that symbol, once per
  obligation. This is what lets `let l : int{ _ > 0 } list = [5; v]`
  discharge the `v` element (`refinement-flow.ml:208-213`), and it is why
  the toplevel needs no fact persistence across phrases — the facts ride on
  value descriptions in `Env`, which persists anyway (vox2's toplevel
  carry-across machinery, listed under its weaknesses, has no analogue
  here).
- **Apply codomains.** An application whose instantiated codomain head is
  refined deposits the instantiated predicate as a fact about the
  application's subject (vox2 `vox_verify.ml:4719-4775`). Combined with the
  let-equality below, `let x = g ()` for `g : unit -> int{ _ > 0 }` gives
  `p(c)` and `x = c` for the call's opaque constant `c`, i.e. the contract
  reaches `x`.
- **Immutable field reads, via the label description.** `Texp_field` keeps
  the label's declared type while the read's result is carrier-typed (the
  field strip, `typing/typecore.ml:7913-7925`; the retained label,
  `typing/typedtree.mli:653-661`). When the lowering meets a read of an
  *immutable* field whose declared type has a refined head, it deposits the
  instantiated predicate as a fact about the read — sound because every
  value that entered the field met the predicate as an obligation at
  construction, and immutability means nothing entered since. Mutable
  fields deposit nothing until the mutable-state policy below says more
  than per-read snapshots. Fixture: `field-fact`. The one other durable
  contract record the tree keeps — `Texp_letop`'s `bop_op_type`
  (`typing/typedtree.mli:896-907`, retained by
  `typing/typecore.ml:8897-8901`) — is *not* consumed day one: letop
  obligations are already collected via the markers; only the fact that a
  letop's *result* meets the operator's refined codomain is deferred, so a
  goal leaning on it is `Unknown`, never unsound. Recorded under out of
  scope.

The walker never reads refined heads off `exp_type`. Those are the
variable-solving residue refinement-flow pins (`refinement-flow.ml:387-392`);
everything they could say is already said by a declared source, and treating
residue as fact would make fact coverage order-dependent in exactly the way
the flow doc confines to the acceptance margin.

### Mutable variables — per-read subjects, per-read facts

A mutable binder's read is its own node — `Texp_mutvar`
(`typing/typedtree.mli:714`, minted at `typing/typecore.ml:7333-7345`) —
and no stable symbol can stand for "the current value of `x`": two reads
separated by a write denote different values, and any lowering that gives
both reads one symbol proves false equalities —
`let w : int{ _ = 0 } = ((x <- x + 1); x) - x` would prove `w = 0` and
evaluate to `1`. So:

- **Each read is a fresh subject.** A `Texp_mutvar` read lowers to a fresh
  opaque constant per read site, tier 1's mechanism (reads are nodes, so
  per-node memoisation *is* per-read).
- **Each read deposits the declared predicate.** When the binder's declared
  type has a refined head, every read deposits the predicate instantiated
  at that read's constant. This is refinement-flow's write rule cashed in:
  the initialiser and every `x <- e` met the predicate as an obligation, so
  "the value just read satisfies `p`" holds at every read. What is *not*
  claimed — deliberately — is cross-read identity: two reads yield two
  constants the solver cannot equate, which is exactly what interleaved
  writes require. The pattern itself deposits no fact (there is no stable
  symbol for it to be about).
- **Mutable binders are excluded from the let-equality rule** below
  (`Texp_letmutable`): an equality with the initialiser is stale after the
  first write.

The `mutable-fact` fixture is specified accordingly: a read of a
`let mutable x : int{ _ > 0 }` proves a `_ > 0` goal through the per-read
fact, and its sentinel twin `mutvar-reads-distinct` pins that
`((x <- x + 1); x) - x` against `int{ _ = 0 }` is `Unknown`, not `Proved`.
vox2, for comparison, has no mutable-variable story: it marks `Val_mut`
idents unstable (vox2 `vox_verify.ml:1280`) and rejects `Texp_mutvar`
subjects outright (vox2 `vox_verify.ml:1590`); the per-read scheme is
strictly more complete and no less sound.

### Predicates over mutable state — rejected at formation

This rejection class is owned by predicate-typing's formation judgment,
not by this piece: the predicate is typed by Typecore reentry inside a
Total closure frame with the hole and dependent binders viewed Logical,
so a mutable variable, a physical mutable read, or a free value whose
mention the predicate could observe mutating is a located Typecore error
when the *type is formed* — before any obligation exists, even in an
unused declaration. Every mention the frame admits denotes stably (a
`ref` admitted through a logical view denotes its identity; its contents
are unreadable inside the frame), so the lowering asks no mode question
of its own: an earlier revision of this piece re-checked logicality
crossing on free values at VC time, fail-closed even for facts, with a
grounding probe to tell a polymorphic scheme from a genuinely mutable
type — all retired as strictly more conservative than the formation
judgment (it rejected valid logical specifications, e.g. a predicate
over a ground `int ref` mention, which now lowers and proves). A later
relaxation may give predicates over physical mutable state per-read
snapshot semantics (instantiate against a snapshot constant taken where
the predicate binds); nothing here forecloses that. Fixtures:
`mutable-in-predicate`, `poly-in-predicate`, `weak-in-predicate`,
`weak-top-in-predicate` (all pinning the formation-time rejections),
`ground-ref-in-predicate` (the admitted logical view, Proved).

### Path conditions — gated

`if c then a else b` adds `c = true` (lowered) to the then-branch and
`c = false` to the else-branch, when — and only when — `c` lowers
*transparently* (next section). `&&`/`||` left operands and `assert` are the
same rule and land day one only if free (they are the same code path);
`while` conditions, match guards and comprehension guards are deferred with
the match facts. The `assert` arm is additionally gated on `-noassert`:
translcore erases `assert e` under the flag
(`lambda/translcore.ml`, `Texp_assert`), so a fact from the erased test
would claim what nothing checked at run time — except syntactic
`assert false`, which translcore keeps raising and whose fact therefore
stays, ungated. Fixtures: `assert-fact` (the fact proves the sequence
tail's goal), and `vc-z3-noassert.ml` (the same shape refuses under
`-noassert`; `assert false` is its ungated positive control).

The gate is about which lowering the condition gets, not about soundness of
the branch fact as such: a condition that lowers to interpreted operators
over stable symbols denotes the same value at every mention, so asserting it
equal to `true` in the branch is exactly the run-time test. A condition that
does not — one containing an effectful or possibly-divergent call — would
need its call abstracted, and a fact about an opaque constant that nothing
else mentions is dead weight; vox2 skips these (vox2
`vox_verify.ml:3545-3630`) and so do we.

### Let equalities

`let x = e in body`, `x` a variable pattern bound immutably
(`Texp_letmutable` is excluded, per the mutable-variables rule above), `e`
an expression that lowers (either tier): add `x = subject(e)` to the body's
facts. Unlike path
conditions, the opaque tier is admitted here, because the equality is about
*this one evaluation*: a `let` right-hand side evaluates exactly once per
entry into the scope where the fact lives, and the per-node constant the
opaque tier mints names precisely that evaluation's value. The fact can
never travel to a different evaluation because facts never outlive their
scope: function bodies are walked in their own fact scope, branch facts die
at the join, and (when loops arrive) loop bodies will get a fresh scope per
the same rule. vox2 reached the same place split across two mechanisms
(stable-rhs equalities, vox2 `vox_verify.ml:2728-2762`, plus `rap_subject`
threading); one rule through the subject lowering is simpler and covers
both.

### Scoping and joins, day one

Facts thread forward through a block (each `let` extends the environment its
body sees) and die at branch joins: after `if`/`match`, the continuation
resumes with the pre-branch environment. No intersection at joins, no
disjunctive summaries, no `expression_may_complete` reachability analysis
(vox2 `vox_verify.ml:4061-4140`) — dropping branch facts at the join makes
completion-gating unnecessary, because a fact recorded under a condition is
only ever *used* under that condition. This forfeits provable goals (nothing
after `if x > 0 then ... else ...` knows anything about `x`), which is a
recorded completeness gap, not a soundness gap, and none of the first corpus
needs the intersection. Unordered-children evaluation order needs no rule
for the same reason: tuple and argument subexpressions contribute no facts
that survive them.

## Stability: the totality projection, plus the operator table

Design question (c): can the Totality axis be the stability gate from day
one? Almost — it is the right spine, with two provisos that the doc states
because getting them wrong flips soundness.

What stability must mean here: a *stable* application may be lowered to an
uninterpreted `Call f args` term — the same term at every syntactic
occurrence — which encodes "equal arguments give equal results, and the
call completes". That needs the function to be terminating, effect-free and
deterministic at this call.

What `total` gives (`design-docs/totality.md`): terminates, no effects, and
the logicality bump stops it reading mutable state through captures. Two
gaps:

1. **Arguments the callee can observe changing.** The capture-based
   contract deliberately exempts *parameters*: a total function may call a
   partial parameter — so `f g` with `f` total is not deterministic when
   `g` is impure — and it may read mutable state through a parameter:
   `let reads_param @ total = fun (r : int ref) -> r.contents` is accepted
   (`testsuite/tests/vox/totality.ml:425`), and `int ref` crosses totality
   (no arrows) while not crossing logicality (mutable parts,
   `design-docs/totality.md:82-87`). Lowering two `reads_param r` calls
   that straddle a write to `r.contents` as one `Call` term would prove
   the second read unchanged. The gate therefore requires the funct's
   totality projection to be `Total` *and* every argument's type to cross
   both totality (no arrows — the impure-parameter case) and logicality
   (no mutable parts — the mutable-read case). Total function arguments
   could be admitted later; day one, conservative. The `unknown-opaque`
   fixture pins the partial-parameter half; `stability-mutable-arg` pins
   the logicality half.
2. **Comparisons are not in the totality allowlist** — deliberately, since
   polymorphic compare can raise on functions and diverge on cyclic values
   (`typing/typecore.ml:699-717`). If the gate were only the axis, `y <= 1`
   would be unstable and the first corpus's showcase (`fact`) would lose its
   path condition. The interpreted-operator table below closes this: an
   application that lowers to an interpreted `Op` at a carrier where the
   OCaml and SMT semantics coincide is stable *by construction* — the
   resulting term contains no `Call` at all. Comparison at `int`/`bool`
   carriers is exactly such a row.

So the gate, precisely: an application is stable iff it lowers entirely to
interpreted operators, or its funct is a path whose totality projection at
the occurrence is `Total` (module-level `@@ total` modality on the value
description; for locals and unsigned structure-level bindings, the
binder's recorded `Texp_mode` annotation — a recursive binder's occurrence
mode never reads `Total` because the annotation caps the checking mode
without pinning the binder's mode variable, and the batch compiler,
unlike the toplevel, leaves it unpinned at walk time, so the recorded
annotation is threaded across structure items exactly as across local
bindings, and a recursive group's evidence is in scope while the group's
own right-hand sides are walked — a peer may call the group's total
binder.  The recorded-annotation route is item-local and Ident-keyed: it
never crosses a module boundary, so a total binder inside an unsigned
module is conservatively refused at Pdot occurrences (the supported
route for module-interior totals is a signature carrying `@@ total`; the
one unsigned shape that proves is the batch route's nonrecursive case,
via the occurrence mode).  Fixtures `rec-total`, `vc_batch_total`,
`rec-group-total`, `local-rec-group-total` and
`module-total-boundary`) and every argument
type crosses totality and logicality. Two trust boundaries ride on the axis and are
inherited knowingly, both pinned in the totality piece's own report:
`external ... @@ total` is an unchecked claim, and the `module rec`
self-justification gap. Both mean a wrong totality claim can make a wrong
proof; that is the axis's contract to tighten, not this piece's.

## Lowering: one sorted language

Two things must reach `Vox_logic.Term.t`: predicates, which are
`Types.refinement_expression` (the `Rexp_*` forms — predicate-typing's
*typed mirror*: paths resolved by Typecore post-disambiguation, and
`rexp_type` carrying each node's ground instance except on `Rexp_hole`
and `Rexp_var`, whose types are contextual by design), and subjects,
which are `Typedtree.expression`. The avoid-dual-translations lesson
(vox2 maintained two SMT emitters and measured a near-miss between them,
vox2 `vox_smt.ml:1-14`) rules out two independent paths to `Term`. Both
inputs now arrive typed: a subject carries `exp_type` on every node, a
predicate was checked against `bool` by Typecore reentry at formation
(so `let accepted : int{ 1 + true } = 0` is a formation-time type error,
never an obligation), and the mirror stores the types the checker
established. But `Term.t` has no let, lambda or match
(`typing/vox_logic.mli:103-123`) while rexp has all three, so predicates
need normalisation, not just mapping.

So the shared language is a small *sorted* VC IR, private to `vox_lower`:
terms in `Term`'s shape with sorts assigned, plus the binder forms that
normalisation removes before emission. Two front ends produce it; one
emitter consumes it:

- **Subjects: typedtree → IR.** Shallow and total on the supported forms:
  idents, constants, applications, tuples, constructors, immutable record
  fields and field reads, `ite`, and the transparent wrappers (sequence,
  `open`, `letmodule`); it does not descend into forms the walk already
  handled structurally (result-position pushing consumed
  `if`/`match`-as-subject before lowering sees them). Sorts are read
  straight off `exp_type`: the types the typechecker established are used,
  not discarded and reconstructed.
- **Predicates: typed mirror → IR.** Sorts are read off the mirror, never
  reconstructed: the hole and the dependent binders get the payload sort
  from the refined type (their nodes are contextual by design), every
  other node carries the ground instance Typecore stored — a polymorphic
  value's use either grounds there (the symbol allocator mangles
  per-instance) or fails the sort mapping as not fully determined.
  `Env.find_value` survives only for the fact deposit: resolving a free
  ident whose declared type is refined deposits the instantiated declared
  fact through the same hook the subject front end uses, so a goal may
  lean on a declared value only its predicate mentions (fixtures
  `predicate-ident-fact` and `wildcard-read`; the *translation* may
  consult `Env`, closedness means the *backend* never does, and every
  resolved symbol lands in the signature). A named non-primitive call
  lowers to the congruent uninterpreted `Call` the subject front end
  would emit for the same callee at the same ground sorts — one
  allocator, one name, so predicate and subject mentions of one call
  meet in one term — with no unfolding (definitional equations are a
  later piece) and no re-asked argument gate: the subject gate exists
  because subject arguments may be physical, and a physical value cannot
  reach a predicate. A field read projects through the mirror's stored
  (owner path, label) identity as the same [Select] the subject front
  end emits; a record that is not modeled as a datatype (a mutable
  record) has no selectors and stays a modelability rejection. Neither
  calls nor field reads deposit facts from the predicate side (a
  recorded completeness gap; the subject-side sources cover the shared
  terms). Typecore checked the predicate
  at `bool`, so a sort clash inside this front end is an internal defect,
  not a user error; what remains user-facing is modelability — a
  well-typed operand pair the operator table has no row for (fixture
  `untabulated-comparison`), or a form outside the term language. The
  same front end normalises to the quantifier-free fragment: predicate
  `let`s are substituted, supported lambdas beta-reduced, `match` lowered
  to `Ite`/`Select` with equality tests (the day-one matchable subjects —
  tuples, integer and Boolean patterns — need no `Term.Test`, so the IR
  carries no tester or binder forms; `Test` earns its IR form with the
  first constructor-pattern corpus), and any residual binder form (an
  unapplied lambda, a match the translation cannot lower) is a located
  rejection in tier 2's shape. Fixture: `predicate-sort-error` (pinning
  that the ill-typed predicate now dies at formation, upstream of this
  pass).
- **One emitter: IR → `Term.t`**, trivial by construction — the IR is
  sorted and binder-free by the time it emits. Every fact, goal, subject
  and predicate crosses this one emitter, so there is exactly one meaning
  assigned to each construct; the front ends share the sort vocabulary and
  the operator table below, and instantiation stays substitution, inside
  the IR.

The sort vocabulary maps known OCaml types onto `Sort.t`: `bool → Bool`,
`int → Bitvec 63`, `Bigint.t → Int`, concrete datatypes → `Datatype`,
abstract types → `Uninterpreted`, anything else → tier 2. `float` maps to
an uninterpreted sort with opaque operations day one — comparisons on it
are unstable anyway (NaN breaks reflexivity, vox2 `vox_vc.ml:195-266`).

**Two-tier fallback**, vox2's shape (vox2 `vox_verify.ml:1448-1591`),
because the failure mode it prevents — silently dropping — is the one this
design refuses:

- **Tier 1, abstraction.** An application that fails the stability gate
  becomes a fresh opaque constant, memoized per node (vox2
  `vox_verify.ml:1347-1362`), declared in the obligation's signature at the
  node's sort. Sound: the constant names the value this node's single
  evaluation in the current fact scope produced, and no hypothesis about it
  exists unless a declared source (apply codomain, binder fact) adds one.
  Reads of mutable state through functions arrive here for free — `!r` is
  an application of a partial function — and `Texp_mutvar` reads land in
  the same tier by the per-read rule above. Abstraction is also the
  fallback for any *value-sorted* form the conversion does not support.
- **Tier 2, loud rejection.** A subject whose *sort* is unrepresentable —
  function-typed, first-class module, object, an unsolved type variable —
  is a located error, vox2's message shape: "this expression cannot yet be
  represented in a verification condition" (vox2 `vox_verify.ml:5640-5645`).
  Never a silent drop: the user wrote an obligation the tool cannot state,
  and must hear so.

**The operator table** maps (resolved path, operand sort) to `Op`, drawn
from the `primitive_is_total` set (`typing/typecore.ml:705`) intersected
with what `Op` expresses:

- comparisons and equality at `int`; `+`, `-`, `*`, unary `-`,
  `land`/`lor`/`lxor` at `int`; `not`/`&&`/`||` at `bool`;
- equality at `bool` (`Eq`/`Distinct`) — but *not* Boolean ordering: `Op`
  has no Boolean ordering operator (`typing/vox_logic.mli:49-87`), and
  rather than invent encoding formulas for a row no corpus needs, a `<` at
  `bool` falls through to the stability gate like any other polymorphic
  comparison and abstracts;
- the shifts `lsl`/`lsr`/`asr` at `int`, guarded: OCaml leaves
  out-of-range counts unspecified while the SMT shift primitives are
  total, and encoding the range is explicitly this translation's job
  (`typing/vox_logic.mli:46-48`). `x lsl n` lowers to
  `Ite (0 <= n && n <= 62, Bv_shl x n, c)` with `c` a fresh per-node
  opaque constant, and likewise `lsr`/`asr` with `Bv_lshr`/`Bv_ashr`:
  interpreted exactly where the two semantics provably coincide, opaque
  outside it — the same shape as vox2's partial-op treatment (vox2
  `vox_smt.ml:184-212`). Boundary fixture: `shift-bounds` (a count of 62
  proves; 63 and negative counts are opaque).

`/` and `mod` are deliberately absent — they raise on zero, vox2 models
them with fixed opaque partial-op functions (vox2 `vox_smt.ml:184-212`),
deferred. Bigint rows are deferred with them:
`Bigint.of_int` needs the `bv2int` conversion solver-interface explicitly
deferred to this translation, and the first corpus has no Bigint fixture, so
adding the rows without the conversion would be untestable surface. `Int`
vs `Bitvec 63` is nonetheless already load-bearing: OCaml `int` arithmetic
wraps, so `x >= 0 ⊬ x + 1 >= 0`, and the corpus pins this (`bitvec-wrap`
below) so the first user to hit it finds a fixture, not a mystery.

**Instantiation.** The imposed type's head is `Trefine { ref_payload;
ref_pred }`; the goal is `ref_pred`'s IR with the subject's IR term
substituted for the hole. Substitution is capture-free by construction:
predicate-local binders are stamped `Ident.t`s distinct from every program
ident. Top-level heads only — nested refinements (`int{p} list`) are never
decomposed structurally, matching vox2's load-bearing corpus finding (its
refinement-corpus report) and refinement-flow's head-only discipline;
components matched out of a structure regain their predicates as binder
facts from pattern types. An `Rexp_var` referring to an *arrow* binder can
reach the lowering through the higher-order escape above and gets the same
located unsupported rejection there — never an internal error — and the
consecutive-heads question (`int{p}{q}`, currently rejected, single-head
semantics carried) is an open owner-level dependency this piece inherits
and does not decide — the lowering assumes exactly one head.

**Naming.** One symbol allocator names everything the signature declares —
variables, uninterpreted functions, uninterpreted sorts, constructors and
selectors, which all share the solver's single namespace
(`typing/vox_smtlib.ml:156-165`; duplicates are renderer errors, `:163`,
and duplicate sorts `:140`). A symbol's key is its *resolved identity*
plus, for functions, the ground sort signature of this use:

- Identity is the stamped ident, never the bare spelling: locals and
  `Pident` paths render as `Ident.unique_name` — `name ^ "_" ^ stamp`
  (`typing/ident.ml:86-88`) — because `Path.name` drops the stamp
  (`typing/path.ml:115`) and two shadowed local `f`s would otherwise
  collapse into one `Call` symbol. Dotted paths stamp their head ident
  the same way (a local module `M`'s type renders `M_<stamp>.t`), because
  two shadowed local modules would otherwise collapse their members into
  one declaration; compilation-unit heads carry `Ident.unique_name`'s
  fake `_0` stamp and renumber canonically like any other stamp.
- A polymorphic function used at two ground instantiations needs two
  declarations, and the signature gives each name exactly one ground
  signature (`typing/vox_logic.mli:176-185`) — so the allocator mangles
  the instance sorts into the name using `Sort.key`
  (`typing/vox_logic.mli:41-43`, built for exactly this), in the
  `name<key,...>` shape `Signature.instantiate` already uses for datatype
  instances (`typing/vox_logic.ml:148`); the mangled signature is the full
  ground signature of the use — argument sorts and result sort — so a
  total polymorphic `id` used at `int` and at `bool` in one obligation
  becomes two symbols, `id_3<Bv63,Bv63>` and `id_3<Bool,Bool>`. Sorts,
  constructors and selectors of instantiated datatypes get exactly
  `instantiate`'s mangling — one convention, two producers — and every
  datatype member (constructor or selector) is *qualified with the stamped
  declaration name* (`box_1.first_pos`, `ab_1.A`, the tuple-selector
  pattern): members of all datatypes share the solver's one namespace, so
  a bare constructor or label name would make two declarations sharing it
  an ill-formed obligation on valid source. Term-side constructor uses
  carry the same qualified, instance-mangled name (`option.Some<Bv63>`),
  so terms and instantiated declarations agree. Fixtures:
  `member-namespace` (duplicate constructor names, duplicate labels, a
  parametric constructor in a subject).
- Opaque constants mint as `result/<counter>`; hypothesis labels are
  `h<id>`.

Collisions cannot occur: every local carries a `_<stamp>` suffix, every
mangled instance angle brackets, every module path a `.`, every minted
constant a `/` — and the `h1, h2, ...` hypothesis labels and the SMT-LIB
builtins contain no stamp suffix, bracket, dot or slash. Names needing it
are `|quoted|` by the renderer; the two characters a quoted symbol cannot
hold are encoded — `|` as `{bar}`, `\` as `{backslash}`, and `{` itself
as `{lbrace}`, so the encoding is injective on all names with no
assumption about which names reach it — keeping valid operators like
`( /|> )` representable (fixture `bar-operator`).
This discharges the name-generation duty solver-interface's
builtin-rejection rule assigns to the translation, in one place. Fixtures:
`poly-instances` (one obligation using a total polymorphic `id` at two
sorts: two declarations in the printed signature) and `shadowed-local`
(two shadowed local total `f`s: two distinct symbols).

**Stable baselines.** Stamped names would make every printing baseline
churn whenever an unrelated edit shifts `Ident` stamps. So obligations are
canonicalised before rendering: per obligation, symbols are renumbered
deterministically in first-occurrence order — a function of the source
text alone — consistently across terms and signature. The z3 and printing
backends receive the same canonicalised bytes, preserving "the baselines
are the bytes z3 receives". (A process-wide `Clflags.canonical_ids`-style
flip was rejected: it is global and would fight the stamped-identity rule
above.)

## Signature assembly and datatypes

Each obligation gets a closed `Signature.t` built from exactly the symbols
its terms mention: variables (program idents and opaque constants) with
their sorts, uninterpreted functions (stable calls and abstract-type
operations), and the datatype declarations reachable from any mentioned
sort, run through `Signature.instantiate` for monomorphisation.

Concrete-vs-abstract follows solver-interface's rule with the deciding
environment pinned and the concrete subset stated. A type whose declaration
(via the subject's `exp_env`) has a visible definition *and* is a regular
closed variant with at least one constructor, a record whose fields are all
immutable, or a tuple becomes a `Datatype.decl` — constructors, selectors,
testers; records and tuples as single-constructor datatypes. A record with
a mutable field is *not* a datatype even though its definition is visible:
a datatype's constructor is extensional, so `mk r.a r.b = mk r.a' r.b'`
would equate two states of the record that differ only across a write —
it becomes an uninterpreted sort instead (its reads already abstract), a
completeness loss where a datatype would be a soundness loss. Fixture:
`mutable-record` (the printed baseline is a `declare-sort`). The same
uninterpreted-sort translation applies to a recursive variant group with
no reachable base constructor (`type t = C of t`): SMT datatypes must be
well-founded — some constructor whose fields are all at
already-well-founded types must be reachable — so the solver rejects the
declaration outright, and a strictly inductive reading would make the
sort empty, turning every fact over its values vacuous. The OCaml type
is inhabited (via cycles), so the sound translation is a declared
uninterpreted sort: values stay opaque constants, and constructor
reasoning over cyclic data is deferred with the rest of cyclic-data
reasoning. Well-foundedness is decided per GROUND INSTANCE, with the
instance's arguments substituted into the constructor fields — a
declaration name is not enough: `type t = C of t box` is baseless even
though `'a box`'s declaration has a parameter-only constructor, because
the instance `box<t>`'s one field is `t` itself. Concretely both
deciders score the ground group `Signature.instantiate` expands (whose
non-regular-recursion rejection bounds the instance set): the subject
front end when it sorts a type, and signature assembly when it grounds
instantiated fields — a well-founded instance that reaches a baseless
one only through a constructor field (`'a good = Good | Wrap of 'a bad`
with `'a bad = Bad of 'a bad`, used ground) stays a datatype whose
offending field is at the uninterpreted sort the baseless instance
lowers to. Mutually recursive groups *with* a base case stay datatypes.
Fixtures: `selfish-cycle` (the printed baseline is a `declare-sort`, and
a benign program touching the type proves), `mutual-datatype` (the
even/odd group declared as one datatype group), `wf-box` (the
parameter-carried knot above: benign goal proves, `declare-sort` pinned),
`good-bad` (the grounded use compiles and proves; the printed baseline
shows the datatype whose field is a declared uninterpreted sort). A visible
definition outside that subset is a
located rejection day one: `Type_open` declarations
(`typing/types.mli:979`) have no finite constructor list to close, an
empty variant is a renderer error (`typing/vox_smtlib.ml:181-182`), and the
remaining kinds (unboxed products, GADT constructors, ...) earn translation
rules when a corpus demands them — rejection rather than silent
abstraction, because the user wrote an obligation over a representation the
tool cannot state, and must hear so. A type abstract in the deciding
environment becomes an uninterpreted sort whose operations are the
uninterpreted function symbols the terms mention. Using the obligation
site's own environment is what makes the same type concrete inside its
defining module and abstract outside it, so client proofs cannot lean on a
hidden representation. Mutually recursive groups and the
non-regular/function-field rejections are `Signature.instantiate`'s job,
already built and tested in the solver piece; this piece only feeds it
declarations. The first corpus exercises little of this machinery (its
predicates are arithmetic; its one structured subject is a tuple), which is
accepted: the datatype path is pinned by `tuple-datatype` plus one
environment-sensitivity fixture — `sealed-datatype`, the same type printed
concrete inside its defining module and as an uninterpreted sort behind its
sealed signature — and one rejection fixture (`open-datatype-reject`), not
explored.

## Discharge and reporting

Per obligation, sequentially, in source order, through the selected
backend's `discharge`. The protocol on failure is vox2's, adopted with its
own rationale (vox2 `vox_verify.ml:1708-1724`) because the alternative was
measured there and found worse:

- **The walk continues past a failure.** Refusing at the first failure
  abandons every later failure and success; a user holding two mistakes
  would be told about one.
- **A failed goal still becomes a fact.** Dropping it would make every
  obligation depending on it fail too, so one mistake prints as a page of
  consequences. Taking the spec anyway localises the failure to the
  obligation that owns it.
- **The unit is refused at exit** if any obligation failed. This is what
  carries the soundness the previous point deliberately spent: an
  obligation proved after a failure may rest on a spec nothing established,
  and the refusal is why that cannot escape the unit — a *within-unit*
  guarantee; what a verdict means to other units is the provenance section
  below. The refusal is a located error naming the count: "N refinement
  obligations were not verified".

Per-obligation reporting, by outcome:

- `Proved` — silent. (`unused_hypotheses` is diagnostic surface for a later
  piece; carried, not yet printed.)
- `Refuted model` — "Refinement verification failed: the predicate is
  refutable", with the counterexample's variable assignments rendered from
  the term-valued model when present. Refuted is trustworthy by
  solver-interface's two-query semantics; the message may say "is false",
  not "could not be proved".
- `Unknown Timeout` / `Unknown (Incomplete s)` — "could not be verified",
  with the reason. Never worded as a refutation.
- `Error { cause; _ }` — a backend failure, reported with the cause line,
  counted as a failure, and the walk continues; the raw payload is kept out
  of the one-line report (vox2's `protect_discharge` buried real defects in
  unread tails; the split types already prevent the misclassification).
  `Unavailable` cannot occur per obligation — availability was checked at
  selection.

Failures are emitted as they are found (each a formatted located report, as
vox2 does), so the editor's readout shows the state of the buffer; the final
refusal error is what fails the build. In the toplevel the same runs per
phrase.

### What a verdict means across units

`-vox-backend none` is the default, so any imported unit may have been
compiled with its refined claims recorded and unverified — and even a fully
verified import chain bottoms out in externals and `.mli`-only declarations
that nothing checked. A `Proved` here is therefore *conditional*: it
certifies this unit's obligations given every imported refined contract as
a hypothesis. Three rules make that stance explicit rather than silent:

- **Imported refined value descriptions are hypotheses, not certified
  facts.** The value-description fact rule above uses them inside proofs;
  the conditionality is the stated meaning of the verdict, not a leak in
  it.
- **Refined externals and interface-only declarations are axioms, and are
  reported.** At unit exit, discharge mode prints a short admission
  report listing the imported refined contracts and axioms the unit's
  obligations were discharged under — the minimal cousin of vox2's
  admission reporting, which had to report refined-result externals for
  exactly this reason (`vc-research-map.md:177-183`). Nothing fails; the
  trust surface becomes visible output. Each admission is reported at its
  *generic* declaration (the declared scheme from the environment, not an
  occurrence's instantiation): a polymorphic axiom used at two ground
  types is one line naming the whole contract that was trusted, where
  reporting an instantiation would name a source-order-dependent
  specialization and understate the surface.
- **Verification provenance in the CMI** — recording whether a unit's own
  claims were discharged, so a consumer can distinguish verified imports
  from merely recorded ones and tighten the conditional verdict into a
  certified one — is a named later piece (out of scope below). The
  admission report is sized to be its seam: what it prints today is what
  provenance will discharge tomorrow.

## What the first corpus proves

The refinement-flow test file is the natural corpus: its expect output
already pins the complete obligation map via `-drefinements`, so the vc
corpus walks the same programs and shows which of those obligations
discharge with which facts. Concretely, the showcase chain:

- `let v : int{ _ > 0 } = 5` — goal `5 > 0`, no hypotheses: `Proved`.
- `f1 5` for `f1 : int{ _ > 0 } -> int` — an arrow-domain obligation, goal
  `5 > 0`: `Proved`.
- `let rec fact (y : int{ _ > 0 }) : int = if y <= 1 then 1 else
  y * fact (y - 1)` (`refinement-flow.ml:345-346`) — the recursive call's
  argument obligation, goal `y - 1 > 0` under the binder fact `y > 0` and
  the else-branch path condition `¬(y <= 1)`: `Proved`, in `Bitvec 63`
  (no wrap: `y >= 2` bounds `y - 1` inside the carrier). One fixture, three
  mechanisms — binder fact, path condition, machine arithmetic — and it
  fails if any one is disabled, which makes it the corpus's discriminating
  centrepiece.
- `let l : int{ _ > 0 } list = [5; v]` — the `v` element discharges only
  through the value-description fact `v > 0`: the ident-fact discriminator.
- `let k c : int{ _ > 0 } = if c then 1 else 2` — result-position pushing:
  two goals, both `Proved`; disabling the push makes it an opaque `Unknown`.

Plus the two verdicts a healthy corpus must contain because their absence
would make "everything green" indistinguishable from "nothing ran":

- **Refuted**: `let z : int{ _ > 0 } = 0` — prove query sat, disprove query
  (`0 > 0` under no hypotheses) unsat: `Refuted`, with the located error in
  the expect block.
- **Unknown**: `let p (h : unit -> int) : int{ _ >= 0 } = h ()` — `h` is a
  partial parameter, the call abstracts to an opaque constant `c`, and
  `c >= 0` is neither provable nor refutable: `Unknown (Incomplete _)`.
- **bitvec-wrap**: `let inc (x : int{ _ >= 0 }) : int{ _ >= 0 } = x + 1` —
  `Unknown`: `x = max_int` defeats the prove query, `x = 0` defeats the
  disprove query. The expected output carries a comment saying *why*, per
  solver-interface's instruction to state the `Bitvec 63` decision
  prominently.

**Expect tests without a live z3.** Two files:

- `vox/vc-printing.ml`, flags `-vox-backend printing`: a compact set (one
  fixture per lowering shape above) whose expected output contains the
  SMT-LIB `Prove` scripts byte-for-byte — `declare-const`s, `:named`
  hypotheses, the bitvec operators. Printing shares the z3 renderer, so
  these baselines are the bytes z3 would receive: a lowering defect is a
  baseline diff, not a mysterious `unknown`, and no solver needs to exist.
  Dump mode does not refuse units, so the `val` lines print alongside the
  queries.
- `vox/vc-z3.ml`, flags `-vox-backend z3`, gated exactly like the solver
  piece's z3 tests: the TEST block carries the gate lines from
  `testsuite/tests/vox-solver/z3_backend.ml:1-15` — `readonly_files`
  naming `has_z3.sh`, `script` running it, skip on its exit 125 — followed
  by the `expect` action. `script` composing with `expect` is in-tree
  precedent
  (`testsuite/tests/typing-jkind-bounds/poly-variant-limit/test.ml:1-13`),
  not a hope. The file holds the full corpus with verdicts in the expected
  output — failures print located errors in their blocks, successes print
  their ordinary `val` lines. The gate (`has_z3.sh:5-8`) and the driver
  resolve z3 identically — `$VOX_TEST_Z3`, then `z3` on `PATH`, then the
  pinned install — so a skip decision and a run decision can never
  disagree.

A green z3 fixture is silent, so greens alone would not discriminate "all
proved" from "pass disabled"; the interleaved Refuted/Unknown fixtures and
the printing baselines carry that discrimination between them.

**Both tracks are mandatory validation, not alternatives** (owner-directed).
The printing track pins the emitted bytes; the z3 track pins the verdicts a
real solver returns for those bytes; each catches what the other cannot (a
lowering change that keeps the query provable is invisible to z3 but a
printing diff; a sign error in the query polarity prints plausibly but
inverts every verdict). The development environment satisfies the gate (a
pinned z3 4.8.5 exists at the path `has_z3.sh` names), so an implementation
of this piece is not validated until `vc-z3.ml` has RUN — its verdicts
promoted from a live solver, not hand-written — and both files are green
under `make dev-test DIR=vox` with the gate passing, alongside the full
suite. A skipped z3 test is a valid CI outcome on machines without a
solver; it is not a valid state for this piece's own development. Every
verdict-bearing fixture in `vc-z3.ml` whose lowering shape is not already
pinned by `vc-printing.ml` gets its query added there too, so the corpus
cross-checks bytes against verdicts fixture by fixture.

## Out of scope

Each recorded, most with the vox2 mechanism named for the eventual piece:

- **KNOWN HOLE (soundness, owner-deferred): recursive value bindings can
  self-justify their declared predicate.** Lowering a recursive binding's
  right-hand side resolves the binding's own ident, and the
  value-description deposit adds the declared predicate as a hypothesis of
  the very obligation meant to establish it — `let rec x : t{ false } =
  C x` is accepted (hypothesis false ⊢ goal false), and the false fact
  then propagates through later deposits (`let boom : int{ 0 > 1 } =
  let _ = x in 0` verifies). The fix — excluding a recursive group's own
  idents from value-description deposits while the group's right-hand
  sides are walked and its goals assembled — needs recursive-group
  infrastructure this piece should not carry, and is owner-deferred to a
  later piece together with cyclic-data reasoning. The accepting behaviour
  is pinned by sentinel fixtures in both tracks (`recursive-knot-hole` in
  `vc-printing.ml`, whose baseline shows the self-justifying hypothesis
  line, and in `vc-z3.ml`, whose verdicts accept), so the later piece has
  a discriminating test to flip and the hole cannot silently vanish or
  worsen; the deposit site carries the matching comment
  (`typing/vox_verify.ml`, `make_deposit`). The sentinel type carries a
  base constructor (`type knot = Stop | K of knot`) so the hole rides a
  well-founded datatype: a baseless recursive variant now lowers to an
  uninterpreted sort, whose opaque subjects never resolve the ident and
  so cannot exhibit the deposit. The hole is a module-level-binding
  behaviour: a *local* `let rec` group's recursive occurrences carry the
  unrefined payload type in the right-hand sides' environment, so no
  self-deposit fires there and the local knot's own obligations are
  refuted — pinned by the `local-knot` fixture in both tracks, which
  flips if the local route ever starts self-justifying.
- **Termination measures** (`[@vox.decreases]`, lexicographic-descent VCs,
  the post-walk completeness re-check). Recursive calls are ordinary
  applications here; totality claims ride the axis.
- **Seal/inclusion obligations.** Refinement-flow already assigns
  strengthening-at-signature-match to a later sealing piece; this pass has
  no `Includemod` hook.
- **`assume`/admissions machinery** (token-per-site, two-tier reporting) and
  **definitional equations** (`let[@vox.def]`).
- **The Lean backend and cross-checking**; the **verdict cache**; the **IDE
  JSON dump**. All layer above or beside `BACKEND` without changing this
  piece's shape.
- **Match shape facts and disjunctive summaries** (per-arm `subject = C(...)`
  equalities, fall-through negations, match-result and try summaries).
  Deferred with argument: the first corpus contains no obligation whose
  discharge needs a constructor-shape hypothesis — its matches are the
  `if`/`else` sugar the path conditions already cover — so deferral does
  not make the corpus vacuous; it bounds it. The cost is real (no datatype
  reasoning beyond what predicates state directly) and lands with the first
  datatype-heavy corpus, together with `Test`/`Select` fact plumbing.
- **Join intersection and reachability gating** (facts after a join;
  `expression_may_complete`). Deferred as a completeness gap, above.
- **Division and modulo, Bigint operator rows, `Int`/`Bitvec` conversions**
  — deferred together; the conversions were explicitly left to this
  translation by solver-interface and pull in the partial-op modelling.
- **While-condition facts, match guards, comprehension guards; loop
  invariants** (vox2 keeps only pre-loop facts; we have no loop facts at
  all yet).
- **Attribution and counterexample presentation beyond the model print**
  (vox2 runs separately-budgeted queries for both; our `BACKEND` already
  returns unused hypotheses and models, so this is reporting work, not
  protocol work).
- **Dependent-arrow substitution** — upstream rejects direct consumption
  and this pass rejects the higher-order escape with a located error; when
  a later piece admits dependency, argument-to-binder substitution lands
  in the instantiation seam built here.
- **Letop result facts.** `Texp_letop`'s retained `bop_op_type` is a
  durable contract record this piece reads for nothing: letop obligations
  are collected via the markers; the fact that a letop *result* meets the
  operator's refined codomain waits for the piece that gives `Texp_letop`
  the apply-codomain treatment. Cost: such a goal is `Unknown`.
- **CMI verification provenance** — recording whether a unit's refined
  claims were themselves discharged, upgrading the conditional verdict of
  the provenance section into a certified one; the admission report is
  its seam.
- **Per-read snapshot semantics for predicates over mutable state** — the
  possible relaxation of the day-one rejection, noted there.

## Tests

`testsuite/tests/vox/vc-printing.ml` and `vc-z3.ml` as above, red-green per
convention: RED adds both corpora *without* the vox flags (they do not exist
yet), pinning that every fixture compiles with its obligations recorded and
nothing discharged; GREEN adds the flags to the TEST blocks and promotes the
queries and verdicts, so the RED-to-GREEN expectation diff is exactly what
this piece decides. Named discriminating fixtures, each failing if its
mechanism alone is disabled:

- `proved-const` — `let v : int{ _ > 0 } = 5`: the end-to-end spine.
- `arrow-domain` — `f1 5`: apply-arrow collection (no marker exists to fall
  back on).
- `optional-and-letop` — `f4 ~o:5 ()` and `let+ y = 5 in y`: the marker
  shapes of the argument normalisation.
- `partial-application` — `(f3 ~b:2) ~a:5`: the `Omitted`-then-supplied
  path; the obligation fires at the second apply.
- `late-solved-arrow` — `let app f x = f x in app f1 0`: collection from
  the solved arrow where no funnel marker exists (the argument was
  pre-stripped as a `Known_arg`); `late-solved-residue` —
  `let h y = let app x f = f x in app y f1`: the argument typed before the
  domain was determined, leaving `exp_type` residue the fact rules ignore
  while the arrow walk still collects.
- `dedup-annotated-arg` — `f1 (5 : int{ _ > 0 })`: marker and arrow domain
  coincide; the printing baseline shows exactly one query.
- `dedup-ignore` — `external drop : int{ _ > 0 } -> unit = "%ignore"` and
  `drop 0`: the `%ignore` special path; one failure reported, not two.
- `dependent-arrow-escape` — `let app f x = f x in app d 5`: the
  higher-order escape; a located unsupported error, pinned so it never
  degrades into a crash or a silent skip.
- `fact-binder-and-path` — the `fact` fixture: binder fact + path condition
  + bitvec arithmetic; the centrepiece.
- `ident-fact` — `[5; v]`: value-description facts. The discriminator is
  `Proved` versus `Unknown` with the value-description source disabled — a
  `Proved` is silent and unused-hypothesis reporting is carried, not
  printed, so nothing observable names the hypothesis itself.
- `push-to-arms` — `k`: result-position pushing through `if`; and
  `match-push` — a refined `match` result pushed to its arms, the case the
  section above calls load-bearing (a match result does not lower as a
  term at all).
- `short-circuit` — a `&&` left-operand (or `assert`) path condition
  discharging a goal in its right operand: the non-`if` arm of the
  path-condition rule, which would otherwise ship untested.
- `eta-domain` — an eta-expansion whose *domain* is refined (the existing
  corpus's eta fixture is codomain-only): the synthetic apply's argument
  obligation.
- `field-fact` — a read of an immutable record field declared
  `int{ _ > 0 }` proving a goal: the label-description fact source.
- `stability-mutable-arg` — two `reads_param r` calls around a write to
  `r.contents`: the calls abstract (argument fails logicality crossing),
  so the false equality is unprovable; `Unknown`, pinned.
- `poly-instances`, `shadowed-local` — the allocator fixtures above.
- `mutable-in-predicate` — `int{ _ = y }` with `y` mutable: rejected at
  predicate formation (Typecore's mutable-variable error); the walk never
  sees it.  Its VC-time siblings after the typed-mirror integration:
  `ground-ref-in-predicate` (a logical view of a ground `int ref`
  mention lowers and proves), `poly-let-in-predicate` (a same-phrase
  polymorphic value at a ground instance, Proved).
- Predicate-side calls (a formation-admitted callee is total over
  logical views, so a call lowers to the congruent uninterpreted `Call`
  the subject front end would emit — same allocator, same name; no
  argument-crossing re-check, because the subject gate exists for
  physical arguments and a physical value cannot reach a predicate):
  `total-call-reimposed` (an external's codomain contract discharges its
  own re-imposition: fact and goal are one term, Proved, admission
  reported), `total-call-binder-fact` (a binder fact supplies the same
  call over the hole, Proved), `total-call-congruence`
  (`gtot 3 - gtot 3 = 0`, Proved), `total-call-no-unfolding`
  (`gtot 3 = 4` holds at run time and must stay unverified — congruence
  only, definitional equations are a later piece),
  `total-call-in-predicate` (a bare uninterpreted goal is honestly
  unprovable), `total-call-in-fact` (the deposited contract makes the
  unit's verdict conditional; the admission report names it).
- Predicate-side field reads: `field-binder-fact` (the fact's projection
  and the subject-side read are one `Select` term, Proved),
  `field-in-goal` (the projection goal lowers, but a record construction
  abstracts on the subject side — no `Texp_record` lowering — so the
  obligation is honestly unprovable rather than rejected),
  `field-unmodeled-goal` (a mutable record has no selectors; a
  modelability rejection).
- `predicate-sort-error` — `int{ 1 + true }`: rejected at predicate
  formation (an ordinary Typecore type clash); the obligation-time sort
  checks are internal assertions now, and `untabulated-comparison`
  (`char < char`) pins the surviving user-facing class — well-typed
  operands the operator table has no row for, a modelability rejection.
- `shift-bounds` — the guarded shift rows at their boundaries.
- `let-equality-opaque` — `let x = g () in (x : int{ _ > 0 })` with
  `g : unit -> int{ _ > 0 }`: apply-codomain fact + opaque-constant
  equality; `Proved` only if both fire.
- `mutable-fact` — a read of a `let mutable x : int{ _ > 0 }` proves a
  `_ > 0` goal through the per-read fact; its sentinel twin
  `mutvar-reads-distinct` pins that `((x <- x + 1); x) - x` against
  `int{ _ = 0 }` is `Unknown` — the per-read subjects are what make the
  false equality unprovable.
- `refuted-const`, `unknown-opaque`, `bitvec-wrap` — the three non-green
  verdicts, as above.
- `tuple-datatype` — a predicate projecting a tuple subject: one datatype
  through `Signature.instantiate`; `sealed-datatype` — the same type
  concrete inside its module, an uninterpreted sort outside;
  `open-datatype-reject` — a `Type_open` subject's located rejection.
- `alias` — the `nat` fixtures: expansion in the collection gate and the
  lowering.
- `unrepresentable` — a refined annotation on a function-typed value:
  tier 2's located error, pinned so it never degrades into silence.
- `continue-past-failure` — two independent defects in one unit: both
  reported, unit refused once; and a proof *after* a failure that leans on
  the failed spec, documenting the trade. Both defects are variable-free
  (`refuted-const`-shaped), because a counterexample model over variables
  prints z3-version-dependent text and the fixture must pin behaviour, not
  a solver build.
- driver: `-vox-backend none` compiles the unrepresentable control
  (`(int -> int){ true }`, a walk-time located error under any running
  backend) silently, pinning that `none` short-circuits *before the walk*,
  not merely before discharge; `-vox-backend nonsense` and an unconfigured
  z3 fail once, at selection; one fixture runs as a batch `ocamlc` action
  over an unrefined phrase, covering the `Compile_common` hook (every
  expect fixture exercises only the `Topcommon` hook) and
  selection-before-obligations for a zero-obligation unit; and one
  malformed-signature test — a compiled driver test (the solver
  piece's `z3_backend.ml` shape) feeding the Dump path an ill-formed
  obligation, pinning that a renderer `Error` reports and refuses even in
  Dump mode. Compiled, because once the allocator above exists, source
  programs cannot produce an ill-formed signature — which is exactly why
  the path needs a synthetic test to stay covered.

Validation harness note: the `-drefinements` obligation map is the
cross-check for collection — every marker line in `refinement-flow.ml`'s
expected output must correspond to a collected obligation, and the apply
fixtures account for the marker-less remainder.

## Decisions taken

Recorded per AGENTS.md: real forks, the route, and why.

- **Basing: this branch builds on the stack branch**
  (`jujacobs/vox/stack/6-refinement-flow`, clean at `32b38a5527`), not on a
  single piece branch, because the piece needs both refinement-flow's typed
  tree and solver-interface's `Vox_logic`/`Vox_backend` modules, which live
  on independent piece branches; the stack is the only ref containing both.
  Cost, accepted knowingly: a stack rebuild forces a rebase of this branch.
- **Verification is off by default** (`-vox-backend` defaults to `none`).
  vox2 defaulted to z3 and a missing binary made 51 of 52 tests fail with
  messages reading as a broken feature. While the feature is experimental,
  every existing build and test stays solver-free and byte-identical, and
  fixtures opt in. The cost is stated loudly: under the default, refined
  types are *recorded, unverified claims*. Flipping the default is a
  deliberate future decision for when the corpus and solver deployment
  harden, and the flip is one constant.
- **Driver hook, not `Typemod`** — probe precedent, two call sites cover
  all four entry points, `Typemod` stays solver-free, `-stop-after typing`
  still verifies. Cost: a hypothetical fifth consumer of
  `Typemod.type_implementation` would not verify; none exists in-tree.
- **`plan` grows a `Dump` arm** rather than special-casing the printing
  backend by name in the vc driver or adding a capability to `BACKEND`.
  Whether non-verdicts refuse the unit is driver policy; `plan` is where
  solver-interface put driver policy; the flag wiring was explicitly left
  to this piece, so finishing `plan`'s contract here is in-bounds.
- **(a) Obligation normalisation**: markers and apply-arrow domains merge
  into one pending stream, deduplicated at the consumer per (subject node,
  imposed type up to `Ctype.is_equal`) — refinement-flow's own rule,
  extended across sources, because the sources overlap (annotated
  arguments; the `%ignore` path). The fail-closed analogue of vox2's
  missing-metadata error is the arrow-pairing internal error — a walker
  defect is a crash, never a dropped obligation — while dependent arrows,
  reachable from valid source via higher-order solving, are a located
  rejection, not a crash.
- **(b) Facts come from declared positions only** — `pat_type` on variable
  and alias patterns, value descriptions at occurrences, apply arrows,
  immutable `Texp_field` labels — never from `exp_type` refined heads
  (residue) and never from local environment entries (binder strip made
  them payload-typed). Stated as the precise reading of refinement-flow's
  contract rather than a choice; the fork was whether residue heads count
  as facts, and they do not, to keep fact coverage order-independent.
  Mutable variables get per-read subjects with per-read facts; a
  predicate that could read mutable state never forms in the first place
  (predicate-typing's Total/Logical frame owns that rejection), so every
  fact this piece deposits denotes the same value at every mention
  without a VC-time re-check.
- **(c) Stability = interpreted-operator lowering, else totality projection
  with arguments crossing totality *and* logicality.** The axis alone is
  insufficient (comparisons deliberately unlisted; partial parameters
  callable from total functions; mutable state readable through
  parameters); the operator table alone would never admit user functions.
  The union is exactly vox2's "recognised builtin or known total" split,
  re-derived on our axes. The axis's two trust boundaries
  (`external @@ total`, `module rec`) are inherited and named.
- **(d) The corpus is the refinement-flow fixture set plus the three
  non-green verdicts**, with the SMT-LIB bytes pinned through the printing
  backend and z3 runs gated by the solver piece's existing skip script.
  The alternative — a fresh minimal corpus — would decouple the two pieces'
  test surfaces exactly where the contract needs them coupled.
- **(e) Failure protocol**: continue past failure, failed goal becomes a
  fact, unit refused at exit — vox2's measured trade, adopted with its
  rationale quoted rather than rediscovered. Per-obligation reports print
  as found; one final located error refuses the unit.
- **(f) One sorted VC IR, two front ends, one emitter**: subjects
  (typedtree, `exp_type` on every node) and predicates (the typed mirror,
  `rexp_type` on every non-contextual node) each get a small front end
  into a private sorted IR; the predicate front end is a quantifier-free
  normaliser that reads sorts off the mirror — Typecore checked the
  predicate at `bool` at formation, so its sort checks are internal
  assertions, and `Term` has no binder forms; a single trivial emitter
  serves predicates, subjects, facts and goals. The alternatives —
  lowering typedtree directly to `Term` beside a rexp lowering (two
  translations of one semantics; vox2 measured that near-miss), or
  converting subjects *into* rexp first (discards the typechecker's
  richer typedtree structure the fact rules read) — lose. (An earlier
  revision predates predicate-typing: the rexp was untyped then and this
  front end doubled as a located sort checker; the typed mirror retired
  that role.) Facts and goals are boolean terms asserted true;
  and/or/not are the OCaml booleans; one meaning per construct end to end,
  enforced at the emitter.
- **(g) Consecutive refinement heads**: `int{p}{q}` is currently rejected
  upstream and an owner-level question is open on its semantics. The
  lowering assumes exactly one head and fail-closed-errors on stacked
  heads; nothing here forecloses either answer. Dependency noted, not
  decided.
- **Persistent fact environment** instead of vox2's mutable
  env-with-restrict: branch scoping and join-dropping become the data
  structure's behaviour instead of bookkeeping, at the cost of re-adding
  shared prefixes (irrelevant at corpus scale). Fact provenance is one
  label and one location — IDE-grade origin tracking stays out of the
  soundness path.
- **Opaque constants are admitted in let equalities but not path
  conditions.** The equality names a single evaluation and is scope-bound
  (sound and useful — it is how codomain contracts reach binders); an
  opaque condition fact is sound but inert, and vox2's precedent of
  skipping keeps conditions readable in dumps. Pure usefulness triage, and
  the cheap end of it.
- **Joins drop branch facts; no completion analysis** — recorded as the
  day-one completeness floor, with vox2's intersection + `may_complete`
  machinery named as the follow-up shape when a corpus demands it.
- **Module layout**: `typing/vox_lower` (the sorted IR, both front ends,
  the predicate normaliser, operator table, symbol allocator,
  instantiation, canonicalisation, signature assembly), `typing/vox_fact`
  (fact environment over IR terms), `typing/vox_verify` (walk, collection,
  discharge, reporting), following the solver piece's precedent that vox
  modules live in `typing/`. Three modules, one dependency direction
  (`vox_verify → vox_fact → vox_lower`), no cycles with `Typecore`.

### Amended after the review round

Two independent reviews (both compiled their counterexamples; every item
below was verified against this worktree or vox2 before adoption). What
changed, and why, one line each:

- **Stability requires logicality-crossing arguments** — `int ref` crosses
  totality but a total callee may read through it, so `Call`-equality over
  such arguments proved a stale read (the `reads_param` counterexample).
- **Mutable variables: per-read opaque subjects + per-read declared
  facts; `Texp_letmutable` excluded from let-equalities** — any stable
  symbol for a mutable variable proves false equalities across writes;
  the doc had also misdescribed vox2, which rejects `Texp_mutvar` subjects
  rather than trusting them (vox2 `vox_verify.ml:1280`, `:1590`).
- **Predicates reading mutable state are rejected at VC time** — the
  predicate filter is syntactic only, so `int{ _ = y }` with mutable `y`
  compiles and has no single denotation; fail-closed beats assigning one
  silently, and per-read snapshots remain a stated relaxation.
- **Verdicts are conditional across units; axioms are reported** — the
  default-off flag exports unverified claims, so a `Proved` certifies this
  unit modulo imported contracts; the admission report makes the trust
  surface visible and CMI provenance is the named follow-up.
- **The sort-assignment pass became a located predicate sort checker over
  a private sorted IR** — rexp is untyped, `int{ 1 + true }` compiles
  today, `Term` has no binder forms, and no other layer can catch any of
  it; the reviewer's two-front-ends/one-emitter architecture was adopted
  as the simplest shape that keeps one semantics.
- **The dependent-arrow "cannot occur" invariant was false** — higher-order
  solving instantiates dependent arrows into applications (`app d 5`
  compiles), so the planned internal error became a located unsupported
  rejection with a fixture.
- **One symbol allocator keyed on stamped identity plus ground sort
  signature** — bare path strings collide for shadowed locals and cannot
  give a polymorphic function its two ground declarations; sorts,
  constructors and selectors share the namespace and the discipline.
- **Obligation sources deduplicate at the consumer** — the claimed
  disjointness fails for annotated arguments and the `%ignore` path, both
  compiled; the (node, imposed type) rule extends refinement-flow's own.
- **The late-solved fixture was replaced** — the doc's `app 0 f1` witness
  is rejected by the typechecker (arrow-order argument typing); the
  working witness and a residue-showing variant took its place and the
  prose now says what actually happens.
- **Texp_field became a fact source; letop result facts were deferred
  honestly** — the tree keeps two durable records the fact rules omitted;
  immutable field reads are small and sound to consume now, the letop
  result fact is deferred with its cost stated.
- **Dump mode suppresses only its expected non-verdict** — a renderer
  `Error` under Dump was silently swallowed by "never refuse"; failures
  now report and refuse in both modes, pinned by a synthetic driver test.
- **Boolean ordering rows dropped; shifts guarded** — `Op` has no Boolean
  ordering, and SMT shifts total what OCaml leaves unspecified, so the
  rows now state semantics the table can actually deliver (out-of-range
  shift counts are opaque, vox2's partial-op shape).
- **Datatype scope narrowed to closed variants, records, tuples** —
  `Type_open` cannot close into a constructor list and the renderer
  rejects empty datatypes; the rest is located rejection until a corpus
  earns the rules, with sealed-module environment sensitivity now pinned
  by a fixture.
- **The z3 gate and driver resolve the solver identically; the expect
  hedge is gone** — `script` + `expect` composes in-tree, so the fallback
  paragraph was replaced by the working stanza, and the driver adopted
  the gate's resolution order rather than a near-miss of it.
- **Binder facts restricted to variable and alias patterns** — a refined
  head on a wildcard or or-pattern names no single value; fail-open
  restriction, recorded as a completeness gap.
- **Printing baselines canonicalise symbols per obligation** — stamped
  names are load-bearing for correctness but would churn every baseline
  on unrelated edits; deterministic first-occurrence renumbering keeps
  the baselines the bytes z3 receives.
- **The ident-fact discriminator was corrected** — unsat cores are not
  observable output; the fixture discriminates by `Proved` versus
  `Unknown` with the fact source disabled.
- **Continue-past-failure defects are variable-free** — counterexample
  model text varies across z3 versions; constants keep the fixture about
  the protocol, not the solver build.

### Amended after round 1 of implementation review

Two grounded reviews of the implementation (each finding reproduced with a
compiled program before adoption). What changed, one line each:

- **Assert path conditions are gated on `-noassert`** — translcore erases
  the assert the fact claims was checked (syntactic `assert false`
  excepted); the path-conditions section states the gate and
  `vc-z3-noassert.ml` pins it.
- **Datatype members are qualified with the stamped declaration name and
  term constructors carry the instantiated name** — bare members made two
  datatypes sharing a constructor or label an ill-formed-obligation
  refusal on valid source, and a bare `Some` disagreed with
  `Signature.instantiate`'s mangled declaration; the renderer's concise
  reason now also rides in `failure.cause` instead of only the raw
  payload.
- **The driver resolves z3 with the gate's executable-aware checks**
  (`command -v` / `test -x`) — `Sys.file_exists` selected a directory or
  non-executable named z3 that the gate skips, so the two decisions could
  disagree on one machine.
- **`Stdlib.Bigint.t` maps to `Int`** as the sort vocabulary always said
  (resolved through `Env.normalize_type_path` so the alias and the unit
  spelling agree); it had fallen to `Uninterpreted`.
- **Both front ends deposit declared facts through one hook** — the
  predicate front end resolved refined free idents without depositing, so
  a goal whose predicate mentions a declared value the subject never
  touches was `Unknown`; goal assembly now deposits through the pending's
  own once-per-obligation set.
- **Every binding rhs is lowered, whatever its pattern** — only
  `Tpat_var` rhs were lowered, so `let _ = w in ...` contributed no facts
  while `let u = w in ...` proved; the let *equality* still needs the
  stable symbol only a variable pattern binds.
- **Driver-glue placement kept in `Vox_verify.run_if_enabled`, with the
  contract stated** — the doc's "the caller builds the arguments" is
  realized as one shared helper both drivers call rather than
  duplicated caller-side construction (one raise serves batch and
  toplevel); the smaller honest fix over extracting a driver-side seam.
  Selection and configuration errors report at the unit's file-level
  ghost location (`Location.in_file`, the convention of other whole-unit
  errors such as inconsistent-assumption reports) instead of a fabricated
  `File "_none_", line 1` header, and the backend name is validated
  before any solver-command resolution.
- **Mutable-field records stay uninterpreted sorts** — stated as the
  datatype rule's own boundary (a constructor's extensional equality
  would equate two states straddling a write), with the `mutable-record`
  fixture pinning the `declare-sort`.
- **The IR carries no `Test`, `Let` or `Lambda` forms and the allocator
  keeps no symbol tables** — predicate lets/lambdas normalise away before
  the IR and the day-one matchable subjects need no tester, so the
  variants were unconstructible; signature assembly closes over the
  obligation's terms, so the tables were unread.  `Term.Test` remains in
  the solver language for the first constructor-pattern corpus.
- **The allocator example shows the full ground signature**
  (`id_3<Bv63,Bv63>`), which is what the implemented rule mangles.
- **Test polish** — the sealed-datatype pair kept only its discriminating
  half (`sd_env`/`sd_env_out`); `dump_policy.ml` prints the renderer
  result and the four policy booleans instead of value dumps; the
  poly-instances narration states the then-arm is `Unknown`; the
  default-`none` fixture's control is the unrepresentable shape, pinning
  skip-before-walk.

### Amended at the typed-mirror integration (2026-08-21)

The piece was rebased onto predicate-typing (per the stacking plan's
staged-commit ruling), which changed who owns which judgment.  The
behaviour-describing sections above were updated in place; two entries in
"Amended after the review round" are superseded as written there —
"Predicates reading mutable state are rejected at VC time" and "The
sort-assignment pass became a located predicate sort checker" — both were
correct against the untyped rexp and are owned by predicate-typing's
formation judgment now.  What changed, one line each:

- **The mirror is typed and is the predicate type authority** — every
  non-contextual node carries its ground instance; the lowering reads
  sorts off it and reconstructs nothing (`Rexp_hole`/`Rexp_var` stay
  contextual by design; the payload-sort and binder-environment paths
  are permanent).
- **Ill-typed predicates die at formation** — Typecore reentry checks
  every predicate against `bool` when the type is formed, even in unused
  declarations; the obligation-time sort checks demoted to internal
  representability assertions, and the `Ill_sorted` error class is gone.
  The surviving user-facing class is modelability: well-typed operands
  the operator table has no row for.
- **The mode-owned predicate-side rejections retired** — the Total
  predicate frame plus Logical hole/binder views subsume
  `Reads_mutable_state` and the logicality-crossing check on free
  values, which were strictly more conservative (the logical `int ref`
  mention now lowers and proves).  The SUBJECT-side stability gate is a
  different judgment and is untouched (`stability-mutable-arg` is
  byte-identical across the integration).
- **The polymorphic-free-value rejection and its grounding probe
  deleted** — the stored ground instance is the occurrence type the old
  design said it lacked; a use either grounds (per-instance symbols
  already existed) or fails the sort mapping as not fully determined.
- **Application arguments arrive as a completion record** — the lowering
  consumes source-order arguments and rejects, explicitly and located,
  any completion entry beyond them (synthesized defaults, call
  positions, omitted parameters) and `Rexp_format`; the mirror grammar
  adapted to is final, so these arms are the permanent coverage
  boundary until a corpus earns the forms.

### Amended at stage-4 coverage (2026-08-21)

Owner-directed coverage growth on the typed mirror, red-green:

- **Predicate-side total calls lower congruently** — a named
  non-primitive callee admitted by formation is total over logical
  views, so the call becomes the subject front end's uninterpreted
  `Call` (one allocator, one name; congruence only, no definitional
  equations, and no re-asked argument gate — the subject gate exists for
  physical arguments, which cannot reach a predicate).  Primitive and
  operator-table behaviour is unchanged.  A hazard found on the way is
  recorded in the corpus: an `%identity` external fabricating a ref from
  `unit` segfaults the toplevel value printer once its obligation proves
  and the phrase executes, so axiom-source fixtures construct real
  values.
- **Predicate-side field reads project** — the mirror's stored (owner
  path, label) identity gives the selector's position, and the read is
  the same `Select` member the subject front end emits; a record not
  modeled as a datatype (a mutable record) has no selectors and keeps a
  modelability rejection.
- Neither form deposits facts from the predicate side; the subject-side
  fact sources cover the shared terms (a recorded completeness gap).
