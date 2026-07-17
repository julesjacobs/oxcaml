# LRA end-to-end wiring plan

## 1. Status and scope

This round adds `Oxsmt_lia.Lra`, an exact incremental decision procedure for
conjunctions of linear constraints over the reals.  It deliberately does not change the
frozen term language or route SMT-LIB input to LRA.  The work below is the follow-up
unfreeze required to make that engine reachable from `Session`.

The first enabled fragment is ground QF_LRA, including Boolean combinations of ground
linear-real atoms.  The same representation and adapter support the ground arithmetic
core of UFLRA, but mixed Int/Real arithmetic, coercions of non-constant Int terms,
arrays or datatypes containing Real, and BV/Real combinations must initially fail closed
to `unknown`.  The public LRA benchmark class is largely quantified (`forall`/`exists`
over `Real`).  Quantifier instantiation over reals is separate future work: the existing
ground engine must not be presented as a decision procedure for quantified LRA.

All arithmetic remains exact.  No coefficient, bound, numerator, denominator, model
value, or parser intermediate may pass through native `int`.  The only arithmetic types
used below are `Bigint.t`, canonical core rational records, and
`Oxsmt_lia.Rational.t`.

## 2. Rollout gate and theory selection

Use one read-once dark flag, `OXSMT_LRA`.  It is off unless its value is one of `1`,
`true`, `yes`, or `on`.  Add a non-frozen `Oxsmt_core.Lra_config` module and expose
`val enabled : unit -> bool`; its implementation owns the sole lazy environment read.
Parser, printer, loader, `Session`, and `Cdclt` must call that accessor rather than each
capturing the environment independently.

When the flag is off:

- `Real` continues to fail at `smt/smtlib/parser/parser.ml:208-239` with the current
  `unknown sort: Real` error.
- a decimal continues to take the existing rejection at `parser.ml:436-440`;
- `QF_LRA` continues to be rejected by `known_logic`/`set-logic` at
  `parser.ml:1367-1410` and `parser.ml:1512-1517`;
- `Cdclt.ensure_theory` takes exactly its existing arrays, datatypes, or
  `Combined(EUF,LIA)` arms at `smt/interface/cdclt.ml:335-348`.
- `printer.ml:28-52` does not reserve the names `/` or `Real`, so existing UF inputs that
  declare those names print exactly as before.  Under the flag, add `/` to
  `predefined_funs` and `Real` to `predefined_sorts`; the parser applies the same
  conditional reservation.

In particular, do not select a theory from the `set-logic` string.  Selection is
content-driven today and must remain so.  Add the non-frozen type
`Cdclt.arithmetic_family = None_seen | Integer | Real | Mixed` and pass one shared ref to
`Cdclt.create`, alongside the datatype and array registry refs.  `Session` owns the ref
and a named `preselect_arithmetic : t -> Term.t list -> unit` operation.  It walks the
complete term DAG before preprocessing or internalization, joins the sorts it actually
sees into that four-state lattice, and is idempotent.  Declarations alone do not count.
`Session.assert_presolved` calls it once on the whole batch at entry;
`Session.assert_term` calls it on its singleton before doing any work.
`tests/loader/oxsmt_query_loader.ml:48-69` therefore needs no logic-string dispatch: its
existing batch call reaches the scan before the first assertion is interned.

`Mixed` is sticky for the query and immediately calls the existing Session degrade path,
so a batch containing both Int and Real terms can return only `unknown`.  A transition to
`Real` after an Integer/array/datatype theory has been instantiated (or conversely to
`Integer` after the real stack is live) also degrades before internalization; never
replace a live theory.  This restriction can be removed only with a combined Int+Real
adapter.  `Cdclt.ensure_theory` then chooses:

1. arrays, if the array registry is nonempty;
2. datatypes, if the datatype registry is nonempty;
3. `Combined_real = Combine(Uflra_router)(Euf_adapter)(Lra_adapter)` when the family is
   `Real`;
4. the existing `Combined = Combine(Uflia_router)(Euf_adapter)(Lia_adapter)` for
   `Integer` or `None_seen`;
5. no theory for `Mixed` (the Session has already degraded; fail loud if this is reached).

Before choosing arms 1 or 2, reject a simultaneous Real family as unsupported.  This
avoids silently sending real atoms to a theory which does not implement them.  The
family ref is set from actual term contents, never from `Parser.t.logic`.  An unused Real
declaration does not instantiate a theory.  With the flag off the parser cannot build a
Real term, and `Session.preselect_arithmetic` rejects a programmatically-built Real term
to `unknown` before internalization.  Existing inputs reach exactly the old arm and keep
byte-identical stdout, counters, intern order, and pivots.

## 3. Frozen core unfreeze

### 3.1 `Sort.Real`

In `smt/core/sort.mli:26-38`, add the nullary variant `Real` and `val real : t`.
Implement it in `smt/core/sort.ml:1-40`.  Give it its own stable `hash` discriminant;
do not renumber the existing discriminants.  `Sort.equal Real Real` is true and Real is
distinct from every other sort.

### 3.2 Exact real term representation

The core cannot depend on `Oxsmt_lia.Rational` (`oxsmt_lia` already depends on
`oxsmt_core`).  Add this core-neutral canonical value type to
`smt/core/term.mli:26-47`:

```ocaml
type rational = private
  { num : Bigint.t
  ; den : Bigint.t
  }

val rational_of_frac_big : num:Bigint.t -> den:Bigint.t -> rational
```

The constructor rejects zero denominators, moves the sign to `num`, divides numerator
and denominator by their gcd, and represents zero only as `0/1`.  It never projects to
native `int`.  Add `Rational.of_big_frac` and `Rational.den_bigint` to the non-frozen LIA
rational API so the adapter can convert between this transport type and the existing
decision-procedure type without string parsing.

Extend the frozen `Term.node` with:

```ocaml
| Real_const of rational
| Real_arith of real_linear

and real_linear = private
  { coeffs : (t * rational) Iarr.t
  ; const : rational
  }
```

`Real_arith` has the same structural invariants as `Arith`: coefficients are nonzero,
children are Real-sorted non-`Real_arith` leaves, and entries are strictly tag-sorted.
An empty form is a `Real_const`; `1*x + 0` is `x`.  Canonical rational values make
hash-cons equality semantic and deterministic.  Update every scalar payload in
`smt/core/node.ml:53-111` (`equal_node` and `hash_node`); omitting a numerator,
denominator, coefficient, or constant would alias unequal terms and is a soundness bug.
Assign new, previously unused `hash_node` discriminants to `Real_const` and `Real_arith`;
do not renumber any existing node discriminant.  Together with preserving every old
constructor path, this keeps the flag-off intern buckets and term tags byte-identical.

Keep the existing `Le of t` node.  Generalize its invariant from “Int argument” to
“Int or Real argument.”  This avoids a second order-atom vocabulary and preserves
`Theory_view.Le_zero`.  The constructor path is sort-dispatched:

- Int `le`/`lt` continues to call the current integer `mk_le` at
  `node.ml:331-360`, including gcd/ceil and `+1`, byte-for-byte.
- Real `le a b` builds the canonical exact real form `a-b` and interns `Le(a-b)` with no
  inequality gcd/ceil tightening.
- Real `lt a b` is `not (le b a)`.  Therefore an asserted strict inequality arrives at
  the LRA adapter as the negative polarity of a non-strict `Le`, where it becomes a
  strict delta bound.  It is never encoded with integer `+1`.
- `ge` and `gt` are the corresponding reversals.

`Eq` remains the common same-sort equality node.  A positive Real equality becomes two
non-strict simplex bounds.  A negative Real equality is not a convex bound; see the
disequality split in section 5.

Implement the new constructors and invariants in `smt/core/node.ml` and
`smt/core/term.ml:56-195`.  The latter's `Debug.check` must validate Real sort,
rational normalization, sorted/nonzero real coefficients, and permit a Real `Le` while
retaining the existing gcd-normalization check only for Int `Le`.  Pipeline mode should
reject both Int- and Real-sorted residual arithmetic `Ite` terms until preprocessing
lifts them.

### 3.3 `Context` constructors

Extend `smt/core/context.mli:33-71` and implement the forwarding calls in
`smt/core/context.ml:13-45`:

```ocaml
val real_const_big : t -> num:Bigint.t -> den:Bigint.t -> Term.t
val mul_real_const_big :
  t -> num:Bigint.t -> den:Bigint.t -> Term.t -> Term.t
val real_linear_combination_big :
  t -> (Term.rational * Term.t) list -> Term.rational -> Term.t
```

Overload `add`, `sub`, and `neg` by operand sort; both operands of binary operations must
have the same numeric sort.  The existing integer-coefficient `mul_const_big` and
`linear_combination_big` may accept Real operands as a convenience, but their Int path
must remain the current implementation.  Rational scaling uses
`mul_real_const_big`.  Overload `le`/`lt`/`ge`/`gt` by operand sort exactly as described
above.  Do not overload `div`, `mod`, or `abs`: they remain Int-only.  All public Real
construction still passes through `Context`/`Node`, so well-sortedness and hash-consing
remain centralized.

### 3.4 Theory view

No new atom payload is needed: `Theory_view.Le_zero` still exposes the argument of a
`Le`, and its sort distinguishes LIA from LRA.  Update the documentation in frozen
`smt/core/theory_view.mli:1-24` and the exhaustive node matches in
`smt/core/theory_view.ml:10-37` for `Real_const`/`Real_arith`.  `is_atom` treats Real
`Le` exactly like Int `Le`; rational terms themselves are not Boolean atoms.

### 3.5 Model value

In frozen `smt/core/model.mli:15-31`, add:

```ocaml
| Real of Term.rational
```

and mirror it in `smt/core/model.ml:5-10`.  This is an exact normalized value; never use
`float`, a decimal approximation, or a numerator/denominator native-int pair.

### 3.6 Exact frozen-file list

The later unfreeze changes exactly these five files from `FROZEN.sha256`:

1. `smt/core/sort.mli`
2. `smt/core/term.mli`
3. `smt/core/context.mli`
4. `smt/core/theory_view.mli` (documentation generalized to Real; atom shape unchanged)
5. `smt/core/model.mli`

It does not change `symbol.mli`, `iarr.mli`, `env.mli`, `rank.mli`, `atom.mli`,
`lit.mli`, `explanation.mli`, `theory.mli`, or `sat.mli`.  Reuse the existing
`Explanation.Rule_tag.Lia_bound` and `Lia_farkas`: these tags name the exact simplex
bound/Farkas rules and carry no integer-specific payload; adding parallel LRA tags would
create an unnecessary sixth frozen edit.  The unfreeze must have an ADR, adversarial
review, and regenerated `FROZEN.sha256`.  None of those frozen files is changed in this
round.

## 4. Parser and printer

### 4.1 Sort and literal parsing

In `smt/smtlib/parser/parser.ml` under `OXSMT_LRA`:

- map the unquoted or quoted builtin name `Real` to `Sort.real` beside Bool/Int at
  `parser.ml:208-217`;
- parse `Tok.Decimal d` at `parser.ml:436-440` exactly by removing the decimal point,
  using a power-of-ten `Bigint.t` denominator, and passing both to
  `Context.real_const_big`; never call `float_of_string`;
- recognize `(/ p q)` as a Real literal when `p` and `q` are signed integer literal
  forms and `q <> 0`; normalize through the core rational constructor.  Any nonconstant
  Real division remains `Unsupported` because it is outside linear arithmetic.  Define
  one helper for this signed-fraction grammar and use the same grammar in the independent
  model reader; do not let the two parsers grow separate sign rules;
- add `QF_LRA` and `QF_UFLRA` to `known_logic` at `parser.ml:1367-1410` only when the
  flag is on.  Preserve the current flag-off diagnostic byte-for-byte and extend its
  supported-logic list only in the on arm.  `LRA` and
  `UFLRA` are already accepted at the name level (`parser.ml:1402-1405`); keep them, but
  reject any `forall`/`exists` binder of sort Real, and any quantified body containing a
  Real term, with `Unsupported` before E-matching.  Merely accepting the logic name must
  not feed Real quantifiers into the existing integer/UF instantiator.  Update the
  on-only diagnostic at `parser.ml:1512-1517`.

SMT-LIB numerals are overloaded by numeric context.  Keep the current default of a bare
numeral as Int, then coerce only an `Int_const` to `Real_const(k/1)` when a same
application supplies a Real expected sort.  Apply that rule to arithmetic/comparison
operators (`parser.ml:820-855`), equality and `ite` branches, declared function arguments
from their `Rank`, every operand of `distinct`, `(as numeral Real)`, and `define-fun`
result checking.  A nonconstant Int term is never silently coerced to Real: reject it
until an explicit `to_real` representation exists.  This admits `(+ x 2)`,
`(distinct x 2)`, `(= x (+ 1 2))`, and `(f 2)` when `x`/`f` require Real, without
admitting mixed integer variables.  Add focused parser tests for each context so a
partial coercion implementation cannot masquerade as QF_LRA support.

Generalize linear multiplication at `parser.ml:944-967`: partition both `Int_const` and
`Real_const` scalars; after numeric-sort inference, multiply a Real term only by the
exact rational product of its constant factors.  Two nonconstant factors are still
nonlinear and rejected.  `/` is not a general operator.

### 4.2 Printing

Update `smt/smtlib/printer.ml`:

- render `Sort.Real` as `Real` in `sort_string` (`printer.ml:469-479`) and mark a
  `uses_real` bit during `collect_decls` (`printer.ml:352-463`);
- render `Real_const q` from its canonical numerator and denominator.  An integral value
  prints as a Real decimal such as `3.0` (negative as `(- 3.0)`); a non-integral value
  prints as exact SMT-LIB `(/ p q)` with a positive denominator, and a negative fraction
  prints as `(- (/ abs-p q))`.  The adapter/CLI obtains the same canonical `p`/`q` from
  `Rational.to_string`; a slash is translated to the prefix form, never printed as the
  invalid atom `p/q`.  Use the identical renderer for term literals and model values;
- render `Real_arith` like `Arith`, but render each rational coefficient with the same
  exact literal routine;
- select `QF_LRA` only when every non-Boolean term is numeric Real content.  Select
  `QF_UFLRA` when there is any uninterpreted-sort declaration/content or a non-nullary
  user application; an uninterpreted-sort equality requires UF even if no function has
  positive arity.  Nullary Real constants alone remain QF_LRA.  Existing
  Int/BV/array/datatype logic-label selection at `printer.ml:518-540` is unchanged for
  sessions without Real.

Round-trip tests must cover decimals, exact non-decimal fractions, negative rationals,
strict comparisons, and an integral Real literal remaining Real after printing.

## 5. THEORY adapter, disequality, and routing

Add `smt/theories/lia/lra_adapter.ml/.mli`, implementing both the frozen
`Theory.THEORY` signature and the non-frozen `Combine.FABRIC_CHILD` signature over
`Lra`.  The latter is required: the current `Combine` functor does not accept a plain
`THEORY` as child B.  Instantiate the engine as `Fabric.justification Lra.t`; a directly
asserted literal uses `Fabric.Real lit`, and a hub equality uses
`Fabric.Fabric edge_id`.

The adapter deterministically maps each Real leaf term to an `Lra.var` in term-tag order.
It flattens a `Real_const`/`Real_arith` into exact `Rational.t` coefficients and rhs;
uninterpreted Real applications are leaves.  Keeping rational coefficients is the
preferred path because `Lra` and `Simplex.new_slack` already store exact rationals; no
denominator product is needed.  If the implementation instead clears denominators, it
must compute one positive `Bigint` lcm per atom and multiply every coefficient and rhs by
that same positive value.

Add the following non-integrality operations to `Lra` so the adapter can satisfy that
fabric seam without reaching into `Simplex`:

- `checkpoint`/`rewind_to_checkpoint`: store the simplex trail watermark and active
  constraint count, restore both without touching the push/pop frame stack, and
  invalidate the candidate model;
- `fixed_value ~coeffs ~constant`: for a canonical linear form, return an exact rational
  value plus its lower/upper premise tokens only when both active non-strict bounds are
  equal; and `oriented_bound` as a separate lower/upper accessor used to re-derive that
  witness independently.  Strict delta bounds never count as an exact fixed value.

`Lra_adapter.check_fabric` maps an LRA conflict to `Fabric.Explanation` and returns no
propagations until a real bound-propagation layer exists; consequently
`explain_fabric` fails loud if called without a cached propagation.  `fixed_bounds`
flattens the queried Real term and calls `fixed_value`, spelling the rational with
`Rational.to_string`.  `fabric_verify` parses that spelling and independently calls the
two oriented accessors; it accepts only matching `Fabric.Real` literals.
`notify_eq ~edge_id eq` flattens the positive Real equality and asserts `Lra.Eq` with the
fabric-edge premise (a tautology is a no-op; a contradictory constant equality fails
closed).  The adapter checkpoint combines the engine checkpoint with the lengths of its
explanation and recorded-disequality trails, so chrono rewind restores all three.  These
methods mirror `Lia_adapter`'s currency, emptiness tripwire, and first-wins explanation
cache; they omit only integer propagation/cuts/branching.

Atom/polarity translation is:

| Term atom | positive literal | negative literal |
|---|---|---|
| `Le e`, `e : Real` | `e <= 0` | `e > 0` (strict lower δ-bound) |
| `Eq(a,b)`, Real sides | `a-b = 0` | record `a-b != 0` |

Positive equality is installed once through `Lra.Eq`, which creates both bounds.  For a
negative equality, do not assert either orientation.  Record the premise in the current
backtrack frame.  At `Final`, if the exact candidate model already has `a <> b`, it is
satisfied.  If it has `a = b`, return the globally valid real trichotomy
`[Context.eq a b; Context.lt a b; Context.gt a b]`.  The currently asserted negative
equality makes the first disjunct false, so this still forces one of the two strict
branches.  Keeping the positive equality in the emitted clause is mandatory: CDCL(T)
split clauses are not premise-guarded and can survive after the disequality is popped;
emitting only `a < b or a > b` would then unsoundly exclude the equality case.  At
`Propagate`, simply omit a disequality propagation.  Pop removes recorded disequalities
with their frame.  The adapter obtains the strict arms from `Lra`'s documented
`Split (Lt,Gt)` hook and adds the equality guard before crossing the frozen `Theory.Split`
seam; a disequality is never dropped or strengthened.

`Lra.check = Unsat c` becomes `Theory.Conflict` with `c.premises` and
`Explanation.Rule_tag.Lia_farkas`; the vector remains in an adapter-side observational
stash just like `Lia_adapter.last_conflict_core`.  `Lra.check = Sat` at `Final` becomes
`Theory.Sat` immediately after the disequality check.  There is no call to
`Lia.suggest_branch`, cube search, HNF, CG, Diophantine reasoning, or any other
integrality code.  `model` converts each exact `Rational.t` to `Model.Real` using
`num_bigint`/`den_bigint`.

Add `smt/combine/uflra_router.ml/.mli`.  It routes Real `Le` to the LRA child and Real
equality to both EUF and LRA at both polarities.  This deliberately differs from
`Uflia_router`: `Lia_adapter` cannot accept disequality, but `Lra_adapter` records it and
implements the exact split above.  Routing a negative Real equality only to EUF would
miss the pure `x != c` case because the LRA child need not otherwise value `x`.  The
router's equality split is the same globally valid real trichotomy
`x=y or x<y or x>y`, built through the sort-dispatched `Context` constructors.

Extend the non-frozen `Combine.ROUTER` signature at
`smt/combine/combine.mli:90-137` and its duplicate definition at
`smt/combine/combine.ml:6-12` with
`val arithmetic_sort : Sort.t -> bool`.  `Uflia_router` returns true only for
`Sort.Int _`; `Uflra_router` returns true only for `Sort.Real`.  The combinator uses
this predicate for every interface/disagreement/model candidate instead of its current
hard-coded Int tests.  Both routers must also check the argument sort of `Le_zero`
before routing it to child B and raise the combinator's fail-closed exception on a
mismatched arithmetic sort; the current unconditional `Le_zero -> B` at
`smt/combine/uflia_router.ml:17-20,52-55` must not send a Real atom to LIA.
Update the test-only `Ctrl_router` at `smt/combine/test/combine_test.ml:240-262` for the
new required method, and add Real-router tests for both polarities of equality, strict
split construction, foreign-sort rejection, and exact merged-model values.

The generic combinator currently hard-codes its second child as integer arithmetic.
Generalize, without changing its frozen public signature, at these points in
`smt/combine/combine.ml`:

- `node_owner`, `walk_children`, and boundary classification at `combine.ml:330-519`:
  `Real_const`/`Real_arith` are arithmetic-owned; a numeric node is eligible for the
  interface only when its sort matches the active arithmetic child.
- `model_eval`/`value_equal` at `combine.ml:671-705`: fold `Real_arith` with exact
  rational operations and compare `Model.Real` by value.
- disagreement filtering at `combine.ml:729-779`, merge-notification filtering at
  `combine.ml:1175-1188`, no-foreign-theory guards at `combine.ml:804-850`, and merged
  model construction at `combine.ml:1440-1575`: add the Real case without making an
  Int term eligible in the real stack or vice versa.
- Real pure-EUF classes need the same realization discipline as Int classes at
  `combine.ml:1450-1515`: if a class inherits an LRA value, propagate that exact value to
  all class members.  A Real class not constrained by LRA may be assigned a deterministic
  fresh rational distinct from all already-used rational values; least unused
  nonnegative integers, widened to rationals, suffice because the class is absent from
  every LRA atom.

In `smt/interface/cdclt.ml`, add `TCombinedReal` and cover it in every `theory_impl`
dispatcher at `cdclt.ml:38-104`, model snapshot/check paths at `cdclt.ml:620-650`, and
conflict/egraph accessors.  Extend subterm collection at `cdclt.ml:251-271` for the two
new nodes.  `ensure_theory` uses the content-derived arithmetic-family ref described in
section 2.  Do not key this choice on the parsed logic string.

The checkpoint log is currently concretely `Combined.checkpoint option Dynarray.t`.
Replace it with a private sum
`CInt of Combined.checkpoint | CReal of Combined_real.checkpoint` and update
`on_assign`/`on_chrono_rewind` at `cdclt.ml:438-515` to log and rewind the matching arm;
a constructor/theory mismatch fails closed.  Cover `TCombinedReal` in
`live_egraph_view` and `live_registered_terms` (`cdclt.ml:569-603`) by reading the same
EUF child type.  At `cdclt.ml:739-761`, clear/read the corresponding adapter's conflict
stash.  Replace the current manifest alias to `Lia_adapter.conflict_core` with a
Cdclt-owned record of the same fields and explicitly convert both adapter records; for an
LRA equality contribution whose caller token represents two oriented half-planes, expose
the atoms but set `farkas = None`, matching the existing fail-closed LIA convention.

`smt/interface/session.ml:694-705` must recognize a Real `Le` as a theory atom, and its
term walks at `session.ml:698-704`, `session.ml:876-910`, `session.ml:1420-1445`, and the
reserved-symbol/model walks must descend `Real_arith` and treat `Real_const` as a leaf.
The CNF atom split remains driven by `Theory_view.is_atom`; update exhaustive node matches
in `smt/preprocess/cnf.ml:54-64,124-166`.

The integer presolver must not rewrite Real with integer rules.  Update exhaustive walks
in `smt/preprocess/preprocess.ml` and `smt/interface/presolve.ml` so structural Boolean,
equality, and ITE rewrites preserve Real nodes through the Real constructors.  Gcd
tightening, Euclidean div/mod elimination, integer alias elimination, and all calls that
rebuild through `Context.linear_combination_big` remain guarded by `Sort.Int`.  A pass not
proved sort-generic must leave the Real subtree unchanged.  This is preferable to
silently feeding a Real expression through `mk_le`; under the dark gate an unhandled Real
shape should degrade to `unknown`.

Two existing rebuilds need special care: `smt/preprocess/preprocess.ml:113-122` and
`:255-261` currently reconstruct every `Le a` against `Context.int_const 0`.  Dispatch
that zero on `a.sort`, using `0/1` for Real, while leaving the existing Int interning order
untouched.  The value-ITE lift at `preprocess.ml:138-161` is otherwise sort-generic and
can lift Real branches once `Context.eq` accepts them.

## 6. Model extraction, checking, and rendering

Thread an exact real value through all three layers:

```ocaml
Model.Real of Term.rational
Cdclt.VReal of Oxsmt_lia.Rational.t
Session.VReal of Oxsmt_lia.Rational.t
```

Update `smt/interface/cdclt.mli:13-32`, `cdclt.ml:106-129`, and
`smt/interface/session.mli:92-109`/`session.ml:36-48`.  Required implementation sites
include:

- `Cdclt.value_of` (`cdclt.ml:764-770`), `value_compare` (`805-815`), real-class/value
  collection and realization (`845-991`), nullary/table extraction (`998-1035`), and
  `default_for`: Real defaults to exact zero.
- `Session.default_value` (`session.ml:1452-1471`): return `VReal Rational.zero`; never
  fall through to `VUninterp`.
- `smt/interface/model_check.ml:20-100`: add exact real equality, `as_real`, evaluation of
  `Real_const` and `Real_arith`, and Real `Le`.  Use only `Rational` operations.  This
  in-process R1 checker remains mandatory before reporting `sat`.
- `tests/solver/oxsmt_cli.ml:102-118`: render `VReal` from `Rational.to_string`.  Integral
  values render with `.0`; fractions render as `(/ p q)` with proper SMT-LIB unary
  negation.  Function-table arguments/results use the same routine.

The independent validator must also understand the new value, without importing the
solver's LRA decision code:

- `tests/eval/value.ml/.mli` gains a local exact rational value backed by `Bigint.t`;
- `tests/eval/reader.ml:51-55,83-102` recognizes `Real`; its term reader and numeric
  coercion paths at `reader.ml:137-315` parse decimals and signed constant fractions with
  the same grammar as the main parser, and its `logic_ok` at `reader.ml:343-345` accepts
  `QF_LRA`/`QF_UFLRA`;
- `tests/eval/eval_model.ml:17-57` parses decimal and `(/ p q)` sidecar values according
  to the declared `Sort.Real`;
- `tests/eval/eval.ml:70-174,210-255` evaluates `Real_const`, `Real_arith`, Real `Le`,
  equality, completeness walks, and function tables with the local exact rational
  implementation.  It must not use float or native-int rational arithmetic.

The external gate under `tests/gate` has its own AST and reader and does not consume
`Oxsmt_core.Sort.t`; it therefore is not made Real-aware merely by this unfreeze.  Until
its independent Lean `Rat` encoding and model reader are added, LRA corpus goals must not
be sent to that gate as supported certification cases.

## 7. Exhaustive `Sort.Real` consumer audit

Adding `Real` makes the following current exhaustive matches non-exhaustive.  Each row
gives the current location and required Real behavior.  Matches with a wildcard still
need review where noted, but do not require a compiler edit merely for exhaustiveness.

| File:current line | Required Real arm |
|---|---|
| `smt/core/sort.ml:22-40` | equality/hash constructor implementation |
| `smt/core/array_defs.ml:53-60` | stable key `R` (arrays-over-Real remain gated off initially) |
| `smt/core/bv.ml:54-58,226-231` | not a bitvector / reject as BV operand |
| `smt/smtlib/printer.ml:369-406,469-479` | collect built-in Real; print `Real` |
| `smt/theories/arr/weq_graph.ml:68-83` | not an array sort; Real is stably infinite |
| `smt/theories/arr/arr.ml:345-350,1751-1760` | not an array sort; mixed array/Real model path rejects until supported |
| `smt/theories/dt/dt.ml:185-189,877-904,925-930` | not a datatype sort; mixed DT/Real model path rejects, never `Uninterp` |
| `smt/interface/array_model_check.ml:82-97` | `Model.Real` inhabits only `Sort.Real`; mixed arrays remain rollout-gated |
| `smt/interface/dt_model_check.ml:96-145` | `Model.Real` inhabits only `Sort.Real`; mixed datatypes remain rollout-gated |
| `smt/combine/uflia_router.ml:17-30,77-85` | keep Real out of the integer router; new `Uflra_router` owns it |
| `smt/combine/combine.ml:429-438,749-755,812-850,1183-1188,1474-1489,1522-1571` | arithmetic-family-aware interface, guards, and merged model |
| `smt/interface/cdclt.ml:856-869,899-930,947-1010` | Real universe/model extraction, exact default, `VReal` |
| `smt/interface/session.ml:656-659,999-1004,1465-1471` | non-datatype case, legal-name recursion, exact default |
| `tests/eval/value.ml:20-28` | print Real distinctly |
| `tests/eval/eval_model.ml:35-57` | parse exact Real token by declared sort |
| `tests/eval/eval.ml:214-223` | no uninterpreted-cardinality obligation for Real |

Also review these wildcard matches because treating Real as their generic fallback would
be unsound even though the compiler will not force a new arm:

- `smt/theories/arr/arr.ml:1700-1713,1761-1806` and
  `smt/theories/dt/dt.ml:877-904`: the current `_ -> Model.Uninterp`/scalar fallbacks
  must not fabricate an uninterpreted value for Real.
- `smt/interface/presolve.ml:1063`: Real is not an uninterpreted sort for symmetry or
  alias heuristics.
- `smt/smtlib/parser/parser.ml:864-882`: Real remains a non-array in select/store errors.
- `smt/bitblast/bv_eval.ml:129-161` and `smt/bitblast/blast.ml:440-477`: Real input is
  outside pure BV and must reject, never reach a wildcard semantic case.

The node-variant unfreeze separately requires exhaustive traversal edits in
`smt/core/term.ml`, `smt/core/theory_view.ml`, `smt/preprocess/{cnf,preprocess}.ml`,
`smt/interface/{presolve,session,cdclt,model_check,bv_dispatch}.ml`,
`smt/combine/combine.ml`, `smt/theories/{euf,euf_adapter,lia,lia_adapter,arr,dt}/*.ml`,
`smt/ematch/{trigger,instance,egraph_view,qvar}.ml`, `smt/smtlib/printer.ml`, the
bit-blaster's rejecting walks, and their tests.  The rule for each is simple: descend
`Real_arith` coefficients, treat `Real_const` as a leaf, and never pass either to an
integer evaluator or normalizer.

## 8. Validation for the wiring tranche

In addition to the direct `lra_test` from this round, the unfreeze tranche needs:

- parser/printer round trips for `Real`, decimals, exact `(/ p q)`, strict comparisons,
  equality, disequality, large Bigint numerators/denominators, and logic labels;
- end-to-end QF_LRA sat/unsat cases through the real `Session`, including a strict-only
  contradiction and a satisfiable open interval whose emitted model is strictly inside;
- Boolean combinations and `push`/`pop`, including a negative equality that forces the
  guarded three-way trichotomy and a regression proving its learned clause remains sound
  after the disequality is popped;
- UFLRA boundary cases proving EUF/LRA model agreement on Real arguments/results;
- R1 and independent evaluator mutation tests: a boundary model for a strict constraint,
  a wrong numerator, a wrong denominator sign, and a Real value mislabeled as
  `VUninterp` must all reject or degrade;
- dark-flag A/B: with `OXSMT_LRA` unset, existing-logics stdout and counters are
  byte-identical; with it on, existing Int-only inputs still select the old stack;
- exact-rational stress beyond int63 for literals, coefficients, bounds, conflict
  multipliers, and model values;
- `make test`, the prescribed dune build, and the regenerated frozen check after the
  formal unfreeze.  Before that formal tranche, `bash tools/check_frozen.sh check` must
  continue to report all 14 interfaces matching.
