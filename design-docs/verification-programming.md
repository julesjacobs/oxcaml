# Verification programming in Vox

A verified operation computes its result and exposes its guarantee in a result
refinement. Its client unwraps that result with `let refine_` and can use the
guarantee immediately. Reusable lemmas are total functions returning refined
unit. Calls made only to establish facts belong inside `ghost_`.

## Operations and lemmas

The constant-folding demo separates an executable transformation, its induction
proof, and the operation a client uses:

```ocaml
let (eval_folded @ total) (expression @ total) input :
    {result : int | result === eval expression input} =
  let (result @ total) = (eval (fold expression) input : int @ total) in
  let refine_ proof = ghost_ (fold_correct expression input) in
  refine_ result
```

`fold_correct` is a total function whose result is
`{u : unit | eval (fold expression) input === eval expression input}`.
Its recursive calls supply induction hypotheses; generated `[@def]` lemmas
supply equations for opaque functions. Its body uses ordinary calls and local
model computations. Erasing the call erases that entire computation.

A result refinement should describe the operation's useful guarantee. Keep
separate lemmas for additional laws, such as associativity and pointwise set
extensionality. An abstract set can expose pointwise membership lemmas without
forcing each insertion to accept a query. Clients erase those lemma calls.

Use `Spec` and `Proof` modules when the size of the example makes their roles
hard to follow. Small examples can keep their definitions together. Module
signatures should preserve the totality needed by callers' ghost expressions.

## Proof blocks and fact scope

For one lemma, use `let refine_ proof = ghost_ (lemma args) in ...`.
For several proof steps, put them in one block and export a refined-unit
conclusion. For example, the queue's enqueue operation computes `result` first:

```ocaml
let refine_ proof = ghost_ (
  let singleton = [value] in
  let reversed = reverse rear in
  let refine_ model = contents_def q in
  let refine_ reverse_law = reverse_def next_rear in
  let refine_ association = append_associative front reversed singleton in
  let u = () in
  (refine_ u : {u : unit |
    contents result === append (contents q) [value]}))
in
refine_ result
```

A helper's result refinement carries its conclusion to callers. A helper
returning plain `unit` exposes no proposition through its interface. Include
every fact needed by callers in its result refinement. Use the same explicit
conclusion in a local proof block so it can later be extracted into a helper;
do not depend on the visibility of its intermediate facts in the local VC.

Keep proof-only model traversals inside the block. When a model value is shared
by several nearby proof calls, `let model = ghost_ (model_of input) in ...` also
works. Computing the model outside `ghost_` would still perform that traversal.

`ghost_` requires total computations, including its captured function values.
Collection signatures preserve conditional immutability: sets depend on their
element type, and maps depend on their key and value types. Collections of
immutable data can be captured in ghost proofs while collections containing
mutable data retain their access restrictions.

`ghost_` checks the proof and erases its evaluation. Marking a parameter `@ ghost`
controls how its value may be used; it does not erase evaluation of an ordinary
argument expression.

Definition lemmas preserve `ghost_` in their logical bodies. The solver reasons
about the value of a ghost expression; runtime predicate replay preserves its
erasure and checks whether the result may be used at real mode. A `[@def]`
operation can put `ghost_` directly around its proof calls.

## Models, evidence, and runtime checks

Use a local ghost value for a local proof. Use `Ghost.t` or a `@@ ghost` record
field when an erased model or witness must be stored in a representation or
carried through an interface. A lemma does not need a `Ghost.t` wrapper merely
because a caller erases its evaluation.

Keep evidence executable when a caller consumes it. Regex `recognize` and
`sound` construct membership derivations that clients can inspect. Their
correctness-only lemma calls erase, while derivation construction remains.
In `contract`, inspecting a derivation's word selects the recursive case, so
that word computation remains executable. Words used solely in equations erase.
The DFA Boolean client erases its correctness proof and returns a refined bool.

Use `assume_` for intentional runtime validation. Its successful check establishes
a fact; failure remains observable. Such checks cannot be moved into `ghost_`.
The machine-integer Fibonacci demo retains checked arithmetic identities and
range errors. Its inductive helper consequently remains executable. The bigint
Fibonacci demo supplies the fully static, total version and erases the induction
proof at the fast-doubling operation boundary.

Executable test oracles also stay executable. Examples include comparisons
against a second set implementation, sparse-array observations in both update
orders, and the independent regex matcher. These exercise computed results in
addition to the static contracts.

## Demo coverage

The [demo tour](../testsuite/tests/vox/README.md) lists every stage. Apply the
idiom according to what each example teaches:

| Demos | Verification idiom |
| --- | --- |
| Clamp, constant folding, bounded search | Refined operation results; erased proof calls. |
| Bigint Fibonacci | Refined recursive accumulators; erased inductive arithmetic proof. |
| Machine-integer Fibonacci | Static unfolding erases; checked arithmetic and range validation execute. |
| Integer lists | Ordinary total induction lemmas; erased client law applications. |
| Integer and polymorphic list sets, AVL sets | Erased invariant proofs in operations; erased pointwise laws in clients. |
| Functional queue | Abstract sequence contract; ghost model traversals and explicit proof-block conclusions. |
| Standard lists, sets, maps, immutable arrays | Refined library operations supply facts directly. |
| Persistent environments | Runtime input validation, refined observations, erased definition lemma. |
| AVL/standard-set comparison, sparse immutable arrays | Refined paired observations and executable comparison oracles. |
| Regex derivatives and DFA | Refined executable evidence; erased correctness-only proofs. |
| Ghost demos | Erasure, fact propagation, stored ghost values, and rejected real uses. |
| Syntax, totality, SMT, unfolding, and rejection fixtures | Keep the construct under test visible, including intentional failures. |

Adding ghost wrappers to a lemma's internal steps is unnecessary when the
entire invocation is already erased. Likewise, unwrapping a refined executable
result is not a reason to erase the operation producing it.
