# vox: lambda reflection for relations (task #68)

Date 2026-07-07.  Goal (user): a relation supplied to a higher-order
contract as an ordinary OCaml lambda —
`apply_step (fun p q -> p <= q) f x` / `iter (fun x y -> y >= x) f n` —
with the lambda reflected into the logic and substituted at the binder,
so the client proves concrete consequences.  Built on the HOR study
(`2026-07-07-vox-higher-order-refinements-study.md`).

## What ships

**A relation is a dependent parameter of function type; a lambda argument
reflects to a Lean lambda and is substituted at the binder.**  Verified
end to end with the real Lean 4.31 solver (demo/lean_lambda_rel.ml):

```ocaml
[%%vox.lean {lean|
def IntRel := Int -> Int -> Prop
@[grind] def rHolds (r : IntRel) (a b : Int) : Prop := r a b
|lean}]

let apply_step :
      (r : (int -> int -> bool)) ->
      (f : ((x : int) -> int{ rHolds r x _ })) ->
      (x : int) -> int{ rHolds r x _ } =
  fun r f x -> ignore r; f x

let client (x : int) : int{ x <= _ } =
  apply_step (fun p q -> p <= q) (fun a -> a + 1) x   (* proves x <= result *)
```

The client's VC is `rHolds (fun p q => p ≤ q) x result ⊢ x ≤ result`;
grind unfolds `rHolds` and beta-reduces the substituted lambda.  The
producer `f` is checked against the opened contract
(`rHolds (fun p q => p ≤ q) a (a+1)` = `a ≤ a+1`).  Fail-closed is pinned
(mechanics/lean_lambda_rel_fail.ml): a producer that violates the relation
(`fun a -> a-1` under `<=`) and a goal the relation does not entail are
both refuted by grind, never silently accepted.

## Mechanism

1. **`Refinement.Plam of Ident.t list * pred`** — a Lean lambda `fun x .. =>
   body`.  Mirrors `Pquant`: fresh unsorted binders, alpha-compared by the
   binder pairing, emitted verbatim (`vox_verify.lean_of_pred`:
   `(fun v_x v_y => <body>)`), printed as `fun x y -> body`.  The Lean term
   is DERIVED from the OCaml body, so — unlike `[@@vox.reflect]` — the
   correspondence is checked, not assumed (zero added TCB).

2. **Reflection of the lambda argument.**  `Vox_reflect.translate_surface`
   (the application-site opener, Parsetree) gains a `Pexp_function` arm: it
   mints fresh idents for the (simple, unlabelled, value) parameters, threads
   a name→ident `bound` context (params are not in `env` during this
   pre-typing pass), reflects the body under it, and returns
   `Plam (ids, body)`.  Inside a lambda body comparisons/equalities DO
   reflect (`cmp_eq = bound <> []`): the body is a Prop-valued relation and
   an ill-sorted comparison is a solver error (fail-closed), so the surface
   pass need not know operand types.  The typed twin
   `Vox_reflect.translate_nameable` gains the matching `Texp_function` arm
   (using the typed `fp_param` idents), so the VC walker's re-substitution
   agrees with the type-checker's opening (alpha-equivalent).

3. **`vox_open_dependent_arrow`** already substitutes `translate_surface`'s
   result for the binder throughout the callee's remaining type
   (`Vox_dep.subst_binder`), so the lambda flows into `f`'s contract and the
   result with NO new code there.

4. **`S_arrow of dsort * dsort`** — a function type now models at the Lean
   arrow over its domain/codomain sorts (`int -> int -> bool` ⇒
   `Int -> Int -> Prop`, since `bool` models at `Prop`).  `dsort_of_type`
   gains a `Tarrow` case; `lean_sort` renders `(a -> b)`.  This is what makes
   the GENERIC body verify: `(r : (int -> int -> bool))` is declared
   `(v_r : Int -> Int -> Prop)` in the VC, so `rHolds v_r ..` is well typed
   (previously `r` sorted as `VoxU`, and the application was ill-typed).

## Named relation values ([@@vox.reflect]) flow too

A relation supplied as a NAMED value carrying `[@@vox.reflect "Sym"]`
flows its Lean symbol instead of degrading to an opaque binder.  The fix
is in two places that must AGREE: `Vox_reflect.translate` (the walker's
`stable_arg_name`) and `Typecore.vox_open_dependent_arrow`'s ident case
(the type-checker's opening) both substitute `Pfun(Sym, [])` for a bare
reference to a reflect value — otherwise the walker names it `Sym` while
the opener leaves an opaque `Pvar`, and the mismatch surfaces as "a
variable that has escaped its scope".  With both, `apply_step le_rel f x`
(where `external le_rel = "%lessequal" [@@vox.reflect "leRel"]`) reasons
with the concrete `leRel` (demo `client_named`).  Caveat (pre-existing,
R-a): `[@@vox.reflect]` on a plain `let` is still dropped from
`val_attributes`, so a named value must be an `external` (or `.mli val`);
the demo uses the real `%lessequal` primitive, whose runtime meaning is
exactly `leRel`.

## Composition

`rcomp` (block def `fun a c => ∃ b, r a b ∧ s b c`) applied to two
lambda-substituted relations verifies: `compose2 (fun a b -> a <= b)
(fun a b -> a < b) f g x` proves `rHolds (rcomp (..<=) (..<)) x result`
(demo `client_comp`) — grind unfolds `rcomp` and discharges the ∃.

## Ghost-invocation boundary

The relation is a REAL `int -> int -> bool`, not a phantom ghost — so it
MAY be invoked at runtime (demo `runtime_call`).  Safe BY CONSTRUCTION:
the reflected Lean term is derived from the same OCaml body, so the
runtime result and the logical meaning agree.  This is *stronger* than a
phantom ghost sort (which cannot be invoked at all).  Modeling note: vox
does not reflect `r a b` (a relation PARAMETER applied) as a spec term
(`r` is neither `total_` nor `[@@vox.reflect]`), so a contract over an
*invocation* of `r` leaves its result opaque; the relation is applied in
the LOGIC (`rHolds`/`rcomp`/a fixpoint), where its meaning is used.  No
soundness consequence.

## Interaction with #67 (bool connectives / ctor-wrap)

#67 (954f36bb3) fixed `decompose_bool` to thread guarded operand facts
through OCaml `&&`/`||`/`not` EXPRESSIONS and constructor-wrapped tier-2
calls.  A lambda relation is substituted into a REFINEMENT predicate,
where a `&&` is `Refinement.Pand` (handled by `lean_of_pred`) — a
different layer from `decompose_bool` (bool-valued *expressions*).  So
lambda terms do not reach #67's machinery; a lambda-derived relation
feeding a `&&` GOAL verifies fine (demo `client_and`).  Orthogonal;
they compose.  (Rebased onto 954f36bb3; the earlier do-not-substitute
conservatism is moot.)

## Files
- `typing/refinement.ml`: `Plam` constructor + all walkers (equal α, subst,
  free/mem, map_paths, printer, …).
- `typing/vox_reflect.ml`: `translate_surface` (`bound` threading + lambda
  arm + comparison-in-lambda), `translate_nameable` (`Texp_function` arm),
  `exact_result_rhs` no_bound arm, `translate` (bare-`[@@vox.reflect]` arm).
- `typing/typecore.ml`: `vox_open_dependent_arrow` ident case resolves a
  `[@@vox.reflect]` value to its symbol (agrees with the walker).
- `typing/vox_verify.ml`: `S_arrow` + `dsort_of_type`/`lean_sort` + emission
  (`lean_of_pred` Plam) + every dsort/pred walker.
- `typing/vox_dep.ml`, `lambda/translcore.ml`: Plam arms.
- Tests: `testsuite/tests/vox/demo/lean_lambda_rel.ml` (positive, two lambda
  bodies), `mechanics/lean_lambda_rel_fail.ml` (fail-closed).

Full vox suite: 186/186 green before these tests (no regression from Plam
or the S_arrow dsort change); the two new tests pass.

## Remaining ergonomic ask (surface only, not a soundness or capability gap)

The relation binder's function type must be **parenthesized**:
`(r : (int -> int -> bool))`, not `(r : int -> int -> bool)`.  Root cause:
the dependent-binder grammar production `vox_named_type` (parser.mly:5111)
accepts only `atomic_type` after the colon, so an un-parenthesized function
type is misparsed as a labelled arrow type (`r:int -> ...`) — which then
cannot be applied (OCaml wants the label, vox's application path rejects
it).  A parenthesized function type IS atomic, so it parses as a proper vox
binder and applies positionally.  Fix (separate, small-but-conflict-prone):
widen `vox_named_type`'s inner type to accept function types without
ambiguity against OCaml's `label:type` arrow syntax.  Until then, the extra
parens are the documented spelling.

Related surface note: a lambda is only reflected in ARGUMENT position (the
dependent-binder opener), NOT inside refinement text `{ ... }` — the
predicate grammar has no lambda.  A relation named in a refinement uses a
spec constant / block def, as before.
