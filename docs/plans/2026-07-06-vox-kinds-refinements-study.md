# vox refinements × the kind/layout zoo: a soundness probe study

*Probe study, 2026-07-06. Compiler: vox-stdlib-v1 @ 921b7c4af,
`_install/bin/ocamlc.opt -vox-solver-path <lean4-4.31.0>`. All probes
under `scratch_probe/kinds/`; verified in private mktemp dirs.*

## TL;DR

vox was built against `value`-layout `int`/`bool`/ADTs. Probed against
`float#`, `int64#`, `int32#`, `nativeint#`, boxed `float`/`int64`,
unboxed products `#(…)`, unboxed records `#{…}`, `[@@unboxed]`
wrappers, `or_null`, `char`, `unit`.

**The refinement machinery is sound on every non-`value` kind probed.**
Every unsupported kind degrades to the uninterpreted solver sort `VoxU`
(equality only), and every boundary where a caller could reach for
arithmetic/structure it does not have is either a clean reject or a
fail-closed Lean error. **No ICE, no accepts-but-unsound cell** other
than the *already-documented* ideal-arithmetic gap on plain `int`.

The one true unsoundness — `x + 1 > x` verifies, false at `max_int` —
is on plain `int`, is **known and accepted** (vox's "ideal arithmetic"
TCB assumption), and is *not* reachable through any fixed-width unboxed
type, because those are never modeled as `Int`.

The realizable finding is a **diagnostic-hygiene / silent-accept** class:
order and arithmetic operators (and `fst`/`snd`) applied to a `VoxU`-sorted
term are accepted at elaboration and only fail deep in Lean with a leaked
`synthInstanceFailed` reported as *"NOT PROVED — the property may still
hold"*, when in fact it can never be discharged. Sound, but misleading.

## How each kind is modeled (the read side, probe-confirmed)

`dsort_of_type` (vox-editor `typing/vox_verify.ml:754`) maps:
`Predef.path_int → S_int` (Lean **`Int`, unbounded**), `path_bool →
S_bool`, `int iarray → S_iarray`; a parameterized/simple variant/record
→ `S_data`; an unlabeled boxed `Ttuple` → `S_tuple`; a declared
`refines`/`[@@vox.sort]` → that sort; **everything else → `S_other`
(`VoxU`, uninterpreted).**

Probe-confirmed base sorts (via `-vox-dump-vc-provenance`):

| Kind | Layout | vox sort | Evidence |
|---|---|---|---|
| `int` | value/immediate | `S_int` = Lean **`Int` (unbounded)** | `w1`,`w2` witnesses |
| `bool` | value/immediate | `S_bool` | baseline |
| `char` | value/immediate | `VoxU` | `d_char_order` synthInstance fail |
| `unit` | value/immediate | `S_data` (1-ctor variant) | `k_unit` |
| `float` (boxed) | value | `VoxU` | `d_fb_order` |
| `float#` | float64 | `VoxU` | provenance `x:float#~>VoxU` |
| `int64#` | bits64 | `VoxU` | provenance `x:int64#~>VoxU` |
| `int32#` | bits32 | `VoxU` | `k_int32u` |
| `nativeint#` | word | `VoxU` | `k_nativeu` |
| `int64` (boxed) | value | `VoxU` | crossing reject |
| `#(int*int)`,`#(_*_*_)` | unboxed prod | `VoxU` (`Tunboxed_tuple`, not `Ttuple`) | provenance |
| `#{a;b}` unboxed record | unboxed prod | `VoxU` (does *not* register as `S_data`) | provenance `x:r~>VoxU` |
| `t = W of int [@@unboxed]` | value | `S_data`, ctor **injective** | `n_unboxed_wrap` |
| `int or_null` | value_or_null | `VoxU`; ctors clean-rejected in preds | `ornull_disc` |

Key structural facts: unboxed products hit the `Tunboxed_tuple` desc,
which `dsort_of_type` does not match (only boxed `Ttuple`), so they fall
to `S_other`. Unboxed records likewise degrade to `VoxU` — unlike their
*boxed* twins, which become `S_data` with projectable fields. `[@@unboxed]`
single-ctor wrappers stay a `S_data` datatype and are seen through
soundly (constructor injective in the model).

## The matrix: kind × position → verdict

Verdict classes: **(a)** clean reject · **(b)** works + sound · **(c)**
accepts-but-unsound · **(d)** ICE · **(e)** silent-accept, fails-closed
in Lean with poor diagnostic.

Positions: **P1** param refinement · **P2** refined result · **P3**
let-binder annotation · **P4** ctor/record field of that kind · **P5a**
predicate *equality* over the kind · **P5b** predicate *order/arith* over
the kind · **P5c** predicate *projection* of the kind · **P6** `[@vox.via]`
skeleton over the payload · **P7** `total_`/`[@@vox.reflect]` twin param.

| Kind | P1/P2/P3 (eq refine) | P4 field | P5a eq | P5b order/arith | P5c proj | P6 via | P7 twin param |
|---|---|---|---|---|---|---|---|
| `int` | b | b | b | **c (overflow, documented)** | b (`fst`/`snd` on `int*int`) | b | b |
| `bool` | b | b | b | b | — | b | b |
| `char`,`float`,`float#`,`int64#`,`int32#`,`nativeint#`,`int64` | b (VoxU eq) | b (opaque field) | b | **e** (synthInstance fail) | — | b (opaque)/user-model → c* | **a** (reject: "must be int/bool/simple variant") |
| `#(int*int)` unboxed prod | b (VoxU eq) | b | b | e | **e** (`fst` mis-sorts) | b | a |
| `#{a;b}` unboxed record | b (VoxU eq) | n/a (opaque) | b | e | **a** (syntax) / opaque | b | a |
| `[@@unboxed]` wrapper | **b (see-through, sound)** | b | b | b (inner `Int`) | b (ctor) | b | b |
| `or_null` | b (VoxU eq) | b | b | e | **a** (ctor rejected) | b | a |
| crossing `int{_=Int64.to_int x}` | — | — | — | — | — | — | **a** (reject: total_/reflect only) |
| bare `float#`/`float#{…}` in value tuple slot | **a** (layout: float64 ≠ value) | — | — | — | — | — | — |

`*` P6-via for a fixed-width kind: vox lets you `[@vox.via]` an `int64#`
skeleton *through* a Lean `Int` function. That is structurally fine
(opaque skeleton → chosen image sort), but if the user's model asserts
`Int` (unbounded) semantics for a 64-bit value, arithmetic on the image
inherits the **documented ideal-arithmetic caveat** — it is a *trusted,
user-asserted* model, the same trust class as `[@@vox.reflect]` and the
plain-`int` stance, not a new hole vox opens on its own.

### Verdict counts

- **(a) clean reject:** 5 distinct boundaries — `Int64.to_int` crossing,
  `or_null` ctor in predicate, non-int/bool/simple-variant `total_`/reflect
  param, bare/`refined` unboxed type in a `value` tuple slot (layout
  check), unboxed-record field projection.
- **(b) works + sound:** all identity/equality refinements at every
  kind; `[@@unboxed]` see-through (injective); records carrying an
  unboxed field, incl. projecting that field; let-binder annotations;
  `int`/`bool`/`unit` baseline (arith honest *modulo* overflow).
- **(c) accepts-but-unsound:** exactly one — **plain `int` overflow**
  (documented, accepted). Plus the *user-asserted* via/reflect fixed-width
  model, which is trusted-by-design, not a vox defect.
- **(d) ICE/crash:** **none.**
- **(e) silent-accept, fail-closed in Lean:** order/arith on `VoxU`;
  unboxed-tuple `fst`/`snd`. Sound (never proves), poor diagnostic.

## Soundness hunts (priority order)

### 1. Fixed-width wrapping — the plain-`int` answer

**vox models OCaml native `int` as unbounded Lean `Int`.** Confirmed by
two verifying-but-false witness programs and by the dumped VC:

- `scratch_probe/kinds/w1_int_wrap_succ.ml`:
  `let succ_gt : (x:int) -> int{ _ > x } = fun x -> x + 1` — **VERIFIES**
  (VC `goal: x + 1 > x`, no hypotheses). False at `max_int` (wraps to `min_int`).
- `scratch_probe/kinds/w2_int_abs.ml`:
  `let myabs : (x:int) -> int{ _ >= 0 } = fun x -> if x<0 then -x else x`
  — **VERIFIES.** False at `min_int` (`-min_int = min_int < 0`).

The verifier is not accepting these vacuously: the negative control
`n0_neg_control.ml` (`x+1 = x+2`) is correctly **DISPROVED** with
counterexample `x=0`.

**This is KNOWN and ACCEPTED, not an unacknowledged hole.** It is vox's
foundational "ideal arithmetic" TCB assumption, stated in:
`vox-editor/typing/vox_reflect.ml:35-37` ("the logic's ints are unbounded
while the machine's wrap … overflow is outside the model");
`docs/plans/2026-07-06-vox-stdlib-design.md:197-198` (Trust ledger:
"unbounded `Int`; machine overflow is out of model");
`docs/plans/2026-07-06-vox-reflect-primitive.md:122-124`; and acknowledged
in tests `testsuite/tests/vox/demo/lean_binsearch.ml:56-59` and
`.../lib/ptrie.ml:10-12`. No test *exercises* wraparound.

**Fixed-width unboxed types do NOT inherit an `Int` model:** `int64#`,
`int32#`, `nativeint#` (and boxed `int64`) are all `VoxU`. You cannot
prove *any* arithmetic fact about them (order/`+` fail-closed at Lean),
so the wraparound unsoundness is **unreachable** through them — they are
strictly *safer* than `int`, at the cost of being uninterpreted. The only
way to bring a fixed-width value into `Int` arithmetic is to *assert* a
via/reflect model, which is trusted by design.

### 2. float# / float

Both boxed `float` and unboxed `float#` are `VoxU`. Refinements
elaborate; only equality is available. There is **no `Real`/`Float`
model** and none is silently assumed — NaN/`-0.0`/associativity never
arise because no float operation is interpreted. Order/arith fail-closed
(`d_fb_order`, `d_fu_arith`: `synthInstanceFailed`). Sound.

### 3. Unboxed products

`#(int*int)` / `#(int*int*int)` are `VoxU` (the `Pproj` path keys on
boxed `Ttuple`; `Tunboxed_tuple` never reaches it). Component projection
does **not** fire: `int{ _ = fst x }` on `x:#(int*int)` is accepted at
elaboration but mis-sorts in Lean ("Application type mismatch") and is
NOT PROVED (`p_unboxprod_proj`). Fail-closed, sound, not expressive.

### 4. Kind-soundness of the refinement type itself

**Preserved.** A refinement wrapper inherits its skeleton's jkind: bare
`float# * int` is rejected ("tuple element must be value; float# is
float64") and so is `float#{ 0 = 0 } * int` — *with the layout still
reported as `float64`* (`j_layout_refwrap.ml`). The `Trefine` node does
**not** erase the layout to `value`, so a refined unboxed value cannot be
smuggled into a slot its layout forbids. No layout-smuggling vector.

### 5. `[@@unboxed]` wrappers

**Seen through, soundly.** `type w = W of int [@@unboxed]` registers as a
1-ctor `S_data`; `w{ _ = W n }` verifies, and the negative control
`w{ _ = W (n+1) }` returning `W n` is **DISPROVED** (counterexample `n=0`)
— the constructor is injective in the model, so there is no double-apply
or collapse. The payload sorts as `Int`.

## Recommendations, ranked by soundness risk then value

**R1 — Clean-reject unsupported operators on `VoxU` at elaboration
(diagnostic hygiene; highest realizable value).** Today `_ > x`, `_ + 1`,
and `fst x` on a `VoxU`-sorted term pass the OCaml-side check and fail
only inside Lean, surfacing a raw `synthInstanceFailed` /
application-type-mismatch reported as *"NOT PROVED … may still hold."*
It is sound (fails closed) but tells the user the opposite of the truth
and leaks solver internals. The predicate sort-checker (`term`,
`vox_verify.ml:1783`) should reject an order/arith operator or `fst`/`snd`
whose operand sort is not `S_int`/`S_tuple`, with a message naming the
kind ("`>` is not available on values of sort VoxU (from `int64#`)").
This is the "refuse-with-diagnostic as default for unsupported kinds"
option, and it makes the whole non-`value` story legible.

**R2 — Keep fixed-width unboxed types uninterpreted by default; if ever
modeled, use a bit-vector theory, never `Int` (highest *risk* if
ignored).** The current default (VoxU) is safe. The trap is a future
stdlib author who `[@vox.via]`s `int64#`/`int32#` through a Lean `Int`
function to "get arithmetic": that silently asserts unbounded semantics
on a value the programmer *expects* to wrap — arguably more surprising
than plain `int`, because fixed width is the whole point of `int64#`.
Recommendation: document intN# as uninterpreted; if refinement arithmetic
on them is wanted, model via Lean `BitVec 64`/`BitVec 32` (faithful
wraparound) rather than `Int`, and gate `[@vox.via … Int]` over a
fixed-width skeleton behind an explicit "ideal-arithmetic" opt-in
mirroring the plain-`int` caveat.

**R3 — Unboxed-product projection support (additive, low risk).** Register
a `Tunboxed_tuple` sort family parallel to `S_tuple` (`VoxUT2`, …) and
wire `fst`/`snd` (or `#0`/`#1`) to it, so `#(int*int)` components can be
named in predicates. Sound today (opaque); this only adds expressiveness.
Same treatment could route unboxed records `#{…}` through
`vox_simple_record` so their fields project like their boxed twins.

**R4 — Float model, only if a concrete need arises (low priority).** Leave
`float`/`float#` uninterpreted by default. If float reasoning is wanted,
choose deliberately — Lean `Float` (models NaN/`-0.0`, kills naive
associativity/order rewrites) for machine fidelity, or `Real` for an
idealized model with a stated caveat like the int one. Never map float to
`Real` silently.

## Probe index (`scratch_probe/kinds/`)

`v.sh` — verify helper. Baseline/witness: `b0_int_baseline`,
`w1_int_wrap_succ`, `w2_int_abs`, `n0_neg_control`. Kind identity:
`k_{floatu,int64u,int32u,nativeu,floatb,char,unit}`. Order/arith
discriminators: `d_{i64,fb,char,fu}_order`, `d_fu_arith`. Products/records:
`p_unboxprod_id`, `p_unboxprod3`, `p_unboxprod_proj`, `urecord`,
`urecord_id`, `urecord_proj`, `rec_field_unboxed`, `rec_field_proj`.
Wrapper: `p_unboxed_wrap`, `n_unboxed_wrap`. Layout: `j_layout_tuple`,
`j_layout_refwrap`, `j_layout_refwrap_id`. or_null: `ornull`,
`ornull_prov`, `ornull_disc`. Crossing/via/twin: `cross_i64_toint`,
`via_unboxed`, `lean_twin_unboxed`, `let_binder_unboxed`. VoxU eq control:
`n_voxu_eq`.
