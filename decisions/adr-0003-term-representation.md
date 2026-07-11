# ADR-0003 — Term and Sort Representation (FINAL)

Status: Accepted (frozen) 2026-07-11 — adversarial review + verification pass recorded in logs/adr-0003-adversarial-review.md
Supersedes: `adr-0003-term-representation-draft.md` (APPROVE-WITH-REQUIRED-
CHANGES; review at `adr-0003-adversarial-review.md`). All 8 required changes
incorporated, per master adjudications (see changelog at end).
Freezes: `Iarr`, `Sort`, `Symbol`, `Rank`/`Env`, `Term` node set +
smart-constructor signatures, `Theory_view`, `Term.Debug.check` invariants, and
the solver-pipeline invariants (Int-`Ite` removal, `div`/`mod` elimination).
Cites INVARIANTS I1/I2 (ill-formed terms unconstructible; no in-place
corruption), I3 (any `Term.t` well-sorted & hash-consed), I6 (term ids
deterministic across runs, fixed construction order).

## Context

The term layer is the frozen spine every module reads: the clausifier (§5)
walks boolean structure and enumerates atoms; EUF (§6) congruence-closes
uninterpreted applications and keys O(1) maps on term ids; LIA (§6) reads the
linear form and produces Farkas-ready `≤0` rows (§7); Nelson-Oppen needs a clean
"shared equality"; E-matching (stage 2, §2) matches triggers against structure;
the oracle cache (§8) needs an order-invariant canonical key; certificates (§7)
require equal terms to be the same node. The adversarial review confirmed the
architecture (hash-consed dynamically-sorted node, booleans-as-terms, eager
linear normalization, `private` single-construction-path — the fast-solver
consensus of Z3/cvc5/Alt-Ergo). This final settles the details it found
underspecified.

---

## Decision 1 — Arithmetic: normalized linear form, in the term type

A single `Arith of linear` node is the canonical form of every compound Int
term; `linear = { coeffs : (t * int) Iarr.t; const : int }`, `coeffs` tag-sorted,
coefficients nonzero, no child that is itself `Arith`. All order comparisons
lower to one `Le` atom (`t ≤ 0`). Multiplication is by an integer literal only
(v1 = linear LIA; `var·var` rejected at construction, revisited only for a
nonlinear theory). Rationale unchanged from draft (single source of truth; LIA
*reads* the form; `x+1`=`1+x` and `2x`=`x+x` coincide) and endorsed by review.

Two consequences the review sharpened:

- **`App` vs `Arith`/`Le` is the theory-dispatch signal (load-bearing).** EUF
  congruence-closes only `App`; `Arith`/`Le` are opaque leaves owned by LIA;
  shared equalities flow through Nelson-Oppen.
- **gcd normalization in `Le` (new, required #5).** The `le` constructor divides
  `coeffs` by `g = gcd(|cᵢ|)` and floor-tightens the constant over ℤ: from
  `Σcᵢaᵢ + k ≤ 0` produce `Σ(cᵢ/g)aᵢ + ⌈k/g⌉ ≤ 0`. Exact over ℤ, cheap, and
  makes `2x ≤ 2` and `x ≤ 1` the same node. Int **equalities are *not* put in a
  zero-normal-form** at construction (`Eq` stays a symmetric pair, polymorphic
  over sorts); `x = y+1` and `x−y = 1` therefore remain distinct nodes — LIA
  re-derives their content and the cache layer (Decision 4 / below) canonicalizes
  them. This is documented, not a soundness issue. **gcd tightening is new
  integer-reasoning code in the trusted computing base (N2):** the Lean-from-dump
  path cannot independently check it (the dump is post-normalization, so oracle and
  solver see the same tightened atom), so it is validated by pre-labeled benchmarks
  (whose labels are computed on the original comparison) plus targeted unit/property
  tests owed by the core implementation task — flagged because DESIGN §10 already
  lists smart constructors as TCB and this step widens that surface.

---

## Decision 2 — Boolean structure: connectives are Bool-sorted term nodes

Bool is an ordinary sort; connectives are nodes; the clausifier consumes
`Term.t`. `Implies a b` lowers to `Or[Not a; b]`. `Iff`/`Xor` reuse `Eq`/`Not`.

**Precise `is_atom` (required #2, HIGH).** A Bool-sorted `Eq` is *iff* — a
connective in disguise — so it is **on the connective list**: the clausifier
descends into it and Tseitin-encodes `e ↔ (a ↔ b)`; it is **never** handed to
EUF as an opaque atom. Formally, for a Bool-sorted term `t`:

> `is_atom t  ⟺  t.sort = Bool ∧ top(t)` is none of: `And`, `Or`, `Not`,
> a *result*-Bool-sorted `Ite`, or an `Eq` whose *arguments* are Bool-sorted.

(The `@Bool` shorthand is deliberately avoided in the frozen definition because it
would be ambiguous — for `Ite` the relevant sort is the *result*, for `Eq` it is
the *arguments* since every `Eq` is already result-Bool. N1.) The atoms are
exactly: `Le _`, `Eq(a,b)` with `a.sort ≠ Bool`, `App(p,args)` with Bool codomain
(incl. nullary Bool constants = propositional variables), and `Bool_const`.
`Xor = Not(Eq …)` over Bool-sorted args inherits the fix (both the `Not` and the
inner Bool-argument `Eq` are connectives). This closes the `(P∧Q)↔R` / `P↔Q`
severance the review found.

---

## Decision 3 — Negation: `Not` node; signed literals are solver-internal

`Not` is a node with double-negation elimination and constant folding only — no
NNF/De Morgan at construction (clausifier's job; preserves subterm sharing
between a term and its negation). Signed literals (atom-id + polarity) are the
SAT core's representation, not `Term.t`. Negated comparisons are rewritten by
LIA at internalization: `¬(t ≤ 0)` ≡ `1 − t ≤ 0` over ℤ (review confirmed
exact). `Debug.check` forbids `Not(Not _)` and `Not Bool_const`.

---

## Decision 4 — Mechanics: explicit interning, strong table, deterministic tags

- `Symbol.t = private int`, interned in `Env` in declaration order → I6.
  Names/ranks live in `Env`, keyed by id.
- Node layout `{ node; sort; tag : int }`. `tag` (monotonic counter) is
  identity: `equal`/`compare`/`hash` and `Term.Set`/`Map`/`Table` use it.
- **Strong (non-weak) monotonic intern table**, no GC collection — weak+GC
  breaks I6 (nondeterministic ids). Terms are not freed on `pop`; table grows
  with the session. Escape hatch: a deterministic arena reset, never weak refs.
- Tag determinism depends only on construction order, never on `Hashtbl`
  traversal (the table is never semantically iterated).
- **`equal_node`/`hash` scalar-payload contract (required #8, MED).** Bucket
  comparison and hashing **must** include every scalar payload: the `int` in
  `Int_const`, the `bool` in `Bool_const`, the `Symbol.t` in `App`, and the
  `int` coefficients **and** `const` in `linear` (via `Iarr` element hashing).
  Omitting any collapses `x+2 ≡ x+3` / `Int_const 2 ≡ Int_const 3` — silent
  unsoundness, the top §12 risk. `Debug.check` gains a scalar-distinctness
  property and the core implementation task carries a property-test obligation
  (`x+2` vs `x+3`, `Int_const k` families, `App` same-args-different-symbol).
- **Cache key is NOT the tag (required #5, MED).** Hash-cons identity is
  construction-order-dependent; a trivially-reordered but logically-identical
  query gets different tags, hence different `And`/`Or`/`Eq` orderings. The
  oracle cache key is therefore a **separate, order-invariant canonical
  serialization** (structural DAG renumbering / alpha-renaming, sorted
  assertions) computed by the cache/gate layer — never conflated with tag
  identity. The gate already owns its own canonicalization; these two must stay
  distinct. Decision 1's earlier "free cache key" phrasing is retracted: hash-
  consing gives *within-run* sharing and O(1) equality, not the cross-run cache
  key.
- Sort checks happen once, in the smart constructors; `t.sort` is O(1)
  thereafter (I3). Arithmetic/boolean constructors reject ill-sorted operands
  (`add`/`sub`/`neg`/`mul_const`/`le` require Int; `eq` same-sort; `and_`/`or_`/
  `not_` and `ite` condition require Bool) — `Debug.check` re-derives as backstop.

---

## Decision 5 — Constructor set and normalization invariants

**Node set (9, frozen).** No new nodes were added for any review finding.

| node | sort | notes |
|---|---|---|
| `Bool_const of bool` | Bool | |
| `Int_const of int` | Int | native int; overflow-guarded |
| `App of Symbol.t * t Iarr.t` | rank codomain | incl. nullary consts; **EUF-congruent**; hosts reserved `div`/`mod` |
| `Arith of linear` | Int | canonical linear form; **LIA leaf to EUF** |
| `Le of t` | Bool | `arg ≤ 0`, `arg : Int`; only order atom; gcd-normalized |
| `Eq of t * t` | Bool | same-sort, tag-ordered; Bool-sort ⇒ iff (a *connective*, Decision 2) |
| `Not of t` | Bool | |
| `And of t Iarr.t` / `Or of t Iarr.t` | Bool | n-ary ≥2 |
| `Ite of t * t * t` | branch sort | cond Bool, branches share result sort |

Nullary constants and program variables are `App(sym,[::])`. Stage-2 bound
variables are a *future* node (e.g. de Bruijn `Bvar`) added under the unfreeze
ritual — this is acknowledged to require touching every exhaustive `match`
(clausifier, theories, printer, evaluator) and re-freezing `SPINE.md`, accepted
under the small-module-rewrite philosophy.

**Normalization enforced at construction:** no nullary/unary `And`/`Or`
(`[]→True/False`, `[x]→x`); `And`/`Or` flattened, deduped, tag-sorted,
constant-folded; `Not` double-neg + const fold (no De Morgan); `Eq` tag-ordered,
`Eq(x,x)→True`, const-fold; `Arith` merged/nonzero/tag-sorted, unwrap
`{[(a,1)];0}→a` and `{[];k}→Int_const k`; `Le` lowers `<`,`≤`,`≥`,`>` to `≤0`
(strict `+1` over ℤ), gcd-normalized, const-fold; `Ite` folds
`Ite(True/False,…)` and `Ite(_,a,a)→a`; all Int/Bool ops const-fold with
overflow guard. **Deferred to later passes:** De Morgan/NNF and Bool-`Ite`
lowering (clausifier); Int-`Ite` removal and `div`/`mod` elimination
(preprocessing, below); nonlinear multiplication; cross-atom simplification.

**Solver-pipeline invariants (not construction restrictions — users build these
freely):**

- **Int-`Ite` removal (required #3, MED-HIGH).** A named *term-ite-removal*
  preprocessing pass lifts every Int-sorted `Ite(c,a,b)` to a fresh
  `App(t,[::])` with Tseitin-visible guarded equalities `(c → t=a) ∧ (¬c → t=b)`.
  Post-pass invariant, enforced by `Debug.check` **on the preprocessed formula**:
  *no Int-sorted `Ite` appears below a theory atom* (`Arith` coeff, `Le`/`Eq`
  operand, or `App` argument). LIA never sees a value-`ite`, so the spurious-`sat`
  hole is closed. `abs` (below) feeds this pass.
- **`div`/`mod`/`abs`/`distinct` (required #4, MED) — no new nodes:**
  - `distinct` desugars to pairwise `Not(Eq)` **at construction**
    (`Context.distinct`); n is small in VCs.
  - `abs x` desugars **at construction** to `Ite(x ≥ 0, x, neg x)` (then removed
    by the Int-`Ite` pass).
  - `div`/`mod` build `App(div_sym|mod_sym, [::x; d::])` on **reserved built-in
    symbols** (declared once in every `Env`); a named *div/mod elimination* pass
    replaces each by a fresh `q`/`r` with `x = d·q + r ∧ 0 ≤ r < |d|` (euclidean).
    **Only nonzero constant divisors** are supported: `Context.div`/`mod_` reject
    a non-`Int_const` or zero divisor with a clean `Unsupported` error (a
    documented v1 limitation, not a crash). Arrays/select-store and datatypes/BV
    stay out of v1 scope (modelable later as `App` or plugin theories); arrays
    are common in refinement VCs and are flagged for an early post-v1 look.

---

## Decision 6 — Sorts, symbols, ranks; where `App` is checked

`Sort.t = private Bool | Int of int_kind | Uninterpreted of Symbol.t`,
`int_kind = Mathematical` in v1 (the §1 width hook for future `Int32`/`Int64`/
`Bv`). `Sort.equal`/`hash` are O(1). Uninterpreted sorts are 0-arity in v1.
`Rank = { domain : Sort.t Iarr.t; codomain : Sort.t }` lives in `Env`;
predicates are symbols with `codomain = Bool`. `App` is sort-checked in
`Context.app`: look up the rank, check arity and each argument sort, set result =
codomain, else raise `Sort_error`. This is the only place ranks are consulted.

**Context threading (required #7, MED).** There is one session `Context`
(bundling `Env` + intern table + tag counter); *all* term construction threads
it — the SMT-LIB parser, the clausifier, preprocessing (Int-`Ite` removal,
`div`/`mod` elimination), **and the theory/combination layer mid-solve**:
Nelson-Oppen shared equalities `Eq(x,y)` and branch-and-bound branch literals
(`Le` atoms) are constructed as terms through the same Context, so they share the
tag stream (I6) and hash-consing. Consequence, accepted for v1: the monotonic
table grows with the *search*, not just the input; the revisit trigger is memory
pressure on the corpus (mitigation: deterministic arena reset). The model
evaluator, SMT-LIB printer, and Lean encoder need **no** Context (they only read
`node` / consume text dumps).

---

## Proposed `.mli` sketch (frozen shapes)

```ocaml
module Iarr : sig                              (* portable immutable array; ADR-0002: stock OCaml 5.4 *)
  type +'a t                                   (* ABSTRACT; represented as 'a array; no mutator, no to_array *)
  val of_list  : 'a list -> 'a t
  val of_array : 'a array -> 'a t              (* COPIES (Array.copy); caller may keep/mutate its own array *)
  val to_list  : 'a t -> 'a list
  val length   : 'a t -> int
  val get      : 'a t -> int -> 'a            (* O(1) *)
  val iter     : ('a -> unit) -> 'a t -> unit
  val iteri    : (int -> 'a -> unit) -> 'a t -> unit
  val fold     : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val map      : ('a -> 'b) -> 'a t -> 'b t
  val exists   : ('a -> bool) -> 'a t -> bool
  val for_all  : ('a -> bool) -> 'a t -> bool
  val equal    : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare  : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val hash_fold : (int -> 'a -> int) -> int -> 'a t -> int   (* accumulator: MUST fold every element *)
end                                            (* NO aliasing / unsafe constructor on the public surface *)
```

`Iarr` replaces the draft's `t array` fields (required #1, HIGH): `array` fields
are mutable *through pattern-matching* even under `private`, so client code could
`coeffs.(0) <- …` and corrupt a shared hash-consed node (violating I1/I2 and
poisoning the intern table). The type is **abstract** (not `private 'a array`) and
carries no mutator and no `to_array`/`unsafe_of_array` — so no read-path hands back
the backing array and no write-path exists; corruption is a type error, not a
convention. Covariance (`+'a`) is sound precisely because the value is immutable.
Both public constructors **copy** (`of_list` builds fresh; `of_array` does
`Array.copy`). The one-time copy is dominated by the normalization already run at
construction (sorting/dedup/flatten of the same array), so the review's
"near-zero runtime" holds with no aliasing escape hatch on the public surface.

**Internal no-copy constructor — build-enforced exclusion, not a comment
(fixes B1).** The draft's `unsafe_of_array` under `(**/**)` was a defect:
`(**/**)` is ocamldoc's *stop-comment* (documentation-only) — it removes nothing
from the signature, so the value stayed fully exported and *aliased* the caller's
mutable array, reopening the exact I1/I2 hole finding #1 was meant to close. It is
**removed from the frozen public surface entirely.** If a `core/` hot path wants to
skip even the single copy, the aliasing constructor lives in a *separate* module
`Iarr_unsafe` declared a dune **`(private_modules Iarr_unsafe)`**: a private module
is compiled into the library but is **mechanically invisible to every downstream
consumer** — any reference from outside `core/` (e.g. a child-agent theory plugin)
fails to compile — so exclusion is enforced by the build system, which is what
"internal-only by convention" failed to do. Its signature is a single
representation-preserving cast
`val of_array : 'a array -> 'a Iarr.t = "%identity"` (a no-op; sound because
`Iarr.t` is laid out as `'a array`; ownership of the argument transfers to `Iarr`
and the caller must not retain it). It is referenced only within `core/`, never
appears in the frozen `.mli` set or `SPINE.md`, and is optional — the copying
constructors are the default and the only thing the interface promises.

**Ergonomic cost, acknowledged:** no `match … with App(_, [| a; b |])` array
patterns — clients use `Iarr.get`/`length`/`to_list`/`fold`. A future swap to
OxCaml `iarray` is a mechanical change under the unfreeze ritual once ADR-0002's
stock-OCaml pin is lifted.

```ocaml
module Symbol : sig
  type t = private int
  val equal : t -> t -> bool   val hash : t -> int   val name : t -> string
end

module Sort : sig
  type t = private Bool | Int of int_kind | Uninterpreted of Symbol.t
  and int_kind = Mathematical
  val bool : t   val int : t   val uninterpreted : Symbol.t -> t
  val equal : t -> t -> bool   val hash : t -> int          (* O(1) *)
end

module Rank : sig type t = { domain : Sort.t Iarr.t; codomain : Sort.t } end

module Env : sig
  type t
  val create       : unit -> t                              (* pre-declares reserved div_sym/mod_sym *)
  val declare_sort : t -> string -> Symbol.t                (* 0-arity *)
  val declare_fun  : t -> string -> Rank.t -> Symbol.t
  val rank         : t -> Symbol.t -> Rank.t
end

module Term : sig
  type t = private { node : node; sort : Sort.t; tag : int }
  and node = private
    | Bool_const of bool
    | Int_const  of int
    | App    of Symbol.t * t Iarr.t                          (* EUF-congruent; nullary = constant *)
    | Arith  of linear                                       (* Int; LIA leaf to EUF *)
    | Le     of t                                            (* (arg <= 0), arg : Int *)
    | Eq     of t * t                                        (* same sort, tag-ordered; Bool = iff/connective *)
    | Not    of t
    | And    of t Iarr.t
    | Or     of t Iarr.t
    | Ite    of t * t * t
  and linear = { coeffs : (t * int) Iarr.t; const : int }    (* tag-sorted, coeff<>0, no Arith child *)

  exception Overflow                                          (* raised BEFORE any table mutation *)
  exception Sort_error  of string
  exception Unsupported of string                             (* e.g. non-constant divisor *)

  val equal : t -> t -> bool   val compare : t -> t -> int   val hash : t -> int   (* all via tag *)
  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  module Table : Hashtbl.S with type key = t
  module Debug : sig val check : t -> unit end
end

module Context : sig
  type t
  val create : Env.t -> t

  val const : t -> Symbol.t -> Term.t
  val app   : t -> Symbol.t -> Term.t list -> Term.t
  val int_const : t -> int -> Term.t   val bool_const : t -> bool -> Term.t

  val add : t -> Term.t -> Term.t -> Term.t   val sub : t -> Term.t -> Term.t -> Term.t
  val neg : t -> Term.t -> Term.t             val mul_const : t -> int -> Term.t -> Term.t
  val linear_combination : t -> (int * Term.t) list -> int -> Term.t   (* convenience for LIA (additive) *)
  val div  : t -> Term.t -> Term.t -> Term.t  (* divisor must be nonzero Int_const, else Unsupported *)
  val mod_ : t -> Term.t -> Term.t -> Term.t  (* same *)
  val abs  : t -> Term.t -> Term.t            (* desugars to Ite *)

  val eq : t -> Term.t -> Term.t -> Term.t
  val le : t -> Term.t -> Term.t -> Term.t    (* lt/ge/gt lower to le; gcd-normalized *)
  val lt : t -> Term.t -> Term.t -> Term.t    val ge : t -> Term.t -> Term.t -> Term.t
  val gt : t -> Term.t -> Term.t -> Term.t
  val distinct : t -> Term.t list -> Term.t   (* pairwise Not(Eq) *)

  val not_ : t -> Term.t -> Term.t
  val and_ : t -> Term.t list -> Term.t       val or_ : t -> Term.t list -> Term.t
  val implies : t -> Term.t -> Term.t -> Term.t   val iff : t -> Term.t -> Term.t -> Term.t
  val ite : t -> Term.t -> Term.t -> Term.t -> Term.t
end

module Theory_view : sig
  type atom =
    | Equality  of Term.t * Term.t             (* non-Bool Eq; uninterpreted / shared equality *)
    | Le_zero   of Term.t                        (* LIA: term <= 0 *)
    | Predicate of Symbol.t * Term.t Iarr.t      (* Bool-codomain App *)
    | Bool_lit  of bool
  val is_atom : Term.t -> bool                   (* Decision 2 definition; false for Bool-Eq *)
  val atom    : Term.t -> atom                    (* requires is_atom *)
  val is_app  : Term.t -> bool                    (* EUF congruence applies *)
  val linear  : Term.t -> Term.linear option
end
```

## `Term.Debug.check` — checked invariants

1. **Well-sorted (I3):** child sorts match each node; `App` matches its rank;
   `Le` arg Int; `Eq` args share a sort; `Ite` cond Bool and both branches equal
   result sort; `And`/`Or`/`Not` Bool.
2. **Hash-consed:** every subterm is its own canonical representative; tags
   unique.
3. **Scalar-distinctness (required #8):** re-interning a node with any scalar
   payload changed (`Int_const`, `Bool_const`, `App` symbol, any `linear` coeff
   or `const`) yields a *different* tag — i.e. `equal_node`/`hash` observe all
   payloads. (Backed by a property test in the core task.)
4. **And/Or:** arity ≥ 2; no same-connective child; strictly tag-increasing; no
   `Bool_const` child.
5. **Not:** child neither `Not` nor `Bool_const`.
6. **Eq:** `fst.tag < snd.tag`; non-identical.
7. **Arith:** all coeffs nonzero; atoms tag-sorted, distinct, Int-sorted, none an
   `Arith`; not a form that should have unwrapped.
8. **Le:** arg Int, not an `Int_const`; **gcd(|coeffs|) = 1** (gcd-normalized).
9. **No overflow witness:** every `Int_const`/`linear.const` in native-`int`
   range.
10. **Pipeline (on preprocessed formula only):** no Int-sorted `Ite` below a
    theory atom; no reserved `div_sym`/`mod_sym` App remains.

## Overflow / Unsupported contract (required #6, B2)

Native `int` coefficients are a **v1 decision**: DESIGN §1's "mathematical ℤ"
constrains variable *values*, not coefficient magnitude, and refinement VCs carry
tiny coefficients (Gomory cuts deferred; B&B literals have coefficient 1). Every
arithmetic constructor that can overflow — `Int_const` folding, coefficient merge
(`c₁+c₂`), `mul_const`/scaling, `neg`/`sub` (incl. `neg min_int`), gcd tightening
— **raises `Term.Overflow` before any intern-table mutation** (construction is
effect-free until the node is interned, so no half-built state escapes). The
**catch boundary is the session/preprocess layer**, which converts it to verdict
`unknown` — never a crash, never partial state, and never interleaved with
theory-state mutation (construct-then-mutate discipline; mid-solve term creation
per Decision 6 constructs fully before touching the simplex tableau/trail). The
LIA plugin's internal numeric type inherits the same no-silent-wraparound rule
(out of this ADR's frozen scope, stated so simplex cannot wrap unsoundly). Revisit
trigger: any real VC overflowing native `int` → swap `linear`'s `int` for a
stdlib bignum.

**`Unsupported` (B2)** — raised by `Context.div`/`mod_` on a non-constant or zero
divisor (an out-of-QF_UFLIA-fragment input, e.g. a nonlinear `x mod y`) — obeys
the **same contract as `Overflow`**: raised before any intern-table mutation
(construction is effect-free until intern), and caught at the same session/parse
boundary, where it becomes a clean verdict `unknown` / skip of the offending query
— never a crash, never partial state. A frozen exception must state its catch
point; both `Overflow` and `Unsupported` terminate at the session layer and
degrade to `unknown` rather than propagating.

## What would make us revisit

- A nonlinear / datatype / BV / **array** theory (arrays are common in refinement
  VCs) — new `int_kind` widths and/or `App`-modeled or plugin theories.
- Coefficient overflow observed → stdlib bignum in `linear`.
- Stage-2 E-matching: arithmetic-argument triggers (`f(x+1)`, `sel(store(a,i+1,v),j)`)
  are **AC-matching** under flat-sorted normalization (survivable — Z3/cvc5 do
  both — but stage 2 should expect it); if it defeats real triggers, push some
  normalization from construction into the theory.
- Quantifiers (stage 2): adds a bound-variable node + scope discipline; forces an
  unfreeze touching every exhaustive `match`.
- Memory pressure from the monotonic table under B&B / stage-3 fixpoint loops →
  deterministic arena reset (never weak refs).

## Open questions (resolved / non-blocking)

1. *Stored vs recomputed structural hash* — implementer's choice, does not affect
   the frozen interface; non-blocking. (Leaning: store it, since `Iarr` hashing is
   O(n).)
2. *Batch arith builder* — **decided**: include `Context.linear_combination`
   (additive, convenient for LIA; harmless).
3. *`Sort_error`/`Overflow`/`Unsupported` payloads* — strings for v1; cosmetic,
   non-blocking.

## Changelog vs review (8 required changes)

1. **[HIGH]** array→`Iarr` (portable, not OxCaml `iarray`, per ADR-0002); module
   sig added; ergonomic cost + future swap noted. ✔
2. **[HIGH]** Bool-`Eq` is a connective; precise `is_atom` given; clausifier
   descends; never opaque to EUF. ✔
3. **[MED-HIGH]** Int-`Ite` removal named as a preprocessing pass; pipeline
   invariant in `Debug.check`; construction stays unrestricted. ✔
4. **[MED]** `distinct`/`abs` desugar at construction, `div`/`mod` (nonzero
   constant divisor) via reserved `App` symbols + elimination pass, non-constant
   rejected `Unsupported`; no new nodes. ✔
5. **[MED]** Tag identity ≠ cache key (order-invariant serialization is the cache
   layer's, kept distinct); gcd normalization added to `Le`. ✔
6. **[MED]** `Overflow` raised pre-mutation, caught at session→`unknown`,
   state-safe; native-int coeffs a stated v1 decision with bignum revisit. ✔
7. **[MED]** Single session Context threaded through theories/combination/
   preprocessing; search-driven table growth acknowledged. ✔
8. **[MED]** `equal_node`/`hash` scalar-payload contract mandated; scalar-
   distinctness `Debug.check` + property-test obligation. ✔

**Verification-pass fixes (post-review):**
- **B1 [HIGH, reopened #1]** — removed `unsafe_of_array` from the public `Iarr`
  surface (`(**/**)` was doc-only, still exported + aliasing); type now abstract,
  copying-only `of_list`/`of_array`; optional no-copy cast confined to a dune
  `(private_modules Iarr_unsafe)`, build-enforced invisible to consumers. ✔
- **B2 [LOW]** — `Unsupported` given the same catch contract as `Overflow`
  (raised pre-mutation; session/parse → `unknown`/skip; never crash/partial). ✔
- **N1** — frozen `is_atom` reworded to "`Eq` with Bool-sorted *arguments*"
  (avoids the result-vs-argument `@Bool` ambiguity). ✔
- **N2** — one sentence: gcd tightening is new TCB, checked by pre-labeled
  benchmarks (labels on the original atom) + targeted tests, not the Lean-dump
  path. ✔
