# Vset_make — the ORD-functor set (supporting cast)

`Set.Make(ORD)` for int-representable ordered elements, productized directly
from `testsuite/tests/vox/mechanics/lean_functor_bst.ml`. Model: characteristic
function `ISet := Int -> Prop` (funext-equality IS set equality → STRUCTURAL
specs `_ = ins x s`). Ordered BST CARRYING the `bst` invariant (the model
`tmem` searches the whole tree, so impl-search = model needs `bst` + the
`not_mem_lt/gt` order lemmas — contrast the map, whose ordered-navigation model
needs no invariant; see notes/vmap_make.md). Verified green: `.mli` + `.ml`
seal + `clients/smoke_vset_make.ml` (cross-unit) + the DISPROVED bad instance.

Ops shipped: `empty` / `add` / `mem` / `singleton`.
Laws shipped (all LIVE): `mem_s_ins`, `mem_s_ins_ne`, `mem_s_empty` (proven
load-bearing by deletion — ins→line 26, ne→line 38, empty→line 31).

## The model trade vs the flat Vset (the one design decision)

Flat `Vset` uses an INDUCTIVE-LIST model with MEMBERSHIP-agreement specs
(`vs_addspec := ∀y, vs_mem y r = ...`) because a list's structural `=` is not
set equality (many list images of one set). `Vset_make` uses a CHARACTERISTIC
FUNCTION, whose funext-equality IS set equality, so it gets STRONGER STRUCTURAL
specs (`_ = ins x s`) for free.

The price, and why it is a real trade: a char-function has NO support size, so
`cardinal` is NOT EXPRESSIBLE here — flat `Vset` ships `cardinal`/`fold`/
`elements`, `Vset_make` cannot without switching to the inductive-sorted-list
model (which a future ordered `Vset` unification would pick precisely to regain
cardinality). This is the crisp "what the char-function functor set gives up".
`union`/`inter`/`diff` ARE expressible as char-function combinators
(`fun y => a y ∨ b y` etc.) but the IMPL must fold one set's elements into the
other via `add` (a tree traversal + a fold-preservation proof) — deferred with
the rest of the enumeration-dependent surface.

Frictions are shared with the map and recorded once in notes/vmap_make.md
(top-level block home / `abbrev` sort / grind-no-beta → opaque ops + laws /
Prop-equality law needs `simp only [...]; grind` not `simp only` alone /
cross-unit + DISPROVED bad instance). Set-specific: the `bst`-carrying model
needs the `not_mem_lt`/`not_mem_gt` completeness lemmas and `bst_tins` +
`all_lt_tins`/`all_gt_tins` invariant-preservation lemmas (all `induction t <;>
grind`), which the map's ordered-navigation model does without.
