# Vox bigint

An arbitrary-precision signed integer module, in pure OCaml, with a real
executable implementation.

Specifications need mathematical integers. OCaml `int` is modelled as
`Bitvec 63`, so `x >= 0` does not imply `x + 1 >= 0`, and a specification that
wants unbounded arithmetic has nowhere to say so. `Bigint.t` is that place.

This piece is standalone. It depends on nothing else in vox, and nothing else in
vox depends on it until the translation piece maps its operations to the solver.

## No GMP, and not by preference

Zarith is the standard OCaml answer and it is out of reach here.

- Nothing in the oxcaml tree references zarith or gmp: no `.ml`, `dune`,
  `.opam` or `configure.ac` hit.
- Zarith is not in the opam switch, though `libgmp.so` is on the box.
- No `otherlibs` entry links an external library. The only link flags in the
  tree are `-ldopt`, `-linkall` and `-lthreadsnat`; `str` carries its own regex
  C in-tree and `unix` uses libc. A GMP dependency would be the first of its
  kind and would break bootstrap and cross-compilation.
- OCaml's own `Num` was removed from the distribution in 4.06, so the project
  has deliberately stopped shipping a bignum.

Performance is not the reason to want GMP here anyway. The solver never runs
this code: `Vox_builtin` maps `Bigint.add` by path to SMT `Int` addition, so
proofs are about mathematical semantics regardless of the implementation. The
implementation runs only where a specification is executed.

## It lives in core stdlib

Not `otherlibs/stdlib_alpha`, which is otherwise the right home for something
experimental.

The reason is runtime checking of specifications, which is planned. Runtime
checks are code the compiler emits into a user's module. If the support lives in
`stdlib_alpha`, compiler-generated code carries a link dependency the user never
asked for. This is the same reason `CamlinternalFormat` and `CamlinternalLazy`
are in stdlib rather than beside them.

It is public rather than `Camlinternal`-prefixed, because specification authors
write `Bigint.t` in source.

The cost is real: this joins the public stdlib API and touches `StdlibModules`,
`.depend`, `dune`, `stdlib.ml` and `stdlib.mli`. It also permanently claims the
name `Bigint` in the default-opened namespace, which any third-party code with
its own `bigint.ml` will feel. Worth naming so nobody thinks it was chosen for
convenience.

## The API is determined by the solver

Every exported operation has to be interpretable as an SMT `Int` operation.
Anything else becomes an uninterpreted function, and specifications that use it
silently stop being provable.

    zero one of_int
    is_zero equal compare lt le gt ge
    neg abs add sub mul

Note the absences. No division or modulo, which bring partiality on a zero
divisor and nonlinear reasoning. No exponentiation. vox2 leaves these out too,
and its `int` division needed a separate constant-divisor restriction on the
oxsmt path, which is the shape of the trouble.

Adding a convenient function later is a way to create unprovable specifications
without noticing, so the rule belongs at the top of the `.mli`, not in a commit
message.

Three functions are runtime-only conveniences with no interpretation, and should
be marked as not for use in specifications:

    to_int_opt to_string of_string

## Representation

Two constraints. The kind must be `immutable_data`, so the type crosses modes
and can be used at `@ logical`. And the representation must be canonical.

Sign-magnitude over `int` limbs. Two candidates for the magnitude:

- `int list`, which vox2 uses. Simple and obviously correct.
- `int iarray`, which is the more idiomatic immutable sequence in OxCaml and
  better on allocation and locality.

Runtime checking makes the second more attractive than it would otherwise be,
since checks run in real programs rather than only in spec tests. Either
satisfies the kind requirement, so this is the implementer's call against how
much the arithmetic code suffers.

Decision: `int iarray`. The arithmetic builds each magnitude in a mutable
scratch array and freezes it through a single `trim` helper, so the loops are
the textbook carry/borrow loops rather than vox2's structural-recursion
variants, whose canonicity-on-unwind subtlety in `subtract_magnitude` is
exactly the kind of cleverness this module should not need. Limbs are
half-word (`radix_bits = (Sys.int_size - 1) / 2`) so limb products with their
carries fit in `int`.

## Canonicity is an invariant, and it is load-bearing

No leading zero limbs, and zero has sign 0 with an empty magnitude. Canonical
representation means polymorphic equality agrees with `equal`.

Polymorphic *compare* does not agree with mathematical order, because it
compares the representation. Document that in the `.mli`; it is the kind of
thing that silently produces a wrong sort order years later.

## The oracle is not optional

The solver reasons about mathematical integers. Runtime checks run this code. If
the two disagree, a guarded `assume` can pass at runtime while the obligation it
was standing in for concerned different numbers. That is a soundness gap in the
`assume` mechanism, so the implementation's correctness matters even though its
speed does not.

Test against an independent oracle rather than against itself:

- machine `int` arithmetic for values in range, which covers carries, signs and
  the boundaries cheaply
- a deliberately naive decimal-string implementation for large values, written
  for obviousness rather than speed
- algebraic properties: commutativity, associativity, distributivity, `sub` as
  `add` of `neg`, `abs` of `neg`

## Tests

- the canonicity invariant holds after every operation, including the ones that
  can produce zero: `sub` of equals, `mul` by zero, `neg` of zero
- oracle agreement on the three fronts above
- `to_string` and `of_string` round-trip, and `of_string` rejects redundant
  leading zeroes and bare `-`
- `to_int_opt` at `min_int` and `max_int`, and one past each
- comparison is a total order consistent with `equal`
- polymorphic equality agrees with `equal`, which is the canonicity invariant
  observed from outside

## Deferred

The mode annotations. vox2's signature is
`val add : t @ logical -> t @ logical -> t @@ total` throughout, which needs the
totality piece. Land the module unannotated and add them after.
(Investigated 2026-08-21; findings and the proposed route in the dated
section at the end of this file.)

The `Vox_builtin` mapping from these operations to SMT `Int`, which needs the
solver interface and the translation piece.

Runtime checking of specifications, which is what makes the executable
implementation earn its keep, and which is a piece of its own.

## 2026-08-21: the deferred mode annotations — findings and proposed route ⚑

Investigated for the gap-refresh shortlist (item 2a: without annotations,
the predicate judgment's Total-callee rule makes every Bigint mention in a
specification a formation error). Three facts, each pinned empirically:

**Placement.** `total`/`logical` are mode names introduced by the totality
piece (`typing/typemode.ml`), two stack slots above this one. On this
branch, `val f : int -> int @@ total` fails with "Unrecognized modality
total", so the annotations cannot land on `jujacobs/vox/bigint`; the
earliest host is the restacked chain at the totality slot (verified at
in-chain tip dbabe4c8e0, where the same interface compiles). They are their
own small piece off the post-Wave-B chain — which is where the roadmap
already lists `bigint-modes/`.

**Checking.** Annotating the interface is not enough. With `@@ total` on the
thirteen operations and the implementation untouched, stdlib inclusion
rejects the claims at the first one — "`val of_int : int -> t` is not
included in `val of_int : int -> t @@ total` ... The first is partial
because it closes over the value (=) ... which is partial" — and the same
holds for every operation: this module's ratified iarray/scratch-loop
representation (see Representation) uses loops, array primitives and
comparisons, all partiality sources under the totality piece's rules. vox2's
identical annotations typecheck only because its list-based
structural-recursion implementation passes vox2's structural termination
check — machinery vox defers to the termination-structural piece, which the
roadmap sequences AFTER bigint-modes. So at the point bigint-modes lands,
the totality of these operations is necessarily claimed, not earned.

**Values vs operations.** `t : immutable_data` crosses both axes, so Bigint
VALUES are already admissible at `total`/`logical` (pinned by fixture); only
the operations' totality is missing. vox2's `t @ logical` argument
annotations are vacuous under crossing and are dropped
(DIVERGENT-BY-DESIGN: same admissibility, fewer annotations; the crossing
is pinned by a fixture instead).

**Proposed route (⚑ owner decision — trust is involved).** Keep the
implementation as ratified, and claim totality at the export boundary with
a module-local trusted cast, the `Obj.magic_portable` idiom:

    external magic_total : 'a -> 'a @ total = "%identity"
    let (add @ total) = magic_total add     (* one per exported operation *)

with `@@ total` on the same thirteen operations vox2 annotates (`of_int`,
`is_zero`, `equal`, `compare`, `lt`, `le`, `gt`, `ge`, `neg`, `abs`, `add`,
`sub`, `mul`) and the three runtime-only conversions left partial — a
fixture pins that scoping (`of_string` in a total closure stays rejected).
The evidence for the claims is the oracle suite ("The oracle is not
optional" above) plus inspection: every loop is bounded by a magnitude
length and mutation touches only fresh scratch. vox2 itself trusts external
modalities the same way (its local `external int_equal : int -> int -> bool
@@ total = "%equal"`); the difference is the trust boundary sitting around
whole operations instead of monomorphised primitives. When
termination-structural lands, the claims can be earned instead (requires
reversing the iarray decision toward vox2's list recursion) or the casts
kept; that is a later owner call. Verified end to end at dbabe4c8e0: with
the casts and annotations, total closures may call all thirteen operations,
and the runtime-only conversions remain rejected.

Alternatives considered: a compiler-side path allowlist in `type_ident`
(where the totality primitive allowlist and the vc predicate-scoped
comparisons already sit) — same trust, but invisible in the interface and
path-matching is fragile; predicate-scoped admission at the vc level (the
comparisons precedent) — unlocks specifications only, leaves user total
closures unable to call Bigint, and leaves the interface silent. Both
rejected in favour of annotations users can read.

The lowering half of shortlist item 2 (guarded `/`/`mod` rows, Bigint
operator rows, int↔bv conversions in `vox_lower`) is unchanged by this and
lands on the vc branch.
