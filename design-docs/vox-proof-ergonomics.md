# Proof ergonomics

These changes support explicit witnesses and pointwise proof functions for
mutable graph verification. They add no solver quantifiers, axioms, or trusted
primitives.

## Recursive functions

Interactive recursive bindings retain the provisional recursive binding's
totality, statefulness, and portability while keeping the other top-level
mode restrictions. The existing descent checks still discharge total recursion.

For a refined result annotation and explicitly typed, unlabelled variable
parameters, recursive type approximation constructs the dependent function
signature before checking the body. This puts parameter names in scope in the
result predicate. Other parameter patterns can use an explicit function type.

## Function refinement

An expected function type implicitly adapts a stable local function `f`
into a checked eta wrapper. The wrapper applies `f`, opens its result refinement,
and proves the expected result refinement. Expected parameter refinements are
available while checking each pointwise obligation. Captured equal heap
observations can therefore transport a model callback to another observation.

The ordinary function checker enforces totality, ghostliness, ownership, and
capture modes. Refinement strengthening needs a proof; incompatible argument
types remain errors. Unlabelled parameters are supported. Labelled and optional
parameters require an explicit wrapper. A real wrapper is an ordinary runtime
closure; a wrapper in ghost code erases. Curried applications are wrapped one
argument at a time, preserving effects at each partial application.

For callbacks, `(f : (int -> int) @ total)` annotates the function value.
`(f : int -> int @ total)` annotates the result value and does not itself
require that calls terminate.

## Dependent arguments

Dependent application accepts local stable variables, literals, and immutable
boxed or unboxed field projections rooted in local stable variables. A checked
logical expression substitutes for the dependent binder in the result type.
The runtime argument stays in place and is evaluated once in the usual order.
No temporary variable escapes in the result type.

Mutable projections and effectful expressions need an explicit `let` snapshot.
This restriction ensures that a later refinement refers to the same observation
as the actual argument. Field disambiguation and mode checks use ordinary
expression typing; predicate conversion checks the supported logical fragment.

## Unboxed ghost fields

An unboxed record field may carry `@@ ghost` directly. Its runtime slot has
layout `void`; its logical payload retains its own type and mode checks. Pattern
bindings and projections use erased placeholders. Real construction and record
operand effects still execute, while `ghost_` computations erase. Updates,
singleton products, all-ghost products, and payloads with non-value layouts use
the same rule. Native code removes the void slots; bytecode can retain void
placeholders in its product representation. Ghost payloads contribute no kind
bounds, matching boxed records.

Block indices cannot point to ghost fields, including nested unboxed fields,
because those fields have no runtime storage.

Legacy boxed records with `[@@unboxed]` and inline variant fields retain their
existing restriction against `@@ ghost`.

## Totality audit

The old negative-datatype warning was stale. Total pattern matching and field
projection already require the checked datatype guarantee. Compile-only
regressions cover negative variants, recursive records, aliases, and attempted
false-refinement construction; ordinary partial code remains legal. This is a
regression audit, not a mechanized soundness proof of Vox.
