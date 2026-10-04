title: Hindley–Milner type inference
blurb: Type inference for a small ML with let-polymorphism, proved sound and principal against a declarative typing relation, so a rejection means the term has no type.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/hm_inference.mli — Public interface
  - testsuite/tests/vox/hm_declarative.ml — Terms, types and the declarative typing relation
  - testsuite/tests/vox/hm_inference_model.ml — Inferred types and substitution, used to state principality
  - testsuite/tests/vox/hmc_word64.ml — 64-bit word constants
  - verification/library/pref.mli — Reference identities used as type variables
  - testsuite/tests/vox/copy_spec.ml — Shared inferred-type representation
  - testsuite/tests/vox/hm_inference.ml — The interface implemented over `Verified_hm`
  - testsuite/tests/vox/verified_hm.mli — Internal interface: inference with its evidence, and elaboration
  - testsuite/tests/vox/hm_routed_infer.ml — The inference algorithm
  - testsuite/tests/vox/hm_inference_public_client.ml — Public-only client
  - testsuite/tests/vox/hm_inference_rejected.ml — Check that the result's evidence cannot be read
  - testsuite/tests/vox/verified_hm_rejected.ml — Rejected clients of `Verified_hm`
---
`Hm_inference.infer` infers the type of a closed term of a small ML: booleans, 64-bit words with addition, subtraction, equality and unsigned comparison, lists with a case form, conditionals, functions, recursive functions, application and `let`. Variables are de Bruijn indices, and `let` generalizes as in Hindley–Milner. Three theorems relate the result to a declarative typing relation, `Hm_declarative.typed`. If `infer` returns a type, the term has that type (`sound`). If the term has any type, `infer` returns a type and the given type is an instance of it (`principal`). Hence, if `infer` returns `None`, the term has no type (`rejected`).

The implementation uses mutable type nodes with union-find links, levels
for generalization and copying for instantiation. These appear in its proofs;
the public guarantees use pure types, substitution and declarative typing.
Only normal return is specified: `infer` may raise or fail to terminate, and
there is no time or memory bound.

## Interface

Read `hm_inference.mli` for the operations and laws, `hm_inference_model.ml`
for inferred types and substitution, and `hm_declarative.ml` for source terms,
scoping and typing. These definitions determine the three guarantees;
`hm_inference.ml` and the inference graph modules contain their proofs.

@code testsuite/tests/vox/hm_inference.mli

`result` is abstract. `source` gives the term that was inferred, as an erased value (`@ ghost`); `inferred_type` gives the answer. A function marked `@@ total` terminates without raising and may be used in refinements. `sound` returns the typing derivation itself, erased. `principal` states an existential in continuation-passing form, because refinements have no existential quantifier: given a typing of the source at `target`, any `claim` that follows from "`inferred_type out` is `Some ty` and `target` is `substitute delta ty`" for an arbitrary substitution `delta` holds. Read directly, inference returns a type and `target` is an instance of it. `rejected` is the case `None`. Because `typed D.Z` requires a type without parameters, every type of a closed term is `embed` of some `Hm_inference_model.ty`, so the two theorems cover all of them.

The declarative system. `typed n g e t d` holds when `d` is a derivation that term `e` has type `t` in context `g`, with `n` type parameters in scope. A scheme `Forall (k, t)` binds `k` parameters, which `Variable args` instantiates. `Let_binding` generalizes over the parameters it introduces; `Abstraction` and `Recursion` bind monomorphic types. `embed` turns an inferred type into a declarative one, with each type variable as a free type constant `Free p`.

@code testsuite/tests/vox/hm_declarative.ml "type index =" "| Word_primitive of typing * typing [@@inductive]"

The typing equation is the familiar structural relation, including the
`let` rule that generalizes the bound expression:

@code testsuite/tests/vox/hm_declarative.ml "let[@def] rec (typed @ total)" "| _ -> false))"

The complete [declarative source](src:testsuite/tests/vox/hm_declarative.ml)
also defines scoping, lookup, substitution of scheme parameters, and the
well-formedness checks used in this equation.

Inferred types use variable identities. `Variable p` and declarative
`Free p` denote the same free type constant; `substitute` replaces it by
`delta p`. Identity equality determines whether two occurrences denote the
same constant. No node contents, links, levels or ownership enter typing
or substitution. The implementation stores these identities as references;
that representation is outside this semantic reading order.

@code testsuite/tests/vox/hm_inference_model.ml

[Word constants](src:testsuite/tests/vox/hmc_word64.ml) are pairs
of 32-bit limbs. Their arithmetic operations do not enter the typing rules.

## Trusted base

- `verification/library/vox_iarray.mli` declares `get`, `set`, `sub` and `extensional` as `external` with assumed contracts. The inference's per-level node pools use `Vox_iarray.updated`, which calls `set`, and lemmas about it stated through `get`.
- `hm_routed_infer.ml` declares `raise_any`, a layout-polymorphic `external` for `%raise`, to raise the level-capacity `Failure`.

## Scope

- Terms are closed: there is no initial environment or prelude. Types are `bool`, 64-bit words, lists and functions. There are no user-defined types, records, references, type annotations or pattern matching beyond the list case.
- `Recursive` binds one recursive function whose type is monomorphic inside its body; `let` is the only place where types are generalized. The language has no effects, so there is no value restriction.
- Indices and scheme arities are unary naturals (`Z`, `S`).
- Only normal return: `infer` may fail to terminate and may raise `Failure` (counter overflow), `Out_of_memory` or `Stack_overflow`.
- `Hm_inference` reports only the type. The internal `Verified_hm.elaborate` also rebuilds a typed derivation from an inference trace; the [HM-to-WebAssembly compiler](hm-wasm-compiler.html) uses it.
- The inference and its proofs are 212 files and about 29,000 lines in `testsuite/tests/vox`. The tests also compile 25 files (about 3,000 lines) that `Hm_inference` does not use: earlier versions of the unifier, copy and lowering, and fixtures for other tests.

## Client example

From the public-only client. `ghost_ (...)` is proof code, checked and then erased. Each `f_def x` call unfolds the definition of `f` at `x` for the solver; here they show that `λx. x` is a closed term, the precondition of `infer`. `identity_typing ()`, defined above in the file, builds the derivation that `λx. x` has type `bool → bool`. `===` is logical equality and `{v : t | p}` is `t` refined by `p`. `principal` is used with the claim that the inferred type is not `None`; its continuation `use` receives a substitution and the fact that the inferred type is `Some`, from which the claim follows. The refinement on `ty` then records statically that inference succeeded. `sound` returns the erased derivation for the inferred type; its last argument, `()`, stands for the proof that `inferred_type out === Some ty`, which the checker discharges. The last lines check at run time that `λx. x x` is rejected.

@code testsuite/tests/vox/hm_inference_public_client.ml "let source = D.Lambda (D.Bound D.Z) in" "self-application accepted"

The same client checks at run time that `infer` returns `word list` for a one-element list of words, and `word` for `let id = λx. x in let _ = id true in id w`, which needs `id` to be polymorphic.

## A rejected program

`Verified_hm` is the module behind `Hm_inference`. Its `infer` returns an unboxed record (`#{...}`, whose fields are read with `r.#root`); `root` is the node of the inferred type, or `None` when inference fails. This client claims that inference always fails. The checker rejects it, because `infer`'s contract allows `Some`:

@code testsuite/tests/vox/verified_hm_rejected.ml "let steal" "V.infer input;;"

```
Line 4, characters 2-15:
4 |   V.infer input;;
      ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 20-36:
3 |     {r : V.answer | r.#root === None} @ unique = fun input ->
                        ^^^^^^^^^^^^^^^^
  The refinement is stated here.
```

The same test rejects a forged evidence value and uses of `principal` and `elaborate` with an unrelated input. `hm_inference_rejected.ml` checks that the evidence inside `Hm_inference.result` cannot be read: `out.evidence` is an unbound field.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/hm_inference_public_client.ml vox/hm_inference_rejected.ml vox/verified_hm_rejected.ml
```

The tests list the inference (244 to 247 files, library files included) as prebuilt modules, which `./dev test` compiles, and so checks, once per run with both compilers before compiling the tests against them. The public client runs on both backends; the two rejection tests are expect tests.
