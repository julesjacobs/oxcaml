# Principal inference for closed STLC terms

This stage supports variables, a Boolean constant, lambda abstraction,
monomorphic recursive lambdas and application.
The public `Stlc_infer.infer` API accepts a closed term with an explicit
`scoped_term Z e` refinement. Variables use de Bruijn indices. A caller handling
raw syntax can test this executable predicate before calling inference.

## Runtime algorithm

`stlc_generate.ml` traverses syntax with an environment of shared Pref handles.
A variable returns its environment handle. A Boolean allocates a Boolean node.
A lambda allocates a fresh parameter variable, generates its body, and allocates
an arrow from the parameter to the body's result. An application generates both
children, allocates a fresh result variable and the required argument/result
arrow, and records equality of that arrow with the function's type.

A recursive lambda allocates an argument variable, a reserved result variable
and their arrow before generating the body. The body environment has the
argument at index 0, the recursive function at index 1, then the enclosing
environment. Every recursive use shares the same arrow. Its final constraint
equates the body's type with the reserved result variable. The returned root is
the preallocated arrow; no recursive type or polymorphic recursion is introduced.

`stlc_solve.ml` processes the generated equalities with the existing mutable
unifier. A binary equation tree gives constant-time concatenation during
generation. Solving visits the left subtree before the right and stops on
failure. It returns ownership of the resulting heap, including earlier writes.
`stlc_infer.ml` starts with an empty token and returns a graph root, success flag
and successor token, together with erased execution and finite-readback witnesses.

This is a two-phase STLC inferencer. The equation tree is real runtime data;
the annotated syntax graph, heap snapshots and proof functions are ghost data.
No substitution table or additional unification cache is introduced. The later
HM stage must solve at let-generalization boundaries rather than defer all
constraints to the end of the term.

## Specification and exported guarantees

`stlc_spec.ml` defines the syntax, contexts and the independent declarative
`typed` predicate. Its witnesses have the ordinary variable, constant,
abstraction, application and monomorphic recursive-lambda rules. The remaining predicates specify generation
(`built`), solving (`solved`) and their composition (`inferred`). The returned
root is tied to the actual execution witness and finite unfolding tree.

For a returned execution, with `P = readback result.tree`:

- `inference_sound` constructs a derivation of `No_types |- e : P` on success.
- `with_typing_factor` takes any derivation of `No_types |- e : T` and constructs
  a total substitution `delta` with `T = substitute delta P`. The substitution
  and equation witness are passed to an explicit total continuation.
- `inference_rejects` derives false from any putative typing derivation when the
  execution returned failure.

Together the first two establish that the returned readback is a principal
type. The third proves correct rejection. These apply to returned executions;
runtime termination and resource failures remain outside the contract. The
public principality theorem is for closed terms. Generation, model extension
and solving have environment-parametric contracts, but an open-term principal
pair API is not exported by this stage.

## Proof ladder

`stlc_graph_proofs.ml` proves allocation-domain and contents framing, constraint
allocation, finite-unfolding preservation, and generation soundness. Soundness
uses a model of the generated heap satisfying its equalities to construct the
declarative typing derivation.

`stlc_model_proofs.ml` proves that a fresh allocation extends an arbitrary model
without changing assignments on old handles. It also transports contexts and
constraint satisfaction through pointwise agreement. `stlc_complete_proofs.ml`
uses these facts recursively: every declarative typing extends an old model to
a model of the generated graph satisfying its constraints. Fresh node values
are constructed from the supplied typing; no model or invariant constructor is
assumed.

The recursive-lambda case extends a supplied model three times, assigning the
argument and result types supplied by the declarative derivation and their
arrow to the self handle. Body completeness preserves those assignments and
constructs its result at the reserved type, establishing the final equality.
Soundness uses the same shared arrow equation and body/result equality. The
public `inference_sound`, `with_typing_factor` and `inference_rejects` statements
therefore cover recursive lambdas without changes.

`stlc_solve_proofs.ml` composes the unifier's exact model and rejection theorems
through the equation tree. It also preserves finite unfoldings on failure.
`stlc_inference_proofs.ml` combines these with canonical readback. Factorization
uses `readback_factor` from the MGU layer on the model constructed from an
arbitrary typing derivation. Thus both the executable constraint generation and
the mutable solving stage participate in the principality proof.

All universal facts are explicit total functions and all existential facts are
explicit witnesses or continuations. The SMT fragment remains quantifier-free.
No compiler change, new trusted primitive or assumed correctness theorem is
introduced.

## Checks and next stages

`stlc_demo.ml` covers identity, nested binders, shared environments, higher-order
application, Boolean application, self-application rejection and constructor
clashes. It invokes soundness on successful executions and constructs a
factorization witness for the Boolean instance of identity. The returned token
is used to read the actual result root on success and failure.

`stlc_rejected.ml` rejects a wrong de Bruijn lookup, an omitted application
equality, a Boolean used as a function, an invalid identity typing, a skipped
solver and an unbound variable passed to the closed-term API. Both test files
run with bytecode and native compilation. Lambda inspection checks that proof
calls and witness construction erase while the runtime equation tree remains.

Recursive tests cover the argument, self-calls, application, captured enclosing
binders, self-return occurs failure, argument self-application, and incompatible
recursive uses. A recursive identity has an explicit Boolean-instance typing
that consumes the factorization contract. Negative proof tests additionally
reject omitting the body/result equality, using the self binder as the argument,
a non-arrow recursive type and an out-of-scope recursive-body index.

Mutable levels, generic templates and let-polymorphism remain subsequent stages.
A completed recursive lambda can later be generalized by an enclosing let;
recursive uses inside its own body remain monomorphic.


## Independent declarative HM layer

`hm_declarative.ml` gives let-polymorphic typing its own specification. Bound
scheme parameters use de Bruijn indices; free type names use handles without
reading their cells. A scheme binds its first `k` indices. Generalizing a RHS
adds `k` ambient parameters and weakens the outer context, preserving every
scheme's own binders. Recursive self and argument bindings have arity zero.

`hm_type_proofs.ml` proves opening/evaluation, weakening/meaning, well-formedness
preservation, embedding and term scoping. Universal facts are total functions;
finite type arguments supply explicit instance witnesses. The positive fixture
constructs an independent `id id` derivation and a monomorphic recursive call.
Rejection probes check argument count, bound-parameter capture and polymorphic
recursive self.

This layer does not yet connect declarative let typing to a mutable execution.
The remaining bridge must construct environment instance translators from RHS
model-extension proofs; the checklist records that obligation separately.


`hm_environment_spec.ml` relates runtime handle lists to erased template lists.
It protects generic descriptors and finite boundaries below a binding's depth.
`hm_environment_proofs.ml` transports those facts through allocation, copying,
unification, memo cleanup and closing at a sufficient cutoff. It constructs
instance translators for empty and monomorphic environments, lifts them under
fresh type binders, and transports them across models that agree on the template
boundaries. The let translator remains part of the RHS model-extension bridge.


`hm_substitution.ml` substitutes free handle names with embedded semantic types
and transforms complete typing derivations. `hm_substitution_proofs.ml` proves
that typing, scheme opening and context weakening commute with that operation.
Indexed scheme parameters remain unchanged. This is the specialization step
needed after syntactic abstraction in the HM soundness proof; abstraction is
still a separate obligation.


`hm_abstraction.ml` replaces selected free names by indexed parameters, preserving
repeated occurrences and shifting existing parameters beneath every nested
scheme binder. `abstraction_typed` transforms the complete typing judgment.
`generalize_typing` additionally requires the selected names to be absent from
the context, then identifies the transformed context with ordinary weakening.
The fixture composes abstraction with arbitrary free-name substitution, including
a substitution that reuses the original handle name. The heap/path argument that
establishes this absence for graph-selected parameters remains in the HM bridge.


`hm_execution_spec.ml` records interleaved richer-node inference operations.
Variable steps use the cleaned copy heap; applications record both child runs
and the actual unifier derivation. Let steps run the RHS one level deeper,
close its pool, transfer retained finite nodes, then run the body. Separate
failure constructors preserve the executed prefix and return no result handle.
`hm_execution_proofs.ml` proves heap extension and ownership of successful
results. The fixture executes allocation, close/transfer and clean copying,
and checks that concrete run against the witness. The recursive inference
driver is now implemented for let-free terms; the semantic typing bridge
remains unfinished.


`hm_infer.ml` runs clean instantiation, pooled allocation and level unification
in syntax order, including monomorphic recursive lambdas and early failures.
Its closed entry constructs the initial evidence from an empty heap.
`hm_runtime_proofs.ml` preserves ownership, level bounds, pool coverage and
cleared visited marks through every let-free execution. These are structural
guarantees; the separate semantic proofs below establish soundness.
Completeness remains open.
The positive fixture runs constants, identity, application and recursion, and
rejects self-application, recursive self-return and Boolean application.
The negative fixture checks scoping, the let-free restriction and missing heap
safety evidence.


`hm_forest_proofs.ml` constructs finite unfolding forests through every
execution constructor, including nested lets and failures.
`hm_model_proofs.ml` restricts any model of the final heap to the initial heap;
this also covers failed unification after partial writes.
`hm_sound_proofs.ml` constructs independent declarative typing witnesses for
successful let-free executions under monomorphic environments. `closed_sound`
specializes this to a finite readback of a closed result. These proofs do not
yet establish model extension, principality, rejection or let-polymorphic
soundness.


`hm_complete_proofs.ml` constructs model extensions for every let-free
execution constructor from an independent declarative typing witness.
Allocation assigns fresh roots their required types, copying preserves the
incoming assignment, and a compatible unification cannot fail.
`with_closed_model` supplies the final model for any declarative closed-term
type, `closed_completes` rules out failure, `closed_reject` refutes a proposed
typing after actual failure, and `closed_factor` supplies a substitution from
finite readback to any proposed declarative type. Together with `closed_sound`,
this establishes closed let-free principality, including recursive lambdas.
The identity fixture exercises arbitrary argument types. Let-polymorphic
environments remain the next semantic extension.


`Hm_infer.closed_hm` accepts all scoped terms and executes lets at a deeper
level, closes their child pools and transfers retained finite nodes before
inferring the body. The original `closed` entry retains its let-free contract
and semantic theorems. `hm_protected_proofs.ml` preserves saved low bounds and
generic descriptors, while `hm_registration_proofs.ml` traces unlisted finite
nodes back to the input heap. `hm_let_runtime_proofs.ml` combines these facts
into nested-level and pool preservation. Failure preserves heap safety; a
failed RHS can retain its deeper level because no continuation uses it.

Level increment uses a runtime overflow guard that raises `Assert_failure`;
it never reports overflow as a typing rejection. Theorems concern returned
executions, so runtime termination and resource exceptions remain outside
scope. The nested-let, shared-boundary and recursive-let fixtures check the
actual driver; full let-polymorphic semantic theorems remain open.

`hm_origin_proofs.run_origin` preserves explicit saved-root paths through every
execution constructor, including nested lets, copy cleanup and failure prefixes.
Its saved cutoff must be strictly below the active inference depth: allocations
at the active depth need not originate in the saved heap. `rhs_origin` constructs
the initial paths when entering a let RHS. `rhs_interpret` then derives relative
interpretation for two models of that RHS heap that agree on the saved low
boundary. These lemmas do not yet construct the two models from declarative
polymorphic typings.

`hm_freshness_proofs` constructs generalized names from high free leaves of a
finite RHS readback. Level order keeps low-boundary readbacks below the cutoff,
and RHS activity separates the selected names from existing generic template
parameters. Reifying templates and abstracting their generic parameters therefore
produces a canonical declarative environment avoiding the selected names.
`generalize_readback_typing` supplies this freshness premise to `generalize_typing`.
A rejection fixture demonstrates why arbitrary free-name substitution must come
after this abstraction: a low boundary assignment can contain a selected name.
The name lists may repeat shared leaves; abstraction uses their first position,
leaving unused binders. This ghost representation does not change copying.
The full inference soundness induction and template-instance correspondence are
still separate obligations.

`hm_one_let_proofs.with_id_id_model` constructs a model of the final heap of
any actual `let id = fun x -> x in id id` execution with result `B -> B`, for
arbitrary finite semantic `B`. The baseline model comes from the finite forest.
For each copy, the let-free completeness theorem constructs an alternative model
of the same RHS execution; `rhs_interpret` supplies its instance choices. The
second copy starts from the first copy's heap and preserves its assigned type.
The application allocations extend that model, so unification cannot fail.
`id_id_completes` excludes failure and `id_id_factor` constructs a substitution
from final readback to `B -> B`. The runtime fixture calls `closed_hm` directly.
This is the concrete one-let bridge, not the general HM completeness theorem.
