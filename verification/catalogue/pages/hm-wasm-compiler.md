title: HM-to-WebAssembly compiler
blurb: A compiler from the Hindley–Milner language to WebAssembly bytes, proved against a WebAssembly model to emit valid modules that cannot trap and return only the source program's result; it rejects a closed program of the stated shape that has a type word → word only for reasons that concern the layout and the compiled program, which are not characterized.
status: in-progress
date: 27 September 2026
sources:
  - testsuite/tests/vox/hmc_compilation.mli — Public interface
  - testsuite/tests/vox/hmc_compilation_model.ml — Layout validity, results, the resource premise and honest exhaustion
  - testsuite/tests/vox/hmc_layout.ml — The memory layout
  - testsuite/tests/vox/hmc_source_semantics.ml — Source semantics: an environment machine
  - testsuite/tests/vox/hm_interpreter_typing.ml — Source values
  - testsuite/tests/vox/hm_declarative.ml — Source terms
  - testsuite/tests/vox/hmc_failed_guard_model.ml — The heap and stack guard instructions
  - testsuite/tests/vox/wasm_binary_execution.ml — WebAssembly model: decode, instantiate and run a module
  - testsuite/tests/vox/wasm_calls.ml — WebAssembly model: execution with calls
  - testsuite/tests/vox/wasm_instruction.ml — WebAssembly model: the instruction subset and its encoding
  - testsuite/tests/vox/wasm_static_module.ml — WebAssembly model: validation
  - testsuite/tests/vox/wasm_differential.ml — Differential test of the WebAssembly model against Node
  - testsuite/tests/vox/hmc_compilation.ml — The interface implemented over the compiler
  - testsuite/tests/vox/hmc_compiler.ml — The compiler pipeline
  - testsuite/tests/vox/hmc_frontend.ml — Frontend: scope check, inference, grounding and admission; the type-error lemmas
  - testsuite/tests/vox/hmc_admission.ml — Admission and the proof that the fragment is admitted
  - testsuite/tests/vox/hmc_compilation_public_client.ml — Public-only client
  - testsuite/tests/vox/hmc_compilation_rejected.ml — Rejected clients
  - testsuite/tests/vox/hmc_wasm_relayout_demo.ml — Runs the compiler on sample programs in the model
---
`Hmc_compilation.compile` takes a closed term of the language of the [Hindley–Milner page](hindley-milner.html), a 64-bit input word, a memory layout and an initial memory image. It returns a rejection or an artifact whose `bytes` are a WebAssembly module. The module computes the term applied to the input, which is fixed at compile time. The theorems are stated against a model of WebAssembly that is part of the demo: the 43 `wasm_*.ml` files, 2,572 lines, on which the theorem statements depend. For every artifact:

- `static_validity`: the bytes pass the model's validation.
- `safe`: at every step count, running the bytes has decoded and instantiated the module and has neither trapped, failed a type check, exceeded the host call depth nor reached an instruction outside the model.
- `reflection`: if the module finishes returning a word, the source program, run by the source machine, returns the same word.
- `preservation`: if the source program returns a word, the module finishes, either returning that word or reporting heap or stack exhaustion.
- `normal`: if in addition the layout meets the resource premise `sufficient`, the module returns the word.
- `exhaustion`: a reported exhaustion happened at one of the emitted memory guards, when the requested space exceeded the space left.

`compile` also says why it rejects a program. A rejection carries a `reason` and the rejected `program`. `Unbound_variable` means the term is not closed. `Non_callable_outer_binding` and `Non_callable_entry` mean that a top-level `let` binds, or the program ends in, something other than a `fun` or recursive function (`M.outer_callable`, `M.entry_callable`). `Unsupported_polymorphic_local_let` means the program has a `let` inside a binding or the entry (`M.no_local_let` fails), and `Invalid_annotation` never happens. The lemmas `untypable` and `no_entry_type` show that `Type_error` and `Entry_type_mismatch` reject only terms with no type at all, or no type `word → word`; they are the [Hindley–Milner page](hindley-milner.html)'s `rejected` theorem carried through the compiler. So a client can prove that a closed term of this shape with a typing at `word → word` is either compiled or rejected for one of the three reasons that concern the layout and the compiled program.

The compiler infers types with the verified [Hindley–Milner inference](hindley-milner.html), rebuilds a typed derivation, specializes polymorphic functions, converts closures, builds a control-flow graph, marks tail calls and lowers to WebAssembly. Every stage is proved in the same composition and connected to the theorems above; no intermediate invariant is assumed. The stages' own contracts are internal and are not part of the interface.

Not proved: when `compile` rejects for `Layout_rejected`, `Initialization_exhausted` or `Encoding_rejected`. These depend on the layout, memory image and page count and on the compiled program (its largest frame, its closure table, its compiled globals and its encoding), which is not a function of the source in the proofs because the inference is not proved deterministic; so an implementation that rejected every program for one of these reasons would still satisfy the interface. The meaning of `Unsupported_polymorphic_local_let` is weaker than its name: a monomorphic local `let` is admitted and a polymorphic one is not, but whether a local `let` is generalized depends on the inferred derivation, so the interface only says that some local `let` exists. The resource premise of `normal` grows linearly with the number of source steps, whatever the program uses, so `normal` applies only to short runs (see Scope). For a source program that does not return, a finished run is not proved to be either a return or an exhaustion. The WebAssembly model's agreement with the standard and with engines is tested, not proved (see Trusted base), and `compile` is not proved to terminate.

## Client example

From the public-only client, which uses only the public interfaces. `(artifact : C.artifact) ->` names an argument so that later types can mention it. `{u : unit | p}` is an argument that carries only a proof of `p`, and `@ ghost` marks a result that is checked and then erased, like the code inside `ghost_ (...)`. `===` is logical equality, and `M.source_returns_def` unfolds the definition of `source_returns`. The function restates `reflection` in terms of the source machine: if a run of the emitted bytes finishes by returning `word`, the source applied to the input reaches `Done (Word word)` after some number `n` of steps.

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let (normal_returns @ total)" "M.source_returns_def (C.source artifact) (C.input artifact) n word; n)"

The client also combines `static_validity` with `safe`, and restates `preservation`, `normal` and `exhaustion`. The last function derives from the interface alone that the identity `λx. x` can be rejected only for one of those three reasons. It takes a typing of the identity at `Word64 → Word64` from `identity_typed` (a derivation built by unfolding `D.typed`), which refutes both type errors through `untypable` and `no_entry_type`, and unfolds `D.scoped_term` and the shape predicates to refute the other source reasons. It cannot conclude that `compile` succeeds.

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let compile_identity" "    out"


## A rejected program

Claiming a normal return without the resource premise is rejected, because `normal` also requires `M.sufficient`:

@code testsuite/tests/vox/hmc_compilation_rejected.ml "let (without_resources @ total)" "C.normal artifact word fuel ());;"

```
Line 8, characters 72-74:
8 |   fun artifact fuel word premise -> ghost_ (C.normal artifact word fuel ());;
                                                                            ^^
Error: Refinement could not be proved (counterexample)
File "hmc_compilation.mli", lines 83-84, characters 14-66:
  The refinement is stated here.
```

The same test checks that the proof evidence inside an artifact cannot be read: `out.evidence` is an unbound field.

## Interface

@code testsuite/tests/vox/hmc_compilation.mli

`artifact` is abstract; its `source`, `input` and `layout` are erased. `Wasm_binary_execution.run k bytes c` decodes `bytes`, checks the memory and table limits, starts the exported function `run` and runs at most `k` steps, stopping early if it finishes or fails, with host call depth `c`. It returns `Rejected` or the state reached: `Running`, `Finished`, `Trap`, `Type_error`, `Host_limit` or `Not_supported`. `Wasm_static_module.bytes_valid` is validation. The model covers the instructions the compiler emits: 32- and 64-bit constants and integer operations, locals, globals, 32- and 64-bit loads and stores, blocks, loops, conditionals, branches, and direct and indirect calls.

@code testsuite/tests/vox/hmc_compilation_model.ml

@code testsuite/tests/vox/hmc_layout.ml

`returned` requires `run` to have returned 1 and reads the result from the module's globals: status 1, tag 1 and the word as payload. `exhausted` requires `run` to have returned 2 (heap) or 3 (stack). The layout gives addresses in linear memory, a stack capacity in frames and a host call depth (the theorems run with one more than `host_capacity`); `valid_layout` orders the regions and requires the memory image to reach `heap_limit`. The source machine and its values:

@code testsuite/tests/vox/hm_interpreter_typing.ml "type value =" "[@@inductive]"

@code testsuite/tests/vox/hmc_source_semantics.ml

`Hmc_word64` adds and subtracts modulo 2^64.

## Trusted base

- The WebAssembly model: the 43 `wasm_*.ml` files (2,572 lines) that define decoding, validation and execution of the emitted subset. The theorems are about this model; its agreement with the WebAssembly specification and with engines is not proved. It is tested: `wasm_differential.ml` generates modules in the model's subset, variants with one mutation (most of them invalid), modules with a corrupted byte, and valid modules outside the subset, with its own encoder, which shares no code with the model's. For each module it checks that the model and Node agree on validity and, when both run the module, on trap or return, the returned value, every global and every byte of final memory. On 100,000 modules (seed 1, Node 22) there was no disagreement. The model has none of the implementation limits that the JavaScript API sets for engines, so it validates, for example, a function with more than 50,000 locals, which Node rejects. It decodes and validates `i32.mul`, `i32.and`, `i32.or`, `i64.and`, `i64.or`, `i64.shl` and `i64.shr_u` but stops with `Not_supported` when it reaches one; `safe` rules this out for the compiler's output.
- The engine that runs the bytes: loading, allocation of the memory and table, and the host call depth it allows.
- `wasm_u32.ml` declares `divide` and `remainder` as `external` (`%divint`, `%modint`) with a nonzero-divisor precondition; the checker gives them OCaml's truncating meaning.
- What the [Hindley–Milner page](hindley-milner.html) lists: the `Vox_iarray` externals and `raise_any`.

## Scope

- Source programs: a sequence of top-level `let`s whose right-hand sides are syntactically `fun` or recursive-function terms, which may be polymorphic, ending in such a term whose type has `word → word` as an instance. `let` inside a function must be monomorphic. Other programs are rejected (`Non_callable_outer_binding`, `Non_callable_entry`, `Unsupported_polymorphic_local_let`, `Entry_type_mismatch`), as are unbound variables and type errors. The interface states the meaning of each rejection, except that for a local `let` it states only that one exists.
- The input is fixed at compile time and the result is one 64-bit word. The module's exported `run` returns the status.
- One linear memory of `pages` pages, whose whole initial image is in the module's data section. The caller must supply a layout with `table_base` ≤ `frame_base` ≤ `stack_base` ≤ `heap_base` ≤ `heap_limit` and `frame_base` ≤ 4,294,967,216, and a memory image of at least `heap_limit` bytes. The emitted image must be exactly `pages` × 65,536 bytes with `pages` < 65,536. `Layout_rejected`, `Initialization_exhausted` and `Encoding_rejected` report a layout, initial heap or encoding that does not fit.
- Resource premise: for a source run of n machine steps, `sufficient` asks for 2(n+1) stack frames and 32(n+1)·(`stack_base` − `frame_base`) bytes of heap above the initial heap pointer, and `stack_base` − `frame_base` ≤ 268,435,455. With a 16-frame stack, as in the presentation's worked example, it covers source runs of at most 7 steps (applying `λx. x` to the input takes 7); longer runs are covered by `preservation`, which allows exhaustion. Heap and stack exhaustion are reported, not trapped.
- Only normal return of `compile`: it may fail to terminate and may raise what the inference raises. There is no bound on compile time or on the size of the output.
- The output is a `Wasm_u32.bytes` list with one cons cell per byte, and the capacities in the layout are unary naturals.
- The tests run the internal `Hmc_compiler.compile`, which `Hmc_compilation.compile` wraps, on sample programs and execute the result in the model. No test on the trunk runs `Hmc_compilation.compile` itself; the public client only proves facts about its result.
- The compiler, with the inference it uses, is 763 files and about 71,600 lines in `testsuite/tests/vox`.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/hmc_compilation_public_client.ml vox/hmc_compilation_rejected.ml vox/hmc_wasm_relayout_demo.ml
```

Each test compiles, and so checks, the whole composition in dependency order: 774 files for the public client and 953 for the sample-program test, which also compares its output with `hmc_wasm_relayout_demo.reference`. The public client runs on both backends.

The differential test of the WebAssembly model runs 300 fixed modules and compares its tally with `wasm_differential.reference`; it is skipped where `node` is not installed:

```
./dev test vox/wasm_differential.ml
```

`verification/wasm-differential/run.sh` runs the same check without ocamltest, and `verification/wasm-differential/run.sh --jobs 16 -count 100000 -seed 1 -pages 2 -detail` repeats the run of 100,000 modules.
