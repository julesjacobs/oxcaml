title: HM-to-WebAssembly compiler
blurb: A compiler from the Hindley–Milner language to WebAssembly bytes, proved against a WebAssembly model to emit one valid module that, for every 64-bit input passed at run time, cannot trap and returns only the source program's result on that input; it rejects a closed program of the stated shape that has a type word → word only for reasons that concern the layout and the compiled program, which are not characterized.
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
  - testsuite/tests/vox/hmc_compilation.ml — The interface implemented over the compiler
  - testsuite/tests/vox/hmc_compiler.ml — The compiler pipeline
  - testsuite/tests/vox/hmc_wasm_program_input.ml — The input at run time: the prologue's store gives the state built for that input
  - testsuite/tests/vox/hmc_frontend.ml — Frontend: scope check, inference, grounding and admission; the type-error lemmas
  - testsuite/tests/vox/hmc_admission.ml — Admission and the proof that the fragment is admitted
  - testsuite/tests/vox/hmc_compilation_public_client.ml — Public-only client
  - testsuite/tests/vox/hmc_compilation_rejected.ml — Rejected clients
  - testsuite/tests/vox/hmc_wasm_relayout_demo.ml — Runs the compiler on sample programs in the model
---
`Hmc_compilation.compile` takes a closed term of the language of the [Hindley–Milner page](hindley-milner.html), a memory layout and an initial memory image. It returns a rejection or an artifact whose `bytes` are a WebAssembly module. The module takes a 64-bit input when it runs: the host sets the module's exported mutable global `payload` to the input and then calls its exported function `run`, as in `instance.exports.payload.value = 4n; instance.exports.run()`. The module computes the term applied to that input. The theorems are stated against a model of WebAssembly that is part of the demo: the 43 `wasm_*.ml` files, 2,572 lines, on which the theorem statements depend. For every artifact and every input word `w`:

- `static_validity`: the bytes pass the model's validation.
- `safe`: at every step count, running the bytes on `w` has decoded and instantiated the module, set `payload` to `w` and has neither trapped, failed a type check, exceeded the host call depth nor reached an instruction outside the model.
- `reflection`: if the module run on `w` finishes returning a word, the source program applied to `w`, run by the source machine, returns the same word.
- `preservation`: if the source program applied to `w` returns a word, the module run on `w` finishes, either returning that word or reporting heap or stack exhaustion.
- `normal`: if in addition the layout meets the resource premise `sufficient`, the module run on `w` returns the word.
- `exhaustion`: a reported exhaustion happened at one of the emitted memory guards, when the requested space exceeded the space left.

Because the input is not known when compiling, one module must be right for every input. A compiler that ran the source program during compilation and emitted a module returning the answer would not satisfy these theorems.

`compile` also says why it rejects a program. A rejection carries a `reason` and the rejected `program`. `Unbound_variable` means the term is not closed. `Non_callable_outer_binding` and `Non_callable_entry` mean that a top-level `let` binds, or the program ends in, something other than a `fun` or recursive function (`M.outer_callable`, `M.entry_callable`). `Unsupported_polymorphic_local_let` means the program has a `let` inside a binding or the entry (`M.no_local_let` fails), and `Invalid_annotation` never happens. The lemmas `untypable` and `no_entry_type` show that `Type_error` and `Entry_type_mismatch` reject only terms with no type at all, or no type `word → word`; they are the [Hindley–Milner page](hindley-milner.html)'s `rejected` theorem carried through the compiler. So a client can prove that a closed term of this shape with a typing at `word → word` is either compiled or rejected for one of the three reasons that concern the layout and the compiled program.

The compiler infers types with the verified [Hindley–Milner inference](hindley-milner.html), rebuilds a typed derivation, specializes polymorphic functions, converts closures, builds a control-flow graph, marks tail calls and lowers to WebAssembly. Every stage is proved in the same composition and connected to the theorems above; no intermediate invariant is assumed. The stages' own contracts are internal and are not part of the interface. The module's initial memory holds the entry function's first frame, built for the input 0; a five-instruction prologue of the exported function stores the run's input in that frame (the payload of its first environment cell) and resets `payload`. The proof shows that the memory after the prologue is the one the compiler would have built for that input, so every later stage's proof applies to it unchanged.

Not proved: when `compile` rejects for `Layout_rejected`, `Initialization_exhausted` or `Encoding_rejected`. These depend on the layout, memory image and page count and on the compiled program (its largest frame, its closure table, its compiled globals and its encoding), which is not a function of the source in the proofs because the inference is not proved deterministic; so an implementation that rejected every program for one of these reasons would still satisfy the interface. The meaning of `Unsupported_polymorphic_local_let` is weaker than its name: a monomorphic local `let` is admitted and a polymorphic one is not, but whether a local `let` is generalized depends on the inferred derivation, so the interface only says that some local `let` exists. The resource premise of `normal` grows linearly with the number of source steps, whatever the program uses, so `normal` applies only to short runs (see Scope). The premise also reads the initial heap pointer from the emitted module's globals, so an implementation could make it false by emitting a module whose heap is already full, and then always stop at a failed heap guard; `exhaustion` requires the guard to have failed, not the source program to need the space. For a source program that does not return, a finished run is not proved to be either a return or an exhaustion. The WebAssembly model is assumed to agree with the standard and with engines, and `compile` is not proved to terminate.

## Client example

From the public-only client, which uses only the public interfaces. `(artifact : C.artifact) ->` names an argument so that later types can mention it. `{u : unit | p}` is an argument that carries only a proof of `p`, and `@ ghost` marks a result that is checked and then erased, like the code inside `ghost_ (...)`. `===` is logical equality, and `M.source_returns_def` unfolds the definition of `source_returns`. The function restates `reflection` in terms of the source machine: for any `input`, if a run of the emitted bytes on `input` finishes by returning `word`, the source applied to `input` reaches `Done (Word word)` after some number `n` of steps.

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let (normal_returns @ total)" "M.source_returns_def (C.source artifact) input n word; n)"

The client also combines `static_validity` with `safe`, and restates `preservation`, `normal` and `exhaustion`. The last function derives from the interface alone that the identity `λx. x` can be rejected only for one of those three reasons. It takes a typing of the identity at `Word64 → Word64` from `identity_typed` (a derivation built by unfolding `D.typed`), which refutes both type errors through `untypable` and `no_entry_type`, and unfolds `D.scoped_term` and the shape predicates to refute the other source reasons. It cannot conclude that `compile` succeeds.

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let compile_identity" "    out"


## A rejected program

Claiming a normal return without the resource premise is rejected, because `normal` also requires `M.sufficient`:

@code testsuite/tests/vox/hmc_compilation_rejected.ml "let (without_resources @ total)" "C.normal artifact input word fuel ());;"

```
Line 8, characters 84-86:
8 |   fun artifact input fuel word premise -> ghost_ (C.normal artifact input word fuel ());;
                                                                                        ^^
Error: Refinement could not be proved (counterexample)
File "hmc_compilation.mli", lines 86-87, characters 14-66:
  The refinement is stated here.
```

The same test checks that the proof evidence inside an artifact cannot be read: `out.evidence` is an unbound field.

## Interface

@code testsuite/tests/vox/hmc_compilation.mli

`artifact` is abstract; its `source` and `layout` are erased. `Wasm_binary_execution.run k bytes w c` decodes `bytes`, checks the memory and table limits, sets the exported global `payload` to `w` (which fails unless it is a mutable 64-bit global), starts the exported function `run` and runs at most `k` steps, stopping early if it finishes or fails, with host call depth `c`. It returns `Rejected` or the state reached: `Running`, `Finished`, `Trap`, `Type_error`, `Host_limit` or `Not_supported`. `Wasm_static_module.bytes_valid` is validation. The model covers the instructions the compiler emits: 32- and 64-bit constants and integer operations, locals, globals, 32- and 64-bit loads and stores, blocks, loops, conditionals, branches, and direct and indirect calls.

@code testsuite/tests/vox/hmc_compilation_model.ml

@code testsuite/tests/vox/hmc_layout.ml

`returned` requires `run` to have returned 1 and reads the result from the module's globals: status 1, tag 1 and the word as payload. `exhausted` requires `run` to have returned 2 (heap) or 3 (stack). The layout gives addresses in linear memory, a stack capacity in frames and a host call depth (the theorems run with one more than `host_capacity`); `valid_layout` orders the regions and requires the memory image to reach `heap_limit`. The source machine and its values:

@code testsuite/tests/vox/hm_interpreter_typing.ml "type value =" "[@@inductive]"

@code testsuite/tests/vox/hmc_source_semantics.ml

`Hmc_word64` adds and subtracts modulo 2^64.

## Trusted base

- The WebAssembly model: the 43 `wasm_*.ml` files (2,572 lines) that define decoding, validation and execution of the emitted subset. The theorems are about this model; its agreement with the WebAssembly specification and with engines is assumed, and nothing on the trunk compares them.
- The engine that runs the bytes: loading, allocation of the memory and table, and the host call depth it allows.
- `wasm_u32.ml` declares `divide` and `remainder` as `external` (`%divint`, `%modint`) with a nonzero-divisor precondition; the checker gives them OCaml's truncating meaning.
- What the [Hindley–Milner page](hindley-milner.html) lists: the `Vox_iarray` externals and `raise_any`.

## Scope

- Source programs: a sequence of top-level `let`s whose right-hand sides are syntactically `fun` or recursive-function terms, which may be polymorphic, ending in such a term whose type has `word → word` as an instance. `let` inside a function must be monomorphic. Other programs are rejected (`Non_callable_outer_binding`, `Non_callable_entry`, `Unsupported_polymorphic_local_let`, `Entry_type_mismatch`), as are unbound variables and type errors. The interface states the meaning of each rejection, except that for a local `let` it states only that one exists.
- The input is one 64-bit word passed at run time in the exported global `payload`, and the result is one 64-bit word, read from the exported globals `tag` and `payload` after `run` returns the status. The prologue that reads the input takes five of the model's steps, which the step counts in the theorems include. Each run is one call of `run` on a freshly instantiated module; calling `run` again on the same instance is not covered.
- One linear memory of `pages` pages, whose whole initial image is in the module's data section. The caller must supply a layout with `table_base` ≤ `frame_base` ≤ `stack_base` ≤ `heap_base` ≤ `heap_limit` and `frame_base` ≤ 4,294,967,216, and a memory image of at least `heap_limit` bytes. The emitted image must be exactly `pages` × 65,536 bytes with `pages` < 65,536. `Layout_rejected`, `Initialization_exhausted` and `Encoding_rejected` report a layout, initial heap or encoding that does not fit.
- Resource premise: for a source run of n machine steps, `sufficient` asks for 2(n+1) stack frames and 32(n+1)·(`stack_base` − `frame_base`) bytes of heap above the initial heap pointer, and `stack_base` − `frame_base` ≤ 268,435,455. With a 16-frame stack, as in the presentation's worked example, it covers source runs of at most 7 steps (applying `λx. x` to the input takes 7); longer runs are covered by `preservation`, which allows exhaustion. Heap and stack exhaustion are reported, not trapped.
- Only normal return of `compile`: it may fail to terminate and may raise what the inference raises. There is no bound on compile time or on the size of the output.
- The output is a `Wasm_u32.bytes` list with one cons cell per byte, and the capacities in the layout are unary naturals.
- The tests run the internal `Hmc_compiler.compile`, which `Hmc_compilation.compile` wraps, on sample programs and execute the result in the model, running one module on the inputs 0, 42 and 2^64 − 1. No test on the trunk runs `Hmc_compilation.compile` itself; the public client only proves facts about its result. `verification/catalogue/compiler-example/original_run_smoke.ml`, which is not a test, compiles the presentation's example with it and runs the one module on the inputs 4 and 8 in the model.
- The compiler, with the inference it uses, is 763 files and about 71,600 lines in `testsuite/tests/vox`.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/hmc_compilation_public_client.ml vox/hmc_compilation_rejected.ml vox/hmc_wasm_relayout_demo.ml
```

Each test compiles, and so checks, the whole composition in dependency order: 774 files for the public client and 953 for the sample-program test, which also compares its output with `hmc_wasm_relayout_demo.reference`. The public client runs on both backends.
