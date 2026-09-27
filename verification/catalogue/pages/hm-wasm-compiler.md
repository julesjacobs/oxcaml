title: HM-to-WebAssembly compiler
blurb: A compiler from the Hindley–Milner language to WebAssembly bytes, proved against a WebAssembly model to emit one valid module that, for every 64-bit input passed at run time, cannot trap and returns only the source program's result on that input; it rejects a closed program of the stated shape that has a type word → word only for reasons that concern the layout and the compiled program, which are not characterized. Checked runs show it compiling eight example programs that then return the right word in the model and in Node.
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
  - testsuite/tests/vox/hmc_wasm_program_input.ml — The input at run time: the prologue's store gives the state built for that input
  - testsuite/tests/vox/hmc_frontend.ml — Frontend: scope check, inference, grounding and admission; the type-error lemmas
  - testsuite/tests/vox/hmc_admission.ml — Admission and the proof that the fragment is admitted
  - testsuite/tests/vox/hmc_compilation_public_client.ml — Public-only client
  - testsuite/tests/vox/hmc_compilation_rejected.ml — Rejected clients
  - testsuite/tests/vox/hmc_compilation_examples.ml — Compiles example programs with the public compile and runs them in the model
  - testsuite/tests/vox/hmc_compilation_examples.reference — The example runs' results and resource use
  - verification/catalogue/compiler-example/run-examples-node.js — Runs the example modules in Node and compares them with the model
  - verification/catalogue/compiler-example/run-examples.sh — Builds the example test and runs its modules in Node
  - testsuite/tests/vox/hmc_wasm_relayout_demo.ml — Runs the internal compiler on sample programs in the model
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

Not proved: when `compile` rejects for `Layout_rejected`, `Initialization_exhausted` or `Encoding_rejected`. These depend on the layout, memory image and page count and on the compiled program (its largest frame, its closure table, its compiled globals and its encoding), which is not a function of the source in the proofs because the inference is not proved deterministic; so an implementation that rejected every program for one of these reasons would still satisfy the interface; the example runs below check that this one compiles eight programs. The meaning of `Unsupported_polymorphic_local_let` is weaker than its name: a monomorphic local `let` is admitted and a polymorphic one is not, but whether a local `let` is generalized depends on the inferred derivation, so the interface only says that some local `let` exists. The resource premise of `normal` grows linearly with the number of source steps, whatever the program uses, so `normal` applies only to short runs (see Scope); of the example runs below, it covers only the identity's. The premise also reads the initial heap pointer from the emitted module's globals, so an implementation could make it false by emitting a module whose heap is already full, and then always stop at a failed heap guard; `exhaustion` requires the guard to have failed, not the source program to need the space. For a source program that does not return, a finished run is not proved to be either a return or an exhaustion. The WebAssembly model's agreement with the standard and with engines is tested, not proved (see Trusted base), and `compile` is not proved to terminate.

## Example runs

The interface would also be met by a compiler that rejected every program for one of the three reasons that concern the layout, or whose modules always reported exhaustion. `hmc_compilation_examples.ml` checks that this one does neither on eight programs. It calls the public `Hmc_compilation.compile`, runs each emitted module in the WebAssembly model, and compares the status and the result word with what the source machine returns. These are checked runs, not theorems. The theorems say what any finished run means: by `reflection` a returned word is the source's result, by `exhaustion` a reported exhaustion happened at a guard that found too little space, and by `preservation` the module finishes one way or the other whenever the source returns. The runs show that the module finishes by returning the answer, with ordinary limits and for runs far longer than `normal` covers.

Every run uses the same layout: two 64 KiB pages of memory, a 2 KiB region for the current frame below byte 4,096, room for 128 saved stack frames from byte 4,096, and a 64 KiB heap from byte 65,536. One run of the identity shrinks the current-frame region to 128 bytes, and the last two runs shrink the stack to 8 frames or the heap to 2 KiB. The programs are written with names and converted to the de Bruijn terms of `Hm_declarative` by the test:

@code testsuite/tests/vox/hmc_compilation_examples.ml "(* Library functions. *)" "Option.iter close_out expected_channel"

The output, which the test compares with `hmc_compilation_examples.reference`:

@text testsuite/tests/vox/hmc_compilation_examples.reference

"heap" is how far the heap pointer moved from `heap_base`, including the closures the module allocates while it initializes (16 bytes for each top-level function). "stack" is the largest number of saved frames seen between two blocks of the program; a frame's size depends on the program. "sufficient" says whether `M.sufficient`, the premise of `normal`, holds for the run's number of source steps; the test evaluates it on the emitted bytes. The runs show:

- `sufficient` holds only for the identity, and only once the current-frame region is cut to 128 bytes: for n source steps the premise sets aside 16·(`stack_base` − `frame_base`) bytes of heap for each of 2(n+1) steps of the compiler's internal machine, so with the 2 KiB region it fails even for the 7 steps of the identity. For that one run, `normal` proves the return that the run shows; for the others, only the run shows it.
- `count` takes about 3.5 million WebAssembly steps (48,022 steps of the source machine) in one frame, allocating nothing beyond its two top-level closures: a recursive function's call to itself in tail position is compiled as a jump.
- Other tail calls keep their frame. `accumulate` is a curried loop `loop acc n`: `loop acc` returns a closure, and the call of that closure in tail position is an ordinary call, so the loop uses one frame per iteration, like the non-tail recursion of `sum-list`.
- Nothing is freed: each closure `loop acc` stays on the heap (about 48 bytes per iteration of `accumulate`).
- With 8 frames, or with 2 KiB of heap, `sum-list` reports stack or heap exhaustion instead of an answer.

With the environment variable `HMC_EXAMPLES_DIR` set, the test also writes each module and the final memory the model computed for it. `run-examples.sh` builds the test that way and `run-examples-node.js` runs the twelve modules in Node's WebAssembly engine and checks that the status, the result word and the whole final memory agree with the model; they did on 27 September 2026 with Node v25.6.1. This checks the model on these modules only; nothing in the tests runs Node.

## Client example

From the public-only client, which uses only the public interfaces. `(artifact : C.artifact) ->` names an argument so that later types can mention it. `{u : unit | p}` is an argument that carries only a proof of `p`, and `@ ghost` marks a result that is checked and then erased, like the code inside `ghost_ (...)`. `===` is logical equality, and `M.source_returns_def` unfolds the definition of `source_returns`. The function restates `reflection` in terms of the source machine: for any `input`, if a run of the emitted bytes on `input` finishes by returning `word`, the source applied to `input` reaches `Done (Word word)` after some number `n` of steps.

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let (normal_returns @ total)" "M.source_returns_def (C.source artifact) input n word; n)"

The client also combines `static_validity` with `safe`, and restates `preservation`, `normal` and `exhaustion`. The last function derives from the interface alone that the identity `λx. x` can be rejected only for one of those three reasons. It takes a typing of the identity at `Word64 → Word64` from `identity_typed` (a derivation built by unfolding `D.typed`), which refutes both type errors through `untypable` and `no_entry_type`, and unfolds `D.scoped_term` and the shape predicates to refute the other source reasons. It cannot conclude that `compile` succeeds. The client then proves, by unfolding the source machine, that the identity returns its input in 7 steps (`identity_steps`), and with `normal` that a compiled identity whose layout meets `sufficient` for 7 steps returns its input:

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let compile_identity" "    out"

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let (identity_returns @ total)" "C.normal artifact (C.input artifact)"

Whether `compile` accepts the identity for a given layout, and whether `sufficient` holds for the emitted bytes, is decided by running them: the example runs do both.


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

- The WebAssembly model: the 43 `wasm_*.ml` files (2,572 lines) that define decoding, validation and execution of the emitted subset. The theorems are about this model; its agreement with the WebAssembly specification and with engines is not proved. It is tested: `wasm_differential.ml` generates modules in the model's subset, variants with one mutation (most of them invalid), modules with a corrupted byte, and valid modules outside the subset, with its own encoder, which shares no code with the model's. For each module it checks that the model and Node agree on validity and, when both run the module, on trap or return, the returned value, every global and every byte of final memory. On 100,000 modules (seed 1, Node 22) there was no disagreement. The model has none of the implementation limits that the JavaScript API sets for engines, so it validates, for example, a function with more than 50,000 locals, which Node rejects. It decodes and validates `i32.mul`, `i32.and`, `i32.or`, `i64.and`, `i64.or`, `i64.shl` and `i64.shr_u` but stops with `Not_supported` when it reaches one; `safe` rules this out for the compiler's output. The twelve example modules below also agree with Node.
- The engine that runs the bytes: loading, allocation of the memory and table, and the host call depth it allows.
- `wasm_u32.ml` declares `divide` and `remainder` as `external` (`%divint`, `%modint`) with a nonzero-divisor precondition; the checker gives them OCaml's truncating meaning.
- What the [Hindley–Milner page](hindley-milner.html) lists: the `Vox_iarray` externals and `raise_any`.

## Scope

- Source programs: a sequence of top-level `let`s whose right-hand sides are syntactically `fun` or recursive-function terms, which may be polymorphic, ending in such a term whose type has `word → word` as an instance. `let` inside a function must be monomorphic. Other programs are rejected (`Non_callable_outer_binding`, `Non_callable_entry`, `Unsupported_polymorphic_local_let`, `Entry_type_mismatch`), as are unbound variables and type errors. The interface states the meaning of each rejection, except that for a local `let` it states only that one exists.
- The input is one 64-bit word passed at run time in the exported global `payload`, and the result is one 64-bit word, read from the exported globals `tag` and `payload` after `run` returns the status. The prologue that reads the input takes five of the model's steps, which the step counts in the theorems include. Each run is one call of `run` on a freshly instantiated module; calling `run` again on the same instance is not covered.
- One linear memory of `pages` pages, whose whole initial image is in the module's data section. The caller must supply a layout with `table_base` ≤ `frame_base` ≤ `stack_base` ≤ `heap_base` ≤ `heap_limit` and `frame_base` ≤ 4,294,967,216, and a memory image of at least `heap_limit` bytes. The emitted image must be exactly `pages` × 65,536 bytes with `pages` < 65,536. `Layout_rejected`, `Initialization_exhausted` and `Encoding_rejected` report a layout, initial heap or encoding that does not fit.
- Resource premise: for a source run of n machine steps, `sufficient` asks for 2(n+1) stack frames and 32(n+1)·(`stack_base` − `frame_base`) bytes of heap above the initial heap pointer, and `stack_base` − `frame_base` ≤ 268,435,455. With a 16-frame stack, as in the presentation's worked example, the stack part covers source runs of at most 7 steps (applying `λx. x` to the input takes 7), and the heap part needs 16·(`stack_base` − `frame_base`) bytes for each of 16 internal steps, 512 KiB for the presentation's 2 KiB current-frame region, more than its heap; so `sufficient` holds for no run in that layout. The identity meets it with a 128-byte region and a 64 KiB heap (see Example runs). Longer runs are covered by `preservation`, which allows exhaustion, and the example runs show modules returning after millions of steps within ordinary limits. Heap and stack exhaustion are reported, not trapped.
- Only normal return of `compile`: it may fail to terminate and may raise what the inference raises. There is no bound on compile time or on the size of the output.
- The output is a `Wasm_u32.bytes` list with one cons cell per byte, and the capacities in the layout are unary naturals.
- Only a recursive function's call to itself in tail position is compiled as a jump. Every other call, in tail position or not, keeps a stack frame until it returns, and a curried recursive function's calls are calls of closures. There is no garbage collector: every closure and list cell stays on the heap until the run ends.
- `hmc_compilation_examples.ml` runs `Hmc_compilation.compile` on the example programs above; `hmc_wasm_relayout_demo.ml` runs the internal `Hmc_compiler.compile`, which `Hmc_compilation.compile` wraps, on further sample programs, running one module on the inputs 0, 42 and 2^64 − 1. Both execute the result in the model.
- The compiler, with the inference it uses, is 763 files and about 71,600 lines in `testsuite/tests/vox`.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/hmc_compilation_public_client.ml vox/hmc_compilation_rejected.ml vox/hmc_compilation_examples.ml vox/hmc_wasm_relayout_demo.ml
```

Each test compiles, and so checks, the whole composition in dependency order: 774 files for the public client and the example runs and 953 for the sample-program test. The example runs and the sample-program test compare their output with their `.reference` files. The public client runs on both backends.

The differential test of the WebAssembly model runs 40 fixed modules (a smoke check; the 100,000-module run is `verification/wasm-differential/run.sh --jobs 16 -count 100000 -seed 1 -pages 2`) and compares its tally with `wasm_differential.reference`; it is skipped where `node` is not installed:

```
./dev test vox/wasm_differential.ml
```

`verification/wasm-differential/run.sh` runs the same check without ocamltest, and `verification/wasm-differential/run.sh --jobs 16 -count 100000 -seed 1 -pages 2 -detail` repeats the run of 100,000 modules.

To run the example modules in Node (this builds the example test's files with `-smt-assume-verified`, since the test above verifies them):

```
verification/catalogue/compiler-example/run-examples.sh
```
