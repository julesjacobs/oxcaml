title: HM-to-WebAssembly compiler
blurb: A compiler from a small ML to WebAssembly, proved to emit valid bytes, run safely, and agree with source results, with explicit heap and stack exhaustion.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/hmc_compilation.mli — Public interface
  - testsuite/tests/vox/hmc_compilation_model.ml — Layout validity, results, the resource premise and honest exhaustion
  - testsuite/tests/vox/hmc_layout.ml — The memory layout
  - testsuite/tests/vox/hmc_source_semantics.ml — Source semantics: an environment machine
  - testsuite/tests/vox/hmc_source_values.ml — Source values
  - testsuite/tests/vox/hmc_word64.ml — Source word arithmetic
  - testsuite/tests/vox/hm_declarative.ml — Source terms
  - testsuite/tests/vox/hmc_failed_guard_model.ml — The heap and stack guard instructions
  - testsuite/tests/vox/wasm_binary_execution.ml — WebAssembly model: decode, instantiate and run a module
  - testsuite/tests/vox/wasm_calls.ml — WebAssembly model: execution with calls
  - testsuite/tests/vox/wasm_binary_module.ml — WebAssembly model: module decoding
  - testsuite/tests/vox/wasm_instance_control.ml — WebAssembly model: instruction execution
  - testsuite/tests/vox/wasm_static_control.ml — WebAssembly model: body validation
  - testsuite/tests/vox/wasm_instruction.ml — WebAssembly model: the instruction subset and its encoding
  - testsuite/tests/vox/wasm_static_module.ml — WebAssembly model: validation
  - testsuite/tests/vox/wasm_differential.ml — Differential test of the WebAssembly model against Node
  - testsuite/tests/vox/hmc_compilation.ml — The interface implemented over the compiler
  - testsuite/tests/vox/hmc_compiler.ml — The compiler pipeline
  - testsuite/tests/vox/hmc_wasm_program_input.ml — The input at run time: the prologue's store gives the state built for that input
  - testsuite/tests/vox/hmc_wasm_program_runtime.ml — The exported function: the prologue and the dispatcher loop
  - testsuite/tests/vox/hmc_tail_sites.ml — The calls compiled as jumps: a recursive function's calls to itself in tail position
  - testsuite/tests/vox/hmc_frontend.ml — Frontend: scope check, inference, grounding and admission; the type-error lemmas
  - testsuite/tests/vox/hmc_admission.ml — Admission and the proof that the fragment is admitted
  - testsuite/tests/vox/hmc_compilation_public_client.ml — Public-only client
  - testsuite/tests/vox/hmc_compilation_rejected.ml — Rejected clients
  - testsuite/tests/vox/hmc_compilation_examples.ml — Compiles example programs with the public compile and runs them in the model
  - testsuite/tests/vox/hmc_compilation_examples.reference — The example runs' results and resource use
  - verification/catalogue/compiler-example/run-examples-node.js — Runs the example modules in Node and compares them with the model
  - verification/catalogue/compiler-example/run-examples.sh — Builds the example test and runs its modules in Node
  - testsuite/tests/vox/hmc_wasm_relayout_demo.ml — Runs the internal stages on sample programs, and the internal compiler on the identity, in the model
  - testsuite/tests/vox/hmc_wasm_program_state_fixture.ml — The sample programs of that test and its run of the internal compiler
---
`Hmc_compilation.compile` takes an ML term, a memory layout and an initial
memory image. It returns a rejection or a WebAssembly module. The host sets
the exported `payload` global to a 64-bit input and calls `run`; the module
computes the source term applied to that input. One module serves every input.
The guarantees are about the demo's concrete WebAssembly model:

- `static_validity`: the bytes pass validation.
- `safe`: execution stays running or finishes, without trapping or failing a type check.
- `reflection`: every returned word is the source result on that input.
- `preservation`: when the source returns, the module returns the same word or reports exhaustion.
- `normal`: with the resource premise `sufficient`, it returns the word.
- `exhaustion`: reported exhaustion follows a heap or stack guard that found too little space.

Compilation infers types, specializes polymorphic functions, converts closures,
builds a control-flow graph, marks self tail calls and lowers to WebAssembly.
The proofs compose across these passes. The initial frame is built for input
0; a five-instruction prologue installs the runtime input, and the proof
relates that frame to the source application.

The interface characterizes source rejections, with two qualifications:
`Unsupported_polymorphic_local_let` establishes only that a local `let`
exists, and the three layout/initialization/encoding rejections are not
characterized. Acceptance is not guaranteed. `normal` uses a conservative
budget based on source steps; the example runs below show successful runs
beyond that budget. A module with a full initial heap could make the premise
false and report exhaustion on every input. `compile` is not proved to
terminate. The model's agreement with engines is tested, not proved.

## Interface

`compile` produces WebAssembly bytes or a rejection. A compiled artifact
keeps its source and layout as erased observations. The main guarantees are
validation, safe execution, and agreement with the source result; `normal`
also needs the stated resource premise. Type-error evidence follows these
contracts in the interface.

@code testsuite/tests/vox/hmc_compilation.mli

The source observation applies the compiled program to a runtime input and
runs the source machine for a number of steps:

@code testsuite/tests/vox/hmc_compilation_model.ml "let[@def] (source_returns @ total)" "type execution ="

`returned` reads the status, tag and payload globals of a finished module:

@code testsuite/tests/vox/hmc_compilation_model.ml "let[@def] (returned @ total)" "| _ -> false)"

The source machine evaluates functions, recursive functions, `let`, lists,
conditionals and word primitives with explicit environments and continuations.
Its full [values](src:testsuite/tests/vox/hmc_source_values.ml)
and [execution definitions](src:testsuite/tests/vox/hmc_source_semantics.ml)
are pure. Its step function defines each source operation:

@code testsuite/tests/vox/hmc_source_semantics.ml "let[@def] (step @ total)" "| _ -> Stuck))"

Source terms and typing are on the [Hindley–Milner page](hindley-milner.html).
[Word arithmetic](src:testsuite/tests/vox/hmc_word64.ml) adds and
subtracts modulo 2^64; proofs start after its `Proofs` marker.

For resources, the [layout](src:testsuite/tests/vox/hmc_layout.ml)
gives memory regions and stack capacities. `valid_layout` requires ordered
regions and an initial memory image reaching the heap limit:

@code testsuite/tests/vox/hmc_compilation_model.ml "let[@def] (valid_layout @ total)" "&& not (Hmc_linear_bytes.drop memory layout.heap_limit === None))"

`normal` requires `sufficient`, which reserves stack and heap space for the
source step count. Its complete arithmetic is shown because it materially
limits the guarantee:

@code testsuite/tests/vox/hmc_compilation_model.ml "let[@def] rec (stack_fits @ total)" "type exhaustion ="

[Rejection predicates and the exhaustion witness](src:testsuite/tests/vox/hmc_compilation_model.ml)
complete the model. `honest_exhaustion` requires a finished exhaustion to
follow a failed heap or stack guard, rather than merely have the right status.

The target contract uses a concrete WebAssembly model.
[Loading and running bytes](src:testsuite/tests/vox/wasm_binary_execution.ml)
sets the exported `payload` global to the input and calls `run`, for at most
the given number of steps. It depends on
[decoding](src:testsuite/tests/vox/wasm_binary_module.ml),
[execution with calls](src:testsuite/tests/vox/wasm_calls.ml), and
[instruction execution](src:testsuite/tests/vox/wasm_instance_control.ml).
[Validation](src:testsuite/tests/vox/wasm_static_module.ml) checks
the decoded module with the [instruction and stack checker](src:testsuite/tests/vox/wasm_static_control.ml).
These are semantic definitions; source-execution proofs are separately in
`hmc_source_proofs.ml` and value-typing proofs in `hm_interpreter_typing.ml`.

## Trusted base

- The WebAssembly model: the 43 `wasm_*.ml` files (2,582 lines) that define decoding, validation and execution of the emitted subset. The theorems are about this model; its agreement with the WebAssembly specification and with engines is not proved. It is tested: `wasm_differential.ml` generates modules in the model's subset, variants with one mutation (most of them invalid), modules with a corrupted byte, and valid modules outside the subset, with its own encoder, which shares no code with the model's. For each module it checks that the model and Node agree on validity and, when both run the module, on trap or return, the returned value, every global and every byte of final memory. On 100,000 modules (seed 1, Node 22) there was no disagreement. The model has none of the implementation limits that the JavaScript API sets for engines, so it validates, for example, a function with more than 50,000 locals, which Node rejects. It decodes and validates `i32.mul`, `i32.and`, `i32.or`, `i64.and`, `i64.or`, `i64.shl` and `i64.shr_u` but stops with `Not_supported` when it reaches one; `safe` rules this out for the compiler's output. The example modules above also agree with Node on all 15 runs.
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
- Only a recursive function's call to itself in tail position is compiled as a jump. Every other call, in tail position or not, keeps a stack frame until it returns, and a curried recursive function's calls are calls of closures. There is no garbage collector: every closure and list cell stays on the heap until the run ends. The backend does not optimize (see How the emitted module runs).
- `hmc_compilation_examples.ml` runs `Hmc_compilation.compile` on the example programs above. `hmc_wasm_relayout_demo.ml` runs the internal stages on further sample programs, and runs the internal `Hmc_compiler.compile`, which `Hmc_compilation.compile` wraps, on the identity, executing one module on the inputs 0, 42 and 2^64 − 1 (`binary_prefixes` in `hmc_wasm_program_state_fixture.ml`). Both execute the result in the model.

## How the emitted module runs

The backend does not use WebAssembly's call stack for source calls. It manages its own frames in linear memory and runs a dispatcher loop, so that running out of stack is detected and reported by the module instead of trapping in the engine. Every block of the control-flow graph, which holds one instruction, becomes a WebAssembly function without parameters or results (`hmc_wasm_program_lower.ml`, `hmc_wasm_program_binary.ml`). The exported function `run` is the five-instruction prologue followed by a loop that reads the current block's number from the current frame, calls that block's function through the table with `call_indirect`, and repeats while the status global is 0 (`dispatch_body` and `loop_code` in `hmc_wasm_program_runtime.ml`). That `call_indirect` is the only call the compiler emits, so the engine's call depth is at most two whatever the source does. The current frame sits at `frame_base`; a call saves it on a stack of fixed-size frames from `stack_base`, and a return copies the saved frame back. Closures and list cells are allocated from the initial heap pointer towards `heap_limit`. Before each allocation and each saved frame, a guard compares the requested bytes with the space left (`hmc_wasm_allocation_exit.ml`; the failed guard is `Hmc_failed_guard_model.failed`); when it fails, the block sets the status to 2 (heap) or 3 (stack) and `run` returns it. This is what `safe` (no trap, no host limit) and `exhaustion` (a failed guard) are about.

It is not an optimizing backend. Every source operation takes at least one block; each block is a call through the table, loads the registers from globals on entry and stores them back on exit, and reads and writes source values in the frame in memory. `count` below takes about 73 WebAssembly steps per step of the source machine. Only a recursive function's call to itself in tail position is compiled as a jump (`collect` in `hmc_tail_sites.ml`); every other call, in tail position or not, saves a frame until it returns. There is no garbage collector: the heap pointer only moves up (`Hmc_heap_allocate.allocate`), and nothing is freed before the run ends.

## Example runs

The interface would also be met by a compiler that rejected every program for one of the three reasons that concern the layout, or whose modules always reported exhaustion. `hmc_compilation_examples.ml` checks that this one does neither on eight programs. It compiles each program once with the public `Hmc_compilation.compile` and runs the emitted module in the WebAssembly model on each of the program's inputs, setting `payload` and calling `run` as a host does, and compares the status and the result word with what the source machine returns. These are checked runs, not theorems. The theorems say what any finished run means: by `reflection` a returned word is the source's result, by `exhaustion` a reported exhaustion happened at a guard that found too little space, and by `preservation` the module finishes one way or the other whenever the source returns. The runs show that the module finishes by returning the answer, with ordinary limits and for runs far longer than `normal` covers.

Every run uses the same layout: two 64 KiB pages of memory, a 2 KiB region for the current frame below byte 4,096, room for 128 saved stack frames from byte 4,096, and a 64 KiB heap from byte 65,536. One run of the identity shrinks the current-frame region to 128 bytes, and the last two runs shrink the stack to 8 frames or the heap to 2 KiB. The programs are written with names and converted to the de Bruijn terms of `Hm_declarative` by the test:

@code testsuite/tests/vox/hmc_compilation_examples.ml "(* Library functions. *)" "Option.iter close_out expected_channel"

The output, which the test compares with `hmc_compilation_examples.reference`:

@text testsuite/tests/vox/hmc_compilation_examples.reference

"heap" is how far the heap pointer moved from `heap_base`, including the closures of the top-level functions and of the entry, which the compiler places in the initial heap (16 bytes each). "stack" is the largest number of saved frames seen between two blocks of the program; a frame's size depends on the program. "sufficient" says whether `M.sufficient`, the premise of `normal`, holds for the run's number of source steps; the test evaluates it on the emitted bytes. The runs show:

- `sufficient` holds only for the identity, and only once the current-frame region is cut to 128 bytes: for n source steps the premise sets aside 16·(`stack_base` − `frame_base`) bytes of heap for each of 2(n+1) steps of the compiler's internal machine, so with the 2 KiB region it fails even for the 7 steps of the identity. For that one run, `normal` proves the return that the run shows; for the others, only the run shows it.
- `count` takes about 3.5 million WebAssembly steps (48,022 steps of the source machine) in one frame, allocating nothing beyond its two top-level closures: a recursive function's call to itself in tail position is compiled as a jump.
- Other tail calls keep their frame. `accumulate` is a curried loop `loop acc n`: `loop acc` returns a closure, and the call of that closure in tail position is an ordinary call, so the loop uses one frame per iteration, like the non-tail recursion of `sum-list`.
- Nothing is freed: each closure `loop acc` stays on the heap (about 48 bytes per iteration of `accumulate`).
- With 8 frames, or with 2 KiB of heap, `sum-list` reports stack or heap exhaustion instead of an answer.
- One module serves every input: `design`, `fibonacci` and `sum-list` each run a single compiled module on several inputs, as the line after their runs records. The WebAssembly step counts include the five steps of the prologue that reads the input.

With the environment variable `HMC_EXAMPLES_DIR` set, the test also writes each module and the final memory the model computed for each run. `run-examples.sh` builds the test that way and `run-examples-node.js` repeats the 15 runs of the 11 modules in Node's WebAssembly engine, each on a fresh instance with `payload` set to the input, and checks that the status, the result word and the whole final memory agree with the model; they did on 27 September 2026 with Node v25.6.1 and Node v22.22.2. This checks the model on these modules only; the suite does not run them in Node.

## Client example

From the public-only client, which uses only the public interfaces. `(artifact : C.artifact) ->` names an argument so that later types can mention it. `{u : unit | p}` is an argument that carries only a proof of `p`, and `@ ghost` marks a result that is checked and then erased, like the code inside `ghost_ (...)`. `===` is logical equality, and `M.source_returns_def` unfolds the definition of `source_returns`. The function restates `reflection` in terms of the source machine: for any `input`, if a run of the emitted bytes on `input` finishes by returning `word`, the source applied to `input` reaches `Done (Word word)` after some number `n` of steps.

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let (normal_returns @ total)" "M.source_returns_def (C.source artifact) input n word; n)"

The client also combines `static_validity` with `safe`, and restates `preservation`, `normal` and `exhaustion`. The last function derives from the interface alone that the identity `λx. x` can be rejected only for one of those three reasons. It takes a typing of the identity at `Word64 → Word64` from `identity_typed` (a derivation built by unfolding `D.typed`), which refutes both type errors through `untypable` and `no_entry_type`, and unfolds `D.scoped_term` and the shape predicates to refute the other source reasons. It cannot conclude that `compile` succeeds. The client then proves, by unfolding the source machine, that the identity returns its input in 7 steps (`identity_steps`), and with `normal` that a compiled identity whose layout meets `sufficient` for 7 steps returns every input it is run on:

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let compile_identity" "    out"

@code testsuite/tests/vox/hmc_compilation_public_client.ml "let (identity_returns @ total)" "C.normal artifact input input"

Whether `compile` accepts the identity for a given layout, and whether `sufficient` holds for the emitted bytes, is decided by running them: the example runs do both.


## A rejected program

Claiming a normal return without the resource premise is rejected, because `normal` also requires `M.sufficient`:

@code testsuite/tests/vox/hmc_compilation_rejected.ml "let (without_resources @ total)" "C.normal artifact input word fuel ());;"

@text testsuite/tests/vox/hmc_compilation_rejected.ml "Line 8, characters 84-86:" "The refinement is stated here."

The same test checks that the proof evidence inside an artifact cannot be read: `out.evidence` is an unbound field.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/hmc_compilation_public_client.ml vox/hmc_compilation_rejected.ml vox/hmc_compilation_examples.ml vox/hmc_wasm_relayout_demo.ml
```

Each test compiles, and so checks, the whole composition in dependency order before the test file itself: 774 files for the public client, the rejected clients and the example runs, and 953 for the sample-program test. The example runs and the sample-program test compare their output with their `.reference` files. The public client runs on both backends.

The differential test of the WebAssembly model runs 40 fixed modules (a smoke check; the 100,000-module run is `verification/wasm-differential/run.sh --jobs 16 -count 100000 -seed 1 -pages 2`) and compares its tally with `wasm_differential.reference`; it is skipped where `node` is not installed:

```
./dev test vox/wasm_differential.ml
```

`verification/wasm-differential/run.sh` runs the same check without ocamltest, and `verification/wasm-differential/run.sh --jobs 16 -count 100000 -seed 1 -pages 2 -detail` repeats the run of 100,000 modules.

To run the example modules in Node (this builds the example test's files with `-smt-assume-verified`, since the test above verifies them):

```
verification/catalogue/compiler-example/run-examples.sh
```
