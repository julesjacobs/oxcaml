title: Register allocation
blurb: A liveness-based register allocator for a small register machine, proved to preserve every finite run of a program it allocates; it may refuse any program.
status: owner-review
date: 4 October 2026
sources:
  - testsuite/tests/vox/register_allocation_spec.ml — The register machine: instructions, execution, validity, initial states and observation
  - testsuite/tests/vox/register_allocation.mli — Public interface
  - testsuite/tests/vox/register_allocation.ml — Liveness, interference, greedy colouring, renaming and the simulation proof
  - testsuite/tests/vox/register_allocation_review.md — Reading order, scope and trusted base
  - testsuite/tests/vox/register_allocation_client.ml — Public-only client
  - testsuite/tests/vox/register_allocation_rejected.ml — Rejected programs
  - testsuite/tests/vox/register_allocation_erasure.ml — The compiled form of a client that uses the theorem
---
`Register_allocation.allocate program physical` maps the virtual registers of a program for a small register machine onto `physical` slots. It computes liveness, builds an interference graph, colours it greedily and renames every register operand; it does not spill. The theorem `preserves` states that when `allocate` returns `Some allocation` and the argument list has one value per declared input, then after any number of steps the source program and `allocation.code` are both running at the same program counter or have both returned the same integer. A stuck run counts as different from every run, including another stuck one, so the theorem also rules out stuck executions. `allocation_domain` states that a successful allocation implies `valid program` and 1 ≤ `physical` ≤ 32, and that the allocation records `physical` and the program's register count and inputs unchanged. The machine, its execution and the observation are defined as ordinary total functions in `register_allocation_spec.ml`.

Nothing is proved about when `allocate` succeeds: every theorem is conditional on `Some`, so a function that always returns `None` satisfies the interface. Success is only tested. The implementation returns `None` when greedy colouring does not fit in `physical` slots, when a register that is not an input may be read before it is written (the machine would read 0), and when liveness has not converged after 2049 sweeps. `allocation_domain` also requires the target to have exactly as many instructions as the source and every target instruction to use registers below `physical` and labels below the target instruction count. This covers unreachable instructions as well.

## Interface

The complete specification is `register_allocation.mli` together with
`register_allocation_spec.ml`. The latter defines program validity, source
and target execution, the input adapter and `observable_equal`. Liveness,
interference, coloring and their proof invariants are in
`register_allocation.ml`.

@code testsuite/tests/vox/register_allocation.mli

`@ ghost` marks the two theorems as proof code: a program may call them only inside `ghost_`, and the calls are erased. The terms they use are defined in `register_allocation_spec.ml`, which imports no other module:

@code testsuite/tests/vox/register_allocation_spec.ml

`[@def]` makes each definition available to proofs through a generated equation lemma, such as `observable_equal_def`. `advance code fuel state` takes `fuel` steps. `observable_equal` compares the program counter of running states and the integer of returned states; register-file equality is unnecessary because allocation changes register placement. The client checks two different register files at the same program counter are observably equal and different program counters are forbidden. The target starts from `initial_of_allocation`: the source input registers are loaded as in `source_initial` and then copied into the physical slots listed in `input_slots`, which the allocation chooses.

## Trusted base

Nothing beyond the shared base. The one `unreachable_ ()` (in `color_at`, `register_allocation.ml`) is not an assumption: the checker must prove the branch unreachable, and a runtime trap is kept as well.

## Scope

- `valid` requires the machine-int instruction count to be 1–64 and the register count to be 1–32. The instruction count uses wrapping addition, so the logic also admits unrealizable lists whose length wraps into this range; for runtime programs these are 1–64 instructions and 1–32 virtual registers; `physical` is 1–32. Instructions are moves, addition, subtraction, equality and less-than on 63-bit wrapping `int`, jumps, two-way branches and returns. There are no calls, memory or spill code. The machine exists only in `register_allocation_spec.ml`; nothing relates it to OxCaml's backend or to hardware.
- No completeness theorem: a function returning `None` everywhere meets the interface. The client checks success on seven programs, including one with 32 inputs and 64 instructions in 32 slots.
- No quality theorem: the number of slots used, optimality of the colouring and running time are not stated. Liveness is bounded by 2049 sweeps; no theorem says that this bound is enough.
- The theorem compares source and target after the same number of steps, which fits an allocator that only renames registers; one that inserted moves would need a different statement. It says nothing for argument lists of the wrong length.
- If an input register is declared twice, the last argument wins, in the source and the target alike.
- `allocation` is a public record, so a client can build one by hand; the theorems cover only records returned by `allocate`.
- The 94 private lemmas in `register_allocation.ml` return their results `@ ghost`, so their bodies are erased; each still compiles to a small function that returns a placeholder.

## Client example

From the public-only client, which is compiled against `register_allocation.mli` and uses only the two public modules. `(program : program) -> ...` names an argument so that later types can mention it, `{result : state option | p}` is the type refined by the predicate `p`, `ghost_ (...)` is proof code, checked and then erased, and `@ total` declares a function that the checker proves terminates without raising. `fuel` is a unary natural number (`Z | S of fuel`). `verified_run` runs the allocated code, and its type states that the result agrees with the source program at the same number of steps.

@code testsuite/tests/vox/register_allocation_client.ml "let (verified_run @ total) :" "    Some (advance"

## A rejected program

`observable_equal` does not identify two stuck states, so a claim that it does is a type error. `observable_equal_def` gives the solver the definition of `observable_equal` at these arguments.

@code testsuite/tests/vox/register_allocation_rejected.ml "let (stuck_is_equivalent @ total) :" "  ());;"

@text testsuite/tests/vox/register_allocation_rejected.ml "Line 7, characters 2-4:" "Error: Refinement could not be proved"

The same test rejects a claim that `Done 1` and `Done 2` are observably equal, and a client that uses the private function `Register_allocation.graph`.

## Reproduce

After `make install` and `./dev init`:

```
./dev test vox/register_allocation_client.ml vox/register_allocation_rejected.ml vox/register_allocation_erasure.ml
```

`./dev test` first builds the spec, the interface and the implementation, which the three tests list as prebuilt, with both compilers, checking every proof. The client test then compiles the client against them as bytecode and as native code, runs it, and compares the output with `register_allocation_client.reference`. The rejection test runs as bytecode. The erasure test compiles, natively, a function that calls `preserves` inside `ghost_` and returns 7, and checks that the body of that function in the Lambda output is just the constant 7.
