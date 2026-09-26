# Register allocation review boundary

Read these files in order. Together they are the complete human-review surface
for the exported claim:

1. `register_allocation_spec.ml`: all data types and complete executable
   definitions of register reads/writes, arithmetic, instruction execution,
   finite execution, validity, argument loading, allocation initialization, and
   observable equality. It imports no other project module. Its generated
   `_def` equation lemmas expose these same definitions to clients.
2. `register_allocation.mli`: the executable `allocate` entrypoint and the
   erased `preserves` and `allocation_domain` theorems. Every semantic symbol
   in these signatures is defined in the first file.
3. This file: the trust assumptions and scope below.

`register_allocation.ml` is checked against that interface. Liveness,
interference, coloring, renaming, and simulation invariants and their auxiliary
proofs are implementation details; none are exported by the interface.
`register_allocation_client.ml` imports only the two public modules. Its
`verified_run` derives an observation guarantee for executable target runs
without receiving an internal invariant, coloring, or certificate.

## Meaning and scope

Values are OCaml machine integers: addition and subtraction wrap, comparison
is signed, and Boolean arithmetic results are zero or one. Operands are read
before the destination is written. Node labels are zero-based list positions;
execution starts at node zero. The CFG permits arbitrary joins and back-edges.
Registers are mutable virtual locations with one global allocation per register.

`valid` admits 1–64 instructions and 1–32 virtual registers, with in-range
register operands, inputs, and successor labels. A successful allocation uses
1–32 physical slots, exactly the requested count, and retains the source input
layout for initialization. Physical slots have the same access semantics as
virtual slots; there is no hardware instruction or stack-access model.

Arguments are loaded in order into an initially zero source file. Duplicate
input declarations therefore use the last supplied value. The target adapter
loads that source input layout, then copies selected values into an initially
zero physical file. Target body execution uses only the physical file.

For a successful allocation and arguments of the declared shape, `preserves`
states observable equality after every inductive fuel value: equal returned
integers, or equal running program counters. `Stuck` is never observably equal,
even to itself. Thus the claim also excludes stuck executions under those
premises. It says nothing for incorrectly shaped arguments or failed
allocations. Allocation failure is permitted; no completeness, minimal-color,
or allocation-quality theorem is claimed. The current implementation bounds
liveness computation to 2049 rounds; execution lengths in the theorem are
unbounded finite lengths.

The allocation record contains executable code and initialization information.
There is no runtime correctness checker or accumulated proof certificate.
The public theorems are ghost results. Calls, phi instructions, spilling
lowering, and a bridge from another compiler IR remain outside this demo.

## Trusted basis and checks

The trusted basis is Vox's refinement, totality, module-signature, reflection,
and ghost-erasure checking, its SMT encoding and Z3, and the compiler/runtime
implementation of ordinary OCaml integers, comparisons, lists, variants and
records. No additional axioms, assumed allocator predicates, or trusted
allocation primitives are introduced. Runtime resource exhaustion is outside
the register-machine model.

The public client runs in bytecode and native code, each with and without
principal mode. `register_allocation_rejected.ml` checks that an internal graph
operation is hidden and that neither stuck states nor different returned
values can satisfy observable equality. `register_allocation_erasure.ml`
checks the runtime form of a client calling the public ghost theorem.
