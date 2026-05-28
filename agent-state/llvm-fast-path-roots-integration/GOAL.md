# Goal

Improve AArch64 LLVM backend `try`/trap performance without weakening the
correctness model for runtime-entered recovery blocks.

The current implementation has the right high-level safety direction: LLVM must
know about control-flow transfers into recovery blocks, and ordinary registers
must not be assumed live across those transfers. The remaining problem is that
the safe implementation uses a heavier trap representation than native code,
and that shows up in exception-heavy benchmarks.

Find and implement a production-quality design whose intended machine-code
model is native-like trap handling:

- entering a trap-protected region publishes a recovery target and links a
  small trap frame into the OCaml trap chain;
- normal exit unlinks that trap frame cheaply;
- exception recovery enters the recovery block with the OCaml recovery ABI
  state, after the trap frame has restored the stack/trap-chain state expected
  by ordinary handler code;
- ordinary stack slots remain addressable correctly before, inside, and after
  the trap-protected region.

The final LLVM representation does not need to literally mirror native
compiler internals. It should use whatever LLVM-side constructs make those
machine-code semantics explicit enough for optimization, frame-index
elimination, register allocation, verification, and tests.

Do not treat the current prototype details as the required final design. They
are evidence, not a specification. The implementation should be chosen after
checking the relevant LLVM mechanisms and failure modes.

The important requirement is the native-like model, not a specific spelling:
LLVM must see the recovery control-flow facts that affect optimization and
register allocation, while final AArch64 code should avoid paying for an
unnecessary helper call or unnecessary frame reshaping on the hot path.

Prefer an implementation that reuses existing LLVM concepts when they match the
semantics. If existing LLVM exception or call-edge machinery does not match the
OCaml runtime ABI, document the mismatch and add the smallest explicit
OxCaml/AArch64 extension needed to model the real control flow and recovery
ABI.

The design must answer these questions:

- How is the active trap region represented so that optimizers and register
  allocation see all relevant recovery edges?
- How are stack-pointer changes around trap setup, normal trap exit, and
  recovery entry represented so that frame-index elimination addresses stack
  slots correctly?
- What are the exact invariants of a runtime-entered recovery block?
- Which parts belong in generic LLVM code, which parts belong in AArch64 target
  code, and which parts belong in OxCaml IR/codegen?
- What malformed shapes must be rejected by verification or tests?

Expected outcome:

- A documented design decision with enough evidence that another reviewer can
  see why it is the right boundary.
- LLVM/OxCaml code changes implementing that design.
- The design is used by the OxCaml LLVM backend for real `try`/`raise`
  lowering, not only demonstrated in standalone LLVM tests.
- Representative `try`/`raise` source cases produce good AArch64 codegen,
  especially hot non-raising paths, hot raising paths, nested traps, and calls
  inside active trap regions.
- The representative microbenchmarks show that the exception-heavy slowdowns
  have materially improved.
- Focused tests covering correct code shape and the important invalid shapes.
- The regular OxCaml test suite passes with the LLVM backend changes.
- Full self-stage2 validation passes, including the stage2 test suite.

The work is not done when only standalone LLVM IR tests pass. It is done when
real OxCaml `try`/`raise` lowering uses the design, the generated code has the
intended native-like shape, and the correctness and performance validation above
both pass.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- May edit OxCaml backend, tests, and vendored LLVM.
- Preserve the integration branch's existing LLVM-backend work unless a change
  is necessary for the trap design.
- Keep progress notes compact and focused on decisions, evidence, and remaining
  risk.

## Branches

- OxCaml: `jujacobs/llvm-fast-path-roots-integration`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/18
