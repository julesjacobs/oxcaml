# Goal

Make the stack-check assembly sequence emitted by the LLVM backend about as
efficient as the native backend sequence, while keeping stack checks in the
function preamble.

This goal is about the local instruction sequence used for the stack check. It
is not about sinking stack checks into the CFG.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Editable sources: OxCaml sources and vendored LLVM sources, if needed.
- Expected output: a focused implementation that improves the generated
  preamble stack-check sequence, plus a small assembly-level reproducer or test
  showing the sequence before and after.

## Non-goals

- Do not move stack checks out of the preamble.
- Do not attempt CFG sinking of stack checks.
- Do not broaden the work to unrelated LLVM backend code generation cleanups.

## Branches

- OxCaml: `jujacobs/llvm-stack-check-preamble`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/3
