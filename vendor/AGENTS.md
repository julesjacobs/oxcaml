# Vendored Source Instructions

`llvm-project/` is a vendored LLVM snapshot used by the OxCaml LLVM backend
integration branch.

Rules:

- Edit `llvm-project/` only when the local `GOAL.md` allows vendored LLVM
  changes.
- Keep OxCaml and vendored LLVM changes on the same OxCaml branch and PR.
- Do not create or push a separate LLVM branch unless explicitly requested.
- Keep `LLVM_BASE.md` accurate when changing the vendored LLVM baseline.
- Prefer a separate mechanical baseline update before semantic LLVM changes.
