# Goal

Continue the LLVM backend no-frontend-roots work and build focused evidence
for GC root correctness.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Editable paths: OxCaml sources, vendored LLVM sources, and testsuite files
  needed for LLVM backend GC root validation.

## Branches

- OxCaml: `jujacobs/llvm-backend`

## Pull Requests

- OxCaml PR: existing branch on Jules's `oxcaml` fork.

## Current Focus

- Preserve the no-frontend-roots model.
- Reduce and test missed or malformed frame-table roots.
- Keep stress tests small by default and scalable with `OCAML_TEST_SIZE`.
