# Goal

Port the fast-path-root-slot optimization onto
`jujacobs/llvm-backend-integration`.

The integration branch already contains the stack-check slack work and the
current LLVM statepoint/root machinery. This agent should preserve that base
and add the narrow fast-path-root optimization on top:

- For eligible heap allocation and poll safepoints, keep the common fast path
  free of explicit root-slot traffic.
- Spill live roots into static root slots only in the cold `caml_call_gc` slow
  path.
- Keep terminator calls, unwinding paths, active trap handlers, and other
  non-basic safepoints on the existing conservative root handling.
- Add/keep expect tests that show the intended code shape and identify missing
  coverage.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- May edit OxCaml backend and tests.
- May edit vendored LLVM only if validation shows the OxCaml-side port is not
  sufficient.
- Expected output: integration-based branch and PR containing the fast-path-root
  optimization, focused expect tests, and compact progress notes.

## Branches

- OxCaml: `jujacobs/llvm-fast-path-roots-integration`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/18
