# Goal

Implement the first slice of the LLVM fast-path root-slot plan: let eligible
`-llvm-backend` heap allocation and poll fast paths keep live values promotable
by moving GC root materialization into the cold `caml_call_gc` path.

Use `agent-state/llvm-fast-path-roots/PLAN.md` as the design guide and source of
acceptance criteria.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Expected output: a focused OxCaml change, targeted LLVM-backend tests, full
  LLVM backend test suite passing, then self-stage2 passing.

## Branches

- OxCaml: `jujacobs/llvm-fast-path-roots`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/14
