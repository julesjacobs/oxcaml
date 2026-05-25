# Goal

Build AMD64 support for `-llvm-backend`.

Success means:

- Simple AMD64 `-llvm-backend` programs compile and run.
- The whole testsuite passes under `-llvm-backend`.
- The stage2 self-built LLVM-backend compiler builds and passes the testsuite.

## Scope

- Primary repo: OxCaml monorepo at `oxcaml/`
- Vendored LLVM path: `oxcaml/vendor/llvm-project`
- Editable areas: OxCaml sources and vendored LLVM sources.
- Expected output: focused implementation patches, minimized reproducers for
  failures, and validation evidence.

Useful Makefile targets:

- `ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-install`
- `ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-test-one TEST=<test>`
- `ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-test`
- `ARCH=amd64 LLVM_PATH="$LLVM_PATH" make llvm-self-stage2-test`

Before LLVM-backend work, run from this checkout:

`eval "$(../../../scripts/agent-tmp-env)"`

## Branches

- OxCaml: `jujacobs/llvm-amd64-support-2`

## Pull Requests

- OxCaml PR: https://github.com/julesjacobs/oxcaml/pull/12
