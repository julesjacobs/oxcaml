#!/usr/bin/env bash
set -euo pipefail

# Just run the single ocamltest invocation via the top-level helper.
make test-one-no-rebuild TEST=testsuite/tests/typing-ikind/basic.ml
