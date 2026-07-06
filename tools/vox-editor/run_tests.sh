#!/bin/sh
# Run every vox-editor test layer. Exits non-zero on the first failure.
#
# Env (auto-detected if unset):
#   VOX_OCAMLC   built ocamlc.opt (default: ../../_build/_bootinstall/bin/ocamlc.opt)
#   VOX_LEAN     lean binary (default: the pinned nix path)
#   TMPDIR       scratch on a big disk (Lean writes here)
set -e
cd "$(dirname "$0")"
: "${TMPDIR:=/usr/local/home/jujacobs/tmp-testsuite}"
mkdir -p "$TMPDIR"
export TMPDIR
export NO_PROXY=127.0.0.1,localhost

echo "== python: vc_index / lean_bridge / server / make_examples (unit + end-to-end) =="
python3 -m unittest test_vc_index test_lean_bridge test_server test_make_examples

echo "== node: selection logic =="
node test_selection.js

echo "== node: vox syntax mode tokens =="
node test_vox_mode.js

if [ -d /tmp/vox-pptr/node_modules/puppeteer-core ]; then
  echo "== node: in-browser smoke test =="
  node browser_test.js
else
  echo "== SKIP browser smoke test (puppeteer-core not installed at /tmp/vox-pptr) =="
  echo "   install: npm install --no-save --prefix /tmp/vox-pptr puppeteer-core"
fi

echo "ALL TEST LAYERS PASSED"
