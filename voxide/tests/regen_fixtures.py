#!/usr/bin/env python3
"""Regenerate the committed proof-pane fixtures from the real compiler.

Each tests/fixtures/<name>.vcs.json is exactly the /vcs payload the server
returns for examples/<name>.ml (compiler.vcs_for_source); xmod.workspace.json is
the /workspace-check payload for the Lib.ml + Client.ml pair (active Client.ml).

Run with the display-fixed compiler so the fixtures carry source-like predicate
display (no raw app[]/constructor[] leaks):

  VOX2_OCAMLC=.../ocamlc.opt TMPDIR=/usr/local/home/jujacobs/tmp \\
    python3 tests/regen_fixtures.py [--check]

--check regenerates in memory and reports which fixtures WOULD change (byte
diff), writing nothing.  Without it, the fixtures are overwritten in place.
"""

import json
import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT))
import compiler  # noqa: E402  # pyright: ignore[reportImplicitRelativeImport]

EXAMPLES = ROOT / "examples"
FIXTURES = ROOT / "tests" / "fixtures"

# The single-buffer fixtures (name -> examples/<name>.ml -> <name>.vcs.json).
SINGLES = [
    "abs",
    "binder",
    "counterexample",
    "dependent",
    "guard",
    "multi_arg",
    "multi_param",
    "nested_call",
    "overview",
    "predicate_forms",
    "proof_tour",
    "recursion",
    "unproved",
]

# The multi-file workspace fixture (order-independent; active = Client.ml).
WS_FIXTURE = "xmod.workspace.json"
WS_FILES = ["Lib.ml", "Client.ml"]
WS_ACTIVE = "Client.ml"


def ocamlc():
    oc = os.environ.get("VOX2_OCAMLC")
    if not oc:
        sys.exit("set VOX2_OCAMLC to a vox2 ocamlc.opt")
    return oc


def dumps(payload):
    # Match the committed formatting exactly: default separators, ASCII-escaped
    # (the generated Lean carries a unicode >=), one trailing newline.
    return json.dumps(payload) + "\n"


def build_single(name, oc):
    source = (EXAMPLES / f"{name}.ml").read_text()
    return dumps(compiler.vcs_for_source(source, 1, oc))


def build_workspace(oc):
    files = [{"name": n, "source": (EXAMPLES / n).read_text()} for n in WS_FILES]
    return dumps(compiler.check_workspace(files, WS_ACTIVE, 1, oc))


def main():
    check = "--check" in sys.argv[1:]
    oc = ocamlc()
    changed = []
    targets = [(f"{n}.vcs.json", build_single(n, oc)) for n in SINGLES]
    targets.append((WS_FIXTURE, build_workspace(oc)))
    for fixture, text in targets:
        path = FIXTURES / fixture
        old = path.read_text() if path.is_file() else None
        if old == text:
            print(f"  unchanged  {fixture}")
            continue
        changed.append(fixture)
        if check:
            print(f"  WOULD CHANGE  {fixture}")
        else:
            path.write_text(text)
            print(f"  wrote  {fixture}")
    print(
        f"\n{len(changed)} of {len(targets)} fixtures {'differ' if check else 'rewritten'}"
    )


if __name__ == "__main__":
    main()
