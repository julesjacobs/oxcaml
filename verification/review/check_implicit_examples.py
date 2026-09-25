#!/usr/bin/env python3
"""Run migrated examples with an existing compiler/test installation, read-only."""
import argparse
import json
import os
from pathlib import Path
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("compiler_root", type=Path)
parser.add_argument("tests", nargs="*")
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
compiler = args.compiler_root.resolve()
prefix = compiler / "_install"
inventory = json.loads((root / "verification/review/implicit-examples-inventory.json").read_text())
names = args.tests or inventory["ocamltest_roots"]
output = root / "_build/implicit-examples"
output.mkdir(parents=True, exist_ok=True)
env = os.environ.copy()
env.update(
    OCAMLSRCDIR=str(compiler / "_runtest"),
    OCAMLTESTDIR=str(output / "tests"),
    OCAMLTEST_EXPECT=str(compiler / "_build/main/oxcaml/testsuite/tools/expect.exe"),
    OCAMLTEST_EXPECTNAT=str(compiler / "_build/main/oxcaml/testsuite/tools/expectnat.exe"),
    CAML_LD_LIBRARY_PATH=str(prefix / "lib/ocaml/stublibs"),
    TERM="dumb",
)
for name in ("OCAMLC_BYTE", "OCAMLC_OPT", "OCAMLOPT_BYTE", "OCAMLOPT_OPT"):
    executable = "ocamlopt" if name.startswith("OCAMLOPT") else "ocamlc"
    env["OCAMLTEST_" + name] = str(prefix / "bin" / executable)
results = []
for name in names:
    source = root / "testsuite/tests/vox" / (name + ".ml")
    result = subprocess.run(
        [str(compiler / "_runtest/ocamltest/ocamltest"), str(source)],
        env=env, capture_output=True, text=True,
    )
    log = result.stdout + result.stderr
    (output / (name + ".log")).write_text(log)
    passed = result.returncode == 0 and "=> failed" not in log and "=> passed" in log
    results.append({"test": name, "passed": passed})
    print(name, "passed" if passed else "FAILED", flush=True)
(output / "results.json").write_text(json.dumps(results, indent=2) + "\n")
sys.exit(0 if all(row["passed"] for row in results) else 1)
