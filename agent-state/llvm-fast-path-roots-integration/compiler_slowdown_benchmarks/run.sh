#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
BUILD="$SCRIPT_DIR/.build"
OCAMLOPT="${OCAMLOPT:-$ROOT/_install/bin/ocamlopt.opt}"
OCAMLLIB="${OCAMLLIB:-$ROOT/_install/lib/ocaml}"
LLVM_PATH="${LLVM_PATH:-}"
PAIRS="${PAIRS:-5}"

if [[ -z "$LLVM_PATH" ]]; then
  echo "Set LLVM_PATH to the branch-local clang wrapper, e.g." >&2
  echo "  export LLVM_PATH=/tmp/oxcaml-agent-llvm-fast-path-roots-integration/clang-wrapper" >&2
  exit 2
fi

mkdir -p "$BUILD"

compile_case() {
  local case="$1"
  local mode="$2"
  shift 2
  "$OCAMLOPT" \
    -nostdlib -I "$OCAMLLIB" -I "$OCAMLLIB/compiler-libs" \
    -O3 -unbox-closures "$@" \
    "$SCRIPT_DIR/$case.ml" -o "$BUILD/$case.$mode"
}

export BENCH_BUILD="$BUILD"
export BENCH_PAIRS="$PAIRS"
export BENCH_SCRIPT_DIR="$SCRIPT_DIR"
export BENCH_OCAMLOPT="$OCAMLOPT"
export BENCH_OCAMLLIB="$OCAMLLIB"
export BENCH_LLVM_PATH="$LLVM_PATH"
export BENCH_CASES="${CASES:-}"
python3 - <<'PY'
import json
import os
import statistics
import subprocess
import time
from pathlib import Path

build = Path(os.environ["BENCH_BUILD"])
script_dir = Path(os.environ["BENCH_SCRIPT_DIR"])
ocamlopt = Path(os.environ["BENCH_OCAMLOPT"])
ocamllib = Path(os.environ["BENCH_OCAMLLIB"])
llvm_path = os.environ["BENCH_LLVM_PATH"]
pairs = int(os.environ["BENCH_PAIRS"])

cases = {
    "try_lookup_hit": (1_000_000, 30),
    "try_lookup_miss": (500_000, 30),
    "no_try_lookup_hit": (1_000_000, 30),
    "string_tree_find": (1_000_000, 40),
    "string_compare_loop": (2_000_000, 40),
    "string_map_find": (1_000_000, 40),
}

selected = os.environ["BENCH_CASES"]
if selected:
    wanted = selected.split(",")
    cases = {name: cases[name] for name in wanted}

def compile_case(name, mode, extra_flags):
    cmd = [
        str(ocamlopt),
        "-nostdlib",
        "-I",
        str(ocamllib),
        "-I",
        str(ocamllib / "compiler-libs"),
        "-O3",
        "-unbox-closures",
        *extra_flags,
        str(script_dir / f"{name}.ml"),
        "-o",
        str(build / f"{name}.{mode}"),
    ]
    subprocess.run(cmd, check=True)

results = {}
for name, (n, reps) in cases.items():
    compile_case(name, "native", [])
    compile_case(name, "llvm", ["-llvm-backend", "-llvm-path", llvm_path])
    exes = {
        "native": build / f"{name}.native",
        "llvm": build / f"{name}.llvm",
    }
    outputs = {
        mode: subprocess.check_output(
            [str(exe), str(n), str(reps)], text=True
        ).strip()
        for mode, exe in exes.items()
    }
    if outputs["native"] != outputs["llvm"]:
        raise SystemExit(f"{name}: output mismatch: {outputs}")

    samples = {"native": [], "llvm": []}
    for i in range(pairs):
        order = ["native", "llvm"] if i % 2 == 0 else ["llvm", "native"]
        for mode in order:
            start = time.perf_counter()
            subprocess.run(
                [str(exes[mode]), str(n), str(reps)],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=True,
            )
            samples[mode].append(time.perf_counter() - start)

    med = {mode: statistics.median(vals) for mode, vals in samples.items()}
    ratio = med["llvm"] / med["native"]
    results[name] = {
        "n": n,
        "reps": reps,
        "output": outputs["native"],
        "samples": samples,
        "median": med,
        "ratio_llvm_over_native": ratio,
    }
    print(
        f"{name}: native={med['native']:.4f}s "
        f"llvm={med['llvm']:.4f}s ratio={ratio:.4f}",
        flush=True,
    )

out = build / "summary.json"
out.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")
print(f"SUMMARY_JSON={out}")
PY
