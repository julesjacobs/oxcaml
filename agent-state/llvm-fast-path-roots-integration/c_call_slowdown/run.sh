#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
BUILD="$SCRIPT_DIR/.build"
INSPECT="$SCRIPT_DIR/inspect"
OCAMLOPT="${OCAMLOPT:-$ROOT/_install/bin/ocamlopt.opt}"
OCAMLLIB="${OCAMLLIB:-$ROOT/_install/lib/ocaml}"
LLVM_PATH="${LLVM_PATH:-}"
PAIRS="${PAIRS:-5}"

if [[ -z "$LLVM_PATH" ]]; then
  echo "Set LLVM_PATH to the branch-local clang wrapper." >&2
  exit 2
fi

mkdir -p "$BUILD" "$INSPECT"

export BENCH_BUILD="$BUILD"
export BENCH_INSPECT="$INSPECT"
export BENCH_SCRIPT_DIR="$SCRIPT_DIR"
export BENCH_OCAMLOPT="$OCAMLOPT"
export BENCH_OCAMLLIB="$OCAMLLIB"
export BENCH_LLVM_PATH="$LLVM_PATH"
export BENCH_PAIRS="$PAIRS"
export BENCH_CASES="${CASES:-}"

python3 - <<'PY'
import json
import os
import re
import statistics
import subprocess
import time
from pathlib import Path

build = Path(os.environ["BENCH_BUILD"])
inspect = Path(os.environ["BENCH_INSPECT"])
script_dir = Path(os.environ["BENCH_SCRIPT_DIR"])
ocamlopt = Path(os.environ["BENCH_OCAMLOPT"])
ocamllib = Path(os.environ["BENCH_OCAMLLIB"])
llvm_path = os.environ["BENCH_LLVM_PATH"]
pairs = int(os.environ["BENCH_PAIRS"])

cases = {
    "noalloc_add1_loop": (5_000_000, 40),
    "noalloc_sum10_loop": (2_000_000, 40),
    "obj_tag_loop": (3_000_000, 40),
    "string_equal_loop": (2_000_000, 40),
    "string_compare_loop": (2_000_000, 40),
    "int_hash_loop": (2_000_000, 20),
    "float_sin_loop": (1_000_000, 20),
    "int_of_string_loop": (500_000, 20),
}

c_sources = {
    "noalloc_add1_loop": [script_dir / "bench_stubs.c"],
    "noalloc_sum10_loop": [script_dir / "bench_stubs.c"],
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
        *map(str, c_sources.get(name, [])),
        str(script_dir / f"{name}.ml"),
        "-o",
        str(build / f"{name}.{mode}"),
    ]
    subprocess.run(cmd, check=True)

def compile_assembly(name, mode, extra_flags):
    out = inspect / f"{name}.{mode}.s"
    cmd = [
        str(ocamlopt),
        "-nostdlib",
        "-I",
        str(ocamllib),
        "-I",
        str(ocamllib / "compiler-libs"),
        "-O3",
        "-unbox-closures",
        "-c",
        "-S",
        *extra_flags,
        str(script_dir / f"{name}.ml"),
    ]
    subprocess.run(cmd, cwd=inspect, check=True)
    produced = script_dir / f"{name}.s"
    produced.replace(out)
    for base in [inspect, script_dir]:
        for suffix in [".cmx", ".cmi", ".o"]:
            path = base / f"{name}{suffix}"
            if path.exists():
                path.unlink()
    return out

def count_asm(path):
    text = path.read_text(errors="replace")
    return {
        "lines": text.count("\n") + 1,
        "wrapper_refs": len(re.findall(r"c_call_wrapper", text)),
        "caml_c_call_refs": len(re.findall(r"caml_c_call", text)),
        "direct_runtime_refs": len(
            re.findall(
                r"caml_(string_compare|string_equal|bytes_equal|obj_tag|hash_exn|int_of_string)",
                text,
            )
        ),
    }

results = {}
for name, (n, reps) in cases.items():
    compile_case(name, "native", [])
    compile_case(name, "llvm", ["-llvm-backend", "-llvm-path", llvm_path])
    native_s = compile_assembly(name, "native", [])
    llvm_s = compile_assembly(name, "llvm", ["-llvm-backend", "-llvm-path", llvm_path])

    exes = {"native": build / f"{name}.native", "llvm": build / f"{name}.llvm"}
    outputs = {
        mode: subprocess.check_output([str(exe), str(n), str(reps)], text=True).strip()
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
        "asm": {
            "native": count_asm(native_s),
            "llvm": count_asm(llvm_s),
        },
    }
    print(
        f"{name}: native={med['native']:.4f}s "
        f"llvm={med['llvm']:.4f}s ratio={ratio:.4f} "
        f"wrappers={results[name]['asm']['llvm']['wrapper_refs']}",
        flush=True,
    )

out = build / "summary.json"
out.write_text(json.dumps(results, indent=2, sort_keys=True) + "\n")
print(f"SUMMARY_JSON={out}")
PY
