#!/usr/bin/env python3
import hashlib
import html
import json
import math
import os
import random
import re
import shutil
import statistics
import subprocess
import time
import urllib.request
from pathlib import Path


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
SRC = HERE / "src"
BUILD = HERE / "build"
INSPECT = HERE / "inspect"

OCAMLOPT = Path(os.environ.get("OCAMLOPT", ROOT / "_install/bin/ocamlopt.opt"))
OCAMLLIB = Path(os.environ.get("OCAMLLIB", ROOT / "_install/lib/ocaml"))
LLVM_PATH = Path(os.environ.get("LLVM_PATH", ROOT.parent / "clang-wrapper"))
SAMPLES = int(os.environ.get("SAMPLES", "3"))
WARMUPS = int(os.environ.get("WARMUPS", "1"))

BASE_URL = "https://benchmarksgame-team.pages.debian.net/benchmarksgame/program"

CASES = {
    "binarytrees_5": {
        "page": f"{BASE_URL}/binarytrees-ocaml-5.html",
        "args": ["20"],
        "stdout": "capture",
    },
    "fannkuchredux_1": {
        "page": f"{BASE_URL}/fannkuchredux-ocaml-1.html",
        "args": ["11"],
        "stdout": "capture",
    },
    "fannkuchredux_2": {
        "page": f"{BASE_URL}/fannkuchredux-ocaml-2.html",
        "args": ["11"],
        "stdout": "capture",
    },
    "fasta_3": {
        "page": f"{BASE_URL}/fasta-ocaml-3.html",
        "args": ["2_000_000"],
        "stdout": "hash",
    },
    "fasta_6": {
        "page": f"{BASE_URL}/fasta-ocaml-6.html",
        "args": ["2_000_000"],
        "stdout": "hash",
    },
    "knucleotide_1": {
        "page": f"{BASE_URL}/knucleotide-ocaml-1.html",
        "args": [],
        "stdin": "fasta_input",
        "stdout": "capture",
    },
    "mandelbrot_6": {
        "page": f"{BASE_URL}/mandelbrot-ocaml-6.html",
        "args": ["5000"],
        "stdout": "hash",
    },
    "nbody_1": {
        "page": f"{BASE_URL}/nbody-ocaml-1.html",
        "args": ["20_000_000"],
        "stdout": "capture",
    },
    "regexredux_2": {
        "page": f"{BASE_URL}/regexredux-ocaml-2.html",
        "args": [],
        "stdin": "fasta_input",
        "stdout": "capture",
        "link_libs": [
            OCAMLLIB / "str/str.cmxa",
        ],
    },
    "revcomp_2": {
        "page": f"{BASE_URL}/revcomp-ocaml-2.html",
        "args": [],
        "stdin": "fasta_input",
        "stdout": "hash",
    },
    "spectralnorm_2": {
        "page": f"{BASE_URL}/spectralnorm-ocaml-2.html",
        "args": ["5000"],
        "stdout": "capture",
    },
}

STDIN_INPUTS = {
    "fasta_input": {
        "page": f"{BASE_URL}/fasta-ocaml-6.html",
        "args": ["500_000"],
        "path": HERE / "fasta_input_500000.fa",
    },
}

COMMON_FLAGS = [
    "-nostdlib",
    "-I",
    str(OCAMLLIB),
    "-I",
    str(OCAMLLIB / "str"),
    "-O3",
    "-unsafe",
    "-noassert",
    "-unbox-closures",
    "-w",
    "-32-34-38-69",
]


def run(cmd, *, cwd=None, env=None, stdout=None):
    start = time.perf_counter()
    subprocess.run(
        [str(arg) for arg in cmd],
        cwd=cwd,
        env=env,
        stdout=stdout,
        stderr=subprocess.STDOUT,
        check=True,
    )
    return time.perf_counter() - start


def compiler_env():
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)
    return env


def llvm_flags():
    return ["-llvm-backend", "-llvm-path", str(LLVM_PATH)]


def selected_cases():
    selected = os.environ.get("CASES", "")
    if not selected:
        return list(CASES)
    names = [name for name in selected.split(",") if name]
    missing = sorted(set(names) - set(CASES))
    if missing:
        raise SystemExit(f"unknown case(s): {', '.join(missing)}")
    return names


def extract_source(page_url):
    with urllib.request.urlopen(page_url) as response:
        data = response.read().decode("utf-8")
    match = re.search(r"<pre>(.*?)</pre>", data, flags=re.S)
    if not match:
        raise RuntimeError(f"no source <pre> block in {page_url}")
    text = re.sub(r"<[^>]+>", "", match.group(1))
    return html.unescape(text).strip() + "\n"


def fetch_sources(names):
    SRC.mkdir(parents=True, exist_ok=True)
    for name in names:
        source = extract_source(CASES[name]["page"])
        (SRC / f"{name}.ml").write_text(source)


def clean_case(name):
    for suffix in [".cmi", ".cmt", ".cmti", ".cmx", ".o", ".s", ".ll"]:
        path = SRC / f"{name}{suffix}"
        if path.exists():
            path.unlink()


def compile_exe(name, mode, extra_flags):
    clean_case(name)
    exe = BUILD / f"{name}.{mode}"
    cmd = [
        OCAMLOPT,
        *COMMON_FLAGS,
        *extra_flags,
        *CASES.get(name, {}).get("link_libs", []),
        SRC / f"{name}.ml",
        "-o",
        exe,
    ]
    elapsed = run(cmd, env=compiler_env())
    if (SRC / f"{name}.ll").exists():
        shutil.move(SRC / f"{name}.ll", INSPECT / f"{name}.{mode}.ll")
    return exe, elapsed


def compile_asm(name, mode, extra_flags):
    clean_case(name)
    cmd = [
        OCAMLOPT,
        *COMMON_FLAGS,
        "-S",
        "-c",
        "-keep-llvmir",
        *extra_flags,
        f"{name}.ml",
    ]
    run(cmd, cwd=SRC, env=compiler_env())
    shutil.move(SRC / f"{name}.s", INSPECT / f"{name}.{mode}.s")
    if (SRC / f"{name}.ll").exists():
        shutil.move(SRC / f"{name}.ll", INSPECT / f"{name}.{mode}.ll")


def compile_input_generator(input_name, cfg):
    name = f"input_{input_name}"
    ml = SRC / f"{name}.ml"
    ml.write_text(extract_source(cfg["page"]))
    exe, _ = compile_exe(name, "native", [])
    return exe


def ensure_stdin_inputs(names):
    required = sorted({
        CASES[name]["stdin"]
        for name in names
        if "stdin" in CASES[name]
    })
    inputs = {}
    for input_name in required:
        cfg = STDIN_INPUTS[input_name]
        path = cfg["path"]
        if not path.exists():
            exe = compile_input_generator(input_name, cfg)
            with path.open("wb") as out:
                subprocess.run(
                    [str(exe), *cfg["args"]],
                    stdout=out,
                    stderr=subprocess.STDOUT,
                    check=True,
                )
        inputs[input_name] = path
    return inputs


def time_exe(exe, args, stdout_mode, stdin_path=None):
    start = time.perf_counter()
    stdin = stdin_path.open("rb") if stdin_path is not None else None
    if stdout_mode == "hash":
        out = subprocess.check_output([str(exe), *args], stdin=stdin)
        digest = hashlib.sha256(out).hexdigest()
        observed = f"sha256:{digest}:bytes:{len(out)}"
    else:
        out = subprocess.check_output([str(exe), *args], stdin=stdin, text=True)
        observed = out.strip()
    if stdin is not None:
        stdin.close()
    return time.perf_counter() - start, observed


def geomean(xs):
    return math.exp(sum(math.log(x) for x in xs) / len(xs))


def main():
    if not OCAMLOPT.exists():
        raise SystemExit(f"missing compiler: {OCAMLOPT}")
    if not OCAMLLIB.exists():
        raise SystemExit(f"missing OCAMLLIB: {OCAMLLIB}")
    if not LLVM_PATH.exists():
        raise SystemExit(f"missing LLVM path/wrapper: {LLVM_PATH}")

    names = selected_cases()
    BUILD.mkdir(parents=True, exist_ok=True)
    INSPECT.mkdir(parents=True, exist_ok=True)
    fetch_sources(names)
    stdin_inputs = ensure_stdin_inputs(names)

    results = {}
    for name in names:
        cfg = CASES[name]
        native_exe, native_compile = compile_exe(name, "native", [])
        llvm_exe, llvm_compile = compile_exe(name, "llvm", llvm_flags())
        compile_asm(name, "native", [])
        compile_asm(name, "llvm", llvm_flags())

        samples = {"native": [], "llvm": []}
        outputs = {}
        exes = {"native": native_exe, "llvm": llvm_exe}
        stdin_path = stdin_inputs.get(cfg.get("stdin"))

        for mode, exe in exes.items():
            for _ in range(WARMUPS):
                _, out = time_exe(exe, cfg["args"], cfg["stdout"], stdin_path)
                outputs.setdefault(mode, out)
                if outputs[mode] != out:
                    raise SystemExit(f"{name}: unstable warmup output for {mode}")

        order = ["native", "llvm"]
        for i in range(SAMPLES):
            random.Random(i).shuffle(order)
            for mode in order:
                elapsed, out = time_exe(
                    exes[mode], cfg["args"], cfg["stdout"], stdin_path)
                outputs.setdefault(mode, out)
                if outputs[mode] != out:
                    raise SystemExit(f"{name}: unstable output for {mode}")
                samples[mode].append(elapsed)

        if outputs["native"] != outputs["llvm"]:
            raise SystemExit(
                f"{name}: output mismatch native={outputs['native']!r} "
                f"llvm={outputs['llvm']!r}")

        med = {mode: statistics.median(values) for mode, values in samples.items()}
        ratio = med["llvm"] / med["native"]
        results[name] = {
            "source_page": cfg["page"],
            "args": cfg["args"],
            "compile_seconds": {
                "native": native_compile,
                "llvm": llvm_compile,
                "llvm_over_native": llvm_compile / native_compile,
            },
            "run_median_seconds": med,
            "run_ratio_llvm_over_native": ratio,
            "run_llvm_speedup_percent": (med["native"] / med["llvm"] - 1) * 100,
            "samples": samples,
            "output": outputs["native"],
        }
        print(
            f"{name}: native={med['native']:.4f}s llvm={med['llvm']:.4f}s "
            f"ratio={ratio:.4f}",
            flush=True,
        )

    ratios = [results[name]["run_ratio_llvm_over_native"] for name in names]
    total_native = sum(results[name]["run_median_seconds"]["native"] for name in names)
    total_llvm = sum(results[name]["run_median_seconds"]["llvm"] for name in names)
    results["_aggregate"] = {
        "cases": len(names),
        "total_run_seconds": {"native": total_native, "llvm": total_llvm},
        "total_run_ratio_llvm_over_native": total_llvm / total_native,
        "total_run_llvm_speedup_percent": (total_native / total_llvm - 1) * 100,
        "geomean_run_ratio_llvm_over_native": geomean(ratios),
        "median_run_ratio_llvm_over_native": statistics.median(ratios),
        "max_run_ratio_llvm_over_native": max(ratios),
        "min_run_ratio_llvm_over_native": min(ratios),
    }
    results_path = HERE / "results.json"
    results_path.write_text(json.dumps(results, indent=2) + "\n")
    print(f"RESULTS_JSON={results_path}")


if __name__ == "__main__":
    main()
