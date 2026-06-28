#!/usr/bin/env python3
import json
import math
import os
import random
import shutil
import statistics
import subprocess
import time
from pathlib import Path


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
SOURCE_ROOT = ROOT / "external/js_of_ocaml/benchmarks/sources/ml"
LOCAL_SOURCE_ROOT = HERE / "local_src"
SRC = HERE / "src"
BUILD = HERE / "build"
INSPECT = HERE / "inspect"

OCAMLOPT = Path(os.environ.get("OCAMLOPT", ROOT / "_install/bin/ocamlopt.opt"))
OCAMLLIB = Path(os.environ.get("OCAMLLIB", ROOT / "_install/lib/ocaml"))
LLVM_PATH = Path(os.environ.get("LLVM_PATH", ROOT.parent / "clang-wrapper"))
LLVM_EXTRA_FLAGS = os.environ.get("LLVM_EXTRA_FLAGS", "")
SAMPLES = int(os.environ.get("SAMPLES", "3"))
WARMUPS = int(os.environ.get("WARMUPS", "1"))
UPSTREAM_COMPAT_DIR = OCAMLLIB / "stdlib_upstream_compatible"
UPSTREAM_COMPAT_LIB = UPSTREAM_COMPAT_DIR / "stdlib_upstream_compatible.cmxa"

CASES = [
    "almabench",
    "bdd",
    "binary_trees",
    "boyer",
    "boyer_no_exc",
    "fannkuch_redux",
    "fannkuch_redux_2",
    "fft",
    "hamming",
    "kb",
    "kb_no_exc",
    "nucleic",
    "quicksort",
    "raytrace",
    "soli",
    "splay",
]

LOCAL_CASES = [
    "adv_bit_mix_hash",
    "adv_complex_mul",
    "adv_float_poly",
    "hash_batch_murmur_mix",
    "hash_batch_xxhash_mix",
    "hash_fnv_bytes",
    "hash_poly_rolling",
    "hash_stdlib_string",
    "hash_stdlib_string_ocaml_mix",
    "hash_stdlib_string_ocaml_mix_inline",
    "hash_stdlib_string_ocaml_mix_int32u",
    "hash_stdlib_string_ocaml_mix_int64u",
    "hash_stdlib_string_ocaml_mix_int64u_odd",
    "hash_stdlib_string_ocaml_mix_param_int64u",
    "hash_stdlib_string_ocaml_mix_param1_int64u",
    "hash_stdlib_string_ocaml_mix_param2_int64u",
    "hash_stdlib_string_ocaml_mix_param4_int64u",
    "hash_stdlib_string_ocaml_mix_get64u_param4_int64u",
    "hash_stdlib_string_ocaml_mix_lift4_int64u",
    "hash_stdlib_string_ocaml_mix_get64u_lift4_int64u",
    "hash_stdlib_string_vecadd_param_int64u",
    "hash_stdlib_string_vecadd_get64u_param_int64u",
    "hash_stdlib_string_vecxor_param_int64u",
    "hash_stdlib_string_vecxor_get64u_param_int64u",
    "adv_int_recurrence",
    "adv_stencil_1d",
    "finance_black_scholes",
    "finance_covariance_risk",
    "finance_greeks_pnl",
    "numeric_float_dot",
    "numeric_float_dot_hof",
    "numeric_float_map3_hof",
    "numeric_int_mix",
    "matmul",
    "matmul_transposed",
    "xxhash",
]

ARGS = {
    # Keep the upstream default work, but make it explicit in results.
    "bdd": ["22", "100"],
}

COMMON_FLAGS = [
    "-nostdlib",
    "-I",
    str(OCAMLLIB),
    "-I",
    str(OCAMLLIB / "compiler-libs"),
    "-I",
    str(UPSTREAM_COMPAT_DIR),
    "-O3",
    "-unbox-closures",
    "-w",
    "-32-34-38-69",
]

EXTRA_LINK_LIBS = {
    "hash_stdlib_string_ocaml_mix_int32u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_int64u_odd": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_param_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_param1_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_param2_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_param4_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_get64u_param4_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_lift4_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_ocaml_mix_get64u_lift4_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_vecadd_param_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_vecadd_get64u_param_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_vecxor_param_int64u": [UPSTREAM_COMPAT_LIB],
    "hash_stdlib_string_vecxor_get64u_param_int64u": [UPSTREAM_COMPAT_LIB],
    "xxhash": [UPSTREAM_COMPAT_LIB],
}


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
    flags = ["-llvm-backend", "-llvm-path", str(LLVM_PATH)]
    if LLVM_EXTRA_FLAGS:
        flags += ["-llvm-flags", LLVM_EXTRA_FLAGS]
    return flags


def selected_cases():
    selected = os.environ.get("CASES", "")
    if not selected:
        return CASES + LOCAL_CASES
    names = [name for name in selected.split(",") if name]
    missing = sorted(set(names) - set(CASES) - set(LOCAL_CASES))
    if missing:
        raise SystemExit(f"unknown case(s): {', '.join(missing)}")
    return names


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
        *EXTRA_LINK_LIBS.get(name, []),
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
    asm = INSPECT / f"{name}.{mode}.s"
    shutil.move(SRC / f"{name}.s", asm)
    if (SRC / f"{name}.ll").exists():
        shutil.move(SRC / f"{name}.ll", INSPECT / f"{name}.{mode}.ll")
    return asm


def count_asm(path):
    text = path.read_text(errors="replace")
    return {
        "lines": text.count("\n") + 1,
        "branches": text.count("\tb.") + text.count("\tb\t"),
        "calls": text.count("\tbl\t") + text.count("\tblr\t"),
        "loads": text.count("\tldr\t") + text.count("\tldp\t"),
        "stores": text.count("\tstr\t") + text.count("\tstp\t"),
    }


def time_exe(exe, args):
    start = time.perf_counter()
    out = subprocess.check_output([str(exe), *args], text=True).strip()
    return time.perf_counter() - start, out


def main():
    if not OCAMLOPT.exists():
        raise SystemExit(f"missing compiler: {OCAMLOPT}")
    if not OCAMLLIB.exists():
        raise SystemExit(f"missing OCAMLLIB: {OCAMLLIB}")
    if not LLVM_PATH.exists():
        raise SystemExit(f"missing LLVM path/wrapper: {LLVM_PATH}")

    SRC.mkdir(parents=True, exist_ok=True)
    BUILD.mkdir(parents=True, exist_ok=True)
    INSPECT.mkdir(parents=True, exist_ok=True)

    names = selected_cases()
    for name in names:
        source_root = LOCAL_SOURCE_ROOT if name in LOCAL_CASES else SOURCE_ROOT
        shutil.copyfile(source_root / f"{name}.ml", SRC / f"{name}.ml")

    results = {}
    for name in names:
        native_exe, native_compile = compile_exe(name, "native", [])
        llvm_exe, llvm_compile = compile_exe(name, "llvm", llvm_flags())
        native_asm = compile_asm(name, "native", [])
        llvm_asm = compile_asm(name, "llvm", llvm_flags())

        args = ARGS.get(name, [])
        samples = {"native": [], "llvm": []}
        outputs = {}
        for mode, exe in [("native", native_exe), ("llvm", llvm_exe)]:
            for _ in range(WARMUPS):
                _, out = time_exe(exe, args)
                previous = outputs.setdefault(mode, out)
                if previous != out:
                    raise SystemExit(
                        f"{name}: unstable warmup output for {mode}")

        order = ["native", "llvm"]
        exes = {"native": native_exe, "llvm": llvm_exe}
        for i in range(SAMPLES):
            random.Random(i).shuffle(order)
            for mode in order:
                elapsed, out = time_exe(exes[mode], args)
                outputs.setdefault(mode, out)
                if outputs[mode] != out:
                    raise SystemExit(f"{name}: unstable {mode} output")
                samples[mode].append(elapsed)

        if outputs["native"] != outputs["llvm"]:
            raise SystemExit(
                f"{name}: output mismatch native={outputs['native']!r} "
                f"llvm={outputs['llvm']!r}")

        med = {mode: statistics.median(values) for mode, values in samples.items()}
        ratio = med["llvm"] / med["native"]
        results[name] = {
            "args": args,
            "compile_seconds": {
                "native": native_compile,
                "llvm": llvm_compile,
                "llvm_over_native": llvm_compile / native_compile,
            },
            "run_median_seconds": med,
            "run_ratio_llvm_over_native": ratio,
            "samples": samples,
            "output": outputs["native"],
            "asm": {
                "native": count_asm(native_asm),
                "llvm": count_asm(llvm_asm),
            },
        }
        print(
            f"{name}: native={med['native']:.4f}s llvm={med['llvm']:.4f}s "
            f"ratio={ratio:.4f} compile_ratio="
            f"{results[name]['compile_seconds']['llvm_over_native']:.4f}",
            flush=True,
        )

    ratios = [results[name]["run_ratio_llvm_over_native"] for name in names]
    compile_ratios = [
        results[name]["compile_seconds"]["llvm_over_native"] for name in names
    ]
    total_native = sum(
        results[name]["run_median_seconds"]["native"] for name in names)
    total_llvm = sum(
        results[name]["run_median_seconds"]["llvm"] for name in names)
    total_compile_native = sum(
        results[name]["compile_seconds"]["native"] for name in names)
    total_compile_llvm = sum(
        results[name]["compile_seconds"]["llvm"] for name in names)
    results["_aggregate"] = {
        "cases": len(names),
        "total_run_seconds": {
            "native": total_native,
            "llvm": total_llvm,
        },
        "total_run_ratio_llvm_over_native": total_llvm / total_native,
        "total_run_llvm_speedup_percent": (total_native / total_llvm - 1) * 100,
        "geomean_run_ratio_llvm_over_native": math.exp(
            sum(math.log(ratio) for ratio in ratios) / len(ratios)),
        "median_run_ratio_llvm_over_native": statistics.median(ratios),
        "max_run_ratio_llvm_over_native": max(ratios),
        "min_run_ratio_llvm_over_native": min(ratios),
        "total_compile_seconds": {
            "native": total_compile_native,
            "llvm": total_compile_llvm,
        },
        "total_compile_ratio_llvm_over_native":
            total_compile_llvm / total_compile_native,
        "geomean_compile_ratio_llvm_over_native": math.exp(
            sum(math.log(ratio) for ratio in compile_ratios)
            / len(compile_ratios)),
    }
    (HERE / "results.json").write_text(json.dumps(results, indent=2) + "\n")
    print(f"RESULTS_JSON={HERE / 'results.json'}")


if __name__ == "__main__":
    main()
