#!/usr/bin/env python3
import json
import os
import random
import shlex
import shutil
import statistics
import subprocess
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
STATE = Path(__file__).resolve().parent
TMP = STATE / "compiler_bench_tmp"

REPETITIONS = int(os.environ.get("REPETITIONS", "5"))
EXTRA_OCAMLOPT_FLAGS = shlex.split(os.environ.get("EXTRA_OCAMLOPT_FLAGS", ""))

MODULES = [
    ("backend/cfg_selectgen.ml", "cfg_selectgen"),
    ("backend/llvm/llvmize.ml", "llvmize"),
    ("lambda/translcore.ml", "translcore"),
    ("typing/ctype.ml", "ctype"),
    ("typing/env.ml", "env"),
    ("typing/typecore.ml", "typecore"),
    ("typing/typemod.ml", "typemod"),
]


CONFIGS = {
    "native": {
        "compiler": ROOT / "_native_install/bin/ocamlopt.opt",
        "ocamllib": ROOT / "_native_install/lib/ocaml",
        "log": ROOT / "_native_build/log",
        "cwd": ROOT / "_native_build/main",
    },
    "llvm": {
        "compiler": ROOT / "_llvm_self_stage_install/bin/ocamlopt.opt",
        "real_compiler": ROOT / "_llvm_self_stage_install/bin/ocamlopt.opt.real",
        "ocamllib": ROOT / "_llvm_self_stage_install/lib/ocaml",
        "log": ROOT / "_llvm_self_stage_main_build/log",
        "cwd": ROOT / "_llvm_self_stage_main_build/main",
    },
}


def require_path(path):
    if not path.exists():
        raise SystemExit(f"missing required path: {path}")


def sha1(path):
    return subprocess.check_output(["shasum", str(path)], text=True).split()[0]


def extract_command(log_path, module):
    matches = []
    for line in log_path.read_text(errors="replace").splitlines():
        if not line.startswith("$ (cd "):
            continue
        if f" -c -impl {module}.ml" not in line:
            continue
        if f"{module}.cmx" not in line:
            continue
        if f"-cmi-file .ocamlcommon.objs/byte/{module}.cmi" in line:
            matches.append(line)
        elif module in {"cfg_selectgen", "llvmize"} and ".ocamloptcomp.objs" in line:
            matches.append(line)
    if len(matches) != 1:
        raise SystemExit(
            f"expected one compile command for {module} in {log_path}, got {len(matches)}"
        )
    line = matches[0]
    prefix = "$ (cd "
    mid = " && "
    if not line.startswith(prefix) or not line.endswith(")"):
        raise SystemExit(f"unexpected log command format: {line[:120]}")
    _, command = line[len(prefix) : -1].split(mid, 1)
    return shlex.split(command)


def rewritten_command(config, module, command):
    out_dir = TMP / config / module
    out_dir.mkdir(parents=True, exist_ok=True)
    out_cmx = out_dir / f"{module}.cmx"
    args = list(command)
    args[0] = str(CONFIGS[config]["compiler"])
    args[1:1] = EXTRA_OCAMLOPT_FLAGS
    for i, arg in enumerate(args):
        if arg == "-o":
            args[i + 1] = str(out_cmx)
            break
    else:
        raise SystemExit(f"no -o in command for {module}")
    return args


def run_timed(config, module, args):
    env = os.environ.copy()
    env["OCAMLLIB"] = str(CONFIGS[config]["ocamllib"])
    start = time.perf_counter()
    subprocess.run(
        args,
        cwd=CONFIGS[config]["cwd"],
        env=env,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        check=True,
    )
    return time.perf_counter() - start


def main():
    for config in CONFIGS.values():
        require_path(config["compiler"])
        require_path(config["ocamllib"])
        require_path(config["log"])
        require_path(config["cwd"])
    if CONFIGS["native"]["compiler"].resolve() == CONFIGS["llvm"]["compiler"].resolve():
        raise SystemExit("native and llvm compiler paths resolve to the same file")
    if TMP.exists():
        shutil.rmtree(TMP)
    TMP.mkdir(parents=True)

    commands = {
        config: {
            module: rewritten_command(config, module, extract_command(CONFIGS[config]["log"], module))
            for _, module in MODULES
        }
        for config in CONFIGS
    }

    modules = []
    native_round_totals = []
    llvm_round_totals = []
    samples = {
        module: {"native": [], "llvm": []}
        for _, module in MODULES
    }

    order = [(config, module) for _, module in MODULES for config in CONFIGS]
    for _ in range(REPETITIONS):
        random.shuffle(order)
        round_totals = {"native": 0.0, "llvm": 0.0}
        for config, module in order:
            elapsed = run_timed(config, module, commands[config][module])
            samples[module][config].append(elapsed)
            round_totals[config] += elapsed
        native_round_totals.append(round_totals["native"])
        llvm_round_totals.append(round_totals["llvm"])

    for source, module in MODULES:
        native_median = statistics.median(samples[module]["native"])
        llvm_median = statistics.median(samples[module]["llvm"])
        modules.append(
            {
                "source": source,
                "module": module,
                "native_median_s": native_median,
                "llvm_median_s": llvm_median,
                "llvm_over_native_ratio": llvm_median / native_median,
                "llvm_speedup_pct": (native_median / llvm_median - 1.0) * 100.0,
                "native_samples_s": samples[module]["native"],
                "llvm_samples_s": samples[module]["llvm"],
            }
        )

    native_sum = sum(m["native_median_s"] for m in modules)
    llvm_sum = sum(m["llvm_median_s"] for m in modules)
    native_round = statistics.median(native_round_totals)
    llvm_round = statistics.median(llvm_round_totals)
    result = {
        "timestamp": time.strftime("%Y%m%d_%H%M%S"),
        "cwd": str(ROOT),
        "native": {k: str(v) for k, v in CONFIGS["native"].items()},
        "llvm": {k: str(v) for k, v in CONFIGS["llvm"].items()},
        "native_sha1": sha1(CONFIGS["native"]["compiler"]),
        "llvm_wrapper_sha1": sha1(CONFIGS["llvm"]["compiler"]),
        "llvm_real_sha1": sha1(CONFIGS["llvm"]["real_compiler"]),
        "repetitions": REPETITIONS,
        "extra_ocamlopt_flags": EXTRA_OCAMLOPT_FLAGS,
        "modules": modules,
        "sum_of_module_medians": {
            "native_s": native_sum,
            "llvm_s": llvm_sum,
            "llvm_over_native_ratio": llvm_sum / native_sum,
            "llvm_speedup_pct": (native_sum / llvm_sum - 1.0) * 100.0,
        },
        "round_total_medians": {
            "native_s": native_round,
            "llvm_s": llvm_round,
            "llvm_over_native_ratio": llvm_round / native_round,
            "llvm_speedup_pct": (native_round / llvm_round - 1.0) * 100.0,
            "native_round_totals_s": native_round_totals,
            "llvm_round_totals_s": llvm_round_totals,
        },
    }

    out = STATE / f"compiler_bench_current_vs_native_{result['timestamp']}.json"
    out.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result["sum_of_module_medians"], indent=2))
    print(json.dumps(result["round_total_medians"], indent=2))
    print(out)


if __name__ == "__main__":
    main()
