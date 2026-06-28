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
MODE = os.environ.get("COMPILER_BENCH_MODE", "log")

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
        "compiler": Path(
            os.environ.get(
                "NATIVE_COMPILER",
                ROOT / "_native_install/bin/ocamlopt.opt")),
        "ocamllib": Path(
            os.environ.get(
                "NATIVE_OCAMLLIB",
                ROOT / "_native_install/lib/ocaml")),
        "log": Path(
            os.environ.get("NATIVE_LOG", ROOT / "_native_build/log")),
        "cwd": Path(
            os.environ.get("NATIVE_CWD", ROOT / "_native_build/main")),
    },
    "llvm": {
        "compiler": Path(
            os.environ.get(
                "LLVM_COMPILER",
                ROOT / "_llvm_self_stage_install/bin/ocamlopt.opt")),
        "real_compiler": Path(
            os.environ.get(
                "LLVM_REAL_COMPILER",
                ROOT / "_llvm_self_stage_install/bin/ocamlopt.opt.real")),
        "ocamllib": Path(
            os.environ.get(
                "LLVM_OCAMLLIB",
                ROOT / "_llvm_self_stage_install/lib/ocaml")),
        "log": Path(
            os.environ.get("LLVM_LOG", ROOT / "_llvm_self_stage_main_build/log")),
        "cwd": Path(
            os.environ.get("LLVM_CWD", ROOT / "_llvm_self_stage_main_build/main")),
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


def direct_include_args():
    build_main = ROOT / "_build/main"
    preferred = [
        build_main / ".ocamlcommon.objs/byte",
        build_main / ".ocamlcommon.objs/native",
        build_main / ".ocamloptcomp.objs/byte",
        build_main / ".ocamloptcomp.objs/native",
        build_main / ".ocamlbytecomp.objs/byte",
        build_main / ".oxcaml_common.objs/byte",
        build_main / ".ocamljcomp.objs/byte",
        build_main / "utils/.oxcaml_utils.objs/byte",
    ]
    cmi_dirs = sorted({path.parent for path in build_main.rglob("*.cmi")})
    dirs = []
    for directory in [*preferred, *cmi_dirs]:
        if directory.exists() and directory not in dirs:
            dirs.append(directory)
    args = []
    for directory in dirs:
        args.extend(["-I", str(directory)])
    return args, dirs


def direct_command(config, module):
    out_dir = TMP / config / module
    out_dir.mkdir(parents=True, exist_ok=True)
    source = ROOT / "_build/main" / f"{module}.ml"
    require_path(source)
    return [
        str(CONFIGS[config]["compiler"]),
        *EXTRA_OCAMLOPT_FLAGS,
        *DIRECT_INCLUDE_ARGS,
        "-c",
        str(source),
        "-o",
        str(out_dir / f"{module}.cmx"),
    ]


def run_timed(config, module, args):
    env = os.environ.copy()
    env["OCAMLLIB"] = str(CONFIGS[config]["ocamllib"])
    start = time.perf_counter()
    subprocess.run(
        args,
        cwd=CONFIGS[config]["cwd"] if MODE == "log" else ROOT,
        env=env,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        check=True,
    )
    return time.perf_counter() - start


DIRECT_INCLUDE_ARGS, DIRECT_INCLUDE_DIRS = direct_include_args()


def main():
    for config in CONFIGS.values():
        require_path(config["compiler"])
        require_path(config["ocamllib"])
        if MODE == "log":
            require_path(config["log"])
            require_path(config["cwd"])
        elif MODE != "direct":
            raise SystemExit("COMPILER_BENCH_MODE must be 'log' or 'direct'")
    if CONFIGS["native"]["compiler"].resolve() == CONFIGS["llvm"]["compiler"].resolve():
        raise SystemExit("native and llvm compiler paths resolve to the same file")
    if TMP.exists():
        shutil.rmtree(TMP)
    TMP.mkdir(parents=True)

    if MODE == "direct":
        commands = {
            config: {module: direct_command(config, module) for _, module in MODULES}
            for config in CONFIGS
        }
    else:
        commands = {
            config: {
                module: rewritten_command(
                    config, module, extract_command(CONFIGS[config]["log"], module))
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
        "mode": MODE,
        "direct_include_dirs": [
            str(path.relative_to(ROOT)) for path in DIRECT_INCLUDE_DIRS
        ] if MODE == "direct" else [],
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
