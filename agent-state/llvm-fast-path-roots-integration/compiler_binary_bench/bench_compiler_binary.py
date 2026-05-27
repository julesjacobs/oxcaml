#!/usr/bin/env python3
import argparse
import hashlib
import json
import math
import os
import re
import shlex
import statistics
import subprocess
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]

MODULES = [
    ("env.ml", "env"),
    ("ctype.ml", "ctype"),
    ("typecore.ml", "typecore"),
    ("translcore.ml", "translcore"),
    ("typemod.ml", "typemod"),
    ("cfg_to_linear.ml", "cfg_to_linear"),
    ("cfg_selectgen.ml", "cfg_selectgen"),
    ("llvmize.ml", "llvmize"),
    ("regalloc_irc.ml", "regalloc_irc"),
]


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def executable_info(path: Path) -> dict:
    stat = path.stat()
    payload = path.with_name(path.name + ".real")
    if not payload.exists():
        payload = path.resolve()
    payload_stat = payload.stat()
    return {
        "path": str(path),
        "realpath": str(path.resolve()),
        "size": stat.st_size,
        "sha256": sha256(path),
        "payload_path": str(payload),
        "payload_size": payload_stat.st_size,
        "payload_sha256": sha256(payload),
    }


def read_ocamlparam_from_log(log_path: Path) -> str | None:
    with log_path.open() as f:
        for line in f:
            if line.startswith("# OCAMLPARAM:"):
                return line.removeprefix("# OCAMLPARAM:").strip()
    return None


def validate_inputs(
    *,
    native_compiler: Path,
    llvm_compiler: Path,
    native_log: Path,
    llvm_log: Path,
    native_lib: Path,
    llvm_lib: Path,
) -> dict:
    for path in [
        native_compiler,
        llvm_compiler,
        native_log,
        llvm_log,
        native_lib / "stdlib.cmxa",
        llvm_lib / "stdlib.cmxa",
    ]:
        if not path.exists():
            raise RuntimeError(f"missing required benchmark input: {path}")

    native_info = executable_info(native_compiler)
    llvm_info = executable_info(llvm_compiler)
    if native_info["payload_path"] == llvm_info["payload_path"]:
        raise RuntimeError(
            "native and LLVM compiler paths resolve to the same executable: "
            f"{native_info['payload_path']}"
        )

    native_ocamlparam = read_ocamlparam_from_log(native_log)
    llvm_ocamlparam = read_ocamlparam_from_log(llvm_log)
    if native_ocamlparam is not None and "llvm-backend=1" in native_ocamlparam:
        raise RuntimeError(
            "native build log is contaminated with llvm-backend=1; rebuild "
            "the native comparison compiler with "
            "tools/build-clean-native-install.sh before benchmarking. "
            f"{native_log} has OCAMLPARAM: {native_ocamlparam}"
        )

    return {
        "native_compiler": native_info,
        "llvm_compiler": llvm_info,
        "native_build_log": str(native_log),
        "llvm_build_log": str(llvm_log),
        "native_build_log_ocamlparam": native_ocamlparam,
        "llvm_build_log_ocamlparam": llvm_ocamlparam,
    }


def extract_compile_args(log_path: Path, module: str) -> tuple[Path, list[str]]:
    pattern = re.compile(r"^\$ \(cd (?P<cwd>.+?) && (?P<cmd>.+)\)$")
    needle = f"/native/{module}.cmx"
    candidates: list[tuple[Path, list[str]]] = []
    with log_path.open() as f:
        for line in f:
            if needle not in line or " -c -impl " not in line:
                continue
            match = pattern.match(line.rstrip("\n"))
            if match is None:
                continue
            args = shlex.split(match.group("cmd"))
            if "-o" not in args:
                continue
            candidates.append((Path(match.group("cwd")), args))
    if not candidates:
        raise RuntimeError(f"could not find compile command for {module} in {log_path}")
    return candidates[-1]


def command_for(
    *,
    log_path: Path,
    compiler: Path,
    module: str,
    output_dir: Path,
) -> tuple[Path, list[str]]:
    cwd, args = extract_compile_args(log_path, module)
    args = list(args)
    args[0] = str(compiler)
    out_index = args.index("-o") + 1
    args[out_index] = str(output_dir / f"{module}.cmx")
    return cwd, args


def run_one(cwd: Path, args: list[str], ocamllib: Path) -> float:
    env = os.environ.copy()
    env["OCAMLLIB"] = str(ocamllib)
    env.pop("OCAMLPARAM", None)
    env.pop("OCAML_LLVM_HELPER_PROFILE", None)
    start = time.perf_counter()
    completed = subprocess.run(
        args,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    if completed.returncode != 0:
        command = " ".join(shlex.quote(arg) for arg in args)
        raise RuntimeError(
            f"command failed in {cwd}:\n{command}\n\n"
            f"stdout:\n{completed.stdout}\n\nstderr:\n{completed.stderr}"
        )
    return time.perf_counter() - start


def geomean(values: list[float]) -> float:
    return math.exp(sum(math.log(v) for v in values) / len(values))


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pairs", type=int, default=7)
    parser.add_argument("--out-dir", type=Path, required=True)
    parser.add_argument("--summary", type=Path, required=True)
    args = parser.parse_args()

    native_compiler = ROOT / "_install/bin/ocamlopt.opt"
    llvm_compiler = ROOT / "_llvm_self_stage_install/bin/ocamlopt.opt"
    native_log = ROOT / "_build/log"
    llvm_log = ROOT / "_llvm_self_stage_main_build/log"
    native_lib = ROOT / "_install/lib/ocaml"
    llvm_lib = ROOT / "_llvm_self_stage_install/lib/ocaml"
    input_info = validate_inputs(
        native_compiler=native_compiler,
        llvm_compiler=llvm_compiler,
        native_log=native_log,
        llvm_log=llvm_log,
        native_lib=native_lib,
        llvm_lib=llvm_lib,
    )

    out_dir = args.out_dir
    if not out_dir.is_absolute():
        out_dir = ROOT / out_dir
    summary_path = args.summary
    if not summary_path.is_absolute():
        summary_path = ROOT / summary_path

    out_dir.mkdir(parents=True, exist_ok=True)
    native_out = out_dir / "native"
    llvm_out = out_dir / "llvm"
    native_out.mkdir(exist_ok=True)
    llvm_out.mkdir(exist_ok=True)

    results = []
    for label, module in MODULES:
        native_cwd, native_cmd = command_for(
            log_path=native_log,
            compiler=native_compiler,
            module=module,
            output_dir=native_out,
        )
        llvm_cwd, llvm_cmd = command_for(
            log_path=llvm_log,
            compiler=llvm_compiler,
            module=module,
            output_dir=llvm_out,
        )

        native_times = []
        llvm_times = []
        for pair in range(args.pairs):
            native_time = run_one(native_cwd, native_cmd, native_lib)
            llvm_time = run_one(llvm_cwd, llvm_cmd, llvm_lib)
            native_times.append(native_time)
            llvm_times.append(llvm_time)
            ratio = llvm_time / native_time
            print(
                f"{label} pair {pair + 1}/{args.pairs}: "
                f"native={native_time:.4f}s llvm={llvm_time:.4f}s ratio={ratio:.3f}",
                flush=True,
            )

        native_median = statistics.median(native_times)
        llvm_median = statistics.median(llvm_times)
        result = {
            "file": label,
            "native_median": native_median,
            "llvm_median": llvm_median,
            "ratio": llvm_median / native_median,
            "native_times": native_times,
            "llvm_times": llvm_times,
        }
        results.append(result)

    ratios = [r["ratio"] for r in results]
    summary = {
        "pairs": args.pairs,
        "native_compiler": str(native_compiler),
        "llvm_compiler": str(llvm_compiler),
        "inputs": input_info,
        "results": results,
        "geomean_ratio": geomean(ratios),
        "median_ratio": statistics.median(ratios),
        "min_ratio": min(ratios),
        "max_ratio": max(ratios),
    }
    summary_path.parent.mkdir(parents=True, exist_ok=True)
    summary_path.write_text(json.dumps(summary, indent=2) + "\n")

    print("\nSummary:")
    print(f"geomean={summary['geomean_ratio']:.4f}")
    print(f"median={summary['median_ratio']:.4f}")
    print(f"min={summary['min_ratio']:.4f}")
    print(f"max={summary['max_ratio']:.4f}")
    for result in results:
        print(
            f"{result['file']}: native={result['native_median']:.4f}s "
            f"llvm={result['llvm_median']:.4f}s ratio={result['ratio']:.3f}"
        )


if __name__ == "__main__":
    main()
