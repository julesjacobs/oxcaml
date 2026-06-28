#!/usr/bin/env python3
import json
import os
import shutil
import statistics
import subprocess
import sys
import textwrap
import time
from pathlib import Path


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
SRC = HERE / "src"
BUILD = HERE / "build"
INSPECT = HERE / "inspect"

OCAMLOPT = Path(os.environ.get("OCAMLOPT", ROOT / "_install/bin/ocamlopt.opt"))
LLVM_PATH = Path(os.environ.get("LLVM_PATH", ROOT.parent / "clang-wrapper"))
OCAMLLIB = Path(os.environ.get("OCAMLLIB", ROOT / "_install/lib/ocaml"))

SAMPLES = int(os.environ.get("SAMPLES", "9"))
N = int(os.environ.get("N", "12000000"))
REPS = int(os.environ.get("REPS", "5"))


COMMON = r"""
let[@inline never] tick i =
  Sys.opaque_identity (i + 1)

let print_result x =
  print_int x;
  print_newline ()
"""


CASES = {
    "loop_invariant_int_across_call": r"""
let rec loop x i acc =
  if i <= 0 then acc
  else
    let y = tick i in
    loop x (i - 1) (acc + x + y)

let run n reps =
  let x = String.length (Sys.opaque_identity "loop_invariant_payload") in
  let acc = ref 0 in
  for _ = 1 to reps do
    acc := !acc + loop x n 0
  done;
  !acc

let () = print_result (run %d %d)
""" % (N, REPS),
    "loop_invariant_gc_across_call": r"""
let rec loop x i acc =
  if i <= 0 then acc
  else
    let y = tick i in
    loop x (i - 1) (acc + String.length x + y)

let run n reps =
  let x = Sys.opaque_identity "loop_invariant_payload" in
  let acc = ref 0 in
  for _ = 1 to reps do
    acc := !acc + loop x n 0
  done;
  !acc

let () = print_result (run %d %d)
""" % (N, REPS),
}


def run(cmd, *, cwd=None, env=None, stdout=None):
    subprocess.run(cmd, cwd=cwd, env=env, stdout=stdout, stderr=subprocess.STDOUT,
                   check=True)


def llvm_env():
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)
    env["OCAMLPARAM"] = f"_,llvm-backend=1,llvm-path={LLVM_PATH}"
    return env


def native_env():
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)
    env.pop("OCAMLPARAM", None)
    return env


def clean_case(name):
    for suffix in [".cmi", ".cmt", ".cmti", ".cmx", ".o", ".s", ".ll"]:
        path = SRC / f"{name}{suffix}"
        if path.exists():
            path.unlink()


def compile_case(name, mode):
    exe = BUILD / f"{name}.{mode}"
    clean_case(name)
    env = llvm_env() if mode == "llvm" else native_env()
    flags = ["-O3", "-unbox-closures"]
    if mode == "llvm":
        flags.append("-keep-llvmir")
    cmd = [str(OCAMLOPT), *flags, str(SRC / f"{name}.ml"), "-o", str(exe)]
    run(cmd, env=env)
    if mode == "llvm" and (SRC / f"{name}.ll").exists():
        shutil.move(SRC / f"{name}.ll", INSPECT / f"{name}.llvm.ll")
    return exe


def compile_asm(name, mode):
    clean_case(name)
    env = llvm_env() if mode == "llvm" else native_env()
    flags = ["-O3", "-unbox-closures", "-S", "-c", "-dasm-comments"]
    if mode == "llvm":
        flags.append("-keep-llvmir")
    cmd = [str(OCAMLOPT), *flags, f"{name}.ml"]
    run(cmd, cwd=SRC, env=env)
    shutil.move(SRC / f"{name}.s", INSPECT / f"{name}.{mode}.s")
    if mode == "llvm" and (SRC / f"{name}.ll").exists():
        shutil.move(SRC / f"{name}.ll", INSPECT / f"{name}.asm.llvm.ll")


def time_exe(exe):
    start = time.perf_counter()
    out = subprocess.check_output([str(exe)], text=True).strip()
    elapsed = time.perf_counter() - start
    return elapsed, out


def main():
    if not OCAMLOPT.exists():
        raise SystemExit(f"missing compiler: {OCAMLOPT}")
    if not LLVM_PATH.exists():
        raise SystemExit(f"missing LLVM path/wrapper: {LLVM_PATH}")
    SRC.mkdir(parents=True, exist_ok=True)
    BUILD.mkdir(parents=True, exist_ok=True)
    INSPECT.mkdir(parents=True, exist_ok=True)

    results = {}
    for name, source in CASES.items():
        (SRC / f"{name}.ml").write_text(
            textwrap.dedent(COMMON + "\n" + source).strip() + "\n")
        native_exe = compile_case(name, "native")
        llvm_exe = compile_case(name, "llvm")
        compile_asm(name, "native")
        compile_asm(name, "llvm")

        samples = {"native": [], "llvm": []}
        outputs = {}
        for mode, exe in [("native", native_exe), ("llvm", llvm_exe)]:
            for _ in range(SAMPLES):
                elapsed, out = time_exe(exe)
                samples[mode].append(elapsed)
                outputs.setdefault(mode, out)
                if outputs[mode] != out:
                    raise SystemExit(f"{name}: unstable {mode} output")
        if outputs["native"] != outputs["llvm"]:
            raise SystemExit(
                f"{name}: output mismatch native={outputs['native']} "
                f"llvm={outputs['llvm']}")

        med = {mode: statistics.median(values) for mode, values in samples.items()}
        ratio = med["llvm"] / med["native"]
        results[name] = {
            "native_median_sec": med["native"],
            "llvm_median_sec": med["llvm"],
            "llvm_over_native": ratio,
            "samples": samples,
            "output": outputs["native"],
        }
        print(
            f"{name}: native={med['native']:.4f}s llvm={med['llvm']:.4f}s "
            f"ratio={ratio:.4f}",
            flush=True,
        )

    (HERE / "results.json").write_text(json.dumps(results, indent=2) + "\n")


if __name__ == "__main__":
    main()
