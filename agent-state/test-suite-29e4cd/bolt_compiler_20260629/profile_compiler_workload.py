#!/usr/bin/env python3
import os
import shutil
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
BUILD_MAIN = Path(
    os.environ.get(
        "PROFILE_BUILD_MAIN", ROOT / "_llvm_current_stage2_main_build/main"
    )
)
OCAMLLIB = Path(
    os.environ.get("PROFILE_OCAMLLIB", ROOT / "_llvm_current_stage2_install/lib/ocaml")
)
DEFAULT_COMPILER = Path(__file__).resolve().parent / "ocamlopt.reloc"
OUT = Path(__file__).resolve().parent / "profile_workload_tmp"

MODULES = [
    "backend/cfg_selectgen.ml",
    "backend/llvm/llvmize.ml",
    "lambda/translcore.ml",
    "typing/ctype.ml",
    "typing/env.ml",
    "typing/typecore.ml",
    "typing/typemod.ml",
]
MODULES = os.environ.get("PROFILE_MODULES", ",".join(MODULES)).split(",")


def include_args():
    dirs = sorted({path.parent for path in BUILD_MAIN.rglob("*.cmi")})
    args = []
    for directory in dirs:
        args.extend(["-I", str(directory)])
    return args


def main():
    compiler = Path(os.environ.get("PROFILE_COMPILER", DEFAULT_COMPILER))
    reps = int(os.environ.get("PROFILE_REPETITIONS", "3"))
    shutil.rmtree(OUT, ignore_errors=True)
    OUT.mkdir(parents=True)

    includes = include_args()
    env = os.environ.copy()
    env["OCAMLLIB"] = str(OCAMLLIB)

    for rep in range(reps):
        rep_out = OUT / f"rep-{rep}"
        rep_out.mkdir()
        for module in MODULES:
            source = BUILD_MAIN / module
            stem = source.stem
            out_cmx = rep_out / f"{stem}.cmx"
            cmd = [
                str(compiler),
                "-w",
                "-a",
                "-g",
                *includes,
                "-c",
                str(source),
                "-o",
                str(out_cmx),
            ]
            subprocess.run(
                cmd,
                cwd=BUILD_MAIN,
                env=env,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                check=True,
            )

    return 0


if __name__ == "__main__":
    sys.exit(main())
