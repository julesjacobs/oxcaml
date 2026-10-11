#!/usr/bin/env python3
"""Compare extracted collection mathematics with the installed Vox compiler."""

import argparse
import csv
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parents[2]
LIBRARY = Path("verification/library")
DEMOS = Path("testsuite/tests/vox")
WORKLOADS = {
    "borrow": ["borrow_demo.ml"],
    "sorted-array": ["sorted_array_proofs.ml", "sorted_array.mli",
                     "sorted_array.ml", "sorted_array_client.ml"],
    "quicksort": ["quicksort_model.ml", "quicksort.mli", "quicksort.ml",
                  "quicksort_client.ml"],
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline",
                        help="optional compatible baseline revision")
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--current-only", action="store_true")
    parser.add_argument("--iarray-model", action="store_true",
                        help="compare the direct iarray quicksort prototype")
    args = parser.parse_args()
    compiler = ROOT / "_install/bin/ocamlc"
    if not compiler.is_file():
        parser.error("run make install before benchmarking")
    if args.repeats < 1:
        parser.error("--repeats must be positive")
    output = csv.writer(sys.stdout)
    output.writerow(["version", "demo", "scope", "median_ms", "min_ms", "max_ms",
                     "source_lines", "queries", "smt_bytes"])
    for demo, files in WORKLOADS.items():
        if args.iarray_model and demo != "quicksort":
            continue
        versions = (["baseline", "candidate"]
                    if args.baseline and not args.current_only
                    else ["candidate"])
        for version in versions:
            names = ["vox_sequence"]
            if demo in ("sorted-array", "quicksort"):
                names.append("vox_int_sequence")
            if demo == "sorted-array":
                names.append("vox_iarray")
            modules = [LIBRARY / (name + suffix)
                       for name in names for suffix in [".mli", ".ml"]]
            if demo != "sorted-array":
                modules += [LIBRARY / "borrow.mli", LIBRARY / "borrow.ml"]
            if demo == "quicksort":
                modules += [LIBRARY / "vox_parallel.mli",
                            LIBRARY / "vox_parallel.ml"]
            selected = files
            if demo == "sorted-array":
                selected = [files[0], "sorted_array_model.ml", *files[1:]]
            modules += [DEMOS / name for name in selected]
            if args.iarray_model and version == "candidate":
                modules = [LIBRARY / (name + suffix)
                           for name in ["vox_sequence", "vox_int_sequence",
                                        "vox_iarray", "borrow_iarray"]
                           for suffix in [".mli", ".ml"]]
                modules += [DEMOS / name.replace("quicksort", "quicksort_iarray")
                            for name in files]
            with tempfile.TemporaryDirectory(prefix="vox-seq-") as directory:
                for module in modules:
                    if version == "candidate":
                        source = (ROOT / module).read_text()
                    else:
                        source = subprocess.check_output(
                            ["git", "show", f"{args.baseline}:{module}"],
                            cwd=ROOT, text=True)
                    Path(directory, module.name).write_text(source)
                command = [str(compiler), "-nostdlib", "-I",
                           str(ROOT / "_install/lib/ocaml"), "-I", directory,
                           "-extension", "refinement_types", "-principal",
                           "-alert", "-unsafe_multidomain", "-alert",
                           "-do_not_spawn_domains", "-c"]
                for scope, selected in [("sources", modules),
                                        ("client", modules[-1:])]:
                    elapsed = []
                    invocation = command + [module.name for module in selected]
                    lines = sum(len(Path(directory, module.name).read_text()
                                    .splitlines()) for module in selected)
                    for _ in range(args.repeats):
                        start = time.perf_counter()
                        result = subprocess.run(
                            invocation, cwd=directory, text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            timeout=180)
                        elapsed.append((time.perf_counter() - start) * 1000)
                        if result.returncode:
                            raise RuntimeError(
                                f"{version}/{demo}:\n{result.stdout}")
                    dump = subprocess.run(
                        [invocation[0], "-dsmtlib", *invocation[1:]], cwd=directory, text=True,
                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                        timeout=180, check=True).stdout
                    output.writerow([
                        version, demo, scope, round(statistics.median(elapsed)),
                        round(min(elapsed)), round(max(elapsed)), lines,
                        dump.count("(check-sat)"), len(dump.encode())])
                    sys.stdout.flush()



if __name__ == "__main__":
    main()
