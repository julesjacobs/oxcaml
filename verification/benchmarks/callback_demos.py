#!/usr/bin/env python3
"""Compare callback proof styles with the installed Vox compiler."""

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
    parser.add_argument("--baseline", default="68b3b6a07d")
    parser.add_argument("--repeats", type=int, default=3)
    args = parser.parse_args()
    compiler = ROOT / "_install/bin/ocamlc"
    if not compiler.is_file():
        parser.error("run make install before benchmarking")
    output = csv.writer(sys.stdout)
    output.writerow(["version", "demo", "median_ms", "min_ms", "max_ms",
                     "source_lines", "queries", "smt_bytes"])
    for demo, files in WORKLOADS.items():
        for version in ["baseline", "candidate"]:
            modules = []
            if demo != "sorted-array":
                modules += [LIBRARY / "borrow_model.mli",
                            LIBRARY / "borrow_model.ml",
                            LIBRARY / "borrow.mli", LIBRARY / "borrow.ml"]
            if demo == "sorted-array" and version == "candidate":
                modules += [LIBRARY / (name + suffix)
                            for name in ["vox_sequence", "vox_int_sequence", "vox_iarray"]
                            for suffix in [".mli", ".ml"]]
            modules += [DEMOS / name for name in files]
            elapsed = []
            with tempfile.TemporaryDirectory(prefix="vox-seq-") as directory:
                source_lines = 0
                for module in modules:
                    if version == "candidate":
                        source = (ROOT / module).read_text()
                    else:
                        source = subprocess.check_output(
                            ["git", "show", f"{args.baseline}:{module}"],
                            cwd=ROOT, text=True)
                    source_lines += len(source.splitlines())
                    Path(directory, module.name).write_text(source)
                command = [str(compiler), "-nostdlib", "-I",
                           str(ROOT / "_install/lib/ocaml"), "-I", directory,
                           "-extension", "refinement_types", "-principal",
                           "-alert", "-unsafe_multidomain", "-alert",
                           "-do_not_spawn_domains", "-c"]
                command += [module.name for module in modules]
                for _ in range(args.repeats):
                    start = time.perf_counter()
                    result = subprocess.run(
                        command, cwd=directory, text=True,
                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                        timeout=180)
                    elapsed.append((time.perf_counter() - start) * 1000)
                    if result.returncode:
                        raise RuntimeError(
                            f"{version}/{demo}:\n{result.stdout}")
                dump = subprocess.run(
                    [command[0], "-dsmtlib", *command[1:]], cwd=directory, text=True,
                    stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                    timeout=180, check=True).stdout
                output.writerow([
                    version, demo, round(statistics.median(elapsed)),
                    round(min(elapsed)), round(max(elapsed)), source_lines,
                    dump.count("(check-sat)"), len(dump.encode())])
                sys.stdout.flush()


if __name__ == "__main__":
    main()
