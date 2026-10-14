#!/usr/bin/env python3
"""Benchmark the installed-compiler OCaml SAT core against Z3's SAT engine.

Run sat_corpus.py first, then pass its output directory here. Solver CPU times
exclude DIMACS parsing, process startup, model checking and Z3 AST construction.
Cold loading is reported separately; fresh assumption queries include loading.
"""

import argparse
import csv
from datetime import datetime, timezone
import hashlib
import json
import platform
import random
import statistics
import subprocess
import sys
import tarfile
import tempfile
import time
from pathlib import Path

import z3


ROOT = Path(__file__).resolve().parents[2]


class BenchmarkTimeout(Exception):
    pass


def build(directory):
    compiler = ROOT / "_install/bin/ocamlopt.opt"
    if not compiler.is_file():
        raise RuntimeError("run make install before benchmarking")
    directory.mkdir(parents=True, exist_ok=True)
    for name in ("vox_sat_solver.mli", "vox_sat_solver.ml"):
        subprocess.run([str(compiler), "-O3", "-c", "-I", str(directory),
                        "-o", str(directory / Path(name).with_suffix(
                            ".cmi" if name.endswith("mli") else ".cmx")),
                        str(ROOT / "verification" / name)], check=True)
    executable = directory / "sat_bench"
    subprocess.run([str(compiler), "-O3", "-I", str(directory), "-I", "+unix",
                    "-c", "-o", str(directory / "sat_bench.cmx"),
                    str(Path(__file__).with_suffix(".ml"))], check=True)
    subprocess.run([str(compiler), "-O3", "-I", str(directory), "-I", "+unix",
                    "unix.cmxa", str(directory / "vox_sat_solver.cmx"),
                    str(directory / "sat_bench.cmx"), "-o", str(executable)],
                   check=True)
    return compiler, executable


def synthetic(directory):
    directory.mkdir(parents=True, exist_ok=True)
    cases = []
    for variables, seed in [(100, 17), (150, 23), (200, 42)]:
        rng = random.Random(seed)
        clauses = [[v if rng.randrange(2) else -v for v in
                    rng.sample(range(1, variables + 1), 3)]
                   for _ in range(int(4.26 * variables))]
        cases.append((f"random-{variables}-{seed}", variables, clauses))
    for holes in (5, 6, 7):
        def var(pigeon, hole):
            return pigeon * holes + hole + 1
        clauses = [[var(p, h) for h in range(holes)]
                   for p in range(holes + 1)]
        clauses += [[-var(p, h), -var(q, h)] for h in range(holes)
                    for p in range(holes + 1) for q in range(p)]
        cases.append((f"pigeonhole-{holes}", holes * (holes + 1), clauses))
    paths = []
    for name, variables, clauses in cases:
        path = directory / f"{name}.cnf"
        path.write_text(f"p cnf {variables} {len(clauses)}\n" + "".join(
            " ".join(map(str, clause)) + " 0\n" for clause in clauses))
        paths.append(path)
    return paths


def read_dimacs(path):
    variables = None
    clauses, pending = [], []
    for line in path.read_text().splitlines():
        words = line.split()
        if not words or words[0] == "c":
            continue
        if words[0] == "p":
            variables, count = map(int, words[2:])
            continue
        for literal in map(int, words):
            if literal:
                pending.append(literal)
            else:
                clauses.append(pending)
                pending = []
    if variables is None or pending or len(clauses) != count:
        raise ValueError(f"invalid DIMACS: {path}")
    return variables, clauses


def z3_benchmark(path, repeats, timeout, assumption_vars):
    variables, clauses = read_dimacs(path)
    symbols = [None] + [z3.Bool(f"v{i}") for i in range(1, variables + 1)]

    def literal(value):
        return symbols[value] if value > 0 else z3.Not(symbols[-value])

    formula = z3.And(*[z3.Or(*map(literal, clause)) for clause in clauses])
    queries = [[literal(v * sign)]
               for v in range(1, min(assumption_vars, variables) + 1)
               for sign in (1, -1)]

    def create():
        solver = z3.SolverFor("QF_FD")
        solver.set(timeout=max(1, int(timeout * 1000)))
        solver.add(formula)
        return solver

    def check(solver, assumptions):
        result = solver.check(*assumptions)
        if result == z3.unknown:
            raise BenchmarkTimeout(path)
        return {"sat": "S", "unsat": "U", "unknown": "X"}[str(result)]

    samples = {key: [] for key in
               ("load", "solve", "cold", "incremental", "fresh")}
    previous = None
    for _ in range(repeats):
        start = time.process_time()
        solver = create()
        samples["load"].append(time.process_time() - start)
        start = time.process_time()
        answer = check(solver, [])
        samples["solve"].append(time.process_time() - start)
        samples["cold"].append(samples["load"][-1] + samples["solve"][-1])
        start = time.process_time()
        answers = "".join(check(solver, query) for query in queries)
        samples["incremental"].append(time.process_time() - start)
        start = time.process_time()
        fresh_answers = "".join(check(create(), query) for query in queries)
        samples["fresh"].append(time.process_time() - start)
        if answers != fresh_answers:
            raise RuntimeError(f"Z3 incremental mismatch: {path}")
        if previous is not None and previous != (answer, answers):
            raise RuntimeError(f"Z3 result changed between repetitions: {path}")
        previous = answer, answers
    return answer, answers, {key: statistics.median(value)
                             for key, value in samples.items()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("corpus", type=Path)
    parser.add_argument("--repeats", type=int, default=5)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--assumption-vars", type=int, default=8,
                        help="query both polarities; zero for cold only")
    parser.add_argument("--limit", type=int)
    parser.add_argument("--synthetic", action="store_true")
    parser.add_argument("--all", action="store_true",
                        help="include cases already solved by unit propagation")
    parser.add_argument("--output", type=Path,
                        default=ROOT / "_build/vox-sat-bench/results.csv")
    args = parser.parse_args()
    if args.repeats <= 0 or args.timeout <= 0 or args.assumption_vars < 0:
        parser.error("positive repeats and timeout required")
    temporary = tempfile.TemporaryDirectory(prefix="vox-sat-bench-")
    corpus = args.corpus
    if corpus.is_file():
        with tarfile.open(corpus) as archive:
            archive.extractall(temporary.name, filter="data")
        manifests = list(Path(temporary.name).rglob("manifest.json"))
        if len(manifests) != 1:
            parser.error("archive must contain one corpus manifest")
        corpus = manifests[0].parent
    manifest_path = corpus / "manifest.json"
    records = {}
    if manifest_path.is_file():
        manifest = json.loads(manifest_path.read_text())
        records = {corpus / record["cnf"]: record
                   for record in manifest["records"]}
        paths = [path for path, record in records.items()
                 if args.all or record["benchmark"]]
    else:
        paths = sorted(corpus.rglob("*.cnf"))
    if args.limit is not None:
        paths = paths[:args.limit]
    if args.synthetic:
        paths += synthetic(ROOT / "_build/vox-sat-bench/synthetic")
    if not paths:
        parser.error("no CNF instances found")
    compiler, executable = build(ROOT / "_build/vox-sat-bench")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    metadata = {"compiler": str(compiler), "compiler_version":
                subprocess.check_output([str(compiler), "-version"], text=True)
                .strip(), "compiler_sha256": hashlib.sha256(
                    compiler.read_bytes()).hexdigest(),
                "z3": z3.get_version_string(),
                "python": sys.version,
                "date": datetime.now(timezone.utc).isoformat(),
                "command": ["python3", *sys.argv],
                "machine": platform.platform(), "repeats": args.repeats,
                "clock": "process CPU seconds", "z3_engine": "QF_FD",
                "excluded": "parsing, process startup, Z3 ASTs, model checks",
                "incremental": "after the cold base solve; +v and -v for first "
                               f"{args.assumption_vars} variables",
                "timeout_seconds": args.timeout,
                "timeouts": "a timeout in any repeat discards all timings "
                            "for that instance/solver and is reported",
                "primary_metric": "cold_s: median per-repeat load + solve",
                "binding_overhead": "Z3 receives one prebuilt conjunction; "
                    "short assumption queries include Python binding overhead",
                "corpus": str(args.corpus),
                "solver_sha256": hashlib.sha256((ROOT /
                    "verification/vox_sat_solver.ml").read_bytes()).hexdigest()}
    if manifest_path.is_file():
        metadata["manifest_sha256"] = hashlib.sha256(
            manifest_path.read_bytes()).hexdigest()
    args.output.with_suffix(".json").write_text(json.dumps(metadata, indent=2)
                                               + "\n")
    with args.output.open("w") as channel:
        output = csv.writer(channel)
        output.writerow(["path", "kind", "solver", "variables", "clauses",
                         "result", "load_s", "solve_s", "cold_s",
                         "incremental_s", "fresh_s",
                         "assumption_results", "decisions", "propagations",
                         "conflicts", "restarts", "learned", "deleted"])
        for path in paths:
            record = records.get(path)
            kind = record["kind"] if record else "synthetic"
            name = record["cnf"] if record else str(path.name)
            if record and hashlib.sha256(path.read_bytes()).hexdigest() != \
                    record["cnf_sha256"]:
                raise RuntimeError(f"corpus checksum mismatch: {path}")
            native = subprocess.check_output([
                str(executable), "--repeats", str(args.repeats),
                "--timeout", str(args.timeout), "--assumption-vars",
                str(args.assumption_vars), str(path)], text=True).strip()
            row = native.split("\t")
            if len(row) == 2 and row[1] == "TIMEOUT":
                nvars, clauses = read_dimacs(path)
                row = [str(path), nvars, len(clauses), "timeout", *([""] * 12)]
            try:
                answer, answers, times = z3_benchmark(
                    path, args.repeats, args.timeout, args.assumption_vars)
            except BenchmarkTimeout:
                answer, answers = "timeout", ""
                times = dict.fromkeys(
                    ("load", "solve", "cold", "incremental", "fresh"), "")
            if record and record["expected"] in ("sat", "unsat") and \
                    any(value != "timeout" and value !=
                        {"sat": "S", "unsat": "U"}[record["expected"]]
                        for value in (answer, row[3])):
                raise RuntimeError(f"corpus answer mismatch: {path}")
            if "timeout" not in (row[3], answer) and \
                    (row[3] != answer or row[9] != answers):
                raise RuntimeError(f"solver mismatch or timeout: {path}: "
                                   f"OCaml {row[3]}/{row[9]}, "
                                   f"Z3 {answer}/{answers}")
            output.writerow([name, kind, "ocaml", *row[1:]])
            output.writerow([name, kind, "z3-sat", row[1], row[2], answer,
                             times["load"], times["solve"], times["cold"],
                             times["incremental"],
                             times["fresh"], answers, *([""] * 6)])
            channel.flush()
            native_time = ("timeout" if row[3] == "timeout" else
                           f"{float(row[6])*1000:.3f} ms")
            z3_time = ("timeout" if answer == "timeout" else
                       f"{times['cold']*1000:.3f} ms")
            print(f"{path.name}: cold OCaml {native_time}, Z3 SAT {z3_time}",
                  flush=True)
    print(f"Results: {args.output}")
    temporary.cleanup()


if __name__ == "__main__":
    main()
