#!/usr/bin/env python3
"""Compare sequence observation VCs; does not measure induction proofs."""

import argparse
import csv
import statistics
import subprocess
import sys
import time


def query(encoding, operation, writes, wrong=False):
    lines = ["(set-option :timeout 2000)", "(set-logic ALL)"]
    for name in ["n", "q", "cut", "x", "i"]:
        lines.append(f"(declare-const {name} Int)")
    lines += ["(assert (> n 0))", "(assert (and (<= 0 q) (< q n)))",
              "(assert (and (<= 0 i) (< i n)))",
              "(assert (and (<= 0 cut) (<= cut n)))"]
    sort = "(Seq Int)" if encoding == "sequence" else "(Array Int Int)"
    lines.append(f"(declare-const a {sort})")
    if encoding == "sequence":
        lines.append("(assert (= (seq.len a) n))")

    def read(a, i):
        op = "seq.nth" if encoding == "sequence" else "select"
        return f"({op} {a} {i})"

    if operation == "split":
        if encoding == "sequence":
            value = ("(seq.++ (seq.extract a 0 cut) "
                     "(seq.extract a cut (- n cut)))")
            observed = read(value, "q")
        elif encoding == "array":
            lines += [
                "(define-fun left () (Array Int Int) "
                "(lambda ((k Int)) (select a k)))",
                "(define-fun right () (Array Int Int) "
                "(lambda ((k Int)) (select a (+ cut k))))",
                "(define-fun joined () (Array Int Int) (lambda ((k Int)) "
                "(ite (< k cut) (select left k) (select right (- k cut)))))"]
            observed = read("joined", "q")
        else:
            observed = ("(ite (< q cut) (select a q) "
                        "(select a (+ cut (- q cut))))")
        expected = read("a", "q")
    else:
        previous = "a"
        observed = read("a", "q")
        for k in range(writes):
            index, value = f"i{k}", f"x{k}"
            lines += [f"(declare-const {index} Int)",
                      f"(declare-const {value} Int)",
                      f"(assert (and (<= 0 {index}) (< {index} n)))"]
            if operation == "frame":
                lines.append(f"(assert (distinct q {index}))")
            if encoding == "ground":
                expression = f"(ite (= q {index}) {value} {observed})"
                observed = f"r{k}"
                lines.append(f"(define-fun {observed} () Int {expression})")
            else:
                if encoding == "array":
                    expression = f"(store {previous} {index} {value})"
                else:
                    expression = (
                        f"(seq.++ (seq.extract {previous} 0 {index}) "
                        f"(seq.unit {value}) "
                        f"(seq.extract {previous} (+ {index} 1) "
                        f"(- n (+ {index} 1))))")
                previous = f"a{k}"
                lines.append(f"(define-fun {previous} () {sort} {expression})")
                observed = read(previous, "q")
        if operation == "update":
            lines.append(f"(assert (= q i{writes - 1}))")
            expected = f"x{writes - 1}"
        else:
            expected = read("a", "q")
    if wrong:
        expected = f"(+ {expected} 1)"
    lines += [f"(assert (distinct {observed} {expected}))", "(check-sat)"]
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--z3", default="z3")
    args = parser.parse_args()
    version = subprocess.check_output([args.z3, "--version"], text=True)
    print(version.strip(), file=sys.stderr)
    output = csv.writer(sys.stdout)
    output.writerow(["encoding", "operation", "writes", "expected", "result",
                     "median_ms", "bytes"])
    workloads = [("update", 1), ("update", 8), ("frame", 1), ("frame", 8),
                 ("split", 0)]
    for operation, writes in workloads:
        for encoding in ["ground", "array", "sequence"]:
            for wrong in [False, True]:
                smt = query(encoding, operation, writes, wrong)
                results, elapsed = [], []
                for _ in range(args.repeats):
                    start = time.perf_counter()
                    process = subprocess.run(
                        [args.z3, "-in"], input=smt, text=True,
                        capture_output=True, timeout=5, check=True)
                    elapsed.append((time.perf_counter() - start) * 1000)
                    result = process.stdout.strip()
                    if result not in ["sat", "unsat", "unknown"]:
                        raise RuntimeError(process.stdout + process.stderr)
                    results.append(result)
                expected = "sat" if wrong else "unsat"
                if set(results) - {expected, "unknown"}:
                    raise RuntimeError(
                        f"Incorrect model: {encoding}/{operation}: {results}")
                output.writerow([encoding, operation, writes, expected,
                                 "/".join(sorted(set(results))),
                                 round(statistics.median(elapsed), 2), len(smt)])
                sys.stdout.flush()


if __name__ == "__main__":
    main()
