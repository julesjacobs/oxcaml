#!/usr/bin/env python3
"""Extract Vox VCs and convert them to independently checked DIMACS CNF.

Requires Python's z3-solver package, Z3, and a Vox compiler from make install.
Imported interfaces are rebuilt separately with verification skipped; their
source and artifact hashes are recorded. Conversion uses Z3; OCaml solving does
not. Exact conversion expands product datatypes, Ackermannizes uninterpreted
functions over Bool/Int/BV, and uses precise bounded arithmetic and bit-blasting
tactics. Unsupported theories use explicitly recorded Boolean abstraction.
"""

import argparse
from collections import Counter, defaultdict, deque
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import time

import z3


ROOT = Path(__file__).resolve().parents[2]
SOURCES = [
    "testsuite/tests/vox/clamp_api.ml",
    "verification/library/vox_control.ml",
    "verification/library/vox_table_probe.ml",
    "verification/library/vox_table_wrap.ml",
    "verification/library/vox_lz4_spec_parse.ml",
    "testsuite/tests/vox/hmc_word64.ml",
    "testsuite/tests/vox/expression_folding.ml",
    "testsuite/tests/vox/functional_queue.ml",
    "testsuite/tests/vox/quicksort.ml",
    "testsuite/tests/vox/mode_solver_public.ml",
    "verification/library/vox_rsa_arithmetic.ml",
    "testsuite/tests/vox/sat_solver_rejected.ml",
    "testsuite/tests/vox/queue_rejected.ml",
]


def digest(data):
    return hashlib.sha256(data).hexdigest()


def prepare_dependencies(sources, compiler):
    owner = tempfile.TemporaryDirectory(prefix="vox-sat-deps-")
    directory = Path(owner.name)
    inventory = {}
    for parent in (ROOT / "verification/library", ROOT / "testsuite/tests/vox"):
        for path in sorted(parent.glob("*.ml*")):
            if path.suffix in (".ml", ".mli"):
                module = path.stem[0].upper() + path.stem[1:]
                if module not in inventory or path.suffix == ".mli":
                    inventory[module] = path
    built = {}
    active = set()
    ocamldep = compiler.parent / "ocamldep"

    def imports(path):
        result = subprocess.check_output(
            [str(ocamldep), "-modules", str(path)], text=True)
        return result.split(":", 1)[1].split()

    def build(module):
        if module in built or module not in inventory:
            return
        if module in active:
            raise ValueError("cyclic dependency: " + module)
        active.add(module)
        source = inventory[module]
        for dependency in imports(source):
            if dependency != module:
                build(dependency)
        local = directory / source.name
        shutil.copyfile(source, local)
        command = [str(compiler), "-extension", "refinement_types",
                   "-smt-assume-verified", "-I", str(directory),
                   "-stop-after", "typing", "-c", source.name]
        if source.parent.name == "library":
            command.insert(1, "-vox-library")
        result = subprocess.run(command, cwd=directory, text=True,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT)
        if result.returncode:
            raise RuntimeError("dependency " + module + "\n" + result.stdout)
        built[module] = {
            "source": str(source.relative_to(ROOT)),
            "source_sha256": digest(source.read_bytes()),
            "cmi_sha256": digest(local.with_suffix(".cmi").read_bytes()),
            "command": command,
        }
        active.remove(module)

    for source in sources:
        for module in imports(ROOT / source):
            build(module)
    return directory, built, owner


def dependency_identity(dependencies):
    return {name: {key: record[key]
                   for key in ("source", "source_sha256", "cmi_sha256")}
            for name, record in dependencies.items()}


def ackermannize(expressions):
    memo = {}
    applications = defaultdict(list)
    congruence = []
    sorts = (z3.Z3_BOOL_SORT, z3.Z3_INT_SORT, z3.Z3_BV_SORT)

    def product(sort, active=()):
        if sort.kind() in sorts:
            return False
        if (sort.kind() != z3.Z3_DATATYPE_SORT
                or sort.num_constructors() != 1 or sort.get_id() in active):
            raise ValueError("unsupported sort: " + str(sort))
        constructor = sort.constructor(0)
        for i in range(constructor.arity()):
            product(constructor.domain(i), active + (sort.get_id(),))
        return True

    def fresh(sort):
        if product(sort):
            constructor = sort.constructor(0)
            return tuple(fresh(constructor.domain(i))
                         for i in range(constructor.arity()))
        return z3.FreshConst(sort, prefix="ack")

    def equal(left, right):
        if isinstance(left, tuple):
            return z3.And(*[equal(a, b) for a, b in zip(left, right)])
        return left == right

    def conditional(condition, left, right):
        if isinstance(left, tuple):
            return tuple(conditional(condition, a, b)
                         for a, b in zip(left, right))
        return z3.If(condition, left, right)

    def visit(expr):
        key = expr.get_id()
        if key in memo:
            return memo[key]
        if z3.is_quantifier(expr):
            raise ValueError("quantifier")
        is_product = product(expr.sort())
        args = [visit(child) for child in expr.children()]
        kind = expr.decl().kind()
        if kind == z3.Z3_OP_UNINTERPRETED and (args or is_product):
            result = fresh(expr.sort())
            previous = applications[expr.decl().get_id()]
            for old_args, old_result in previous:
                equal_args = z3.And(*[equal(a, b)
                                     for a, b in zip(args, old_args)])
                congruence.append(z3.Implies(equal_args,
                                           equal(result, old_result)))
            previous.append((args, result))
        elif kind == z3.Z3_OP_DT_CONSTRUCTOR:
            result = tuple(args)
        elif kind == z3.Z3_OP_DT_ACCESSOR:
            sort = expr.arg(0).sort()
            index = next(i for i in range(sort.constructor(0).arity())
                         if sort.accessor(0, i).eq(expr.decl()))
            result = args[0][index]
        elif kind == z3.Z3_OP_DT_IS:
            result = z3.BoolVal(True)
        elif kind == z3.Z3_OP_EQ:
            result = equal(*args)
        elif kind == z3.Z3_OP_DISTINCT:
            result = z3.And(*[z3.Not(equal(a, b))
                              for i, a in enumerate(args)
                              for b in args[:i]])
        elif kind == z3.Z3_OP_ITE:
            result = conditional(*args)
        else:
            result = expr.decl()(*args)
        memo[key] = result
        return result

    result = [visit(expr) for expr in expressions]
    return result + congruence


def boolean_abstraction(expressions):
    memo = {}
    atoms = {}
    connectives = {
        z3.Z3_OP_AND, z3.Z3_OP_OR, z3.Z3_OP_NOT, z3.Z3_OP_IMPLIES,
        z3.Z3_OP_IFF, z3.Z3_OP_XOR, z3.Z3_OP_EQ, z3.Z3_OP_ITE,
        z3.Z3_OP_DISTINCT,
    }

    def visit(expr):
        key = expr.get_id()
        if key in memo:
            return memo[key]
        if z3.is_true(expr) or z3.is_false(expr):
            result = expr
        elif (not z3.is_quantifier(expr)
              and expr.decl().kind() in connectives
              and all(z3.is_bool(child) for child in expr.children())):
            result = expr.decl()(*[visit(c) for c in expr.children()])
        else:
            name = "atom_" + str(len(atoms) + 1)
            result = z3.Bool(name)
            atoms[name] = expr.sexpr()
        memo[key] = result
        return result

    result = [visit(expr) for expr in expressions]
    return result, atoms


def dimacs(goal):
    variables = {}
    clauses = []
    for expr in goal:
        if z3.is_true(expr):
            continue
        terms = expr.children() if z3.is_or(expr) else [expr]
        clause = []
        for term in terms:
            if z3.is_false(term):
                continue
            negative = z3.is_not(term)
            atom = term.arg(0) if negative else term
            if not (z3.is_const(atom) and z3.is_bool(atom)
                    and atom.decl().kind() == z3.Z3_OP_UNINTERPRETED):
                raise ValueError("residual theory term: " + atom.sexpr()[:80])
            index = variables.setdefault(atom.get_id(), len(variables) + 1)
            clause.append(-index if negative else index)
        clauses.append(clause)
    return len(variables), clauses


def convert(expressions, timeout_ms):
    started = time.perf_counter()
    reason = None
    atoms = None
    try:
        goal = z3.Goal()
        goal.add(*ackermannize(expressions))
        tactic = z3.Then("simplify", "nla2bv", "bit-blast", "tseitin-cnf")
        result = z3.TryFor(tactic, timeout_ms)(goal)
        if len(result) != 1 or result[0].precision() != z3.Z3_GOAL_PRECISE:
            raise ValueError("bounded conversion is not equisatisfiable")
        nvars, clauses = dimacs(result[0])
        kind = "exact"
    except (ValueError, z3.Z3Exception) as error:
        reason = str(error)
        abstraction, atoms = boolean_abstraction(expressions)
        goal = z3.Goal()
        goal.add(*abstraction)
        result = z3.TryFor(z3.Tactic("tseitin-cnf"), timeout_ms)(goal)
        nvars, clauses = dimacs(result[0])
        kind = "boolean_abstraction"
    seconds = time.perf_counter() - started
    return kind, reason, atoms, nvars, clauses, seconds


def unit_result(clauses):
    occurrences = defaultdict(list)
    pending = deque()
    counts = []
    assignments = set()
    for index, clause in enumerate(clauses):
        if not clause:
            return "unsat"
        counts.append(len(clause))
        for literal in clause:
            occurrences[literal].append(index)
        if len(clause) == 1:
            pending.append(clause[0])
    while pending:
        literal = pending.popleft()
        if -literal in assignments:
            return "unsat"
        if literal in assignments:
            continue
        assignments.add(literal)
        for index in occurrences[literal]:
            counts[index] = -1
        for index in occurrences[-literal]:
            if counts[index] <= 0:
                continue
            counts[index] -= 1
            if counts[index] == 0:
                return "unsat"
            if counts[index] == 1:
                pending.extend(x for x in clauses[index]
                               if -x not in assignments)
    return "sat" if all(n < 0 for n in counts) else "undecided"


def check(expressions, timeout_ms):
    solver = z3.Solver()
    solver.set(timeout=timeout_ms)
    solver.add(*expressions)
    started = time.perf_counter()
    status = str(solver.check())
    return status, time.perf_counter() - started


def check_cnf(nvars, clauses, timeout_ms):
    solver = z3.Solver()
    solver.set(timeout=timeout_ms)
    with tempfile.NamedTemporaryFile(mode="w", suffix=".cnf") as temporary:
        temporary.write(f"p cnf {nvars} {len(clauses)}\n")
        for clause in clauses:
            temporary.write(" ".join(map(str, clause)) + " 0\n")
        temporary.flush()
        solver.from_file(temporary.name)
    started = time.perf_counter()
    return str(solver.check()), time.perf_counter() - started


def extract(source, compiler, library, output, args, provenance):
    source_path = ROOT / source
    name = source_path.stem
    dump_path = output / "dumps" / (name + ".txt")
    info_path = dump_path.with_suffix(".json")
    if args.reuse_dumps and dump_path.exists():
        info = json.loads(info_path.read_text())
        if info["source_sha256"] != digest(source_path.read_bytes()):
            raise ValueError("source changed since dump: " + source)
        if info.get("compiler_sha256") != provenance["compiler_sha256"]:
            raise ValueError("compiler changed since dump: " + source)
        if (dependency_identity(info.get("dependencies", {}))
                != dependency_identity(provenance["dependencies"])):
            raise ValueError("dependencies changed since dump: " + source)
        if info["compiler_exit_status"] and "rejected" not in name:
            raise ValueError("previous extraction failed: " + source)
        return dump_path.read_text(), info
    with tempfile.TemporaryDirectory(prefix="vox-sat-") as temporary:
        local = Path(temporary) / source_path.name
        shutil.copyfile(source_path, local)
        command = [str(compiler), "-extension", "refinement_types",
                   "-dsmtlib", "-I", str(library), "-c", local.name]
        if source.startswith("verification/library/"):
            command.insert(1, "-vox-library")
        env = dict(os.environ, VOX_VERIFY_CACHE="")
        started = time.perf_counter()
        result = subprocess.run(command, cwd=temporary, env=env,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True,
                                timeout=args.compile_timeout)
    info = {
        "source": source,
        "source_sha256": digest(source_path.read_bytes()),
        "command": command,
        "compiler_exit_status": result.returncode,
        "compile_seconds": time.perf_counter() - started,
        "dump": str(dump_path.relative_to(output)),
        **provenance,
    }
    dump_path.write_text(result.stdout)
    info_path.write_text(json.dumps(info, indent=2) + "\n")
    if result.returncode and "rejected" not in name:
        raise RuntimeError("compilation failed: " + source + "\n"
                           + result.stdout[-4000:])
    if result.returncode and "counterexample" not in result.stdout:
        raise RuntimeError("rejected source failed before a counterexample: "
                           + source + "\n" + result.stdout[-4000:])
    return result.stdout, info


def emit_record(query, name, source, ordinal, variant, original_smt,
                info, output, args, manifest):
    smt_path = Path("smt2") / (name + ".smt2")
    cnf_path = Path("cnf") / (name + ".cnf")
    (output / smt_path).write_text(query)
    expressions = z3.parse_smt2_string(query)
    smt_status, smt_seconds = check(expressions, args.solver_timeout_ms)
    kind, reason, atoms, nvars, clauses, seconds = convert(
        expressions, args.conversion_timeout_ms)
    cnf_status, cnf_seconds = check_cnf(nvars, clauses,
                                       args.solver_timeout_ms)
    if (kind == "exact" and smt_status != "unknown"
            and cnf_status != "unknown" and smt_status != cnf_status):
        raise RuntimeError("exact conversion changed status: " + name)
    if (kind == "boolean_abstraction" and smt_status == "sat"
            and cnf_status == "unsat"):
        raise RuntimeError("Boolean abstraction lost a model: " + name)
    use_smt = kind == "exact" and smt_status in ("sat", "unsat")
    status = smt_status if use_smt else cnf_status
    body = ["c " + name + " " + kind,
            f"p cnf {nvars} {len(clauses)}"]
    body.extend(" ".join(map(str, clause)) + " 0" for clause in clauses)
    cnf_bytes = ("\n".join(body) + "\n").encode()
    (output / cnf_path).write_bytes(cnf_bytes)
    propagated = unit_result(clauses)
    record = {
        "name": name, "source": source, "query_ordinal": ordinal + 1,
        "variant": variant, "original_smt": str(original_smt),
        "query_command": query.rstrip().splitlines()[-1],
        "source_sha256": info["source_sha256"],
        "smt": str(smt_path), "smt_sha256": digest(query.encode()),
        "cnf": str(cnf_path), "cnf_sha256": digest(cnf_bytes),
        "kind": kind, "conversion_fallback_reason": reason,
        "smt_status": smt_status, "expected": status,
        "z3_cnf_status": cnf_status,
        "expected_from": "smt" if use_smt else "converted_cnf",
        "variables": nvars, "clauses": len(clauses),
        "literals": sum(map(len, clauses)),
        "conversion_seconds": seconds,
        "z3_smt_seconds": smt_seconds, "z3_cnf_seconds": cnf_seconds,
        "unit_result": propagated,
        "benchmark": nvars >= 20 and len(clauses) >= 50
                     and propagated == "undecided"
                     and status in ("sat", "unsat"),
    }
    if atoms is not None:
        atom_path = Path("atoms") / (name + ".json")
        (output / atom_path).write_text(json.dumps(atoms, indent=2)
                                       + "\n")
        record["atoms"] = str(atom_path)
    manifest["records"].append(record)
    print(name, kind, smt_status, "->", status,
          nvars, len(clauses), "unit=" + propagated, flush=True)


def archive_corpus(manifest, output, destination):
    destination.parent.mkdir(parents=True, exist_ok=True)
    members = {"manifest.json"}
    for source in manifest["sources"]:
        members.add(source["dump"])
        members.add(str(Path(source["dump"]).with_suffix(".json")))
    for record in manifest["records"]:
        for key in ("smt", "cnf", "original_smt", "atoms"):
            if key in record:
                members.add(record[key])
    with tempfile.NamedTemporaryFile(dir=destination.parent,
                                     delete=False) as temporary:
        filename = Path(temporary.name)
    try:
        with tarfile.open(filename, "w:gz") as archive:
            for member in sorted(members):
                archive.add(output / member, arcname=member)
        os.replace(filename, destination)
    finally:
        filename.unlink(missing_ok=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("sources", nargs="*", default=SOURCES)
    parser.add_argument("--output", type=Path,
                        default=ROOT / "_build/vox-sat-corpus")
    parser.add_argument("--compiler", type=Path,
                        default=ROOT / "_install/bin/ocamlopt")
    parser.add_argument("--max-per-source", type=int, default=8)
    parser.add_argument("--compile-timeout", type=int, default=180)
    parser.add_argument("--solver-timeout-ms", type=int, default=10000)
    parser.add_argument("--conversion-timeout-ms", type=int, default=10000)
    parser.add_argument("--reuse-dumps", action="store_true")
    parser.add_argument("--premises", action="store_true",
                        help="also retain VCs with their negated goal removed")
    parser.add_argument("--archive", type=Path,
                        help="write a portable gzip corpus (excludes CMIs)")
    args = parser.parse_args()
    compiler = args.compiler.resolve()
    output = args.output.resolve()
    if "boot" in compiler.parts or not compiler.is_file():
        parser.error("use the compiler installed by make install")
    if args.max_per_source < 1:
        parser.error("--max-per-source must be positive")
    names = [Path(source).stem for source in args.sources]
    if len(names) != len(set(names)):
        parser.error("source names must have distinct stems")
    for directory in ("dumps", "smt2", "cnf", "atoms"):
        (output / directory).mkdir(parents=True, exist_ok=True)
    print("prepare dependency interfaces", flush=True)
    library, dependencies, dependency_directory = prepare_dependencies(
        args.sources, compiler)
    provenance = {
        "compiler_sha256": digest(compiler.read_bytes()),
        "dependencies": dependencies,
    }
    manifest = {
        "format_version": 1,
        "scope": "Solver queries, including preliminary proof attempts "
                 "that can be SAT even when the final verification succeeds.",
        "generation_command": ["python3", *sys.argv],
        "compiler": str(compiler),
        "compiler_sha256": digest(compiler.read_bytes()),
        "git_revision": subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "z3_version": z3.get_version_string(),
        "exact_conversion": "expand product datatypes; ackermannize; "
                            "simplify; nla2bv; bit-blast; "
                            "tseitin-cnf (precise goals only)",
        "abstraction": "Each distinct theory atom becomes an unconstrained "
                       "Boolean variable. SAT does not imply SMT SAT.",
        "benchmark_filter": "Known status, at least 20 variables and 50 "
                            "clauses, not solved by unit propagation.",
        "sources": [],
        "records": [],
        "dependencies": dependencies,
    }
    for source in args.sources:
        print("extract", source, flush=True)
        dump, info = extract(source, compiler, library, output, args,
                             provenance)
        if re.search(r"\(check-sat-assuming\b", dump):
            raise ValueError("unsupported query command in dump: " + source)
        query_pattern = r"^\(reset\)\n.*?^\(check-sat(?:-using [^\n]*)?\)\n"
        queries = re.findall(query_pattern, dump,
                             flags=re.DOTALL | re.MULTILINE)
        commands = re.findall(r"^\(check-sat", dump, flags=re.MULTILINE)
        if len(queries) != len(commands):
            raise ValueError("query command was not captured: " + source)
        info["queries"] = len(queries)
        manifest["sources"].append(info)
        if not queries:
            raise RuntimeError("no VCs extracted from " + source + "\n" + dump)
        count = min(len(queries), args.max_per_source)
        chosen = sorted({i * (len(queries) - 1) // max(1, count - 1)
                         for i in range(count)})
        for ordinal in chosen:
            query = queries[ordinal]
            name = Path(source).stem + "-" + str(ordinal + 1).zfill(4)
            original_smt = Path("smt2") / (name + ".smt2")
            emit_record(query, name, source, ordinal, "vc", original_smt,
                        info, output, args, manifest)
            if args.premises:
                expressions = z3.parse_smt2_string(query)
                if expressions and z3.is_not(expressions[-1]):
                    offset = query.rfind("\n(assert ")
                    if offset < 0:
                        raise ValueError("missing goal assertion: " + name)
                    premises = query[:offset] + "\n(check-sat)\n"
                    emit_record(premises, name + "-premises", source, ordinal,
                                "premises", original_smt, info, output,
                                args, manifest)
        (output / "manifest.json").write_text(
            json.dumps(manifest, indent=2) + "\n")
    summary = Counter((r["kind"], r["expected"], r["benchmark"])
                      for r in manifest["records"])
    print("manifest:", output / "manifest.json")
    for key, count in sorted(summary.items()):
        print(key, count)
    if args.archive:
        archive_corpus(manifest, output, args.archive)
        print("archive:", args.archive, args.archive.stat().st_size, "bytes")
    dependency_directory.cleanup()


if __name__ == "__main__":
    main()
