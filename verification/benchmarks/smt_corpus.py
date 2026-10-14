#!/usr/bin/env python3
"""Record the complete SMT protocol while verifying the Vox demonstrations."""

import argparse
from collections import Counter, defaultdict
from concurrent.futures import (
    FIRST_COMPLETED, ProcessPoolExecutor, ThreadPoolExecutor, wait,
)
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tarfile
import threading
import time
import uuid


ROOT = Path(__file__).resolve().parents[2]
CHECKS = {"check-sat", "check-sat-assuming", "check-sat-using"}
SPECIAL = re.compile(rb'[();"|]')
ATOM = re.compile(rb'[^\s();"|]+')
WHITESPACE = re.compile(rb'\s+')
END_OF_LINE = re.compile(rb'[\r\n]')
SYMBOL_DELIMITER = re.compile(rb'[|\\]')
SOURCE_DIRS = ("verification/library", "testsuite/tests/vox",
               "verification/demos")


def digest(data):
    return hashlib.sha256(data).hexdigest()


def identity(value):
    return digest(json.dumps(value, sort_keys=True).encode())


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")
    temporary.replace(path)


def write_executable(path, contents):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp-" + uuid.uuid4().hex)
    temporary.write_text(contents)
    temporary.chmod(0o755)
    temporary.replace(path)


def expressions(data):
    """Yield complete SMT-LIB expressions and their exact byte offsets.

    A killed solver may leave an incomplete final expression; its bytes remain
    in the session stream and are deliberately not indexed as a command.
    """
    start = None
    depth = 0
    quoted = None
    comment = False
    i = 0
    length = len(data)
    while i < length:
        byte = data[i]
        if comment:
            end = END_OF_LINE.search(data, i)
            if end is None:
                break
            i = end.end()
            comment = False
            continue
        if quoted is not None:
            if quoted == 34:
                i = data.find(b'"', i)
                if i < 0:
                    break
                if i + 1 < length and data[i + 1] == 34:
                    i += 2
                    continue
            else:
                delimiter = SYMBOL_DELIMITER.search(data, i)
                if delimiter is None:
                    break
                i = delimiter.start()
                if data[i] == 92:
                    i += 2
                    continue
            quoted = None
            if depth == 0:
                yield start, i + 1, data[start:i + 1]
                start = None
            i += 1
            continue
        if byte == 59:
            if start is not None and depth == 0:
                yield start, i, data[start:i]
                start = None
            comment = True
        elif byte in (34, 124):
            if start is None:
                start = i
            quoted = byte
        elif byte == 40:
            if start is None:
                start = i
            depth += 1
        elif byte == 41:
            if depth == 0:
                raise ValueError(f"unmatched ')' at byte {i}")
            depth -= 1
            if depth == 0:
                yield start, i + 1, data[start:i + 1]
                start = None
        elif byte in b" \t\r\n":
            if start is not None and depth == 0:
                yield start, i, data[start:i]
                start = None
            whitespace = WHITESPACE.match(data, i)
            i = whitespace.end()
            continue
        elif start is None:
            start = i
        if depth and byte not in (34, 59, 124):
            special = SPECIAL.search(data, i + 1)
            if special is None:
                break
            i = special.start()
        elif depth == 0 and byte not in (34, 59, 124, 40, 41):
            atom = ATOM.match(data, i)
            i = atom.end() if atom else i + 1
        else:
            i += 1
    if start is not None and depth == 0 and quoted is None:
        yield start, length, data[start:]


def command_name(expression):
    if not expression.startswith(b"("):
        return None
    rest = re.sub(rb";[^\r\n]*(?:\r?\n|$)", b" ", expression[1:])
    match = re.match(rb"\s*([^\s()]+)", rest)
    return match[1].decode("ascii", errors="replace") if match else None


def index_session(directory):
    metadata = json.loads((directory / "session.json").read_text())
    missing = []

    def read(name):
        try:
            return (directory / name).read_bytes()
        except FileNotFoundError:
            missing.append(name)
            return b""

    incoming = read("input.smt2")
    outgoing = read("stdout")
    stderr = read("stderr")
    errors = []
    try:
        commands = list(expressions(incoming))
    except ValueError as error:
        commands = []
        errors.append(str(error))
    try:
        responses = list(expressions(outgoing))
    except ValueError as error:
        responses = []
        errors.append(str(error))
    checks = []
    context_start = 0
    for start, end, expression in commands:
        name = command_name(expression)
        if name == "reset":
            context_start = start
        if name in CHECKS:
            checks.append((start, end, name, context_start))
    answers = [(a, b, e.decode()) for a, b, e in responses
               if e in (b"sat", b"unsat", b"unknown")]
    aligned = len(checks) == len(answers)
    queries = []
    for ordinal, (start, end, command, context_start) in enumerate(checks, 1):
        answer = answers[ordinal - 1] if ordinal <= len(answers) else None
        queries.append({
            "id": f"{directory.name}:{ordinal}",
            "session": directory.name,
            "source": metadata.get("source"),
            "run_id": metadata.get("run_id"),
            "ordinal": ordinal,
            "command": command,
            "command_start": start,
            "command_end": end,
            "replay_start": context_start,
            "replay_end": end,
            "query_sha256": digest(incoming[context_start:end]),
            "command_sha256": digest(incoming[start:end]),
            "result": answer[2] if answer else None,
            "response_start": answer[0] if answer else None,
            "response_end": answer[1] if answer else None,
            "response_alignment_complete": aligned,
        })
    last = commands[-1][1] if commands else 0
    trailing = re.sub(rb";[^\r\n]*", b"", incoming[last:]).strip()
    summary = dict(metadata, id=directory.name, queries=len(checks),
                   responses=len(answers), input_sha256=digest(incoming),
                   stdout_sha256=digest(outgoing),
                   stderr_sha256=digest(stderr), capture_files_missing=missing,
                   input_bytes=len(incoming), stdout_bytes=len(outgoing),
                   incomplete_input=bool(trailing), parse_errors=errors,
                   response_alignment_complete=aligned)
    return summary, queries


def wrap(arguments):
    real_solver = os.environ["VOX_SMT_REAL_SOLVER"]
    directory = (Path(os.environ["VOX_SMT_CORPUS_DIR"])
                 / f"{time.time_ns()}-{os.getpid()}-{uuid.uuid4().hex[:8]}")
    directory.mkdir(parents=True)
    started = time.monotonic()
    metadata = {
        "source": os.environ.get("VOX_SMT_CORPUS_SOURCE"),
        "run_id": os.environ.get("VOX_SMT_CORPUS_RUN_ID"),
        "configuration": os.environ.get("VOX_SMT_CORPUS_CONFIG"),
        "extractor_sha256": digest(Path(__file__).read_bytes()),
        "argv": [real_solver, *arguments],
        "cwd": os.getcwd(), "pid": os.getpid(),
        "started_ns": time.time_ns(), "complete": False,
    }
    try:
        parent = Path(f"/proc/{os.getppid()}/cmdline").read_bytes()
        parent_arguments = [part.decode() for part in parent.split(b"\0")
                            if part]
        metadata["parent_argv"] = parent_arguments
        inventory_path = os.environ.get("VOX_SMT_CORPUS_INVENTORY")
        if inventory_path:
            inventory = json.loads(Path(inventory_path).read_text())["sources"]
            names = {Path(path).name: path for path in inventory}
            candidates = [names[Path(arg).name] for arg in parent_arguments
                          if Path(arg).name in names]
            if candidates:
                metadata["source_hint"] = metadata["source"]
                metadata["source"] = candidates[-1]
    except (OSError, ValueError):
        pass
    write_json(directory / "session.json", metadata)
    streams = {name: open(directory / filename, "wb", buffering=0)
               for name, filename in (("input", "input.smt2"),
                                      ("output", "stdout"),
                                      ("error", "stderr"))}
    events = open(directory / "events.jsonl", "w", buffering=1)
    lock = threading.Lock()
    offsets = defaultdict(int)
    child = subprocess.Popen([real_solver, *arguments], stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    metadata["solver_pid"] = child.pid
    write_json(directory / "session.json", metadata)

    def forward_signal(number, _frame):
        try:
            child.send_signal(number)
        except ProcessLookupError:
            pass

    for number in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
        signal.signal(number, forward_signal)

    def pump(source, destination, name):
        try:
            while True:
                chunk = os.read(source, 65536)
                if not chunk:
                    break
                streams[name].write(chunk)
                with lock:
                    events.write(json.dumps({
                        "stream": name, "offset": offsets[name],
                        "length": len(chunk),
                        "elapsed_ns": int((time.monotonic() - started) * 1e9),
                    }) + "\n")
                    offsets[name] += len(chunk)
                remaining = memoryview(chunk)
                while remaining:
                    remaining = remaining[os.write(destination, remaining):]
        except (BrokenPipeError, OSError):
            pass
        finally:
            if name == "input":
                child.stdin.close()

    pumps = [threading.Thread(target=pump, args=args, daemon=True) for args in
             ((0, child.stdin.fileno(), "input"),
              (child.stdout.fileno(), 1, "output"),
              (child.stderr.fileno(), 2, "error"))]
    for thread in pumps:
        thread.start()
    returncode = child.wait()
    for thread in pumps[1:]:
        thread.join()
    metadata.update(complete=True, returncode=returncode,
                    elapsed_seconds=time.monotonic() - started)
    write_json(directory / "session.json", metadata)
    return returncode if returncode >= 0 else 128 - returncode


def install_wrapper(output, solver):
    solver = Path(shutil.which(str(solver)) or solver).resolve()
    if not solver.is_file():
        raise ValueError(f"solver does not exist: {solver}")
    directory = output / "bin"
    directory.mkdir(parents=True, exist_ok=True)
    wrapper = directory / "z3"
    if solver == wrapper:
        raise ValueError("--solver must name the real solver, not the wrapper")
    script = Path(__file__).resolve()
    sessions = shlex.quote(str(output / "sessions"))
    text = ("#!/bin/sh\n"
            f"export VOX_SMT_REAL_SOLVER={shlex.quote(str(solver))}\n"
            f"export VOX_SMT_CORPUS_DIR={sessions}\n"
            f"exec {shlex.quote(sys.executable)} {shlex.quote(str(script))} "
            'wrap "$@"\n')
    write_executable(wrapper, text)
    return wrapper, solver


def catalogue_modules():
    sys.path.insert(0, str(ROOT / "verification/catalogue"))
    import line_stats
    import pages
    return line_stats, pages


def source_kind(raw):
    if b"[%%expect" in raw or re.search(rb"^\s*#\w+", raw, re.M):
        return "expect_driver"
    if re.search(rb"(?:byte_)?exit_status\s*=\s*\"2\"", raw):
        return "rejected"
    return "ordinary"


def page_sources(meta, body):
    paths = {entry.split()[0] for entry in meta.get("sources", [])}
    paths.update(re.findall(r"^@(?:code|text)\s+(\S+)", body, re.M))
    paths.update(re.findall(r"src:([^\s)#]+)", body))
    reproduction = body.partition("## Reproduce")[2]
    for name in re.findall(r"(?<![\w])([\w_]+\.mli?)\b", reproduction):
        candidate = "testsuite/tests/vox/" + name
        if (ROOT / candidate).is_file():
            paths.add(candidate)
    return {path for path in paths if Path(path).suffix in (".ml", ".mli")
            and any(path.startswith(directory + "/")
                    for directory in SOURCE_DIRS)}


def test_header(raw):
    match = re.search(rb"\(\*\s*TEST\b(.*?)\*\)", raw, re.S)
    return match[1].decode() if match else ""


def header_sources(header, index):
    result = set()
    for match in re.finditer(
            r"\b(?:prebuilt_modules|all_modules|modules|module|readonly_files)"
            r"\s*(?:\+?=)\s*\"([^\"]*)\"", header):
        for item in match[1].split():
            name = Path(item).name
            if name in index:
                result.add(index[name])
    return result


def header_rejections(header, index):
    sources = set()
    rejected = set()
    assignments = re.finditer(
        r'\b(all_modules|module|ocaml(?:c|opt)_\w*_exit_status)'
        r'\s*=\s*"([^\"]*)"', header)
    for match in assignments:
        if match[1] in ("all_modules", "module"):
            sources = {index[Path(name).name] for name in match[2].split()
                       if Path(name).name in index}
        elif match[2] == "2":
            rejected.update(sources)
    return rejected


def discover(prefix, demos):
    stats, pages = catalogue_modules()
    source = pages.Source(ROOT, "HEAD", working_tree=True)
    parser = stats.Parser(prefix, ROOT / "_build/catalogue-cache")
    catalogue = json.loads((ROOT / "verification/catalogue/catalogue.json")
                           .read_text())
    selected = demos or catalogue["demos"]
    unknown = set(selected) - set(catalogue["demos"])
    if unknown:
        raise ValueError(f"unknown demos: {sorted(unknown)}")
    listed = subprocess.check_output([
        "git", "-C", str(ROOT), "ls-files", "--cached", "--others",
        "--exclude-standard", "--", *SOURCE_DIRS], text=True).splitlines()
    listed = sorted(set(path for path in listed
                        if Path(path).suffix in (".ml", ".mli")
                        and str(Path(path).parent) in SOURCE_DIRS
                        and (ROOT / path).is_file()))
    by_name = {}
    by_module = defaultdict(list)
    for path in listed:
        name = Path(path).name
        if name in by_name and by_name[name] != path:
            raise ValueError(f"duplicate source basename: {name}")
        by_name[name] = path
        by_module[stats.module(path)].append(path)
    for name in stats.BUILTIN:
        by_module.pop(name, None)
    memberships = defaultdict(set)
    reasons = defaultdict(set)
    opens = defaultdict(set)
    syntax = {}
    raw = {}
    aliases = {}
    for demo in selected:
        scope = catalogue["census"][demo]
        aliases.update(scope.get("module_sources", {}))
        closure = stats.closure(source, parser, scope)
        units = [(stats.module(path), path, contents, ast)
                 for path, contents, ast in closure]
        for path, contents, ast in closure:
            memberships[path].add(demo)
            reasons[path].add("census_closure")
            syntax[path] = ast
            raw[path] = contents
        for path, units in stats.wrapper_opens(scope, units).items():
            opens[path].update(units)
        meta, body = pages.parse(ROOT / "verification/catalogue/pages"
                                / (demo + ".md"))
        for path in page_sources(meta, body):
            if not (ROOT / path).is_file():
                raise ValueError(f"missing page source: {path}")
            memberships[path].add(demo)
            reasons[path].add("page")
    for name, path in aliases.items():
        by_module[name] = [path]
    inner_sources = defaultdict(set)
    for path, ast in syntax.items():
        if not any(not declaration["context"]
                   for declaration in ast["declarations"]):
            for context, _ in ast["modules"]:
                if len(context) == 1 and context[0] != stats.module(path):
                    inner_sources[context[0]].add(stats.module(path))
    for name, providers in inner_sources.items():
        if (len(providers) == 1 and name not in aliases
                and all(source_kind((ROOT / path).read_bytes())
                        == "expect_driver" for path in by_module[name])):
            by_module[name] = by_module[next(iter(providers))]

    done = set()
    while set(memberships) - done:
        path = min(set(memberships) - done)
        done.add(path)
        contents = raw.setdefault(path, (ROOT / path).read_bytes())
        header = test_header(contents)
        dependencies = header_sources(header, by_name)
        if source_kind(contents) != "expect_driver":
            if path not in syntax:
                sha = digest(contents)
                syntax[path] = parser.parse({sha: (Path(path).suffix,
                                                   contents)})[sha]
            for module in syntax[path]["dependencies"]:
                dependencies.update(by_module.get(module, []))
        else:
            for name in re.findall(rb'^\s*#(?:use|load)\s+"([^\"]+)"',
                                   contents, re.M):
                basename = Path(name.decode()).with_suffix(".ml").name
                if basename in by_name:
                    dependencies.add(by_name[basename])
        dependencies.update(by_module.get(stats.module(path), []))
        for dependency in dependencies - {path}:
            old = len(memberships[dependency])
            memberships[dependency].update(memberships[path])
            reasons[dependency].add("source_or_header_dependency")
            if dependency in done and len(memberships[dependency]) != old:
                done.remove(dependency)

    rejection_sources = defaultdict(set)
    for path in memberships:
        for rejected in header_rejections(test_header(raw[path]), by_name):
            rejection_sources[rejected].add(path)
    records = {}
    for path in sorted(memberships):
        contents = raw[path]
        dependencies = set()
        for module in syntax.get(path, {}).get("dependencies", []):
            candidates = by_module.get(module, [])
            interfaces = [p for p in candidates if p.endswith(".mli")]
            dependencies.update(interfaces or candidates)
        for unit in opens[path]:
            dependencies.update(by_module[unit])
        own_interface = str(Path(path).with_suffix(".mli"))
        if path.endswith(".ml") and own_interface in memberships:
            dependencies.add(own_interface)
        dependencies.discard(path)
        dependencies.intersection_update(memberships)
        header = test_header(contents)
        records[path] = {
            "source": path, "source_sha256": digest(contents),
            "module": stats.module(path), "demos": sorted(memberships[path]),
            "discovery": sorted(reasons[path]), "kind": (
                "rejected" if rejection_sources[path]
                and source_kind(contents) != "expect_driver"
                else source_kind(contents)),
            "rejection_expectations": sorted(rejection_sources[path]),
            "dependencies": sorted(dependencies),
            "opens": sorted(opens[path]), "flags": [],
            "header_flag_assignments": re.findall(
                r'\bflags\s*\+?=\s*"([^\"]*)"', header),
            "has_test_header": bool(header),
            "driver_required": bool(header) and bool(re.search(
                r"\b(?:expect|run-expect|script|setup-\w+-build-env)\b",
                header)),
        }
    return {"format_version": 1, "demos": selected,
            "census_roots": {demo: catalogue["census"][demo]["roots"]
                             for demo in selected},
            "sources": records, "parser_key": parser.key,
            "revision": source.commit}


def compiler_identity(prefix):
    compiler = prefix / "bin/ocamlopt.opt"
    if not compiler.is_file():
        raise ValueError(f"run make install first: missing {compiler}")
    stdlib = prefix / "lib/ocaml"
    interfaces = {path.name: digest(path.read_bytes())
                  for path in sorted(stdlib.glob("*.cmi"))}
    return compiler, {"compiler": str(compiler),
                      "compiler_sha256": digest(compiler.read_bytes()),
                      "installed_interfaces_sha256": identity(interfaces)}


def run_corpus(args):
    output = args.output.resolve()
    prefix = args.prefix.resolve()
    compiler, compiler_info = compiler_identity(prefix)
    inventory = discover(prefix, args.demo)
    write_json(output / "inventory.json", inventory)
    wrapper, solver = install_wrapper(output, args.solver)
    solver_info = {"solver": str(solver),
                   "solver_sha256": digest(solver.read_bytes()),
                   "solver_version": subprocess.check_output(
                       [str(solver), "-version"], text=True).strip()}
    configuration = dict(compiler_info, **solver_info,
                         timeout=args.timeout, smt_timeout=args.smt_timeout,
                         source_paths_sha256=identity(
                             sorted(inventory["sources"])),
                         extractor_sha256=digest(Path(__file__).read_bytes()))
    configuration_key = identity(configuration)
    write_json(output / "configuration.json", configuration)
    write_json(output / "configurations" / (configuration_key + ".json"),
               configuration)
    build = output / "build" / configuration_key[:16]
    build.mkdir(parents=True, exist_ok=True)
    sources = inventory["sources"]
    results = {}
    pending = set()
    for path, record in sources.items():
        target = build / Path(path).name
        contents = (ROOT / path).read_bytes()
        if not target.exists() or target.read_bytes() != contents:
            target.write_bytes(contents)
        if record["kind"] == "expect_driver":
            results[path] = {
                "source": path, "status": "pending_driver",
                "reason": "expect phrases require the test driver"}
        else:
            pending.add(path)
    environment = dict(os.environ, VOX_VERIFY_CACHE="",
                       VOX_SMT_CORPUS_INVENTORY=str(output / "inventory.json"))

    def compile_source(path):
        record = sources[path]
        artifacts = {}
        for dependency in record["dependencies"]:
            cmi = (build / Path(dependency).name).with_suffix(".cmi")
            if not cmi.is_file():
                return {
                    "source": path, "status": "blocked",
                    "reason": f"missing dependency interface: {dependency}"}
            artifacts[dependency] = digest(cmi.read_bytes())
        command = [str(compiler), "-c", "-stop-after", "typing",
                   "-extension", "refinement_types", "-dcanonical-ids",
                   "-smt-solver", str(wrapper), "-I", str(build)]
        if path.startswith("verification/library/"):
            command.append("-vox-library")
        if args.smt_timeout:
            command += ["-smt-timeout", str(args.smt_timeout)]
        command += record["flags"]
        for unit in record["opens"]:
            command += ["-open", unit]
        command.append(Path(path).name)
        key = identity({"source": record["source_sha256"],
                        "configuration": configuration_key,
                        "dependencies": artifacts, "command": command})
        checkpoint = output / "sources" / (digest(path.encode()) + ".json")
        cmi = (build / Path(path).name).with_suffix(".cmi")
        if checkpoint.exists():
            previous = json.loads(checkpoint.read_text())
            if (previous.get("key") == key
                    and previous.get("status") in ("verified", "rejected")
                    and not args.retry
                    and (previous["status"] == "rejected"
                         or cmi.is_file() and digest(cmi.read_bytes())
                         == previous.get("cmi_sha256"))):
                return dict(previous, resumed=True)
        run_id = key[:20] + "-" + uuid.uuid4().hex[:8]
        log = output / "logs" / (run_id + ".log")
        log.parent.mkdir(parents=True, exist_ok=True)
        started = time.monotonic()
        result = {"source": path, "key": key, "run_id": run_id,
                  "source_sha256": record["source_sha256"],
                  "dependency_interfaces": artifacts, "command": command,
                  "cwd": str(build), "log": str(log.relative_to(output))}
        env = dict(environment, VOX_SMT_CORPUS_SOURCE=path,
                   VOX_SMT_CORPUS_CONFIG=configuration_key,
                   VOX_SMT_CORPUS_RUN_ID=run_id)
        with log.open("wb") as stream:
            process = subprocess.Popen(command, cwd=build, env=env,
                                       stdout=stream, stderr=subprocess.STDOUT,
                                       start_new_session=True)
            try:
                code = process.wait(timeout=args.timeout or None)
                result["returncode"] = code
                result["status"] = ("verified" if code == 0 else
                                    "rejected" if code == 2
                                    and record["kind"] == "rejected"
                                    else "failed")
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
                result["status"] = "timeout"
        result["elapsed_seconds"] = time.monotonic() - started
        if result["status"] == "verified" and cmi.is_file():
            result["cmi_sha256"] = digest(cmi.read_bytes())
        write_json(checkpoint, result)
        write_json(output / "runs" / (run_id + ".json"), result)
        return result

    running = {}
    last_progress = 0
    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        while pending or running:
            for path in sorted(pending):
                dependencies = sources[path]["dependencies"]
                if all(dependency in results for dependency in dependencies):
                    failed = [dependency for dependency in dependencies
                              if results[dependency]["status"] != "verified"]
                    if failed:
                        results[path] = {"source": path, "status": "blocked",
                                         "dependencies": failed}
                        pending.remove(path)
                    elif len(running) < args.jobs:
                        running[executor.submit(compile_source, path)] = path
                        pending.remove(path)
            if running:
                completed, _ = wait(running, timeout=10,
                                    return_when=FIRST_COMPLETED)
                for future in completed:
                    path = running.pop(future)
                    try:
                        results[path] = future.result()
                    except Exception as error:
                        results[path] = {"source": path, "status": "failed",
                                         "exception": repr(error)}
            elif pending:
                for path in pending:
                    results[path] = {"source": path, "status": "blocked",
                                     "reason": "cyclic dependencies",
                                     "dependencies": sources[path]
                                     ["dependencies"]}
                pending.clear()
            if time.monotonic() - last_progress >= 20 or not pending:
                counts = dict(Counter(r["status"] for r in results.values()))
                print(json.dumps({"completed": len(results),
                                  "total": len(sources), "status": counts,
                                  "running": sorted(running.values())}),
                      flush=True)
                write_json(output / "coverage.json", results)
                last_progress = time.monotonic()
    write_json(output / "coverage.json", results)
    collect(output)
    return int(any(r["status"] in ("failed", "timeout", "blocked")
                   for r in results.values()))


def collect(output, jobs=4):
    sessions = []
    queries = []
    historical_queries = []
    coverage_path = output / "coverage.json"
    coverage = (json.loads(coverage_path.read_text())
                if coverage_path.exists() else {})
    active_runs = {record.get("run_id") for record in coverage.values()}
    drivers = [json.loads(path.read_text())
               for path in sorted((output / "drivers").glob("*.json"))]
    active_runs.update(driver.get("run_id") for driver in drivers)
    directories = [path.parent for path in
                   sorted((output / "sessions").glob("*/session.json"))]
    with ProcessPoolExecutor(max_workers=jobs) as executor:
        for session, extracted in executor.map(index_session, directories):
            run_id = session.get("run_id")
            active = (not coverage or not run_id or run_id in active_runs
                      or run_id.startswith("test-library-"))
            session["active"] = active
            sessions.append(session)
            (queries if active else historical_queries).extend(extracted)
    for name, records in (("sessions.jsonl", sessions),
                          ("queries.jsonl", queries),
                          ("historical-queries.jsonl", historical_queries)):
        temporary = output / (name + ".tmp")
        with temporary.open("w") as stream:
            for record in records:
                stream.write(json.dumps(record, sort_keys=True) + "\n")
        temporary.replace(output / name)
    by_source = defaultdict(Counter)
    for query in queries:
        by_source[query["source"] or "unattributed"][query["result"]
                                                   or "missing"] += 1
    summary = {"sessions": len(sessions), "queries": len(queries),
               "historical_queries": len(historical_queries),
               "results": dict(Counter(q["result"] or "missing"
                                       for q in queries)),
               "commands": dict(Counter(q["command"] for q in queries)),
               "by_source": {p: dict(v) for p, v in by_source.items()}}
    if coverage_path.exists():
        inventory = json.loads((output / "inventory.json").read_text())
        inventory = inventory["sources"]
        session_counts = Counter(s.get("source") for s in sessions
                                 if s["active"])
        for path, record in coverage.items():
            if (record["status"] == "failed" and record.get("returncode") == 2
                    and inventory[path]["kind"] == "rejected"):
                record["status"] = "rejected"
                record["rejection_expectations"] = inventory[path][
                    "rejection_expectations"]
            record["solver_results"] = dict(by_source[path])
            record["query_count"] = sum(by_source[path].values())
            record["session_count"] = session_counts[path]
        for driver in drivers:
            source = driver["source"]
            if source in coverage:
                coverage[source]["driver"] = driver
                if coverage[source]["status"] == "pending_driver":
                    coverage[source]["status"] = driver["status"]
        summary["coverage"] = dict(Counter(record["status"]
                                            for record in coverage.values()))
        summary["verified_with_zero_queries"] = sum(
            record["status"] == "verified" and not record["query_count"]
            for record in coverage.values())
        write_json(coverage_path, coverage)
        by_demo = {}
        for path, record in inventory.items():
            for demo in record["demos"]:
                entry = by_demo.setdefault(demo, {"sources": 0, "queries": 0,
                                                  "coverage": Counter()})
                entry["sources"] += 1
                entry["queries"] += sum(by_source[path].values())
                entry["coverage"][coverage[path]["status"]] += 1
        summary["demos"] = by_demo
    write_json(output / "summary.json", summary)
    print(json.dumps({key: value for key, value in summary.items()
                      if key not in ("by_source", "demos")}), flush=True)
    return summary


def run_driver(args):
    command = args.arguments
    if command and command[0] == "--":
        command = command[1:]
    if not command:
        raise ValueError("driver requires a command after --")
    wrapper, solver = install_wrapper(args.output, args.solver)
    run_id = "driver-" + uuid.uuid4().hex
    source = args.source
    path = ROOT / source
    record = {"source": source, "command": command, "cwd": os.getcwd(),
              "source_sha256": digest(path.read_bytes()), "run_id": run_id,
              "solver": str(solver), "started_ns": time.time_ns()}
    environment = dict(os.environ, VOX_VERIFY_CACHE="",
                       VOX_SMT_CORPUS_SOURCE=source,
                       VOX_SMT_CORPUS_RUN_ID=run_id,
                       VOX_SMT_CORPUS_SOLVER=str(wrapper),
                       PATH=(str(wrapper.parent) + os.pathsep
                             + os.environ["PATH"]))
    started = time.monotonic()
    process = subprocess.Popen(command, env=environment,
                               start_new_session=True)
    try:
        code = process.wait(timeout=args.timeout or None)
        record.update(returncode=code,
                      status="driver_passed" if code == 0 else "driver_failed")
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.wait()
        code = 124
        record["status"] = "driver_timeout"
    record["elapsed_seconds"] = time.monotonic() - started
    write_json(args.output / "drivers" / (digest(source.encode()) + ".json"),
               record)
    collect(args.output)
    return code


def link_file(source, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.is_symlink():
        if target.resolve() == source.resolve():
            return
        target.unlink()
    elif target.exists():
        raise ValueError(f"cannot replace driver support file: {target}")
    target.symlink_to(source.resolve())


def driver_environment(output, prefix, solver):
    wrapper, _ = install_wrapper(output, solver)
    sysroot = output / "driver-sysroot"
    library = prefix / "lib/ocaml"
    link_file(library, sysroot / "stdlib")
    link_file(library / "compiler-libs", sysroot / "toplevel")
    for path in library.iterdir():
        link_file(path, sysroot / "runtime" / path.name)
        if path.is_dir():
            link_file(path, sysroot / "otherlibs" / path.name)
    for name in ("ocamlrun", "ocamlrund", "ocamlruni"):
        link_file(prefix / "bin" / name, sysroot / "runtime" / name)
    for name in ("ocamllex", "ocamlyacc", "ocamldoc", "ocamldebug",
                 "ocamlobjinfo", "ocamlmklib"):
        directory = {"ocamllex": "lex", "ocamlyacc": "yacc",
                     "ocamldoc": "ocamldoc", "ocamldebug": "debugger"}
        link_file(prefix / "bin" / name,
                  sysroot / directory.get(name, "tools") / name)
    proxies = {}
    for name in ("ocamlc.opt", "ocamlopt.opt"):
        proxy = output / "bin" / name
        command = [sys.executable, str(Path(__file__).resolve()), "compiler",
                   str(prefix / "bin" / name)]
        write_executable(proxy, "#!/bin/sh\nexec "
                         + shlex.join(command) + ' "$@"\n')
        proxies[name] = str(proxy)
    expect_tools = ROOT / "_build/main/oxcaml/testsuite/tools"
    environment = dict(
        os.environ, VOX_VERIFY_CACHE="", OCAMLSRCDIR=str(sysroot),
        CAML_LD_LIBRARY_PATH=str(library / "stublibs"),
        OCAMLTEST_OCAMLC_OPT=proxies["ocamlc.opt"],
        OCAMLTEST_OCAMLOPT_OPT=proxies["ocamlopt.opt"],
        OCAMLTEST_OCAMLC_BYTE=proxies["ocamlc.opt"],
        OCAMLTEST_OCAMLOPT_BYTE=proxies["ocamlopt.opt"],
        OCAMLTEST_OCAML=str(prefix / "bin/ocaml"),
        OCAMLTEST_EXPECT=str(expect_tools / "expect.exe"),
        OCAMLTEST_EXPECTNAT=str(expect_tools / "expectnat.exe"),
        OCAMLTEST_VOX_TEST_LIBRARY=str(output / "test-library"),
        VOX_SMT_CORPUS_INVENTORY=str(output / "inventory.json"),
        VOX_SMT_CORPUS_SOLVER=str(wrapper),
        VOX_SLOW_PROOFS=str(output / "slow-proofs"),
        PATH=str(wrapper.parent) + os.pathsep + os.environ["PATH"],
        MAKE="make", SORT="sort", TERM="dumb")
    return environment, proxies


def run_drivers(args):
    output = args.output
    (output / "driver-runs").mkdir(parents=True, exist_ok=True)
    prefix = args.prefix.resolve()
    inventory = json.loads((output / "inventory.json").read_text())
    tests = args.test or [path for path, record in inventory["sources"].items()
                          if record["has_test_header"]]
    planner = ROOT / "_build/default/ocamltest/main.exe"
    if not planner.is_file():
        raise ValueError(f"missing test driver: {planner}")
    environment, proxies = driver_environment(output, prefix, args.solver)
    _, compiler_info = compiler_identity(prefix)
    native = prefix / "bin/ocamlc.opt"
    compiler_info["ocamlc_sha256"] = digest(native.read_bytes())
    compiler_info["expect_sha256"] = digest(
        Path(environment["OCAMLTEST_EXPECT"]).read_bytes())
    config = identity(compiler_info)
    write_json(output / "configurations" / (config + ".json"), compiler_info)
    environment["VOX_SMT_CORPUS_CONFIG"] = config
    if not args.skip_library:
        command = ["sh", str(ROOT / "testsuite/vox-test-library.sh"),
                   str(output / "test-library"), str(planner),
                   proxies["ocamlc.opt"], proxies["ocamlopt.opt"],
                   str(prefix / "lib/ocaml"),
                   *[str(ROOT / test) for test in tests]]
        print("Building verified test library", flush=True)
        library_log = output / "test-library.log"
        env = dict(environment, VOX_BUILD_JOBS=str(args.jobs),
                   VOX_SMT_CORPUS_RUN_ID="test-library-" + uuid.uuid4().hex)
        with library_log.open("wb") as log:
            code = subprocess.call(command, env=env,
                                   stdout=log, stderr=subprocess.STDOUT)
        write_json(output / "test-library.json", {
            "command": command, "returncode": code,
            "log": str(library_log.relative_to(output))})
        print(f"Test library exited {code}; running every selected driver",
              flush=True)

    def execute(source):
        run_id = "driver-" + uuid.uuid4().hex
        command = [str(planner), "-keep-test-dir-on-success",
                   "-timeout", str(int(args.timeout)), str(ROOT / source)]
        log = output / "logs" / (run_id + ".log")
        env = dict(environment, VOX_SMT_CORPUS_SOURCE=source,
                   VOX_SMT_CORPUS_RUN_ID=run_id,
                   OCAMLTESTDIR=str(output / "driver-runs" / run_id))
        record = {"source": source, "run_id": run_id, "command": command,
                  "source_sha256": digest((ROOT / source).read_bytes()),
                  "configuration": config, "log": str(log.relative_to(output))}
        started = time.monotonic()
        log.parent.mkdir(parents=True, exist_ok=True)
        with log.open("wb") as stream:
            process = subprocess.Popen(command, env=env, stdout=stream,
                                       stderr=subprocess.STDOUT,
                                       start_new_session=True)
            try:
                code = process.wait(timeout=args.timeout * 4 or None)
                record.update(returncode=code, status=(
                    "driver_passed" if code == 0 else "driver_failed"))
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
                record["status"] = "driver_timeout"
        record["elapsed_seconds"] = time.monotonic() - started
        write_json(output / "drivers" / (digest(source.encode()) + ".json"),
                   record)
        write_json(output / "runs" / (run_id + ".json"), record)
        print(source + ": " + record["status"], flush=True)
        return record

    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        results = list(executor.map(execute, tests))
    collect(output)
    return int(any(record["status"] != "driver_passed" for record in results))


def pack(output, archive):
    summary = json.loads((output / "summary.json").read_text())
    queries = [json.loads(line)
               for line in (output / "queries.jsonl").read_text().splitlines()]
    manifest = {
        "format_version": 1,
        "summary": summary,
        "unique_queries": len({query["query_sha256"] for query in queries}),
        "active_index": "queries.jsonl",
        "historical_index": "historical-queries.jsonl",
        "session_input": "sessions/<session>/input.smt2",
        "replay": "Read bytes [replay_start, replay_end) from session_input.",
        "historical_policy": "All historical session streams are retained.",
    }
    write_json(output / "artifact-manifest.json", manifest)
    names = [path for path in output.iterdir() if path.is_file()]
    for name in ("sessions", "sources", "runs", "configurations", "drivers",
                 "logs"):
        if (output / name).is_dir():
            names.append(output / name)
    archive.parent.mkdir(parents=True, exist_ok=True)
    temporary = archive.with_name(archive.name + ".tmp")
    with tarfile.open(temporary, "w:gz", compresslevel=3) as stream:
        for path in sorted(names):
            stream.add(path, arcname="vox-smt-corpus/" + path.name)
    temporary.replace(archive)
    checksum = hashlib.sha256()
    with archive.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            checksum.update(chunk)
    report = {"archive": str(archive), "bytes": archive.stat().st_size,
              "sha256": checksum.hexdigest(), "queries": len(queries),
              "unique_queries": manifest["unique_queries"]}
    write_json(archive.with_suffix(archive.suffix + ".json"), report)
    print(json.dumps(report), flush=True)


def main():
    if len(sys.argv) >= 2 and sys.argv[1] == "wrap":
        return wrap(sys.argv[2:])
    if len(sys.argv) >= 3 and sys.argv[1] == "compiler":
        arguments = [arg for arg in sys.argv[3:]
                     if arg != "-smt-assume-verified"]
        os.execv(sys.argv[2], [sys.argv[2], *arguments])
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    for name in ("inventory", "run", "wrapper", "collect", "driver", "drivers",
                 "pack"):
        command = commands.add_parser(name)
        command.add_argument("--output", type=Path,
                             default=ROOT / "_build/vox-smt-corpus")
        if name in ("inventory", "run", "drivers"):
            command.add_argument("--prefix", type=Path,
                                 default=ROOT / "_install")
        if name in ("inventory", "run"):
            command.add_argument("--demo", action="append")
        if name in ("wrapper", "run", "driver", "drivers"):
            command.add_argument("--solver", default="z3")
        if name == "run":
            command.add_argument("--jobs", type=int, default=4)
            command.add_argument("--timeout", type=float, default=600)
            command.add_argument("--smt-timeout", type=int, default=0)
            command.add_argument("--retry", action="store_true")
        if name == "driver":
            command.add_argument("--source", required=True)
            command.add_argument("--timeout", type=float, default=600)
            command.add_argument("arguments", nargs=argparse.REMAINDER)
        if name == "drivers":
            command.add_argument("--jobs", type=int, default=8)
            command.add_argument("--timeout", type=float, default=900)
            command.add_argument("--skip-library", action="store_true")
            command.add_argument("--test", action="append")
        if name == "collect":
            command.add_argument("--jobs", type=int, default=4)
        if name == "pack":
            command.add_argument("--archive", type=Path)
    args = parser.parse_args()
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=True)
    if args.command == "inventory":
        inventory = discover(args.prefix.resolve(), args.demo)
        write_json(args.output / "inventory.json", inventory)
        kinds = Counter(record["kind"]
                        for record in inventory["sources"].values())
        print(json.dumps({"demos": len(inventory["demos"]),
                          "sources": len(inventory["sources"]),
                          "kinds": dict(kinds)}))
    elif args.command == "wrapper":
        wrapper, _ = install_wrapper(args.output, args.solver)
        print(wrapper)
    elif args.command == "collect":
        collect(args.output, args.jobs)
    elif args.command == "driver":
        return run_driver(args)
    elif args.command == "drivers":
        return run_drivers(args)
    elif args.command == "pack":
        pack(args.output, args.archive or args.output.with_suffix(".tar.gz"))
    else:
        return run_corpus(args)
    return 0


if __name__ == "__main__":
    sys.exit(main())
