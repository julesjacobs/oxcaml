#!/usr/bin/env python3
"""Replay every captured SMT call and enforce a per-call performance gate."""

import argparse
from collections import Counter, OrderedDict
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import platform
import secrets
import selectors
import shutil
import statistics
import subprocess
import threading
import time

from smt_corpus import command_name, expressions
from smt_reference import ReferenceVerifier, eligible


def changed_result_queries(script, original, replay):
    if original != "unknown" or replay not in ("sat", "unsat"):
        return []
    queries = []
    readonly = {"get-info", "get-option", "get-assertions", "get-assignment",
                "get-value", "get-model", "get-unsat-core",
                "get-unsat-assumptions", "echo"}
    for start, end, command in expressions(script):
        arguments = [part for _, _, part in expressions(command[1:-1])]
        if (len(arguments) == 3
                and arguments[:2] == [b"set-option", b":rlimit"]):
            continue
        if command_name(command) not in readonly:
            break
        if arguments == [b"get-info", b":reason-unknown"]:
            queries.append(dict(command=command.decode(), command_start=start,
                                command_end=end, original_result=original,
                                replay_result=replay))
    return queries


def protocol_result(output, expected_checks, regions=()):
    responses = [e for _, _, e in expressions(bytes(output))]
    boundaries = {}
    for index, (before, after, _) in enumerate(regions):
        for marker, entering in ((before, True), (after, False)):
            boundaries[marker] = boundaries[b'"' + marker + b'"'] = (
                index, entering)
    active = None
    seen = []
    reported = set()
    answers, errors, changed_errors = [], [], []
    expected_error = b'(error "Reason unavailable without an unknown result")'
    for response in responses:
        if response in boundaries:
            index, entering = boundaries[response]
            if (entering and active is not None
                    or not entering and active != index):
                errors.append("Invalid result-query response boundary")
            active = index if entering else None
            seen.append((index, entering))
        elif response in (b"sat", b"unsat", b"unknown"):
            answers.append(response.decode())
        elif command_name(response) == "error":
            if (active is not None and active not in reported
                    and response == expected_error):
                reported.add(active)
                changed_errors.append(dict(regions[active][2],
                                           response=response.decode()))
            else:
                errors.append(response.decode(errors="replace"))
    expected = [(index, entering) for index in range(len(regions))
                for entering in (True, False)]
    if seen != expected:
        errors.append("Missing or reordered result-query response boundaries")
    if errors or len(answers) != expected_checks:
        details = errors or [output.decode(errors="replace")]
        return "error", details, changed_errors
    return (answers[-1] if answers else "complete"), [], changed_errors


class Solver:
    def __init__(self, executable, timeout):
        self.executable = executable
        self.timeout = timeout
        self.process = None

    def close(self):
        if self.process is not None:
            self.process.kill()
            self.process.communicate()
            self.process = None

    def memory(self):
        if self.process is None:
            return {}
        try:
            lines = Path(f"/proc/{self.process.pid}/status").read_text().splitlines()
        except OSError:
            return {}
        fields = {}
        for line in lines:
            parts = line.split()
            if parts and parts[0] in ("VmRSS:", "VmHWM:"):
                key = "rss_bytes" if parts[0] == "VmRSS:" else "peak_rss_bytes"
                fields[key] = int(parts[1]) * 1024
        return fields

    def run(self, script, *, expected_checks=1):
        result, seconds, errors, _ = self._run(script, expected_checks)
        return result, seconds, errors

    def run_warm(self, script, original, replay):
        queries = changed_result_queries(script, original, replay)
        return self._run(script, 1, queries)

    def _run(self, script, expected_checks, result_queries=()):
        if self.process is None:
            self.process = subprocess.Popen(
                [self.executable, "-in", "-smt2"], stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, bufsize=0)
            os.set_blocking(self.process.stdin.fileno(), False)
            os.set_blocking(self.process.stdout.fileno(), False)
        output = bytearray()
        marker = b"vox-benchmark-" + secrets.token_hex(16).encode()
        regions, parts, begin = [], [], 0
        for index, query in enumerate(result_queries):
            before = marker + f"-query-{index}-begin".encode()
            after = marker + f"-query-{index}-end".encode()
            start, end = query["command_start"], query["command_end"]
            parts.extend((script[begin:start], b'\n(echo "' + before + b'")\n',
                          script[start:end], b'\n(echo "' + after + b'")\n'))
            regions.append((before, after, query))
            begin = end
        script = b"".join(parts) + script[begin:]
        started = time.perf_counter()
        deadline = started + self.timeout
        try:
            data = memoryview(script + b'\n(echo "' + marker + b'")\n')
            with selectors.DefaultSelector() as selector:
                selector.register(self.process.stdin, selectors.EVENT_WRITE)
                selector.register(self.process.stdout, selectors.EVENT_READ)
                while (marker + b"\n" not in output
                       and b'"' + marker + b'"\n' not in output):
                    remaining = deadline - time.perf_counter()
                    if remaining <= 0:
                        raise TimeoutError
                    events = selector.select(remaining)
                    if not events:
                        raise TimeoutError
                    for key, _ in events:
                        if key.fileobj is self.process.stdin:
                            written = self.process.stdin.write(data[:65536])
                            if written is None:
                                continue
                            if not written:
                                raise BrokenPipeError
                            data = data[written:]
                            if not data:
                                selector.unregister(self.process.stdin)
                        else:
                            chunk = self.process.stdout.read(65536)
                            if chunk is None:
                                continue
                            if not chunk:
                                raise BrokenPipeError
                            output.extend(chunk)
            elapsed = time.perf_counter() - started
            result, errors, changed_errors = protocol_result(
                output, expected_checks, regions)
            return result, elapsed, errors, changed_errors
        except TimeoutError:
            self.close()
            return "timeout", time.perf_counter() - started, [], []
        except (BrokenPipeError, OSError) as error:
            self.close()
            return "crash", time.perf_counter() - started, [str(error)], []


def scripts(corpus, replay_scripts=None):
    sessions = OrderedDict()
    ignored = {"check-sat", "check-sat-assuming", "check-sat-using",
               "get-value", "get-model", "get-info", "get-unsat-core",
               "get-unsat-assumptions", "echo", "exit"}
    try:
        with (corpus / "queries.jsonl").open() as stream:
            for line in stream:
                query = json.loads(line)
                if replay_scripts is not None:
                    digest = query.get("replay_script_sha256", "")
                    if (len(digest) != 64
                            or any(c not in "0123456789abcdef" for c in digest)):
                        raise ValueError("missing or invalid replay script hash")
                    script = (replay_scripts / (digest + ".smt2")).read_bytes()
                    if hashlib.sha256(script).hexdigest() != digest:
                        raise ValueError("replay script hash mismatch: " + digest)
                    yield query, script
                    continue
                session = query["session"]
                if session not in sessions:
                    sessions[session] = (corpus / "sessions" / session
                                         / "input.smt2").open("rb")
                    if len(sessions) > 4:
                        sessions.popitem(last=False)[1].close()
                sessions.move_to_end(session)
                incoming = sessions[session]
                begin = query.get("replay_start", 0)
                incoming.seek(begin)
                data = incoming.read(query["command_end"] - begin)
                prefix = []
                for start, _, command in expressions(data):
                    name = command_name(command)
                    if name == "reset":
                        prefix = []
                    if name not in ignored or begin + start == query["command_start"]:
                        prefix.append(command)
                script = b"\n".join(prefix) + b"\n"
                if not script.startswith(b"(reset)"):
                    script = b"(reset)\n" + script
                yield query, script
    finally:
        for incoming in sessions.values():
            incoming.close()


def measurement_script(script, resource_policy):
    if resource_policy == "preserve":
        return script
    if resource_policy != "unlimited":
        raise ValueError("unknown resource policy: " + resource_policy)
    commands = list(expressions(script))
    checks = [start for start, _, command in commands
              if command_name(command) in
              ("check-sat", "check-sat-assuming", "check-sat-using")]
    if len(checks) != 1:
        raise ValueError("a measurement must contain exactly one check")
    parts = [b"(set-option :rlimit 0)\n"]
    previous = 0
    for start, end, command in commands:
        if command_name(command) != "set-option":
            continue
        tokens = [part for _, _, part in expressions(command[1:-1])]
        if len(tokens) == 3 and tokens[:2] == [b"set-option", b":rlimit"]:
            parts.extend((script[previous:start], b"(set-option :rlimit 0)"))
            previous = end
    parts.append(script[previous:])
    return b"".join(parts)


def measure(index, query, script, solvers, repeats, instant_ms,
            resource_policy="preserve", reference_verifier=None):
    row = dict(query, query_sha256=hashlib.sha256(script).hexdigest())
    script = measurement_script(script, resource_policy)
    row["measurement_sha256"] = hashlib.sha256(script).hexdigest()
    runs = {name: [] for name in solvers}
    for repeat in range(repeats):
        order = ("ocaml", "z3") if (index + repeat) % 2 else ("z3", "ocaml")
        for name in order:
            result, seconds, errors = solvers[name].run(script)
            sample = dict(result=result, seconds=seconds, errors=errors)
            sample.update(solvers[name].memory())
            runs[name].append(sample)
    row["runs"] = runs
    if reference_verifier is not None:
        captured = [query.get("result"), *query.get("captured_results", [])]
        row["reference_evidence"] = reference_verifier.verify(
            script, runs, captured)
    return summarize(query, row, runs, instant_ms)


def summarize(query, row, runs, instant_ms):
    timing = {name: statistics.median(run["seconds"] for run in samples)
              for name, samples in runs.items()}
    row["median_seconds"] = timing
    answers = {name: {run["result"] for run in samples}
               for name, samples in runs.items()}
    correct = (len(answers["z3"]) == 1
               and answers["z3"] <= {"sat", "unsat"}
               and answers["ocaml"] == answers["z3"])
    original = query.get("result")
    captured = [original, *query.get("captured_results", [])]
    evidence = row.get("reference_evidence") or {}
    verified = (evidence.get("verified") is True
                and evidence.get("query_sha256")
                == row.get("measurement_sha256")
                and eligible(runs, captured))
    row["independently_verified_result"] = "unsat" if verified else None
    row["original_matches_replay"] = answers["z3"] == {original}
    effective = {"unsat"} if verified else answers["z3"]
    correct = correct or verified
    if any(answer in ("sat", "unsat") and effective != {answer}
           for answer in captured):
        correct = False
    row["correct"] = correct
    row["performance_pass"] = (timing["ocaml"] <= instant_ms / 1000
                               or timing["ocaml"] <= 2 * timing["z3"])
    row["pass"] = correct and row["performance_pass"]
    return row


def session_inputs(corpus, limit=None):
    sessions = OrderedDict()
    with (corpus / "queries.jsonl").open() as stream:
        for index, line in enumerate(stream):
            if limit is not None and index >= limit:
                break
            query = json.loads(line)
            sessions.setdefault(query["session"], []).append(query)
    with (corpus / "sessions.jsonl").open() as stream:
        manifests = {row["id"]: row for line in stream
                     if (row := json.loads(line))["id"] in sessions}
    for session, queries in sessions.items():
        if Path(session).name != session or session in (".", ".."):
            raise ValueError("invalid session identifier")
        data = (corpus / "sessions" / session / "input.smt2").read_bytes()
        digest = hashlib.sha256(data).hexdigest()
        if digest != manifests[session]["input_sha256"]:
            raise ValueError("session input hash mismatch: " + session)
        outgoing = (corpus / "sessions" / session / "stdout").read_bytes()
        if (hashlib.sha256(outgoing).hexdigest()
                != manifests[session].get("stdout_sha256")):
            raise ValueError("session stdout hash mismatch: " + session)
        answers = [(start, end, response.decode())
                   for start, end, response in expressions(outgoing)
                   if response in (b"sat", b"unsat", b"unknown")]
        selected = {query["command_end"]: query for query in queries}
        if len(selected) != len(queries):
            raise ValueError("duplicate session check offsets: " + session)
        last = max(selected)
        chunks = []
        begin = 0
        checks = []
        for start, end, command in expressions(data):
            name = command_name(command)
            if name not in ("check-sat", "check-sat-assuming", "check-sat-using"):
                if name == "exit" and start < last:
                    raise ValueError("session exits before selected check")
                continue
            checks.append((start, end, command))
        if len(checks) != len(answers):
            raise ValueError("session response alignment mismatch: " + session)
        for ordinal, ((start, end, command), answer) in enumerate(
                zip(checks, answers), 1):
            if end > last:
                break
            query = selected.pop(end, None)
            if query is not None:
                if (start != query["command_start"]
                        or hashlib.sha256(command).hexdigest()
                        != query["command_sha256"]):
                    raise ValueError("session check hash or offset mismatch")
                if (query.get("result") != answer[2]
                        or query.get("ordinal", ordinal) != ordinal
                        or query.get("response_start", answer[0]) != answer[0]
                        or query.get("response_end", answer[1]) != answer[1]):
                    raise ValueError("session check response mismatch")
            chunks.append((query, data[begin:end], begin, end, answer[2]))
            begin = end
        if selected:
            raise ValueError("selected session checks were not found")
        yield session, digest, chunks


def measure_session(index, item, solvers, repeats, instant_ms, resource_policy):
    session, digest, chunks = item
    rows = []
    for query, script, begin, end, _ in chunks:
        if query is None:
            rows.append(None)
        else:
            measured = measurement_script(script, resource_policy)
            rows.append(dict(query, session_input_sha256=digest,
                             measured_start=begin, measured_end=end,
                             query_sha256=hashlib.sha256(script).hexdigest(),
                             measurement_sha256=hashlib.sha256(measured).hexdigest(),
                             runs={name: [] for name in solvers}))
    for repeat in range(repeats):
        for solver in solvers.values():
            solver.close()
        failed = {}
        previous = {name: None for name in solvers}
        original = None
        for position, (query, script, begin, _, captured) in enumerate(chunks):
            measured = measurement_script(script, resource_policy)
            order = ("ocaml", "z3") if (index + repeat + position) % 2 else ("z3", "ocaml")
            for name in order:
                if name in failed:
                    sample = dict(result="session-unavailable", seconds=0,
                                  errors=[failed[name]])
                else:
                    result, seconds, errors, changed_errors = (
                        solvers[name].run_warm(
                            measured, original, previous[name]))
                    original_queries = changed_result_queries(
                        script, original, previous[name])
                    measured_queries = changed_result_queries(
                        measured, original, previous[name])
                    source_offsets = {
                        transformed["command_start"]: source
                        for source, transformed in
                        zip(original_queries, measured_queries)}
                    for response in changed_errors:
                        source = source_offsets[response["command_start"]]
                        response["command_start"] = source["command_start"]
                        response["command_end"] = source["command_end"]
                        response["command_start"] += begin
                        response["command_end"] += begin
                        response["check_position"] = position
                    sample = dict(result=result, seconds=seconds, errors=errors,
                                  changed_result_errors=changed_errors)
                    sample.update(solvers[name].memory())
                    previous[name] = result
                    if result in ("timeout", "crash", "error"):
                        failed[name] = (
                            f"{session}: check {position + 1} returned {result}")
                if query is not None:
                    rows[position]["runs"][name].append(sample)
            original = captured
    for (query, _, _, _, _), row in zip(chunks, rows):
        if row is not None:
            yield summarize(query, row, row["runs"], instant_ms)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--replay-scripts", type=Path,
                        help="hash-checked canonical scripts from corpus capture")
    parser.add_argument("--solver", required=True)
    parser.add_argument("--z3", default="z3")
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--timeout", type=float, default=60)
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--instant-ms", type=float, default=10)
    parser.add_argument("--resource-policy", choices=("preserve", "unlimited"),
                        default="preserve",
                        help="retain rlimit, or disable it equally for timing")
    parser.add_argument("--session-mode", choices=("cold", "warm"),
                        default="cold",
                        help="cold prefixes, or original sessions with prior checks")
    parser.add_argument("--check-reference-cores", action="store_true",
                        help="prove assertion subsets outside timing when Z3 "
                             "is inconclusive; supports flat cold queries")
    parser.add_argument("--prior-reference-evidence", type=Path,
                        action="append", default=[],
                        help="reconstruct and recheck a previous evidence.json")
    parser.add_argument("--limit", type=int)
    parser.add_argument("--jobs", type=int, default=1,
                        help="parallel diagnostic workers; use 1 for timing")
    args = parser.parse_args()
    if args.repeats <= 0 or args.timeout <= 0 or args.jobs <= 0:
        parser.error("repeats, timeout and jobs must be positive")
    if args.check_reference_cores and args.resource_policy != "unlimited":
        parser.error("checked core fallback requires unlimited timing")
    if args.check_reference_cores and args.session_mode != "cold":
        parser.error("checked core fallback requires cold queries")
    if args.prior_reference_evidence and not args.check_reference_cores:
        parser.error("prior evidence requires --check-reference-cores")
    if args.session_mode == "warm" and (args.replay_scripts or args.jobs != 1):
        parser.error("warm sessions require original streams and --jobs 1")
    with (args.corpus / "queries.jsonl").open() as stream:
        expected_calls = sum(1 for line in stream if line.strip())
    args.output.mkdir(parents=True, exist_ok=True)
    original_solver = Path(shutil.which(args.solver) or args.solver).resolve()
    snapshot = (args.output / "solver").resolve()
    shutil.copy2(original_solver, snapshot)
    configuration = {
        "host": platform.node(), "platform": platform.platform(),
        "corpus": str(args.corpus.resolve()),
        "replay_scripts": (str(args.replay_scripts.resolve())
                           if args.replay_scripts is not None else None),
        "corpus_index_sha256": hashlib.file_digest(
            (args.corpus / "queries.jsonl").open("rb"), "sha256").hexdigest(),
        "instant_ms": args.instant_ms, "ratio": 2,
        "timeout_seconds": args.timeout, "repeats": args.repeats,
        "jobs": args.jobs,
        "cpu_affinity": (sorted(os.sched_getaffinity(0))
                         if hasattr(os, "sched_getaffinity") else None),
        "timing_mode": "serial" if args.jobs == 1 else "parallel-diagnostic",
        "resource_policy": args.resource_policy,
        "resource_policy_scope": (
            "disable rlimit during ingestion and solving; preserve timeouts"
            if args.resource_policy == "unlimited" else
            "preserve captured resource limits and timeouts"),
        "session_mode": args.session_mode,
        "checked_core_fallback": args.check_reference_cores,
        "prior_reference_evidence": {
            str(path.resolve()): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in args.prior_reference_evidence},
        "checked_core_timing": "outside all recorded measurements",
        "warm_response_policy": "isolate reason-unknown after changed unknown",
        "timing": "elapsed input write through completion echo; persistent process",
        "memory_observation": "Linux VmRSS and process-lifetime VmHWM bytes; "
                              "sampled after timing; absent when unavailable",
        "solver_path": str(original_solver),
        "solver_sha256": hashlib.sha256(snapshot.read_bytes()).hexdigest(),
        "z3_path": str(Path(shutil.which(args.z3) or args.z3).resolve()),
        "z3_sha256": hashlib.sha256(
            Path(shutil.which(args.z3) or args.z3).read_bytes()).hexdigest(),
        "harness_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
    }
    for name, executable in (("solver", snapshot), ("z3", args.z3)):
        configuration[name + "_version"] = subprocess.check_output(
            [str(executable), "-version"], text=True, timeout=10).strip()
    (args.output / "configuration.json").write_text(
        json.dumps(configuration, indent=2) + "\n")
    reference_verifier = (ReferenceVerifier(
        snapshot, args.z3, args.output / "reference-evidence", args.timeout,
        args.prior_reference_evidence)
        if args.check_reference_cores else None)
    worker = threading.local()
    solver_groups = []
    def run(item):
        if not hasattr(worker, "solvers"):
            worker.solvers = {
                name: Solver(exe, args.timeout)
                for name, exe in (("ocaml", str(snapshot)), ("z3", args.z3))}
            solver_groups.append(worker.solvers)
        index, (query, script) = item
        return measure(index, query, script, worker.solvers, args.repeats,
                       args.instant_ms, args.resource_policy,
                       reference_verifier)
    def inputs():
        for index, item in enumerate(scripts(args.corpus, args.replay_scripts)):
            if args.limit is not None and index >= args.limit:
                break
            yield index, item
    def results():
        if args.session_mode == "warm":
            solvers = {name: Solver(exe, args.timeout)
                       for name, exe in (("ocaml", str(snapshot)), ("z3", args.z3))}
            solver_groups.append(solvers)
            for index, item in enumerate(session_inputs(args.corpus, args.limit)):
                yield from measure_session(index, item, solvers, args.repeats,
                                           args.instant_ms, args.resource_policy)
        elif args.jobs == 1:
            yield from map(run, inputs())
        else:
            with ThreadPoolExecutor(max_workers=args.jobs) as executor:
                # Bound queued scripts, including results awaiting earlier calls.
                pending = []
                for item in inputs():
                    pending.append(executor.submit(run, item))
                    if len(pending) >= 2 * args.jobs:
                        yield pending.pop(0).result()
                for future in pending:
                    yield future.result()
    counts = Counter()
    completed = False
    failure = None
    last_update = time.monotonic()
    try:
        with (args.output / "calls.jsonl").open("w") as output:
            for row in results():
                counts["calls"] += 1
                counts["pass" if row["pass"] else "fail"] += 1
                if not row["correct"]:
                    counts["correctness_or_completion_fail"] += 1
                elif not row["performance_pass"]:
                    counts["performance_fail"] += 1
                for name, samples in row["runs"].items():
                    counts[name + "_changed_result_errors"] += sum(
                        len(sample.get("changed_result_errors", []))
                        for sample in samples)
                output.write(json.dumps(row, sort_keys=True) + "\n")
                output.flush()
                if time.monotonic() - last_update >= 10:
                    print(json.dumps(counts), flush=True)
                    last_update = time.monotonic()
        completed = counts["calls"] == expected_calls
    except BaseException as error:
        failure = type(error).__name__ + ": " + str(error)
        raise
    finally:
        for solvers in solver_groups:
            for solver in solvers.values():
                solver.close()
        summary = dict(counts, instant_ms=args.instant_ms, ratio=2,
                       repeats=args.repeats, expected_calls=expected_calls,
                       resource_policy=args.resource_policy,
                       partial=not completed, failure=failure)
        (args.output / "summary.json").write_text(
            json.dumps(summary, indent=2) + "\n")
        print(json.dumps(summary), flush=True)
    return 0 if counts["fail"] == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
