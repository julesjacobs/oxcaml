#!/usr/bin/env python3
"""Pin and check public SMT suites separately from the Vox corpus."""

import argparse
from collections import Counter
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
from contextlib import nullcontext
import fcntl
import hashlib
import json
import mmap
import os
import platform
import secrets
import selectors
import statistics
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tarfile
import tempfile
import time
import urllib.request

from smt_corpus import CHECKS, command_name, expressions, write_json


ROOT = Path(__file__).resolve().parents[2]
FAMILIES = (
    "QF_UF", "QF_IDL", "QF_LIA", "QF_NIA", "QF_BV", "QF_DT",
    "QF_UFDT", "QF_UFDTLIA", "QF_UFLIA", "QF_UFNIA", "QF_UFBV",
    "QF_UFIDL",
)
RELEASES = {"non-incremental": "16740866", "incremental": "15493096"}
REGRESSIONS = {
    "z3": {"repository": "Z3Prover/z3test",
           "revision": "a865fb5760b9154650692b3a584ffb4178ce8f23",
           "license": "MIT", "prefix": ""},
    "cvc5": {"repository": "cvc5/cvc5",
             "revision": "a3f47bdffd29f30f65f886e180f233227cb4b27e",
             "license": "BSD-3-Clause", "prefix": "test/regress/"},
}
SUPPORTED_LOGICS = set(FAMILIES) | {
    "QF_UFDTNIA", "QF_BVLIA", "QF_BVNIA", "QF_UFBVLIA", "QF_UFBVNIA",
    "QF_BOOL", "QF_DTLIA", "QF_BVDT", "QF_ALL", "SAT", "QF_SAT",
}
UNSUPPORTED_WORDS = {
    "forall": "quantifiers", "exists": "quantifiers", "Real": "reals",
    "Array": "arrays", "String": "strings", "RegLan": "strings",
    "FloatingPoint": "floating_point", "RoundingMode": "floating_point",
    "Set": "sets", "Bag": "bags", "Seq": "sequences",
    "FiniteField": "finite_fields",
    "Float16": "floating_point", "Float32": "floating_point",
    "Float64": "floating_point", "Float128": "floating_point",
    "fp": "floating_point", "NaN": "floating_point",
    "+oo": "floating_point", "-oo": "floating_point",
    "+zero": "floating_point", "-zero": "floating_point",
    "to_fp": "floating_point", "to_fp_unsigned": "floating_point",
    "RNE": "floating_point", "RNA": "floating_point",
    "RTP": "floating_point", "RTN": "floating_point",
    "RTZ": "floating_point", "roundNearestTiesToEven": "floating_point",
    "roundNearestTiesToAway": "floating_point",
    "roundTowardPositive": "floating_point",
    "roundTowardNegative": "floating_point",
    "roundTowardZero": "floating_point",
    "/": "reals", "to_real": "reals", "to_int": "reals",
    "is_int": "reals", "real.pi": "reals", "sqrt": "reals",
    "exp": "reals", "sin": "reals", "cos": "reals", "tan": "reals",
    "csc": "reals", "sec": "reals", "cot": "reals",
    "arcsin": "reals", "arccos": "reals", "arctan": "reals",
    "arccsc": "reals", "arcsec": "reals", "arccot": "reals",
    "Tuple": "tuples", "tuple": "tuples", "Nullable": "nullable",
    "declare-heap": "separation_logic", "pto": "separation_logic",
    "wand": "separation_logic", "emp": "separation_logic",
    "sep": "separation_logic", "bag": "bags",
    "int.to.str": "strings", "lambda": "higher_order",
}
UNSUPPORTED_PREFIXES = {
    "fp.": "floating_point", "fp-": "floating_point",
    "tuple.": "tuples", "nullable.": "nullable",
    "sep.": "separation_logic", "str.": "strings", "re.": "strings",
    "set.": "sets", "bag.": "bags", "seq.": "sequences",
    "rel.": "relations", "ff.": "finite_fields",
}
STANDARD_OPTIONS = {
    ":diagnostic-output-channel", ":global-declarations",
    ":interactive-mode", ":print-success", ":produce-assertions",
    ":produce-assignments", ":produce-models", ":produce-proofs",
    ":produce-unsat-assumptions", ":produce-unsat-cores", ":random-seed",
    ":regular-output-channel", ":reproducible-resource-limit", ":verbosity",
}
NONSTANDARD_COMMANDS = {
    "maximize", "minimize", "assert-soft", "check-synth", "synth-fun",
    "get-interpolant", "get-interpolants", "get-abduct", "simplify",
    "apply", "eval", "query", "declare-rel", "rule", "get-qe",
    "get-qe-disjunct",
    "declare-codatatype", "declare-codatatypes", "define-fun-rec",
    "define-funs-rec",
}


def request(url, headers=None):
    return urllib.request.urlopen(urllib.request.Request(
        url, headers={"User-Agent": "Vox-SMT-suite-inventory/1",
                      **(headers or {})}), timeout=60)


def json_url(url):
    with request(url) as response:
        return json.load(response)


def hash_file(path, algorithm="sha256"):
    checksum = hashlib.new(algorithm)
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            checksum.update(chunk)
    return checksum.hexdigest()


def inventory(output, families):
    sources = []
    unavailable = []
    for track, record in RELEASES.items():
        data = json_url("https://zenodo.org/api/records/" + record)
        write_json(output / "provenance" / (record + ".json"), data)
        files = {item["key"]: item for item in data["files"]}
        for family in families:
            filename = family + ".tar.zst"
            if filename not in files:
                unavailable.append({"suite": "smtlib", "track": track,
                                    "family": family,
                                    "reason": "not published in this release"})
                continue
            item = files[filename]
            sources.append({
                "id": "smtlib-2025-" + track + "-" + family,
                "suite": "smtlib", "track": track, "family": family,
                "record": record, "doi": data["metadata"]["doi"],
                "license": data["metadata"]["license"]["id"],
                "published_checksum": item["checksum"],
                "download_bytes": item["size"], "extension": ".tar.zst",
                "url": (f"https://zenodo.org/records/{record}/files/"
                        + filename + "?download=1"),
                "status": "pending",
            })
    for name, spec in REGRESSIONS.items():
        repository, revision = spec["repository"], spec["revision"]
        sources.append(dict(spec, id=name + "-" + revision[:12], suite=name,
                            extension=".tar.gz", status="pending",
                            url=(f"https://codeload.github.com/{repository}/"
                                 f"tar.gz/{revision}")))
    manifest = {
        "format_version": 1, "families": list(families),
        "family_selection": "complete archives; no within-family sampling",
        "excluded_families": sorted(set(FAMILIES) - set(families)),
        "sources": sources, "unavailable": unavailable,
        "smtlib_download_bytes": sum(source.get("download_bytes", 0)
                                      for source in sources),
    }
    write_json(output / "manifest.json", manifest)
    return manifest


def download_ranges(source, temporary, jobs):
    total = source["download_bytes"]
    prefix_size = temporary.stat().st_size if temporary.exists() else 0
    if prefix_size > total:
        raise ValueError("partial download is longer than its source")
    parts = temporary.with_name(temporary.name + ".parts")
    parts.mkdir(parents=True, exist_ok=True)
    width = 32 * 1024 * 1024

    def get_range(start):
        end = min(start + width, total) - 1
        target = parts / f"{start}-{end}"
        if target.exists() and target.stat().st_size == end - start + 1:
            return target
        pending = target.with_suffix(".partial")
        headers = {"Range": f"bytes={start}-{end}"}
        with request(source["url"], headers) as response:
            content_range = f"bytes {start}-{end}/{total}"
            if response.headers.get("Content-Range") != content_range:
                raise ValueError("server did not honor requested byte range")
            with pending.open("wb") as stream:
                shutil.copyfileobj(response, stream)
        if pending.stat().st_size != end - start + 1:
            raise ValueError("downloaded range has the wrong length")
        pending.replace(target)
        return target

    with ThreadPoolExecutor(max_workers=jobs) as executor:
        segments = list(executor.map(get_range,
                                     range(prefix_size, total, width)))
    assembled = temporary.with_name(temporary.name + ".assembled")
    with assembled.open("wb") as stream:
        if prefix_size:
            with temporary.open("rb") as prefix:
                shutil.copyfileobj(prefix, stream)
        for segment in segments:
            with segment.open("rb") as part:
                shutil.copyfileobj(part, stream)
    assembled.replace(temporary)
    for segment in segments:
        segment.unlink()
    return temporary


def download(source, output, jobs=8):
    archive = output / "archives" / (source["id"] + source["extension"])
    archive.parent.mkdir(parents=True, exist_ok=True)
    receipt = archive.with_suffix(archive.suffix + ".json")
    if receipt.exists() and archive.exists():
        previous = json.loads(receipt.read_text())
        if previous["bytes"] == archive.stat().st_size:
            return previous
    temporary = archive.with_name(archive.name + ".part")
    for attempt in range(3):
        try:
            if source.get("download_bytes") is not None:
                download_ranges(source, temporary, jobs)
            else:
                with (request(source["url"]) as response,
                      temporary.open("wb") as out):
                    shutil.copyfileobj(response, out)
            expected = source.get("download_bytes")
            if expected is not None and temporary.stat().st_size != expected:
                raise ValueError("download length disagrees with provenance")
            if source.get("published_checksum"):
                algorithm, checksum = source["published_checksum"].split(
                    ":", 1)
                if hash_file(temporary, algorithm) != checksum:
                    raise ValueError(
                        "download checksum disagrees with provenance")
            temporary.replace(archive)
            result = dict(source, archive=str(archive.relative_to(output)),
                          bytes=archive.stat().st_size,
                          sha256=hash_file(archive), status="downloaded")
            write_json(receipt, result)
            print(json.dumps({"downloaded": source["id"],
                              "bytes": result["bytes"]}), flush=True)
            return result
        except Exception as error:
            if attempt == 2:
                return dict(source, status="download_failed",
                            error=repr(error))
            time.sleep(attempt + 1)


def extract(source, output):
    if source["status"] != "downloaded":
        return source
    destination = output / "sources" / source["id"]
    receipt = destination / ".extracted.json"
    if receipt.exists():
        return json.loads(receipt.read_text())
    archive = output / source["archive"]
    destination.mkdir(parents=True, exist_ok=True)
    try:
        with tarfile.open(archive, "r:*") as stream:
            stream.extractall(destination, filter="data")
        result = dict(source, status="extracted",
                      directory=str(destination.relative_to(output)))
        write_json(receipt, result)
        return result
    except Exception as error:
        return dict(source, status="extract_failed", error=repr(error))


def fetch(args):
    path = args.output / "manifest.json"
    manifest = (json.loads(path.read_text()) if path.exists()
                else inventory(args.output, args.family or FAMILIES))
    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        fetched = list(executor.map(
            lambda source: download(source, args.output, args.download_jobs),
            manifest["sources"]))
        manifest["sources"] = list(executor.map(
            lambda source: extract(source, args.output), fetched))
    write_json(path, manifest)
    print(json.dumps(dict(Counter(source["status"]
                                  for source in manifest["sources"]))),
          flush=True)
    return manifest


def iter_complete_expressions(data):
    end = 0
    for item in expressions(data):
        end = item[1]
        yield item
    if re.sub(rb';[^\r\n]*', b'', data[end:]).strip():
        raise ValueError("incomplete final expression")


def complete_expressions(data):
    return list(iter_complete_expressions(data))


def script_commands(data, errors, expects_error):
    try:
        yield from iter_complete_expressions(data)
    except ValueError as error:
        if not expects_error:
            errors.append("parse: " + str(error))


def theory_exclusions(expression, declared=()):
    expression = re.sub(rb':named\s+(\|[^|]*\||[^\s()]+)', b' ', expression)
    stripped = re.sub(rb';[^\r\n]*|\|[^|]*\|', b' ', expression)
    result = set()
    if b'"' in stripped:
        result.add("strings")
    stripped = re.sub(rb'"(?:[^"]|"")*"', b' ', stripped)
    atoms = {atom for token in re.findall(rb'[^\s()]+', stripped)
             if (atom := token.decode(errors="replace")) not in declared}
    result.update(UNSUPPORTED_WORDS[atom] for atom in atoms
                  if atom in UNSUPPORTED_WORDS)
    for prefix, feature in UNSUPPORTED_PREFIXES.items():
        if any(atom.startswith(prefix) for atom in atoms):
            result.add(feature)
    if any(re.fullmatch(r'[+-]?\d+\.\d+(?:[eE][+-]?\d+)?', atom)
           for atom in atoms):
        result.add("reals")
    return result


def semantic_override(option, value):
    name = option.lstrip(":-").replace("_", "-")
    if name in ("rewriter.hi-div0", "smt.hi-div0") and value == "false":
        return "legacy bit-vector division by zero"
    if name == "global-negate" and value == "true":
        return "global assertion negation"
    if (name in ("smt.arith.ignore-int", "arith.ignore-int")
            and value == "true"):
        return "integer constraints interpreted over reals"
    if name in ("solve-int-as-bv", "solve-real-as-int"):
        if value not in ("false", "0", "-1"):
            return "arithmetic domain replacement"
    return None


def analyze_script(data):
    errors = []
    expected_output = re.findall(rb'^\s*;\s*EXPECT:\s*(.*?)\s*$', data, re.M)
    expected_error_exit = bool(re.search(rb'^\s*;\s*EXIT:\s*1\s*$', data, re.M))
    expects_error = any(item.startswith(b"(error") for item in expected_output)
    logic = None
    latest_status = None
    statuses = []
    names = Counter()
    options = []
    overrides = []
    declared = Counter()
    declaration_scopes = {0: set()}
    scope_depth = 0
    global_declarations = False
    for _, _, expression in script_commands(data, errors, expects_error):
        name = command_name(expression)
        if name:
            names[name] += 1
        if name == "exit" and children(expression) == [b'exit']:
            break
        if name == "reset":
            latest_status = None
            declared.clear()
            declaration_scopes = {0: set()}
            scope_depth, global_declarations = 0, False
        if name == "reset-assertions":
            declaration_scopes = {0: set(declared)}
            declared = Counter({symbol: 1 for symbol in declared})
            scope_depth = 0
        if name in ("push", "pop"):
            parts = children(expression)
            try:
                count = int(parts[1]) if len(parts) > 1 else 1
            except ValueError:
                count = 0
            if count >= 0:
                scope_depth += count if name == "push" else -count
                while declaration_scopes:
                    depth = next(reversed(declaration_scopes))
                    if depth <= scope_depth:
                        break
                    for symbol in declaration_scopes.pop(depth):
                        declared[symbol] -= 1
                        if not declared[symbol]:
                            del declared[symbol]
        if name in ("declare-fun", "declare-const", "define-fun",
                    "declare-sort", "define-sort"):
            parts = children(expression)
            if len(parts) > 1:
                symbol = parts[1].decode(errors="replace")
                depth = 0 if global_declarations else scope_depth
                scope = declaration_scopes.setdefault(depth, set())
                if symbol not in scope:
                    declared[symbol] += 1
                    scope.add(symbol)
        if name == "set-logic":
            match = re.match(rb'\(set-logic\s+([^\s)]+)', expression)
            if match:
                logic = match[1].decode(errors="replace")
        if name == "set-info":
            match = re.match(rb'\(set-info\s+:status\s+(sat|unsat|unknown)',
                             expression)
            if match:
                latest_status = match[1].decode()
        if name == "set-option":
            parts = children(expression)
            if len(parts) == 3:
                option, value = (part.decode(errors="replace")
                                 for part in parts[1:])
                options.append({"option": option, "value": value,
                                "standard": option in STANDARD_OPTIONS})
                if option in (":global-declarations", ":global-decls"):
                    global_declarations = value == "true"
                reason = semantic_override(option, value)
                if reason:
                    overrides.append({"option": option, "value": value,
                                      "reason": reason, "source": "script"})
        if name in CHECKS:
            statuses.append(latest_status)
            latest_status = None
        if name in NONSTANDARD_COMMANDS:
            errors.append("unsupported_command: " + name)
        if name not in ("set-info", "echo", "set-option"):
            errors.extend(theory_exclusions(expression, declared))
    command_lines = [line.decode(errors="replace") for line in
                     re.findall(rb'^\s*;\s*COMMAND-LINE:\s*(.*)$', data, re.M)]
    for command_line in command_lines:
        try:
            arguments = shlex.split(command_line)
        except ValueError:
            errors.append("unparseable_vendor_command_line")
            continue
        for argument in arguments:
            option, separator, value = argument.partition("=")
            reason = semantic_override(option, value if separator else "true")
            if reason:
                overrides.append({"option": option, "value": value,
                                  "reason": reason, "source": "command-line"})
    errors.extend("semantic_override: " + item["reason"] for item in overrides)
    if logic is not None and logic not in SUPPORTED_LOGICS and logic != "ALL":
        errors.append("unsupported_logic: " + logic)
    if not statuses and not expected_output:
        errors.append("no_check_command")
    expected_statuses = [item.decode() for item in expected_output
                         if item in (b"sat", b"unsat", b"unknown")]
    status_source = "set-info"
    if len(expected_statuses) == len(statuses):
        statuses = expected_statuses
        status_source = "EXPECT"
    return {
        "logic": logic, "checks": sum(names[name] for name in CHECKS),
        "commands": dict(names), "expected_statuses": statuses,
        "expected_status_source": status_source,
        "expected_output": [item.decode(errors="replace")
                            for item in expected_output],
        "expected_error_exit": expected_error_exit,
        "options": options, "semantic_overrides": overrides,
        "vendor_command_lines": command_lines,
        "exclusions": sorted(set(errors)),
        "has_model_commands": any(
            names[name] for name in
            ("get-model", "get-value", "get-assignment")),
        "has_core_commands": bool(names["get-unsat-core"]
                                  or names["get-unsat-assumptions"]),
    }


def inspect_file(item):
    source, path, output = item
    result = dict(source_id=source["id"], suite=source["suite"],
                  path=str(path.relative_to(output)),
                  bytes=path.stat().st_size)
    result.update({key: source[key] for key in ("track", "family")
                   if key in source})
    if path.suffix == ".smt":
        result.update(eligible=False, exclusions=["SMT-LIB version 1"])
        result["sha256"] = hash_file(path)
    elif not result["bytes"]:
        result.update(analyze_script(b''))
        result["sha256"] = hashlib.sha256(b'').hexdigest()
    else:
        with path.open("rb") as stream, mmap.mmap(
                stream.fileno(), 0, access=mmap.ACCESS_READ) as data:
            result["sha256"] = hashlib.sha256(data).hexdigest()
            result.update(analyze_script(data))
    result["eligible"] = not result["exclusions"]
    return result


def index(args):
    manifest = json.loads((args.output / "manifest.json").read_text())
    selected = set(args.suite or ("smtlib", "z3", "cvc5"))
    items = []
    unavailable = []
    for source in manifest["sources"]:
        if source["suite"] not in selected:
            continue
        directory = args.output / "sources" / source["id"]
        receipt = directory / ".extracted.json"
        if not receipt.exists():
            unavailable.append(source["id"])
            continue
        roots = list(directory.iterdir())
        if source["suite"] in REGRESSIONS:
            roots = [path / source["prefix"] for path in roots
                     if path.is_dir()]
        else:
            roots = [directory]
        paths = set()
        for root in roots:
            paths.update(root.rglob("*.smt2"))
            paths.update(root.rglob("*.smt"))
        items.extend((source, path, args.output) for path in sorted(paths))
    items.sort(key=lambda item: -item[1].stat().st_size)
    summary = {"selected_suites": sorted(selected),
               "unavailable_sources": unavailable, "total": 0,
               "eligible": 0, "exclusions": Counter(), "suites": {},
               "families": {}}
    temporary = args.output / "cases.jsonl.tmp"
    with temporary.open("w") as stream, ProcessPoolExecutor(
            max_workers=args.jobs) as executor:
        for record in executor.map(inspect_file, items, chunksize=1):
            stream.write(json.dumps(record, sort_keys=True) + "\n")
            summary["total"] += 1
            summary["eligible"] += record["eligible"]
            summary["exclusions"].update(record["exclusions"])
            for field, key in (("suites", suite_group(record)),
                               ("families", record["source_id"])):
                counts = summary[field].setdefault(key, Counter())
                counts["total"] += 1
                counts["eligible"] += record["eligible"]
            if summary["total"] % 1000 == 0:
                print(json.dumps({"indexed": summary["total"],
                                  "total": len(items)}), flush=True)
    temporary.replace(args.output / "cases.jsonl")
    write_json(args.output / "index-summary.json", summary)
    print(json.dumps(summary), flush=True)
    return summary


def response_kind(text):
    return ("error" if command_name(text) == "error" else
            text.decode() if text in (b"sat", b"unsat", b"unknown", b"success")
            else "string" if text.startswith(b'"') else "value")


def active_expressions(data):
    for item in iter_complete_expressions(data):
        yield item
        if command_name(item[2]) == 'exit' and children(item[2]) == [b'exit']:
            break


def normalize_echo_output(script, output):
    if script.find(b'echo') < 0:
        return output
    commands = [part for _, _, part in active_expressions(script)]
    if not any(command_name(command) == 'echo' for command in commands):
        return output
    parsed = {}
    edits = [None]
    edit_ids = {}
    visited = {}
    completed = set()
    pending = [(0, 0, False, 0)]

    def response(position):
        if position not in parsed:
            try:
                item = next(expressions(output[position:]), None)
            except ValueError:
                item = None
            if item is None:
                parsed[position] = None
            else:
                end = position + item[1]
                ending = re.compile(rb'\r?\n').match(output, end)
                parsed[position] = (ending.end() if ending else end, item[2])
        return parsed[position]

    def change(previous, start, end, quoted):
        key = previous, start, end, quoted
        if key not in edit_ids:
            edit_ids[key] = len(edits)
            edits.append(key)
        return edit_ids[key]

    while pending:
        index, position, print_success, edit = pending.pop()
        key = index, position, print_success
        if key in visited:
            if visited[key] != edit:
                return output
            continue
        visited[key] = edit
        if len(visited) > 100000:
            return output
        if index == len(commands):
            if not output[position:].strip():
                completed.add(edit)
            continue
        command = commands[index]
        name = command_name(command) or ''
        parts = children(command) if command.startswith(b'(') else []
        mandatory = name in CHECKS or name.startswith('get-') or name == 'echo'
        policy = print_success
        if (name == 'set-option' and len(parts) == 3
                and parts[1] == b':print-success'
                and parts[2] in (b'true', b'false')):
            policy = parts[2] == b'true'
        elif parts == [b'reset']:
            # SMT-LIB resets options; Z3 retains :print-success.
            policy = None
        if not mandatory and policy is not True:
            pending.append((index + 1, position, policy, edit))
        valid_echo = (name == 'echo' and len(parts) == 2
                      and parts[1].startswith(b'"'))
        if valid_echo:
            quoted = parts[1]
            raw = quoted[1:-1].replace(b'""', b'"')
            for value in dict.fromkeys((quoted, raw)):
                ending = rb'\r?\n' if not value.strip() else rb'(?:\r?\n|$)'
                match = re.compile(rb'\s*?(' + re.escape(value)
                                   + rb')' + ending).match(output, position)
                if match is not None:
                    normalized = (edit if value == quoted else
                                  change(edit, match.start(1), match.end(1),
                                         quoted))
                    pending.append((index + 1, match.end(),
                                    print_success, normalized))
        item = response(position)
        if item is None:
            continue
        end, answer = item
        if (not valid_echo
                and (answer == b'unsupported' or command_name(answer) == 'error')):
            pending.append((index + 1, end, print_success, edit))
        elif not mandatory:
            if policy is not False and answer == b'success':
                pending.append((index + 1, end, policy, edit))
        elif name != 'echo' and answer != b'success':
            if name not in CHECKS or answer in (b'sat', b'unsat', b'unknown'):
                pending.append((index + 1, end, policy, edit))
    if len(completed) != 1:
        return output
    edit = completed.pop()
    replacements = []
    while edit:
        edit, start, end, quoted = edits[edit]
        replacements.append((start, end, quoted))
    fragments = []
    position = 0
    for start, end, quoted in reversed(replacements):
        fragments.extend((output[position:start], quoted))
        position = end
    fragments.append(output[position:])
    return b''.join(fragments)


def reference_script(data, profile=None):
    if profile != 'z3' or b':incremental' not in data:
        return data, None
    transformations = []
    parts = []
    previous = 0
    for start, end, command in active_expressions(data):
        expression = children(command) if command.startswith(b'(') else []
        if (profile == 'z3' and len(expression) == 3
                and expression[:2] == [b'set-option', b':incremental']
                and expression[2] in (b'true', b'false')):
            replacement = b'(set-info :source "vox reference adaptation")'
            parts.extend((data[previous:start], replacement))
            previous = end
            transformations.append({
                'command_start': start, 'command_end': end,
                'source_command': command.decode(),
                'replacement_command': replacement.decode(),
                'reason': 'z3-always-supports-incremental-commands',
            })
    parts.append(data[previous:])
    if not transformations:
        return data, None
    adapted = b''.join(parts)
    return adapted, {
        'profile': profile, 'source_sha256': hashlib.sha256(data).hexdigest(),
        'adapted_sha256': hashlib.sha256(adapted).hexdigest(),
        'transformations': transformations,
    }


def run_reference(args, script):
    if getattr(args, 'reference_profile', None) is None:
        return run_script(args.reference, args.reference_args, script,
                          args.timeout, getattr(args, 'memory_limit_mib', 0),
                          getattr(args, 'output_max_mib', 64))
    data = script.read_bytes() if isinstance(script, Path) else script
    adapted, adaptation = reference_script(
        data, getattr(args, 'reference_profile', None))
    result = run_script(args.reference, args.reference_args, adapted,
                        args.timeout, getattr(args, 'memory_limit_mib', 0),
                        getattr(args, 'output_max_mib', 64))
    if adaptation is not None:
        result['reference_adaptation'] = adaptation
    return result


def run_script(executable, arguments, script, timeout, memory_limit_mib=0,
               output_max_mib=64):
    started = time.monotonic()
    try:
        command = [executable, *arguments]
        if memory_limit_mib:
            limiter = shutil.which("prlimit")
            if not limiter:
                raise ValueError("memory limits require prlimit")
            command = [limiter, f"--as={memory_limit_mib * 1024 * 1024}",
                       "--", *command]
        with (
                (script.open("rb") if isinstance(script, Path)
                 else nullcontext(None)) as stream,
                tempfile.TemporaryFile() as out,
                tempfile.TemporaryFile() as err):
            incoming = {"stdin": stream} if stream else {"input": script}
            result = subprocess.run(command, **incoming,
                                    stdout=out, stderr=err, timeout=timeout)
            sizes = {"stdout_bytes": out.tell(), "stderr_bytes": err.tell()}
            if max(sizes.values()) > output_max_mib * 1024 * 1024:
                return {"output_limit": True, **sizes,
                        "returncode": result.returncode,
                        "seconds": time.monotonic() - started}
            out.seek(0)
            err.seek(0)
            output, diagnostics = out.read(), err.read()
        raw_output = output
        if isinstance(script, Path):
            if script.stat().st_size:
                with script.open('rb') as source, mmap.mmap(
                        source.fileno(), 0, access=mmap.ACCESS_READ) as mapped:
                    output = normalize_echo_output(mapped, output)
        else:
            output = normalize_echo_output(script, output)
        parsed = complete_expressions(output)
        return {"returncode": result.returncode,
                "seconds": time.monotonic() - started,
                "statuses": [text.decode() for _, _, text in parsed
                             if text in (b"sat", b"unsat", b"unknown")],
                "errors": [text.decode(errors="replace")
                           for _, _, text in parsed
                           if command_name(text) == "error"],
                "response_kinds": [response_kind(text)
                                   for _, _, text in parsed],
                "stdout": output.decode(errors="replace"),
                **({"raw_stdout": raw_output.decode(errors="replace")}
                   if output != raw_output else {}),
                "stderr": diagnostics.decode(errors="replace")}
    except subprocess.TimeoutExpired:
        return {"timeout": True, "seconds": time.monotonic() - started}
    except (OSError, ValueError) as error:
        return {"failure": repr(error),
                "seconds": time.monotonic() - started}


def children(expression):
    if not expression.startswith(b"(") or not expression.endswith(b")"):
        raise ValueError("expected a list")
    return [part for _, _, part in complete_expressions(expression[1:-1])]


def named_terms(expression):
    pending = [expression]
    while pending:
        term = pending.pop()
        if not term.startswith(b'('):
            continue
        parts = children(term)
        if parts and parts[0] == b'!':
            for index, attribute in enumerate(parts[2:], 2):
                if attribute == b':named':
                    yield parts[index + 1]
            pending.append(parts[1])
        else:
            pending.extend(parts)


def command_terms(expression):
    name = command_name(expression)
    parts = children(expression)
    if name in ('assert', 'define-fun', 'define-fun-rec'):
        return [parts[-1]]
    if name in ('check-sat-assuming', 'get-value'):
        return children(parts[1])
    if name == 'check-sat':
        return parts[1:]
    return []


def logical_prefix(commands, model_observations=None):
    ignored = CHECKS | {"echo", "exit"}
    replay = []
    for expression in commands:
        name = command_name(expression) or ""
        if (model_observations is not None
                and name in CHECKS | {"reset", "reset-assertions", "assert",
                                     "push", "pop"}):
            model_observations.clear()
        if name in ignored or name.startswith("get-"):
            for term in command_terms(expression):
                if any(named_terms(term)):
                    if model_observations is not None and name == 'get-value':
                        model_observations.append(len(replay))
                    replay.append(b'(assert (let ((vox_certificate_unused '
                                  + term + b')) true))')
        else:
            replay.append(expression)
    return replay


def without_named(expression):
    if not expression.startswith(b'('):
        return expression
    parts = children(expression)
    if parts and parts[0] == b'!':
        return without_named(parts[1])
    return b'(' + b' '.join(without_named(part) for part in parts) + b')'


def last_assumptions(commands):
    assumptions = []
    for expression in commands:
        name = command_name(expression)
        if name in CHECKS or name == "reset":
            parts = children(expression)
            assumptions = (children(parts[1])
                           if name == "check-sat-assuming" else
                           parts[1:] if name == "check-sat" else [])
    return assumptions


def scope_count(expression):
    parts = children(expression)
    return int(parts[1]) if len(parts) > 1 else 1


def active_declarations(commands, include_definitions=False):
    scopes = [({}, {})]
    global_scope = ({}, {})
    global_declarations = False
    for expression in commands:
        name = command_name(expression)
        if name == "reset":
            scopes, global_scope = [({}, {})], ({}, {})
            global_declarations = False
        elif name == "reset-assertions":
            scopes = ([(dict(item for scope in scopes
                            for item in scope[0].items()),
                        dict(item for scope in scopes
                             for item in scope[1].items()))]
                      if global_declarations else [({}, {})])
        elif name == "push":
            scopes.extend(({}, {}) for _ in range(scope_count(expression)))
        elif name == "pop":
            count = scope_count(expression)
            if count:
                del scopes[-count:]
        elif name == "set-option":
            parts = children(expression)
            if parts[1] in (b':global-declarations', b':global-decls'):
                global_declarations = parts[2] == b'true'
        elif name in ("declare-sort", "define-sort"):
            target = global_scope if global_declarations else scopes[-1]
            parts = children(expression)
            target[1][symbol_key(parts[1])] = parts
        elif (name in ("declare-const", "declare-fun")
              or include_definitions
              and name in ('define-const', 'define-fun', 'define-fun-rec')):
            target = global_scope if global_declarations else scopes[-1]
            sorts = dict(item for scope in [global_scope, *scopes]
                         for item in scope[1].items())
            signature = model_signature(children(expression),
                                        sort_normalizer(sorts))
            target[0][signature] = expression
    return dict(item for scope in [global_scope, *scopes]
                for item in scope[0].items())


def canonical(expression):
    return (tuple(canonical(part) for part in children(expression))
            if expression.startswith(b'(') else expression)


def symbols(expression):
    return set(re.findall(rb'\|[^|]*\||[^\s()]+', expression))


def symbol_key(value):
    return value[1:-1] if value.startswith(b'|') and value.endswith(b'|') else value


def active_sort_declarations(commands):
    scopes = [{}]
    global_scope = {}
    global_declarations = False
    for expression in commands:
        name = command_name(expression)
        if name == "reset":
            scopes, global_scope, global_declarations = [{}], {}, False
        elif name == "reset-assertions":
            scopes = ([dict(item for scope in scopes for item in scope.items())]
                      if global_declarations else [{}])
        elif name == "push":
            scopes.extend({} for _ in range(scope_count(expression)))
        elif name == "pop":
            count = scope_count(expression)
            if count:
                del scopes[-count:]
        elif name == "set-option":
            parts = children(expression)
            if parts[1] in (b':global-declarations', b':global-decls'):
                global_declarations = parts[2] == b'true'
        elif name in ("declare-sort", "define-sort"):
            target = global_scope if global_declarations else scopes[-1]
            parts = children(expression)
            target[symbol_key(parts[1])] = parts
    return dict(item for scope in [global_scope, *scopes]
                for item in scope.items())


def model_sort_normalizer(prefix):
    return sort_normalizer(active_sort_declarations(prefix))


def sort_normalizer(declarations):
    def normalize_sort(expression, parameters=None, visiting=frozenset()):
        parameters = parameters or {}
        if symbol_key(expression) in parameters:
            return parameters[symbol_key(expression)]
        parts = children(expression) if expression.startswith(b'(') else [expression]
        key = symbol_key(parts[0])
        args = [normalize_sort(part, parameters, visiting) for part in parts[1:]]
        declaration = declarations.get(key)
        if declaration and declaration[0] == b'define-sort':
            if key in visiting:
                raise ValueError("cyclic sort alias in model")
            parameters = children(declaration[2])
            if len(parameters) != len(args):
                raise ValueError("invalid sort alias in model")
            return normalize_sort(declaration[3],
                                  dict(zip(map(symbol_key, parameters), args)),
                                  visiting | {key})
        return (b'(' + b' '.join([parts[0], *args]) + b')'
                if len(parts) > 1 else parts[0])

    return normalize_sort


def sort_identity(expression):
    return (tuple(sort_identity(part) for part in children(expression))
            if expression.startswith(b'(') else symbol_key(expression))


def model_signature(parts, normalize_sort):
    if parts[0] == b'declare-const' and len(parts) == 3:
        domains, result = [], parts[2]
    elif parts[0] == b'define-const' and len(parts) == 4:
        domains, result = [], parts[2]
    elif parts[0] == b'declare-fun' and len(parts) == 4:
        domains, result = children(parts[2]), parts[3]
    elif parts[0] in (b'define-fun', b'define-fun-rec') and len(parts) == 5:
        parameters = [children(parameter) for parameter in children(parts[2])]
        if any(len(parameter) != 2 for parameter in parameters):
            raise ValueError('malformed model parameter')
        domains, result = [parameter[1] for parameter in parameters], parts[3]
    else:
        raise ValueError('malformed function declaration')
    return (symbol_key(parts[1]),
            tuple(sort_identity(normalize_sort(sort)) for sort in domains),
            sort_identity(normalize_sort(result)))


def abstract_model_values(prefix, answer):
    declarations = active_sort_declarations(prefix)
    used = {symbol_key(item) for item in set().union(
        symbols(answer), *(symbols(item) for item in prefix))}
    bound_symbols = {signature[0] for signature in
                     active_declarations(prefix, include_definitions=True)}
    bound_symbols.update(active_annotation_labels(prefix))
    pending = [answer]
    while pending:
        expression = pending.pop()
        if expression.startswith(b'('):
            parts = children(expression)
            if parts and parts[0] == b'define-fun' and len(parts) == 5:
                bound_symbols.add(symbol_key(parts[1]))
            pending.extend(parts)
    values = {}
    groups = {}
    additions = []

    normalize_sort = model_sort_normalizer(prefix)

    def rewrite(expression, bound):
        if not expression.startswith(b'('):
            return expression
        parts = children(expression)
        if parts and parts[0] == b'define-fun' and len(parts) == 5:
            parameters = {symbol_key(children(item)[0])
                          for item in children(parts[2])}
            return b'(' + b' '.join(parts[:4] + [
                rewrite(parts[4], bound | parameters)]) + b')'
        if parts and parts[0] == b'let' and len(parts) == 3:
            bindings = [children(item) for item in children(parts[1])]
            parameters = {symbol_key(item[0]) for item in bindings}
            assignments = [b'(' + name + b' ' + rewrite(value, bound) + b')'
                           for name, value in bindings]
            return b'(let (' + b' '.join(assignments) + b') ' + rewrite(
                parts[2], bound | parameters) + b')'
        if parts and parts[0] in (b'forall', b'exists', b'lambda'):
            parameters = {symbol_key(children(item)[0])
                          for item in children(parts[1])}
            return b'(' + parts[0] + b' ' + parts[1] + b' ' + rewrite(
                parts[2], bound | parameters) + b')'
        if (len(parts) == 3 and parts[0] == b'as'
                and symbol_key(parts[1]).startswith(b'@')
                and symbol_key(parts[1]) not in bound):
            sort = normalize_sort(parts[2])
            sort_parts = children(sort) if sort.startswith(b'(') else [sort]
            declaration = declarations.get(symbol_key(sort_parts[0]))
            if (not declaration or declaration[0] != b'declare-sort'
                    or len(sort_parts) - 1 != (int(declaration[2])
                                              if len(declaration) > 2 else 0)):
                raise ValueError("abstract model value has no uninterpreted sort")
            sort_key = sort_identity(sort)
            key = sort_key, symbol_key(parts[1])
            if key not in values:
                index = len(used)
                name = b'vox_model_value_' + str(index).encode()
                while name in used:
                    index += 1
                    name = b'vox_model_value_' + str(index).encode()
                used.add(name)
                values[key] = name
                groups.setdefault(sort_key, []).append(name)
                additions.append(b'(declare-const ' + name + b' ' + sort + b')')
            return values[key]
        return b'(' + b' '.join(rewrite(part, bound) for part in parts) + b')'

    rewritten = rewrite(answer, bound_symbols)
    additions.extend(b'(assert (distinct ' + b' '.join(group) + b'))'
                     for group in groups.values() if len(group) > 1)
    return rewritten, additions


def assertion_label(expression):
    parts = children(expression)
    if len(parts) != 2 or parts[0] != b'assert' or not parts[1].startswith(b'('):
        return None
    term = children(parts[1])
    if not term or term[0] != b'!':
        return None
    attributes = term[2:]
    for index, attribute in enumerate(attributes):
        if attribute == b':named':
            if index + 1 == len(attributes):
                raise ValueError("missing assertion name")
            return symbol_key(attributes[index + 1])
    return None


def core_script(prefix, answer, *, semantics="standard", assumptions=None):
    core = [symbol_key(item) for item in children(answer)]
    scopes = [set()]
    for expression in prefix:
        name = command_name(expression)
        if name == "reset" or name == "reset-assertions":
            scopes = [set()]
        elif name == "push":
            scopes.extend(set() for _ in range(scope_count(expression)))
        elif name == "pop":
            count = scope_count(expression)
            if count:
                del scopes[-count:]
        elif name == "assert":
            label = assertion_label(expression)
            if label is not None:
                scopes[-1].add(label)
    original_assumptions = last_assumptions(prefix)
    if semantics not in ("standard", "z3-mixed"):
        raise ValueError("unsupported core semantics")
    allowed = set().union(*scopes)
    if semantics == "z3-mixed":
        allowed.update(symbol_key(item) for item in original_assumptions)
    if len(set(core)) != len(core):
        raise ValueError("duplicate core entry")
    if set(core) - allowed:
        raise ValueError("core contains an inactive or unknown name")
    replay = []
    for expression in logical_prefix(prefix):
        if command_name(expression) == "assert":
            label = assertion_label(expression)
            if label is not None and label not in core:
                continue
        replay.append(expression)
    if assumptions is None:
        assumptions = (original_assumptions if semantics == "standard" else
                       [item for item in original_assumptions
                        if symbol_key(item) in core])
    replay.append(b'(check-sat-assuming ('
                  + b' '.join(map(without_named, assumptions)) + b'))')
    return b'\n'.join(replay)


def assumption_script(prefix, answer):
    assumptions = children(answer)
    original = {canonical(item) for item in last_assumptions(prefix)}
    if any(canonical(item) not in original for item in assumptions):
        raise ValueError("core contains an unknown assumption")
    if len({canonical(item) for item in assumptions}) != len(assumptions):
        raise ValueError("duplicate unsat assumption")
    return b'\n'.join(logical_prefix(prefix) + [
        b'(check-sat-assuming ('
        + b' '.join(map(without_named, assumptions)) + b'))'])


def model_script(prefix, answer, requested_values=None, observations=()):
    observations = [*observations, (answer, requested_values)]
    for value, requested in observations:
        if requested is not None:
            pairs = [children(pair) for pair in children(value)]
            if any(len(pair) != 2 for pair in pairs):
                raise ValueError("malformed get-value pair")
            if (list(map(canonical, (pair[0] for pair in pairs)))
                    != list(map(canonical, requested))):
                raise ValueError("get-value did not return the requested terms")
    retained = []
    replay = logical_prefix(prefix, retained)
    answers = b'(' + b' '.join(value for value, _ in observations) + b')'
    definitions = b'(' + b' '.join(replay[index] for index in retained) + b')'
    combined = b'(' + answers + b' ' + definitions + b')'
    rewritten, abstract_values = abstract_model_values(prefix, combined)
    rewritten, definitions = children(rewritten)
    pending = {children(item)[1]: item for item in abstract_values
               if command_name(item) == 'declare-const'}
    distinct = [item for item in abstract_values
                if command_name(item) != 'declare-const']
    replacements = dict(zip(retained, children(definitions)))
    expanded = []
    for index, expression in enumerate(replay):
        if index in replacements:
            expression = replacements[index]
            used = symbols(expression)
            for name in list(pending):
                if name in used:
                    expanded.append(pending.pop(name))
        expanded.append(expression)
    replay = expanded
    abstract_values = [*pending.values(), *distinct]
    if any(requested is None for _, requested in observations):
        replay = [b'(set-logic ALL)' if command_name(item) == 'set-logic'
                  else item for item in replay]
    additions = [b'(assert ' + without_named(assumption) + b')'
                 for assumption in last_assumptions(prefix)]
    declarations = active_declarations(prefix)
    declaration_names = {signature[0] for signature in declarations}
    normalize_sort = model_sort_normalizer(prefix)
    inputs = active_declarations(prefix, include_definitions=True)
    input_symbols = {signature[0] for signature in inputs}
    input_definitions = {signature[0] for signature in inputs
                         if signature not in declarations}
    input_definitions.update(active_annotation_labels(prefix))
    input_symbols.update(input_definitions)
    occupied = {symbol_key(item) for item in set().union(
        symbols(combined), *(symbols(item) for item in prefix),
        *(symbols(item) for item in abstract_values))}
    helpers = {}
    for answer, (_, requested_values) in zip(children(rewritten), observations):
        if requested_values is not None:
            for pair in children(answer):
                parts = children(pair)
                if len(parts) != 2:
                    raise ValueError("malformed get-value pair")
                used = {symbol_key(name) for name in symbols(parts[1])}
                if used & input_symbols:
                    raise ValueError(
                        "get-value answer uses unfixed input symbols")
                additions.append(b'(assert (= ' + without_named(parts[0])
                                 + b' ' + parts[1] + b'))')
        else:
            definitions = {}
            entries = children(answer)
            if entries and entries[0] == b'model':
                entries = entries[1:]
            for definition in entries:
                parts = children(definition)
                if not parts:
                    continue
                if parts[0] == b'define-fun' and len(parts) == 5:
                    signature = model_signature(parts, normalize_sort)
                    if signature in definitions:
                        raise ValueError("duplicate model definition")
                    if (signature[0] in declaration_names
                            and signature not in declarations):
                        raise ValueError('model definition signature mismatch')
                    if (signature not in declarations
                            and signature[0] in input_definitions):
                        raise ValueError(
                            'model helper shadows input definition')
                    definitions[signature] = parts
                else:
                    raise ValueError("unsupported model entry")
            missing = set(declarations) - set(definitions)
            if missing:
                raise ValueError("model omits declared input symbols")
            by_name = {}
            for signature in definitions:
                by_name.setdefault(signature[0], set()).add(signature)
            edges = {}
            for signature, parts in definitions.items():
                parameters = {symbol_key(children(part)[0])
                              for part in children(parts[2])}
                references = {symbol_key(item) for item in symbols(parts[4])}
                references.difference_update(parameters)
                if references & input_definitions:
                    raise ValueError('model body refers to input definitions')
                edges[signature] = set().union(
                    *(by_name.get(name, set()) for name in references))
            todo = set(edges)
            order = []
            while todo:
                ready = [signature for signature in definitions
                         if signature in todo and not (edges[signature] & todo)]
                if not ready:
                    raise ValueError("cyclic model definitions")
                order.extend(ready)
                todo.difference_update(ready)
            for signature in order:
                parts = definitions[signature]
                definition = b'(' + b' '.join(parts) + b')'
                if signature not in declarations:
                    if signature in helpers:
                        if (canonical(helpers[signature])
                                != canonical(definition)):
                            raise ValueError(
                                'conflicting model helper definitions')
                    else:
                        helpers[signature] = definition
                        additions.append(definition)
                    continue
                parameters = children(parts[2])
                declared = children(declarations[signature])
                names = [children(parameter)[0] for parameter in parameters]
                qualified = b'(as ' + declared[1] + b' ' + parts[3] + b')'
                application = (b'(' + b' '.join([qualified, *names]) + b')'
                               if names else qualified)
                helper = b'vox_model_definition_' + str(len(occupied)).encode()
                while helper in occupied:
                    helper += b'_'
                occupied.add(helper)
                additions.append(b'(define-fun ' + helper + b' '
                                 + b' '.join(parts[2:]) + b')')
                applied_helper = (b'(' + b' '.join([helper, *names]) + b')'
                                  if names else helper)
                equality = b'(= ' + application + b' ' + applied_helper + b')'
                if parameters:
                    equality = b'(forall ' + parts[2] + b' ' + equality + b')'
                additions.append(b'(assert ' + equality + b')')
    return b'\n'.join(replay + abstract_values + additions + [b'(check-sat)'])


def annotation_parts(parts):
    if len(parts) < 3:
        raise ValueError('annotation has no attributes')
    attributes = []
    index = 2
    while index < len(parts):
        attribute = parts[index]
        if not attribute.startswith(b':'):
            raise ValueError('annotation attribute is not a keyword')
        index += 1
        value = None
        if index < len(parts) and not parts[index].startswith(b':'):
            value = canonical(parts[index])
            index += 1
        if attribute in (b':named', b':pattern') and value is None:
            raise ValueError('annotation attribute has no value')
        attributes.append((attribute, value))
    return parts[1], attributes


def assertion_matches(expected, actual):
    left = children(expected) if expected.startswith(b'(') else None
    right = children(actual) if actual.startswith(b'(') else None
    if right and right[0] == b'!':
        annotation_parts(right)
    if left and left[0] == b'!':
        body, attributes = annotation_parts(left)
        required = (b':named', b':pattern')
        if (all(name not in required for name, _ in attributes)
                and assertion_matches(body, actual)):
            return True
        if not right or right[0] != b'!':
            return False
        returned_body, returned = annotation_parts(right)
        position = 0
        for attribute in attributes:
            if position < len(returned) and returned[position] == attribute:
                position += 1
            elif attribute[0] in required:
                return False
        return (position == len(returned)
                and assertion_matches(body, returned_body))
    if left is None or right is None:
        return expected == actual
    return (len(left) == len(right)
            and all(assertion_matches(first, second)
                    for first, second in zip(left, right)))


def validate_assertions(prefix, answer):
    scopes = [[]]
    for expression in prefix:
        name = command_name(expression)
        if name in ('reset', 'reset-assertions'):
            scopes = [[]]
        elif name == 'push':
            scopes.extend([] for _ in range(scope_count(expression)))
        elif name == 'pop':
            count = scope_count(expression)
            if count:
                del scopes[-count:]
        elif name == 'assert':
            scopes[-1].append(children(expression)[1])
    expected = [term for scope in scopes for term in scope]
    returned = children(answer)
    return (all(any(assertion_matches(term, value) for value in returned)
                for term in expected)
            and all(any(assertion_matches(term, value) for term in expected)
                    for value in returned))


def assignment_pairs(answer):
    pairs = {}
    for item in children(answer):
        pair = children(item)
        if (len(pair) != 2 or pair[0].startswith(b'(')
                or pair[1] not in (b'true', b'false')):
            raise ValueError('invalid Boolean assignment pair')
        name = symbol_key(pair[0])
        if name in pairs:
            raise ValueError('duplicate assignment name')
        pairs[name] = (pair[0], pair[1])
    return pairs


def active_annotation_labels(prefix):
    scopes = [{}]
    global_scope = {}
    global_declarations = False
    for expression in prefix:
        name = command_name(expression)
        if name == 'reset':
            scopes, global_scope, global_declarations = [{}], {}, False
        elif name == 'reset-assertions':
            scopes = ([dict(item for scope in scopes for item in scope.items())]
                      if global_declarations else [{}])
        elif name == 'push':
            scopes.extend({} for _ in range(scope_count(expression)))
        elif name == 'pop':
            count = scope_count(expression)
            if count:
                del scopes[-count:]
        elif name == 'set-option':
            parts = children(expression)
            if parts[1] in (b':global-declarations', b':global-decls'):
                global_declarations = parts[2] == b'true'
        else:
            target = global_scope if global_declarations else scopes[-1]
            for term in command_terms(expression):
                for label in named_terms(term):
                    target[symbol_key(label)] = label
    return dict(item for scope in [global_scope, *scopes]
                for item in scope.items())


def assignment_values(answer, inventory):
    pairs = assignment_pairs(answer)
    if pairs.keys() != assignment_pairs(inventory).keys():
        raise ValueError('assignment omits or adds a named Boolean term')
    names = [name for name, _ in pairs.values()]
    values = b'(' + b' '.join(b'(' + name + b' ' + value + b')'
                             for name, value in pairs.values()) + b')'
    return names, values


def validate_certificates(script, output, args):
    commands = [expression for _, _, expression in active_expressions(script)]
    output = normalize_echo_output(script, output)
    responses = [expression for _, _, expression in expressions(output)
                 if expression != b'success']
    response_index = 0
    validations = []
    named_core = assumption_core = None
    observations = []
    for index, expression in enumerate(commands):
        name = command_name(expression) or ""
        if name == 'exit' and children(expression) == [b'exit']:
            break
        if name in CHECKS or name in ("reset", "reset-assertions", "assert",
                                     "push", "pop"):
            named_core = assumption_core = None
            observations = []
        if (name not in CHECKS and not name.startswith("get-")
                and name != "echo"):
            continue
        if response_index >= len(responses):
            return [{"result": "invalid", "reason": "missing response"}]
        answer = responses[response_index]
        response_index += 1
        if name == 'echo':
            parts = children(expression)
            valid = len(parts) == 2 and answer == parts[1]
            validations.append({'command': name,
                                'result': 'valid' if valid else 'invalid'})
            continue
        if name == 'get-option':
            parts = children(expression)
            expected = None
            for previous in commands[:index]:
                if command_name(previous) == 'reset':
                    expected = None
                elif command_name(previous) == 'set-option':
                    setting = children(previous)
                    if len(parts) == 2 and len(setting) == 3 \
                            and setting[1] == parts[1]:
                        expected = setting[2]
            validations.append({
                'command': name,
                'result': ('unvalidated' if expected is None else
                           'valid' if answer == expected else 'invalid'),
                'source': 'explicit-option-setting'})
            continue
        if name.startswith("get-") and name not in (
                "get-model", "get-value", "get-unsat-core",
                "get-unsat-assumptions", "get-assignment", "get-assertions"):
            validations.append({"command": name, "result": "unvalidated",
                                "reason": "no validator for this command"})
            continue
        if name not in ("get-model", "get-value", "get-unsat-core",
                        "get-unsat-assumptions", "get-assignment",
                        "get-assertions"):
            continue
        try:
            prefix = commands[:index]
            if name == "get-assertions":
                valid = validate_assertions(prefix, answer)
                validations.append({"command": name,
                                    "result": "valid" if valid else "invalid"})
                continue
            if name == "get-assignment":
                labels = active_annotation_labels(prefix)
                inventory = b'()'
                if labels:
                    replay = [b'(set-option :produce-models true)']
                    consistent = model_script(prefix, b'()', [], observations)
                    for _, _, command in expressions(consistent):
                        parts = children(command)
                        if (command_name(command) == 'set-option'
                                and parts[1] == b':produce-models'):
                            continue
                        replay.append(command)
                        if command_name(command) == 'reset':
                            replay.append(b'(set-option :produce-models true)')
                    probe = b'\n'.join(replay + [
                        b'(get-value (' + b' '.join(labels.values()) + b'))'])
                    result = run_reference(args, probe)
                    if (result.get('statuses') != ['sat']
                            or result.get('errors')
                            or result.get('returncode') != 0):
                        validations.append({"command": name,
                                            "result": "unvalidated",
                                            "reason": "assignment sort probe",
                                            "oracle": result})
                        continue
                    values = [part for _, _, part in expressions(
                        result['stdout'].encode())
                              if part not in (b'sat', b'success')]
                    if len(values) != 1:
                        raise ValueError('unexpected assignment sort output')
                    pairs = [children(item) for item in children(values[0])]
                    if (any(len(pair) != 2 for pair in pairs)
                            or len(pairs) != len(labels)):
                        raise ValueError('unexpected assignment sort terms')
                    # cvc5 expands named terms but preserves get-value order.
                    inventory = b'(' + b' '.join(
                        b'(' + label + b' ' + pair[1] + b')'
                        for label, pair in zip(labels.values(), pairs)
                        if pair[1] in (b'true', b'false')) + b')'
                requested, values = assignment_values(answer, inventory)
                check = model_script(prefix, values, requested, observations)
                observations.append((values, requested))
                expected = 'sat'
            elif name == "get-unsat-core":
                named_core = answer
                check = core_script(prefix, answer,
                                    semantics=getattr(args, "core_semantics",
                                                      "standard"),
                                    assumptions=assumption_core)
                expected = "unsat"
            elif name == "get-unsat-assumptions":
                check = assumption_script(prefix, answer)
                assumption_core = children(answer)
                if named_core is not None:
                    check = core_script(prefix, named_core,
                                        semantics=getattr(args, "core_semantics",
                                                          "standard"),
                                        assumptions=assumption_core)
                expected = "unsat"
            else:
                requested = (children(children(expression)[1])
                             if name == "get-value" else None)
                model_prefix = (prefix + [expression]
                                if name == 'get-value' else prefix)
                check = model_script(model_prefix, answer, requested,
                                     observations)
                observations.append((answer, requested))
                expected = "sat"
            result = run_reference(args, check)
            answer = result.get("statuses")
            conclusive = (answer in (["sat"], ["unsat"])
                          and not result.get("errors")
                          and result.get("returncode") == 0)
            outcome = ("valid" if answer == [expected] else "invalid")
            validations.append({"command": name, "result": (
                outcome if conclusive else "unvalidated"),
                "oracle": result})
        except (ValueError, IndexError, RecursionError) as error:
            validations.append({"command": name, "result": "unvalidated",
                                "reason": str(error)})
    if response_index != len(responses):
        validations.append({'result': 'invalid', 'reason': 'extra response'})
    return validations


def validate_continued_errors(script, tested, args):
    try:
        commands = [part for _, _, part in active_expressions(script)]
    except ValueError as error:
        return {"result": "unvalidated", "reason": str(error)}
    if len(commands) > 256 or len(script) > 1024 * 1024:
        return {"result": "unvalidated", "reason": "error replay input limit"}
    exits = [index for index, command in enumerate(commands)
             if command_name(command) == 'exit'
             and children(command) == [b'exit']]
    terminal_exit = bool(exits)
    exit_responses = 0

    def run(reference, commands):
        if reference:
            return run_reference(args, b'\n'.join(commands))
        return run_script(args.solver, args.solver_args,
                          b'\n'.join(commands), args.timeout,
                          getattr(args, "memory_limit_mib", 0),
                          getattr(args, "output_max_mib", 64))

    def complete(result):
        return (result.get('returncode') in (0, 1)
                and 'response_kinds' in result
                and not result.get('output_limit'))

    policy = run(False, [b'(get-info :error-behavior)'])
    if (policy.get('returncode') != 0 or policy.get('errors')
            or policy.get('stdout', '').strip()
            != '(:error-behavior continued-execution)'):
        return {"result": "unvalidated", "reason": "continued error policy",
                "policy": policy}
    accepted = []
    kinds = []
    statuses = []
    rejected = []
    for index, command in enumerate(commands):
        reference = run(True, [*accepted, command])
        if not complete(reference):
            return {"result": "unvalidated", "reason": "error replay oracle",
                    "command": index, "oracle": reference}
        original_kinds = [kind for kind in kinds if kind != 'error']
        current_kinds = reference['response_kinds']
        current_statuses = reference['statuses']
        if (current_kinds[:len(original_kinds)] != original_kinds
                or current_statuses[:len(statuses)] != statuses):
            return {"result": "unvalidated", "reason": "unstable oracle prefix",
                    "command": index, "oracle": reference}
        delta = current_kinds[len(original_kinds):]
        model_response = (command_name(command) in ('get-model', 'get-value')
                          and delta == ['value'])
        echo_response = (command_name(command) == 'echo' and delta == ['string'])
        if (not model_response and not echo_response
                and any(kind not in ('sat', 'unsat', 'success', 'error')
                        for kind in delta)):
            return {"result": "unvalidated",
                    "reason": "unsupported error replay response",
                    "command": index, "oracle": reference}
        if reference['errors']:
            if len(reference['errors']) != 1 or delta != ['error']:
                return {"result": "unvalidated", "reason": "ambiguous rejection",
                        "command": index, "oracle": reference}
            rejected.append(index)
            kinds.append('error')
        else:
            if reference['returncode'] != 0:
                return {"result": "unvalidated", "reason": "unexpected oracle exit",
                        "command": index, "oracle": reference}
            accepted.append(command)
            kinds.extend(delta)
            statuses = current_statuses
        if terminal_exit and index == len(commands) - 1:
            if reference['errors']:
                return {"result": "unvalidated",
                        "reason": "terminal exit rejected by oracle"}
            exit_responses = len(delta)
        actual = run(False, commands[:index + 1])
        if not complete(actual):
            return {"result": "unvalidated", "reason": "error replay solver",
                    "command": index, "tested": actual}
        if (actual['response_kinds'] != kinds
                or actual['statuses'] != statuses
                or len(actual['errors']) != len(rejected)):
            return {"result": "invalid", "reason": "command rejection or continuation",
                    "command": index, "tested": actual, "oracle": reference}
    if (not complete(tested)
            or tested.get('response_kinds') != kinds
            or tested.get('statuses') != statuses
            or len(tested.get('errors', [])) != len(rejected)):
        return {"result": "invalid", "reason": "full rejection transcript"}
    if not rejected:
        return {"result": "invalid", "reason": "no rejected commands"}
    certificates = []
    model_commands = sum(command_name(command) in ('get-model', 'get-value',
                                                  'echo')
                         for command in accepted)
    if model_commands:
        try:
            output = b'\n'.join(
                answer for _, _, answer in complete_expressions(
                    tested.get('stdout', '').encode())
                if command_name(answer) != 'error')
            certificates = validate_certificates(
                b'\n'.join(accepted), output, args)
        except (ValueError, IndexError, RecursionError) as error:
            return {"result": "unvalidated", "reason": str(error)}
        if (len(certificates) != model_commands
                or any(item['result'] != 'valid' for item in certificates)):
            return {"result": (
                "invalid" if any(item['result'] == 'invalid'
                                 for item in certificates) else "unvalidated"),
                "reason": "error replay certificate validation",
                "certificates": certificates}
    recovery = [b'(set-info :status unknown)', b'(check-sat)',
                b'(push 1)', b'(assert false)', b'(check-sat)',
                b'(pop 1)', b'(check-sat)']
    diagnostic_commands = commands[:-1] if terminal_exit else commands
    diagnostic_accepted = accepted[:-1] if terminal_exit else accepted
    diagnostic_kinds = kinds[:-exit_responses] if exit_responses else kinds
    reference = run(True, diagnostic_accepted + recovery)
    actual = run(False, diagnostic_commands + recovery)
    if (not complete(reference) or not complete(actual)
            or reference.get('errors') or reference.get('returncode') != 0
            or reference.get('statuses', [])[-3:] not in (
                ['sat', 'unsat', 'sat'], ['unsat', 'unsat', 'unsat'])):
        return {"result": "unvalidated", "reason": "recovery oracle",
                "oracle": reference, "tested": actual}
    if (actual['statuses'] != reference['statuses']
            or len(actual['errors']) != len(rejected)
            or actual['response_kinds'] != diagnostic_kinds
               + reference['response_kinds'][
                   len(diagnostic_kinds) - len(rejected):]):
        return {"result": "invalid", "reason": "state after rejected command",
                "oracle": reference, "tested": actual}
    return {"result": "valid", "rejected_commands": rejected,
            "script_sha256": hashlib.sha256(script).hexdigest(),
            "response_kinds": kinds,
            "statuses": statuses,
            "reference_command": [args.reference, *args.reference_args],
            "policy": "continued-execution",
            "recovery_statuses": actual['statuses'][-3:],
            **({"certificates": certificates} if certificates else {}),
            **({"diagnostic_omitted_exit": len(commands) - 1}
               if terminal_exit else {})}



def check_case(item):
    case, args = item
    return check_script(case, args, args.output / case["path"])


def check_script(case, args, script):
    memory_limit = getattr(args, "memory_limit_mib", 0)
    output_maximum = getattr(args, "output_max_mib", 64)
    reference = run_reference(args, script)
    tested = run_script(args.solver, args.solver_args, script, args.timeout,
                        memory_limit, output_maximum)
    expected = case["expected_statuses"]
    oracle = reference.get("statuses", [])
    explicit = case["expected_status_source"] == "EXPECT"
    known = (all(status in ("sat", "unsat") for status in expected)
             or explicit)
    statuses = expected if known else oracle
    expected_errors = sum(item.startswith("(error")
                          for item in case["expected_output"])
    error_fragments = False
    if expected_errors and case.get('expected_error_exit', False):
        try:
            complete_expressions('\n'.join(case['expected_output']).encode())
        except ValueError:
            error_fragments = True
    reference_valid = (reference.get("returncode") is not None
                       and not reference.get("output_limit")
                       and len(oracle) == case["checks"]
                       and (explicit or all(status in ("sat", "unsat")
                                            for status in oracle))
                       and (not known or oracle == expected)
                       and len(reference.get("errors", [])) == expected_errors)
    status_correct = (reference_valid and tested.get("statuses") == statuses)
    error_protocol = (len(tested.get("errors", [])) == expected_errors
                      and len(reference.get("errors", [])) == expected_errors)
    shape_protocol = (tested.get("response_kinds")
                      == reference.get("response_kinds"))
    transport = (tested.get("returncode") is not None
                 and tested.get("returncode") == reference.get("returncode"))
    conflicts = [
        {"check": index + 1, "expected": wanted, "actual": actual}
        for index, (wanted, actual) in enumerate(zip(
            expected, tested.get("statuses", [])))
        if wanted in ("sat", "unsat") and actual in ("sat", "unsat")
        and wanted != actual]
    reference_conflicts = [
        {"check": index + 1, "reference": wanted, "actual": actual}
        for index, (wanted, actual) in enumerate(zip(
            oracle, tested.get("statuses", [])))
        if wanted in ("sat", "unsat") and actual in ("sat", "unsat")
        and wanted != actual]
    result = dict(case, reference=reference, tested=tested,
                  status_correct=status_correct, error_protocol=error_protocol,
                  shape_protocol=shape_protocol,
                  expected_status_conflicts=conflicts,
                  reference_status_conflicts=reference_conflicts,
                  complete_script=True, vendor_flags_applied=False)
    inferred_errors = ((not expected_errors or error_fragments)
                       and (not case['expected_output']
                        or case.get('expected_error_exit', False))
                       and reference.get('errors') and tested.get('errors'))
    result['expected_error_fragments'] = error_fragments
    if (expected_errors or inferred_errors) and tested.get("errors"):
        validation = validate_continued_errors(
            script.read_bytes() if isinstance(script, Path) else script,
            tested, args)
        result["error_recovery"] = validation
        if 'certificates' in validation:
            result['certificates'] = validation['certificates']
        if validation["result"] == "valid":
            source = (script.read_bytes() if isinstance(script, Path)
                      else script)
            commands = [command for _, _, command in active_expressions(source)]
            check_indices = [index for index, command in enumerate(commands)
                             if command_name(command) in CHECKS]
            rejected_checks = [ordinal for ordinal, index in enumerate(
                check_indices, 1) if index in validation['rejected_commands']]
            accepted_expected = [(ordinal, wanted) for ordinal, wanted in
                                 enumerate(expected, 1)
                                 if ordinal not in rejected_checks]
            recovered_conflicts = [
                {'check': ordinal, 'expected': wanted, 'actual': actual}
                for (ordinal, wanted), actual in zip(
                    accepted_expected, tested.get('statuses', []))
                if wanted in ('sat', 'unsat') and actual in ('sat', 'unsat')
                and wanted != actual]
            result['expected_status_conflicts'] = recovered_conflicts
            result['rejected_check_ordinals'] = rejected_checks
            correct = (not recovered_conflicts
                       and (inferred_errors
                            or len(validation['rejected_commands'])
                            == expected_errors)
                       and len(tested.get("statuses", []))
                       == case["checks"] - len(rejected_checks)
                       and all(status in ("sat", "unsat")
                               for status in tested["statuses"]))
            result.update(status_correct=correct, error_protocol=True,
                          shape_protocol=True,
                          result="passed" if correct else "failed")
            return result
        if validation["result"] == "invalid":
            result["result"] = "failed"
            return result
        result["result"] = "statuses_pass_error_recovery_unvalidated"
        return result
    if not reference_valid:
        result["result"] = "reference_inconclusive"
    elif (not status_correct or not error_protocol or not shape_protocol
          or not transport):
        result["result"] = "failed"
    elif (not expected_errors
          and (case["has_model_commands"] or case["has_core_commands"]
               or case["commands"].get("get-unsat-assumptions")
               or case["commands"].get("get-assertions")
               or case["commands"].get("get-option")
               or case["commands"].get("echo"))):
        limit = getattr(args, "certificate_max_mib", 64) * 1024 * 1024
        size = script.stat().st_size if isinstance(script, Path) else len(script)
        if size > limit:
            validations = [{"result": "unvalidated",
                            "reason": "certificate replay input size limit"}]
        else:
            validations = validate_certificates(
                script.read_bytes() if isinstance(script, Path) else script,
                tested["stdout"].encode(), args)
        result["certificates"] = validations
        outcomes = {validation["result"] for validation in validations}
        result["result"] = ("failed" if "invalid" in outcomes else
                            "passed" if outcomes == {"valid"} else
                            "statuses_pass_certificate_unvalidated")
    elif any(name.startswith("get-") for name in case["commands"]):
        result["result"] = "statuses_pass_auxiliary_unvalidated"
    else:
        result["result"] = "passed"
    return result


def suite_group(case):
    return (case["suite"] + "-" + case["track"]
            if case["suite"] == "smtlib" else case["suite"])


def executable_identity(executable, arguments):
    path = Path(shutil.which(executable) or executable).resolve()
    return {"path": str(path), "arguments": arguments,
            "sha256": hash_file(path)}


def saved_results(destination, selected):
    cases = {case["path"]: case for case in selected}
    seen = set()
    with destination.open("r+b") as stream:
        while line := stream.readline():
            start = stream.tell() - len(line)
            try:
                record = json.loads(line)
            except json.JSONDecodeError:
                if line.endswith(b'\n'):
                    raise ValueError("invalid complete result line")
                stream.truncate(start)
                break
            path = record["path"]
            if path not in cases or record["sha256"] != cases[path]["sha256"]:
                raise ValueError("saved result does not match selected case")
            if path in seen:
                raise ValueError("duplicate saved result")
            seen.add(path)
            if not line.endswith(b'\n'):
                stream.write(b'\n')
            yield record


def count_result(summary, record):
    summary["results"][record["result"]] += 1
    for field, key in (("suites", suite_group(record)),
                       ("families", record["source_id"])):
        counts = summary[field].setdefault(key, Counter())
        counts[record["result"]] += 1
    summary["completed"] = sum(summary["results"].values())
    summary["remaining"] = summary["selected"] - summary["completed"]


def run(args):
    destination = args.results or args.output / "results.jsonl"
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.with_suffix(destination.suffix + ".lock").open("a") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise ValueError("another runner owns these results") from error
        return run_locked(args)


def run_locked(args):
    case_index = (getattr(args, "case_index", None)
                  or args.output / "cases.jsonl")
    with case_index.open() as stream:
        cases = [json.loads(line) for line in stream]
    cases = [case for case in cases
             if (not args.suite or case["suite"] in args.suite)
             and (not args.track or case.get("track") in args.track)
             and (not args.family or case.get("family") in args.family)]
    scope = getattr(args, 'interface_scope', 'all')
    for case in cases:
        case['separate_interfaces'] = classify_interfaces(case)
        case['release_scope'] = ('previously-excluded' if not case['eligible']
                                 else 'theory-and-standard-with-extra-interfaces'
                                 if case['separate_interfaces']
                                 else 'theory-and-standard')
    selected = [case for case in cases if case['eligible']
                and (scope != 'separate-interfaces'
                     or case['separate_interfaces'])]
    summary = {"indexed": len(cases), "selected": len(selected),
               "excluded": len(cases) - len(selected), "results": Counter(),
               "original_eligible": sum(case['eligible'] for case in cases),
               "scopes": dict(Counter(case['release_scope'] for case in cases)),
               "classification_rules": classification_rules(),
               "eligibility_unchanged": True,
               "suites": {}, "families": {},
               "solver": executable_identity(args.solver, args.solver_args),
               "reference": executable_identity(args.reference,
                                                args.reference_args),
               "case_index_sha256": hash_file(case_index),
               "manifest_sha256": hash_file(args.output / "manifest.json"),
               "harness_sha256": {
                   name: hash_file(Path(__file__).with_name(name))
                   for name in ("smt_suites.py", "smt_corpus.py")},
               "filters": {"suites": args.suite, "tracks": args.track,
                           "families": args.family, "interface_scope": scope},
               "timeout_seconds": args.timeout, "jobs": args.jobs,
               "memory_limit_mib": args.memory_limit_mib,
               "output_max_mib": args.output_max_mib,
               "certificate_max_mib": args.certificate_max_mib,
               "core_semantics": getattr(args, "core_semantics", "standard"),
               "input_transport": "complete original files streamed to stdin",
               "semantics": "complete original scripts; no sampling",
               "certificate_validation": (
                   "replay cores; check complete acyclic models and values "
                   "against original constraints with the reference solver"),
               "vendor_configuration": (
                   "command-line hints retained, not applied")}
    destination = args.results or args.output / "results.jsonl"
    destination.parent.mkdir(parents=True, exist_ok=True)
    metadata = destination.with_suffix(".run.json")
    completed = set()
    if args.resume:
        if json.loads(metadata.read_text()) != summary:
            raise ValueError("resume settings or executable hashes changed")
        for record in saved_results(destination, selected):
            count_result(summary, record)
            completed.add(record["path"])
    else:
        write_json(metadata, summary)
    summary["completed"] = len(completed)
    summary["remaining"] = len(selected) - len(completed)
    pending = [case for case in selected if case["path"] not in completed]
    with destination.open("a" if args.resume else "w") as stream:
        with ThreadPoolExecutor(max_workers=args.jobs) as executor:
            for record in executor.map(check_case, ((case, args)
                                                    for case in pending)):
                count_result(summary, record)
                expected_conflicts = record["expected_status_conflicts"]
                reference_conflicts = record.get("reference_status_conflicts")
                if ((expected_conflicts or reference_conflicts)
                        and not record["tested"].get("errors")
                        and not record["reference"].get("errors")):
                    print(json.dumps({
                        "status_conflict": record["path"],
                        "expected_conflicts": expected_conflicts,
                        "reference_conflicts": reference_conflicts}),
                        flush=True)
                if any(item["result"] == "invalid"
                       for item in record.get("certificates", [])):
                    print(json.dumps({"invalid_certificate": record["path"]}),
                          flush=True)
                stream.write(json.dumps(record, sort_keys=True) + "\n")
                stream.flush()
                if summary["completed"] % 100 == 0:
                    write_json(destination.with_suffix(".summary.json"),
                               summary)
                    print(json.dumps({"progress": summary["results"],
                                      "selected": len(selected)}), flush=True)
    write_json(destination.with_suffix(".summary.json"), summary)
    print(json.dumps(summary), flush=True)
    return summary


EXTRA_INTERFACES = {
    'get-proof': 'proof_export',
    'get-unsat-core-lemmas': 'proof_export',
    'find-synth': 'synthesis',
    'find-synth-next': 'synthesis',
    'dbg-set-next-id': 'internal_debugging',
    'get-difficulty': 'internal_debugging',
    'get-learned-literals': 'internal_debugging',
    'get-timeout-core': 'internal_debugging',
    'get-timeout-core-assuming': 'internal_debugging',
}


EXTRA_INTERFACE_OPTIONS = {
    (':output', 'post-asserts'): 'internal_debugging',
    (':parse-skolem-definitions', 'true'): 'internal_debugging',
    (':dump-proofs', 'true'): 'proof_export',
    (':dump-difficulty', 'true'): 'internal_debugging',
    (':minisat-dump-dimacs', 'true'): 'internal_debugging',
}

STRATEGY_INTERFACES = {'check-sat-using': 'tactic_selection'}


def classification_rules():
    return {'commands': EXTRA_INTERFACES,
            'query_commands': STRATEGY_INTERFACES,
            'options': [{'option': option, 'value': value,
                         'interface': interface}
                        for (option, value), interface in
                        EXTRA_INTERFACE_OPTIONS.items()]}


def classify_interfaces(case):
    reasons = [{'command': name, 'interface': EXTRA_INTERFACES[name]}
               for name in sorted(case.get('commands', {}))
               if name in EXTRA_INTERFACES]
    reasons.extend({'command': name, 'interface': STRATEGY_INTERFACES[name]}
                   for name in sorted(case.get('commands', {}))
                   if name in STRATEGY_INTERFACES)
    for option in case.get('options', []):
        key = option['option'], option['value']
        if key in EXTRA_INTERFACE_OPTIONS:
            reasons.append({'option': key[0], 'value': key[1],
                            'interface': EXTRA_INTERFACE_OPTIONS[key]})
    return reasons


RESOURCE_OPTIONS = {b':rlimit', b':rlimit-per', b':reproducible-resource-limit'}


def timing_script(data):
    parts = []
    transformations = []
    previous = 0
    for start, end, command in active_expressions(data):
        name = command_name(command)
        expression = children(command) if command.startswith(b'(') else []
        reason = EXTRA_INTERFACES.get(name)
        replacement = command
        if (name == 'set-option' and len(expression) == 3
                and (expression[1].decode(), expression[2].decode())
                in EXTRA_INTERFACE_OPTIONS):
            reason = EXTRA_INTERFACE_OPTIONS[(expression[1].decode(),
                                              expression[2].decode())]
        if name in STRATEGY_INTERFACES and len(expression) == 2:
            replacement = b'(check-sat)'
            replacement += b' ' * (len(command) - len(replacement))
            reason, action = STRATEGY_INTERFACES[name], 'separate-interface'
        elif reason is not None:
            replacement = re.sub(rb'[^\r\n]', b' ', command)
            action = 'separate-interface'
        elif (name == 'set-option' and len(expression) == 3
              and expression[1] in RESOURCE_OPTIONS
              and re.fullmatch(rb'0|[1-9][0-9]*', expression[2])
              and expression[2] != b'0'):
            replacement = b'(set-option ' + expression[1] + b' 0)'
            if len(replacement) > len(command):
                raise ValueError('resource option cannot preserve source offsets')
            replacement += b' ' * (len(command) - len(replacement))
            reason, action = 'disable-reproducible-limit', 'timing-policy'
        if replacement != command:
            transformations.append({
                'command_start': start, 'command_end': end, 'command': name,
                'source_sha256': hashlib.sha256(command).hexdigest(),
                'replacement_sha256': hashlib.sha256(replacement).hexdigest(),
                'action': action, 'reason': reason,
            })
            parts.extend((data[previous:start], replacement))
            previous = end
    parts.append(data[previous:])
    canonical = b''.join(parts)
    return canonical, {
        'source_sha256': hashlib.sha256(data).hexdigest(),
        'canonical_sha256': hashlib.sha256(canonical).hexdigest(),
        'source_offsets_preserved': True, 'transformations': transformations,
    }


def vendor_configuration(data, command_lines):
    names = {
        '--produce-models': ':produce-models',
        '--produce-assignments': ':produce-assignments',
        '--produce-unsat-cores': ':produce-unsat-cores',
        '--produce-unsat-assumptions': ':produce-unsat-assumptions',
        '--produce-assertions': ':produce-assertions',
        '--interactive-mode': ':interactive-mode',
        '--global-declarations': ':global-declarations',
    }
    variants = []
    try:
        for line in command_lines:
            options = {}
            unprojected = []
            arguments = shlex.split(line)
            for argument in arguments:
                if argument in ('-m', '-qm', '-mq'):
                    options[':produce-models'] = 'true'
                    if argument != '-m':
                        unprojected.append('-q')
                    continue
                name, separator, value = argument.partition('=')
                if name.startswith('--no-') and not separator:
                    name, value = '--' + name[5:], 'false'
                elif not separator:
                    value = 'true'
                if name in names:
                    if value not in ('true', 'false'):
                        raise ValueError('invalid vendor Boolean: ' + argument)
                    options[names[name]] = value
                else:
                    unprojected.append(argument)
            variants.append({'arguments': arguments, 'options': options,
                             'unprojected_arguments': unprojected})
    except ValueError as error:
        return data, {'result': 'unvalidated', 'reason': str(error)}
    record = {'result': 'valid', 'variants': variants,
              'policy': 'standard-SMT-LIB-option-projection',
              'source_sha256': hashlib.sha256(data).hexdigest()}
    configurations = [item['options'] for item in variants]
    if any(options != configurations[0] for options in configurations[1:]):
        return data, dict(record, result='unvalidated',
                          reason='vendor variants require different options')
    options = configurations[0] if configurations else {}
    commands = [f'(set-option {name} {value})'.encode()
                for name, value in sorted(options.items())]
    if not commands:
        return data, dict(record, configured_sha256=record['source_sha256'],
                          insertions=[])
    prelude = b'\n'.join(commands) + b'\n'
    insertions = [{'source_offset': 0, 'commands': [part.decode()
                                                 for part in commands]}]
    parts = [prelude]
    previous = 0
    for _, end, command in active_expressions(data):
        if command_name(command) == 'reset' and children(command) == [b'reset']:
            parts.extend((data[previous:end], b'\n', prelude))
            previous = end
            insertions.append({'source_offset': end,
                               'commands': [part.decode()
                                            for part in commands]})
    parts.append(data[previous:])
    configured = b''.join(parts)
    return configured, dict(record, insertions=insertions,
                            configured_sha256=hashlib.sha256(
                                configured).hexdigest())


STANDARD_FOLLOWUPS = {
    'get-model', 'get-value', 'get-assignment', 'get-unsat-core',
    'get-unsat-assumptions', 'get-assertions',
}


def query_scripts(data, rejected_commands=()):
    prefix = []
    ordinal = 0
    rejected_commands = set(rejected_commands)
    expressions = list(active_expressions(data))
    for position, (start, end, command) in enumerate(expressions):
        name = command_name(command)
        if name == 'exit' and children(command) == [b'exit']:
            break
        if name == 'reset' and position not in rejected_commands:
            prefix = []
        if name in CHECKS:
            ordinal += 1
            commands = []
            rejected = []
            for index, expression in prefix:
                if index in rejected_commands:
                    commands.append(expression)
                    rejected.append(index)
                else:
                    commands.extend(logical_prefix([expression]))
            commands.append(command)
            if position in rejected_commands:
                rejected.append(position)
            if not commands or commands[0] != b'(reset)':
                commands.insert(0, b'(reset)')
            original = b'\n'.join(commands) + b'\n'
            disabled = []
            measured = []
            for expression in commands:
                parts = children(expression)
                if (len(parts) == 3 and parts[0] == b'set-option'
                        and parts[1] in RESOURCE_OPTIONS
                        and re.fullmatch(rb'0|[1-9][0-9]*', parts[2])):
                    disabled.append({'option': parts[1].decode(),
                                     'value': parts[2].decode()})
                    expression = b'(set-option ' + parts[1] + b' 0)'
                measured.append(expression)
            script = b'\n'.join(measured) + b'\n'
            followups = []
            followup_indices = []
            followup_rejected = []
            followup_end = end
            for index, (_, next_end, expression) in enumerate(
                    expressions[position + 1:], position + 1):
                next_name = command_name(expression)
                if next_name not in (STANDARD_FOLLOWUPS
                                     | {'echo', 'get-info', 'get-option'}):
                    break
                if next_name in STANDARD_FOLLOWUPS:
                    followups.append(expression)
                    followup_indices.append(index)
                    if index in rejected_commands:
                        followup_rejected.append(index)
                    followup_end = next_end
            operation_commands = followups
            trailing = []
            for index, (_, next_end, expression) in enumerate(
                    expressions[position + 1:], position + 1):
                next_name = command_name(expression)
                if (next_name in CHECKS | {'exit'}
                        or next_name == 'reset'
                        and index not in rejected_commands):
                    break
                trailing.append((index, next_end, expression))
            rejected_followups = [i for i, _, part in trailing
                                  if i in rejected_commands
                                  and command_name(part) in STANDARD_FOLLOWUPS]
            if rejected_followups:
                last = max(followup_indices + rejected_followups)
                trailing = [item for item in trailing if item[0] <= last]
                operation_commands = [part for _, _, part in trailing]
                followups = [part for _, _, part in trailing
                             if command_name(part) in STANDARD_FOLLOWUPS]
                followup_indices = [i for i, _, part in trailing
                                    if command_name(part) in STANDARD_FOLLOWUPS]
                followup_rejected = [i for i, _, _ in trailing
                                     if i in rejected_commands]
                followup_end = trailing[-1][1]
            operation = script + b'\n'.join(operation_commands) + b'\n'
            yield {'ordinal': ordinal, 'command_start': start,
                   'command_end': end, 'command': name,
                   'query_sha256': hashlib.sha256(original).hexdigest(),
                   'measurement_sha256': hashlib.sha256(script).hexdigest(),
                   'disabled_resource_limits': disabled,
                   'check_rejected': position in rejected_commands,
                   'expected_error_commands': rejected,
                   'operation_expected_error_commands': (
                       rejected + followup_rejected),
                   'followup_commands': [command_name(item)
                                         for item in followups],
                   'followup_command_indices': followup_indices,
                   'operation_end': followup_end,
                   'operation_sha256': hashlib.sha256(operation).hexdigest()
                   if followups else None}, script, (operation
                                                    if followups else None)
        prefix.append((position, command))


class QuerySolver:
    def __init__(self, executable, arguments, args):
        self.reference_profile = getattr(args, 'reference_profile', None)
        self.command = [executable, *arguments]
        if getattr(args, 'memory_limit_mib', 0):
            limiter = shutil.which('prlimit')
            if not limiter:
                raise ValueError('memory limits require prlimit')
            self.command = [limiter,
                            f'--as={args.memory_limit_mib * 1024 * 1024}',
                            '--', *self.command]
        self.timeout = args.timeout
        self.output_limit = getattr(args, 'output_max_mib', 64) * 1024 * 1024
        self.process = None

    def close(self):
        if self.process is not None:
            self.process.kill()
            self.process.communicate()
            self.process = None

    def memory(self):
        if self.process is None or platform.system() != 'Linux':
            return {}
        try:
            status = Path(f'/proc/{self.process.pid}/status').read_text()
        except OSError:
            return {}
        fields = {}
        for line in status.splitlines():
            parts = line.split()
            if parts and parts[0] in ('VmRSS:', 'VmHWM:'):
                key = 'rss_bytes' if parts[0] == 'VmRSS:' else 'peak_rss_bytes'
                fields[key] = int(parts[1]) * 1024
        return fields

    def run(self, script, *, auxiliary=False, error_protocol=None):
        if self.process is None:
            try:
                self.process = subprocess.Popen(
                    self.command, stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                    bufsize=0)
                os.set_blocking(self.process.stdin.fileno(), False)
                os.set_blocking(self.process.stdout.fileno(), False)
                os.set_blocking(self.process.stderr.fileno(), False)
            except OSError as error:
                return {'result': 'crash', 'seconds': 0,
                        'errors': [str(error)]}
            ready = self.exchange(b'(reset)', checks=0)
            if ready['result'] != 'complete':
                ready['phase'] = 'startup'
                self.close()
                return ready
        if error_protocol is not None:
            ready = self.exchange(b'(set-option :print-success false)',
                                  checks=0)
            if ready['result'] != 'complete':
                ready['phase'] = 'preparation'
                self.close()
                return ready
        adapted, adaptation = reference_script(script, self.reference_profile)
        if error_protocol is not None and adapted != script:
            error_protocol = dict(error_protocol,
                                  script_sha256=adaptation['adapted_sha256'])
        checks = 1
        if (error_protocol is not None
                and error_protocol.get('result') == 'valid'
                and error_protocol.get('script_sha256')
                == hashlib.sha256(adapted).hexdigest()):
            checks = sum(kind in ('sat', 'unsat', 'unknown')
                         for kind in error_protocol['response_kinds'])
        result = self.exchange(adapted, checks=checks, auxiliary=auxiliary,
                               error_protocol=error_protocol)
        result.update(self.memory())
        if adaptation is not None:
            result['reference_adaptation'] = adaptation
        return result

    def exchange(self, script, checks, auxiliary=False, error_protocol=None):
        marker = b'vox-external-query-' + secrets.token_hex(16).encode()
        outgoing = memoryview(script + b'\n(echo "' + marker + b'")\n')
        output = bytearray()
        diagnostics = bytearray()
        started = time.perf_counter()
        try:
            with selectors.DefaultSelector() as selector:
                selector.register(self.process.stdin, selectors.EVENT_WRITE)
                selector.register(self.process.stdout, selectors.EVENT_READ)
                selector.register(self.process.stderr, selectors.EVENT_READ)
                while (marker + b'\n' not in output
                       and b'"' + marker + b'"\n' not in output):
                    remaining = self.timeout - (time.perf_counter() - started)
                    if remaining <= 0:
                        raise TimeoutError
                    events = selector.select(remaining)
                    if not events:
                        raise TimeoutError
                    for key, _ in events:
                        if key.fileobj is self.process.stdin:
                            count = self.process.stdin.write(outgoing[:65536])
                            if count is None:
                                continue
                            if count == 0:
                                raise BrokenPipeError
                            outgoing = outgoing[count:]
                            if not outgoing:
                                selector.unregister(self.process.stdin)
                        else:
                            chunk = key.fileobj.read(65536)
                            if chunk is None:
                                continue
                            if not chunk:
                                if key.fileobj is self.process.stderr:
                                    selector.unregister(key.fileobj)
                                    continue
                                raise BrokenPipeError
                            target = (diagnostics
                                      if key.fileobj is self.process.stderr
                                      else output)
                            target.extend(chunk)
                            if len(target) > self.output_limit:
                                self.close()
                                return {'result': 'output-limit',
                                        'seconds': (time.perf_counter()
                                                    - started),
                                        'errors': []}
            while chunk := self.process.stderr.read(65536):
                diagnostics.extend(chunk)
                if len(diagnostics) > self.output_limit:
                    self.close()
                    return {'result': 'output-limit',
                            'seconds': time.perf_counter() - started, 'errors': []}
            elapsed = time.perf_counter() - started
            responses = [part for _, _, part in complete_expressions(output)]
            transcript = [part for part in responses
                          if part not in (marker, b'"' + marker + b'"')]
            kinds = [response_kind(part) for part in transcript]
            expected_errors = []
            error_protocol_valid = (
                error_protocol is not None
                and error_protocol.get('result') == 'valid'
                and error_protocol.get('script_sha256')
                == hashlib.sha256(script).hexdigest()
                and error_protocol.get('response_kinds') == kinds)
            if error_protocol_valid:
                expected_errors = [part.decode(errors='replace')
                                   for part in transcript
                                   if command_name(part) == 'error']
            answers = [part.decode() for part in responses
                       if part in (b'sat', b'unsat', b'unknown')]
            errors = [part.decode(errors='replace') for part in responses
                      if (command_name(part) == 'error' or not auxiliary)
                      and not (error_protocol_valid
                               and command_name(part) == 'error')
                      and part not in (b'sat', b'unsat', b'unknown', b'success',
                                      b'unsupported', marker,
                                      b'"' + marker + b'"')]
            if error_protocol is not None and not error_protocol_valid:
                errors.append('expected error transcript mismatch')
            if diagnostics:
                errors.append('solver wrote diagnostics to stderr')
            if b'unsupported' in responses:
                errors.append('solver returned unsupported')
            if len(answers) != checks:
                errors.append('unexpected satisfiability response count')
            result = ('error' if errors else
                      answers[0] if answers else 'complete')
            return {'result': result, 'seconds': elapsed, 'errors': errors,
                    **({'expected_errors': expected_errors,
                        'response_kinds': kinds}
                       if error_protocol is not None else {}),
                    'stderr': diagnostics.decode(errors='replace'),
                    'unsupported_responses': responses.count(b'unsupported'),
                    **({'stdout': bytes(output).replace(
                        b'\"' + marker + b'\"', b'').replace(marker, b'')
                        .decode(errors='replace')} if auxiliary else {})}
        except TimeoutError:
            self.close()
            return {'result': 'timeout',
                    'seconds': time.perf_counter() - started, 'errors': [],
                    'stderr': diagnostics.decode(errors='replace')}
        except (OSError, ValueError) as error:
            self.close()
            return {'result': 'crash', 'seconds': time.perf_counter() - started,
                    'errors': [str(error)],
                    'stderr': diagnostics.decode(errors='replace')}


def summarize_query(query, runs, expected, instant_ms, evidence=None,
                    *, error_protocol=None):
    evidence = evidence or []
    timing = {name: statistics.median(sample['seconds'] for sample in samples)
              for name, samples in runs.items()}
    answers = {name: {sample['result'] for sample in samples}
               for name, samples in runs.items()}
    definitive = {sample['result'] for sample in runs['z3']
                  if sample['result'] in ('sat', 'unsat')
                  and not sample.get('errors')
                  and not sample.get('stderr')
                  and not sample.get('unsupported_responses')
                  and not sample.get('phase')}
    evidence_answers = {item['result'] for item in evidence
                        if item.get('validated')
                        and item.get('result') in ('sat', 'unsat')}
    combined = definitive | evidence_answers
    if expected in ('sat', 'unsat'):
        combined.add(expected)
    reference_known = bool(definitive or evidence_answers)
    correct = (reference_known and len(combined) == 1
               and answers['ocaml'] == combined
               and not any(sample.get('errors') or sample.get('phase')
                           or sample.get('stderr')
                           or sample.get('unsupported_responses')
                           for sample in runs['ocaml']))
    rejected = query.get('check_rejected', False)
    reference_results = {'sat', 'unsat', 'timeout'}
    if rejected:
        correct = (error_protocol is not None
                   and error_protocol.get('result') == 'valid'
                   and all(sample['result'] == 'complete'
                           and sample.get('expected_errors')
                           and sample.get('response_kinds')
                           == error_protocol['response_kinds']
                           and not any(sample.get(key) for key in (
                               'errors', 'phase', 'stderr',
                               'unsupported_responses'))
                           for sample in runs['ocaml']))
        reference_results = {'complete', 'timeout'}
    if timing['ocaml'] <= instant_ms / 1000:
        performance, basis = True, 'instant'
    elif (answers['z3'] <= reference_results
          and not any(sample.get('phase') or sample.get('errors')
                      or sample.get('stderr')
                      or sample.get('unsupported_responses')
                      for sample in runs['z3'])):
        performance = timing['ocaml'] <= 2 * timing['z3']
        basis = ('z3-timeout-lower-bound' if 'timeout' in answers['z3']
                 else 'z3-validated-rejection' if rejected
                 else 'z3-definitive')
        if not performance and 'timeout' in answers['z3']:
            performance = None
    else:
        performance, basis = None, 'z3-inconclusive'
    result = dict(query, runs=runs, evidence=evidence, expected=expected,
                  median_seconds=timing, correct=correct,
                  performance_pass=performance, performance_basis=basis,
                  passed=correct and performance is True)
    if rejected:
        result['expected_response'] = 'error'
    return result


def timing_error_protocol(script, rejected, args):
    if not rejected:
        return None
    tested = run_script(args.solver, args.solver_args, script, args.timeout,
                        getattr(args, 'memory_limit_mib', 0),
                        getattr(args, 'output_max_mib', 64))
    validation = validate_continued_errors(script, tested, args)
    if validation['result'] == 'valid':
        commands = [command for _, _, command in complete_expressions(script)]
        actual = [commands[index]
                  for index in validation['rejected_commands']]
        if actual != rejected:
            return dict(validation, result='invalid',
                        reason='projected rejected commands changed')
    return validation


def time_case(case, args):
    data = (args.output / case['path']).read_bytes()
    if hashlib.sha256(data).hexdigest() != case['sha256']:
        raise ValueError('external case hash changed: ' + case['path'])
    case = dict(case, expected_error_exit=bool(re.search(
        rb'^\s*;\s*EXIT:\s*1\s*$', data, re.M)))
    raw_args = argparse.Namespace(**vars(args))
    if not args.reference or case.get('suite') != 'cvc5':
        raw_args.reference = args.z3
        raw_args.reference_args = args.z3_args
    raw = check_script(case, raw_args, data)
    try:
        canonical, transformation = timing_script(data)
        canonical, configuration = vendor_configuration(
            canonical, case.get('vendor_command_lines', [])
            if case.get('suite') == 'cvc5' else [])
        transformation['vendor_configuration'] = configuration
        transformation['canonical_sha256'] = hashlib.sha256(
            canonical).hexdigest()
        if configuration.get('insertions'):
            transformation['source_offsets_preserved'] = False
    except (ValueError, RecursionError) as error:
        return dict(case, queries=[], raw_complete_script=raw,
                    result='query_replay_unvalidated',
                    query_error=str(error))
    canonical_case = dict(case, **analyze_script(canonical))
    canonical_case['expected_statuses'] = case['expected_statuses']
    canonical_case['expected_status_source'] = case['expected_status_source']
    if any(item['action'] == 'separate-interface'
           for item in transformation['transformations']):
        canonical_case['expected_output'] = [item for item in
                                            case['expected_output']
                                            if item in ('sat', 'unsat', 'unknown')]
    canonical_result = (check_script(canonical_case, raw_args, canonical)
                        if canonical != data else raw)
    protocol_args = raw_args
    if (canonical_result['result'] == 'reference_inconclusive'
            and args.reference and raw_args.reference != args.reference):
        alternate_result = check_script(canonical_case, args, canonical)
        canonical_result = dict(alternate_result,
                                primary_attempt=canonical_result)
        protocol_args = args
    z3_args = argparse.Namespace(**vars(args))
    z3_args.reference = args.z3
    z3_args.reference_args = args.z3_args
    z3_args.reference_profile = 'z3'
    reference_is_z3 = (
        Path(shutil.which(protocol_args.reference)
             or protocol_args.reference).resolve()
        == Path(shutil.which(args.z3) or args.z3).resolve())
    if ((canonical_result['result'] == 'failed' and reference_is_z3
         or canonical_result['result'] in (
            'reference_inconclusive',
            'statuses_pass_error_recovery_unvalidated',
            'statuses_pass_certificate_unvalidated'))
            and reference_script(canonical, 'z3')[1] is not None):
        adapted_result = check_script(canonical_case, z3_args, canonical)
        canonical_result = dict(adapted_result,
                                primary_attempt=canonical_result)
        protocol_args = z3_args
    solvers = {
        'ocaml': QuerySolver(args.solver, args.solver_args, args),
        'z3': QuerySolver(args.z3, args.z3_args, z3_args),
    }
    alternate = (QuerySolver(args.reference, args.reference_args, args)
                 if getattr(args, 'reference', None) else None)
    rows = []
    rejection_operation = None
    try:
        error_recovery = canonical_result.get('error_recovery', {})
        rejected = (error_recovery.get('rejected_commands', [])
                    if canonical_result['result'] == 'passed'
                    and error_recovery.get('result') == 'valid' else [])
        commands = [command for _, _, command in
                    active_expressions(canonical)]
        for query, script, operation in query_scripts(canonical, rejected):
            error_protocol = timing_error_protocol(
                script, [commands[index]
                         for index in query['expected_error_commands']],
                protocol_args)
            if error_protocol is not None:
                query['error_recovery'] = error_protocol
            runs = {'ocaml': [], 'z3': []}
            for repeat in range(args.repeats):
                order = (('ocaml', 'z3') if (query['ordinal'] + repeat) % 2
                         else ('z3', 'ocaml'))
                for name in order:
                    runs[name].append(solvers[name].run(
                        script, error_protocol=error_protocol))
            evidence = []
            expected = case['expected_statuses'][query['ordinal'] - 1]
            if (error_protocol is not None
                    and error_protocol.get('result') == 'valid'
                    and error_protocol.get('statuses') in (['sat'], ['unsat'])
                    and error_protocol.get('script_sha256')
                    == query['measurement_sha256']):
                identity = executable_identity(protocol_args.reference,
                                               protocol_args.reference_args)
                independent = identity['sha256'] != executable_identity(
                    args.solver, args.solver_args)['sha256']
                evidence.append({
                    'source': 'validated-error-recovery',
                    'result': error_protocol['statuses'][0],
                    'reference': identity, 'independent': independent,
                    'validated': independent})
            if (not query['check_rejected']
                    and not any(run['result'] in ('sat', 'unsat')
                                for run in runs['z3'])):
                if alternate is not None:
                    sample = alternate.run(script,
                                           error_protocol=error_protocol)
                    independent = (executable_identity(
                        args.reference, args.reference_args)['sha256'] !=
                        executable_identity(args.solver, args.solver_args)
                        ['sha256'])
                    evidence.append(dict(sample, source='independent-reference',
                                         independent=independent,
                                         validated=independent
                                         and sample['result'] in
                                         ('sat', 'unsat')))
                verifier = getattr(args, 'reference_verifier', None)
                if verifier and not any(item['validated'] for item in evidence):
                    checked = verifier.verify(script, runs, captured=[expected])
                    if checked is not None:
                        evidence.append(dict(
                            checked, source='checked-assertion-subset',
                            result='unsat', validated=checked['verified']))
            row = summarize_query(query, runs, expected,
                                  args.instant_ms, evidence,
                                  error_protocol=error_protocol)
            if operation is not None:
                operation_protocol = timing_error_protocol(
                    operation, [commands[index] for index in
                                query['operation_expected_error_commands']],
                    protocol_args)
                operation_runs = {'ocaml': [], 'z3': []}
                for repeat in range(args.repeats):
                    order = (('ocaml', 'z3')
                             if (query['ordinal'] + repeat) % 2
                             else ('z3', 'ocaml'))
                    for name in order:
                        operation_runs[name].append(
                            solvers[name].run(
                                operation, auxiliary=True,
                                error_protocol=operation_protocol))
                operation_evidence = [*evidence, *(
                    dict(sample, source='query-z3', validated=True)
                    for sample in runs['z3']
                    if sample['result'] in ('sat', 'unsat')
                    and not sample.get('errors'))]
                operation_result = summarize_query(
                    {'check_rejected': query['check_rejected']},
                    operation_runs, expected, args.instant_ms,
                    operation_evidence, error_protocol=operation_protocol)
                if operation_protocol is not None:
                    operation_result['error_recovery'] = operation_protocol
                certificate_args = argparse.Namespace(**vars(args))
                certificate_args.reference_profile = 'z3'
                certificate_args.reference = args.z3
                certificate_args.reference_args = args.z3_args
                certificates = []
                for sample in operation_runs['ocaml']:
                    if (sample['result'] in ('sat', 'unsat')
                            or query['check_rejected']
                            and sample['result'] == 'complete'
                            and operation_protocol is not None
                            and operation_protocol['result'] == 'valid'):
                        certificate_script = operation
                        certificate_output = sample['stdout'].encode()
                        if (operation_protocol is not None
                                and operation_protocol['result'] == 'valid'):
                            removed = operation_protocol['rejected_commands']
                            certificate_script = b'\n'.join(
                                command for index, (_, _, command) in enumerate(
                                    complete_expressions(operation))
                                if index not in removed)
                            certificate_output = b'\n'.join(
                                answer for _, _, answer in
                                complete_expressions(certificate_output)
                                if command_name(answer) != 'error')
                            certificates.append({
                                'result': 'valid',
                                'source': 'verified-command-rejection'})
                        checked = validate_certificates(
                            certificate_script, certificate_output,
                            certificate_args)
                        if (args.reference and checked
                                and not any(item['result'] == 'invalid'
                                            for item in checked)
                                and any(item['result'] == 'unvalidated'
                                        for item in checked)):
                            alternate_args = argparse.Namespace(**vars(args))
                            alternate_checked = validate_certificates(
                                certificate_script, certificate_output,
                                alternate_args)
                            if len(checked) == len(alternate_checked):
                                checked = [dict(second, primary_attempt=first)
                                           if first['result'] == 'unvalidated'
                                           else first for first, second in
                                           zip(checked, alternate_checked)]
                        certificates.extend(checked)
                    else:
                        certificates.append({'result': 'unvalidated',
                                             'reason': 'operation incomplete'})
                certificate_valid = (bool(certificates)
                                     and all(item['result'] == 'valid'
                                             for item in certificates))
                operation_result['certificates'] = certificates
                operation_result['correct'] &= certificate_valid
                operation_result['passed'] &= certificate_valid
                row['operation'] = operation_result
            rows.append(row)
        if (not rows and canonical_result['result'] == 'passed'
                and error_recovery.get('result') == 'valid'):
            script = canonical
            active = list(active_expressions(script))
            omitted_exit = None
            if (active and command_name(active[-1][2]) == 'exit'
                    and children(active[-1][2]) == [b'exit']):
                omitted_exit = active[-1][0]
                script = script[:omitted_exit]
            script = b'(reset)\n' + script
            protocol = timing_error_protocol(
                script, [commands[index] for index in rejected], protocol_args)
            runs = {'ocaml': [], 'z3': []}
            for repeat in range(args.repeats):
                order = ('ocaml', 'z3') if repeat % 2 else ('z3', 'ocaml')
                for name in order:
                    runs[name].append(solvers[name].run(
                        script, auxiliary=True, error_protocol=protocol))
            rejection_operation = summarize_query(
                {'check_rejected': True, 'kind': 'command-rejection',
                 'initial_reset': True,
                 'measurement_sha256': hashlib.sha256(script).hexdigest(),
                 'omitted_terminal_exit_offset': omitted_exit,
                 'error_recovery': protocol}, runs, None, args.instant_ms,
                error_protocol=protocol)
    except (ValueError, IndexError, RecursionError) as error:
        return dict(case, queries=rows, result='query_replay_unvalidated',
                    transformation=transformation, raw_complete_script=raw,
                    canonical_complete_script=canonical_result,
                    query_error=str(error))
    finally:
        for solver in solvers.values():
            solver.close()
        if alternate:
            alternate.close()
    complete = (len(rows) == case['checks']
                and configuration['result'] == 'valid')
    requested = sum(case['commands'].get(name, 0)
                    for name in STANDARD_FOLLOWUPS)
    measured_indices = {index for row in rows for index in (
        row['followup_command_indices'] + row['expected_error_commands'])
        if command_name(commands[index]) in STANDARD_FOLLOWUPS}
    if rejection_operation is not None and rejection_operation['passed']:
        measured_indices.update(index for index in rejected
                                if command_name(commands[index])
                                in STANDARD_FOLLOWUPS)
    measured = len(measured_indices)
    complete = complete and requested == measured
    protocol_passed = (canonical_result['result'] == 'passed'
                       and not canonical_result['tested'].get('stderr'))
    if canonical_result['result'] == 'reference_inconclusive':
        tested = canonical_result['tested']
        expected_answers = [row['runs']['ocaml'][0]['result'] for row in rows]
        protocol_passed = (tested.get('returncode') == 0
                           and not tested.get('errors')
                           and not tested.get('stderr')
                           and tested.get('statuses') == expected_answers
                           and all(kind in ('sat', 'unsat', 'success')
                                   for kind in tested.get('response_kinds', []))
                           and not any(name.startswith('get-') for name in
                                       canonical_case['commands']))
    complete = (complete and protocol_passed
                and (rejection_operation is None
                     or rejection_operation['passed']))
    return dict(case, queries=rows, transformation=transformation,
                raw_complete_script=raw,
                canonical_complete_script=canonical_result,
                protocol_passed=protocol_passed,
                **({'rejection_operation': rejection_operation}
                   if rejection_operation is not None else {}),
                unmeasured_followups=requested - measured,
                result=('no_check_call' if not rows and complete else
                        'passed' if complete and all(row['passed']
                                                    and row.get('operation', {})
                                                    .get('passed', True)
                                                    for row in rows) else
                        'failed'))


def gate_cases(args):
    case_index = (getattr(args, 'case_index', None)
                  or args.output / 'cases.jsonl')
    with case_index.open() as stream:
        cases = [json.loads(line) for line in stream]
    cases = [case for case in cases
             if (not args.suite or case['suite'] in args.suite)
             and (not args.track or case.get('track') in args.track)
             and (not args.family or case.get('family') in args.family)]
    for case in cases:
        case['separate_interfaces'] = classify_interfaces(case)
        case['release_scope'] = ('previously-excluded' if not case['eligible']
                                 else 'theory-and-standard-with-extra-interfaces'
                                 if case['separate_interfaces']
                                 else 'theory-and-standard')
    return case_index, cases


def scope_report(args):
    case_index, cases = gate_cases(args)
    destination = args.results or args.output / 'scope.jsonl'
    destination.parent.mkdir(parents=True, exist_ok=True)
    summary = {'case_index_sha256': hash_file(case_index),
               'indexed': len(cases),
               'original_eligible': sum(case['eligible'] for case in cases),
               'scope': dict(Counter(case['release_scope'] for case in cases)),
               'classification_rules': classification_rules(),
               'eligibility_unchanged': True}
    with destination.open('w') as stream:
        for case in cases:
            stream.write(json.dumps(case, sort_keys=True) + '\n')
    write_json(destination.with_suffix('.summary.json'), summary)
    print(json.dumps(summary), flush=True)
    return summary


def query_gate(args):
    if args.jobs != 1:
        raise ValueError('query timing qualification requires --jobs 1')
    if args.repeats <= 0 or args.timeout <= 0 or args.instant_ms < 0:
        raise ValueError('invalid query timing limits')
    candidate = executable_identity(args.solver, args.solver_args)['sha256']
    references = [(args.z3, args.z3_args)]
    if args.reference:
        references.append((args.reference, args.reference_args))
    if any(executable_identity(executable, arguments)['sha256'] == candidate
           for executable, arguments in references):
        raise ValueError('query qualification requires independent reference '
                         'executables')
    destination = args.results or args.output / 'query-results.jsonl'
    destination.parent.mkdir(parents=True, exist_ok=True)
    with destination.with_suffix('.lock').open('a') as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise ValueError('another runner owns these results') from error
        return query_gate_locked(args, destination)


def query_gate_locked(args, destination):
    case_index, cases = gate_cases(args)
    evidence_path = getattr(args, 'checked_core_evidence', None)
    prior_evidence = getattr(args, 'prior_reference_evidence', [])
    if prior_evidence and evidence_path is None:
        raise ValueError('prior evidence requires --checked-core-evidence')
    if evidence_path is not None:
        from smt_reference import ReferenceVerifier
        args.reference_verifier = ReferenceVerifier(
            args.solver, args.z3, evidence_path, args.timeout, prior_evidence)
    selected = [case for case in cases if case['eligible']
                and (args.interface_scope != 'separate-interfaces'
                     or case['separate_interfaces'])]
    configuration = {
        'host': platform.node(), 'platform': platform.platform(),
        'cpu_affinity': (sorted(os.sched_getaffinity(0))
                         if hasattr(os, 'sched_getaffinity') else None),
        'case_index_sha256': hash_file(case_index),
        'manifest_sha256': hash_file(args.output / 'manifest.json'),
        'harness_sha256': {name: hash_file(Path(__file__).with_name(name))
                          for name in ('smt_suites.py', 'smt_corpus.py')},
        'indexed': len(cases),
        'original_eligible': sum(case['eligible'] for case in cases),
        'scopes': dict(Counter(case['release_scope'] for case in cases)),
        'classification_rules': classification_rules(),
        'eligibility_unchanged': True,
        'checked_core_evidence': str(evidence_path.resolve())
        if evidence_path is not None else None,
        'prior_reference_evidence': {
            str(path.resolve()): hash_file(path) for path in prior_evidence},
        'selected': len(selected),
        'expected_queries': sum(case['checks'] for case in selected),
        'solver': executable_identity(args.solver, args.solver_args),
        'z3': executable_identity(args.z3, args.z3_args),
        'correctness_reference': (executable_identity(args.reference,
                                                     args.reference_args)
                                 if args.reference else None),
        'filters': {'suites': args.suite, 'tracks': args.track,
                    'families': args.family,
                    'interface_scope': args.interface_scope},
        'timeout_seconds': args.timeout, 'repeats': args.repeats,
        'instant_ms': args.instant_ms, 'ratio': 2,
        'memory_limit_mib': args.memory_limit_mib,
        'memory_observation': ('Linux VmRSS and process-lifetime VmHWM bytes; '
                               'sampled after timing; absent when unavailable'),
        'output_max_mib': args.output_max_mib,
        'timing': 'elapsed input write through completion echo',
        'process_policy': 'persistent processes; untimed startup handshake',
        'query_policy': ('remove only recorded separate-interface commands; '
                         'retain every theory check; one check with replayed '
                         'logical prefix after last reset'),
        'raw_policy': 'complete unchanged script results retained separately',
        'diagnostic_policy': ('bounded stderr capture; every stderr message or '
                              'unsupported response fails query qualification'),
        'operation_policy': ('query plus contiguous requested standard model, '
                             'core, assignment and assertion retrievals; '
                             'all candidate certificates checked; scripts with '
                             'only verified rejections measured as operations'),
        'error_policy': ('expected malformed checks require independently '
                         'validated rejection and recovery and the same '
                         'timing gate; each syntactic check remains counted'),
        'resource_policy': ('disable reproducible limits; '
                            'retain original timeouts'),
        'reference_policy': ('definitive Z3 or independent reference; Z3 full '
                             'query timing retained, timeout is a lower bound'),
        'reference_adaptation': ('Z3 valid :incremental Boolean settings become '
                                 'inert metadata; every changed input recorded'),
        'vendor_configuration': ('project recognized Boolean standard-feature '
                                 'flags into explicit SMT-LIB setup, including '
                                 'after reset; conflicting variants fail'),
    }
    metadata = destination.with_suffix('.run.json')
    completed = set()
    summary = dict(configuration, results=Counter(), query_results=Counter(),
                   operation_results=Counter(),
                   completed=0, remaining=len(selected), queries=0)

    def record_result(record):
        summary['results'][record['result']] += 1
        for query in record.get('queries', []):
            outcome = 'passed' if query['passed'] else 'failed'
            summary['query_results'][outcome] += 1
            if not query['correct']:
                summary['query_results']['answer_unestablished'] += 1
            if query['performance_pass'] is not True:
                summary['query_results']['performance_unestablished'] += 1
            summary['queries'] += 1
            if 'operation' in query:
                operation = query['operation']
                summary['operation_results'][
                    'passed' if operation['passed'] else 'failed'] += 1
        if record.get('rejection_operation') is not None:
            summary['operation_results'][
                'passed' if record['rejection_operation']['passed']
                else 'failed'] += 1
        summary['completed'] += 1
        summary['remaining'] = len(selected) - summary['completed']

    if args.resume:
        if json.loads(metadata.read_text()) != configuration:
            raise ValueError('resume settings or executable hashes changed')
        for record in saved_results(destination, selected):
            record_result(record)
            completed.add(record['path'])
    else:
        write_json(metadata, configuration)
    with destination.open('a' if args.resume else 'w') as stream:
        for case in selected:
            if case['path'] in completed:
                continue
            record = time_case(case, args)
            record_result(record)
            stream.write(json.dumps(record, sort_keys=True) + '\n')
            stream.flush()
            if summary['completed'] % 100 == 0:
                write_json(destination.with_suffix('.summary.json'), summary)
                print(json.dumps({'completed': summary['completed'],
                                  'queries': summary['query_results']}),
                      flush=True)
    for name, executable, arguments in (
            ('solver', args.solver, args.solver_args),
            ('z3', args.z3, args.z3_args),
            ('correctness_reference', args.reference, args.reference_args)):
        if (executable and executable_identity(executable, arguments)
                != configuration[name]):
            raise ValueError('solver executable changed during measurement')
    summary['gate_passed'] = (summary['remaining'] == 0
                             and summary['queries'] > 0
                             and summary['queries'] ==
                             configuration['expected_queries']
                             and not summary['query_results']['failed']
                             and not summary['operation_results']['failed']
                             and all(name in ('passed', 'no_check_call')
                                     for name in summary['results']))
    write_json(destination.with_suffix('.summary.json'), summary)
    print(json.dumps(summary), flush=True)
    return summary

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command",
                        choices=("inventory", "fetch", "index", "run",
                                 "scope", "query-gate"))
    parser.add_argument("--output", type=Path,
                        default=ROOT / "_build/smt-suites")
    parser.add_argument("--family", action="append", choices=FAMILIES)
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--download-jobs", type=int, default=8)
    parser.add_argument("--suite", action="append",
                        choices=("smtlib", "z3", "cvc5"))
    parser.add_argument("--track", action="append", choices=RELEASES)
    parser.add_argument("--results", type=Path)
    parser.add_argument("--case-index", type=Path)
    parser.add_argument("--resume", action="store_true")
    parser.add_argument("--solver")
    parser.add_argument("--checked-core-evidence", type=Path)
    parser.add_argument("--prior-reference-evidence", type=Path,
                        action="append", default=[],
                        help="reconstruct and recheck a previous evidence.json")
    parser.add_argument("--z3", default="z3")
    parser.add_argument("--z3-arg", action="append", dest="z3_args")
    parser.add_argument("--repeats", type=int, default=3)
    parser.add_argument("--instant-ms", type=float, default=10)
    parser.add_argument("--interface-scope", default="theory-and-standard",
                        choices=("all", "theory-and-standard",
                                 "separate-interfaces"))
    parser.add_argument("--reference", default="z3")
    parser.add_argument("--solver-args", nargs="*")
    parser.add_argument("--reference-args", nargs="*")
    parser.add_argument("--solver-arg", action="append", dest="solver_args")
    parser.add_argument("--reference-arg", action="append",
                        dest="reference_args")
    parser.add_argument("--timeout", type=float, default=10)
    parser.add_argument("--memory-limit-mib", type=int, default=0)
    parser.add_argument("--output-max-mib", type=int, default=64)
    parser.add_argument("--certificate-max-mib", type=int, default=64)
    parser.add_argument("--core-semantics", choices=("standard", "z3-mixed"),
                        default="standard")
    args = parser.parse_args()
    if args.z3_args is None:
        args.z3_args = ["-in", "-smt2"]
    if args.solver_args is None:
        args.solver_args = ["-in", "-smt2"]
    if args.reference_args is None:
        args.reference_args = ["-in", "-smt2"]
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=True)
    if args.command == "inventory":
        manifest = inventory(args.output, args.family or FAMILIES)
        print(json.dumps({"sources": len(manifest["sources"]),
                          "smtlib_bytes": manifest["smtlib_download_bytes"],
                          "unavailable": manifest["unavailable"]}))
    elif args.command == "fetch":
        manifest = fetch(args)
        return int(any(source["status"] != "extracted"
                       for source in manifest["sources"]))
    elif args.command == "index":
        summary = index(args)
        return int(bool(summary["unavailable_sources"]))
    elif args.command == "scope":
        scope_report(args)
    elif args.command == "query-gate":
        if not args.solver:
            parser.error("query-gate requires --solver")
        summary = query_gate(args)
        return int(not summary["gate_passed"])
    else:
        if not args.solver:
            parser.error("run requires --solver")
        summary = run(args)
        return int(summary["results"]["passed"] != summary["selected"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
