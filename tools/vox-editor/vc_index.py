#!/usr/bin/env python3
"""Layer 1: compile a vox source file and turn the compiler's verification
output into a JSON-serialisable index of verification conditions (VCs).

The vox compiler exposes two relevant output shapes on stderr:

  * ``-dump-vc -vox-dry-run`` dumps EVERY VC (its source location, goal,
    hypotheses and kind) without invoking the solver.  This is the
    "shape" pass and is fast and deterministic.

  * a real solver run (``-vox-solver-path <lean>``) compiles normally;
    on the first failed proof it raises a single error carrying the
    goal, hypotheses and (often) a counterexample, anchored at the
    failing VC's source location.  Success (exit 0) means every ``Prove``
    VC discharged.

``build_index`` runs the dry-run pass for shapes and, when a solver is
supplied, a real run to attach pass/fail status and any counterexample.

Locations use the compiler's own convention verbatim: ``line`` is
1-based, ``col`` is a 0-based character offset (as OCaml's
``Location.print_loc`` prints).  Consumers that need 0-based lines
(CodeMirror) adjust at the boundary; see selection.js.
"""

import json
import re
import subprocess
from typing import Dict, List, Optional, Tuple

# A location header.  Two spellings occur: toplevel/expect output uses
# "Line 9, characters 26-27:"; file compilation prefixes the file name
# and lowercases the keyword: 'File "a.ml", line 2, characters 20-21:'.
# Multi-line spans use "lines N-M".  Both are accepted here.
_FILE = r'(?:File "[^"]*", )?'
_LOC_SINGLE = re.compile(_FILE + r"[Ll]ine (\d+), characters (\d+)-(\d+):")
_LOC_MULTI = re.compile(_FILE + r"[Ll]ines (\d+)-(\d+), characters (\d+)-(\d+):")

# The tail that marks a dumped VC header: "... vox VC:" or
# "... vox VC (RUNTIME CHECKED):".  A line is a VC header when this tail
# matches AND the line's prefix parses as a location.
_VC_TAIL = re.compile(r": vox VC( \(([A-Z ]+)\))?:$")

# The provenance suffix that -vox-dump-vc-provenance appends to a goal or
# hypothesis: exactly two spaces, "@ ", then "line.col-line.col" (1-based
# line, 0-based col, matching the "Line N, characters A-B" header).  A
# predicate can itself contain '@' (SSA names like x@1), so we split on
# the LAST "  @ " that is followed by the exact coordinate pattern anchored
# at end of string -- never on a bare '@'.  The leading ".*" is greedy, so
# the match lands on the final such suffix.
_SPAN_SUFFIX = re.compile(r"^(.*)  @ (\d+)\.(\d+)-(\d+)\.(\d+)$")

Loc = Dict[str, int]
Range = Tuple[Loc, Loc]
Span = Optional[Dict[str, Loc]]  # {"start": Loc, "end": Loc}, or None


def split_span_suffix(text: str) -> Tuple[str, Span]:
    """Split a dumped predicate into (text_without_suffix, span).

    ``span`` is ``{"start": {line, col}, "end": {line, col}}`` (1-based
    line, 0-based col) when the provenance suffix is present, else None.
    Text without a suffix (a plain -dump-vc predicate, or a hypothesis
    the compiler had no meaningful span for) is returned unchanged with a
    None span, so this is safe to run on either dump flavour."""
    m = _SPAN_SUFFIX.match(text)
    if m is None:
        return text, None
    span: Span = {
        "start": {"line": int(m.group(2)), "col": int(m.group(3))},
        "end": {"line": int(m.group(4)), "col": int(m.group(5))},
    }
    return m.group(1), span


def parse_loc(header: str) -> Optional[Range]:
    """Parse the location prefix of a compiler message line into a
    (start, end) pair of {line, col} dicts, or None if it has none."""
    m = _LOC_SINGLE.match(header)
    if m is not None:
        line = int(m.group(1))
        return (
            {"line": line, "col": int(m.group(2))},
            {"line": line, "col": int(m.group(3))},
        )
    m = _LOC_MULTI.match(header)
    if m is not None:
        return (
            {"line": int(m.group(1)), "col": int(m.group(3))},
            {"line": int(m.group(2)), "col": int(m.group(4))},
        )
    return None


def _kind_from_suffix(suffix: Optional[str]) -> str:
    if suffix == "RUNTIME CHECKED":
        return "runtime_check"
    if suffix == "ASSUMED":
        return "assume"
    return "prove"


def parse_dump(text: str) -> List[Dict[str, object]]:
    """Parse ``-dump-vc`` output into a list of VC dicts.

    Each VC dict has: start, end (locations), goal (str),
    hypotheses (list of str), kind (prove|runtime_check|assume).
    """
    lines = text.split("\n")
    vcs: List[Dict[str, object]] = []
    i = 0
    n = len(lines)
    while i < n:
        header = lines[i]
        tail = _VC_TAIL.search(header)
        rng = parse_loc(header) if tail is not None else None
        if tail is None or rng is None:
            i += 1
            continue
        start, end = rng
        kind = _kind_from_suffix(tail.group(2))
        i += 1
        # goal: everything after "  goal: " until the "  hypotheses:" line.
        # A multi-line goal carries its span suffix on the FIRST line only,
        # so we strip per-line and keep the first span we find.
        goal_parts: List[str] = []
        goal_span: Span = None
        while i < n and not lines[i].lstrip().startswith("hypotheses:"):
            stripped = lines[i].strip()
            if stripped.startswith("goal:"):
                piece = stripped[len("goal:") :].strip()
            elif stripped:
                piece = stripped
            else:
                i += 1
                continue
            piece, span = split_span_suffix(piece)
            if span is not None and goal_span is None:
                goal_span = span
            if piece:
                goal_parts.append(piece)
            i += 1
        goal = " ".join(goal_parts)
        hypotheses: List[str] = []
        # Parallel to ``hypotheses``: each entry is that hypothesis's source
        # span, or None when the compiler synthesized it with no meaningful
        # span (or under plain -dump-vc, where there are no suffixes).
        hyp_spans: List[Span] = []
        if i < n:
            hyp_line = lines[i].strip()
            rest = hyp_line[len("hypotheses:") :].strip()
            i += 1
            if rest and rest != "<none>":
                text, span = split_span_suffix(rest)
                hypotheses.append(text)
                hyp_spans.append(span)
            if not rest:
                # Following indented lines are the hypotheses, until the
                # next VC header or a blank/dedented line.
                while i < n:
                    raw = lines[i]
                    if _VC_TAIL.search(raw) is not None and parse_loc(raw):
                        break
                    if not raw.startswith("  "):
                        break
                    text, span = split_span_suffix(raw.strip())
                    hypotheses.append(text)
                    hyp_spans.append(span)
                    i += 1
        vcs.append(
            {
                "start": start,
                "end": end,
                "goal": goal,
                "goal_span": goal_span,
                "hypotheses": hypotheses,
                "hyp_spans": hyp_spans,
                "kind": kind,
                "status": "unknown",
            }
        )
    return vcs


def parse_error(text: str) -> Optional[Dict[str, object]]:
    """Parse the first vox verification error out of compiler output.

    Returns a dict with location, message, and (for a failed proof)
    goal/hypotheses/counterexample/lean_msg, or None if there is no vox
    error in the text.
    """
    lines = text.split("\n")
    n = len(lines)
    for i in range(n):
        if not lines[i].startswith("Error: vox:"):
            continue
        # Find the nearest preceding location header.
        rng: Optional[Range] = None
        for j in range(i - 1, max(-1, i - 8), -1):
            rng = parse_loc(lines[j])
            if rng is not None:
                break
        message = lines[i][len("Error: ") :].strip()
        result: Dict[str, object] = {"message": message}
        if rng is not None:
            result["start"], result["end"] = rng
        # Scan the remaining lines for the structured verification-failure
        # payload (Goal / Hypotheses / Possible counterexample / lean msg).
        goal: Optional[str] = None
        hyps: List[str] = []
        cex: List[str] = []
        lean_msg: Optional[str] = None
        mode = None
        k = i + 1
        while k < n:
            line = lines[k]
            stripped = line.strip()
            if stripped.startswith("Goal:"):
                goal = stripped[len("Goal:") :].strip()
                mode = None
            elif stripped.startswith("Hypotheses:"):
                rest = stripped[len("Hypotheses:") :].strip()
                if rest and rest != "<none>":
                    hyps.append(rest)
                mode = "hyps"
            elif stripped.startswith("Possible counterexample:"):
                mode = "cex"
            elif stripped.startswith("(lean:"):
                lean_msg = stripped
                mode = None
            elif line.startswith("  ") and mode == "hyps":
                hyps.append(stripped)
            elif line.startswith("  ") and mode == "cex":
                cex.append(stripped)
            elif stripped == "":
                pass
            else:
                # A non-indented, unrecognised line ends the payload only
                # once we have started reading one.
                if goal is not None or hyps or cex:
                    break
            k += 1
        if goal is not None:
            result["goal"] = goal
        if hyps:
            result["hypotheses"] = hyps
        if cex:
            result["counterexample"] = cex
        if lean_msg is not None:
            result["lean_msg"] = lean_msg
        return result
    return None


def compile_capture(
    source_path: str,
    ocamlc: str,
    extra_flags: List[str],
    cwd: Optional[str] = None,
) -> Tuple[int, str]:
    """Run the built ocamlc on a single file, returning (exit, stderr+stdout).

    ``-c`` is always passed so we only type-check/verify (no linking)."""
    cmd = [ocamlc, "-c"] + extra_flags + [source_path]
    proc = subprocess.run(
        cmd,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        universal_newlines=True,
    )
    return proc.returncode, proc.stdout


# The dump flag that adds provenance spans; implies -dump-vc.  Compilers
# that predate it reject it outright, so we probe once and cache the
# verdict, falling back to plain -dump-vc (spans simply absent) thereafter.
_PROVENANCE_FLAG = "-vox-dump-vc-provenance"
_provenance_supported: Optional[bool] = None  # None = not yet probed


def _flag_rejected(output: str, flag: str) -> bool:
    """Did the compiler reject ``flag`` as unknown?  OCaml's arg parser
    prints e.g. ``ocamlc.opt: unknown option '-vox-dump-vc-provenance'.``"""
    return "unknown option" in output and flag in output


def dump_capture(source_path: str, ocamlc: str, cwd: Optional[str]) -> str:
    """Run the VC-shape pass, preferring the provenance flag and caching a
    one-time fallback to plain -dump-vc for compilers that lack it."""
    global _provenance_supported
    if _provenance_supported is not False:
        _, out = compile_capture(
            source_path, ocamlc, [_PROVENANCE_FLAG, "-vox-dry-run"], cwd=cwd
        )
        if _flag_rejected(out, _PROVENANCE_FLAG):
            _provenance_supported = False
        else:
            _provenance_supported = True
            return out
    _, out = compile_capture(source_path, ocamlc, ["-dump-vc", "-vox-dry-run"], cwd=cwd)
    return out


def build_index(
    source_path: str,
    ocamlc: str,
    lean: Optional[str] = None,
    cwd: Optional[str] = None,
) -> Dict[str, object]:
    """Compile ``source_path`` and return a JSON-serialisable index.

    {"vcs": [...], "errors": [...], "ok": bool, "raw_dump": str,
     "raw_solve": str|None}
    """
    dump_out = dump_capture(source_path, ocamlc, cwd=cwd)
    vcs = parse_dump(dump_out)
    errors: List[Dict[str, object]] = []
    ok = True
    raw_solve: Optional[str] = None
    # A dry-run can still surface elaboration errors (bad sorts etc.).
    dry_err = parse_error(dump_out)
    if dry_err is not None:
        errors.append(dry_err)
        ok = False
    if lean is not None:
        code, solve_out = compile_capture(
            source_path, ocamlc, ["-vox-solver-path", lean], cwd=cwd
        )
        raw_solve = solve_out
        err = parse_error(solve_out)
        if code == 0 and err is None:
            for vc in vcs:
                if vc["kind"] == "prove":
                    vc["status"] = "proved"
                elif vc["kind"] == "assume":
                    # Assumed VCs are never sent to the solver -- they are
                    # trusted by construction (borrow/slice framing). Badge
                    # them honestly as "trusted", not the grey "unknown"
                    # that reads as "didn't verify" on a verified file.
                    vc["status"] = "trusted"
        else:
            ok = False
            if err is not None:
                errors.append(err)
                _attach_failure(vcs, err)
    return {
        "vcs": vcs,
        "errors": errors,
        "ok": ok,
        "raw_dump": dump_out,
        "raw_solve": raw_solve,
    }


def _attach_failure(vcs: List[Dict[str, object]], err: Dict[str, object]) -> None:
    """Mark the VC whose location matches the failure as 'failed' and copy
    its counterexample across."""
    if "start" not in err:
        return
    estart = err["start"]
    for vc in vcs:
        if vc["start"] == estart:
            vc["status"] = "failed"
            if "counterexample" in err:
                vc["counterexample"] = err["counterexample"]
            if "lean_msg" in err:
                vc["lean_msg"] = err["lean_msg"]
            return


def main() -> None:
    import argparse

    ap = argparse.ArgumentParser(description="Index vox VCs in a source file.")
    ap.add_argument("source")
    ap.add_argument("--ocamlc", required=True)
    ap.add_argument("--lean", default=None)
    ap.add_argument("--cwd", default=None)
    args = ap.parse_args()
    index = build_index(args.source, args.ocamlc, lean=args.lean, cwd=args.cwd)
    print(json.dumps(index, indent=2))


if __name__ == "__main__":
    main()
