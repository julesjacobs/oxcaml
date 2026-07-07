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
import os
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


# A scope row: "name : OCAML TYPE  ~>  LEAN SORT".  The OCaml type can
# itself contain " : " (labeled arrows), so the name is split on the
# FIRST " : " and the sort on the LAST "  ~>  ".
def parse_scope_line(line: str) -> Optional[Dict[str, object]]:
    line, span = split_span_suffix(line)
    if "  ~>  " not in line or " : " not in line:
        return None
    body, _, sort = line.rpartition("  ~>  ")
    name, _, otype = body.partition(" : ")
    if not name or not sort:
        return None
    return {
        "name": name.strip(),
        "ocaml": otype.strip(),
        "lean": sort.strip(),
        "span": span,
    }


def _parse_used(rest: str) -> List[str]:
    """Parse a dumped "used:" value into a list of lemma names.  The marker
    "<arithmetic>" (grind closed the goal with no user facts) becomes the
    empty list."""
    if not rest or rest == "<arithmetic>":
        return []
    return [n.strip() for n in rest.split(",") if n.strip()]


def _parse_unused_hyps(rest: str) -> List[int]:
    """Parse a dumped "unused_hyps:" value into a list of 0-based indices
    into the VC's (local) ``hypotheses`` list -- the hypotheses grind did
    not reference in the proof it found (``-vox-explain-proofs``).  The
    value is space-separated integers; anything unparseable is dropped."""
    out: List[int] = []
    for tok in rest.split():
        try:
            out.append(int(tok))
        except ValueError:
            pass
    return out


def _join_wrapped(lines: List[str]) -> List[str]:
    """Rejoin predicates the compiler's Format-based dumper WRAPPED across
    physical lines at its margin.

    The dump is a line-oriented protocol: one predicate (goal / hypothesis
    / scope entry) per line, section content indented two spaces.  A long
    predicate, however, is broken by Format at its margin, and the vox
    predicate printer boxes conjunctions/implications so the break lands
    right after a ``&&`` / ``||`` / ``->`` with the continuation at COLUMN
    0 -- which the line-based parsers below would otherwise read as a
    predicate truncated at ``... < n &&`` with the rest bleeding out (the
    reported qsort bug).

    A COMPLETE predicate never ends with a dangling ``&&`` / ``||`` /
    ``->``, so use exactly that as the continuation signal: fold a
    non-indented, non-empty line onto the previous logical line iff that
    line ends with one of those operators.  This leaves every other
    column-0 line alone -- VC/state headers, ``module``/``val``/``sig``
    compiler output, alert underlines -- and is a no-op on an unwrapped
    dump."""
    cont_ops = ("&&", "||", "->")
    out: List[str] = []
    for ln in lines:
        if out and ln and not ln[0].isspace() and out[-1].rstrip().endswith(cont_ops):
            out[-1] = out[-1].rstrip() + " " + ln.strip()
        else:
            out.append(ln)
    return out


def parse_dump(text: str) -> List[Dict[str, object]]:
    """Parse ``-dump-vc`` output into a list of VC dicts.

    Each VC dict has: start, end (locations), goal (str),
    hypotheses (list of str), kind (prove|runtime_check|assume).
    """
    lines = _join_wrapped(text.split("\n"))
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
        # The VC's variables: {name, ocaml, lean} per entry, from the dump's
        # "scope:" section (provenance flag only; empty otherwise).
        scope: List[Dict[str, object]] = []
        # Facts about module-level names only: true but noisy; the pane
        # folds them away.
        module_hypotheses: List[str] = []
        module_hyp_spans: List[Span] = []
        # The lemmas grind used to close this VC (-vox-explain-proofs, under
        # -vox-dump-vc-provenance): None when absent, a list of names, or []
        # for an arithmetic/logic-only proof.
        used: Optional[List[str]] = None
        # This VC's own verdict from a FAILED solve dump (-vox-dump-vc-provenance):
        # "proved" | "unproved" | "disproved" | "failed", or None when the dump
        # carries no verdict (the dry-run pass, or a successful solve).
        verdict: Optional[str] = None
        # The hypotheses grind did not reference to close this VC
        # (-vox-explain-proofs): 0-based indices into ``hypotheses``, or
        # None when the compiler reported nothing (fade nothing).
        unused_hyps: Optional[List[int]] = None
        if i < n:
            hyp_line = lines[i].strip()
            rest = hyp_line[len("hypotheses:") :].strip()
            i += 1
            if rest and rest != "<none>":
                text, span = split_span_suffix(rest)
                hypotheses.append(text)
                hyp_spans.append(span)
            if not rest or rest == "<none>":
                # Following indented lines are the hypotheses (when any),
                # then the "scope:"/"used:" sections, until the next VC
                # header or a dedent.  A "<none>" VC has no hypothesis
                # lines but can still carry a scope and a used line.
                section = "hyps"
                while i < n:
                    raw = lines[i]
                    if _VC_TAIL.search(raw) is not None and parse_loc(raw):
                        break
                    if not raw.startswith("  "):
                        break
                    stripped = raw.strip()
                    if stripped == "scope:":
                        section = "scope"
                        i += 1
                        continue
                    if stripped == "module hypotheses:":
                        section = "mod"
                        i += 1
                        continue
                    if stripped.startswith("used:"):
                        used = _parse_used(stripped[len("used:") :].strip())
                        i += 1
                        continue
                    if stripped.startswith("verdict:"):
                        verdict = stripped[len("verdict:") :].strip() or None
                        i += 1
                        continue
                    if stripped.startswith("unused_hyps:"):
                        unused_hyps = _parse_unused_hyps(
                            stripped[len("unused_hyps:") :].strip()
                        )
                        i += 1
                        continue
                    if section == "scope":
                        entry = parse_scope_line(stripped)
                        if entry is not None:
                            scope.append(entry)
                        i += 1
                        continue
                    text, span = split_span_suffix(stripped)
                    if section == "mod":
                        module_hypotheses.append(text)
                        module_hyp_spans.append(span)
                    else:
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
                "module_hypotheses": module_hypotheses,
                "module_hyp_spans": module_hyp_spans,
                "scope": scope,
                "kind": kind,
                "status": verdict if verdict else "unknown",
                "used": used,
                "verdict": verdict,
                # 0-based indices into ``hypotheses`` grind did not use, or
                # None when unreported.
                "unused_hyps": unused_hyps,
                # Parallel to ``hypotheses``: True where the hypothesis was
                # used in the proof grind found (or unknown -> shown solid),
                # False where the linter flagged it unused (faded).
                "hyp_used": [
                    unused_hyps is None or idx not in unused_hyps
                    for idx in range(len(hypotheses))
                ],
            }
        )
    return vcs


_STATE_TAIL = re.compile(r"vox state:\s*$")


def parse_states(text: str) -> List[Dict[str, object]]:
    """Parse ``-vox-dump-states`` blocks: the fact context + scope at
    each walked expression's entry.  Same hypothesis/scope line formats
    as VCs, no goal."""
    lines = _join_wrapped(text.split("\n"))
    out: List[Dict[str, object]] = []
    i = 0
    n = len(lines)
    while i < n:
        header = lines[i]
        if _STATE_TAIL.search(header) is None:
            i += 1
            continue
        rng = parse_loc(header)
        if rng is None:
            i += 1
            continue
        start, end = rng
        i += 1
        hypotheses: List[str] = []
        hyp_spans: List[Span] = []
        module_hypotheses: List[str] = []
        module_hyp_spans: List[Span] = []
        scope: List[Dict[str, object]] = []
        if i < n and lines[i].lstrip().startswith("hypotheses:"):
            rest = lines[i].strip()[len("hypotheses:") :].strip()
            i += 1
            if rest and rest != "<none>":
                t, sp = split_span_suffix(rest)
                hypotheses.append(t)
                hyp_spans.append(sp)
            # "<none>" is printed inline, but a scope: section can still
            # follow -- a point with no facts still has variables.
            section = "hyps"
            while i < n:
                raw = lines[i]
                if (
                    _VC_TAIL.search(raw) is not None
                    or _STATE_TAIL.search(raw) is not None
                ) and parse_loc(raw):
                    break
                if not raw.startswith("  "):
                    break
                stripped = raw.strip()
                if stripped == "scope:":
                    section = "scope"
                    i += 1
                    continue
                if stripped == "module hypotheses:":
                    section = "mod"
                    i += 1
                    continue
                if section == "scope":
                    entry = parse_scope_line(stripped)
                    if entry is not None:
                        scope.append(entry)
                    i += 1
                    continue
                if section == "hyps" and rest:
                    # inline hypotheses ("<none>" or a single fact):
                    # plain facts cannot follow -- stop unless a section
                    # header switched us.
                    break
                t, sp = split_span_suffix(stripped)
                if section == "mod":
                    module_hypotheses.append(t)
                    module_hyp_spans.append(sp)
                else:
                    hypotheses.append(t)
                    hyp_spans.append(sp)
                i += 1
        out.append(
            {
                "start": start,
                "end": end,
                "hypotheses": hypotheses,
                "hyp_spans": hyp_spans,
                "module_hypotheses": module_hypotheses,
                "module_hyp_spans": module_hyp_spans,
                "scope": scope,
            }
        )
    return out


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
        # A failed proof now carries a VERDICT: DISPROVED (a
        # counterexample was validated by evaluation) vs NOT PROVED
        # (automation gave up -- the property may still hold, and no
        # nonsense witness is shown).
        if "DISPROVED" in message:
            result["verdict"] = "disproved"
        elif "NOT PROVED" in message:
            result["verdict"] = "unproved"
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
            elif stripped.startswith("Possible counterexample:") or stripped.startswith(
                "Counterexample (validated"
            ):
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


def parse_any_error(text: str) -> Optional[Dict[str, object]]:
    """Like ``parse_error`` but falls back to the first PLAIN compiler
    error (``Error: Syntax error``, type errors, ...), so the dry-run
    pass surfaces every compilation failure, not just vox ones.  The
    plain fallback takes the Error line plus its indented continuation
    lines as the message."""
    err = parse_error(text)
    if err is not None:
        return err
    lines = text.split("\n")
    for i, line in enumerate(lines):
        if not line.startswith("Error:"):
            continue
        rng: Optional[Range] = None
        for j in range(i - 1, max(-1, i - 8), -1):
            rng = parse_loc(lines[j])
            if rng is not None:
                break
        msg = [line[len("Error:") :].strip()]
        for k in range(i + 1, len(lines)):
            if lines[k].startswith((" ", "\t")) and lines[k].strip():
                msg.append(lines[k].strip())
            else:
                break
        result: Dict[str, object] = {"message": " ".join(msg)}
        if rng is not None:
            result["start"], result["end"] = rng
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
_STATES_FLAG = "-vox-dump-states"
# The solve pass adds these two to report the lemmas grind used per VC
# ("used:" lines in the provenance dump).  Probe once; compilers that
# predate the flag fall back to a plain solve (no used-list).
_EXPLAIN_FLAG = "-vox-explain-proofs"
_states_supported: Optional[bool] = None
_provenance_supported: Optional[bool] = None  # None = not yet probed
_explain_supported: Optional[bool] = None


def _flag_rejected(output: str, flag: str) -> bool:
    """Did the compiler reject ``flag`` as unknown?  OCaml's arg parser
    prints e.g. ``ocamlc.opt: unknown option '-vox-dump-vc-provenance'.``"""
    return "unknown option" in output and flag in output


def dump_capture(source_path: str, ocamlc: str, cwd: Optional[str]) -> Tuple[int, str]:
    """Run the VC-shape pass, preferring the provenance flag and caching a
    one-time fallback to plain -dump-vc for compilers that lack it.
    Returns (exit code, output)."""
    global _provenance_supported, _states_supported
    if _provenance_supported is not False:
        flags = [_PROVENANCE_FLAG]
        if _states_supported is not False:
            flags.append(_STATES_FLAG)
        code, out = compile_capture(
            source_path, ocamlc, flags + ["-vox-dry-run", "-annot"], cwd=cwd
        )
        if _flag_rejected(out, _STATES_FLAG):
            # Older compiler: retry once without states, cache the verdict.
            _states_supported = False
            code, out = compile_capture(
                source_path,
                ocamlc,
                [_PROVENANCE_FLAG, "-vox-dry-run", "-annot"],
                cwd=cwd,
            )
        elif _states_supported is None:
            _states_supported = True
        if _flag_rejected(out, _PROVENANCE_FLAG):
            _provenance_supported = False
        else:
            _provenance_supported = True
            return code, out
    return compile_capture(source_path, ocamlc, ["-dump-vc", "-vox-dry-run"], cwd=cwd)


def solve_capture(
    source_path: str, ocamlc: str, lean: str, cwd: Optional[str]
) -> Tuple[int, str]:
    """Run the real solver pass.  Under a compiler that supports it, also
    request the provenance dump with per-VC "used:" lines
    (-vox-explain-proofs); the used-lists ride along in this pass's output
    (parsed by build_index).  Probe once and cache a fallback to a plain
    solve for older compilers.  Returns (exit code, output)."""
    global _explain_supported
    base = ["-vox-solver-path", lean]
    if _explain_supported is not False:
        flags = base + [_PROVENANCE_FLAG, _EXPLAIN_FLAG]
        code, out = compile_capture(source_path, ocamlc, flags, cwd=cwd)
        if _flag_rejected(out, _EXPLAIN_FLAG) or _flag_rejected(out, _PROVENANCE_FLAG):
            _explain_supported = False
            return compile_capture(source_path, ocamlc, base, cwd=cwd)
        _explain_supported = True
        return code, out
    return compile_capture(source_path, ocamlc, base, cwd=cwd)


# .annot blocks: a location line ("file" lnum bol cnum, twice) followed
# by one or more kind( ... ) payloads; we keep the type( ... ) ones.
# Columns are cnum - bol (0-based); lines are 1-based.
_ANNOT_LOC = re.compile(r'^"[^"]*" (\d+) (\d+) (\d+) "[^"]*" (\d+) (\d+) (\d+)\s*$')


def parse_annot(text: str) -> List[Dict[str, object]]:
    """Parse ``-annot`` output into [{start, end, type}] (1-based lines,
    0-based cols -- the same convention as provenance spans)."""
    out: List[Dict[str, object]] = []
    lines = text.split("\n")
    i = 0
    n = len(lines)
    loc: Optional[Tuple[Dict[str, int], Dict[str, int]]] = None
    while i < n:
        m = _ANNOT_LOC.match(lines[i])
        if m is not None:
            l1, b1, c1, l2, b2, c2 = (int(g) for g in m.groups())
            loc = (
                {"line": l1, "col": c1 - b1},
                {"line": l2, "col": c2 - b2},
            )
            i += 1
            continue
        if lines[i].startswith("type(") and loc is not None:
            body: List[str] = []
            i += 1
            while i < n and lines[i] != ")":
                body.append(lines[i].strip())
                i += 1
            out.append({"start": loc[0], "end": loc[1], "type": " ".join(body)})
        i += 1
    return out


def read_annot(source_path: str) -> List[Dict[str, object]]:
    annot = os.path.splitext(source_path)[0] + ".annot"
    try:
        with open(annot, "r") as fh:
            return parse_annot(fh.read())
    except OSError:
        return []


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
    dump_code, dump_out = dump_capture(source_path, ocamlc, cwd=cwd)
    vcs = parse_dump(dump_out)
    states = parse_states(dump_out)
    errors: List[Dict[str, object]] = []
    ok = True
    raw_solve: Optional[str] = None
    # The dry-run surfaces every compilation failure: vox elaboration
    # errors AND plain OCaml ones (syntax, typing) -- the fast editor
    # pass has no solve step to catch them later.
    dry_err = parse_any_error(dump_out)
    if dry_err is not None:
        errors.append(dry_err)
        ok = False
    elif dump_code != 0:
        errors.append({"message": "compilation failed (see raw dump)"})
        ok = False
    if lean is not None:
        code, solve_out = solve_capture(source_path, ocamlc, lean, cwd=cwd)
        raw_solve = solve_out
        err = parse_error(solve_out)
        if code == 0 and err is None:
            # The solve pass emits its own provenance dump (with "used:"
            # lines) in the SAME VC order as the dry-run, so attach each
            # used-list to the matching VC by position.
            solve_vcs = parse_dump(solve_out)
            if len(solve_vcs) == len(vcs):
                for vc, svc in zip(vcs, solve_vcs):
                    if svc.get("used") is not None:
                        vc["used"] = svc["used"]
                    # The unused-hypothesis report also rides the solve
                    # pass (the dry-run has no proof term); carry it and the
                    # parallel used-flag across from the solve VC.
                    if svc.get("unused_hyps") is not None:
                        vc["unused_hyps"] = svc["unused_hyps"]
                        vc["hyp_used"] = svc["hyp_used"]
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
            # On a failure the compiler still dumps a per-VC verdict block
            # (under -vox-dump-vc-provenance) BEFORE it raises, in the same VC
            # order as the dry-run: a Prove VC whose theorem carried no Lean
            # error is "proved", the failing ones "unproved"/"disproved".  Copy
            # those verdicts across so still-holding obligations aren't left
            # grey when a sibling fails.  Assumed VCs are trusted regardless.
            solve_vcs = parse_dump(solve_out)
            if len(solve_vcs) == len(vcs):
                for vc, svc in zip(vcs, solve_vcs):
                    if svc.get("verdict") is not None:
                        vc["status"] = svc["status"]
            for vc in vcs:
                if vc["kind"] == "assume" and vc["status"] == "unknown":
                    vc["status"] = "trusted"
            if err is not None:
                errors.append(err)
                # The primary error also carries the validated counterexample
                # and lean message; attach those to its VC (status already set).
                _attach_failure(vcs, err)
    return {
        "vcs": vcs,
        "states": states,
        "errors": errors,
        "ok": ok,
        "types": read_annot(source_path),
        "raw_dump": dump_out,
        "raw_solve": raw_solve,
    }


def _attach_failure(vcs: List[Dict[str, object]], err: Dict[str, object]) -> None:
    """Mark the VC whose location matches the failure with its verdict
    ('disproved' or 'unproved', falling back to 'failed') and copy its
    counterexample across."""
    if "start" not in err:
        return
    estart = err["start"]
    for vc in vcs:
        if vc["start"] == estart:
            vc["status"] = err.get("verdict", "failed")
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
