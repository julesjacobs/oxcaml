#!/usr/bin/env python3
"""Per-caret attribution audit for the vox2 IDE proof pane.

For every caret position (ch = 0..n on each line) of each example it compares:

  SHOWN    what the pane ACTUALLY shows there, obtained by driving the real tool
           `node tools/voxide-pane.js --vcs-json <fixture> --line L --col C
           --json` at that caret and reading the goal it attributes (the
           obligation's `goalDisplay`, or `·` when the pane is not on an
           obligation).  This is the SAME shared model (pane_model.js) the
           browser renders from, so the audit measures the shipped pane, never a
           re-implementation of it.
  OPTIMAL  the target semantics, computed directly from the raw
           `-vox-dump-vc-json` dump: anchor = location-first skipping ghost,
           membership = [start, end] inclusive, innermost-wins, id tiebreak.

OPTIMAL is judged against the raw dump `location` span (the tight subterm the
obligation constrains) and the source -- never the browser.  SHOWN is judged
against the committed fixture the tool consumes; a consistency guard flags a
fixture whose goals no longer match the dump (regenerate it).  This is the
instrument behind review/ide-pane-audit-catalog.md's granularity section.

Earlier revisions of this file modelled SHOWN with a hardcoded, static rule
(program_point-first, half-open [start, end)) instead of invoking the tool, so
it reported deviations that the C1/B1/tiebreak IDE fixes have since removed.
SHOWN now IS the tool's output, so the count reflects reality.

Usage:
  VOX2_OCAMLC=.../ocamlc.opt TMPDIR=/usr/local/home/jujacobs/tmp \\
    python3 review/caret_attribution_audit.py [example ...]

With no args it audits every example under examples/.  Examples are ASCII, so
the raw dump's byte columns equal caret columns here, and the raw dump's 1-based
line/0-based column map to the tool's --line L (1-based) / --col C (C = col + 1).
"""

import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
EXAMPLES = ROOT / "examples"
FIXTURES = ROOT / "tests" / "fixtures"
TOOL = ROOT / "tools" / "voxide-pane.js"
NODE = os.environ.get("NODE", "node")

# The sentinel a caret with no attributed obligation shows (both sides use it).
NO_PANE = "·"


def ocamlc():
    oc = os.environ.get("VOX2_OCAMLC")
    if not oc:
        sys.exit("set VOX2_OCAMLC to a vox2 ocamlc.opt")
    return oc


def raw_dump(names):
    """Compile `names` together and return the schema-v2 dump conditions."""
    with tempfile.TemporaryDirectory(prefix="caret-") as scratch:
        for name in names:
            (Path(scratch) / name).write_text((EXAMPLES / name).read_text())
        subprocess.run(
            [ocamlc(), "-c", "-vox-dump-vc-json", "vcs.json", *names],
            cwd=scratch,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        doc = json.loads((Path(scratch) / "vcs.json").read_text())
    return doc["verification_conditions"]


def norm(span):
    if not isinstance(span, dict):
        return None
    return {
        "l": span["start"]["line"],
        "c0": span["start"]["column"],
        "l2": span["end"]["line"],
        "c1": span["end"]["column"],
        "ghost": bool(span.get("ghost")),
        "file": span.get("file"),
    }


def vcs_of(conditions, unit=None):
    out = []
    for i, vc in enumerate(conditions):
        loc = norm(vc.get("location"))
        pp = norm(vc.get("program_point"))
        ss = norm((vc.get("goal", {}) or {}).get("source_span"))
        if unit is not None and (loc or pp or {}).get("file") != unit:
            continue
        out.append(
            {
                "id": i,
                "goal": (vc.get("goal", {}) or {}).get("display"),
                "loc": loc,
                "pp": pp,
                "ss": ss,
            }
        )
    return out


def pick(vc, order):
    for k in order:
        s = vc[k]
        if s and not s["ghost"]:
            return s
    return None


def contains(s, line, ch, inclusive):
    if s is None or line < s["l"] or line > s["l2"]:
        return False
    aft = line > s["l"] or (line == s["l"] and ch >= s["c0"])
    if inclusive:
        bef = line < s["l2"] or (line == s["l2"] and ch <= s["c1"])
    else:
        bef = line < s["l2"] or (line == s["l2"] and ch < s["c1"])
    return aft and bef


def size(s):
    return (s["l2"] - s["l"]) * 100000 + (s["c1"] - s["c0"])


def select(vcs, line, ch, order, inclusive):
    hits = sorted(
        (size(s), vc["id"], vc)
        for vc in vcs
        for s in [pick(vc, order)]
        if contains(s, line, ch, inclusive)
    )
    return hits[0][2]["goal"] if hits else NO_PANE


# OPTIMAL: location-first anchor (skip ghost), inclusive [start, end], innermost
# then id.  Kept exactly as the target semantics; SHOWN is measured separately.
OPT = ["loc", "pp", "ss"]


# ---------------------------------------------------------------------------
# SHOWN -- the ACTUAL pane the tool produces, driven per caret.
# ---------------------------------------------------------------------------


def fixture_path(name, unit):
    """The committed fixture the tool consumes for `name` (unit = multi-file)."""
    if unit is not None:
        return FIXTURES / "xmod.workspace.json"
    return FIXTURES / f"{name}.vcs.json"


def tool_goal(fixture, unit, tool_line, tool_col):
    """Drive the real voxide-pane point query and return the goal it shows.

    Returns the obligation's goalDisplay when the pane is on an obligation, else
    NO_PANE (a context / placeholder pane attributes no obligation).
    """
    args = [
        NODE,
        str(TOOL),
        "--vcs-json",
        str(fixture),
        "--line",
        str(tool_line),
        "--col",
        str(tool_col),
        "--json",
        "--no-file",
        "--stdout",
    ]
    if unit is not None:
        args += ["--file", unit]
    res = subprocess.run(
        args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        universal_newlines=True,
    )
    if res.returncode != 0:
        raise SystemExit(
            f"voxide-pane failed at L{tool_line}C{tool_col} "
            f"({fixture.name}{'/' + unit if unit else ''}): {res.stderr.strip()}"
        )
    vm = json.loads(res.stdout)
    if vm.get("mode") == "obligation":
        return (vm.get("obligation") or {}).get("goalDisplay") or NO_PANE
    return NO_PANE


def fixture_goals(fixture, unit):
    """The multiset of goal displays the fixture carries for `unit` (or all)."""
    try:
        doc = json.loads(fixture.read_text())
    except OSError:
        return None
    out = []
    for vc in doc.get("vcs", []) or []:
        if unit is not None and (vc.get("file") or None) != unit:
            continue
        out.append(((vc.get("goal") or {}).get("display")) or "")
    return sorted(out)


def stale_guard(name, vcs, fixture, unit):
    """Warn if the committed fixture's goals diverge from the fresh dump.

    A mismatch means the fixture is stale relative to the compiler under test
    (regenerate it via compiler.py), which would make SHOWN vs OPTIMAL numbers
    meaningless.  Non-fatal -- surfaced loudly so the numbers are trusted only
    when the fixture is fresh.
    """
    fix = fixture_goals(fixture, unit)
    dump = sorted(vc["goal"] or "" for vc in vcs if pick(vc, OPT) is not None)
    if fix is None:
        print(f"   !! {name}: fixture {fixture} missing -- regenerate")
        return False
    if fix != dump:
        print(
            f"   !! {name}: fixture goals differ from dump (STALE fixture?)\n"
            f"      fixture: {fix}\n      dump:    {dump}"
        )
        return False
    return True


def audit(name, vcs, src, fixture, unit):
    lines = src.split("\n")
    interesting = set()
    for vc in vcs:
        for k in ("loc", "pp"):
            s = vc[k]
            if s and not s["ghost"]:
                interesting |= {s["l"], s["l2"]}
    total = dev = 0
    for lineno in sorted(interesting):
        text = lines[lineno - 1] if 0 <= lineno - 1 < len(lines) else ""
        n = len(text)
        printed = False
        for ch in range(n + 1):
            # SHOWN: the tool at the same physical caret.  Raw-dump 1-based line
            # == tool --line; 0-based ch == tool --col - 1 (ASCII examples).
            shown = tool_goal(fixture, unit, lineno, ch + 1)
            optimal = select(vcs, lineno, ch, OPT, True)
            total += 1
            if shown == optimal:
                continue
            dev += 1
            if not printed:
                print(f"L{lineno}: {text!r}")
                printed = True
            oc = f"[{text[ch - 1]}|{text[ch] if ch < n else ''}]" if ch else "l"
            print(
                f"   ch={ch:<3} {oc:10s} shown={shown!s:<28} "
                f"optimal={optimal!s:<28} DEVIATION"
            )
        if printed:
            print()
    return total, dev


def main():
    which = sys.argv[1:]
    singles = [
        "overview",
        "abs",
        "binder",
        "guard",
        "dependent",
        "counterexample",
        "proof_tour",
        "recursion",
        "multi_arg",
        "multi_param",
        "nested_call",
        "predicate_forms",
        "unproved",
    ]
    if which:
        singles = [w for w in singles if w in which]
    grand_t = grand_d = 0
    for name in singles:
        conds = raw_dump([f"{name}.ml"])
        vcs = vcs_of(conds)
        fixture = fixture_path(name, None)
        stale_guard(name, vcs, fixture, None)
        t, d = audit(name, vcs, (EXAMPLES / f"{name}.ml").read_text(), fixture, None)
        print(f"== {name}: {d}/{t} carets deviate ==\n")
        grand_t += t
        grand_d += d
    if not which or "xmod" in which:
        conds = raw_dump(["Lib.ml", "Client.ml"])
        fixture = fixture_path("xmod", "Lib.ml")
        for unit in ("Lib.ml", "Client.ml"):
            vcs = vcs_of(conds, unit)
            stale_guard(f"xmod:{unit}", vcs, fixture, unit)
            t, d = audit(
                f"xmod:{unit}", vcs, (EXAMPLES / unit).read_text(), fixture, unit
            )
            print(f"== xmod {unit}: {d}/{t} carets deviate ==\n")
            grand_t += t
            grand_d += d
    print(f"TOTAL: {grand_d}/{grand_t} carets deviate from optimal attribution")


if __name__ == "__main__":
    main()
