#!/usr/bin/env python3
"""vox-stats -- a proof-size and trust-surface dashboard for the vox
testsuite (testsuite/tests/vox).

Numbers, not adjectives.  For every .ml/.mli it reports, per file and in
total:

  Lean       lines of [%%vox.lean {lean| ... |lean}] block text (the
             proof/model burden carried in Lean)
  Thm        theorems + lemmas declared in those blocks (proved facts)
  Axiom      axioms declared in those blocks (in an .mli an axiom is an
             OBLIGATION discharged by the impl's seal; in an .ml it joins
             the trusted base -- both are counted, .ml axioms flagged in
             the TRUST total)
  refine_    refine_ unpack sites
  a_unchk    assume_unchecked_ occurrences (the raw trust surface: facts
             asserted to the solver without proof)
  assume_    plain assume_ occurrences (in-code proof hints)
  sort       [@@vox.sort ...] ghost-sort declarations

The TRUST SURFACE of the corpus is (assume_unchecked_) + (axioms in .ml
blocks): everything else in the Lean column is proved or is an interface
obligation.

Optionally (--vc, needs a built compiler + Lean) it adds a VC column by
compiling each file with -dump-vc and counting `vox VC:` obligations;
files whose module dependencies are not already built are left blank.

Usage:
  tools/vox-stats.py [--root DIR] [--vc] [--compiler PATH] [--lean PATH]

Plain python3, stdlib only.
"""

import argparse
import os
import re
import subprocess

BLOCK_RE = re.compile(r"\{lean\|(.*?)\|lean\}", re.DOTALL)
THM_RE = re.compile(r"\b(?:theorem|lemma)\b")
AXIOM_RE = re.compile(r"\baxiom\b")
AUNCHK_RE = re.compile(r"assume_unchecked_")
ASSUME_RE = re.compile(r"assume_(?!unchecked_)")
SORT_RE = re.compile(r"@@vox\.sort")
REFINE_RE = re.compile(r"\brefine_\b")


def block_line_count(block):
    body = block.strip("\n")
    if not body.strip():
        return 0
    return len(body.splitlines())


def analyze(path):
    with open(path, encoding="utf-8", errors="replace") as f:
        text = f.read()
    blocks = BLOCK_RE.findall(text)
    block_text = "\n".join(blocks)
    ocaml = BLOCK_RE.sub("", text)  # code with Lean blocks removed
    is_mli = path.endswith(".mli")
    axioms = len(AXIOM_RE.findall(block_text))
    return {
        "lean": sum(block_line_count(b) for b in blocks),
        "thm": len(THM_RE.findall(block_text)),
        "axiom": axioms,
        # .ml axioms are trusted; .mli axioms are obligations
        "axiom_ml": 0 if is_mli else axioms,
        "refine": len(REFINE_RE.findall(ocaml)),
        "aunchk": len(AUNCHK_RE.findall(ocaml)),
        "assume": len(ASSUME_RE.findall(ocaml)),
        "sort": len(SORT_RE.findall(ocaml)),
    }


def count_vcs(path, compiler, lean):
    """Best-effort VC count via -dump-vc; blank on any failure/dep miss."""
    d = os.path.dirname(path)
    try:
        proc = subprocess.run(
            [compiler, "-vox-solver-path", lean, "-dump-vc", "-I", d, "-c", path],
            capture_output=True,
            text=True,
            timeout=120,
            cwd=d,
        )
    except Exception:
        return -1
    out = proc.stdout + proc.stderr
    if "vox VC:" not in out:
        return -1
    return out.count("vox VC:")


def has_content(s):
    return any(s[k] for k in ("lean", "refine", "aunchk", "assume", "sort", "axiom"))


def fmt_table(rows, with_vc):
    cols = ["File", "Lean", "Thm", "Axiom", "refine_", "a_unchk", "assume_", "sort"]
    if with_vc:
        cols.insert(1, "VC")
    lines = ["| " + " | ".join(cols) + " |", "|" + "|".join(["---"] * len(cols)) + "|"]
    for name, s in rows:
        cells = [name]
        if with_vc:
            cells.append("" if s.get("vc", -1) < 0 else str(s["vc"]))
        cells += [
            str(s[k])
            for k in ("lean", "thm", "axiom", "refine", "aunchk", "assume", "sort")
        ]
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    ap.add_argument(
        "--root",
        default="testsuite/tests/vox",
        help="directory to walk (default testsuite/tests/vox)",
    )
    ap.add_argument(
        "--vc",
        action="store_true",
        help="add a VC column via -dump-vc (needs compiler+lean)",
    )
    ap.add_argument("--compiler", default="_build/_bootinstall/bin/ocamlc.opt")
    ap.add_argument("--lean", default=os.environ.get("VOX_LEAN", "lean"))
    args = ap.parse_args()

    files = []
    for dirpath, _, names in os.walk(args.root):
        for n in sorted(names):
            if n.endswith((".ml", ".mli")):
                files.append(os.path.join(dirpath, n))
    files.sort()

    per_dir = {}
    rows = []
    for path in files:
        s = analyze(path)
        if not has_content(s):
            continue
        if args.vc:
            s["vc"] = count_vcs(path, args.compiler, args.lean)
        rel = os.path.relpath(path, args.root)
        rows.append((rel, s))
        d = os.path.dirname(rel) or "."
        agg = per_dir.setdefault(
            d,
            dict.fromkeys(
                (
                    "lean",
                    "thm",
                    "axiom",
                    "axiom_ml",
                    "refine",
                    "aunchk",
                    "assume",
                    "sort",
                    "vc",
                ),
                0,
            ),
        )
        for k in (
            "lean",
            "thm",
            "axiom",
            "axiom_ml",
            "refine",
            "aunchk",
            "assume",
            "sort",
        ):
            agg[k] += s[k]
        if args.vc and s.get("vc", -1) > 0:
            agg["vc"] += s["vc"]

    total = dict.fromkeys(
        (
            "lean",
            "thm",
            "axiom",
            "axiom_ml",
            "refine",
            "aunchk",
            "assume",
            "sort",
            "vc",
        ),
        0,
    )
    for _, s in rows:
        for k in (
            "lean",
            "thm",
            "axiom",
            "axiom_ml",
            "refine",
            "aunchk",
            "assume",
            "sort",
        ):
            total[k] += s[k]
        if args.vc and s.get("vc", -1) > 0:
            total["vc"] += s["vc"]

    print("# vox proof-size and trust-surface stats\n")
    print(f"Root: `{args.root}`  |  files with vox content: {len(rows)}\n")
    print("## Totals\n")
    vc_line = f"- VC obligations (dumped): **{total['vc']}**\n" if args.vc else ""
    print(
        f"- Lean block lines: **{total['lean']}**\n"
        f"- Theorems/lemmas proved: **{total['thm']}**\n"
        f"- Axioms in blocks: **{total['axiom']}** "
        f"(of which in .ml, i.e. trusted: **{total['axiom_ml']}**; "
        f"the rest are .mli obligations)\n"
        f"{vc_line}"
        f"- refine_ unpack sites: **{total['refine']}**\n"
        f"- assume_unchecked_ (raw trust surface): **{total['aunchk']}**\n"
        f"- assume_ (proof hints): **{total['assume']}**\n"
        f"- [@@vox.sort] ghost sorts: **{total['sort']}**\n"
    )
    print(
        f"**Trust surface** = assume_unchecked_ + .ml-block axioms = "
        f"**{total['aunchk'] + total['axiom_ml']}**.\n"
    )

    print("## Per directory\n")
    dcols = ["Dir", "Lean", "Thm", "Axiom", "refine_", "a_unchk", "assume_", "sort"]
    print("| " + " | ".join(dcols) + " |")
    print("|" + "|".join(["---"] * len(dcols)) + "|")
    for d in sorted(per_dir):
        a = per_dir[d]
        print(
            f"| {d} | {a['lean']} | {a['thm']} | {a['axiom']} | "
            f"{a['refine']} | {a['aunchk']} | {a['assume']} | {a['sort']} |"
        )

    print("\n## Per file\n")
    print(fmt_table(rows, args.vc))


if __name__ == "__main__":
    main()
