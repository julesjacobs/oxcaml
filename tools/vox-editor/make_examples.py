#!/usr/bin/env python3
"""Curate editor-loadable examples from the vox testsuite.

A hand-written MANIFEST names the curated examples (a slug, a title and
description written by hand, the source test file, and whether the file
is expected to verify or to fail).  For each entry this script:

  * TRANSFORMS the source into a plain single-file program the editor can
    compile: it strips the leading ``(* TEST ... *)`` header and every
    ``[%%expect{|...|}]`` block (an expect-mode file is a sequence of
    phrases each followed by the compiler's expected output; those
    blocks are test scaffolding, not source, and the editor compiles a
    plain file);

  * VALIDATES it through the SAME compile path vc_index uses (the real
    ``ocamlc`` and Lean), and keeps it only when the WHOLE file's outcome
    matches the manifest -- a "verify" entry must elaborate and prove
    every obligation; a "fail" entry must elaborate and then fail
    verification with a counterexample.  Multi-unit clients (files that
    need another unit's ``.cmi``) and files that carry a deliberately
    broken tail therefore drop out here, on their own.

Emits ``examples/<slug>.ml`` and ``examples/index.json`` (an ordered list
of ``{name, title, description, verifies}``).

  python3 make_examples.py              # transform, validate, emit
  python3 make_examples.py --check-only # transform + validate, report only
"""

import argparse
import json
import os
import re
import sys
import tempfile
from typing import Dict, List, Optional, cast

import vc_index  # pyright: ignore[reportImplicitRelativeImport]

HERE = os.path.dirname(os.path.abspath(__file__))
SUITE = os.path.normpath(os.path.join(HERE, "..", "..", "testsuite", "tests", "vox"))
OUT = os.path.join(HERE, "examples")

# The curated set, in pedagogical order.  Titles and descriptions are
# written by hand; the script only transforms and validates the sources.
MANIFEST: List[Dict[str, str]] = [
    {
        "slug": "overview",
        "source": "demo/lean_overview.ml",
        "expect": "verify",
        "title": "Sixty seconds",
        "description": (
            "First contact: a division whose divisor carries a "
            "not (_ = 0) contract, discharged at each call site. The "
            "smallest end-to-end proof."
        ),
    },
    {
        "slug": "reflect",
        "source": "demo/lean_reflect.ml",
        "expect": "verify",
        "title": "The program as its own spec",
        "description": (
            "total_ recursive functions reflect into the logic, so len, "
            "mem and depth denote themselves in refinements -- no "
            "separate spec library, no proof text."
        ),
    },
    {
        "slug": "nth",
        "source": "demo/lean_nth.ml",
        "expect": "verify",
        "title": "A precondition makes a branch dead",
        "description": (
            "Safe list indexing: len and append prove inductively, and "
            "the bound 0 <= i < len l rides nth's parameter as a "
            "contract, turning the Nil arm into a false obligation "
            "rather than an exception."
        ),
    },
    {
        "slug": "tuples",
        "source": "demo/lean_tuples.ml",
        "expect": "verify",
        "title": "Native tuples in refinements",
        "description": (
            "Construction, fst/snd projection, tuple-pattern matching "
            "and structural equality, reasoned through Lean's product "
            "structures."
        ),
    },
    {
        "slug": "quant",
        "source": "demo/lean_quant.ml",
        "expect": "verify",
        "title": "Quantifiers in predicates",
        "description": (
            "forall_, exists_ and native implication in refinements, "
            "with notes on where grind's automation is reliable and "
            "where quantified reasoning belongs in a prelude block."
        ),
    },
    {
        "slug": "fib",
        "source": "demo/lean_fib.ml",
        "expect": "verify",
        "title": "Fibonacci: fast doubling, one file",
        "description": (
            "The naive recursion is reflected and total; the O(log n) "
            "fast-doubling loop is verified against it. The addition and "
            "doubling identities are proved by functional induction in "
            "an embedded [%%vox.lean] block -- put the cursor there and "
            "ask for the live Lean goal."
        ),
    },
    {
        "slug": "reverse",
        "source": "demo/lean_reverse.ml",
        "expect": "verify",
        "title": "In-place array reverse (McCarthy stores)",
        "description": (
            "Mutable-array verification via McCarthy upd/elem stores and "
            "a loop-invariant Prop, with the step lemma proved in the "
            "block. The postcondition is the full reversal permutation."
        ),
    },
    {
        "slug": "mutable",
        "source": "demo/lean_mutable.ml",
        "expect": "verify",
        "title": "Flow-sensitive mutable locals",
        "description": (
            "SSA versioning, conditional joins, let mutable, and for / "
            "while loops with [@vox.invariant], all with reflected "
            "arithmetic through the mutable reads."
        ),
    },
    {
        "slug": "counterexample",
        "source": "mechanics/lean_wrong.ml",
        "expect": "fail",
        "title": "When you're wrong (counterexample)",
        "description": (
            "A deliberately false spec: fib n = n + 1. Verification "
            "fails and the solver hands back a concrete witness -- n = 0, "
            "where fib 0 = 0, not 1. The one example here that does NOT "
            "verify."
        ),
    },
]


# --- transform -------------------------------------------------------------


def strip_test_header(src: str) -> str:
    """Remove a leading ``(* TEST ... *)`` comment block.

    OCaml comments nest, so the close is found by depth rather than by
    the first ``*)``."""
    m = re.search(r"\(\*\s*TEST", src)
    if m is None:
        return src
    i = m.start()
    depth = 0
    j = i
    n = len(src)
    while j < n:
        if src[j : j + 2] == "(*":
            depth += 1
            j += 2
        elif src[j : j + 2] == "*)":
            depth -= 1
            j += 2
            if depth == 0:
                break
        else:
            j += 1
    return src[:i] + src[j:]


_EXPECT = re.compile(r"\[%%expect\s*\{\|.*?\|\}\]", re.DOTALL)


def strip_expect_blocks(src: str) -> str:
    """Remove every ``[%%expect{|...|}]`` block (test scaffolding)."""
    return _EXPECT.sub("", src)


def normalize(src: str) -> str:
    """Drop trailing whitespace, collapse runs of blank lines, and end
    with exactly one newline."""
    src = re.sub(r"[ \t]+\n", "\n", src)
    src = re.sub(r"\n{3,}", "\n\n", src)
    return src.strip("\n") + "\n"


def transform(src: str) -> str:
    return normalize(strip_expect_blocks(strip_test_header(src)))


# --- validation ------------------------------------------------------------


class Validation:
    def __init__(
        self,
        elaborated: bool,
        verifies: bool,
        has_counterexample: bool,
        n_vcs: int,
        detail: str,
    ) -> None:
        self.elaborated = elaborated
        self.verifies = verifies
        self.has_counterexample = has_counterexample
        self.n_vcs = n_vcs
        self.detail = detail


def validate(src: str, ocamlc: str, lean: Optional[str]) -> Validation:
    scratch = tempfile.mkdtemp(prefix="voxmkex")
    path = os.path.join(scratch, "input.ml")
    with open(path, "w") as fh:
        fh.write(src)
    index = vc_index.build_index(path, ocamlc, lean=lean, cwd=scratch)
    raw_dump = str(index.get("raw_dump", ""))
    # An elaboration failure surfaces already in the (solver-free)
    # dry-run pass; a plain verification failure does not.
    elaborated = vc_index.parse_error(raw_dump) is None
    errors = cast(List[Dict[str, object]], index["errors"])
    vcs = cast(List[Dict[str, object]], index["vcs"])
    has_cex = any(("counterexample" in e or "goal" in e) for e in errors)
    first_err = str(errors[0].get("message", "")) if errors else ""
    return Validation(
        elaborated=elaborated,
        verifies=bool(index["ok"]),
        has_counterexample=has_cex,
        n_vcs=len(vcs),
        detail=first_err,
    )


def outcome_matches(expect: str, v: Validation) -> bool:
    if expect == "verify":
        return v.elaborated and v.verifies
    if expect == "fail":
        return v.elaborated and not v.verifies and v.has_counterexample
    raise ValueError("unknown expect: %s" % expect)


# --- driver ----------------------------------------------------------------


def find_ocamlc(cli: Optional[str]) -> str:
    if cli:
        return cli
    env = os.environ.get("VOX_OCAMLC")
    if env and os.path.exists(env):
        return env
    cand = os.path.join(HERE, "..", "..", "_build", "_bootinstall", "bin", "ocamlc.opt")
    cand = os.path.normpath(cand)
    if os.path.exists(cand):
        return cand
    raise SystemExit("no ocamlc found; pass --ocamlc or set VOX_OCAMLC")


def find_lean(cli: Optional[str]) -> str:
    if cli:
        return cli
    env = os.environ.get("VOX_LEAN")
    if env and os.path.exists(env):
        return env
    pinned = "/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean"
    if os.path.exists(pinned):
        return pinned
    raise SystemExit(
        "no lean found; pass --lean or set VOX_LEAN (validation needs the real solver)"
    )


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--ocamlc", default=None)
    ap.add_argument("--lean", default=None)
    ap.add_argument(
        "--check-only",
        action="store_true",
        help="transform and validate, but do not write examples/",
    )
    args = ap.parse_args()
    ocamlc = find_ocamlc(args.ocamlc)
    lean = find_lean(args.lean)

    if not args.check_only:
        os.makedirs(OUT, exist_ok=True)
    kept: List[Dict[str, object]] = []
    dropped: List[str] = []
    for entry in MANIFEST:
        slug = entry["slug"]
        src_path = os.path.join(SUITE, entry["source"])
        if not os.path.exists(src_path):
            dropped.append("%s: source %s missing" % (slug, entry["source"]))
            continue
        with open(src_path) as fh:
            raw = fh.read()
        transformed = transform(raw)
        v = validate(transformed, ocamlc, lean)
        status = (
            "verifies"
            if v.verifies
            else ("fails (cex)" if v.has_counterexample else "fails")
        )
        if not v.elaborated:
            status = "elaboration error: " + v.detail
        print(
            "%-16s %-22s %-13s vcs=%d expect=%s%s"
            % (
                slug,
                entry["source"],
                status,
                v.n_vcs,
                entry["expect"],
                "  OK" if outcome_matches(entry["expect"], v) else "  DROP",
            ),
            flush=True,
        )
        if not outcome_matches(entry["expect"], v):
            dropped.append(
                "%s (%s): expected %s, got %s"
                % (slug, entry["source"], entry["expect"], status)
            )
            continue
        if not args.check_only:
            with open(os.path.join(OUT, slug + ".ml"), "w") as fh:
                fh.write(transformed)
        kept.append(
            {
                "name": slug,
                "title": entry["title"],
                "description": entry["description"],
                "verifies": entry["expect"] == "verify",
            }
        )

    if not args.check_only:
        os.makedirs(OUT, exist_ok=True)
        with open(os.path.join(OUT, "index.json"), "w") as fh:
            json.dump({"examples": kept}, fh, indent=2)
            fh.write("\n")

    print("\nkept %d, dropped %d" % (len(kept), len(dropped)), flush=True)
    for d in dropped:
        print("  drop: " + d, flush=True)
    if not kept:
        sys.exit("no examples kept")


if __name__ == "__main__":
    main()
