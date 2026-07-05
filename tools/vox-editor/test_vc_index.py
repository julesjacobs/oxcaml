#!/usr/bin/env python3
"""Tests for vc_index.

The parser tests use output copied BYTE-FOR-BYTE from the vox mechanics
suite expect blocks (testsuite/tests/vox/mechanics/refines_kind.ml and
lean_refines_fact.ml), so they pin the parser against real compiler
output without needing a built compiler.

The end-to-end tests run only when a built ocamlc is discoverable (env
VOX_OCAMLC, else the sibling clone's _build path); they are skipped
otherwise.
"""

import os
import unittest
from typing import Any, Dict, List, cast

import vc_index  # pyright: ignore[reportImplicitRelativeImport]

# --- byte-exact fixtures from mechanics/refines_kind.ml -------------------

DUMP_TWO_NONE = """\
Line 9, characters 26-27: vox VC:
  goal: 0 = 0
  hypotheses: <none>
Line 10, characters 50-55: vox VC:
  goal: (x + 1) = (x + 1)
  hypotheses: <none>
module M :
  sig type t val zero : t{ _ = 0 } val next : (x : t) -> t{ _ = (x + 1) } end
"""

DUMP_MULTI_HYP = """\
Line 4, characters 20-21: vox VC:
  goal: o = 1
  hypotheses:
  o = (z + 1)
  z = M.zero
  M.zero = 0
val one : unit -> M.t = <fun>
"""

DUMP_BYPATH = """\
Line 8, characters 20-21: vox VC:
  goal: 0 >= 0
  hypotheses:
  bound = 5
module ByPath : sig type nat2 val zero : nat2 end
"""

# --- byte-exact fixture from mechanics/lean_refines_fact.ml ---------------

ERROR_FAIL = """\
Line 3, characters 19-20:
3 |   let refine_ r = (n : M.nat{ _ >= 1 }) in
                       ^
Error: vox: verification failed (lean).
       Goal: n >= 1
Hypotheses:
  n >= 0
Possible counterexample:
  n = 0
(lean: error: `grind` failed)
"""


class TestParseLoc(unittest.TestCase):
    def test_single(self):
        rng = vc_index.parse_loc("Line 9, characters 26-27: vox VC:")
        assert rng is not None
        start, end = rng
        self.assertEqual(start, {"line": 9, "col": 26})
        self.assertEqual(end, {"line": 9, "col": 27})

    def test_multi(self):
        rng = vc_index.parse_loc("Lines 3-5, characters 6-3:")
        assert rng is not None
        start, end = rng
        self.assertEqual(start, {"line": 3, "col": 6})
        self.assertEqual(end, {"line": 5, "col": 3})

    def test_none(self):
        self.assertIsNone(vc_index.parse_loc("Error: vox: nope"))


class TestParseDump(unittest.TestCase):
    def test_two_none(self):
        vcs = vc_index.parse_dump(DUMP_TWO_NONE)
        self.assertEqual(len(vcs), 2)
        self.assertEqual(
            vcs[0],
            {
                "start": {"line": 9, "col": 26},
                "end": {"line": 9, "col": 27},
                "goal": "0 = 0",
                "hypotheses": [],
                "kind": "prove",
                "status": "unknown",
            },
        )
        self.assertEqual(vcs[1]["goal"], "(x + 1) = (x + 1)")
        self.assertEqual(vcs[1]["hypotheses"], [])
        # The trailing "module M : ... sig ..." lines must not leak in.
        self.assertEqual(len(vcs), 2)

    def test_multi_hyp(self):
        vcs = vc_index.parse_dump(DUMP_MULTI_HYP)
        self.assertEqual(len(vcs), 1)
        self.assertEqual(vcs[0]["goal"], "o = 1")
        self.assertEqual(
            vcs[0]["hypotheses"], ["o = (z + 1)", "z = M.zero", "M.zero = 0"]
        )

    def test_bypath(self):
        vcs = vc_index.parse_dump(DUMP_BYPATH)
        self.assertEqual(len(vcs), 1)
        self.assertEqual(vcs[0]["hypotheses"], ["bound = 5"])

    def test_kind_suffix(self):
        text = (
            "Line 1, characters 0-1: vox VC (RUNTIME CHECKED):\n"
            "  goal: x = 1\n  hypotheses: <none>\n"
        )
        vcs = vc_index.parse_dump(text)
        self.assertEqual(vcs[0]["kind"], "runtime_check")
        text2 = (
            "Line 1, characters 0-1: vox VC (ASSUMED):\n"
            "  goal: x = 1\n  hypotheses: <none>\n"
        )
        self.assertEqual(vc_index.parse_dump(text2)[0]["kind"], "assume")

    def test_concatenated(self):
        # Several VC dumps back to back parse to the sum.
        vcs = vc_index.parse_dump(DUMP_TWO_NONE + DUMP_MULTI_HYP + DUMP_BYPATH)
        self.assertEqual(len(vcs), 4)


class TestParseError(unittest.TestCase):
    def test_fail(self):
        err = vc_index.parse_error(ERROR_FAIL)
        assert err is not None
        self.assertEqual(err["start"], {"line": 3, "col": 19})
        self.assertEqual(err["end"], {"line": 3, "col": 20})
        self.assertEqual(err["goal"], "n >= 1")
        self.assertEqual(err["hypotheses"], ["n >= 0"])
        self.assertEqual(err["counterexample"], ["n = 0"])
        self.assertTrue(str(err["lean_msg"]).startswith("(lean:"))

    def test_no_error(self):
        self.assertIsNone(vc_index.parse_error(DUMP_TWO_NONE))

    def test_attach_failure(self):
        vcs = vc_index.parse_dump(
            "Line 3, characters 19-20: vox VC:\n"
            "  goal: n >= 1\n  hypotheses:\n  n >= 0\n"
        )
        err = vc_index.parse_error(ERROR_FAIL)
        assert err is not None
        vc_index._attach_failure(vcs, err)
        self.assertEqual(vcs[0]["status"], "failed")
        self.assertEqual(vcs[0]["counterexample"], ["n = 0"])


# --- end-to-end (skipped unless a built compiler is available) ------------


def _find_ocamlc():
    env = os.environ.get("VOX_OCAMLC")
    if env and os.path.exists(env):
        return env
    here = os.path.dirname(os.path.abspath(__file__))
    root = os.path.dirname(os.path.dirname(here))  # tools/vox-editor -> repo
    cand = os.path.join(root, "_build", "_bootinstall", "bin", "ocamlc.opt")
    if os.path.exists(cand):
        return cand
    return None


def _find_lean():
    env = os.environ.get("VOX_LEAN")
    if env and os.path.exists(env):
        return env
    pinned = "/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean"
    return pinned if os.path.exists(pinned) else None


OCAMLC = _find_ocamlc()
LEAN = _find_lean()

FIXTURE_OK = """\
let f (x : int{ _ >= 0 }) =
  let refine_ ok = (x : int{ _ + 1 >= 1 }) in
  ok
"""

FIXTURE_FAIL = """\
let f (x : int{ _ >= 0 }) =
  let refine_ ok = (x : int{ _ >= 1 }) in
  ok
"""


@unittest.skipUnless(OCAMLC, "no built ocamlc found (set VOX_OCAMLC)")
class TestEndToEnd(unittest.TestCase):
    def _write(self, name, text):
        import tempfile

        d = tempfile.mkdtemp(prefix="voxvc")
        p = os.path.join(d, name)
        with open(p, "w") as fh:
            fh.write(text)
        return d, p

    def test_dump_shapes(self):
        assert OCAMLC is not None
        d, p = self._write("ok.ml", FIXTURE_OK)
        index = vc_index.build_index(p, OCAMLC, cwd=d)
        vcs = cast(List[Dict[str, Any]], index["vcs"])
        self.assertTrue(len(vcs) >= 1)
        vc = vcs[0]
        self.assertIn("goal", vc)
        self.assertIn("hypotheses", vc)

    @unittest.skipUnless(LEAN, "no lean found (set VOX_LEAN)")
    def test_solve_ok(self):
        assert OCAMLC is not None
        d, p = self._write("ok.ml", FIXTURE_OK)
        index = vc_index.build_index(p, OCAMLC, lean=LEAN, cwd=d)
        vcs = cast(List[Dict[str, Any]], index["vcs"])
        self.assertTrue(index["ok"], msg=index.get("raw_solve"))
        self.assertTrue(any(vc["status"] == "proved" for vc in vcs))

    @unittest.skipUnless(LEAN, "no lean found (set VOX_LEAN)")
    def test_solve_fail(self):
        assert OCAMLC is not None
        d, p = self._write("bad.ml", FIXTURE_FAIL)
        index = vc_index.build_index(p, OCAMLC, lean=LEAN, cwd=d)
        errors = cast(List[Dict[str, Any]], index["errors"])
        self.assertFalse(index["ok"])
        self.assertTrue(len(errors) >= 1)
        err = errors[-1]
        self.assertIn("counterexample", err)

    @unittest.skipUnless(LEAN, "no lean found (set VOX_LEAN)")
    def test_assumed_vcs_trusted(self):
        # The reverse example verifies fully; its borrow/slice framing VCs
        # are ASSUMED (never sent to the solver) and must badge as
        # "trusted", not the grey "unknown" that reads as "didn't verify"
        # on a fully verified file.
        assert OCAMLC is not None
        example = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "examples", "reverse.ml"
        )
        with open(example) as fh:
            d, p = self._write("reverse.ml", fh.read())
        index = vc_index.build_index(p, OCAMLC, lean=LEAN, cwd=d)
        self.assertTrue(index["ok"], msg=index.get("raw_solve"))
        vcs = cast(List[Dict[str, Any]], index["vcs"])
        assumed = [v for v in vcs if v["kind"] == "assume"]
        self.assertTrue(assumed, "expected at least one ASSUMED VC")
        self.assertTrue(all(v["status"] == "trusted" for v in assumed))
        # Nothing is left grey (unknown) on a verified file.
        self.assertFalse(any(v["status"] == "unknown" for v in vcs))


if __name__ == "__main__":
    unittest.main()
