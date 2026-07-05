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

# --- byte-exact fixtures from mechanics/provenance.ml (spans present) -----

# A refined parameter: goal + one spanned hypothesis.
DUMP_PROV_PARAM = """\
Line 1, characters 58-59: vox VC:
  goal: x > 0  @ 1.58-1.59
  hypotheses:
  x > 0  @ 1.15-1.16
val use_param : int{ _ > 0 } -> int{ _ > 0 } = <fun>
"""

# A loop back-edge VC: the goal and three hypotheses carry spans, one of
# which (x@1 >= 0) is an @-CONTAINING predicate with a trailing span; the
# synthesized fresh-version equation (x@2 = (x@1 + 1)) carries NO span.
DUMP_PROV_LOOP = """\
Line 5, characters 9-32: vox VC:
  goal: x@2 >= 0  @ 5.9-5.32
  hypotheses:
  1 <= i  @ 3.2-5.8
  i <= n  @ 3.2-5.8
  x@1 >= 0  @ 5.9-5.32
  x@2 = (x@1 + 1)
val loopy : int -> int{ _ >= 0 } = <fun>
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
                "goal_span": None,
                "hypotheses": [],
                "hyp_spans": [],
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

    def test_no_suffix_spans_are_none(self):
        # Plain -dump-vc output (no suffixes, e.g. an old compiler via the
        # fallback path): the schema keys exist but every span is None.
        vcs = vc_index.parse_dump(DUMP_MULTI_HYP)
        self.assertIsNone(vcs[0]["goal_span"])
        self.assertEqual(vcs[0]["hyp_spans"], [None, None, None])

    def test_prov_goal_and_hyp_span(self):
        vcs = vc_index.parse_dump(DUMP_PROV_PARAM)
        self.assertEqual(len(vcs), 1)
        # Text is stripped of the suffix; the span is captured separately.
        self.assertEqual(vcs[0]["goal"], "x > 0")
        self.assertEqual(
            vcs[0]["goal_span"],
            {"start": {"line": 1, "col": 58}, "end": {"line": 1, "col": 59}},
        )
        self.assertEqual(vcs[0]["hypotheses"], ["x > 0"])
        self.assertEqual(
            vcs[0]["hyp_spans"],
            [{"start": {"line": 1, "col": 15}, "end": {"line": 1, "col": 16}}],
        )

    def test_prov_at_predicate_and_spanless_hyp(self):
        # The loop VC mixes an @-containing spanned hypothesis with a
        # span-less synthesized one; parsing must keep the '@' in the text,
        # attach the trailing span, and leave the synthesized hyp span None.
        vcs = vc_index.parse_dump(DUMP_PROV_LOOP)
        self.assertEqual(len(vcs), 1)
        self.assertEqual(vcs[0]["goal"], "x@2 >= 0")
        self.assertEqual(
            vcs[0]["goal_span"],
            {"start": {"line": 5, "col": 9}, "end": {"line": 5, "col": 32}},
        )
        self.assertEqual(
            vcs[0]["hypotheses"],
            ["1 <= i", "i <= n", "x@1 >= 0", "x@2 = (x@1 + 1)"],
        )
        spans = vcs[0]["hyp_spans"]
        assert isinstance(spans, list)
        self.assertEqual(
            spans[0], {"start": {"line": 3, "col": 2}, "end": {"line": 5, "col": 8}}
        )
        self.assertEqual(
            spans[2], {"start": {"line": 5, "col": 9}, "end": {"line": 5, "col": 32}}
        )
        # The synthesized fresh-version equation has no source span.
        self.assertIsNone(spans[3])


class TestSplitSpanSuffix(unittest.TestCase):
    def test_with_suffix(self):
        text, span = vc_index.split_span_suffix("x > 0  @ 1.58-1.59")
        self.assertEqual(text, "x > 0")
        self.assertEqual(
            span, {"start": {"line": 1, "col": 58}, "end": {"line": 1, "col": 59}}
        )

    def test_without_suffix(self):
        text, span = vc_index.split_span_suffix("x > 0")
        self.assertEqual(text, "x > 0")
        self.assertIsNone(span)

    def test_at_in_predicate_with_suffix(self):
        # An SSA name (x@1) must survive: split only on the trailing "  @ L.C"
        # coordinate suffix, never on the bare '@' inside the predicate.
        text, span = vc_index.split_span_suffix("x@1 = x + 1  @ 1.15-1.16")
        self.assertEqual(text, "x@1 = x + 1")
        self.assertEqual(
            span, {"start": {"line": 1, "col": 15}, "end": {"line": 1, "col": 16}}
        )

    def test_at_in_predicate_without_suffix(self):
        text, span = vc_index.split_span_suffix("x@2 = (x@1 + 1)")
        self.assertEqual(text, "x@2 = (x@1 + 1)")
        self.assertIsNone(span)

    def test_last_occurrence_wins(self):
        # A predicate that itself literally contains a coordinate-looking run
        # keeps it; only the FINAL suffix is peeled off.
        text, span = vc_index.split_span_suffix("a  @ 1.2-3.4  @ 9.0-9.7")
        self.assertEqual(text, "a  @ 1.2-3.4")
        self.assertEqual(
            span, {"start": {"line": 9, "col": 0}, "end": {"line": 9, "col": 7}}
        )


class TestProvenanceFlagFallback(unittest.TestCase):
    def test_flag_rejected_detector(self):
        rej = (
            "ocamlc.opt: unknown option '-vox-dump-vc-provenance'.\n"
            "Usage: ocamlc <options> <files>\n"
        )
        self.assertTrue(vc_index._flag_rejected(rej, "-vox-dump-vc-provenance"))
        self.assertFalse(
            vc_index._flag_rejected("Line 1, ...: vox VC:\n", "-vox-dump-vc-provenance")
        )

    def test_fallback_probes_once_and_caches(self):
        # Simulate an old compiler that rejects the provenance flag: the
        # first dump_capture falls back to -dump-vc, and the verdict is
        # cached so later calls never re-try the flag.
        calls = []

        def fake_compile(source_path, ocamlc, flags, cwd=None):
            calls.append(list(flags))
            if vc_index._PROVENANCE_FLAG in flags:
                return 2, (
                    "ocamlc.opt: unknown option '%s'.\n" % vc_index._PROVENANCE_FLAG
                )
            return 0, DUMP_MULTI_HYP

        saved_state = vc_index._provenance_supported
        saved_fn = vc_index.compile_capture
        try:
            vc_index._provenance_supported = None
            vc_index.compile_capture = fake_compile
            out1 = vc_index.dump_capture("x.ml", "ocamlc", cwd=None)
            self.assertEqual(out1, DUMP_MULTI_HYP)
            self.assertFalse(vc_index._provenance_supported)
            # First call probed the flag then fell back (two invocations).
            self.assertEqual(len(calls), 2)
            self.assertIn(vc_index._PROVENANCE_FLAG, calls[0])
            self.assertNotIn(vc_index._PROVENANCE_FLAG, calls[1])
            # Second call skips the flag entirely (cached).
            calls.clear()
            vc_index.dump_capture("x.ml", "ocamlc", cwd=None)
            self.assertEqual(len(calls), 1)
            self.assertNotIn(vc_index._PROVENANCE_FLAG, calls[0])
        finally:
            vc_index._provenance_supported = saved_state
            vc_index.compile_capture = saved_fn


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

    def test_provenance_spans_present(self):
        # With the real compiler and the provenance flag, the refined
        # parameter's contract hypothesis and the goal both carry a span.
        assert OCAMLC is not None
        d, p = self._write("ok.ml", FIXTURE_OK)
        index = vc_index.build_index(p, OCAMLC, cwd=d)
        vcs = cast(List[Dict[str, Any]], index["vcs"])
        self.assertTrue(any(vc.get("goal_span") is not None for vc in vcs))
        self.assertTrue(
            any(s is not None for vc in vcs for s in vc.get("hyp_spans", [])),
            msg="expected at least one hypothesis to carry a provenance span",
        )

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
