#!/usr/bin/env python3
"""Tests for server.

These are end-to-end: build_check_response / build_goal_response compile
through the built ocamlc, so the suite is skipped unless one is found.
The HTTP tests spawn a real server on an ephemeral port and drive it with
urllib.
"""

import json
import os
import threading
import unittest
import urllib.error
import urllib.request
from typing import Any, Dict, List, cast

import server  # pyright: ignore[reportImplicitRelativeImport]

OCAMLC = server.find_ocamlc()
LEAN = server.find_lean()

# A block is only spliced into the solver input when a VC references it
# (here the reflected `dbl` is used by the refinement), so the live-goal
# path has something to open.
SOURCE = """\
let rec total_ dbl n =
  if n <= 0 then 0 else 2 + dbl (n - 1)
[@@vox.decreases n]

[%%vox.lean {lean|
theorem dbl_nonneg (n : Int) (h : 0 <= n) : dbl n >= 0 := by
  fun_induction dbl n <;> omega
|lean}]

let use () =
  let refine_ ok = (dbl 0 : int{ _ = 0 }) in
  ok
"""


def _regions_of_kind(resp: Dict[str, object], kind: str) -> List[Dict[str, Any]]:
    regions = cast(List[Dict[str, Any]], resp["regions"])
    return [r for r in regions if r["kind"] == kind]


@unittest.skipUnless(OCAMLC, "no ocamlc (set VOX_OCAMLC)")
class TestCheckResponse(unittest.TestCase):
    def test_regions(self):
        resp = server.build_check_response(SOURCE, 7, OCAMLC or "", LEAN)
        self.assertEqual(resp["revision"], 7)
        vcs = _regions_of_kind(resp, "vc")
        self.assertTrue(len(vcs) >= 1)
        # 0-based line: the VC (dbl 0 refinement) is on source line 11
        # (1-based) => line 10.
        self.assertEqual(vcs[0]["start"]["line"], 10)
        self.assertIn("goal", vcs[0])
        blocks = _regions_of_kind(resp, "block")
        self.assertEqual(len(blocks), 1)
        thms = _regions_of_kind(resp, "theorem")
        self.assertEqual(len(thms), 1)
        self.assertEqual(thms[0]["name"], "dbl_nonneg")
        self.assertEqual(thms[0]["goal"], "dbl n >= 0")

    @unittest.skipUnless(LEAN, "no lean")
    def test_status_and_generated(self):
        resp = server.build_check_response(SOURCE, 1, OCAMLC or "", LEAN)
        self.assertTrue(resp["ok"])
        vcs = _regions_of_kind(resp, "vc")
        self.assertTrue(any(v["status"] == "proved" for v in vcs))
        self.assertIsInstance(resp["generated_lean"], str)


@unittest.skipUnless(OCAMLC, "no ocamlc (set VOX_OCAMLC)")
class TestPointStates(unittest.TestCase):
    SRC = (
        "let top1 (u : unit) : int{ _ >= 0 } = 0\n"
        "\n"
        "let f (u : unit) : int =\n"
        "  let x = 1 in\n"
        "  let y = 2 in\n"
        "  x + y\n"
    )

    def _state_at(self, states, line, col):
        best = None
        for st in states:
            if (st["start"]["line"], st["start"]["col"]) <= (line, col) <= (
                st["end"]["line"],
                st["end"]["col"],
            ):
                if best is None or (
                    st["end"]["line"] - st["start"]["line"],
                    st["end"]["col"] - st["start"]["col"],
                ) < (
                    best["end"]["line"] - best["start"]["line"],
                    best["end"]["col"] - best["start"]["col"],
                ):
                    best = st
        return best

    def test_gap_after_in_sees_the_binder(self):
        resp = server.build_check_response(self.SRC, 1, OCAMLC or "", None)
        states = cast(List[Dict[str, Any]], resp["states"])
        self.assertTrue(states)
        # 0-based (3, 13): just after "let x = 1 in" on source line 4.
        st = self._state_at(states, 3, 13)
        self.assertIsNotNone(st)
        names = [v["name"] for v in cast(List[Dict[str, Any]], st["scope"])]
        self.assertIn("x", names)
        # 0-based (4, 13): after the second in -> both binders.
        st2 = self._state_at(states, 4, 13)
        names2 = [v["name"] for v in cast(List[Dict[str, Any]], st2["scope"])]
        self.assertIn("x", names2)
        self.assertIn("y", names2)

    def test_toplevel_names_excluded(self):
        resp = server.build_check_response(self.SRC, 1, OCAMLC or "", None)
        states = cast(List[Dict[str, Any]], resp["states"])
        for st in states:
            names = [v["name"] for v in cast(List[Dict[str, Any]], st["scope"])]
            self.assertNotIn("top1", names)
            self.assertNotIn("f", names)


@unittest.skipUnless(OCAMLC and LEAN, "need ocamlc + lean")
class TestGoalResponse(unittest.TestCase):
    def test_live_goal_in_block(self):
        # cursor on the proof line inside the block, before the induction.
        proof_line = None
        for i, line in enumerate(SOURCE.split("\n")):
            if "fun_induction" in line:
                proof_line = i
        assert proof_line is not None
        resp = server.build_goal_response(SOURCE, proof_line, 2, 3, OCAMLC or "", LEAN)
        self.assertEqual(resp["revision"], 3)
        self.assertEqual(resp["status"], "ok", msg=resp.get("detail"))
        joined = "\n".join(cast(List[str], resp["goals"]))
        self.assertIn("dbl n", joined)


@unittest.skipUnless(OCAMLC, "no ocamlc")
class TestHttp(unittest.TestCase):
    httpd: server.ThreadingHTTPServer  # pyright: ignore[reportUninitializedInstanceVariable]
    port: int  # pyright: ignore[reportUninitializedInstanceVariable]
    thread: threading.Thread  # pyright: ignore[reportUninitializedInstanceVariable]

    def setUp(self):
        self.httpd, self.port = server.make_server(0, OCAMLC or "", LEAN)
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.httpd.shutdown()

    def _post(self, path: str, body: Dict[str, object]) -> Dict[str, object]:
        data = json.dumps(body).encode("utf-8")
        req = urllib.request.Request(
            "http://127.0.0.1:%d%s" % (self.port, path),
            data=data,
            headers={"Content-Type": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=120) as resp:
            return json.loads(resp.read().decode("utf-8"))

    def test_check_over_http(self):
        resp = self._post("/check", {"source": SOURCE, "revision": 42})
        self.assertEqual(resp["revision"], 42)
        self.assertIn("regions", resp)
        self.assertTrue(len(cast(List[object], resp["regions"])) >= 1)
        self.assertFalse(resp["fast"])

    def test_fast_check_skips_lean(self):
        """fast:true is the as-you-type pass: full VC shapes with spans,
        no Lean solve (statuses unknown), no generated Lean."""
        resp = self._post(
            "/check", {"source": SOURCE, "revision": 43, "fast": True}
        )
        self.assertEqual(resp["revision"], 43)
        self.assertTrue(resp["fast"])
        self.assertIsNone(resp["generated_lean"])
        vcs = _regions_of_kind(resp, "vc")
        self.assertTrue(len(vcs) >= 1)
        self.assertIn("hypotheses", vcs[0])
        # No solver ran: nothing gets a Lean verdict.
        self.assertTrue(all(v["status"] != "proved" for v in vcs))

    def test_fast_check_reports_elaboration_errors(self):
        resp = self._post(
            "/check",
            {"source": "let x : int{ _ = } = 1\n", "revision": 44, "fast": True},
        )
        self.assertFalse(resp["ok"])
        self.assertTrue(len(cast(List[object], resp["errors"])) >= 1)

    def test_unknown_endpoint(self):
        try:
            self._post("/nope", {})
            self.fail("expected HTTP error")
        except urllib.error.HTTPError as e:
            self.assertEqual(e.code, 404)

    def _get(self, path: str) -> bytes:
        req = urllib.request.Request("http://127.0.0.1:%d%s" % (self.port, path))
        with urllib.request.urlopen(req, timeout=30) as resp:
            return resp.read()

    def test_serves_index(self):
        body = self._get("/").decode("utf-8")
        self.assertIn("vox editor", body)
        self.assertIn("app.js", body)

    def test_serves_static_js(self):
        body = self._get("/selection.js").decode("utf-8")
        self.assertIn("selectRegion", body)

    def test_no_traversal(self):
        try:
            self._get("/../server.py")
            self.fail("expected HTTP error")
        except urllib.error.HTTPError as e:
            self.assertIn(e.code, (400, 404))


class TestExampleEndpoints(unittest.TestCase):
    """GET /examples and /examples/<name>. These serve committed static
    files, so no compiler is needed and the tests run unconditionally."""

    httpd: server.ThreadingHTTPServer  # pyright: ignore[reportUninitializedInstanceVariable]
    port: int  # pyright: ignore[reportUninitializedInstanceVariable]
    thread: threading.Thread  # pyright: ignore[reportUninitializedInstanceVariable]

    def setUp(self):
        self.httpd, self.port = server.make_server(0, "", None)
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.httpd.shutdown()

    def _get(self, path: str):
        req = urllib.request.Request("http://127.0.0.1:%d%s" % (self.port, path))
        with urllib.request.urlopen(req, timeout=30) as resp:
            return resp.headers.get("Content-Type", ""), resp.read()

    def test_examples_index(self):
        ctype, body = self._get("/examples")
        self.assertIn("application/json", ctype)
        data = json.loads(body.decode("utf-8"))
        examples = cast(List[Dict[str, Any]], data["examples"])
        self.assertTrue(len(examples) >= 1)
        for ex in examples:
            self.assertIn("name", ex)
            self.assertIn("title", ex)
            self.assertIn("description", ex)
            self.assertIn("verifies", ex)
        # The failing counterexample is present and flagged.
        cex = [e for e in examples if not e["verifies"]]
        self.assertEqual(len(cex), 1)

    def test_example_source(self):
        # Load the index and fetch the first example's source.
        _, body = self._get("/examples")
        first = cast(List[Dict[str, Any]], json.loads(body)["examples"])[0]
        ctype, src = self._get("/examples/" + first["name"])
        self.assertIn("text/plain", ctype)
        self.assertIn("let", src.decode("utf-8"))

    def test_example_not_found(self):
        try:
            self._get("/examples/no_such_example")
            self.fail("expected HTTP error")
        except urllib.error.HTTPError as e:
            self.assertEqual(e.code, 404)

    def test_example_no_traversal(self):
        try:
            self._get("/examples/..%2f..%2fserver")
            self.fail("expected HTTP error")
        except urllib.error.HTTPError as e:
            self.assertEqual(e.code, 404)


if __name__ == "__main__":
    unittest.main()
