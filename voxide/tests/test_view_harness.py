"""Drive the headless textual-view tool (tools/voxide-view.js) against a real
server.py, and assert what it renders.

The server here uses deterministic FAKE ``/check`` and ``/vcs`` providers, so
the test needs neither the vox2 compiler nor Lean: the proof pane, the per-VC
underlines, the legend, the read-only doc viewer, and the discard-edits guard
are all driven by the real ``workspace`` and the real ``app.js``, none of which
depend on the compiler. The test is skipped if ``node`` is not on PATH."""

import os
import shutil
import subprocess
import tempfile
import threading
import unittest

import server  # pyright: ignore[reportImplicitRelativeImport]

HERE = os.path.dirname(os.path.abspath(__file__))
VOXIDE = os.path.dirname(HERE)
TOOL = os.path.join(VOXIDE, "tools", "voxide-view.js")
NODE = shutil.which("node")


def fake_check(source, revision, ocamlc, backend="lean"):
    # A deterministic stand-in for the compiler (so the test needs neither the
    # compiler nor Lean): a buffer containing the sentinel below returns a
    # located type error, everything else verifies clean. The exact message is
    # not asserted -- only the harness's rendering of the error path is.
    if "MULTILINE" in source:
        # An error whose span crosses two line boundaries (three covered
        # lines), to pin both the endpoint suffix (marks store {line, ch}, not
        # {line, col}) and the per-line segments: first line from start.ch,
        # the middle line in full, the last line up to end.ch.
        return {
            "revision": revision,
            "ok": False,
            "errors": [
                {
                    "message": "This expression spans three lines",
                    "kind": "type",
                    "start": {"line": 0, "col": 0},
                    "end": {"line": 2, "col": 4},
                }
            ],
            "types": [],
            "signature": {"status": "not-requested", "text": "", "error": ""},
            "outcome": {
                "kind": "type-mode",
                "message": "This expression spans three lines",
                "source_located": True,
            },
            "verification": {"status": "blocked", "message": "", "obligations": False},
        }
    if "(true : int)" in source:
        return {
            "revision": revision,
            "ok": False,
            "errors": [
                {
                    "message": "This expression has type bool but int was expected",
                    "kind": "type",
                    "start": {"line": 0, "col": 11},
                    "end": {"line": 0, "col": 15},
                }
            ],
            "types": [],
            "signature": {"status": "not-requested", "text": "", "error": ""},
            "outcome": {
                "kind": "type-mode",
                "message": "This expression has type bool but int was expected",
                "source_located": True,
            },
            "verification": {
                "status": "blocked",
                "message": "Verification runs once the type errors are fixed.",
                "obligations": False,
            },
        }
    return {
        "revision": revision,
        "ok": True,
        "errors": [],
        "types": [],
        "signature": "val demo : int",
        "verification": {
            "status": "verified",
            "message": "All refinement obligations discharged.",
            "obligations": True,
        },
        **fake_vcs(source, revision, ocamlc, backend),
    }


def fake_vcs(source, revision, ocamlc, backend="lean"):
    # A deterministic prover-style dump (real schema shape) for a
    # buffer that mentions ``need_pos``; every other buffer reports no
    # obligations, so the /check-focused tests are unperturbed. This exercises
    # the real proof-pane rendering (named hypothesis, turnstile goal) without a
    # compiler.
    #
    # ``state_demo`` seeds a single obligation carrying two facts -- a NAMED
    # binder fact and an UNNAMED branch fact -- so the off-obligation
    # "known here" view can be checked for the honesty property: the binder
    # appears, the branch condition never does. ``unproved_demo`` seeds an
    # obligation automation gave up on (distinct from a refutation).
    if "let seven = positive 7" in source:
        return {
            "revision": revision,
            "unavailable": False,
            "hidden": 0,
            "vcs": [
                {
                    "id": 0,
                    "status": "proved",
                    "kind": "contract",
                    "span": {
                        "start": {"line": 10, "col": 21},
                        "end": {"line": 10, "col": 22},
                    },
                    "goal": {"display": "7 > 0", "raw": ""},
                    "hypotheses": [],
                    "counterexample": None,
                    "detail": None,
                    "generated_lean": None,
                }
            ],
        }
    if "state_demo" in source:
        return {
            "revision": revision,
            "unavailable": False,
            "hidden": 0,
            "vcs": [
                {
                    "id": 0,
                    "status": "proved",
                    "kind": "contract",
                    "span": {
                        "start": {"line": 0, "col": 4},
                        "end": {"line": 0, "col": 8},
                    },
                    "goal": {"display": "goal here", "raw": ""},
                    "hypotheses": [
                        {
                            "name": "b",
                            "kind": "binder",
                            "display": "b > 0",
                            "raw": "",
                            # A binder fact: a real in-scope variable, so it is
                            # kept in the off-obligation "known here" view.
                            "span": {
                                "start": {"line": 0, "col": 15},
                                "end": {"line": 0, "col": 16},
                            },
                            "used": True,
                        },
                        {
                            "name": None,
                            "kind": "branch",
                            "display": "GUARDCOND > 0",
                            "raw": "",
                            # A branch fact: it holds only inside its branch, so
                            # it must NOT leak into the off-obligation view even
                            # though its span sits above the caret (excluded by
                            # kind, and by its null name).
                            "span": {
                                "start": {"line": 0, "col": 20},
                                "end": {"line": 0, "col": 25},
                            },
                            "used": True,
                        },
                    ],
                    "counterexample": None,
                    "detail": None,
                    "generated_lean": None,
                }
            ],
        }
    if "unproved_demo" in source:
        return {
            "revision": revision,
            "unavailable": False,
            "hidden": 0,
            "vcs": [
                {
                    "id": 0,
                    "status": "unproved",
                    "kind": "annotation",
                    "span": {
                        "start": {"line": 0, "col": 4},
                        "end": {"line": 0, "col": 8},
                    },
                    "goal": {"display": "hard goal", "raw": ""},
                    "hypotheses": [],
                    "counterexample": None,
                    "detail": "grind gave up",
                    "generated_lean": "theorem vc_0 : ... := by grind",
                }
            ],
        }
    if "witness_demo" in source:
        # A disproved obligation for which the solver DID echo a witness. Since
        # the model is over unbounded Int, it must be labelled a candidate, not
        # a validated runtime counterexample.
        return {
            "revision": revision,
            "unavailable": False,
            "hidden": 0,
            "vcs": [
                {
                    "id": 0,
                    "status": "disproved",
                    "kind": "annotation",
                    "span": {
                        "start": {"line": 0, "col": 4},
                        "end": {"line": 0, "col": 8},
                    },
                    "goal": {"display": "n < 5", "raw": ""},
                    "hypotheses": [],
                    "counterexample": ["n = 7"],
                    "detail": None,
                    "generated_lean": None,
                }
            ],
        }
    if "need_pos" not in source:
        return {"revision": revision, "unavailable": False, "hidden": 0, "vcs": []}
    return {
        "revision": revision,
        "unavailable": False,
        "hidden": 0,
        "vcs": [
            {
                "id": 0,
                "status": "disproved",
                "kind": "contract",
                "span": {"start": {"line": 0, "col": 4}, "end": {"line": 0, "col": 8}},
                "goal": {"display": "0 > 0", "raw": "(app[Stdlib!.>] 0 0)"},
                "hypotheses": [
                    {
                        "name": "y",
                        "display": "y > 0",
                        "raw": "(app[Stdlib!.>] y 0)",
                        "span": None,
                        "used": True,
                    }
                ],
                "counterexample": None,
                "detail": "grind failed",
                "generated_lean": "theorem vc_0 : ...",
            }
        ],
    }


@unittest.skipUnless(NODE, "node is not installed")
class ViewHarnessTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            cls.httpd, port = server.make_server(
                0, "/fake/ocamlc.opt", fake_check, fake_vcs
            )
        except PermissionError as exc:
            raise unittest.SkipTest("sandbox forbids localhost sockets: %s" % exc)
        cls.base = "http://127.0.0.1:%d" % port
        cls.thread = threading.Thread(target=cls.httpd.serve_forever)
        cls.thread.daemon = True
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.httpd.shutdown()
        cls.httpd.server_close()
        cls.thread.join(timeout=2)

    def view(self, *commands):
        env = dict(os.environ)
        env["NO_PROXY"] = "127.0.0.1"
        args = [
            str(NODE),
            TOOL,
            "--server",
            self.base,
            "--frontend",
            VOXIDE,
            "--redact",
        ]
        for c in commands:
            args += ["-e", c]
        proc = subprocess.run(
            args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=60
        )
        return proc.returncode, proc.stdout.decode("utf-8")

    def test_startup_opens_default_example(self):
        code, out = self.view()
        self.assertEqual(code, 0, out)
        self.assertIn("active: examples/overview.ml", out)
        self.assertIn("tokenizer=real", out)
        self.assertIn("actions: automatic (no Check or Verify buttons)", out)
        self.assertRegex(out, r"controls: backend=lean\s+\|.*\(\d+ ms\)")
        self.assertIn("⊢ 7 > 0", out)
        self.assertIn("[vc:proved]", out)

    def test_compact_toggle_defaults_switches_and_saves(self):
        code, out = self.view(
            "render header",
            "toggle compact",
            "render header",
            "toggle compact",
            "render header",
        )
        self.assertEqual(code, 0, out)
        default = "compact=on (saved=default)"
        full = "compact=off (saved=off)"
        restored = "compact=on (saved=on)"
        self.assertIn(default, out)
        self.assertIn(full, out)
        self.assertIn(restored, out)
        self.assertLess(out.index(default), out.index(full))
        self.assertLess(out.index(full), out.index(restored))

    def test_real_type_error_view(self):
        # Purely /check-driven (no /vcs): a real type error surfaces as an
        # inline [type/mode] underline, a diagnostics entry, an unavailable
        # signature, and a gated verification banner.
        code, out = self.view(
            "source let bad = (true : int)",
            "render editor",
            "render diagnostics",
            "render signature",
            "render verification",
        )
        self.assertEqual(code, 0, out)
        self.assertIn("[type/mode]", out)
        self.assertIn("--- Diagnostics ---", out)
        self.assertIn("Unavailable for this check.", out)

    def test_nonzero_exit_on_surfaced_error(self):
        # An app.js error that surfaces through the always-present render (here,
        # opening a path that is not an allowlisted file) must both be shown
        # AND drive a nonzero exit -- displaying it may not clear the signal.
        code, out = self.view("open examples/does_not_exist.ml")
        self.assertIn("Frontend errors", out)
        self.assertNotEqual(code, 0, out)
        # openfile (force=true, no guard) must fail the same way, not silently.
        code, out = self.view("openfile examples/does_not_exist.ml")
        self.assertIn("Frontend errors", out)
        self.assertNotEqual(code, 0, out)

    def test_multiline_mark_span(self):
        # Pin a diagnostic whose span covers three lines: the endpoint suffix
        # must read a real line:col (never NaN), and a segment must be drawn on
        # EVERY covered line -- the middle line fully, and each continuation
        # line labelled "(cont.)" -- not just the first.
        middle = "the middle line is fully underlined"
        with tempfile.NamedTemporaryFile("w", suffix=".ml", delete=False) as fh:
            fh.write("(* MULTILINE marker *)\n" + middle + "\nlet value = 1\n")
            src = fh.name
        try:
            code, out = self.view("source-file " + src, "render editor")
        finally:
            os.unlink(src)
        self.assertEqual(code, 0, out)
        self.assertIn("(to 3:5)", out)
        self.assertNotIn("NaN", out)
        # The middle line is underlined in full (its whole width of tildes) ...
        self.assertIn("~" * len(middle) + "  [type] (cont.)", out)
        # ... and both the middle and last covered lines carry a segment.
        self.assertEqual(out.count("(cont.)"), 2)

    def test_real_proof_pane_is_prover_style(self):
        # Real (fake_vcs) data renders the redesigned pane: the loud verdict
        # token headline, the turnstile goal, and (in full view) the named
        # hypothesis.  Toggle off compact so the depth-1 proof state shows.
        code, out = self.view(
            "source let bad = need_pos 0",
            "toggle compact",
            "cursor 1:6",
            "render proof",
        )
        self.assertEqual(code, 0, out)
        # The verdict rides the goal line: a leading ✗ glyph + coloured goal
        # (the old loud "DISPROVED" token line and "[disproved]" badge are gone).
        self.assertIn("⊢ 0 > 0", out)
        self.assertIn("✗", out)  # verdict glyph (now to the RIGHT of the goal)
        self.assertNotIn("DISPROVED", out)
        self.assertNotIn("mode: obligation", out)
        self.assertNotIn("[disproved]", out)
        self.assertIn("y : y > 0", out)

    def test_state_at_cursor_shows_binder_hides_branch(self):
        # Off every obligation, the pane shows the approximate "known here"
        # view: a NAMED binder fact introduced above the caret appears, and the
        # honesty caveat is shown. The load-bearing property is that the UNNAMED
        # branch condition (which holds only inside its branch) is NEVER shown,
        # even though its span sits above the caret.
        code, out = self.view(
            "source let state_demo = padding_to_make_the_line_wide_enough_xx",
            "toggle compact",
            "cursor 1:40",
            "render proof",
        )
        self.assertEqual(code, 0, out)
        # The grey CONTEXT token with its pinned `· approximate` qualifier
        # replaces the old "mode: context" header and "known at this point".
        self.assertIn("◦ CONTEXT · approximate", out)
        self.assertNotIn("mode: context", out)
        self.assertIn("b : b > 0", out)
        self.assertIn("Branch conditions are omitted", out)
        # The honesty constraint: the branch fact must not leak by position.
        self.assertNotIn("GUARDCOND", out)

    def test_disproved_without_witness_is_honest(self):
        # A disproved obligation whose solver produced no model must say so
        # explicitly rather than leave the pane looking empty, and it exposes
        # the generated-Lean escape hatch (copy / download / open).
        code, out = self.view(
            "source let bad = need_pos 0",
            "toggle compact",
            "cursor 1:6",
            "render proof",
        )
        self.assertEqual(code, 0, out)
        # The no-witness fact is welded to the goal line in both views; the full
        # refutation note appears at depth 1.
        # Glyph to the RIGHT of the goal, welded `· no witness` kept with the goal.
        self.assertIn("⊢ 0 > 0 · no witness  ✗", out)
        self.assertIn(
            "Disproved: the solver refuted this goal but produced no concrete witness.",
            out,
        )
        # Escape hatch on the generated theorem.
        self.assertIn("generated Lean", out)
        self.assertIn("copy", out)
        self.assertIn("download .lean", out)
        self.assertIn("open in new tab", out)

    def test_disproved_witness_is_labeled_candidate(self):
        # A witness echoed from a Lean disproof is over the unbounded-Int model,
        # so it is a candidate, not a validated runtime counterexample. The pane
        # must say so rather than presenting it as an established fact.
        code, out = self.view(
            "source let x = witness_demo ()",
            "toggle compact",
            "cursor 1:6",
            "render proof",
        )
        self.assertEqual(code, 0, out)
        # A concrete witness exists, so the goal line welds `· witness` (not `no
        # witness`); the candidate model appears at depth 1.
        self.assertIn("· witness", out)
        self.assertNotIn("· no witness", out)
        self.assertIn("✗", out)
        self.assertIn("candidate counterexample (unbounded-int model)", out)
        self.assertIn("may not be a valid machine int", out)
        self.assertIn("n = 7", out)

    def test_unproved_is_distinct_from_disproved(self):
        # "unproved" (automation gave up) must read differently from
        # "disproved" (refuted): the note says the goal may still hold, and no
        # counterexample section is presented.
        code, out = self.view(
            "source let x = unproved_demo ()",
            "toggle compact",
            "cursor 1:6",
            "render proof",
        )
        self.assertEqual(code, 0, out)
        # The verdict rides the goal line: a leading ⚠ glyph (distinct from
        # disproved's ✗ -- colour-blind-safe), never the loud "UNPROVED" token.
        self.assertIn("⚠", out)  # verdict glyph (now to the RIGHT of the goal)
        self.assertNotIn("UNPROVED", out)
        self.assertNotIn("[unproved]", out)
        self.assertIn("Unproved: automation gave up", out)
        self.assertIn("the goal may still hold", out)
        # An unproved goal is not refuted, so no witness is presented and it is
        # never labelled disproved.
        self.assertNotIn("goal is false when", out)
        self.assertNotIn("Disproved:", out)

    def test_doc_mode_shows_viewer_and_suppresses_proof_pane(self):
        # Opening a read-only doc shows the rendered doc (not the editor buffer),
        # and the proof pane shows the doc placeholder with no obligation marks:
        # ux2's doc-mode guard meets the real /vcs path.
        code, out = self.view(
            "openfile docs/welcome.md", "render editor", "render proof"
        )
        self.assertEqual(code, 0, out)
        self.assertIn("Document (docs/welcome.md, read-only)", out)
        self.assertIn("Documentation (read-only).", out)
        self.assertNotIn("[vc:", out)

    def test_doc_then_editable_file_restores_the_editor(self):
        # Leaving a doc for an editable file must restore the editor (exitDocMode
        # calls cm.refresh) with no frontend error -- the editor, not the doc
        # viewer, is shown again.
        code, out = self.view(
            "openfile docs/welcome.md", "open! overview", "render editor"
        )
        self.assertEqual(code, 0, out)
        self.assertNotIn("Frontend errors", out)
        self.assertIn("Editor (examples/overview.ml", out)
        self.assertIn("let seven = positive 7", out)

    def test_discard_guard_declined_is_clean(self):
        code, out = self.view(
            "source let bogus = 1",
            "confirm no",
            "open abs",
            "render dialogs",
            "render explorer",
        )
        # A declined guard is expected behaviour, so the run still succeeds ...
        self.assertEqual(code, 0, out)
        self.assertIn("-> no", out)
        # ... and the file did NOT switch (overview stays active).
        self.assertIn("active: examples/overview.ml", out)

    def test_bad_command_fails(self):
        code, out = self.view("frobnicate the pylons")
        self.assertNotEqual(code, 0)


if __name__ == "__main__":
    unittest.main()
