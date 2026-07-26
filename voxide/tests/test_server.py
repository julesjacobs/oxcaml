import json
import threading
import unittest
import urllib.error
import urllib.request
from unittest import mock

import server  # pyright: ignore[reportImplicitRelativeImport]


def fake_vcs(source, revision, ocamlc, backend="lean"):
    # A single disproved obligation in the flat frontend shape, so the /vcs
    # transport can be exercised without a real compiler.  The real provider
    # (compiler.vcs_for_source) is unit-tested against the built compiler in
    # test_compiler.py; here we only pin the HTTP relay.
    return {
        "revision": revision,
        "vcs": [
            {
                "id": 0,
                "status": "disproved",
                "kind": "contract",
                "span": {
                    "start": {"line": 0, "col": 4},
                    "end": {"line": 0, "col": 8},
                },
                "goal": "(app[Stdlib!.=] 2 1)",
                "hypotheses": [{"text": "n >= 0", "used": True}],
                "counterexample": None,
                "detail": "grind failed",
                "generated_lean": "theorem vc_0 : ...",
                "test_source_length": len(source),
                "test_compiler": ocamlc,
                "test_backend": backend,
            }
        ],
    }


def fake_check(source, revision, ocamlc, backend="lean"):
    # A refined buffer whose obligation "failed" verification, so both the
    # /check passthrough and the /verify projection have something to carry.
    return {
        "revision": revision,
        "ok": False,
        "errors": [
            {
                "message": "Refinement verification failed (not proved)",
                "kind": "verification",
                "start": {"line": 0, "col": 4},
                "end": {"line": 0, "col": 8},
            },
            {"message": "unrelated note", "kind": "type"},
        ],
        "types": [],
        "signature": "val chars : int",
        "verification": {
            "status": "failed",
            "message": "Refinement verification failed (not proved)",
            "obligations": True,
        },
        "test_source_length": len(source),
        "test_compiler": ocamlc,
        "test_backend": backend,
        **fake_vcs(source, revision, ocamlc, backend),
    }


def fake_signature(source, revision, ocamlc, backend="lean"):
    return {
        "revision": revision,
        "backend": backend,
        "signature": {
            "status": "unavailable",
            "text": "",
            "error": "signature presentation failed",
        },
    }


class HttpRoundTripTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            cls.httpd, port = server.make_server(
                0, "/fake/ocamlc.opt", fake_check, fake_vcs
            )
        except PermissionError as exc:
            raise unittest.SkipTest(
                "sandbox does not permit localhost sockets: {0}".format(exc)
            )
        cls.base = "http://127.0.0.1:{0}".format(port)
        cls.thread = threading.Thread(target=cls.httpd.serve_forever)
        cls.thread.daemon = True
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.httpd.shutdown()
        cls.httpd.server_close()
        cls.thread.join(timeout=2)

    def post(self, path, payload):
        request = urllib.request.Request(
            self.base + path,
            data=json.dumps(payload).encode("utf-8"),
            headers={"Content-Type": "application/json"},
            method="POST",
        )
        with urllib.request.urlopen(request, timeout=3) as response:
            return response.status, json.loads(response.read().decode("utf-8"))

    def get(self, path):
        with urllib.request.urlopen(self.base + path, timeout=3) as response:
            return response.status, response.read().decode("utf-8")

    def test_ls_lists_examples_root(self):
        status, body = self.get("/ls")
        self.assertEqual(status, 200)
        tree = json.loads(body)
        roots = tree["roots"]
        self.assertEqual(roots[0]["id"], "examples")
        names = [child["name"] for child in roots[0]["children"]]
        self.assertIn("overview.ml", names)

    def test_examples_index_is_served(self):
        status, body = self.get("/examples")
        self.assertEqual(status, 200)
        index = json.loads(body)
        self.assertTrue(any(e.get("default") for e in index["examples"]))

    def test_backend_config_is_available_before_checking(self):
        status, body = self.get("/config")
        self.assertEqual(status, 200)
        config = json.loads(body)
        self.assertEqual(config["backend_options"], ["lean"])
        self.assertEqual(config["default_backend"], "lean")

    def test_file_serves_example_source(self):
        status, body = self.get("/file?path=examples/overview.ml")
        self.assertEqual(status, 200)
        self.assertIn("refinement", body.lower())

    def test_ls_includes_docs_root(self):
        status, body = self.get("/ls")
        self.assertEqual(status, 200)
        tree = json.loads(body)
        roots = {root["id"]: root for root in tree["roots"]}
        self.assertIn("docs", roots)
        names = [child["name"] for child in roots["docs"]["children"]]
        self.assertIn("welcome.md", names)

    def test_file_serves_a_read_only_doc(self):
        status, body = self.get("/file?path=docs/welcome.md")
        self.assertEqual(status, 200)
        self.assertIn("vox2 IDE", body)

    def test_doc_traversal_is_rejected(self):
        with self.assertRaises(urllib.error.HTTPError) as raised:
            self.get("/file?path=docs/../server.py")
        self.assertEqual(raised.exception.code, 404)

    def test_doc_nul_byte_is_rejected_cleanly(self):
        with self.assertRaises(urllib.error.HTTPError) as raised:
            self.get("/file?path=docs/%00.md")
        self.assertEqual(raised.exception.code, 404)

    def test_file_traversal_is_rejected(self):
        with self.assertRaises(urllib.error.HTTPError) as raised:
            self.get("/file?path=examples/../server.py")
        self.assertEqual(raised.exception.code, 404)

    def test_file_nul_byte_is_rejected_cleanly(self):
        # A NUL in the path must yield a clean 404, not a crashed connection
        # (workspace.resolve swallows the ValueError realpath raises).
        with self.assertRaises(urllib.error.HTTPError) as raised:
            self.get("/file?path=examples/%00.ml")
        self.assertEqual(raised.exception.code, 404)

    def test_vcs_relays_provider_output(self):
        # /vcs compiles the live buffer's source and relays the provider's
        # translated VCs.  The source (not a path) is what the provider
        # receives.
        status, payload = self.post(
            "/vcs", {"source": "let bad = need_one 2", "revision": 7}
        )
        self.assertEqual(status, 200)
        self.assertEqual(payload["revision"], 7)
        self.assertEqual(len(payload["vcs"]), 1)
        vc = payload["vcs"][0]
        self.assertEqual(vc["status"], "disproved")
        self.assertEqual(vc["detail"], "grind failed")
        self.assertIn("generated_lean", vc)
        self.assertEqual(vc["test_source_length"], len("let bad = need_one 2"))

    def test_vcs_rejects_non_string_source(self):
        with self.assertRaises(urllib.error.HTTPError) as raised:
            self.post("/vcs", {"source": 5, "revision": 1})
        self.assertEqual(raised.exception.code, 400)

    def test_backend_is_validated_and_relayed(self):
        with mock.patch.dict(
            "os.environ",
            {"VOXIDE_SMT_SOLVER": "/solver/z3 -in"},
            clear=True,
        ):
            status, payload = server.process_post(
                "/vcs",
                b'{"source":"x","revision":2,"backend":"cross"}',
                fake_check,
                "/fake/ocamlc.opt",
                fake_vcs,
                available_backends=("lean", "z3", "oxsmt", "cross"),
            )
        self.assertEqual(status, 200)
        self.assertEqual(payload["backend"], "cross")
        self.assertEqual(payload["vcs"][0]["test_backend"], "cross")
        self.assertEqual(payload["backend_options"], ["lean", "z3", "oxsmt", "cross"])
        self.assertEqual(
            payload["backend_solver_configuration"],
            {"z3": True, "oxsmt": False},
        )

        status, payload = server.process_post(
            "/vcs",
            b'{"source":"x","backend":"z3"}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
            available_backends=("lean",),
        )
        self.assertEqual(status, 400)
        self.assertIn("does not support", payload["error"])

    def test_check_round_trip_preserves_revision(self):
        status, payload = self.post("/check", {"source": "let x = 1", "revision": 23})
        self.assertEqual(status, 200)
        self.assertEqual(payload["revision"], 23)
        self.assertEqual(payload["test_source_length"], 9)
        self.assertEqual(payload["signature"], "val chars : int")
        self.assertEqual(payload["verification"]["status"], "failed")
        self.assertEqual(payload["vcs"][0]["status"], "disproved")

    def test_verify_projects_verification_view(self):
        status, payload = self.post("/verify", {"source": "let x = 1", "revision": 4})
        self.assertEqual(status, 200)
        self.assertEqual(payload["revision"], 4)
        self.assertEqual(payload["verification"]["status"], "failed")
        # Only verification-kind errors survive the projection.
        self.assertEqual(len(payload["errors"]), 1)
        self.assertEqual(payload["errors"][0]["kind"], "verification")

    def test_verify_defaults_empty_source(self):
        status, payload = self.post("/verify", {"revision": 4})
        self.assertEqual(status, 200)
        self.assertIn("verification", payload)

    def test_static_index_is_served(self):
        with urllib.request.urlopen(self.base + "/", timeout=3) as response:
            body = response.read().decode("utf-8")
        self.assertIn("vox2 IDE", body)

    def test_static_path_traversal_is_rejected(self):
        with self.assertRaises(urllib.error.HTTPError) as raised:
            urllib.request.urlopen(self.base + "/%2e%2e/server.py", timeout=3)
        self.assertEqual(raised.exception.code, 404)


class RequestLogicTests(unittest.TestCase):
    def test_cancellation_lane_is_monotonic_latest_revision_wins(self):
        lane = server.CancellationLane()
        revision_two = lane.begin(2)
        stale = lane.begin(1)
        self.assertTrue(stale.is_set())
        self.assertFalse(revision_two.is_set())
        revision_three = lane.begin(3)
        self.assertTrue(revision_two.is_set())
        self.assertFalse(revision_three.is_set())
        same_revision_newer = lane.begin(3)
        self.assertTrue(revision_three.is_set())
        self.assertFalse(same_revision_newer.is_set())
        lane.end(same_revision_newer)
        self.assertIsNone(lane.active)

    def test_signature_failure_is_a_separate_presentation_channel(self):
        status, primary = server.process_post(
            "/check",
            b'{"source":"x","revision":12}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
        )
        signature_status, signature = server.process_post(
            "/signature",
            b'{"source":"x","revision":12}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
            signature_checker=fake_signature,
        )
        self.assertEqual((status, signature_status), (200, 200))
        self.assertEqual(primary["verification"]["status"], "failed")
        self.assertEqual(signature["signature"]["status"], "unavailable")
        self.assertEqual(primary["revision"], signature["revision"])

    def test_adapter_cancellation_has_non_error_transport_state(self):
        def cancelled(source, revision, ocamlc, backend, cancel_check=None):
            self.assertTrue(cancel_check())
            raise server.compiler_adapter.CompileCancelled()

        status, payload = server.process_post(
            "/check",
            b'{"source":"x","revision":3}',
            cancelled,
            "/fake/ocamlc.opt",
            fake_vcs,
            cancel_check=lambda: True,
        )
        self.assertEqual(status, 499)
        self.assertEqual(payload, {"error": "request cancelled"})
    def test_preferred_backend_uses_configured_oxsmt(self):
        options = ("lean", "z3", "oxsmt", "cross")
        self.assertEqual(
            server.preferred_backend(options, {"z3": True, "oxsmt": True}),
            "oxsmt",
        )

    def test_preferred_backend_degrades_when_oxsmt_is_unusable(self):
        options = ("lean", "z3", "oxsmt", "cross")
        self.assertEqual(
            server.preferred_backend(options, {"z3": True, "oxsmt": False}),
            "lean",
        )
        self.assertEqual(
            server.preferred_backend(("lean",), {"oxsmt": True}),
            "lean",
        )

    def test_backend_configuration_exposes_default(self):
        with mock.patch.object(
            server.compiler_adapter,
            "backend_options",
            return_value=("lean", "z3", "oxsmt", "cross"),
        ), mock.patch.object(
            server.compiler_adapter,
            "backend_solver_configuration",
            return_value={"z3": True, "oxsmt": True},
        ):
            config = server.backend_configuration("/fake/ocamlc")
        self.assertEqual(config["default_backend"], "oxsmt")
        self.assertEqual(
            config["backend_options"], ["lean", "z3", "oxsmt", "cross"]
        )

    def test_type_only_backend_is_validated_and_relayed(self):
        status, payload = server.process_post(
            "/check",
            b'{"source":"let x = 1","revision":2,"backend":"none"}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
            available_backends=("lean", "none"),
        )
        self.assertEqual(status, 200)
        self.assertEqual(payload["backend"], "none")
        self.assertEqual(payload["test_backend"], "none")

    def test_solver_configuration_is_consistent_across_payloads(self):
        configuration = {"z3": True, "oxsmt": True}
        contradictory = lambda *args, **kwargs: {
            **fake_check(*args, **kwargs),
            "backend_solver_configuration": {"z3": False, "oxsmt": False},
        }
        with mock.patch.object(
            server.compiler_adapter,
            "backend_solver_configuration",
            return_value=configuration,
        ):
            config = server.backend_configuration(
                "/current/ocamlc", ("lean", "z3", "oxsmt", "cross")
            )
            _, check = server.process_post(
                "/check",
                b'{"source":"x"}',
                contradictory,
                "/current/ocamlc",
                fake_vcs,
                available_backends=("lean", "z3", "oxsmt", "cross"),
            )
            _, vcs = server.process_post(
                "/vcs",
                b'{"source":"x"}',
                fake_check,
                "/current/ocamlc",
                fake_vcs,
                available_backends=("lean", "z3", "oxsmt", "cross"),
            )
        self.assertEqual(config["backend_solver_configuration"], configuration)
        self.assertEqual(check["backend_solver_configuration"], configuration)
        self.assertEqual(vcs["backend_solver_configuration"], configuration)

    def test_check_payload_carries_unified_vcs(self):
        status, payload = server.process_post(
            "/check",
            b'{"source":"x","revision":8}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
        )
        self.assertEqual(status, 200)
        self.assertEqual(payload["revision"], 8)
        self.assertEqual(payload["vcs"][0]["status"], "disproved")

    def test_solver_configuration_metadata_is_added(self):
        with mock.patch.dict(
            "os.environ",
            {"VOXIDE_OXSMT_SOLVER": "/solver/oxsmt"},
            clear=True,
        ):
            status, payload = server.process_post(
                "/vcs",
                b'{"source":"x","backend":"oxsmt"}',
                fake_check,
                "/fake/ocamlc.opt",
                fake_vcs,
                available_backends=("lean", "z3", "oxsmt", "cross"),
            )
        self.assertEqual(status, 200)
        self.assertEqual(
            payload["backend_solver_configuration"],
            {"z3": False, "oxsmt": True},
        )

    def test_deep_json_returns_json_error(self):
        raw = b'{"source":' + b"[" * 1100 + b"]" * 1100 + b"}"
        status, payload = server.process_post(
            "/check", raw, fake_check, "/fake/ocamlc.opt", fake_vcs
        )
        self.assertEqual(status, 400)
        self.assertIsInstance(payload["error"], str)

    def test_lone_surrogate_source_returns_json_error(self):
        raw = b'{"source":"\\ud800","revision":9}'
        status, payload = server.process_post(
            "/check", raw, fake_check, "/fake/ocamlc.opt", fake_vcs
        )
        self.assertEqual(status, 400)
        self.assertIn("Unicode scalar", payload["error"])

    def test_checker_exception_returns_json_error(self):
        def broken_check(source, revision, ocamlc, backend):
            raise UnicodeEncodeError("utf-8", "\ud800", 0, 1, "invalid")

        status, payload = server.process_post(
            "/check", b'{"source":"valid"}', broken_check, "/fake/ocamlc.opt", fake_vcs
        )
        self.assertEqual(status, 500)
        self.assertEqual(payload, {"error": "internal compiler service error"})

    def test_verify_checker_exception_returns_json_error(self):
        # The /verify error path must also yield a clean error envelope (which
        # the client then drops behind its revision guard).
        def broken_check(source, revision, ocamlc, backend):
            raise RuntimeError("boom")

        status, payload = server.process_post(
            "/verify", b'{"source":"valid"}', broken_check, "/fake/ocamlc.opt", fake_vcs
        )
        self.assertEqual(status, 500)
        self.assertEqual(payload, {"error": "internal compiler service error"})

    def test_vcs_provider_exception_returns_json_error(self):
        # A provider blow-up (e.g. a compiler crash) must yield the same clean
        # envelope, which the client drops behind its revision guard.
        def broken_vcs(source, revision, ocamlc, backend):
            raise RuntimeError("boom")

        status, payload = server.process_post(
            "/vcs", b'{"source":"valid"}', fake_check, "/fake/ocamlc.opt", broken_vcs
        )
        self.assertEqual(status, 500)
        self.assertEqual(payload, {"error": "internal compiler service error"})

    def test_workspace_check_relays_provider_payload(self):
        # The transport passes the validated file set / active / revision to the
        # workspace checker and returns its payload verbatim.
        seen = {}

        def fake_workspace(files, active, revision, ocamlc, backend):
            seen["files"] = files
            seen["active"] = active
            return {"revision": revision, "active": active, "ok": True, "vcs": []}

        raw = (
            b'{"revision":4,"active":"Demo.ml","files":['
            b'{"name":"Demo.mli","source":"val x : int"},'
            b'{"name":"Demo.ml","source":"let x = 1"}]}'
        )
        status, payload = server.process_post(
            "/workspace-check",
            raw,
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
            fake_workspace,
        )
        self.assertEqual(status, 200)
        self.assertEqual(payload["revision"], 4)
        self.assertEqual(payload["active"], "Demo.ml")
        self.assertEqual([f["name"] for f in seen["files"]], ["Demo.mli", "Demo.ml"])

    def test_workspace_check_rejects_empty_file_set(self):
        status, payload = server.process_post(
            "/workspace-check",
            b'{"revision":1,"active":"x","files":[]}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
        )
        self.assertEqual(status, 400)
        self.assertIsInstance(payload["error"], str)

    def test_workspace_check_rejects_non_object_entry(self):
        status, payload = server.process_post(
            "/workspace-check",
            b'{"revision":1,"active":"x","files":["oops"]}',
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
        )
        self.assertEqual(status, 400)

    def test_workspace_check_provider_exception_returns_json_error(self):
        def broken_workspace(files, active, revision, ocamlc, backend):
            raise RuntimeError("boom")

        raw = b'{"revision":1,"active":"D.ml","files":[{"name":"D.ml","source":"x"}]}'
        status, payload = server.process_post(
            "/workspace-check",
            raw,
            fake_check,
            "/fake/ocamlc.opt",
            fake_vcs,
            broken_workspace,
        )
        self.assertEqual(status, 500)
        self.assertEqual(payload, {"error": "internal compiler service error"})


class StaticResolutionTests(unittest.TestCase):
    def test_frontend_allowlist_refuses_python(self):
        self.assertIsNone(server.resolve_static_path("/server.py"))
        self.assertIsNone(server.resolve_static_path("/tests/test_server.py"))
        self.assertEqual(server.resolve_static_path("/app.js"), server.HERE / "app.js")

    def test_vendor_assets_are_allowed_but_cannot_escape(self):
        self.assertEqual(
            server.resolve_static_path("/vendor/codemirror/codemirror.js"),
            server.VENDOR / "codemirror" / "codemirror.js",
        )
        self.assertIsNone(server.resolve_static_path("/vendor/%2e%2e/server.py"))

    def test_embedded_nul_is_not_found(self):
        self.assertIsNone(server.resolve_static_path("/%00"))

    def test_one_shot_file_rejects_noncanonical_paths(self):
        absolute = server.workspace.EXAMPLES_DIR + "/bst/bst.ml"
        targets = [
            "/file?path=examples/bst/../overview.ml",
            "/file?path=examples/" + absolute,
            "/file?path=examples/bst//bst.ml",
            "/file?path=examples/./overview.ml",
        ]
        for target in targets:
            with self.subTest(target=target):
                result = server.one_shot("GET", target, b"", "/fake/ocamlc.opt")
                self.assertEqual(result["status"], 404)

    def test_one_shot_file_serves_manifest_workspace_file(self):
        result = server.one_shot(
            "GET",
            "/file?path=examples/bst/bst.mli",
            b"",
            "/fake/ocamlc.opt",
        )
        self.assertEqual(result["status"], 200)
        self.assertIn("val empty", result["text"])


if __name__ == "__main__":
    unittest.main()
