import copy
import json
import os
import stat
import tempfile
import time
import unittest
from unittest import mock

import compiler as compiler_adapter  # pyright: ignore[reportImplicitRelativeImport]


class BackendConfigurationTests(unittest.TestCase):
    def backend_arguments(self, backend, environment):
        with mock.patch.object(
            compiler_adapter,
            "backend_options",
            return_value=compiler_adapter.BACKENDS,
        ), mock.patch.dict(os.environ, environment, clear=True):
            return compiler_adapter._backend_arguments("/fake/ocamlc.opt", backend)

    def test_solver_flags_are_only_passed_to_backends_that_need_them(self):
        environment = {
            "VOXIDE_SMT_SOLVER": "/solver/z3 -in",
            "VOXIDE_OXSMT_SOLVER": "/solver/oxsmt",
        }
        self.assertEqual(
            self.backend_arguments("lean", environment),
            ["-vox-backend", "lean"],
        )
        self.assertEqual(
            self.backend_arguments("z3", environment),
            ["-vox-backend", "z3", "-vox-smt-solver", "/solver/z3 -in"],
        )
        self.assertEqual(
            self.backend_arguments("oxsmt", environment),
            [
                "-vox-backend",
                "oxsmt",
                "-vox-oxsmt-solver",
                "/solver/oxsmt",
            ],
        )
        self.assertEqual(
            self.backend_arguments("cross", environment),
            [
                "-vox-backend",
                "cross",
                "-vox-smt-solver",
                "/solver/z3 -in",
                "-vox-oxsmt-solver",
                "/solver/oxsmt",
            ],
        )

    def test_unconfigured_solver_flags_are_omitted(self):
        self.assertEqual(
            self.backend_arguments("cross", {}),
            ["-vox-backend", "cross"],
        )
        self.assertEqual(
            self.backend_arguments("z3", {"VOXIDE_SMT_SOLVER": "   "}),
            ["-vox-backend", "z3"],
        )

    def test_legacy_compiler_stays_flag_free(self):
        with mock.patch.object(
            compiler_adapter, "backend_options", return_value=("lean",)
        ), mock.patch.dict(
            os.environ,
            {
                "VOXIDE_SMT_SOLVER": "/solver/z3 -in",
                "VOXIDE_OXSMT_SOLVER": "/solver/oxsmt",
            },
            clear=True,
        ):
            self.assertEqual(
                compiler_adapter._backend_arguments("/legacy/ocamlc.opt", "lean"),
                [],
            )
        with mock.patch.object(
            compiler_adapter,
            "backend_options",
            return_value=("lean", "none"),
        ):
            self.assertEqual(
                compiler_adapter._backend_arguments(
                    "/type-only-legacy/ocamlc.opt", "lean"
                ),
                [],
            )

    def test_solver_configuration_metadata_is_boolean_only(self):
        with mock.patch.dict(
            os.environ,
            {"VOXIDE_SMT_SOLVER": "/private/solver command"},
            clear=True,
        ):
            self.assertEqual(
                compiler_adapter.backend_solver_configuration(),
                {"z3": True, "oxsmt": False},
            )

    def test_in_process_oxsmt_is_configured_only_for_new_compiler(self):
        current_help = mock.Mock(
            stdout=(
                "-vox-oxsmt-solver <command>  Ignored by in-process oxsmt; "
                "legacy benchmark command"
            ),
            stderr="",
        )
        legacy_help = mock.Mock(
            stdout="-vox-oxsmt-solver <command>  External oxsmt solver command",
            stderr="",
        )
        compiler_adapter.supports_in_process_oxsmt.cache_clear()
        with mock.patch.dict(os.environ, {}, clear=True), mock.patch.object(
            compiler_adapter.subprocess,
            "run",
            side_effect=[current_help, legacy_help],
        ):
            self.assertEqual(
                compiler_adapter.backend_solver_configuration(
                    "/current/ocamlc"
                ),
                {"z3": False, "oxsmt": True},
            )
            self.assertEqual(
                compiler_adapter.backend_solver_configuration(
                    "/legacy/ocamlc"
                ),
                {"z3": False, "oxsmt": False},
            )
        compiler_adapter.supports_in_process_oxsmt.cache_clear()

    def test_legacy_oxsmt_environment_configuration_is_unchanged(self):
        compiler_adapter.supports_in_process_oxsmt.cache_clear()
        legacy_help = mock.Mock(stdout="-vox-backend {lean|oxsmt}", stderr="")
        with mock.patch.dict(
            os.environ,
            {"VOXIDE_OXSMT_SOLVER": "/solver/legacy-oxsmt"},
            clear=True,
        ), mock.patch.object(
            compiler_adapter.subprocess, "run", return_value=legacy_help
        ):
            self.assertEqual(
                compiler_adapter.backend_solver_configuration(
                    "/legacy/ocamlc"
                ),
                {"z3": False, "oxsmt": True},
            )
        compiler_adapter.supports_in_process_oxsmt.cache_clear()

    def test_vc_dump_capability_degrades_for_legacy_compiler(self):
        legacy_help = mock.Mock(stdout="usage: ocamlc", stderr="")
        current_help = mock.Mock(stdout="-vox-dump-vc-json <file>", stderr="")
        compiler_adapter.supports_vc_dump.cache_clear()
        with mock.patch.object(
            compiler_adapter.subprocess,
            "run",
            side_effect=[legacy_help, current_help],
        ):
            self.assertFalse(compiler_adapter.supports_vc_dump("/legacy/ocamlc"))
            self.assertTrue(compiler_adapter.supports_vc_dump("/current/ocamlc"))
        compiler_adapter.supports_vc_dump.cache_clear()

    def test_type_only_is_advertised_only_when_compiler_help_supports_it(self):
        compiler_adapter.backend_options.cache_clear()
        current_help = mock.Mock(
            stdout="-vox-backend {lean|z3|oxsmt|cross}\n-vox-type-only",
            stderr="",
        )
        legacy_help = mock.Mock(stdout="usage: ocamlc", stderr="")
        with mock.patch.object(
            compiler_adapter.subprocess,
            "run",
            side_effect=[current_help, legacy_help],
        ):
            self.assertEqual(
                compiler_adapter.backend_options("/current/ocamlc"),
                ("lean", "z3", "oxsmt", "cross", "none"),
            )
            self.assertEqual(
                compiler_adapter.backend_options("/legacy/ocamlc"),
                ("lean",),
            )
        compiler_adapter.backend_options.cache_clear()

    def test_type_only_check_uses_flag_and_never_requests_vc_dump(self):
        calls = []

        def completed(ocamlc, arguments, scratch, **kwargs):
            calls.append(arguments)
            return compiler_adapter.subprocess.CompletedProcess(
                [ocamlc, *arguments], 0, "", ""
            )

        with mock.patch.object(
            compiler_adapter,
            "backend_options",
            return_value=("lean", "none"),
        ), mock.patch.object(compiler_adapter, "_run", side_effect=completed):
            payload = compiler_adapter.check_source(
                "let x = 1", 9, "/current/ocamlc", "none"
            )
        self.assertEqual(calls, [["-vox-type-only", "-c", "-annot", "input.ml"]])
        self.assertTrue(payload["ok"])
        self.assertEqual(payload["outcome"]["kind"], "checked-no-verification")
        self.assertEqual(payload["verification"]["status"], "not-run")
        self.assertTrue(payload["unavailable"])
        self.assertEqual(payload["unavailable_reason"], "verification-not-run")
        self.assertEqual(payload["vcs"], [])


class ParserTests(unittest.TestCase):
    def test_parse_single_line_location_is_zero_based(self):
        location = compiler_adapter.parse_loc(
            'File "input.ml", line 4, characters 14-18:',
            ["", "", "", " " * 18],
        )
        self.assertEqual(
            location,
            ({"line": 3, "col": 14}, {"line": 3, "col": 18}),
        )

    def test_parse_multiline_location_is_zero_based(self):
        location = compiler_adapter.parse_loc(
            'File "input.ml", lines 2-4, characters 3-9:',
            ["", " " * 3, "", " " * 9],
        )
        self.assertEqual(
            location,
            ({"line": 1, "col": 3}, {"line": 3, "col": 9}),
        )

    def test_parse_type_error_and_continuation(self):
        output = """File \"input.ml\", line 8, characters 14-18:
8 | let broken = (true : int{ _ > 0 })
                  ^^^^
Error: The constructor true has type bool
        but an expression was expected of type int
"""
        source_lines = [""] * 7 + ["let broken = (true : int{ _ > 0 })"]
        self.assertEqual(
            compiler_adapter.parse_any_error(output, source_lines),
            {
                "message": (
                    "The constructor true has type bool but an expression "
                    "was expected of type int"
                ),
                "start": {"line": 7, "col": 14},
                "end": {"line": 7, "col": 18},
            },
        )

    def test_parse_annotation(self):
        annotation = """\"input.ml\" 2 32 36 \"input.ml\" 2 32 41
type(
  int{ (app[Stdlib!.>=] _ 3) }
)
"""
        self.assertEqual(
            compiler_adapter.parse_annot(annotation, ["", " " * 9]),
            [
                {
                    "start": {"line": 1, "col": 4},
                    "end": {"line": 1, "col": 9},
                    "type": "int{ (app[Stdlib!.>=] _ 3) }",
                }
            ],
        )

    def test_non_ascii_byte_columns_become_utf16_columns(self):
        line = 'let x = ("日本語😀", true)'
        start_index = line.index("true")
        end_index = start_index + len("true")
        start_byte = len(line[:start_index].encode("utf-8"))
        end_byte = len(line[:end_index].encode("utf-8"))
        start_utf16 = len(line[:start_index].encode("utf-16-le")) // 2
        end_utf16 = len(line[:end_index].encode("utf-16-le")) // 2

        location = compiler_adapter.parse_loc(
            (f'File "input.ml", line 1, characters {start_byte}-{end_byte}:'),
            [line],
        )
        self.assertEqual(
            location,
            (
                {"line": 0, "col": start_utf16},
                {"line": 0, "col": end_utf16},
            ),
        )

        annotation = (
            f'"input.ml" 1 0 {start_byte} "input.ml" 1 0 {end_byte}\ntype(\n  bool\n)\n'
        )
        annotation_location = compiler_adapter.parse_annot(annotation, [line])[0]
        self.assertEqual(
            (annotation_location["start"], annotation_location["end"]),
            (
                {"line": 0, "col": start_utf16},
                {"line": 0, "col": end_utf16},
            ),
        )

    def test_long_multiline_diagnostic_keeps_location(self):
        source_lines = ["let f ="] + ["  fun x ->"] * 10 + ["  0"]
        excerpt = "\n".join(
            f"{line_number:2} | {line}"
            for line_number, line in enumerate(source_lines, start=1)
        )
        output = (
            'File "input.ml", lines 1-12, characters 4-3:\n'
            f"{excerpt}\n"
            "Error: This expression has type int -> int\n"
            "       but an expression was expected of type int\n"
        )

        error = compiler_adapter.parse_any_error(output, source_lines)
        assert error is not None
        self.assertEqual(error["start"], {"line": 0, "col": 4})
        self.assertEqual(error["end"], {"line": 11, "col": 3})

    def test_warning_as_error_keeps_location(self):
        output = """File "input.ml", line 1, characters 4-5:
1 | let x = 1
        ^
Error (warning 26 [unused-var]): unused variable x.
"""
        error = compiler_adapter.parse_any_error(output, ["let x = 1"])
        assert error is not None
        self.assertEqual(error["start"], {"line": 0, "col": 4})
        self.assertIn("unused variable", error["message"])

    def test_extract_signature_trims_transport_whitespace(self):
        self.assertEqual(
            compiler_adapter.extract_signature("\nval answer : int\n\n"),
            "val answer : int",
        )


class RealCompilerTests(unittest.TestCase):
    ocamlc: str = ""

    @classmethod
    def setUpClass(cls):
        found = compiler_adapter.find_ocamlc()
        if found is None:
            raise unittest.SkipTest("vox2 compiler is not built in this worktree")
        cls.ocamlc = found

    def test_good_refinements_produce_types_and_signature(self):
        source = """type positive = int{ _ > 0 }
let three : int{ _ >= 3 } = 5
let f (x : int{ _ > 0 }) = x
"""
        response = compiler_adapter.check_source(source, 7, self.ocamlc)
        self.assertTrue(response["ok"])
        self.assertEqual(response["errors"], [])
        # The refinement predicate prints source-like in the inferred
        # signature (the display fix), not the raw app-syntax it desugars to.
        self.assertEqual(response["signature"]["status"], "not-requested")
        signature = compiler_adapter.signature_for_source(
            source, 7, self.ocamlc
        )["signature"]
        self.assertIn("val three : int{ _ >= 3 }", signature["text"])
        self.assertTrue(response["types"])
        # The same check response carries the proof-pane data from its -c pass;
        # the client needs no racing /vcs compile.
        self.assertFalse(response["unavailable"])
        self.assertTrue(response["vcs"])

    def test_fib_body_imposition_carries_checked_and_imposed_types(self):
        source = (
            "let rec fib (n : int{ _ >= 0 }) : int{ _ >= 0 } =\n"
            "  if n <= 1 then n else fib (n - 1) + fib (n - 2)\n"
        )
        response = compiler_adapter.check_source(source, 17, self.ocamlc)
        self.assertEqual(
            response["imposed_types"],
            [
                {
                    "start": {"line": 1, "col": 2},
                    "end": {"line": 1, "col": 49},
                    "checked_type": "int",
                    "imposed_type": "int{ _ >= 0 }",
                }
            ],
        )

    def test_bad_skeleton_type_has_real_span(self):
        source = "let broken = (true : int{ _ > 0 })\n"
        response = compiler_adapter.check_source(source, 8, self.ocamlc)
        self.assertFalse(response["ok"])
        self.assertEqual(response["revision"], 8)
        error = response["errors"][0]
        self.assertEqual(error["start"], {"line": 0, "col": 14})
        self.assertEqual(error["end"], {"line": 0, "col": 18})
        self.assertIn("true has type bool", error["message"])
        # A skeleton type error is a type error, and it blocks verification.
        self.assertEqual(error["kind"], "type-mode")
        self.assertEqual(response["verification"]["status"], "blocked")

    def test_provable_refinement_verifies_clean(self):
        source = """let three : int{ _ >= 3 } = 5
let f (x : int{ _ > 0 }) = x
let seven = f 7
"""
        response = compiler_adapter.check_source(source, 1, self.ocamlc)
        self.assertTrue(response["ok"], response["errors"])
        self.assertEqual(response["verification"]["status"], "verified")

    def test_false_annotation_is_a_verification_failure(self):
        source = "let f (x : int{ _ > 0 }) = x\nlet bad = f 0\n"
        response = compiler_adapter.check_source(source, 2, self.ocamlc)
        self.assertFalse(response["ok"])
        self.assertEqual(response["verification"]["status"], "failed")
        error = response["errors"][0]
        self.assertEqual(error["kind"], "verification")
        self.assertIn("Refinement verification failed", error["message"])
        # A verification failure is still located in the buffer.
        self.assertIn("start", error)

    def test_unrefined_program_has_nothing_to_verify(self):
        response = compiler_adapter.check_source("let x = 1\n", 3, self.ocamlc)
        self.assertTrue(response["ok"])
        self.assertEqual(response["verification"]["status"], "none")

    def test_index_operator_is_not_falsely_verified(self):
        # `val ( .%{} ) : int -> int -> int` has braces in the operator name,
        # not a refinement: it compiles clean but must read "none", never
        # "verified".
        source = "let ( .%{} ) c i = c + i\n"
        response = compiler_adapter.check_source(source, 5, self.ocamlc)
        self.assertTrue(response["ok"], response["errors"])
        signature = compiler_adapter.signature_for_source(
            source, 5, self.ocamlc
        )["signature"]
        self.assertIn("{", signature["text"])
        self.assertEqual(response["verification"]["status"], "none")

    def test_impure_predicate_is_rejected(self):
        source = "let bad = (read_int () : int{ _ = read_int () })\n"
        response = compiler_adapter.check_source(source, 4, self.ocamlc)
        self.assertFalse(response["ok"])
        # Totality rejection happens during elaboration: a type/mode error,
        # not a Lean discharge failure.
        self.assertEqual(response["errors"][0]["kind"], "type-mode")


class ClassificationTests(unittest.TestCase):
    def test_error_kind_splits_verification_from_type(self):
        self.assertEqual(
            compiler_adapter._error_kind("Refinement verification failed (not proved)"),
            "verification",
        )
        self.assertEqual(
            compiler_adapter._error_kind("The constructor true has type bool"),
            "type-mode",
        )

    def test_has_refinements_detects_hole_but_not_records(self):
        refined = [{"type": "int{ (app[Stdlib!.>=] _ 3) }"}]
        record = [{"type": "{ field : int; other : bool }"}]
        self.assertTrue(compiler_adapter._has_refinements("", refined))
        self.assertFalse(compiler_adapter._has_refinements("", record))
        self.assertTrue(compiler_adapter._has_refinements("val x : int{ _ > 0 }", []))

    def test_has_refinements_ignores_operator_name_braces(self):
        # An index-operator binding carries braces in its *name*, not a
        # refinement in its type; it must not read as "verified".
        self.assertFalse(
            compiler_adapter._has_refinements(
                "val ( .@{} ) : 'a array -> int -> 'a", []
            )
        )
        # A refinement in the type of such a binding still counts.
        self.assertTrue(
            compiler_adapter._has_refinements(
                "val ( .@{} ) : 'a array -> int{ _ >= 0 } -> 'a", []
            )
        )

    def test_verification_status_transitions(self):
        verify_error = [
            {"kind": "verification", "message": "Refinement verification failed (x)"}
        ]
        type_error = [{"kind": "type", "message": "boom"}]
        self.assertEqual(
            compiler_adapter._verification_status(verify_error, "", [])["status"],
            "failed",
        )
        self.assertEqual(
            compiler_adapter._verification_status(type_error, "", [])["status"],
            "blocked",
        )
        self.assertEqual(
            compiler_adapter._verification_status([], "val x : int{ _ > 0 }", [])[
                "status"
            ],
            "verified",
        )
        self.assertEqual(
            compiler_adapter._verification_status([], "val x : int", [])["status"],
            "none",
        )


class AdapterPolishTests(unittest.TestCase):
    def test_spanless_unproved_vc_is_counted_in_authoritative_summary(self):
        document = {
            "schema_version": 2,
            "verification_conditions": [
                {
                    "kind": "annotation",
                    "location": {"ghost": True},
                    "program_point": {"ghost": True},
                    "goal": {"text": "false"},
                    "discharge": {"status": "not-proved"},
                }
            ],
        }
        with tempfile.TemporaryDirectory() as scratch:
            path = os.path.join(scratch, "vcs.json")
            with open(path, "w") as output:
                json.dump(document, output)
            payload = compiler_adapter._vcs_from_dump_path(
                compiler_adapter.Path(path), ["let x = 1"], 0, 3, "lean", ("lean",)
            )
        self.assertFalse(payload["unavailable"])
        self.assertEqual(payload["vcs"], [])
        self.assertEqual(payload["hidden"], 1)
        self.assertEqual(payload["obligation_summary"]["total"], 1)
        self.assertEqual(
            payload["obligation_summary"]["statuses"]["unproved"], 1
        )

    def test_malformed_vc_anchor_is_hidden_never_defaulted_to_zero(self):
        malformed = (
            {},
            {"file": "input.ml", "ghost": False},
            {
                "file": "input.ml",
                "ghost": False,
                "start": {"line": 1, "column": 4},
                "end": {"line": 1, "column": 2},
            },
            {
                "file": "input.ml",
                "ghost": False,
                "start": {"line": 1, "column": 0},
                "end": {"line": 1, "column": 99},
            },
        )
        for index, location in enumerate(malformed):
            with self.subTest(anchor=index), tempfile.TemporaryDirectory() as scratch:
                path = os.path.join(scratch, "vcs.json")
                with open(path, "w") as output:
                    json.dump(
                        {
                            "verification_conditions": [
                                {
                                    "kind": "annotation",
                                    "location": location,
                                    "discharge": {"status": "not-proved"},
                                    "goal": {"text": "false"},
                                }
                            ]
                        },
                        output,
                    )
                payload = compiler_adapter._vcs_from_dump_path(
                    compiler_adapter.Path(path),
                    ["let x = 1"],
                    0,
                    3,
                    "lean",
                    ("lean",),
                )
            self.assertEqual(payload["vcs"], [])
            self.assertEqual(payload["hidden"], 1)
            self.assertEqual(
                payload["obligation_summary"]["statuses"]["unproved"], 1
            )

    def test_primary_check_never_runs_signature_inference(self):
        calls = []

        def fake_run(ocamlc, arguments, scratch, **kwargs):
            calls.append(list(arguments))
            with open(os.path.join(scratch, "vcs.json"), "w") as output:
                json.dump(
                    {"schema_version": 2, "verification_conditions": []},
                    output,
                )
            return compiler_adapter.subprocess.CompletedProcess(
                [ocamlc, *arguments], 0, "", ""
            )

        with mock.patch.object(compiler_adapter, "_run", side_effect=fake_run), mock.patch.object(
            compiler_adapter, "backend_options", return_value=("lean",)
        ), mock.patch.object(compiler_adapter, "supports_vc_dump", return_value=True):
            response = compiler_adapter.check_source("let x = 1", 4, "/compiler")
        self.assertEqual(len(calls), 1)
        self.assertIn("-c", calls[0])
        self.assertNotIn("-i", calls[0])
        self.assertEqual(response["outcome"]["kind"], "ok")
        self.assertEqual(response["signature"]["status"], "not-requested")

    def test_failed_signature_is_presentation_only(self):
        failed = compiler_adapter.subprocess.CompletedProcess(
            ["/compiler", "-i", "input.ml"], 2, "", "signature failed"
        )
        with mock.patch.object(compiler_adapter, "_run", return_value=failed), mock.patch.object(
            compiler_adapter, "backend_options", return_value=("lean",)
        ):
            payload = compiler_adapter.signature_for_source(
                "let x = 1", 8, "/compiler"
            )
        self.assertEqual(payload["revision"], 8)
        self.assertEqual(payload["signature"]["status"], "unavailable")
        self.assertIn("signature failed", payload["signature"]["error"])

    def test_spawn_timeout_and_crash_have_distinct_outcomes(self):
        options = mock.patch.object(
            compiler_adapter, "backend_options", return_value=("lean",)
        )
        dump = mock.patch.object(compiler_adapter, "supports_vc_dump", return_value=True)
        with options, dump, mock.patch.object(
            compiler_adapter,
            "_run",
            side_effect=compiler_adapter.subprocess.TimeoutExpired(["ocamlc"], 60),
        ):
            timed_out = compiler_adapter.check_source("x", 1, "/compiler")
        with options, dump, mock.patch.object(
            compiler_adapter, "_run", side_effect=OSError("missing")
        ):
            missing = compiler_adapter.check_source("x", 1, "/compiler")
        crashed_process = compiler_adapter.subprocess.CompletedProcess(
            ["/compiler"], 2, "", "Fatal error: boom"
        )
        with options, dump, mock.patch.object(
            compiler_adapter, "_run", return_value=crashed_process
        ):
            crashed = compiler_adapter.check_source("x", 1, "/compiler")
        self.assertEqual(timed_out["outcome"]["kind"], "timeout")
        self.assertEqual(timed_out["unavailable_reason"], "timeout")
        self.assertEqual(missing["outcome"]["kind"], "compiler-unavailable")
        self.assertEqual(missing["unavailable_reason"], "compiler-unavailable")
        self.assertEqual(crashed["outcome"]["kind"], "compiler-crashed")
        self.assertEqual(crashed["unavailable_reason"], "compiler-crashed")

    def test_cancel_kills_compiler_process_group(self):
        with tempfile.TemporaryDirectory() as scratch:
            script = os.path.join(scratch, "compiler.sh")
            pids = os.path.join(scratch, "pids")
            with open(script, "w") as output:
                output.write(
                    "#!/bin/sh\n"
                    f"echo $$ > {pids}\n"
                    "sleep 30 &\n"
                    f"echo $! >> {pids}\n"
                    "wait\n"
                )
            os.chmod(script, os.stat(script).st_mode | stat.S_IEXEC)
            started = time.monotonic()
            with self.assertRaises(compiler_adapter.CompileCancelled):
                compiler_adapter._run(
                    script,
                    [],
                    scratch,
                    cancel_check=lambda: time.monotonic() - started > 0.2,
                )
            with open(pids) as input_file:
                process_ids = [int(line) for line in input_file]
            for _ in range(20):
                states = [
                    compiler_adapter.subprocess.run(
                        ["ps", "-o", "stat=", "-p", str(pid)],
                        stdout=compiler_adapter.subprocess.PIPE,
                        encoding="utf-8",
                        check=False,
                    ).stdout.strip()
                    for pid in process_ids
                ]
                if all(not state or state.startswith("Z") for state in states):
                    break
                time.sleep(0.05)
            self.assertTrue(
                all(not state or state.startswith("Z") for state in states), states
            )


def _span_v1(line1, col0, col1, ghost=False):
    return {
        "file": "input.ml",
        "start": {"line": line1, "column": col0},
        "end": {"line": line1, "column": col1},
        "ghost": ghost,
    }


class TranslateVcTests(unittest.TestCase):
    """Pure schema-v1 -> flat-frontend-shape translation, no compiler."""

    # Wide lines so a byte column maps to itself under the UTF-16 conversion.
    SOURCE_LINES = ["x" * 60] * 14

    def _maybe(self, **overrides):
        vc = {
            "location": _span_v1(11, 21, 22),
            "program_point": _span_v1(11, 12, 22),
            "kind": "contract-argument",
            "goal": {
                "text": "(app[Stdlib!.>]\n   7 0)",
                "source_span": _span_v1(8, 23, 28),
            },
            "facts": [{"text": "(app a\n  b)", "source_span": None}],
            "discharge": {"status": "proved", "detail": None, "counterexample": None},
            "generated_lean": "theorem vc_0 : ...",
        }
        vc.update(overrides)
        return compiler_adapter.translate_vc(vc, 0, self.SOURCE_LINES)

    def _one(self, **overrides):
        result = self._maybe(**overrides)
        assert result is not None
        return result

    def test_location_is_the_anchor_span_zero_based(self):
        # location (the constrained value/subterm) is preferred over
        # program_point (the enclosing site); 1-based line, 0-based byte col ->
        # 0-based editor span.
        result = self._one()
        assert result is not None
        self.assertEqual(result["span"]["start"], {"line": 10, "col": 21})
        self.assertEqual(result["span"]["end"], {"line": 10, "col": 22})

    def test_kind_and_status_are_mapped(self):
        self.assertEqual(self._one()["kind"], "contract")
        self.assertEqual(self._one(kind="annotation")["kind"], "annotation")
        self.assertEqual(self._one(kind="seal-implication")["kind"], "seal")
        for raw, shown in [
            ("proved", "proved"),
            ("disproved", "disproved"),
            ("not-proved", "unproved"),
            ("solver-error", "solver-error"),
            ("unknown", "unknown"),
            ("unavailable", "unavailable"),
        ]:
            got = self._one(
                discharge={"status": raw, "detail": None, "counterexample": None}
            )
            self.assertEqual(got["status"], shown)
        # An unrecognized discharge status maps to the "unknown" sentinel (the
        # frontend then fails it closed to an anomaly, never "not yet checked").
        weird = self._one(
            discharge={"status": "surprise", "detail": None, "counterexample": None}
        )
        self.assertEqual(weird["status"], "unknown")

    def test_v1_goal_and_facts_fall_back_to_raw_text(self):
        # Schema v1 has no display/origin: display falls back to raw text
        # (whitespace preserved, not collapsed), hypotheses are positional
        # (name None) and have no clickable span.  It carries neither of the
        # two later provenance fields either, and both stay explicitly None:
        # a consumer asking who introduced a fact, or which backend read it,
        # has to see "not reported" rather than an absent key it could read
        # as an empty answer.
        result = self._one()
        self.assertEqual(
            result["goal"],
            {"display": "(app[Stdlib!.>]\n   7 0)", "raw": "(app[Stdlib!.>]\n   7 0)"},
        )
        self.assertEqual(
            result["hypotheses"],
            [
                {
                    "name": None,
                    "kind": None,
                    "display": "(app a\n  b)",
                    "raw": "(app a\n  b)",
                    "span": None,
                    "producers": None,
                    "used_by": None,
                }
            ],
        )

    def test_v2_display_and_origin_are_consumed(self):
        # Schema v2: the pretty display drives the pane, raw is kept, and a
        # fact's origin becomes a named, source-linked hypothesis.
        result = self._one(
            goal={
                "text": "(app[Stdlib!.>] _ 0)",
                "display": "_ > 0",
                "source_span": _span_v1(8, 23, 28),
            },
            facts=[
                {
                    "text": "(app[Stdlib!.>=] n 0)",
                    "display": "n >= 0",
                    "origin": {"name": "n", "span": _span_v1(11, 12, 22)},
                },
                # An origin whose span is ghost is not clickable.
                {
                    "text": "(app a b)",
                    "display": "a b",
                    "origin": {"name": "h", "span": _span_v1(11, 12, 22, ghost=True)},
                },
                # No origin at all -> positional, unlinked.
                {"text": "(app c d)", "display": "c d"},
            ],
        )
        self.assertEqual(
            result["goal"], {"display": "_ > 0", "raw": "(app[Stdlib!.>] _ 0)"}
        )
        hyps = result["hypotheses"]
        self.assertEqual(hyps[0]["name"], "n")
        self.assertEqual(hyps[0]["display"], "n >= 0")
        self.assertEqual(hyps[0]["raw"], "(app[Stdlib!.>=] n 0)")
        self.assertEqual(
            hyps[0]["span"],
            {"start": {"line": 10, "col": 12}, "end": {"line": 10, "col": 22}},
        )
        self.assertEqual(hyps[1]["name"], "h")
        self.assertIsNone(hyps[1]["span"])
        self.assertIsNone(hyps[2]["name"])
        self.assertIsNone(hyps[2]["span"])

    def test_counterexample_and_detail_and_lean_are_surfaced(self):
        got = self._one(
            discharge={
                "status": "disproved",
                "detail": "grind failed",
                "counterexample": "x = 0",
            }
        )
        self.assertEqual(got["counterexample"], ["x = 0"])
        self.assertEqual(got["detail"], "grind failed")
        self.assertEqual(got["generated_lean"], "theorem vc_0 : ...")
        # A null counterexample stays null; a null lean stays null.
        plain = self._one(generated_lean=None)
        self.assertIsNone(plain["counterexample"])
        self.assertIsNone(plain["generated_lean"])

    def test_cross_backend_results_are_ordered_and_capability_aware(self):
        got = self._one(
            discharge={
                "status": "solver-error",
                "detail": "cross-check failed",
                "counterexample": None,
                "backends": [
                    {
                        "backend": "lean",
                        "status": "proved",
                        "detail": None,
                        "fact_usage": True,
                    },
                    {
                        "backend": "z3",
                        "status": "unavailable",
                        "detail": "not installed",
                        "fact_usage": False,
                    },
                    {
                        "backend": "oxsmt",
                        "status": "unknown",
                        "detail": "nonlinear",
                        "fact_usage": False,
                    },
                ],
            }
        )
        self.assertEqual(
            [(r["backend"], r["status"]) for r in got["backends"]],
            [("lean", "proved"), ("z3", "unavailable"), ("oxsmt", "unknown")],
        )
        self.assertTrue(got["backends"][0]["fact_usage"])
        self.assertFalse(got["backends"][1]["fact_usage"])

    def test_missing_fact_usage_stays_absent(self):
        missing = self._one(facts=[{"text": "p"}])["hypotheses"][0]
        explicit = self._one(facts=[{"text": "p", "used": False}])["hypotheses"][0]
        self.assertNotIn("used", missing)
        self.assertIs(explicit["used"], False)

    def test_ghost_anchor_falls_back_then_drops(self):
        # location ghost -> falls back to the non-ghost program_point (the
        # annotation case, whose location is synthesized/ghost).
        fell_back = self._one(location=_span_v1(11, 21, 22, ghost=True))
        self.assertEqual(fell_back["span"]["start"], {"line": 10, "col": 12})
        # Every candidate ghost -> the VC has no placeable mark and is dropped.
        dropped = self._maybe(
            program_point=_span_v1(11, 12, 22, ghost=True),
            location=_span_v1(11, 21, 22, ghost=True),
            goal={"text": "g", "source_span": _span_v1(8, 23, 28, ghost=True)},
        )
        self.assertIsNone(dropped)


class LemmaCallChannelTests(unittest.TestCase):
    """The producer/usage channel a lemma-call marker is decided from."""

    SOURCE_LINES = ["x" * 60] * 14

    def _document(self, entries):
        return {"lemma_calls": entries}

    def _calls(self, entries):
        return compiler_adapter.lemma_calls(
            self._document(entries),
            {"input.ml": self.SOURCE_LINES},
            expected_file="input.ml",
        )

    def _entry(self, **overrides):
        entry = {
            "span": _span_v1(5, 11, 23),
            "name": "some_law",
            "introduced": True,
        }
        entry.update(overrides)
        return entry

    def test_absent_channel_is_unknown_not_empty(self):
        # An older compiler that never names such a call is not a buffer that
        # holds none: the two must not collapse into the same value.
        self.assertIsNone(
            compiler_adapter.lemma_calls(
                {}, {"input.ml": self.SOURCE_LINES}, expected_file="input.ml"
            )
        )
        self.assertEqual(self._calls([]), [])

    def test_entries_convert_to_editor_coordinates(self):
        calls = self._calls([self._entry()])
        assert calls is not None
        self.assertEqual(len(calls), 1)
        self.assertEqual(calls[0]["start"], {"line": 4, "col": 11})
        self.assertEqual(calls[0]["end"], {"line": 4, "col": 23})
        self.assertEqual(calls[0]["name"], "some_law")
        self.assertTrue(calls[0]["introduced"])

    def test_one_unplaceable_entry_makes_the_whole_channel_unknown(self):
        # A call the editor cannot point at is a call it cannot decide, and a
        # partial list would silently shrink into a confident answer.
        ghost = self._entry(span={**_span_v1(5, 11, 23), "ghost": "true"})
        self.assertIsNone(self._calls([self._entry(), ghost]))

    def test_missing_introduced_flag_makes_the_channel_unknown(self):
        entry = self._entry()
        del entry["introduced"]
        self.assertIsNone(self._calls([entry]))
        self.assertIsNone(self._calls([self._entry(introduced="yes")]))

    def test_producers_are_reported_per_fact(self):
        fact = {
            "text": "p",
            "producers": [
                {
                    "kind": "application",
                    "name": "some_law",
                    "span": _span_v1(5, 11, 23),
                },
                {
                    "kind": "application",
                    "name": "some_law",
                    "span": _span_v1(6, 11, 23),
                },
            ],
        }
        hypothesis = compiler_adapter._hypothesis(fact, self.SOURCE_LINES)
        producers = hypothesis["producers"]
        assert producers is not None
        self.assertEqual(
            [p["span"]["start"] for p in producers],
            [{"line": 4, "col": 11}, {"line": 5, "col": 11}],
        )

    def test_absent_or_unplaceable_producers_are_unknown(self):
        # Falling back to `origin` here would name one introducer of a fact
        # several sites introduced, which is how a needed call gets called
        # unnecessary.
        self.assertIsNone(
            compiler_adapter._hypothesis({"text": "p"}, self.SOURCE_LINES)[
                "producers"
            ]
        )
        ghost = {
            "text": "p",
            "producers": [
                {"kind": "application", "span": {**_span_v1(5, 11, 23), "ghost": "true"}}
            ],
        }
        self.assertIsNone(
            compiler_adapter._hypothesis(ghost, self.SOURCE_LINES)["producers"]
        )

    def test_per_backend_usage_keeps_a_silent_backend_silent(self):
        discharge = {
            "status": "proved",
            "backends": [
                {"backend": "lean", "status": "proved", "unused_facts": [1]},
                {"backend": "z3", "status": "proved", "unused_facts": []},
                {"backend": "oxsmt", "status": "unknown"},
            ],
        }
        per_backend = compiler_adapter._backend_unused_facts(discharge)
        assert per_backend is not None
        self.assertEqual(per_backend["lean"], [1])
        self.assertEqual(per_backend["z3"], [])
        self.assertIsNone(per_backend["oxsmt"])
        # Fact 0: read by lean and by z3; oxsmt reported nothing, so it is
        # absent rather than recorded as having left the fact unread.
        self.assertEqual(
            compiler_adapter._used_by(0, per_backend), {"lean": True, "z3": True}
        )
        # Fact 1: lean left it unread, z3 read it.
        self.assertEqual(
            compiler_adapter._used_by(1, per_backend), {"lean": False, "z3": True}
        )

    def test_single_backend_run_reports_no_per_backend_usage(self):
        self.assertIsNone(compiler_adapter._backend_unused_facts({"status": "proved"}))
        self.assertIsNone(compiler_adapter._used_by(0, None))

    def test_malformed_unused_indices_are_treated_as_no_accounting(self):
        discharge = {
            "backends": [
                {"backend": "z3", "status": "proved", "unused_facts": ["1"]},
                {"backend": "lean", "status": "proved", "unused_facts": [True]},
            ]
        }
        per_backend = compiler_adapter._backend_unused_facts(discharge)
        assert per_backend is not None
        self.assertIsNone(per_backend["z3"])
        self.assertIsNone(per_backend["lean"])
        self.assertIsNone(compiler_adapter._used_by(0, per_backend))


class RefinementTypesTests(unittest.TestCase):
    """Pure translation of schema-v2 refinement_expression_types, no compiler."""

    # Wide lines so a byte column maps to itself under the UTF-16 conversion.
    SOURCE_LINES = ["x" * 60] * 4

    def _entry(self, col0, col1, type_text, ghost=False):
        return {"location": _span_v1(1, col0, col1, ghost=ghost), "type": type_text}

    def test_entries_become_zero_based_editor_ranges(self):
        # 1-based line / 0-based byte col in the dump -> 0-based editor span;
        # the {start,end,type} shape matches parse_annot so the client folds
        # both into one cursor lookup.
        document = {
            "refinement_expression_types": [
                self._entry(16, 21, "bool"),
                self._entry(16, 17, "int"),
            ]
        }
        got = compiler_adapter.refinement_types(document, self.SOURCE_LINES)
        self.assertEqual(
            got,
            [
                {
                    "start": {"line": 0, "col": 16},
                    "end": {"line": 0, "col": 21},
                    "type": "bool",
                },
                {
                    "start": {"line": 0, "col": 16},
                    "end": {"line": 0, "col": 17},
                    "type": "int",
                },
            ],
        )

    def test_ghost_and_malformed_entries_are_dropped(self):
        # A ghost span cannot be placed; an entry with no type is not a type;
        # both are dropped (honesty: only a placeable, emitted type is shown).
        document = {
            "refinement_expression_types": [
                self._entry(16, 17, "int", ghost=True),
                self._entry(18, 19, ""),
                {"type": "int"},  # no location
                "not a dict",
                self._entry(20, 21, "int"),
            ]
        }
        got = compiler_adapter.refinement_types(document, self.SOURCE_LINES)
        self.assertEqual([r["type"] for r in got], ["int"])
        self.assertEqual(got[0]["start"], {"line": 0, "col": 20})

    def test_missing_or_non_list_field_is_empty(self):
        # The field is optional (omitted for a buffer with no refinements) and
        # must never crash the adapter.
        self.assertEqual(compiler_adapter.refinement_types({}, self.SOURCE_LINES), [])
        self.assertEqual(
            compiler_adapter.refinement_types(
                {"refinement_expression_types": None}, self.SOURCE_LINES
            ),
            [],
        )
        self.assertEqual(
            compiler_adapter.refinement_types("not a dict", self.SOURCE_LINES), []
        )

    def test_workspace_ranges_keep_file_identity_at_identical_coordinates(self):
        left = self._entry(4, 5, "int")
        left["location"]["file"] = "A.ml"
        right = self._entry(4, 5, "bool")
        right["location"]["file"] = "B.ml"
        ghost = self._entry(4, 5, "string", ghost=True)
        ghost["location"]["file"] = "A.ml"
        foreign = self._entry(4, 5, "float")
        foreign["location"]["file"] = "Closed.ml"
        got = compiler_adapter.refinement_types_by_file(
            {"refinement_expression_types": [left, right, ghost, foreign]},
            {"A.ml": self.SOURCE_LINES, "B.ml": self.SOURCE_LINES},
        )
        self.assertEqual(
            [(entry["file"], entry["type"]) for entry in got],
            [("A.ml", "int"), ("B.ml", "bool")],
        )


class ImposedTypesTests(unittest.TestCase):
    """Join only exact schema-v2 annotation facts; never parse type text."""

    SOURCE = "if p then n else n + 1       _ >= 0"
    SOURCE_LINES = [SOURCE]
    ANCHOR = _span_v1(1, 0, 23)
    PREDICATE = _span_v1(1, 29, 35)
    HOLE = _span_v1(1, 29, 30)

    def _document(self):
        return {
            "schema_version": 2,
            "verification_conditions": [
                {
                    "kind": "annotation",
                    "goal": {"source_span": self.PREDICATE},
                    "provenance": {
                        "kind": "annotation",
                        "source_span": self.ANCHOR,
                        "related_spans": [
                            {"role": "subject", "span": _span_v1(1, 10, 11)}
                        ],
                    },
                }
            ],
            "refinement_expression_types": [
                {"location": self.PREDICATE, "type": "bool"},
                {"location": self.HOLE, "type": "int"},
            ],
        }

    def _expression_types(self):
        return [
            {
                "start": {"line": 0, "col": 0},
                "end": {"line": 0, "col": 23},
                "type": "int{ _ >= 0 }",
            }
        ]

    def _join(self, document):
        return compiler_adapter.imposed_types(
            document,
            self.SOURCE_LINES,
            self._expression_types(),
            expected_file="input.ml",
        )

    def test_exact_emitted_join_establishes_both_types(self):
        self.assertEqual(
            self._join(self._document()),
            [
                {
                    "start": {"line": 0, "col": 0},
                    "end": {"line": 0, "col": 23},
                    "checked_type": "int",
                    "imposed_type": "int{ _ >= 0 }",
                }
            ],
        )

    def test_legacy_missing_and_ambiguous_data_fail_closed(self):
        legacy = self._document()
        legacy["schema_version"] = 1
        self.assertEqual(self._join(legacy), [])
        no_annot = self._document()
        self.assertEqual(
            compiler_adapter.imposed_types(
                no_annot,
                self.SOURCE_LINES,
                [],
                expected_file="input.ml",
            ),
            [],
        )
        ambiguous = self._document()
        ambiguous["refinement_expression_types"].append(
            {"location": self.HOLE, "type": "string"}
        )
        self.assertEqual(
            self._join(ambiguous),
            [],
        )

    def test_missing_anchor_endpoints_fail_closed(self):
        for missing in ("start", "end"):
            with self.subTest(missing=missing):
                document = copy.deepcopy(self._document())
                anchor = document["verification_conditions"][0]["provenance"][
                    "source_span"
                ]
                del anchor[missing]
                self.assertEqual(self._join(document), [])

        document = copy.deepcopy(self._document())
        document["verification_conditions"][0]["provenance"][
            "source_span"
        ] = {"file": "input.ml", "ghost": False}
        self.assertEqual(self._join(document), [])

        document = copy.deepcopy(self._document())
        del document["verification_conditions"][0]["provenance"][
            "source_span"
        ]["file"]
        self.assertEqual(self._join(document), [])

    def test_every_joined_span_must_name_the_expected_file(self):
        mutations = (
            lambda doc: doc["verification_conditions"][0]["provenance"][
                "source_span"
            ],
            lambda doc: doc["verification_conditions"][0]["provenance"][
                "related_spans"
            ][0]["span"],
            lambda doc: doc["verification_conditions"][0]["goal"][
                "source_span"
            ],
            lambda doc: doc["refinement_expression_types"][1]["location"],
        )
        for index, span_of in enumerate(mutations):
            with self.subTest(span=index):
                document = copy.deepcopy(self._document())
                span_of(document)["file"] = "foreign.ml"
                self.assertEqual(self._join(document), [])

    def test_malformed_subject_span_fails_closed(self):
        malformed = (
            {"file": "input.ml", "ghost": False},
            _span_v1(1, 10, 11, ghost="false"),
            _span_v1(1, 10, 11, ghost=None),
        )
        for index, subject in enumerate(malformed):
            with self.subTest(subject=index):
                document = copy.deepcopy(self._document())
                document["verification_conditions"][0]["provenance"][
                    "related_spans"
                ][0]["span"] = subject
                self.assertEqual(self._join(document), [])

    def test_inverted_or_out_of_bounds_anchor_fails_closed(self):
        anchors = []
        inverted = copy.deepcopy(self.ANCHOR)
        inverted["start"]["column"] = 24
        anchors.append(inverted)
        bad_line = copy.deepcopy(self.ANCHOR)
        bad_line["end"]["line"] = 2
        anchors.append(bad_line)
        bad_column = copy.deepcopy(self.ANCHOR)
        bad_column["end"]["column"] = len(self.SOURCE.encode("utf-8")) + 1
        anchors.append(bad_column)
        negative = copy.deepcopy(self.ANCHOR)
        negative["start"]["column"] = -1
        anchors.append(negative)
        non_integral = copy.deepcopy(self.ANCHOR)
        non_integral["start"]["line"] = "1"
        anchors.append(non_integral)
        boolean_coordinate = copy.deepcopy(self.ANCHOR)
        boolean_coordinate["start"]["line"] = True
        anchors.append(boolean_coordinate)
        non_boolean_ghost = copy.deepcopy(self.ANCHOR)
        non_boolean_ghost["ghost"] = 0
        anchors.append(non_boolean_ghost)

        for index, anchor in enumerate(anchors):
            with self.subTest(anchor=index):
                document = copy.deepcopy(self._document())
                document["verification_conditions"][0]["provenance"][
                    "source_span"
                ] = anchor
                self.assertEqual(self._join(document), [])


class IdentifierModesTests(unittest.TestCase):
    """Pure translation of identifier_modes, without running the compiler."""

    SOURCE_LINES = ["x" * 60] * 4

    def _entry(self, col0, col1, mode, ghost=False, file="input.ml"):
        location = _span_v1(1, col0, col1, ghost=ghost)
        location["file"] = file
        return {"location": location, "mode": mode}

    def test_entries_become_zero_based_editor_ranges(self):
        document = {
            "identifier_modes": [
                self._entry(4, 5, "@ total"),
                self._entry(8, 9, "@ local once"),
            ]
        }
        got = compiler_adapter.identifier_modes(document, self.SOURCE_LINES)
        self.assertEqual(
            got,
            [
                {
                    "start": {"line": 0, "col": 4},
                    "end": {"line": 0, "col": 5},
                    "mode": "@ total",
                },
                {
                    "start": {"line": 0, "col": 8},
                    "end": {"line": 0, "col": 9},
                    "mode": "@ local once",
                },
            ],
        )

    def test_file_tagged_workspace_ranges_use_each_file(self):
        document = {
            "identifier_modes": [self._entry(4, 5, "@ total", file="Demo.ml")]
        }
        got = compiler_adapter.identifier_modes_by_file(
            document, {"Demo.ml": self.SOURCE_LINES}
        )
        self.assertEqual(got[0]["file"], "Demo.ml")
        self.assertEqual(got[0]["start"], {"line": 0, "col": 4})
        self.assertEqual(got[0]["mode"], "@ total")

    def test_ghost_malformed_and_missing_entries_are_dropped(self):
        document = {
            "identifier_modes": [
                self._entry(1, 2, "@ total", ghost=True),
                self._entry(2, 3, ""),
                {"mode": "@ total"},
                "not a dict",
                self._entry(3, 4, "@ portable"),
            ]
        }
        got = compiler_adapter.identifier_modes(document, self.SOURCE_LINES)
        self.assertEqual([entry["mode"] for entry in got], ["@ portable"])
        self.assertEqual(compiler_adapter.identifier_modes({}, self.SOURCE_LINES), [])


class ScrubDetailTests(unittest.TestCase):
    """CL1: the solver's temp Lean scratch path is relabeled before display."""

    def test_temp_lean_path_relabeled(self):
        detail = (
            "/tmp/vox2-vc6891af.lean:4:2: error: "
            "`grind` failed\ncase grind"
        )
        self.assertEqual(
            compiler_adapter._scrub_detail(detail),
            "vc.lean:4:2: error: `grind` failed\ncase grind",
        )

    def test_none_and_empty_stay_none(self):
        self.assertIsNone(compiler_adapter._scrub_detail(None))
        self.assertIsNone(compiler_adapter._scrub_detail(""))

    def test_detail_without_a_path_is_unchanged(self):
        self.assertEqual(compiler_adapter._scrub_detail("no path here"), "no path here")


class RealCompilerVcTests(unittest.TestCase):
    ocamlc: str = ""

    @classmethod
    def setUpClass(cls):
        found = compiler_adapter.find_ocamlc()
        if found is None:
            raise unittest.SkipTest("vox2 compiler is not built in this worktree")
        cls.ocamlc = found

    @staticmethod
    def _underlined(source, vc):
        # The exact source text a VC's span underlines (single-line spans only).
        lines = source.split("\n")
        start, end = vc["span"]["start"], vc["span"]["end"]
        if start["line"] != end["line"]:
            return None
        return lines[start["line"]][start["col"] : end["col"]]

    def test_overview_single_proved_vc_at_call_site(self):
        # The call site `positive 7` carries the sole obligation, proved.
        source = "let positive (x : int{ _ > 0 }) = x\nlet seven = positive 7\n"
        payload = compiler_adapter.vcs_for_source(source, 1, self.ocamlc)
        self.assertEqual(payload["revision"], 1)
        self.assertEqual(len(payload["vcs"]), 1)
        vc = payload["vcs"][0]
        self.assertEqual(vc["status"], "proved")
        # Coordinate base: the anchor is 0-based line 1 (source line 2).  The
        # contract-argument obligation anchors on the argument VALUE (`7`), not
        # the whole call (C1: location preferred over program_point).
        self.assertEqual(self._underlined(source, vc), "7")
        self.assertIn("generated_lean", vc)
        self.assertTrue(vc["generated_lean"])

    def test_disproved_vc_carries_detail_and_location(self):
        source = "let need_one (x : int{ _ = 1 }) = x\nlet wrong = need_one 2\n"
        payload = compiler_adapter.vcs_for_source(source, 2, self.ocamlc)
        disproved = [vc for vc in payload["vcs"] if vc["status"] == "disproved"]
        self.assertEqual(len(disproved), 1)
        vc = disproved[0]
        # Anchors on the argument value `2` (C1: location, not the whole call).
        self.assertEqual(self._underlined(source, vc), "2")
        # Real disproved obligations usually have no discrete counterexample;
        # the solver diagnostic lands in detail instead.
        self.assertTrue(vc["detail"])

    def test_every_real_span_underlines_nonblank_source(self):
        # A recursive proof emits several obligations; each one must anchor on
        # a real, non-blank slice of the buffer (no drifted or ghost marks).
        source = (
            "let rec fib (n : int{ _ >= 0 }) : int{ _ >= 0 } =\n"
            "  if n <= 1 then n else fib (n - 1) + fib (n - 2)\n"
        )
        payload = compiler_adapter.vcs_for_source(source, 3, self.ocamlc)
        self.assertTrue(payload["vcs"])
        lines = source.split("\n")
        for vc in payload["vcs"]:
            self.assertEqual(vc["status"], "proved", vc)
            start, end = vc["span"]["start"], vc["span"]["end"]
            self.assertGreaterEqual(start["line"], 0)
            self.assertLess(start["line"], len(lines))
            sliced = lines[start["line"]][start["col"] : end["col"]]
            if start["line"] == end["line"]:
                self.assertTrue(sliced.strip(), "blank span: %r" % (vc,))

    def test_pre_abort_vcs_survive_a_failing_buffer(self):
        # A proved obligation precedes a disproved one; the compile aborts at
        # the failure, but the earlier obligation is still dumped.
        source = (
            "let need_pos (x : int{ _ > 0 }) = x\n"
            "let good = need_pos 1\n"
            "let bad = need_pos 0\n"
        )
        payload = compiler_adapter.vcs_for_source(source, 4, self.ocamlc)
        statuses = [vc["status"] for vc in payload["vcs"]]
        self.assertIn("proved", statuses)
        self.assertIn("disproved", statuses)

    def test_empty_and_unrefined_buffers_are_available_but_empty(self):
        # A genuine "no obligations": the compile completed, so this is
        # distinct from an unavailable dump even though both have empty vcs.
        for source in ("   \n", "let x = 1\n"):
            payload = compiler_adapter.vcs_for_source(source, 5, self.ocamlc)
            self.assertEqual(payload["vcs"], [])
            self.assertFalse(payload["unavailable"], source)
            self.assertEqual(payload["hidden"], 0)

    def test_completed_dump_is_available_with_no_hidden(self):
        source = "let positive (x : int{ _ > 0 }) = x\nlet seven = positive 7\n"
        payload = compiler_adapter.vcs_for_source(source, 7, self.ocamlc)
        self.assertFalse(payload["unavailable"])
        self.assertEqual(payload["hidden"], 0)

    def test_identifier_modes_include_binder_and_read(self):
        source = "let id (x : int) = x\nlet y = id 1\n"
        payload = compiler_adapter.vcs_for_source(source, 9, self.ocamlc)
        modes = payload["identifier_modes"]
        if not modes:
            self.skipTest("configured compiler does not emit identifier_modes")
        self.assertTrue(all(entry["mode"].startswith("@ ") for entry in modes))
        spans = {
            (
                entry["start"]["line"],
                entry["start"]["col"],
                entry["end"]["line"],
                entry["end"]["col"],
            )
            for entry in modes
        }
        self.assertIn((0, 8, 0, 9), spans)  # binder x
        self.assertIn((0, 19, 0, 20), spans)  # read x

    def test_errored_buffer_with_no_obligations_is_unavailable(self):
        # S1-clarity: a buffer that fails to compile and yields no placeable
        # obligations must DEFER to the error state (unavailable), never read as
        # a clean "no obligations".  Contrast with the clean-unrefined case
        # above, which stays available-but-empty.
        payload = compiler_adapter.vcs_for_source(
            "let x : int = true\n", 8, self.ocamlc
        )
        self.assertTrue(payload["unavailable"])
        self.assertEqual(payload["vcs"], [])


class RealCompilerLemmaCallTests(unittest.TestCase):
    """The channel against the real compiler, not a hand-built payload."""

    ocamlc: str = ""

    @classmethod
    def setUpClass(cls):
        found = compiler_adapter.find_ocamlc()
        if found is None:
            raise unittest.SkipTest("vox2 compiler is not built in this worktree")
        cls.ocamlc = found

    # A definition equation reaches its callers only through the companion
    # binding the compiler generates for it, and a call to that binding hands
    # back a refined unit -- the shape this channel is about.  One call earns
    # the annotation below it; the other states the same kind of thing where
    # nothing reads it.
    SOURCE = (
        "let[@vox.def] double x = x + x\n"
        "\n"
        "let read (a : int{ _ = 3 }) =\n"
        "  let () = double_def a in\n"
        "  (double a : int{ _ = 6 })\n"
        "\n"
        "let unread (b : int{ _ = 4 }) =\n"
        "  let () = double_def b in\n"
        "  (b : int{ _ > 0 })\n"
    )

    def _payload(self):
        return compiler_adapter.vcs_for_source(self.SOURCE, 1, self.ocamlc, "z3")

    def test_both_call_sites_are_reported_with_placeable_spans(self):
        payload = self._payload()
        if payload.get("unavailable"):
            raise unittest.SkipTest("solver unavailable: %s" % payload)
        calls = payload["lemma_calls"]
        assert calls is not None
        lines = self.SOURCE.split("\n")
        self.assertEqual(len(calls), 2)
        for call in calls:
            self.assertEqual(call["name"], "double_def")
            self.assertTrue(call["introduced"])
            sliced = lines[call["start"]["line"]][
                call["start"]["col"] : call["end"]["col"]
            ]
            self.assertIn(sliced, ("double_def a", "double_def b"))
        self.assertEqual(
            sorted(call["start"]["line"] for call in calls), [3, 7]
        )

    def test_the_read_call_is_credited_and_the_unread_one_is_not(self):
        payload = self._payload()
        if payload.get("unavailable"):
            raise unittest.SkipTest("solver unavailable: %s" % payload)
        # Fold usage over every obligation, keyed by producer span, exactly as
        # the frontend model does.
        read = set()
        seen = set()
        for vc in payload["vcs"]:
            self.assertEqual(vc["status"], "proved", vc)
            for hypothesis in vc["hypotheses"]:
                producers = hypothesis["producers"]
                self.assertIsNotNone(producers, hypothesis)
                assert producers is not None
                for producer in producers:
                    if producer["kind"] != "application":
                        continue
                    key = producer["span"]["start"]["line"]
                    seen.add(key)
                    if hypothesis.get("used") is True:
                        read.add(key)
        self.assertEqual(seen, {3, 7})
        self.assertEqual(read, {3})

    def test_a_folded_pair_of_calls_credits_both_sites(self):
        # Both arms state the same proposition; the fact environment keeps one
        # entry for it and the annotation after the merge reads that entry.
        # Naming only the entry's own origin would leave the other arm's call
        # looking unread -- and it is the call a proof there depends on.
        source = (
            "let[@vox.def] double x = x + x\n"
            "\n"
            "let after_merge (c : bool) (d : int{ _ = 5 }) =\n"
            "  let () = if c then double_def d else double_def d in\n"
            "  (double d : int{ _ = 10 })\n"
        )
        payload = compiler_adapter.vcs_for_source(source, 1, self.ocamlc, "z3")
        if payload.get("unavailable"):
            raise unittest.SkipTest("solver unavailable: %s" % payload)
        calls = payload["lemma_calls"]
        assert calls is not None
        self.assertEqual(len(calls), 2)
        call_columns = sorted(call["start"]["col"] for call in calls)
        read_columns = set()
        for vc in payload["vcs"]:
            for hypothesis in vc["hypotheses"]:
                if hypothesis.get("used") is not True:
                    continue
                for producer in hypothesis["producers"] or []:
                    if producer["kind"] == "application":
                        read_columns.add(producer["span"]["start"]["col"])
        self.assertEqual(sorted(read_columns), call_columns)


class VcsUnavailableTests(unittest.TestCase):
    """The unavailable state needs no built compiler: a run that cannot even
    start (a bogus compiler path) is the clearest "data unavailable" case, and
    is distinct from an empty-but-available dump."""

    def test_unrunnable_compiler_is_unavailable_not_empty(self):
        payload = compiler_adapter.vcs_for_source(
            "let f (x : int{ _ > 0 }) = x\nlet a = f 0\n",
            1,
            "/nonexistent/ocamlc.opt",
        )
        self.assertTrue(payload["unavailable"])
        self.assertEqual(payload["vcs"], [])
        self.assertEqual(payload["revision"], 1)

    def test_empty_source_is_available(self):
        payload = compiler_adapter.vcs_for_source("", 2, "/nonexistent/ocamlc.opt")
        self.assertFalse(payload["unavailable"])
        self.assertEqual(payload["vcs"], [])


class WorkspaceUnitTests(unittest.TestCase):
    """Pure (no-compiler) tests for the multi-file helpers."""

    def test_valid_unit_names(self):
        for good in ("Demo.ml", "Demo.mli", "foo.ml", "A_b'.mli"):
            self.assertTrue(compiler_adapter._valid_unit_name(good), good)
        for bad in (
            "../evil.ml",
            "a/b.ml",
            "x.txt",
            "1bad.ml",
            ".ml",
            "Demo.ml\x00",
            "",
            42,
        ):
            self.assertFalse(compiler_adapter._valid_unit_name(bad), bad)

    def test_build_order_interfaces_first_modules_in_given_order(self):
        order = compiler_adapter._build_order(["Demo.ml", "Demo.mli", "Client.ml"])
        # Within a module the .mli precedes its .ml; module order follows first
        # appearance (Demo before Client).
        self.assertEqual(order, ["Demo.mli", "Demo.ml", "Client.ml"])

    def test_parse_all_errors_attributes_and_returns_all(self):
        # Two independent errors in two units: each routes to its own file, and
        # both are returned (not just the first).
        text = (
            'File "Foo.ml", line 2, characters 4-5:\n'
            "Error: This expression has type int\n"
            'File "Bar.ml", line 1, characters 0-3:\n'
            "Error: Unbound value baz\n"
        )
        lines = {"Foo.ml": ["", "", "let x = y"], "Bar.ml": ["baz"]}
        errors = compiler_adapter.parse_all_errors(text, lines, "Foo.ml")
        self.assertEqual(len(errors), 2)
        self.assertEqual(errors[0]["file"], "Foo.ml")
        self.assertEqual(errors[1]["file"], "Bar.ml")

    def test_parse_all_errors_defaults_to_active_when_no_file(self):
        text = "Error: something with no File header\n"
        errors = compiler_adapter.parse_all_errors(text, {}, "Active.ml")
        self.assertEqual(len(errors), 1)
        self.assertEqual(errors[0]["file"], "Active.ml")

    def _stub_ocamlc(self, script):
        # A throwaway executable standing in for the compiler, so the crash /
        # nonzero-exit paths can be exercised without the real toolchain.
        handle = tempfile.NamedTemporaryFile(
            mode="w", suffix=".sh", delete=False, prefix="stub-ocamlc-"
        )
        handle.write(script)
        handle.close()
        os.chmod(handle.name, os.stat(handle.name).st_mode | stat.S_IEXEC)
        self.addCleanup(os.unlink, handle.name)
        return handle.name

    def test_fatal_crash_is_not_a_false_green(self):
        # H1: a nonzero exit that prints only "Fatal error: ..." (no located
        # Error: line, no dump) must NOT read as "workspace typechecks/verified".
        stub = self._stub_ocamlc(
            '#!/bin/sh\necho "Fatal error: exception Assert_failure" 1>&2\nexit 2\n'
        )
        files = [
            {"name": "Demo.ml", "source": "let x = 1\n"},
            {"name": "Client.ml", "source": "let y = 2\n"},
        ]
        result = compiler_adapter.check_workspace(files, "Demo.ml", 1, stub)
        self.assertFalse(result["ok"])
        self.assertNotEqual(result["workspace_verification"]["status"], "verified")
        self.assertNotEqual(result["workspace_verification"]["status"], "none")
        self.assertTrue(result["files"]["Demo.ml"]["errors"])
        self.assertIn("Fatal error", result["files"]["Demo.ml"]["errors"][0]["message"])

    def test_nonzero_exit_with_no_output_still_not_ok(self):
        stub = self._stub_ocamlc("#!/bin/sh\nexit 3\n")
        result = compiler_adapter.check_workspace(
            [{"name": "Demo.ml", "source": "let x = 1\n"}], "Demo.ml", 1, stub
        )
        self.assertFalse(result["ok"])
        self.assertNotEqual(result["workspace_verification"]["status"], "verified")


class RealCompilerWorkspaceTests(unittest.TestCase):
    ocamlc: str = ""

    DEMO_MLI = "val positive : int{ _ > 0 }\nval nonneg : int -> int{ _ >= 0 }\n"
    DEMO_ML = (
        "let positive = (1 : int{ _ = 1 })\n"
        "let nonneg (x : int) : int{ _ >= 0 } = if x >= 0 then x else 0\n"
    )
    CLIENT_ML = "let at_least : int{ _ >= -1 } = Demo.nonneg 5\n"

    @classmethod
    def setUpClass(cls):
        found = compiler_adapter.find_ocamlc()
        if found is None:
            raise unittest.SkipTest("vox2 compiler is not built in this worktree")
        cls.ocamlc = found

    def _demo(self, active):
        files = [
            {"name": "Demo.mli", "source": self.DEMO_MLI},
            {"name": "Demo.ml", "source": self.DEMO_ML},
            {"name": "Client.ml", "source": self.CLIENT_ML},
        ]
        return compiler_adapter.check_workspace(files, active, 7, self.ocamlc)

    def test_demo_all_units_verify(self):
        result = self._demo("Demo.ml")
        self.assertTrue(result["ok"])
        self.assertEqual(result["revision"], 7)
        self.assertFalse(result["unavailable"])
        self.assertEqual(result["workspace_verification"]["status"], "verified")
        for name in ("Demo.ml", "Client.ml"):
            self.assertIn(name, result["files"])
            self.assertEqual(
                result["files"][name]["verification"]["status"], "verified"
            )
        self.assertEqual(
            result["files"]["Demo.mli"]["verification"]["status"], "none"
        )

    def test_seal_vc_anchors_in_interface_with_cross_unit_hypothesis(self):
        # The seal implication's anchor is the .mli's refinement; its supporting
        # hypothesis originates in the .ml -- the cross-file jump.
        result = self._demo("Demo.ml")
        seals = [vc for vc in result["vcs"] if vc["kind"] == "seal"]
        self.assertEqual(len(seals), 1)
        seal = seals[0]
        self.assertEqual(seal["status"], "proved")
        self.assertEqual(seal["file"], "Demo.ml")
        hyps = [h for h in seal["hypotheses"] if h.get("span")]
        self.assertTrue(hyps)
        self.assertEqual(hyps[0]["span"]["file"], "Demo.ml")

    def test_cross_unit_use_proves_with_imported_contract(self):
        result = self._demo("Client.ml")
        client_vcs = [vc for vc in result["vcs"] if vc["file"] == "Client.ml"]
        self.assertEqual(len(client_vcs), 1)
        self.assertEqual(client_vcs[0]["status"], "proved")

    def test_every_vc_tagged_with_a_workspace_file(self):
        names = {"Demo.mli", "Demo.ml", "Client.ml"}
        result = self._demo("Demo.ml")
        self.assertTrue(result["vcs"])
        for vc in result["vcs"]:
            self.assertIn(vc["file"], names, vc)

    def test_types_and_signature_only_for_active_unit(self):
        result = self._demo("Demo.ml")
        self.assertEqual(
            result["files"]["Demo.ml"]["signature"]["status"],
            "not-requested",
        )
        self.assertTrue(result["files"]["Demo.ml"].get("types"))
        inferred = compiler_adapter.signature_for_workspace(
            [
                {"name": "Demo.mli", "source": self.DEMO_MLI},
                {"name": "Demo.ml", "source": self.DEMO_ML},
                {"name": "Client.ml", "source": self.CLIENT_ML},
            ],
            "Demo.ml",
            7,
            self.ocamlc,
        )
        self.assertEqual(inferred["signature"]["status"], "available")
        # A non-active unit carries neither.
        self.assertIsNone(result["files"]["Client.ml"].get("signature"))

    def test_conformance_mismatch_routes_to_implementation(self):
        # A bare implementation that does not meet the interface's refinement is
        # a located error on the .ml, even with a different unit active.
        files = [
            {"name": "Demo.mli", "source": "val positive : int{ _ > 0 }\n"},
            {"name": "Demo.ml", "source": "let positive = (0 : int{ _ = 0 })\n"},
            {"name": "Client.ml", "source": "let y = 1\n"},
        ]
        result = compiler_adapter.check_workspace(files, "Client.ml", 1, self.ocamlc)
        self.assertFalse(result["ok"])
        demo_errors = result["files"]["Demo.ml"]["errors"]
        self.assertTrue(demo_errors)
        self.assertEqual(demo_errors[0]["file"], "Demo.ml")
        self.assertEqual(result["files"]["Client.ml"]["errors"], [])

    def test_invalid_file_set_is_unavailable(self):
        result = compiler_adapter.check_workspace(
            [{"name": "../evil.ml", "source": "x"}], "../evil.ml", 1, self.ocamlc
        )
        self.assertTrue(result["unavailable"])
        self.assertEqual(result["vcs"], [])


if __name__ == "__main__":
    unittest.main()
