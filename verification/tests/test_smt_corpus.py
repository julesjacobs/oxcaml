import importlib.util
import json
import io
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import Mock, patch


SCRIPT = Path(__file__).resolve().parents[1] / "benchmarks/smt_corpus.py"
SPEC = importlib.util.spec_from_file_location("smt_corpus", SCRIPT)
corpus = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(corpus)
sys.path.insert(0, str(SCRIPT.parent))
import smt_suites as suites
import smt_bench as benchmark
import smt_api_bench as api_benchmark
import smt_compile_bench as compile_benchmark
import smt_reference as reference


class CompilerPerformanceTests(unittest.TestCase):
    def assess(self, ocaml, z3, zero_calls):
        return compile_benchmark.source_performance({
            backend: [dict(repeat=i, seconds=value)
                      for i, value in enumerate(values)]
            for backend, values in [('ocaml', ocaml), ('z3', z3)]}, zero_calls)

    def test_solver_calls_require_speedup(self):
        self.assertFalse(self.assess([1] * 15, [1] * 15, False)
                         ['performance_pass'])
        self.assertTrue(self.assess([.9] * 15, [1] * 15, False)
                        ['performance_pass'])

    def test_zero_calls_allow_parity(self):
        result = self.assess([1] * 15, [1] * 15, True)
        self.assertTrue(result['performance_pass'])
        self.assertFalse(result['faster'])
        self.assertEqual(result['parity']['p_value'], 1 / 32768)

    def test_zero_calls_reject_consistent_regression(self):
        result = self.assess([1.01] * 15, [1] * 15, True)
        self.assertFalse(result['performance_pass'])
        self.assertEqual(result['parity']['p_value'], 1)

    def test_zero_calls_require_evidence_of_equivalence(self):
        result = self.assess([.98, 1.02] * 7 + [1.02], [1] * 15, True)
        self.assertFalse(result['performance_pass'])
        self.assertFalse(result['faster'])
        self.assertGreater(result['parity']['p_value'], .5)

    def test_zero_calls_accept_noise_inside_parity_band(self):
        result = self.assess([.995, 1.005] * 7 + [1.005], [1] * 15, True)
        self.assertTrue(result['performance_pass'])
        self.assertEqual(result['parity']['p_value'], 1 / 32768)

    def test_zero_calls_allow_strict_speedup(self):
        result = self.assess([.9] * 15, [1] * 15, True)
        self.assertTrue(result['performance_pass'])
        self.assertEqual(result['parity']['p_value'], 1)

    def test_parity_requires_fixed_minimum_sample(self):
        self.assertFalse(self.assess([1] * 3, [1] * 3, True)
                         ['performance_pass'])

    def test_pairs_are_matched_by_repeat(self):
        rows = [dict(repeat=i, seconds=float(i + 1)) for i in range(15)]
        result = compile_benchmark.source_performance(
            dict(ocaml=rows, z3=list(reversed(rows))), True)
        self.assertTrue(result['performance_pass'])
        self.assertEqual(result['parity']['p_value'], 1 / 32768)

    def test_parity_family_controls_multiple_comparisons(self):
        rows = [dict(self.assess([1.005] * 14 + [1.02],
                                [1] * 15, True),
                     correct=True, zero_calls_verified=True)
                for _ in range(200)]
        regression = dict(self.assess([1.01] * 15, [1] * 15, True),
                          correct=True, zero_calls_verified=True)
        solver = dict(self.assess([1.01] * 15, [1] * 15, False),
                      correct=True, zero_calls_verified=False)
        compile_benchmark.adjust_parity_family([*rows, regression, solver])
        self.assertFalse(any(row['performance_pass'] for row in rows))
        self.assertFalse(regression['performance_pass'])
        self.assertFalse(solver['performance_pass'])
        self.assertEqual(regression['parity']['family_size'], 201)
        self.assertEqual(regression['parity']['adjusted_p_value'], 1)
        self.assertEqual(rows[0]['parity']['adjusted_p_value'],
                         201 * 16 / 32768)

    def test_parity_correction_preserves_sample_and_correctness_gates(self):
        short = dict(self.assess([1] * 3, [1] * 3, True),
                     correct=True, zero_calls_verified=True)
        incorrect = dict(self.assess([1] * 15, [1] * 15, True),
                         correct=False, zero_calls_verified=True)
        compile_benchmark.adjust_parity_family([short, incorrect])
        self.assertFalse(short['performance_pass'])
        self.assertFalse(incorrect['performance_pass'])

    def test_holm_adjusted_values_remain_monotone(self):
        rows = [dict(correct=True, zero_calls_verified=True, faster=False,
                     parity=dict(p_value=p, sufficient_samples=True))
                for p in [.02, .025, .04]]
        compile_benchmark.adjust_parity_family(rows)
        self.assertEqual([r['parity']['adjusted_p_value'] for r in rows],
                         [.06, .06, .06])


@unittest.skipUnless(os.environ.get('VOX_TEST_SMT_CLI'),
                     'standalone solver requires VOX_TEST_SMT_CLI')
class OutputChannelTests(unittest.TestCase):
    def run_script(self, directory, script, **kwargs):
        result = subprocess.run(
            [os.environ['VOX_TEST_SMT_CLI'], '-in', '-smt2'],
            input=script, text=True, capture_output=True, cwd=directory,
            timeout=5, **kwargs)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result

    def test_reserved_channels_do_not_create_files(self):
        with tempfile.TemporaryDirectory() as directory:
            result = self.run_script(directory, '''
                (set-option :diagnostic-output-channel "stdout")
                (get-option :diagnostic-output-channel)
                (set-option :regular-output-channel "stderr")
                (get-option :regular-output-channel)(check-sat)
                (set-option :regular-output-channel "stdout")
                (get-option :regular-output-channel)(check-sat)
            ''')
            self.assertEqual(result.stdout, '"stdout"\n"stdout"\nsat\n')
            self.assertEqual(result.stderr, '"stderr"\nsat\n')
            self.assertEqual(list(Path(directory).iterdir()), [])

    def test_exit_responds_on_current_channel_and_ignores_trailing_input(self):
        for channel in ('stdout', 'stderr', 'output.txt'):
            for success in ('true', 'false'):
                with self.subTest(channel=channel, success=success):
                    with tempfile.TemporaryDirectory() as directory:
                        result = self.run_script(directory, f'''
                            (set-option :regular-output-channel "{channel}")
                            (set-option :print-success {success})(exit)
                            (check-sat)(invalid unclosed trailing
                        ''')
                        expected = ('success\nsuccess\n'
                                    if success == 'true' else '')
                        self.assertEqual(
                            result.stdout, expected if channel == 'stdout'
                            else '')
                        self.assertEqual(
                            result.stderr, expected if channel == 'stderr'
                            else '')
                        if channel == 'output.txt':
                            self.assertEqual((Path(directory) / channel)
                                             .read_text(), expected)

    def test_malformed_exit_recovers_before_valid_exit(self):
        with tempfile.TemporaryDirectory() as directory:
            result = self.run_script(directory,
                '(exit true)(check-sat)(exit)(check-sat)')
            responses = [part for _, _, part in corpus.expressions(
                result.stdout.encode())]
            self.assertEqual(len(responses), 2)
            self.assertTrue(responses[0].startswith(b'(error '))
            self.assertEqual(responses[1], b'sat')
            self.assertEqual(result.stderr, '')

    def test_files_append_and_reset_restores_channels(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / 'output.txt'
            output.write_text('prior\n')
            result = self.run_script(directory, '''
                (set-option :print-success true)
                (set-option :regular-output-channel "output.txt")
                (echo "first")
                (set-option :diagnostic-output-channel "output.txt")
                (set-option :diagnostic-output-channel "stderr")
                (reset-assertions)(echo "second")
                (reset)(get-option :regular-output-channel)
                (get-option :diagnostic-output-channel)(check-sat)
            ''')
            self.assertEqual(result.stdout,
                             'success\n"stdout"\n"stderr"\nsat\n')
            self.assertEqual(result.stderr, '')
            self.assertEqual(output.read_text(),
                             'prior\nsuccess\n"first"\nsuccess\n'
                             'success\nsuccess\n"second"\n')

    def test_failed_channel_options_recover_on_the_regular_channel(self):
        for option in ('diagnostic', 'regular'):
            with self.subTest(option=option):
                with tempfile.TemporaryDirectory() as directory:
                    result = self.run_script(directory, f'''
                        (set-option :regular-output-channel "output.txt")
                        (set-option :{option}-output-channel "missing/file")
                        (set-option :{option}-output-channel stdout)
                        (get-option :{option}-output-channel)(check-sat)
                    ''')
                    self.assertEqual(result.stdout, '')
                    self.assertEqual(result.stderr, '')
                    responses = list(corpus.expressions(
                        (Path(directory) / 'output.txt').read_bytes()))
                    self.assertEqual(len(responses), 4)
                    self.assertTrue(all(part.startswith(b'(error ')
                                        for _, _, part in responses[:2]))
                    expected = (b'"stderr"' if option == 'diagnostic'
                                else b'"output.txt"')
                    self.assertEqual([part for _, _, part in responses[2:]],
                                     [expected, b'sat'])

    @unittest.skipUnless(sys.platform == 'linux', 'requires resource limits')
    def test_replaced_and_reset_channels_release_files(self):
        import resource

        def limit_files():
            resource.setrlimit(resource.RLIMIT_NOFILE, (32, 32))

        with tempfile.TemporaryDirectory() as directory:
            script = '\n'.join(
                '(set-option :regular-output-channel "output.txt")'
                '(set-option :diagnostic-output-channel "diagnostic.txt")'
                '(check-sat)(reset)' for _ in range(64))
            result = self.run_script(directory, script, preexec_fn=limit_files)
            self.assertEqual(result.stdout, '')
            self.assertEqual(result.stderr, '')
            self.assertEqual((Path(directory) / 'output.txt').read_text(),
                             'sat\n' * 64)
            self.assertEqual(
                (Path(directory) / 'diagnostic.txt').read_text(), '')


class ProtocolTests(unittest.TestCase):
    def test_strings_symbols_comments_and_atoms(self):
        data = (b'; (check-sat)\n(assert (= |a;(b)| "hello ""(x)"";"))\n'
                b'(check-sat-assuming (|a;(b)|)) ; trailing comment\n'
                b'sat\n(error "a doubled ""quote"" and )")\n')
        parsed = list(corpus.expressions(data))
        self.assertEqual([item[2] for item in parsed], [
            b'(assert (= |a;(b)| "hello ""(x)"";"))',
            b'(check-sat-assuming (|a;(b)|))', b'sat',
            b'(error "a doubled ""quote"" and )")'])
        for start, end, expression in parsed:
            self.assertEqual(data[start:end], expression)

    def test_partial_final_command_is_not_a_query(self):
        data = b'(check-sat)\n(check-sat-assuming (|unterminated'
        self.assertEqual(list(corpus.expressions(data)),
                         [(0, 11, b'(check-sat)')])

    def test_quoted_token_delimiters_and_truncated_escapes(self):
        for data in (b'|name\\|part|', b'"text""part"', b'|a\\\\b|'):
            self.assertEqual(list(corpus.expressions(data)),
                             [(0, len(data), data)])
        for data in (b'|name\\', b'"text""', b'|name\\|'):
            self.assertEqual(list(corpus.expressions(data)), [])

    def test_each_query_and_exact_replay_prefix(self):
        incoming = (b'(set-option :produce-models true)\n'
                    b'(declare-const p Bool)\n(push 1)\n(assert p)\n'
                    b'(check-sat-assuming (p))\n(get-model)\n(pop 1)\n'
                    b'(reset)\n(assert false)\n'
                    b'(check-sat-using (then simplify solve-eqs smt))\n'
                    b'(reset)\n(check-sat)\n')
        outgoing = (b'sat\n((define-fun p () Bool true))\n'
                    b'unsat\nsat\n')
        with tempfile.TemporaryDirectory() as temporary:
            session = Path(temporary) / "session"
            session.mkdir()
            (session / "session.json").write_text(json.dumps({
                "source": "example.ml", "run_id": "example",
                "complete": True}))
            (session / "input.smt2").write_bytes(incoming)
            (session / "stdout").write_bytes(outgoing)
            (session / "stderr").write_bytes(b'')
            metadata, queries = corpus.index_session(session)
        self.assertEqual(metadata["queries"], 3)
        self.assertFalse(metadata["incomplete_input"])
        self.assertEqual([query["command"] for query in queries], [
            "check-sat-assuming", "check-sat-using", "check-sat"])
        self.assertEqual([query["result"] for query in queries],
                         ["sat", "unsat", "sat"])
        for query in queries:
            prefix = incoming[query["replay_start"]:query["replay_end"]]
            self.assertTrue(prefix.endswith(
                incoming[query["command_start"]:query["command_end"]]))
            self.assertEqual(query["query_sha256"], corpus.digest(prefix))
        self.assertEqual(queries[0]["replay_start"], 0)
        self.assertEqual(queries[1]["replay_start"],
                         incoming.index(b'(reset)'))
        self.assertEqual(queries[2]["replay_start"],
                         incoming.rindex(b'(reset)'))

    def test_missing_answer_remains_visible(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            (directory / "session.json").write_text('{}')
            (directory / "input.smt2").write_bytes(
                b'(check-sat)\n(reset)\n(check-sat)\n')
            (directory / "stdout").write_bytes(b'unknown\n')
            (directory / "stderr").write_bytes(b'')
            metadata, queries = corpus.index_session(directory)
        self.assertFalse(metadata["response_alignment_complete"])
        self.assertEqual([query["result"] for query in queries],
                         ["unknown", None])

    def test_interactive_wrapper_preserves_bytes(self):
        program = ("import os\n"
                   "while True:\n"
                   " line = b''\n"
                   " while not line.endswith(b'\\n'):\n"
                   "  byte = os.read(0, 1)\n"
                   "  if not byte: raise SystemExit(0)\n"
                   "  line += byte\n"
                   " if line == b'(check-sat)\\n':\n"
                   "  os.write(1, b'sat\\n')\n"
                   "  os.write(2, b'diagnostic\\n')\n")
        with tempfile.TemporaryDirectory() as temporary:
            environment = dict(os.environ, VOX_SMT_REAL_SOLVER=sys.executable,
                               VOX_SMT_CORPUS_DIR=temporary,
                               VOX_SMT_CORPUS_SOURCE="interactive.ml",
                               VOX_SMT_CORPUS_RUN_ID="test")
            process = subprocess.Popen([sys.executable, str(SCRIPT), "wrap",
                                        "-u", "-c", program], env=environment,
                                       stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE)
            sent = b'(declare-const p Bool)\n(check-sat)\n'
            process.stdin.write(sent)
            process.stdin.flush()
            self.assertEqual(process.stdout.readline(), b'sat\n')
            self.assertEqual(process.stderr.readline(), b'diagnostic\n')
            process.stdin.write(b'(check-sat)\n')
            process.stdin.flush()
            self.assertEqual(process.stdout.readline(), b'sat\n')
            process.stdin.close()
            self.assertEqual(process.wait(timeout=10), 0)
            process.stdout.close()
            process.stderr.close()
            sessions = list(Path(temporary).iterdir())
            self.assertEqual(len(sessions), 1)
            session = sessions[0]
            self.assertEqual((session / "input.smt2").read_bytes(),
                             sent + b'(check-sat)\n')
            self.assertEqual((session / "stdout").read_bytes(), b'sat\nsat\n')
            self.assertEqual((session / "stderr").read_bytes(),
                             b'diagnostic\ndiagnostic\n')
            metadata, queries = corpus.index_session(session)
            self.assertTrue(metadata["complete"])
            self.assertEqual(metadata["source"], "interactive.ml")
            self.assertEqual(len(queries), 2)


class DiscoveryTests(unittest.TestCase):
    def test_expect_classification_ignores_filename(self):
        self.assertEqual(corpus.source_kind(b'let x = 1\n[%%expect{|value|}]'),
                         "expect_driver")
        self.assertEqual(corpus.source_kind(b'#use "library.ml";;'),
                         "expect_driver")
        self.assertEqual(corpus.source_kind(
            b'(* TEST ocamlc_byte_exit_status = "2"; *)\nlet bad = 1'),
            "rejected")
        self.assertEqual(corpus.source_kind(b'let correct = 1'), "ordinary")

    def test_header_dependency_fields(self):
        index = {"one.mli": "lib/one.mli", "one.ml": "lib/one.ml",
                 "two.ml": "tests/two.ml", "three.ml": "tests/three.ml"}
        header = ('prebuilt_modules = "one.mli one.ml";\n'
                  'readonly_files += " two.ml";\n'
                  'module = "three.ml"; flags = "-principal";')
        self.assertEqual(corpus.header_sources(header, index),
                         set(index.values()))

    def test_rejection_expectations_belong_to_active_module(self):
        index = {name: name for name in ("good.ml", "bad.ml", "other.ml")}
        header = ('all_modules = "good.ml"; ocamlc_opt_exit_status = "0";'
                  'ocamlc.opt; all_modules = "bad.ml";'
                  'ocamlc_opt_exit_status = "2"; ocamlc.opt;'
                  'all_modules = "other.ml"; ocamlopt_opt_exit_status = "2";')
        self.assertEqual(corpus.header_rejections(header, index),
                         {"bad.ml", "other.ml"})


class BenchmarkScriptsTests(unittest.TestCase):
    def test_canonical_scripts_preserve_occurrences_and_check_hashes(self):
        script = b'(reset)\n(assert false)\n(check-sat)\n'
        digest = corpus.digest(script)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            replay = root / 'scripts'
            replay.mkdir()
            path = replay / (digest + '.smt2')
            path.write_bytes(script)
            queries = [dict(id=i, replay_script_sha256=digest) for i in range(3)]
            index = root / 'queries.jsonl'
            index.write_text(''.join(json.dumps(q) + '\n' for q in queries))
            self.assertEqual(list(benchmark.scripts(root, replay)),
                             [(q, script) for q in queries])
            path.write_bytes(b'(check-sat)')
            with self.assertRaisesRegex(ValueError, 'hash mismatch'):
                list(benchmark.scripts(root, replay))
            for invalid in ('../elsewhere', '0' * 63, 'g' * 64, ''):
                index.write_text(json.dumps(dict(replay_script_sha256=invalid)))
                with self.assertRaisesRegex(ValueError, 'invalid replay'):
                    list(benchmark.scripts(root, replay))

    def test_resource_policy_covers_assertion_setup(self):
        prefix = (b'(reset)\n(set-option :rlimit 1000000)\n'
                  b'(declare-const |check-sat| Bool)\n'
                  b'(assert |check-sat|)\n(push 1)\n')
        for check in (b'(check-sat)', b'(check-sat-assuming (|check-sat|))',
                      b'(check-sat-using (then simplify smt))'):
            script = prefix + check + b'\n'
            self.assertEqual(benchmark.measurement_script(script, 'preserve'),
                             script)
            self.assertEqual(benchmark.measurement_script(script, 'unlimited'),
                             b'(set-option :rlimit 0)\n'
                             + prefix.replace(b':rlimit 1000000', b':rlimit 0')
                             + check + b'\n')
        for script in (b'(assert true)', b'(check-sat)(check-sat)'):
            with self.assertRaises(ValueError):
                benchmark.measurement_script(script, 'unlimited')

    def test_resource_policy_preserves_timeouts_and_other_commands(self):
        script = (b'(set-option :timeout 17)\n'
                  b'(set-option ; budget\n :rlimit 25)\n'
                  b'(assert (= ":rlimit 10" ":rlimit 10"))\n'
                  b'(reset)\n(set-option :rlimit 40)\n'
                  b'(check-sat)\n')
        self.assertEqual(benchmark.measurement_script(script, 'unlimited'),
                         b'(set-option :rlimit 0)\n'
                         b'(set-option :timeout 17)\n'
                         b'(set-option :rlimit 0)\n'
                         b'(assert (= ":rlimit 10" ":rlimit 10"))\n'
                         b'(reset)\n(set-option :rlimit 0)\n'
                         b'(check-sat)\n')

    def test_replay_slice_preserves_scopes_and_only_the_target_check(self):
        commands = [
            b'(declare-const stale Bool)', b'(check-sat)', b'(reset)',
            b'(declare-const p Bool)', b'(push 1)', b'(assert p)',
            b'(check-sat)', b'(get-model)', b'(get-value (p))',
            b'(get-info :reason-unknown)', b'(get-unsat-core)',
            b'(echo "old check-sat")', b'(check-sat-assuming (p))',
            b'(get-value (p))', b'(pop 1)',
            b'(check-sat-using (then simplify smt))', b'(reset)',
            b'(declare-const q Bool)', b'(assert q)', b'(check-sat)',
            b'(get-model)', b')']
        starts = [sum(len(command) + 1 for command in commands[:index])
                  for index in range(len(commands))]
        queries = [dict(session='one', replay_start=starts[begin],
                        command_start=starts[end],
                        command_end=starts[end] + len(commands[end]))
                   for begin, end in ((0, 1), (2, 12), (0, 15), (16, 19))]
        opened, inputs = [], []
        real_open = Path.open

        def track_open(path, *args, **kwargs):
            stream = real_open(path, *args, **kwargs)
            opened.append(stream)
            if path.name == 'input.smt2':
                tracked = Mock(wraps=stream)
                inputs.append(tracked)
                return tracked
            return stream

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            session = root / 'sessions/one'
            session.mkdir(parents=True)
            (session / 'input.smt2').write_bytes(b'\n'.join(commands) + b'\n')
            (root / 'queries.jsonl').write_text(''.join(
                json.dumps(query) + '\n' for query in queries))
            with patch.object(Path, 'open', track_open):
                actual = list(benchmark.scripts(root))
        expected = [
            [b'(reset)', commands[0], commands[1]],
            [commands[index] for index in (2, 3, 4, 5, 12)],
            [commands[index] for index in (2, 3, 4, 5, 14, 15)],
            [commands[index] for index in (16, 17, 18, 19)]]
        self.assertEqual(actual, [(query, b'\n'.join(script) + b'\n')
                                 for query, script in zip(queries, expected)])
        self.assertEqual(len(inputs), 1)
        self.assertEqual([call.args for call in inputs[0].read.call_args_list],
                         [(q['command_end'] - q['replay_start'],)
                          for q in queries])
        self.assertTrue(all(stream.closed for stream in opened))

    def test_eviction_and_early_close_release_session_handles(self):
        opened = []
        real_open = Path.open

        def track_open(path, *args, **kwargs):
            stream = real_open(path, *args, **kwargs)
            opened.append(stream)
            return stream

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            queries = []
            for index in range(6):
                session = root / 'sessions' / str(index)
                session.mkdir(parents=True)
                (session / 'input.smt2').write_bytes(b'(check-sat)')
                queries.append(dict(session=str(index), command_start=0,
                                    command_end=11))
            (root / 'queries.jsonl').write_text(''.join(
                json.dumps(query) + '\n' for query in queries))
            with patch.object(Path, 'open', track_open):
                iterator = benchmark.scripts(root)
                for _ in range(5):
                    next(iterator)
                self.assertTrue(opened[1].closed)
                self.assertFalse(opened[-1].closed)
                iterator.close()
        self.assertTrue(all(stream.closed for stream in opened))


class SuiteTests(unittest.TestCase):
    def test_incremental_expected_results_and_resets(self):
        result = suites.analyze_script(
            b'; EXPECT: sat\n; EXPECT: unsat\n'
            b'(set-logic QF_UF)(declare-const p Bool)(push 1)(assert p)'
            b'(check-sat)(pop 1)(reset)(assert false)(check-sat)')
        self.assertEqual(result["expected_statuses"], ["sat", "unsat"])
        self.assertEqual(result["commands"]["reset"], 1)
        self.assertEqual(result["exclusions"], [])

    def test_unsupported_features_do_not_become_coverage(self):
        result = suites.analyze_script(
            b'(set-logic ALL)(declare-const a (Array Int Real))'
            b'(assert (forall ((x Int)) (= (select a x) 0.0)))(check-sat)')
        self.assertEqual(result["exclusions"],
                         ["arrays", "quantifiers", "reals"])

    def test_status_metadata_does_not_claim_later_incremental_results(self):
        result = suites.analyze_script(
            b'(set-info :status sat)(check-sat)(assert false)(check-sat)')
        self.assertEqual(result["expected_statuses"], ["sat", None])
        self.assertEqual(result["expected_status_source"], "set-info")

    def test_expected_unknown_is_preserved(self):
        result = suites.analyze_script(b'; EXPECT: unknown\n(check-sat)')
        self.assertEqual(result["expected_statuses"], ["unknown"])
        self.assertEqual(result["expected_status_source"], "EXPECT")

    def test_implicit_out_of_scope_theories_are_classified(self):
        examples = {
            "floating_point": [b'(_ fp-to_ubv 16)', b'(_ +oo 11 53)',
                               b'(_ NaN 8 24)', b'(declare-const x Float64)'],
            "reals": [b'(= 0.375 0.375)', b'(= (/ 1 2) (/ 1 2))',
                      b'(to_real 1)', b'real.pi'],
            "tuples": [b'(tuple 1 false)', b'(Tuple Int Bool)',
                       b'(_ tuple.select 0)'],
            "separation_logic": [b'(declare-heap (Int Int))', b'(pto 0 1)',
                                 b'sep.emp'],
            "strings": [b'(str.from_int 2)', b'(= "a" "a")'],
            "sets": [b'(set.singleton 2)'],
            "sequences": [b'(seq.unit 3)'],
        }
        for feature, expressions in examples.items():
            for expression in expressions:
                with self.subTest(expression=expression):
                    self.assertIn(feature,
                                  suites.theory_exclusions(expression))

    def test_semantic_overrides_are_distinct_from_solver_tuning(self):
        result = suites.analyze_script(
            b'(set-option :rewriter.hi-div0 false)'
            b'(set-option :sat.random_seed 7)(check-sat)')
        self.assertEqual(result["exclusions"], [
            "semantic_override: legacy bit-vector division by zero"])
        self.assertEqual(len(result["semantic_overrides"]), 1)
        result = suites.analyze_script(
            b'(set-option :rewriter.hi-div0 true)'
            b'(set-option :sat.random_seed 7)(check-sat)')
        self.assertEqual(result["exclusions"], [])
        self.assertEqual(len(result["options"]), 2)

    def test_vendor_command_line_semantics_are_explicit(self):
        result = suites.analyze_script(
            b'; COMMAND-LINE: --global-negate\n(check-sat)')
        self.assertEqual(result["exclusions"], [
            "semantic_override: global assertion negation"])
        self.assertEqual(result["semantic_overrides"][0]["source"],
                         "command-line")

    def test_supported_bv_and_integer_operators_remain_eligible(self):
        result = suites.analyze_script(
            b'(set-logic ALL)(declare-fun f (Int) Int)'
            b'(assert (= (f (int.pow2 2)) (ubv_to_int (bvcomp #b0 #b1))))'
            b'(assert (bvuaddo #b0 #b1))(check-sat)')
        self.assertEqual(result["exclusions"], [])

    def test_user_names_do_not_imply_a_theory(self):
        result = suites.analyze_script(
            b'(set-logic QF_UF)(declare-fun sin (Bool) Bool)'
            b'(assert (! (sin true) :named sqrt))(check-sat)')
        self.assertEqual(result["exclusions"], [])

    def test_popped_user_names_do_not_mask_builtin_operators(self):
        result = suites.analyze_script(
            b'(push 1)(declare-fun fp (Bool) Bool)(pop 1)'
            b'(assert (= (fp #b0 #b00000000 #b00000000000000000000000)'
            b'(_ +zero 8 24)))(check-sat)')
        self.assertIn("floating_point", result["exclusions"])

    def test_documentation_does_not_change_feature_classification(self):
        result = suites.analyze_script(
            b'(set-info :source "forall Array Real")'
            b'(set-logic QF_UF)(assert true)(check-sat)')
        self.assertEqual(result["exclusions"], [])

    def test_error_only_regression_is_kept(self):
        result = suites.analyze_script(
            b'; EXPECT: (error "malformed input")\n)')
        self.assertEqual(result["checks"], 0)
        self.assertEqual(result["exclusions"], [])

    def test_certificate_commands_are_recorded(self):
        result = suites.analyze_script(
            b'(set-logic QF_UF)(check-sat)(get-model)'
            b'(assert false)(check-sat)(get-unsat-core)')
        self.assertTrue(result["has_model_commands"])
        self.assertTrue(result["has_core_commands"])

    def test_incomplete_suite_script_is_not_silently_accepted(self):
        result = suites.analyze_script(b'(check-sat)\n(assert')
        self.assertEqual(result["exclusions"],
                         ["parse: incomplete final expression"])
        self.assertEqual(result["checks"], 1)
        self.assertEqual(suites.complete_expressions(b'sat\n; comment'),
                         [(0, 3, b'sat')])

    def test_get_value_must_answer_the_requested_terms(self):
        with self.assertRaisesRegex(ValueError, "requested terms"):
            suites.model_script([], b'((q true))', [b'p'])

    def test_declaration_scope_follows_pop_reset_and_global_option(self):
        commands = [expression for _, _, expression in suites.expressions(
            b'(declare-const a Int)(push 1)(declare-const b Int)(pop 1)'
            b'(set-option :global-declarations true)(push 1)'
            b'(declare-const c Int)(pop 1)')]
        self.assertEqual(set(suites.active_declarations(commands)),
                         {(b'a', (), b'Int'), (b'c', (), b'Int')})
        commands += [b'(reset)', b'(declare-const d Bool)']
        self.assertEqual(set(suites.active_declarations(commands)),
                         {(b'd', (), b'Bool')})

    def test_overload_inventory_normalizes_sorts_when_declared(self):
        script = (b'(push 1)(define-sort Number () Int)'
                  b'(set-option :global-decls true)'
                  b'(declare-fun f (Number) Number)'
                  b'(set-option :global-decls false)(pop 1)'
                  b'(define-sort Number () Bool)'
                  b'(declare-fun f (Number) Int)')
        commands = [part for _, _, part in suites.expressions(script)]
        self.assertEqual(set(suites.active_declarations(commands)),
                         {(b'f', (b'Int',), b'Int'),
                          (b'f', (b'Bool',), b'Int')})
        commands.append(b'(reset-assertions)')
        self.assertEqual(set(suites.active_declarations(commands)),
                         {(b'f', (b'Int',), b'Int')})

    def test_core_rejects_popped_assertion_names(self):
        commands = [expression for _, _, expression in suites.expressions(
            b'(push 1)(assert (! false :named stale))(pop 1)(check-sat)')]
        with self.assertRaisesRegex(ValueError, "inactive"):
            suites.core_script(commands, b'(stale)')

    def test_reference_timeout_with_no_checks_is_inconclusive(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'case.smt2'
            script = b'; EXPECT: (error "bad command")\n(invalid-command)'
            path.write_bytes(script)
            case = dict(suites.analyze_script(script), path=path.name)
            args = SimpleNamespace(output=Path(directory), solver='solver',
                                   solver_args=[], reference='reference',
                                   reference_args=[], timeout=1)
            with patch.object(suites, 'run_script',
                              return_value={"timeout": True}):
                result = suites.check_case((case, args))
        self.assertEqual(result["result"], "reference_inconclusive")

    def test_mapped_file_index_preserves_script_metadata(self):
        script = (b'(set-logic QF_UF)(declare-const p Bool)(push 1)'
                  b'(assert p)(check-sat)(pop 1)(check-sat-assuming (p))')
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            path = output / 'case.smt2'
            path.write_bytes(script)
            record = suites.inspect_file(({"id": "example", "suite": "z3"},
                                          path, output))
        for key, value in suites.analyze_script(script).items():
            self.assertEqual(record[key], value)
        self.assertEqual(record["sha256"], corpus.digest(script))

    def test_known_status_conflict_survives_reference_timeout(self):
        script = b'(set-info :status unsat)(assert false)(check-sat)'
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'case.smt2'
            path.write_bytes(script)
            case = dict(suites.analyze_script(script), path=path.name)
            args = SimpleNamespace(output=Path(directory), solver='solver',
                                   solver_args=[], reference='reference',
                                   reference_args=[], timeout=1)
            responses = [{"timeout": True}, {"returncode": 0,
                         "statuses": ["sat"], "errors": [],
                         "response_kinds": ["sat"], "stdout": "sat"}]
            with patch.object(suites, 'run_script', side_effect=responses):
                result = suites.check_case((case, args))
        self.assertEqual(result["result"], "reference_inconclusive")
        self.assertEqual(result["expected_status_conflicts"],
                         [{"check": 1, "expected": "unsat", "actual": "sat"}])

    @unittest.skipUnless(shutil.which("prlimit"), "requires Linux prlimit")
    def test_solver_memory_limit_is_enforced(self):
        result = suites.run_script(sys.executable,
                                   ['-c', 'bytearray(128 * 1024 * 1024)'],
                                   b'', 5, memory_limit_mib=64)
        self.assertNotEqual(result["returncode"], 0)
        self.assertIn("MemoryError", result["stderr"])

    def test_large_solver_output_is_bounded(self):
        result = suites.run_script(sys.executable,
                                   ['-c', 'print("x" * (2 * 1024 * 1024))'],
                                   b'', 5, output_max_mib=1)
        self.assertTrue(result["output_limit"])
        self.assertGreater(result["stdout_bytes"], 1024 * 1024)
        self.assertNotIn("stdout", result)



class ExternalQueryGateTests(unittest.TestCase):
    @staticmethod
    def samples(result, seconds):
        return [{'result': result, 'seconds': seconds, 'errors': []}] * 3

    def summarize(self, actual='unsat', actual_seconds=.015,
                  reference='unsat', reference_seconds=.01,
                  expected='unsat', evidence=None):
        runs = {'ocaml': self.samples(actual, actual_seconds),
                'z3': self.samples(reference, reference_seconds)}
        return suites.summarize_query({}, runs, expected, 10, evidence)

    def test_each_incremental_check_has_its_logical_prefix(self):
        data = (b'(declare-const p Bool)(push 1)(assert p)(check-sat)'
                b'(get-model)(pop 1)(check-sat-assuming ((not p)))'
                b'(reset)(declare-const q Bool)(assert q)(check-sat)')
        queries = list(suites.query_scripts(data))
        self.assertEqual([query['ordinal'] for query, _, _ in queries], [1, 2, 3])
        second = queries[1][1]
        self.assertIn(b'(push 1)\n(assert p)\n(pop 1)', second)
        self.assertNotIn(b'(get-model)', second)
        self.assertNotIn(b'(check-sat)', second)
        self.assertTrue(second.endswith(b'(check-sat-assuming ((not p)))\n'))
        self.assertEqual(queries[2][1],
                         b'(reset)\n(declare-const q Bool)\n'
                         b'(assert q)\n(check-sat)\n')
        for query, _, _ in queries:
            command = data[query['command_start']:query['command_end']]
            self.assertEqual(corpus.command_name(command), query['command'])

    def test_operations_include_only_requested_contiguous_followups(self):
        data = (b'(declare-const p Bool)(assert p)(check-sat)'
                b'(echo "state")(get-model)(get-info :status)(get-value (p))'
                b'(push 1)(assert false)(check-sat)(get-unsat-core)')
        queries = list(suites.query_scripts(data))
        query, script, operation = queries[0]
        self.assertEqual(query['followup_commands'], ['get-model', 'get-value'])
        self.assertEqual(operation, script + b'(get-model)\n(get-value (p))\n')
        self.assertNotIn(b'(push 1)', operation)
        self.assertEqual(query['operation_end'], data.index(b'(push 1)'))

    def test_resource_limits_are_disabled_but_timeouts_remain(self):
        data = (b'(set-option :rlimit 12)(set-option :timeout 456)'
                b'(set-option :rlimit-per 18)(check-sat)')
        query, measured, _ = next(suites.query_scripts(data))
        self.assertEqual(query['disabled_resource_limits'],
                         [{'option': ':rlimit', 'value': '12'},
                          {'option': ':rlimit-per', 'value': '18'}])
        self.assertIn(b'(set-option :rlimit 0)', measured)
        self.assertIn(b'(set-option :rlimit-per 0)', measured)
        self.assertIn(b'(set-option :timeout 456)', measured)
        self.assertNotEqual(query['query_sha256'], query['measurement_sha256'])

    def test_named_observations_retain_their_definitions(self):
        queries = list(suites.query_scripts(
            b'(declare-const p Bool)(check-sat)'
            b'(get-value ((! p :named a)))(assert a)(check-sat)'))
        self.assertIn(b'(! p :named a)', queries[1][1])
        self.assertNotIn(b'get-value', queries[1][1])

    def test_rejected_reset_and_observation_remain_in_timed_input(self):
        data = (b'(assert false)(reset 1)'
                b'(get-value ((! missing :named a)))(check-sat)')
        query, script, _ = next(suites.query_scripts(data, [1, 2]))
        self.assertEqual(script, b'(reset)\n(assert false)\n(reset 1)\n'
                         b'(get-value ((! missing :named a)))\n(check-sat)\n')
        self.assertEqual(query['expected_error_commands'], [1, 2])

    def test_unknown_never_satisfies_answer_gate(self):
        self.assertFalse(self.summarize(actual='unknown', actual_seconds=.001)
                         ['passed'])
        self.assertFalse(self.summarize(reference='unknown',
                                        actual_seconds=.001)['passed'])
        self.assertTrue(self.summarize(expected='unknown')['passed'])

    def test_explicit_status_conflicts_fail_with_independent_evidence(self):
        evidence = [{'source': 'independent-reference',
                     'result': 'sat', 'seconds': 1, 'validated': True}]
        result = self.summarize(actual='sat', reference='timeout',
                                expected='unsat', evidence=evidence)
        self.assertFalse(result['correct'])
        self.assertFalse(result['passed'])

    def test_full_query_timeout_is_a_lower_bound(self):
        evidence = [{'source': 'independent-reference',
                     'result': 'unsat', 'seconds': .001, 'validated': True}]
        result = self.summarize(actual_seconds=1.9, reference='timeout',
                                reference_seconds=1, evidence=evidence)
        self.assertTrue(result['passed'])
        self.assertEqual(result['performance_basis'], 'z3-timeout-lower-bound')
        self.assertEqual(result['median_seconds']['z3'], 1)
        result = self.summarize(actual_seconds=2.1, reference='timeout',
                                reference_seconds=1, evidence=evidence)
        self.assertFalse(result['passed'])

    def test_reference_unknown_does_not_use_the_ratio_gate(self):
        evidence = [{'source': 'independent-reference',
                     'result': 'unsat', 'seconds': 100, 'validated': True}]
        result = self.summarize(reference='unknown', reference_seconds=1,
                                evidence=evidence)
        self.assertTrue(result['correct'])
        self.assertIsNone(result['performance_pass'])
        self.assertFalse(result['passed'])
        self.assertTrue(self.summarize(actual_seconds=.001,
                                       reference='unknown', evidence=evidence)
                        ['passed'])

    def test_inconsistent_repeated_answers_fail(self):
        runs = {'ocaml': self.samples('unsat', .001),
                'z3': self.samples('unsat', .001)}
        runs['ocaml'] = [*runs['ocaml'], {'result': 'sat', 'seconds': .001}]
        self.assertFalse(suites.summarize_query({}, runs, None, 10)['passed'])
        runs['z3'] = runs['ocaml']
        self.assertFalse(suites.summarize_query({}, runs, None, 10)['passed'])

    def test_startup_timeout_does_not_bound_query_time(self):
        runs = {'ocaml': self.samples('unsat', .02),
                'z3': [{'result': 'timeout', 'seconds': 10, 'phase': 'startup'}]}
        evidence = [{'result': 'unsat', 'validated': True}]
        result = suites.summarize_query({}, runs, None, 10, evidence)
        self.assertTrue(result['correct'])
        self.assertIsNone(result['performance_pass'])
        self.assertFalse(result['passed'])

    def test_interface_classification_keeps_theory_extensions(self):
        case = suites.analyze_script(
            b'(assert (= (int.pow2 2) 4))(check-sat)(get-model)')
        self.assertEqual(suites.classify_interfaces(case), [])
        case = suites.analyze_script(b'(assert false)(check-sat)(get-proof)')
        self.assertEqual(suites.classify_interfaces(case),
                         [{'command': 'get-proof', 'interface': 'proof_export'}])
        self.assertEqual(case['exclusions'], [])

    def test_incidental_vendor_options_remain_theory_relevant(self):
        case = suites.analyze_script(
            b'(set-option :solve-bv-as-int sum)'
            b'(set-option :proof true)(assert false)(check-sat)')
        self.assertEqual(suites.classify_interfaces(case), [])
        case = suites.analyze_script(
            b'(set-option :output post-asserts)(check-sat)')
        self.assertEqual(suites.classify_interfaces(case),
                         [{'option': ':output', 'value': 'post-asserts',
                           'interface': 'internal_debugging'}])

    def test_scope_report_preserves_inventory_and_hashes(self):
        cases = [dict(path='theory.smt2', sha256='one', eligible=True,
                      suite='z3', commands={'check-sat': 1}),
                 dict(path='proof.smt2', sha256='two', eligible=True,
                      suite='z3', commands={'check-sat': 1, 'get-proof': 1})]
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            index = output / 'cases.jsonl'
            original = ''.join(json.dumps(case) + '\n' for case in cases)
            index.write_text(original)
            args = SimpleNamespace(output=output, results=None, suite=None,
                                   track=None, family=None)
            with patch('builtins.print'):
                report = suites.scope_report(args)
            self.assertEqual(index.read_text(), original)
            self.assertEqual(report['original_eligible'], 2)
            self.assertEqual(report['scope'],
                             {'theory-and-standard': 1,
                              'theory-and-standard-with-extra-interfaces': 1})
            classified = [json.loads(line) for line in
                          (output / 'scope.jsonl').read_text().splitlines()]
            self.assertTrue(all(case['eligible'] for case in classified))
            self.assertEqual([case['sha256'] for case in classified],
                             ['one', 'two'])

    def test_mixed_script_removes_only_explicit_extra_interfaces(self):
        data = (b'(set-option :proof true)(assert false)(check-sat)'
                b'(get-proof)(set-option :output post-asserts)'
                b'(get-model)(check-sat)')
        canonical, record = suites.timing_script(data)
        self.assertEqual(len(canonical), len(data))
        self.assertEqual(record['source_sha256'], corpus.digest(data))
        self.assertEqual(record['canonical_sha256'], corpus.digest(canonical))
        self.assertIn(b'(set-option :proof true)', canonical)
        self.assertIn(b'(assert false)', canonical)
        self.assertIn(b'(get-model)', canonical)
        self.assertNotIn(b'get-proof', canonical)
        self.assertNotIn(b'post-asserts', canonical)
        self.assertEqual(len(list(suites.query_scripts(canonical))), 2)
        self.assertEqual([item['reason'] for item in record['transformations']],
                         ['proof_export', 'internal_debugging'])
        for item in record['transformations']:
            original = data[item['command_start']:item['command_end']]
            self.assertEqual(item['source_sha256'], corpus.digest(original))

    def test_malformed_exit_does_not_truncate_queries(self):
        queries = list(suites.query_scripts(
            b'(assert false)(exit 1)(check-sat)', rejected_commands=[1]))
        self.assertEqual(len(queries), 1)
        self.assertIn(b'(exit 1)', queries[0][1])
        self.assertEqual(queries[0][0]['expected_error_commands'], [1])

    def test_terminal_exit_ignores_malformed_unreachable_input(self):
        script = b'(check-sat)(exit)(assert false))'
        self.assertEqual(suites.analyze_script(script)['checks'], 1)
        self.assertEqual(len(list(suites.query_scripts(script))), 1)
        self.assertEqual(suites.timing_script(script)[0], script)
        self.assertEqual(suites.validate_certificates(script, b'sat', None), [])

    def test_timing_preserves_malformed_resource_limits(self):
        for value in (b'-1', b'01', b'1.0', b'true', b'"12"', b'(+ 1 2)'):
            script = b'(set-option :rlimit ' + value + b')(check-sat)'
            transformed, record = suites.timing_script(script)
            self.assertEqual(transformed, script)
            self.assertEqual(record['transformations'], [])
            query, measured, _ = next(suites.query_scripts(script, [0]))
            self.assertIn(b'(set-option :rlimit ' + value + b')', measured)
            self.assertEqual(query['disabled_resource_limits'], [])
        script = b'(set-option :rlimit 123)(check-sat)'
        transformed, record = suites.timing_script(script)
        self.assertIn(b'(set-option :rlimit 0)', transformed)
        self.assertEqual(len(transformed), len(script))
        self.assertEqual(len(record['transformations']), 1)

    def test_tactic_interface_preserves_the_logical_check(self):
        data = (b'(assert false)(check-sat-using (then fail smt))'
                b'(reset)(check-sat-using smt)')
        canonical, record = suites.timing_script(data)
        self.assertEqual(len(canonical), len(data))
        queries = list(suites.query_scripts(canonical))
        self.assertEqual(len(queries), 2)
        self.assertIn(b'(assert false)', queries[0][1])
        self.assertNotIn(b'(assert false)', queries[1][1])
        self.assertTrue(all(b'(check-sat)' in script
                            for _, script, _ in queries))
        self.assertEqual([item['reason'] for item in record['transformations']],
                         ['tactic_selection', 'tactic_selection'])
        malformed = b'(check-sat-using)(check-sat-using smt extra)'
        self.assertEqual(suites.timing_script(malformed)[0], malformed)

    def test_unsupported_and_diagnostics_cannot_qualify_a_definitive_answer(self):
        for extra in ({'unsupported_responses': 1}, {'stderr': 'warning'}):
            runs = {'ocaml': [dict(result='unsat', seconds=.001, **extra)],
                    'z3': self.samples('unsat', .01)}
            self.assertFalse(suites.summarize_query({}, runs, None, 10)['passed'])

    def test_query_gate_requires_serial_timing(self):
        with self.assertRaisesRegex(ValueError, '--jobs 1'):
            suites.query_gate(SimpleNamespace(jobs=2))

    def test_query_gate_requires_independent_reference_binaries(self):
        with tempfile.TemporaryDirectory() as directory:
            candidate = Path(directory) / 'candidate'
            reference = Path(directory) / 'reference'
            copied = Path(directory) / 'copy'
            candidate.write_bytes(b'candidate')
            reference.write_bytes(b'reference')
            copied.write_bytes(candidate.read_bytes())
            for z3, alternate in ((candidate, None), (copied, None),
                                  (reference, candidate), (reference, copied)):
                args = SimpleNamespace(
                    jobs=1, repeats=1, timeout=1, instant_ms=10,
                    solver=str(candidate), solver_args=[], z3=str(z3),
                    z3_args=['different-arguments'],
                    reference=str(alternate) if alternate else None,
                    reference_args=[])
                with self.assertRaisesRegex(ValueError, 'independent'):
                    suites.query_gate(args)

    def test_gate_resume_keeps_every_query_and_rejects_changed_settings(self):
        cases = [dict(path=f'{index}.smt2', sha256=str(index), suite='z3',
                      source_id='z3', eligible=True, checks=1,
                      commands=({'check-sat': 1} if index == 0 else
                                {'check-sat': 1, 'get-proof': 1}))
                 for index in range(2)]

        def measured(case, _):
            return dict(case, result='passed', queries=[{
                'passed': True, 'correct': True, 'performance_pass': True}])

        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            (output / 'cases.jsonl').write_text(''.join(
                json.dumps(case) + '\n' for case in cases))
            (output / 'manifest.json').write_text('{}')
            reference = output / 'reference'
            reference.write_bytes(b'independent reference')
            args = SimpleNamespace(
                output=output, solver=sys.executable, solver_args=[],
                z3=str(reference), z3_args=[], reference=None, reference_args=[],
                suite=None, track=None, family=None, results=None,
                interface_scope='theory-and-standard', jobs=1, repeats=1,
                timeout=1,
                instant_ms=10, memory_limit_mib=0, output_max_mib=1,
                resume=False)
            with patch('builtins.print'), patch.object(
                    suites, 'time_case', side_effect=[measured(cases[0], args),
                                                      RuntimeError('stop')]):
                with self.assertRaisesRegex(RuntimeError, 'stop'):
                    suites.query_gate(args)
            args.resume = True
            with patch('builtins.print'), patch.object(
                    suites, 'time_case', side_effect=measured) as runner:
                report = suites.query_gate(args)
            self.assertEqual(runner.call_count, 1)
            self.assertEqual(report['queries'], 2)
            self.assertEqual(report['query_results'], {'passed': 2})
            self.assertTrue(report['gate_passed'])
            args.instant_ms = 5
            with self.assertRaisesRegex(ValueError, 'resume settings'):
                suites.query_gate(args)

    @unittest.skipUnless(shutil.which('z3'), 'query timing requires Z3')
    def test_query_transport_and_recovery_after_error(self):
        solver = suites.QuerySolver(shutil.which('z3'), ['-in', '-smt2'],
                                    SimpleNamespace(timeout=5))
        try:
            result = solver.run(b'(reset)(declare-const p Bool)(assert p)'
                                b'(check-sat)')
            self.assertEqual(result['result'], 'sat')
            self.assertGreater(result['seconds'], 0)
            if sys.platform == 'linux':
                self.assertGreater(result['rss_bytes'], 0)
                self.assertGreaterEqual(result['peak_rss_bytes'],
                                        result['rss_bytes'])
            self.assertEqual(solver.run(b'(reset)(assert 1)(check-sat)')
                             ['result'], 'error')
            self.assertEqual(solver.run(b'(reset)(assert false)(check-sat)')
                             ['result'], 'unsat')
        finally:
            solver.close()

    def test_query_memory_is_observed_after_measured_exchange(self):
        solver = suites.QuerySolver('unused', [], SimpleNamespace(timeout=1))
        solver.process = SimpleNamespace(pid=123)
        order = []

        def exchange(*args, **kwargs):
            order.append('exchange')
            return {'result': 'sat', 'seconds': 0.012, 'errors': []}

        def read_status():
            order.append('memory')
            return 'Name:\tsolver\nVmRSS:\t4 kB\nVmHWM:\t8 kB\n'

        with (patch.object(solver, 'exchange', side_effect=exchange),
              patch.object(suites.platform, 'system', return_value='Linux'),
              patch.object(Path, 'read_text', side_effect=read_status)):
            result = solver.run(b'(check-sat)')
        self.assertEqual(order, ['exchange', 'memory'])
        self.assertEqual(result['seconds'], 0.012)
        self.assertEqual(result['rss_bytes'], 4096)
        self.assertEqual(result['peak_rss_bytes'], 8192)

    def test_unavailable_query_memory_is_not_reported_as_zero(self):
        solver = suites.QuerySolver('unused', [], SimpleNamespace(timeout=1))
        self.assertEqual(solver.memory(), {})
        solver.process = SimpleNamespace(pid=123)
        with (patch.object(suites.platform, 'system', return_value='Linux'),
              patch.object(Path, 'read_text', side_effect=FileNotFoundError)):
            self.assertEqual(solver.memory(), {})
        with (patch.object(suites.platform, 'system', return_value='Darwin'),
              patch.object(Path, 'read_text') as read):
            self.assertEqual(solver.memory(), {})
            read.assert_not_called()

    @unittest.skipUnless(shutil.which('z3'), 'operation timing requires Z3')
    def test_complete_operation_requires_valid_certificates(self):
        data = (b'(set-option :produce-models true)(declare-const p Bool)'
                b'(assert p)(check-sat)(get-model)(get-value (p))')
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'case.smt2'
            path.write_bytes(data)
            case = dict(suites.analyze_script(data), path=path.name,
                        sha256=corpus.digest(data))
            args = SimpleNamespace(output=Path(directory),
                                   solver=shutil.which('z3'),
                                   solver_args=['-in', '-smt2'],
                                   z3=shutil.which('z3'), z3_args=['-in', '-smt2'],
                                   reference=shutil.which('z3'),
                                   reference_args=['-in', '-smt2'],
                                   timeout=5, repeats=1, instant_ms=1000)
            result = suites.time_case(case, args)
            self.assertEqual(result['result'], 'passed')
            self.assertTrue(result['queries'][0]['operation']['passed'])
            original = suites.QuerySolver.run

            def malformed(solver, script, **options):
                sample = original(solver, script, **options)
                if options.get('auxiliary') and 'stdout' in sample:
                    sample['stdout'] = sample['stdout'].replace('true', 'false')
                return sample

            with patch.object(suites.QuerySolver, 'run', malformed):
                result = suites.time_case(case, args)
            self.assertEqual(result['result'], 'failed')
            self.assertTrue(result['queries'][0]['passed'])
            self.assertFalse(result['queries'][0]['operation']['passed'])

            data = (b'(set-option :produce-proofs true)(assert true)'
                    b'(check-sat)(get-proof)(assert false)(check-sat)'
                    b'(get-proof)')
            path.write_bytes(data)
            case = dict(suites.analyze_script(data), path=path.name,
                        sha256=corpus.digest(data))
            result = suites.time_case(case, args)
            self.assertEqual(result['result'], 'passed')
            self.assertEqual(len(result['queries']), 2)
            self.assertNotEqual(result['raw_complete_script']['result'], 'passed')
            self.assertEqual(result['canonical_complete_script']['result'],
                             'passed')
            self.assertEqual(len(result['transformation']['transformations']), 2)

    def test_nonreading_solver_cannot_bypass_timeout(self):
        solver = suites.QuerySolver(sys.executable,
                                    ['-c', 'import time; time.sleep(5)'],
                                    SimpleNamespace(timeout=.05))
        try:
            result = solver.run(b'(check-sat)')
            self.assertEqual(result['result'], 'timeout')
            self.assertEqual(result['phase'], 'startup')
            self.assertLess(result['seconds'], 1)
        finally:
            solver.close()

    def test_query_transport_rejects_unsupported_and_stderr(self):
        for response in ('print("unsupported", flush=True)',
                         'print("warning", file=sys.stderr, flush=True)'):
            program = ('import sys\nfor line in sys.stdin:\n'
                       ' if line.startswith("(echo "):\n'
                       '  print(line[6:-2], flush=True)\n'
                       ' elif line.startswith("(check-sat)"):\n'
                       '  ' + response + '\n  print("sat", flush=True)\n')
            solver = suites.QuerySolver(sys.executable, ['-c', program],
                                        SimpleNamespace(timeout=5))
            try:
                sample = solver.run(b'(reset)\n(check-sat)')
                self.assertEqual(sample['result'], 'error')
                if 'stderr' in response:
                    self.assertIn('warning', sample['stderr'])
                else:
                    self.assertEqual(sample['unsupported_responses'], 1)
            finally:
                solver.close()

    def test_query_transport_bounds_stderr(self):
        solver = suites.QuerySolver(
            sys.executable,
            ['-c', 'import sys; sys.stderr.write("x" * (2 * 1024 * 1024))'],
            SimpleNamespace(timeout=5, output_max_mib=1))
        try:
            self.assertEqual(solver.run(b'(check-sat)')['result'],
                             'output-limit')
        finally:
            solver.close()

    def test_query_transport_bounds_output(self):
        solver = suites.QuerySolver(
            sys.executable, ['-c', 'print("x" * (2 * 1024 * 1024))'],
            SimpleNamespace(timeout=5, output_max_mib=1))
        try:
            self.assertEqual(solver.run(b'(check-sat)')['result'],
                             'output-limit')
        finally:
            solver.close()

class ResumeTests(unittest.TestCase):
    def test_concurrent_result_writers_are_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            destination = output / 'results.jsonl'
            args = SimpleNamespace(output=output, results=destination)
            with (output / 'results.jsonl.lock').open('a') as lock:
                suites.fcntl.flock(lock, suites.fcntl.LOCK_EX)
                with self.assertRaisesRegex(ValueError, "another runner"):
                    suites.run(args)

    def test_partial_result_is_recovered_without_reusing_it(self):
        case = {"path": "case.smt2", "sha256": "hash"}
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "results.jsonl"
            path.write_text(json.dumps(case) + '\n{"path": "partial')
            self.assertEqual(list(suites.saved_results(path, [case])), [case])
            self.assertEqual(path.read_text(), json.dumps(case) + '\n')

    def test_changed_cases_and_duplicate_results_are_rejected(self):
        case = {"path": "case.smt2", "sha256": "hash"}
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "results.jsonl"
            path.write_text(json.dumps(case) + '\n')
            with self.assertRaisesRegex(ValueError, "selected case"):
                list(suites.saved_results(path, [dict(case, sha256="new")]))
            path.write_text((json.dumps(case) + '\n') * 2)
            with self.assertRaisesRegex(ValueError, "duplicate"):
                list(suites.saved_results(path, [case]))

    def test_resume_reuses_only_matching_completed_results(self):
        cases = [dict(path=f"{index}.smt2", sha256=str(index), suite="z3",
                      source_id="z3", eligible=True) for index in range(2)]

        def check(item):
            case, _ = item
            return dict(case, result="passed", tested={},
                        expected_status_conflicts=[])

        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            (output / 'cases.jsonl').write_text(''.join(
                json.dumps(case) + '\n' for case in cases))
            (output / 'manifest.json').write_text('{}')
            args = SimpleNamespace(
                output=output, solver=sys.executable, solver_args=[],
                reference=sys.executable, reference_args=[], suite=None,
                track=None, family=None, jobs=1, timeout=1,
                memory_limit_mib=0, output_max_mib=1, certificate_max_mib=1,
                results=output / 'results.jsonl', resume=False)
            with (patch('builtins.print'), patch.object(
                    suites, 'check_case', side_effect=[
                        check((cases[0], args)),
                        RuntimeError("interrupted")])):
                with self.assertRaisesRegex(RuntimeError, "interrupted"):
                    suites.run(args)
            args.resume = True
            with (patch('builtins.print'), patch.object(
                    suites, 'check_case', side_effect=check) as runner):
                summary = suites.run(args)
            self.assertEqual(runner.call_count, 1)
            self.assertEqual(runner.call_args.args[0][0], dict(
                cases[1], separate_interfaces=[],
                release_scope='theory-and-standard'))
            self.assertEqual(summary["results"], {"passed": 2})
            self.assertEqual(summary["remaining"], 0)
            args.timeout = 2
            with self.assertRaisesRegex(ValueError, "settings"):
                suites.run(args)


class ReferenceAdaptationTests(unittest.TestCase):
    def test_vendor_configuration_keeps_resets_and_source(self):
        script = (b'(set-option :produce-models false)(check-sat)'
                  b'(reset)(check-sat)(reset true)(check-sat)')
        configured, record = suites.vendor_configuration(
            script, ['--produce-models --model-cores=simple'])
        self.assertEqual(record['result'], 'valid')
        self.assertEqual(len(record['insertions']), 2)
        self.assertEqual(configured.count(b'(set-option :produce-models true)'),
                         2)
        self.assertIn(b'(set-option :produce-models false)', configured)
        self.assertIn(b'(reset true)', configured)
        self.assertEqual(record['source_sha256'], corpus.digest(script))
        self.assertEqual(record['configured_sha256'], corpus.digest(configured))
        self.assertEqual(record['variants'][0]['unprojected_arguments'],
                         ['--model-cores=simple'])

    def test_vendor_global_declarations_projection(self):
        script = (b'(push 1)(declare-const x Int)(pop 1)'
                  b'(assert (> x 0))(check-sat)(reset)(check-sat)')
        configured, record = suites.vendor_configuration(
            script, ['--global-declarations -i'])
        self.assertEqual(record['result'], 'valid')
        self.assertEqual(record['variants'][0]['options'],
                         {':global-declarations': 'true'})
        self.assertEqual(record['variants'][0]['unprojected_arguments'], ['-i'])
        self.assertEqual(len(record['insertions']), 2)
        self.assertEqual(configured.count(
            b'(set-option :global-declarations true)'), 2)
        self.assertEqual(record['source_sha256'], corpus.digest(script))
        for option in ['--no-global-declarations',
                       '--global-declarations=false']:
            configured, record = suites.vendor_configuration(script, [option])
            self.assertEqual(record['result'], 'valid')
            self.assertEqual(record['variants'][0]['options'],
                             {':global-declarations': 'false'})
        for variants in [['--global-declarations=invalid'],
                         ['--global-declarations',
                          '--no-global-declarations']]:
            configured, record = suites.vendor_configuration(script, variants)
            self.assertEqual(record['result'], 'unvalidated')
            self.assertEqual(configured, script)

    def test_vendor_configuration_does_not_merge_incompatible_variants(self):
        script = b'(check-sat)(get-model)'
        for variants in (['--produce-models', '--no-produce-models'],
                         ['--produce-models=perhaps'], ["'"]):
            configured, record = suites.vendor_configuration(script, variants)
            self.assertEqual(configured, script)
            self.assertEqual(record['result'], 'unvalidated')
        configured, record = suites.vendor_configuration(
            script, ['-qm', '--produce-models --check-models'])
        self.assertEqual(record['result'], 'valid')
        self.assertEqual(len(record['insertions']), 1)
        self.assertIn(b'(set-option :produce-models true)', configured)

    @unittest.skipUnless(shutil.which('z3'), 'reference requires Z3')
    def test_vendor_models_are_available_after_reset(self):
        script = (b'(declare-const x Int)(assert (= x 3))(check-sat)'
                  b'(get-model)(reset)(declare-const x Int)'
                  b'(assert (= x 4))(check-sat)(get-model)')
        configured, record = suites.vendor_configuration(
            script, ['--produce-models'])
        args = SimpleNamespace(reference=shutil.which('z3'),
                               reference_args=['-in', '-smt2'], timeout=5)
        result = suites.run_reference(args, configured)
        self.assertEqual(result['statuses'], ['sat', 'sat'])
        self.assertEqual(result['errors'], [])
        certificates = suites.validate_certificates(
            configured, result['stdout'].encode(), args)
        self.assertEqual([item['result'] for item in certificates],
                         ['valid', 'valid'])

    def test_z3_incremental_adaptation_preserves_logical_commands(self):
        script = (b'(set-option :incremental true)(set-logic QF_LIA)'
                  b'(declare-const x Int)(assert (= x 1))(push 1)'
                  b'(assert (> x 2))(check-sat)(pop 1)(check-sat)')
        adapted, record = suites.reference_script(script, 'z3')
        original = [part for _, _, part in suites.active_expressions(script)]
        actual = [part for _, _, part in suites.active_expressions(adapted)]
        self.assertEqual(original[1:], actual[1:])
        self.assertEqual(suites.command_name(actual[0]), 'set-info')
        self.assertEqual(record['source_sha256'], corpus.digest(script))
        self.assertEqual(record['adapted_sha256'], corpus.digest(adapted))
        self.assertEqual(len(record['transformations']), 1)
        self.assertEqual(record['transformations'][0]['source_command'],
                         original[0].decode())

    def test_reference_adaptation_is_narrow_and_explicit(self):
        for value in (b'0', b'yes', b'"true"', b'(true)', b'true extra'):
            script = b'(set-option :incremental ' + value + b')(check-sat)'
            self.assertEqual(suites.reference_script(script, 'z3'),
                             (script, None))
        script = (b'(set-option :incremental true)'
                  b'(set-option :global-negate true)(check-sat)')
        self.assertEqual(suites.reference_script(script), (script, None))
        self.assertEqual(suites.reference_script(script, 'cvc5'),
                         (script, None))
        adapted, _ = suites.reference_script(script, 'z3')
        self.assertIn(b'(set-option :global-negate true)', adapted)

    @unittest.skipUnless(shutil.which('z3'), 'reference requires Z3')
    def test_reference_timing_preserves_success_and_validates_models(self):
        script = (b'(reset)(set-option :print-success true)'
                  b'(set-option :incremental false)'
                  b'(declare-const x Int)(assert (= x 3))(check-sat)'
                  b'(get-model)')
        args = SimpleNamespace(reference=shutil.which('z3'),
                               reference_args=['-in', '-smt2'],
                               reference_profile='z3', timeout=5)
        solver = suites.QuerySolver(args.reference, args.reference_args, args)
        try:
            result = solver.run(script, auxiliary=True)
        finally:
            solver.close()
        self.assertEqual(result['result'], 'sat')
        self.assertEqual(result['stdout'].split().count('success'), 4)
        self.assertEqual(result['reference_adaptation']['source_sha256'],
                         corpus.digest(script))
        certificates = suites.validate_certificates(
            script, result['stdout'].encode(), args)
        self.assertEqual([item['result'] for item in certificates], ['valid'])
        self.assertIn('reference_adaptation', certificates[0]['oracle'])


@unittest.skipUnless(shutil.which("z3"), "error replay requires Z3")
class ErrorRecoveryTests(unittest.TestCase):
    def timing_args(self, directory):
        return SimpleNamespace(
            output=Path(directory), solver=shutil.which('z3'),
            solver_args=['-in', '-smt2'], z3=shutil.which('z3'),
            z3_args=['-in', '-smt2'], reference=shutil.which('z3'),
            reference_args=['-in', '-smt2'], timeout=5, repeats=2,
            instant_ms=1000)

    def time_script(self, directory, script):
        path = Path(directory) / 'case.smt2'
        path.write_bytes(script)
        case = dict(suites.analyze_script(script), path=path.name,
                    sha256=corpus.digest(script))
        return suites.time_case(case, self.timing_args(directory))

    def test_scrubbed_expected_error_requires_checked_recovery(self):
        script = (b'; EXPECT: rejected assertion\n; EXIT: 1\n'
                  b'(assert 1)(check-sat)')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        recovery = result['canonical_complete_script']['error_recovery']
        self.assertEqual(recovery['result'], 'valid')
        self.assertEqual(recovery['rejected_commands'], [0])
        self.assertEqual(recovery['recovery_statuses'], ['sat', 'unsat', 'sat'])

    @unittest.skipUnless(os.environ.get('VOX_TEST_SMT_CLI'),
                         'standalone solver requires VOX_TEST_SMT_CLI')
    def test_z3_option_error_does_not_mask_actual_rejection(self):
        script = (b'; EXPECT: rejected assertion\n; EXIT: 1\n'
                  b'(set-option :incremental true)(assert 1)(check-sat)')
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'case.smt2'
            path.write_bytes(script)
            case = dict(suites.analyze_script(script), path=path.name,
                        sha256=corpus.digest(script))
            args = self.timing_args(directory)
            args.solver = os.environ['VOX_TEST_SMT_CLI']
            result = suites.time_case(case, args)
        self.assertEqual(result['raw_complete_script']['result'], 'failed')
        complete = result['canonical_complete_script']
        self.assertEqual(complete['primary_attempt']['result'], 'failed')
        self.assertIn('reference_adaptation', complete['reference'])
        self.assertEqual(complete['error_recovery']['rejected_commands'], [1])
        self.assertEqual(result['result'], 'passed')
        self.assertTrue(result['queries'][0]['passed'])

    def test_scrubbed_partial_error_uses_validated_continued_execution(self):
        script = (b'; EXPECT: (error "Parse Error:\n; EXIT: 1\n'
                  b'(assert 1)(assert 2)(check-sat)')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        complete = result['canonical_complete_script']
        self.assertTrue(complete['expected_error_fragments'])
        self.assertEqual(complete['error_recovery']['rejected_commands'], [0, 1])

    def test_rejected_check_is_preserved_timed_and_counted(self):
        script = (b'; EXPECT: invalid assumption\n; EXIT: 1\n'
                  b'(set-info :status sat)(check-sat-assuming (3))')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        self.assertEqual(len(result['queries']), 1)
        query = result['queries'][0]
        self.assertTrue(query['check_rejected'])
        self.assertEqual(query['expected_response'], 'error')
        self.assertEqual(query['expected_error_commands'], [1])
        self.assertTrue(query['passed'])
        for samples in query['runs'].values():
            self.assertTrue(all(sample['result'] == 'complete'
                                and sample['expected_errors']
                                for sample in samples))

    def test_unvalidated_rejected_check_cannot_qualify(self):
        run = {'result': 'complete', 'seconds': 0.001,
               'response_kinds': ['error'], 'expected_errors': ['error']}
        result = suites.summarize_query(
            {'check_rejected': True}, {'ocaml': [run], 'z3': [run]},
            None, 10, error_protocol={'result': 'unvalidated'})
        self.assertFalse(result['correct'])
        self.assertFalse(result['passed'])

    def test_expected_answers_stay_aligned_after_rejected_check(self):
        script = (b'; EXPECT: (error "bad assumption")\n; EXPECT: unsat\n'
                  b'(set-info :status sat)(check-sat-assuming (3))'
                  b'(set-info :status unsat)(assert false)(check-sat)')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        self.assertEqual(len(result['queries']), 2)
        self.assertTrue(result['queries'][0]['check_rejected'])
        self.assertEqual(result['queries'][1]['expected'], 'unsat')
        self.assertEqual(result['canonical_complete_script']
                         ['rejected_check_ordinals'], [1])

    def test_no_check_rejection_has_a_measured_operation(self):
        for command in (b'(assert 1)', b'(get-model)(exit)',
                        b'(declare-fun f (Bool) Bool)'
                        b'(assert (f true false))'):
            script = b'; EXPECT: expected error\n; EXIT: 1\n' + command
            with tempfile.TemporaryDirectory() as directory:
                result = self.time_script(directory, script)
            self.assertEqual(result['result'], 'no_check_call')
            self.assertEqual(result['queries'], [])
            self.assertEqual(result['unmeasured_followups'], 0)
            self.assertTrue(result['rejection_operation']['passed'])
            self.assertEqual(result['rejection_operation']['expected_response'],
                             'error')

    def test_timing_preserves_validated_rejections_and_continues_checks(self):
        script = (b'; EXPECT: (error "invalid assertion")\n'
                  b'; EXPECT: sat\n; EXPECT: unsat\n; EXPECT: sat\n'
                  b'(assert 1)(check-sat)(push 1)(assert false)'
                  b'(check-sat)(pop 1)(check-sat)')
        seen = []
        original = suites.QuerySolver.run

        def capture(solver, incoming, **options):
            seen.append(incoming)
            return original(solver, incoming, **options)

        with tempfile.TemporaryDirectory() as directory, patch.object(
                suites.QuerySolver, 'run', capture):
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        self.assertEqual(len(result['queries']), 3)
        self.assertTrue(all(b'(assert 1)' in incoming for incoming in seen))
        for query in result['queries']:
            self.assertEqual(query['error_recovery']['result'], 'valid')
            self.assertEqual(query['error_recovery']['script_sha256'],
                             query['measurement_sha256'])
            for samples in query['runs'].values():
                self.assertTrue(all(len(sample['expected_errors']) == 1
                                    and not sample['errors']
                                    for sample in samples))

    def test_timing_replays_rejected_commands_and_success_option(self):
        examples = [
            (b'(get-info)(check-sat)', b'sat'),
            (b'(assert false)(reset 1)(check-sat)', b'unsat'),
            (b'(get-value ((! missing :named a)))(check-sat)', b'sat'),
            (b'(set-option :print-success true)(assert 1)(check-sat)', b'sat'),
        ]
        with tempfile.TemporaryDirectory() as directory:
            for commands, expected in examples:
                with self.subTest(commands=commands):
                    script = (b'; EXPECT: (error "invalid command")\n'
                              b'; EXPECT: ' + expected + b'\n' + commands)
                    result = self.time_script(directory, script)
                    self.assertEqual(result['result'], 'passed')
                    self.assertEqual(result['unmeasured_followups'], 0)

    def test_operation_times_and_validates_rejected_followup(self):
        script = (b'; EXPECT: sat\n; EXPECT: (error "invalid observation")\n'
                  b'(check-sat)(get-value (missing))')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        operation = result['queries'][0]['operation']
        self.assertTrue(operation['passed'])
        self.assertEqual(operation['error_recovery']['response_kinds'],
                         ['sat', 'error'])
        self.assertTrue(all(sample['expected_errors'] for samples in
                            operation['runs'].values() for sample in samples))
        self.assertTrue(all(item['result'] == 'valid'
                            for item in operation['certificates']))

    def test_rejected_followup_preserves_intervening_state_changes(self):
        data = (b'(declare-const p Bool)(check-sat)'
                b'(declare-const q Bool)(get-model)')
        query, script, operation = next(suites.query_scripts(data, [3]))
        self.assertEqual(operation, script
                         + b'(declare-const q Bool)\n(get-model)\n')
        self.assertEqual(query['followup_command_indices'], [3])
        self.assertEqual(query['operation_expected_error_commands'], [3])

    def test_unvalidated_rejection_cannot_pass_timing(self):
        script = (b'; EXPECT: (error "invalid assertion")\n'
                  b'; EXPECT: sat\n(assert 1)(check-sat)')
        with tempfile.TemporaryDirectory() as directory, patch.object(
                suites, 'timing_error_protocol',
                return_value={'result': 'unvalidated'}):
            result = self.time_script(directory, script)
        self.assertEqual(result['raw_complete_script']['result'], 'passed')
        self.assertEqual(result['result'], 'failed')
        self.assertFalse(result['queries'][0]['passed'])

    def test_error_transcript_is_bound_to_input_and_response_order(self):
        script = b'(reset)(assert 1)(check-sat)'
        args = self.timing_args('.')
        protocol = suites.timing_error_protocol(script, [b'(assert 1)'], args)
        self.assertEqual(protocol['result'], 'valid')
        solver = suites.QuerySolver(args.solver, args.solver_args, args)
        try:
            self.assertEqual(solver.run(script, error_protocol=protocol)
                             ['result'], 'sat')
            for changed in (
                    dict(protocol, result='unvalidated'),
                    dict(protocol, script_sha256='different'),
                    dict(protocol, response_kinds=['sat', 'error']),
                    dict(protocol, response_kinds=['error', 'error', 'sat']),
                    dict(protocol, response_kinds=['sat'])):
                self.assertEqual(solver.run(script, error_protocol=changed)
                                 ['result'], 'error')
        finally:
            solver.close()
        changed = suites.timing_error_protocol(script, [b'(assert 2)'], args)
        self.assertEqual(changed['result'], 'invalid')

    def test_expected_error_count_cannot_hide_additional_rejections(self):
        script = (b'; EXIT: 1\n; EXPECT: (error "invalid assertion")\n'
                  b'; EXPECT: sat\n(assert 1)(assert 2)(check-sat)')
        args = self.timing_args('.')
        result = suites.check_script(suites.analyze_script(script), args,
                                     script)
        self.assertEqual(result['result'], 'failed')
        self.assertEqual(result['error_recovery']['result'], 'valid')
        self.assertEqual(result['error_recovery']['rejected_commands'], [0, 1])

    def test_unannotated_errors_require_independent_replay(self):
        script = (b'(echo "begin")(assert 1)(check-sat)'
                  b'(echo "end")(assert false)(check-sat)(get-model)')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        self.assertEqual(result['raw_complete_script']['error_recovery']
                         ['rejected_commands'], [1, 6])
        for query in result['queries']:
            self.assertTrue(query['passed'])
        self.assertTrue(result['queries'][1]['operation']['passed'])

    def test_solver_only_error_does_not_become_expected(self):
        script = b'(declare-const p Bool)(assert p)(check-sat)'
        args = self.timing_args('.')
        original = suites.run_script

        def run(executable, arguments, incoming, *limits):
            if executable == 'tested':
                incoming = incoming.replace(b'(assert p)', b'(assert 1)')
                executable = args.reference
            return original(executable, arguments, incoming, *limits)

        args.solver = 'tested'
        with patch.object(suites, 'run_script', side_effect=run):
            result = suites.check_script(suites.analyze_script(script), args,
                                         script)
        self.assertEqual(result['result'], 'failed')
        self.assertNotIn('error_recovery', result)

    def test_suite_accepts_recovery_but_preserves_expected_answers(self):
        script = (b'; EXPECT: (error "invalid assertion")\n'
                  b'; EXPECT: sat\n(assert 1)(check-sat)')
        arguments = SimpleNamespace(reference=shutil.which('z3'),
                                    reference_args=['-in', '-smt2'],
                                    solver=shutil.which('z3'),
                                    solver_args=['-in', '-smt2'], timeout=5)
        case = suites.analyze_script(script)
        result = suites.check_script(case, arguments, script)
        self.assertEqual(result['result'], 'passed')
        self.assertEqual(result['error_recovery']['result'], 'valid')
        case['expected_statuses'] = ['unsat']
        result = suites.check_script(case, arguments, script)
        self.assertEqual(result['result'], 'failed')

    def test_suite_requires_validated_recovery(self):
        script = b'; EXPECT: (error "invalid assertion")\n(assert 1)'
        arguments = SimpleNamespace(reference=shutil.which('z3'),
                                    reference_args=['-in', '-smt2'],
                                    solver=shutil.which('z3'),
                                    solver_args=['-in', '-smt2'], timeout=5)
        case = suites.analyze_script(script)
        with patch.object(suites, 'validate_continued_errors',
                          return_value={'result': 'unvalidated'}):
            result = suites.check_script(case, arguments, script)
        self.assertEqual(result['result'],
                         'statuses_pass_error_recovery_unvalidated')

    def validate(self, script, transform=None):
        arguments = SimpleNamespace(reference=shutil.which('z3'),
                                    reference_args=['-in', '-smt2'],
                                    solver='tested', solver_args=['-in', '-smt2'],
                                    timeout=5)
        original = suites.run_script

        def run(executable, arguments, incoming, *limits):
            is_tested = executable == 'tested'
            if transform:
                incoming, override = transform(is_tested, incoming)
                if override is not None:
                    return override
            return original(shutil.which('z3'), arguments, incoming, *limits)

        with patch.object(suites, 'run_script', side_effect=run):
            tested = run('tested', arguments.solver_args, script, 5)
            return suites.validate_continued_errors(script, tested, arguments)

    def test_continued_errors_preserve_later_checks(self):
        script = (b'(assert 1)(check-sat)(push 1)(assert false)'
                  b'(assert 1)(check-sat)(pop 1)(check-sat)')
        result = self.validate(script)
        self.assertEqual(result['result'], 'valid')
        self.assertEqual(result['rejected_commands'], [0, 4])
        self.assertEqual(result['recovery_statuses'], ['sat', 'unsat', 'sat'])

    def test_error_recovery_keeps_echoed_protocol_keywords(self):
        for literal in (b'"success"', b'"unsupported"',
                        b'"(error ""echo"")"', b'""'):
            with self.subTest(literal=literal):
                script = (b'(set-option :print-success false)(assert 1)'
                          b'(echo ' + literal + b')(check-sat)')
                result = self.validate(script)
                self.assertEqual(result['result'], 'valid')
                self.assertEqual(result['rejected_commands'], [1])
                self.assertEqual(result['response_kinds'],
                                 ['error', 'string', 'sat'])
                self.assertEqual(result['certificates'][0]['result'], 'valid')

    def test_terminal_exit_is_omitted_only_from_recovery_diagnostic(self):
        for option in (b'', b'(set-option :print-success true)'):
            script = option + b'(assert 1)(check-sat)(exit)'
            seen = []

            def capture(tested, incoming):
                if tested:
                    seen.append(incoming)
                return incoming, None

            result = self.validate(script, capture)
            self.assertEqual(result['result'], 'valid')
            self.assertEqual(result['script_sha256'], corpus.digest(script))
            self.assertEqual(result['diagnostic_omitted_exit'],
                             3 if option else 2)
            self.assertEqual(result['response_kinds'],
                             ['success', 'error', 'sat', 'success']
                             if option else ['error', 'sat'])
            self.assertIn(script, seen)
            diagnostic = seen[-1]
            self.assertNotIn(b'(exit)', diagnostic)
            self.assertIn(b'(assert 1)', diagnostic)
            self.assertEqual(diagnostic.count(b'(check-sat)'), 4)

    def test_exit_ignores_unreachable_error_replay_input(self):
        for trailing in (b'(check-sat)', b'(', b')'):
            with self.subTest(trailing=trailing):
                script = b'(assert 1)(exit)' + trailing
                result = self.validate(script)
                self.assertEqual(result['result'], 'valid')
                self.assertEqual(result['script_sha256'], corpus.digest(script))
                self.assertEqual(result['rejected_commands'], [0])
                self.assertEqual(result['diagnostic_omitted_exit'], 1)

    def test_malformed_exit_is_rejected_and_retained(self):
        script = b'(assert 1)(check-sat)(exit 1)'
        seen = []

        def capture(tested, incoming):
            if tested:
                seen.append(incoming)
            return incoming, None

        result = self.validate(script, capture)
        self.assertEqual(result['result'], 'valid')
        self.assertEqual(result['rejected_commands'], [0, 2])
        self.assertNotIn('diagnostic_omitted_exit', result)
        self.assertIn(b'(exit 1)', seen[-1])

    def test_successful_models_after_rejection_are_certified(self):
        script = (b'(set-option :produce-models true)(declare-const p Bool)'
                  b'(assert p)(assert 1)(check-sat)(get-model)(get-value (p))')
        result = self.validate(script)
        self.assertEqual(result['result'], 'valid')
        self.assertEqual([item['result'] for item in result['certificates']],
                         ['valid', 'valid'])
        self.assertTrue(all(item['oracle']['statuses'] == ['sat']
                            for item in result['certificates']))

    def test_abstract_value_after_rejected_observation_is_certified(self):
        script = (b'(set-option :produce-models true)(declare-sort U 0)'
                  b'(declare-const x U)(check-sat-assuming (false))'
                  b'(get-value (x))(check-sat-assuming (true))(get-value (x))')
        original = suites.run_script

        def transform(tested, incoming):
            if tested and b'(get-value (x))' in incoming:
                result = original(shutil.which('z3'), ['-in', '-smt2'],
                                  incoming, 5)
                result['stdout'] = result['stdout'].replace(
                    'U!val!0', '(as @v0 U)')
                return incoming, result
            return incoming, None

        result = self.validate(script, transform)
        self.assertEqual(result['result'], 'valid')
        self.assertEqual(result['rejected_commands'], [4])
        self.assertEqual([item['result'] for item in result['certificates']],
                         ['valid'])

    def test_error_recovery_rejects_altered_or_unsupported_values(self):
        script = (b'(declare-const p Bool)(assert p)(assert 1)'
                  b'(check-sat)(get-value (p))')
        original = suites.run_script
        for output, expected in ((b'((p false))', 'invalid'),
                                 (b'unsupported', 'unvalidated')):
            def transform(tested, incoming):
                if tested and b'(get-value (p))' in incoming:
                    result = original(shutil.which('z3'), ['-in', '-smt2'],
                                      incoming, 5)
                    result['stdout'] = result['stdout'].replace(
                        '((p true))', output.decode())
                    return incoming, result
                return incoming, None

            result = self.validate(script, transform)
            self.assertEqual(result['result'], expected)
            self.assertEqual(result['reason'],
                             'error replay certificate validation')

    def test_error_recovery_rejects_altered_models(self):
        script = (b'(declare-const p Bool)(assert p)(assert 1)'
                  b'(check-sat)(get-model)')
        original = suites.run_script

        def transform(tested, incoming):
            if tested and b'(get-model)' in incoming:
                result = original(shutil.which('z3'), ['-in', '-smt2'],
                                  incoming, 5)
                result['stdout'] = result['stdout'].replace('true', 'false')
                return incoming, result
            return incoming, None

        result = self.validate(script, transform)
        self.assertEqual(result['result'], 'invalid')
        self.assertEqual(result['reason'],
                         'error replay certificate validation')

    def test_error_recovery_certificate_oracle_must_be_conclusive(self):
        script = (b'(declare-const p Bool)(assert p)(assert 1)'
                  b'(check-sat)(get-value (p))')
        for response in ('unknown', 'unsupported', '(error "unsupported")'):
            def transform(tested, incoming):
                if not tested and b'(assert (= p true))' in incoming:
                    parsed = response.encode()
                    return incoming, {
                        'returncode': 0, 'stdout': response,
                        'statuses': (['unknown'] if response == 'unknown'
                                     else []),
                        'errors': ([response] if response.startswith('(error')
                                   else []),
                        'response_kinds': [suites.response_kind(parsed)]}
                return incoming, None

            result = self.validate(script, transform)
            self.assertEqual(result['result'], 'unvalidated')
            self.assertEqual(result['reason'],
                             'error replay certificate validation')

    def test_timing_certifies_values_after_rejected_commands(self):
        script = (b'; EXPECT: (error "invalid assertion")\n; EXPECT: sat\n'
                  b'(declare-const p Bool)(assert p)(assert 1)'
                  b'(check-sat)(get-model)(get-value (p))(exit)')
        with tempfile.TemporaryDirectory() as directory:
            result = self.time_script(directory, script)
        self.assertEqual(result['result'], 'passed')
        self.assertEqual(result['raw_complete_script']['result'], 'passed')
        self.assertTrue(all(item['result'] == 'valid' for item in
                            result['raw_complete_script']['certificates']))
        operation = result['queries'][0]['operation']
        self.assertTrue(operation['passed'])
        self.assertTrue(all(item['result'] == 'valid'
                            for item in operation['certificates']))

    def test_silently_accepted_and_wrongly_rejected_commands_fail(self):
        script = b'(assert true)(assert 1)(check-sat)'
        for replacement in (b'', b'(assert true)'):
            def transform(tested, incoming):
                return (incoming.replace(b'(assert 1)', replacement)
                        if tested else incoming), None
            self.assertEqual(self.validate(script, transform)['result'],
                             'invalid')

        def wrong_rejection(tested, incoming):
            if tested:
                incoming = incoming.replace(b'(assert 1)', b'')
                incoming = incoming.replace(b'(assert true)', b'(assert 1)')
            return incoming, None
        self.assertEqual(self.validate(script, wrong_rejection)['result'],
                         'invalid')

    def test_extra_status_or_value_fails(self):
        script = b'(assert 1)'
        for extra in (b'(check-sat)', b'(get-info :name)'):
            def transform(tested, incoming):
                if tested and b'(assert 1)' in incoming:
                    incoming += extra
                return incoming, None
            self.assertEqual(self.validate(script, transform)['result'],
                             'invalid')

    def test_immediate_exit_violates_declared_continuation(self):
        def transform(tested, incoming):
            if tested and b'(assert 1)' in incoming:
                incoming = incoming.split(b'(assert 1)', 1)[0] + b'(assert 1)'
            return incoming, None
        self.assertEqual(self.validate(b'(assert 1)(check-sat)', transform)
                         ['result'], 'invalid')

    def test_failed_assertion_cannot_persist_false(self):
        def transform(tested, incoming):
            if tested:
                incoming = incoming.replace(b'(assert 1)',
                                            b'(assert false)(assert 1)')
            return incoming, None
        result = self.validate(b'(assert 1)', transform)
        self.assertEqual(result['result'], 'invalid')
        self.assertEqual(result['reason'], 'state after rejected command')

    def test_oracle_timeout_is_unvalidated(self):
        def transform(tested, incoming):
            return incoming, None if tested else {'timeout': True}
        result = self.validate(b'(assert 1)', transform)
        self.assertEqual(result['result'], 'unvalidated')
        self.assertEqual(result['reason'], 'error replay oracle')

    def test_unknown_or_auxiliary_answer_is_unvalidated(self):
        for extra in (b'(get-info :name)', b'(check-sat)'):
            def transform(tested, incoming):
                if not tested and extra == b'(check-sat)' and extra in incoming:
                    return incoming, {'returncode': 0, 'errors': [],
                                      'statuses': ['unknown'],
                                      'response_kinds': ['unknown']}
                return incoming, None
            result = self.validate(extra + b'(assert 1)', transform)
            self.assertEqual(result['result'], 'unvalidated')
            self.assertEqual(result['reason'],
                             'unsupported error replay response')

    def test_continued_policy_is_required(self):
        for response in ('(:error-behavior immediate-exit)', 'unsupported'):
            def transform(tested, incoming):
                override = ({'returncode': 0, 'errors': [], 'stdout': response}
                            if tested and incoming ==
                            b'(get-info :error-behavior)' else None)
                return incoming, override
            result = self.validate(b'(assert 1)', transform)
            self.assertEqual(result['result'], 'unvalidated')
            self.assertEqual(result['reason'], 'continued error policy')



@unittest.skipUnless(shutil.which("z3"), "certificate checks require Z3")
class CertificateTests(unittest.TestCase):
    def validate(self, script, output, *, core_semantics="standard"):
        arguments = SimpleNamespace(reference=shutil.which("z3"),
                                    reference_args=["-in", "-smt2"], timeout=5,
                                    core_semantics=core_semantics)
        return suites.validate_certificates(script, output, arguments)

    def test_script_file_is_streamed_to_solver_stdin(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'incremental.smt2'
            path.write_bytes(b'(check-sat)(assert false)(check-sat)')
            result = suites.run_script(shutil.which("z3"), ['-in', '-smt2'],
                                       path, 5)
        self.assertEqual(result["statuses"], ['sat', 'unsat'])

    def test_model_must_satisfy_assumptions(self):
        script = (b'(declare-const p Bool)(check-sat-assuming (p))'
                  b'(get-model)')
        good = self.validate(script, b'sat\n((define-fun p () Bool true))')
        bad = self.validate(script, b'sat\n((define-fun p () Bool false))')
        self.assertEqual(good[0]["result"], "valid")
        self.assertEqual(bad[0]["result"], "invalid")

    def test_incremental_models_ignore_popped_and_reset_declarations(self):
        script = (b'(declare-const a Bool)(reset)(declare-const p Bool)'
                  b'(push 1)(declare-const q Bool)(pop 1)'
                  b'(assert p)(check-sat)(get-model)')
        result = self.validate(script, b'sat\n((define-fun p () Bool true))')
        self.assertEqual(result[0]["result"], "valid")

    def test_implicit_scope_counts_in_models_and_cores(self):
        script = (b'(set-option :produce-unsat-cores true)'
                  b'(declare-const p Bool)(push)(declare-const q Bool)(pop)'
                  b'(assert p)(check-sat)(get-model)'
                  b'(push)(assert (! (not p) :named negative))'
                  b'(check-sat)(get-unsat-core)(pop)')
        result = self.validate(script,
                               b'sat\n((define-fun p () Bool true))'
                               b'\nunsat\n(negative)')
        self.assertEqual([item["result"] for item in result],
                         ["valid", "valid"])

    def test_abstract_model_values_are_distinct_within_each_sort(self):
        prefix = (b'(declare-sort S 0)(declare-const x S)(declare-const y S)')
        equal_model = (b'((define-fun x () S (as @v0 S))'
                       b'(define-fun y () S (as @v0 S)))')
        distinct_model = (b'((define-fun x () S (as @v0 S))'
                          b'(define-fun y () S (as @v1 S)))')
        for assertion, good, bad in [(b'(= x y)', equal_model, distinct_model),
                                     (b'(distinct x y)', distinct_model,
                                      equal_model)]:
            script = prefix + b'(assert ' + assertion + b')(check-sat)(get-model)'
            self.assertEqual(self.validate(script, b'sat\n' + good)[0]["result"],
                             "valid")
            self.assertEqual(self.validate(script, b'sat\n' + bad)[0]["result"],
                             "invalid")

    def test_abstract_values_in_nested_datatypes_and_sort_aliases(self):
        script = (b'(declare-sort S 1)(define-sort A () (S Int))'
                  b'(declare-datatype D ((box (val A))))'
                  b'(declare-const x D)(declare-const y D)'
                  b'(assert (= x y))(check-sat)(get-model)')
        model = (b'((define-fun x () D (box (as @v0 A)))'
                 b'(define-fun y () D (box (as @v1 (S Int)))))')
        self.assertEqual(self.validate(script, b'sat\n' + model)[0]["result"],
                         "invalid")
        model = model.replace(b'@v1', b'@v0')
        self.assertEqual(self.validate(script, b'sat\n' + model)[0]["result"],
                         "valid")

    def test_abstract_values_do_not_rewrite_function_parameters(self):
        script = (b'(declare-sort S 0)(declare-const a S)(declare-const b S)'
                  b'(declare-fun f (S) S)(assert (distinct a b))'
                  b'(assert (= (f a) a))(assert (= (f b) b))'
                  b'(check-sat)(get-model)')
        model = (b'((define-fun a () S (as @v0 S))'
                 b'(define-fun b () S (as @v1 S))'
                 b'(define-fun f ((@arg0 S)) S (as @arg0 S)))')
        self.assertEqual(self.validate(script, b'sat\n' + model)[0]["result"],
                         "valid")
        cyclic = (b'((define-fun a () S (as @helper S))'
                  b'(define-fun b () S (as @v1 S))'
                  b'(define-fun @helper () S (as @helper S))'
                  b'(define-fun f ((@arg0 S)) S (as @arg0 S)))')
        result = self.validate(script, b'sat\n' + cyclic)
        self.assertEqual(result[0]["result"], "unvalidated")
        self.assertIn("cyclic", result[0]["reason"])

    def test_abstract_value_labels_are_local_to_their_sort(self):
        script = (b'(declare-sort S 0)(declare-sort T 0)'
                  b'(declare-const x S)(declare-const y T)'
                  b'(check-sat)(get-value (x y))')
        value = b'sat\n((x (as @v0 S))(y (as @v0 T)))'
        self.assertEqual(self.validate(script, value)[0]["result"], "valid")
        bad = b'sat\n((x (as @v0 Int))(y (as @v0 T)))'
        self.assertEqual(self.validate(script, bad)[0]["result"], "unvalidated")

    def test_function_model_is_checked_at_every_argument(self):
        script = (b'(set-logic QF_UFLIA)'
                  b'(declare-fun f (Int) Int)(assert (= (f 2) 7))'
                  b'(check-sat)(get-model)')
        good = self.validate(script,
                             b'sat\n((define-fun f ((x Int)) Int (+ x 5)))')
        bad = self.validate(script,
                            b'sat\n((define-fun f ((x Int)) Int x))')
        self.assertEqual(good[0]["result"], "valid")
        self.assertEqual(bad[0]["result"], "invalid")

    def test_standard_core_retains_original_assumptions(self):
        script = (b'(set-option :produce-unsat-cores true)'
                  b'(declare-const p Bool)(assert (! (not p) :named negative))'
                  b'(check-sat-assuming (p))(get-unsat-core)')
        self.assertEqual(self.validate(script, b'unsat\n(negative)')[0]["result"],
                         "valid")
        self.assertEqual(self.validate(script, b'unsat\n()')[0]["result"],
                         "invalid")
        self.assertEqual(self.validate(script, b'unsat\n(negative p)')[0]["result"],
                         "unvalidated")
        self.assertEqual(self.validate(script, b'unsat\n(negative p)',
                                       core_semantics="z3-mixed")[0]["result"],
                         "valid")
        self.assertEqual(self.validate(script, b'unsat\n(negative)',
                                       core_semantics="z3-mixed")[0]["result"],
                         "invalid")

    def test_unsat_assumptions_and_named_core_must_agree(self):
        prefix = (b'(set-option :produce-unsat-cores true)'
                  b'(set-option :produce-unsat-assumptions true)'
                  b'(declare-const p Bool)(declare-const q Bool)'
                  b'(assert (! p :named a))(assert (! q :named b))'
                  b'(check-sat-assuming ((not p) (not q)))')
        result = self.validate(prefix + b'(get-unsat-core)'
                               b'(get-unsat-assumptions)',
                               b'unsat\n(a)\n((not p))')
        self.assertEqual([item["result"] for item in result], ["valid", "valid"])
        result = self.validate(prefix + b'(get-unsat-core)'
                               b'(get-unsat-assumptions)',
                               b'unsat\n(a)\n((not q))')
        self.assertEqual([item["result"] for item in result], ["valid", "invalid"])
        result = self.validate(prefix + b'(get-unsat-assumptions)'
                               b'(get-unsat-core)',
                               b'unsat\n((not q))\n(a)')
        self.assertEqual([item["result"] for item in result], ["valid", "invalid"])
        result = self.validate(prefix + b'(get-unsat-assumptions)',
                               b'unsat\n(p)')
        self.assertEqual(result[0]["result"], "unvalidated")

    def test_legacy_check_assumptions_are_retained(self):
        script = (b'(declare-const p Bool)(assert (not p))'
                  b'(check-sat p)(get-unsat-core)')
        result = self.validate(script, b'unsat\n()')
        self.assertEqual(result[0]["result"], "valid")

    def test_core_names_only_outer_assertion_annotations(self):
        script = (b'(declare-const p Bool)'
                  b'(assert (! (! p :named inner) :named |outer|))'
                  b'(assert (not p))(check-sat)(get-unsat-core)')
        self.assertEqual(self.validate(script, b'unsat\n(outer)')[0]["result"],
                         "valid")
        self.assertEqual(self.validate(script, b'unsat\n(inner)')[0]["result"],
                         "unvalidated")

    def test_empty_core_can_be_justified_by_unnamed_assertions(self):
        script = b'(assert false)(check-sat)(get-unsat-core)'
        self.assertEqual(self.validate(script, b'unsat\n()')[0]["result"],
                         "valid")

    def test_values_are_checked_against_original_constraints(self):
        script = (b'(declare-const n Int)(assert (> n 10))'
                  b'(check-sat)(get-value (n))')
        good = self.validate(script, b'sat\n((n 11))')
        bad = self.validate(script, b'sat\n((n 10))')
        self.assertEqual(good[0]["result"], "valid")
        self.assertEqual(bad[0]["result"], "invalid")

    def test_auxiliary_commands_do_not_inherit_model_validation(self):
        script = (b'(declare-const p Bool)(check-sat)(get-model)'
                  b'(get-info :name)')
        result = self.validate(script,
                               b'sat\n((define-fun p () Bool true))'
                               b'\n(:name "Vox SMT")')
        self.assertEqual([item["result"] for item in result],
                         ["valid", "unvalidated"])

    def test_option_observation_requires_the_current_explicit_value(self):
        script = (b'(set-option :auto-config false)(get-option :auto-config)'
                  b'(reset)(set-option :auto-config true)'
                  b'(get-option :auto-config)')
        self.assertEqual([x['result'] for x in self.validate(
            script, b'false\ntrue')], ['valid', 'valid'])
        self.assertEqual([x['result'] for x in self.validate(
            script, b'false\nfalse')], ['valid', 'invalid'])
        result = self.validate(b'(set-option :auto-config false)(reset)'
                               b'(get-option :auto-config)', b'false')
        self.assertEqual(result[0]['result'], 'unvalidated')

    def test_echo_cannot_hide_status_or_extra_response(self):
        script = b'(echo "sat")(check-sat)'
        for output in (b'"sat"\nsat', b'sat\nsat'):
            normalized = suites.normalize_echo_output(script, output)
            self.assertEqual(normalized, b'"sat"\nsat')
            self.assertEqual(self.validate(script, output)[0]['result'],
                             'valid')
        self.assertEqual(self.validate(script, b'sat')[0]['result'], 'invalid')
        result = self.validate(script, b'"sat"\nsat\nsat')
        self.assertEqual(result[-1]['reason'], 'extra response')
        result = self.validate(script, b'"different"\nsat')
        self.assertEqual(result[0]['result'], 'invalid')

    def test_echo_keywords_follow_silent_commands(self):
        for literal in (b'"success"', b'"unsupported"', b'"(error ""echo"")"',
                        b'"sat"', b'""', b'"a ""quote""\nnext line"'):
            for prefix in (b'(declare-const p Bool)',
                           b'(set-option :print-success false)(assert true)',
                           b'(set-option :print-success true)'
                           b'(set-option :print-success false)'):
                with self.subTest(literal=literal, prefix=prefix):
                    script = prefix + b'(echo ' + literal + b')(check-sat)'
                    result = suites.run_script(shutil.which('z3'),
                                               ['-in', '-smt2'], script, 5)
                    kinds = ['string', 'sat']
                    if b':print-success true' in prefix:
                        kinds.insert(0, 'success')
                    self.assertEqual(result['response_kinds'], kinds)
                    self.assertEqual(result['statuses'], ['sat'])
                    self.assertEqual(result['errors'], [])
                    self.assertEqual(self.validate(
                        script, result['stdout'].encode())[0]['result'],
                        'valid')

    def test_echo_empty_and_multiline_sequences(self):
        script = b'(echo "")(echo "")(echo "\n")(echo "success")(check-sat)'
        output = b'\n\n\n\nsuccess\nsat\n'
        expected = b'""\n""\n"\n"\n"success"\nsat\n'
        self.assertEqual(suites.normalize_echo_output(script, output), expected)
        self.assertTrue(all(item['result'] == 'valid'
                            for item in self.validate(script, output)))

    def test_missing_empty_echo_is_not_inserted(self):
        for script, output in ((b'(echo "")', b''),
                               (b'(echo "")(echo "")', b'\n'),
                               (b'(echo "\n")', b'\n')):
            with self.subTest(script=script, output=output):
                self.assertEqual(suites.normalize_echo_output(script, output),
                                 output)
                self.assertEqual(self.validate(script, output)[0]['result'],
                                 'invalid')

    def test_echo_success_preserves_response_failures(self):
        script = b'(set-option :print-success false)(echo "success")(check-sat)'
        for output in (b'', b'success', b'sat', b'success\nsuccess\nsat',
                       b'success\nsat\nsat', b'"different"\nsat'):
            with self.subTest(output=output):
                self.assertTrue(any(item['result'] != 'valid'
                                    for item in self.validate(script, output)))

    def test_echo_normalization_accepts_both_reset_option_policies(self):
        script = (b'(set-option :print-success true)(reset)'
                  b'(declare-const p Bool)(echo "success")(check-sat)')
        for acknowledgments in (1, 3):
            output = b'success\n' * (acknowledgments + 1) + b'sat\n'
            expected = (b'success\n' * acknowledgments
                        + b'"success"\nsat\n')
            self.assertEqual(suites.normalize_echo_output(script, output),
                             expected)

    def test_malformed_option_does_not_change_echo_alignment(self):
        script = (b'(set-option :print-success false)'
                  b'(set-option :print-success true extra)'
                  b'(echo "success")(check-sat)')
        output = b'(error "arity")\nsuccess\nsat\n'
        self.assertEqual(suites.normalize_echo_output(script, output),
                         b'(error "arity")\n"success"\nsat\n')

    def test_echo_normalization_preserves_raw_transcript(self):
        script = (b'(declare-const p Bool)(echo ") (hello world")'
                  b'(check-sat)(echo "a ""quote""\nnext line")')
        result = suites.run_script(shutil.which('z3'), ['-in', '-smt2'],
                                   script, 5)
        self.assertEqual(result['statuses'], ['sat'])
        self.assertEqual(result['response_kinds'], ['string', 'sat', 'string'])
        self.assertIn(') (hello world', result['raw_stdout'])
        self.assertTrue(all(x['result'] == 'valid' for x in self.validate(
            script, result['stdout'].encode())))

    def test_exit_ends_query_inventory_and_certificates(self):
        script = (b'(check-sat)(exit)(assert false)(check-sat)'
                  b'(get-model)')
        case = suites.analyze_script(script)
        self.assertEqual(case['checks'], 1)
        self.assertEqual(case['commands'], {'check-sat': 1, 'exit': 1})
        self.assertEqual(case['expected_statuses'], [None])
        self.assertEqual(self.validate(script, b'sat'), [])

    def test_assignment_values_are_jointly_checked(self):
        script = (b'(set-option :produce-assignments true)'
                  b'(declare-const p Bool)'
                  b'(assert (or (! p :named a) (! (not p) :named |b|)))'
                  b'(check-sat)(get-assignment)')
        for result in (b'((a true) (b false))', b'((|b| true) (a false))'):
            self.assertEqual(self.validate(script, b'sat\n' + result)[0][
                'result'], 'valid')
        self.assertEqual(self.validate(script, b'sat\n((a true) (b true))')[0][
            'result'], 'invalid')
        for result in (b'((a true))', b'((a true) (b false) (p true))',
                       b'((a true) (a false) (b true))', b'((a p) (b true))'):
            self.assertEqual(self.validate(script, b'sat\n' + result)[0][
                'result'], 'unvalidated')

    def test_assignment_inventory_tracks_context_and_boolean_sorts(self):
        script = (b'(declare-const p Bool)(assert (! p :named old))'
                  b'(reset)(declare-const p Bool)'
                  b'(assert (= (! 1 :named number) 1))'
                  b'(push 1)(assert (! p :named popped))(pop 1)'
                  b'(assert (or (! p :named a) (not p)))'
                  b'(check-sat-assuming ((not p)))(get-assignment)')
        self.assertEqual(self.validate(script, b'sat\n((a false))')[0][
            'result'], 'valid')
        self.assertEqual(self.validate(script, b'sat\n((a true))')[0][
            'result'], 'invalid')
        self.assertEqual(self.validate(script, b'sat\n()')[0][
            'result'], 'unvalidated')
        script += b'(reset-assertions)(check-sat)(get-assignment)'
        result = self.validate(script, b'sat\n((a false))\nsat\n()')
        self.assertEqual([item['result'] for item in result],
                         ['valid', 'valid'])

    def test_assignment_tracks_global_annotations_and_definitions(self):
        script = (b'(set-option :global-declarations true)'
                  b'(push 1)(define-fun f () Bool (! true :named a))(pop 1)'
                  b'(reset-assertions)(set-option :produce-models false)'
                  b'(check-sat)(get-assignment)')
        self.assertEqual(self.validate(script, b'sat\n((a true))')[0][
            'result'], 'valid')
        self.assertEqual(self.validate(script, b'sat\n((a false))')[0][
            'result'], 'invalid')
        script += b'(reset)(check-sat)(get-assignment)'
        result = self.validate(script, b'sat\n((a true))\nsat\n()')
        self.assertEqual([item['result'] for item in result],
                         ['valid', 'valid'])

    def test_assignments_include_names_from_assumptions_and_value_queries(self):
        script = (b'(declare-const p Bool)'
                  b'(check-sat-assuming ((! p :named a)))(get-assignment)'
                  b'(get-value ((! (not p) :named b)))(get-assignment)')
        good = (b'sat\n((a true))\n(((! (not p) :named b) false))'
                b'\n((a true) (b false))')
        self.assertEqual([r['result'] for r in self.validate(script, good)],
                         ['valid'] * 3)
        missing = good.replace(b'((a true))', b'()', 1)
        self.assertEqual(self.validate(script, missing)[0]['result'],
                         'unvalidated')

    def test_model_observations_must_share_one_model(self):
        prefix = (b'(declare-const p Bool)'
                  b'(assert (or (! p :named a) (not p)))(check-sat)')
        for first, first_answer in [
                (b'(get-assignment)', b'((a true))'),
                (b'(get-value (p))', b'((p true))'),
                (b'(get-model)', b'((define-fun p () Bool true))')]:
            script = prefix + first + b'(get-assignment)'
            good = b'sat\n' + first_answer + b'\n((a true))'
            bad = b'sat\n' + first_answer + b'\n((a false))'
            self.assertEqual([r['result'] for r in self.validate(script, good)],
                             ['valid', 'valid'])
            self.assertEqual([r['result'] for r in self.validate(script, bad)],
                             ['valid', 'invalid'])
            script += b'(check-sat)(get-assignment)'
            output = good + b'\nsat\n((a false))'
            self.assertEqual([r['result'] for r in
                              self.validate(script, output)],
                             ['valid'] * 3)

    def test_value_queries_use_current_names_and_prior_abstract_values(self):
        script = (b'(declare-const p Bool)(check-sat)'
                  b'(get-value ((and (! p :named a) a)))')
        output = b'sat\n(((and (! p :named a) a) true))'
        self.assertEqual(self.validate(script, output)[0]['result'], 'valid')
        script = (b'(declare-sort U 0)(declare-const x U)(check-sat)'
                  b'(get-value (x))(get-value ((= x (as @v0 U))))')
        output = b'sat\n((x (as @v0 U)))\n(((= x (as @v0 U)) true))'
        self.assertEqual([r['result'] for r in self.validate(script, output)],
                         ['valid', 'valid'])
        output = output.replace(b'true', b'false')
        self.assertEqual([r['result'] for r in self.validate(script, output)],
                         ['valid', 'invalid'])

    def test_overloaded_models_require_every_declared_signature(self):
        script = (b'(declare-fun f (Int) Int)(declare-fun |f| (Bool) Int)'
                  b'(assert (= (f 0) 7))(assert (= (f true) 5))'
                  b'(check-sat)(get-model)')
        first = b'(define-fun f ((x Int)) Int 7)'
        second = b'(define-fun |f| ((x Bool)) Int 5)'
        for definitions in (first + second, second + first):
            self.assertEqual(self.validate(script, b'sat\n(' + definitions
                                           + b')')[0]['result'], 'valid')
        for definitions in (first, second, b''):
            result = self.validate(script, b'sat\n(' + definitions + b')')[0]
            self.assertEqual(result['result'], 'unvalidated')
            self.assertIn('omits', result['reason'])
        bad = b'sat\n(' + first.replace(b'Int 7', b'Int 8') + second + b')'
        self.assertEqual(self.validate(script, bad)[0]['result'], 'invalid')

    def test_overloaded_models_match_domain_and_result_sorts(self):
        script = (b'(declare-const x Int)(declare-const x Bool)'
                  b'(declare-fun f (Int) Int)(declare-fun f (Int) Bool)'
                  b'(assert (= (as x Int) 3))(assert (as x Bool))'
                  b'(assert (= ((as f Int) 0) 7))'
                  b'(assert ((as f Bool) 0))(check-sat)(get-model)')
        definitions = [b'(define-fun x () Int 3)',
                       b'(define-fun |x| () Bool true)',
                       b'(define-fun f ((x Int)) Int 7)',
                       b'(define-fun f ((x Int)) Bool true)']
        model = b'(' + b''.join(definitions) + b')'
        self.assertEqual(self.validate(script, b'sat\n' + model)[0]['result'],
                         'valid')
        for index in range(len(definitions)):
            partial = b'(' + b''.join(definitions[:index]
                                     + definitions[index + 1:]) + b')'
            self.assertEqual(self.validate(script, b'sat\n' + partial)[0][
                'result'], 'unvalidated')
        duplicate = model[:-1] + b'(define-fun |f| ((y Int)) Int 7))'
        self.assertEqual(self.validate(script, b'sat\n' + duplicate)[0][
            'result'], 'unvalidated')
        extra = model[:-1] + b'(define-fun f ((y Bool)) Bool true))'
        self.assertEqual(self.validate(script, b'sat\n' + extra)[0]['result'],
                         'unvalidated')
        wrong = model.replace(b'Bool true', b'Bool false')
        self.assertEqual(self.validate(script, b'sat\n' + wrong)[0]['result'],
                         'invalid')

    def test_overloaded_declaration_scopes_are_independent(self):
        script = (b'(declare-fun f (Int) Int)(push 1)'
                  b'(declare-fun f (Bool) Int)(pop 1)'
                  b'(assert (= (f 0) 7))(check-sat)(get-model)'
                  b'(reset-assertions)(check-sat)(get-model)')
        output = b'sat\n((define-fun f ((x Int)) Int 7))\nsat\n()'
        self.assertEqual([r['result'] for r in self.validate(script, output)],
                         ['valid'] * 2)
        script = (b'(set-option :global-decls true)'
                  b'(declare-fun f (Int) Int)(push 1)'
                  b'(declare-fun f ((_ BitVec 3)) Int)(pop 1)'
                  b'(assert (= (f 0) 7))(assert (= (f #b000) 5))'
                  b'(check-sat)(get-model)(reset-assertions)'
                  b'(check-sat)(get-model)(reset)(check-sat)(get-model)')
        integer = b'(define-fun f ((x Int)) Int 7)'
        bits = b'(define-fun f ((x (_ BitVec 3))) Int 5)'
        model = b'(' + integer + bits + b')'
        output = b'sat\n' + model + b'\nsat\n' + model + b'\nsat\n()'
        self.assertEqual([r['result'] for r in self.validate(script, output)],
                         ['valid'] * 3)
        partial = output.replace(integer, b'', 1)
        self.assertEqual(self.validate(script, partial)[0]['result'],
                         'unvalidated')

    def test_overloaded_model_signatures_expand_sort_aliases(self):
        script = (b'(define-sort Number () Int)(define-sort Flag () Bool)'
                  b'(declare-fun f (Number) Number)'
                  b'(declare-fun f (Flag) Int)'
                  b'(assert (= (f 0) 7))(assert (= (f true) 5))'
                  b'(check-sat)(get-model)')
        model = (b'((define-fun f ((x Int)) Int 7)'
                 b'(define-fun f ((x Bool)) Number 5))')
        self.assertEqual(self.validate(script, b'sat\n' + model)[0]['result'],
                         'valid')
        duplicate = model[:-1] + b'(define-fun |f| ((y Flag)) Int 5))'
        self.assertEqual(self.validate(script, b'sat\n' + duplicate)[0][
            'result'], 'unvalidated')

    def test_overloaded_helpers_are_checked_and_cannot_change(self):
        script = (b'(declare-fun f (Int) Int)(declare-fun f (Bool) Int)'
                  b'(assert (= (f 0) 7))(assert (= (f true) 5))'
                  b'(check-sat)(get-model)(get-model)')
        helper = (b'(define-fun h ((x Int)) Int 7)'
                  b'(define-fun h ((x Bool)) Int 5)')
        definitions = (b'(define-fun f ((x Int)) Int (h x))'
                       b'(define-fun f ((x Bool)) Int (h x))')
        model = b'(' + definitions + helper + b')'
        self.assertEqual([r['result'] for r in self.validate(
            script, b'sat\n' + model + model)], ['valid', 'valid'])
        changed = model.replace(b'Int 7', b'Int 8')
        result = self.validate(script, b'sat\n' + model + changed)
        self.assertEqual(result[1]['result'], 'unvalidated')
        self.assertIn('conflicting model helper', result[1]['reason'])
        cyclic = (b'((define-fun f ((x Int)) Int (f true))'
                  b'(define-fun f ((x Bool)) Int (f 0)))')
        self.assertEqual(self.validate(script, b'sat\n' + cyclic + cyclic)[0][
            'result'], 'unvalidated')

    def test_overloads_cannot_hide_cycles_through_input_definitions(self):
        script = (b'(declare-fun f (Int) Int)'
                  b'(define-fun f ((x Bool)) Int (f 0))'
                  b'(assert (= (f 0) 7))(check-sat)(get-model)')
        output = b'sat\n((define-fun f ((x Int)) Int (f true)))'
        self.assertEqual(self.validate(script, output)[0]['result'],
                         'unvalidated')

    def test_overloaded_models_agree_with_all_value_observations(self):
        script = (b'(declare-fun f (Int) Int)(declare-fun f (Bool) Int)'
                  b'(check-sat)(get-value ((f 0) (f true)))(get-model)')
        values = b'sat\n(((f 0) 7) ((f true) 5))\n'
        model = (b'((define-fun f ((x Int)) Int 7)'
                 b'(define-fun f ((x Bool)) Int 5))')
        self.assertEqual([r['result'] for r in self.validate(
            script, values + model)], ['valid', 'valid'])
        for altered in (model.replace(b'Int 7', b'Int 8'),
                        model.replace(b'Int 5', b'Int 6')):
            self.assertEqual(self.validate(script, values + altered)[1][
                'result'], 'invalid')

    def test_model_definition_signatures_are_checked(self):
        script = (b'(declare-const x Int)(assert (= x 0))'
                  b'(check-sat)(get-model)')
        for definition in [b'(define-fun x () Bool 0)',
                           b'(define-fun x () Real 0.0)',
                           b'(define-fun x () NoSuchSort 0)',
                           b'(define-fun x ((p Int)) Int 0)']:
            result = self.validate(script, b'sat\n(' + definition + b')')
            self.assertEqual(result[0]['result'], 'unvalidated')
        script = b'(declare-fun f (Int) Int)(check-sat)(get-model)'
        for definition in [b'(define-fun f ((x Bool)) Int 0)',
                           b'(define-fun f ((x Int)) Bool true)',
                           b'(define-fun f () Int 0)']:
            result = self.validate(script, b'sat\n(' + definition + b')')
            self.assertEqual(result[0]['result'], 'unvalidated')
        script = (b'(define-sort Number () Int)(declare-fun f (Number) Number)'
                  b'(check-sat)(get-model)')
        result = self.validate(script, b'sat\n((define-fun f ((x Int)) Int 0))')
        self.assertEqual(result[0]['result'], 'valid')

    def test_quoted_model_names_cannot_hide_definition_cycles(self):
        script = (b'(declare-const x Int)(assert (= x 7))'
                  b'(check-sat)(get-model)')
        for definition in [b'(define-fun x () Int |x|)',
                           b'(define-fun |x| () Int x)',
                           b'(define-fun x () Int (let ((z |x|)) z))']:
            result = self.validate(script, b'sat\n(' + definition + b')')
            self.assertEqual(result[0]['result'], 'unvalidated')
        result = self.validate(script, b'sat\n((define-fun |x| () Int 7))')
        self.assertEqual(result[0]['result'], 'valid')
        result = self.validate(script, b'sat\n((define-fun x () Int 7)'
                               b'(define-fun |x| () Int 7))')
        self.assertEqual(result[0]['result'], 'unvalidated')

    def test_model_bodies_cannot_hide_cycles_through_input_aliases(self):
        for prefix, value in [
                (b'(define-fun d () Int x)', b'd'),
                (b'(define-const d Int x)', b'd'),
                (b'(assert (= (! x :named d) 7))', b'd'),
                (b'(define-fun d () Int x)', b'|d|')]:
            script = (b'(declare-const x Int)' + prefix
                      + b'(assert (= x 7))(check-sat)(get-model)')
            output = b'sat\n((define-fun x () Int ' + value + b'))'
            self.assertEqual(self.validate(script, output)[0]['result'],
                             'unvalidated')

    def test_named_observations_can_refer_to_prior_abstract_values(self):
        script = (b'(declare-sort U 0)(declare-const x U)(check-sat)'
                  b'(get-value (x))'
                  b'(get-value ((! (= x (as @v0 U)) :named a)))'
                  b'(get-value (a))(get-assignment)')
        output = (b'sat\n((x (as @v0 U)))'
                  b'\n(((! (= x (as @v0 U)) :named a) true))'
                  b'\n((a true))\n((a true))')
        self.assertEqual([r['result'] for r in self.validate(script, output)],
                         ['valid'] * 4)
        output = output.replace(b':named a) true)', b':named a) false)')
        self.assertEqual(self.validate(script, output)[1]['result'], 'invalid')

    def test_named_abstract_observations_preserve_distinctness(self):
        script = (b'(declare-sort U 0)(declare-const x U)(check-sat)'
                  b'(get-value (x))'
                  b'(get-value ((! (= x (as @v1 U)) :named a)))')
        for value, result in [(b'false', 'valid'), (b'true', 'invalid')]:
            output = (b'sat\n((x (as @v0 U)))'
                      b'\n(((! (= x (as @v1 U)) :named a) ' + value + b'))')
            self.assertEqual([r['result'] for r in self.validate(script, output)],
                             ['valid', result])

    def test_named_abstract_observations_avoid_input_name_collisions(self):
        script = (b'(declare-sort U 0)(declare-const @v0 U)'
                  b'(declare-const x U)(assert (distinct x @v0))(check-sat)'
                  b'(get-value (x))'
                  b'(get-value ((! (= x (as @v0 U)) :named a)))')
        output = (b'sat\n((x (as @v1 U)))'
                  b'\n(((! (= x (as @v0 U)) :named a) false))')
        self.assertEqual([r['result'] for r in self.validate(script, output)],
                         ['valid', 'valid'])

    def test_values_cannot_use_input_definitions_or_quoted_aliases(self):
        for script, output in [
                (b'(declare-const |p| Bool)(check-sat)(get-value (|p|))',
                 b'sat\n((|p| p))'),
                (b'(declare-const p Bool)(assert (or (! p :named a) (not p)))'
                 b'(check-sat)(get-value (p))', b'sat\n((p a))'),
                (b'(define-fun a () Bool true)(check-sat)(get-value (a))',
                 b'sat\n((a a))')]:
            self.assertEqual(self.validate(script, output)[0]['result'],
                             'unvalidated')

    def test_repeated_models_and_abstract_observations_are_joint(self):
        script = (b'(declare-const p Bool)(check-sat)(get-model)(get-model)')
        first = b'((define-fun p () Bool true))'
        good = b'sat\n' + first + b'\n' + first
        bad = b'sat\n' + first + b'\n((define-fun p () Bool false))'
        self.assertEqual([r['result'] for r in self.validate(script, good)],
                         ['valid', 'valid'])
        self.assertEqual([r['result'] for r in self.validate(script, bad)],
                         ['valid', 'invalid'])
        script = (b'(declare-sort U 0)(declare-const x U)(declare-const y U)'
                  b'(assert (= x y))(check-sat)(get-model)(get-value (y))')
        model = (b'sat\n((define-fun x () U (as @v0 U))'
                 b'(define-fun y () U (as @v0 U)))\n')
        self.assertEqual([r['result'] for r in self.validate(
            script, model + b'((y (as @v0 U)))')], ['valid', 'valid'])
        self.assertEqual([r['result'] for r in self.validate(
            script, model + b'((y (as @v1 U)))')], ['valid', 'invalid'])

    def test_abstract_values_share_identity_between_observations(self):
        script = (b'(declare-sort U 0)(declare-const x U)(declare-const y U)'
                  b'(assert (= x y))(check-sat)(get-value (x))'
                  b'(get-value (y))')
        good = b'sat\n((x (as @v0 U)))\n((y (as @v0 U)))'
        bad = b'sat\n((x (as @v0 U)))\n((y (as @v1 U)))'
        self.assertEqual([r['result'] for r in self.validate(script, good)],
                         ['valid', 'valid'])
        self.assertEqual([r['result'] for r in self.validate(script, bad)],
                         ['valid', 'invalid'])

    def test_assertion_inventory_rejects_malformed_and_added_annotations(self):
        script = b'(declare-const p Bool)(assert p)(get-assertions)'
        for output in (b'((! p))', b'((! p extra garbage))'):
            self.assertEqual(self.validate(script, output)[0]['result'],
                             'unvalidated')
        self.assertEqual(self.validate(script, b'((! p :added value))')[0][
            'result'], 'invalid')
        script = (b'(declare-const p Bool)'
                  b'(assert (! (! p :named n) :custom value))'
                  b'(get-assertions)')
        for output in (b'((! p :named n))',
                       b'((! (! p :named n) :custom value))'):
            self.assertEqual(self.validate(script, output)[0]['result'],
                             'valid')
        output = b'((! (! p :named n) :custom changed))'
        self.assertEqual(self.validate(script, output)[0]['result'], 'invalid')

    def test_assertion_inventory_preserves_original_terms_and_annotations(self):
        script = (b'(declare-const p Bool)'
                  b'(assert (! (or p (not p)) :named a :custom value))'
                  b'(push 1)(assert false)(pop 1)(get-assertions)')
        for result in (b'((! (or p (not p)) :named a :custom value))',
                       b'((! (or p (not p)) :named a))'):
            self.assertEqual(self.validate(script, result)[0]['result'],
                             'valid')
        for result in (b'()', b'(true)', b'((or p (not p)))',
                       b'((! (or p (not p)) :named a) false)'):
            self.assertEqual(self.validate(script, result)[0]['result'],
                             'invalid')
        script += b'(reset-assertions)(get-assertions)(assert false)(reset)'
        script += b'(get-assertions)'
        result = self.validate(script,
                               b'((! (or p (not p)) :named a))\n()\n()')
        self.assertEqual([item['result'] for item in result], ['valid'] * 3)

    def test_uninterpreted_model_values_remain_unvalidated(self):
        script = (b'(declare-sort U 0)(declare-const a U)'
                  b'(check-sat)(get-model)')
        result = self.validate(script,
                               b'sat\n((define-fun a () U U!val!0))')
        self.assertEqual(result[0]["result"], "unvalidated")

    def test_unfixed_model_helpers_remain_unvalidated(self):
        script = (b'(declare-const x Int)(assert (> x 0))'
                  b'(check-sat)(get-model)')
        result = self.validate(script, b'sat\n((declare-fun helper () Int)'
                               b'(define-fun x () Int helper))')
        self.assertEqual(result[0]["result"], "unvalidated")

    def test_get_value_cannot_repeat_an_unfixed_term_as_its_value(self):
        script = b'(declare-const p Bool)(check-sat)(get-value (p))'
        result = self.validate(script, b'sat\n((p p))')
        self.assertEqual(result[0]["result"], "unvalidated")


class WarmSessionTests(unittest.TestCase):
    def fixture(self, directory):
        commands = [b'(declare-const p Bool)', b'(check-sat)',
                    b'(get-model)', b'(push 1)', b'(assert p)',
                    b'(check-sat-assuming ((not p)))', b'(pop 1)',
                    b'(check-sat)', b'(get-model)', b'(exit)']
        data = b'\n'.join(commands) + b'\n'
        session = directory / 'sessions' / 'one'
        session.mkdir(parents=True)
        (session / 'input.smt2').write_bytes(data)
        outgoing = b'sat\n(model)\nunsat\nsat\n(model)\n'
        (session / 'stdout').write_bytes(outgoing)
        queries = []
        begin = 0
        for index, command in enumerate(commands):
            end = begin + len(command)
            if index in (5, 7):
                queries.append(dict(session='one', id=str(index),
                                    command_start=begin, command_end=end,
                                    command_sha256=benchmark.hashlib.sha256(
                                        command).hexdigest(),
                                    result='unsat' if index == 5 else 'sat'))
            begin = end + 1
        (directory / 'queries.jsonl').write_text(''.join(
            json.dumps(query) + '\n' for query in queries))
        (directory / 'sessions.jsonl').write_text(json.dumps(dict(
            id='one', input_sha256=benchmark.hashlib.sha256(data).hexdigest(),
            stdout_sha256=benchmark.hashlib.sha256(outgoing).hexdigest())))
        return data

    def test_original_checks_models_scopes_and_offsets(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            data = self.fixture(directory)
            items = list(benchmark.session_inputs(directory))
            self.assertEqual(len(items), 1)
            _, _, chunks = items[0]
            self.assertEqual(len(chunks), 3)
            self.assertIsNone(chunks[0][0])
            self.assertEqual([chunk[4] for chunk in chunks],
                             ['sat', 'unsat', 'sat'])
            self.assertEqual(b''.join(chunk[1] for chunk in chunks),
                             data[:chunks[-1][3]])
            self.assertIn(b'(check-sat)', chunks[0][1])
            self.assertIn(b'(get-model)', chunks[1][1])
            self.assertIn(b'(push 1)', chunks[1][1])
            self.assertIn(b'(pop 1)', chunks[2][1])
            self.assertNotIn(b'(reset)', b''.join(c[1] for c in chunks))

    def test_session_integrity(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            data = self.fixture(directory)
            incoming = directory / 'sessions/one/input.smt2'
            incoming.write_bytes(data + b' ')
            with self.assertRaisesRegex(ValueError, 'input hash mismatch'):
                list(benchmark.session_inputs(directory))
            incoming.write_bytes(data)
            index = directory / 'queries.jsonl'
            queries = [json.loads(line) for line in index.read_text().splitlines()]
            queries[0]['command_sha256'] = '0' * 64
            index.write_text(''.join(json.dumps(q) + '\n' for q in queries))
            with self.assertRaisesRegex(ValueError, 'check hash or offset'):
                list(benchmark.session_inputs(directory))

    def test_repeats_replay_the_session_and_unselected_checks(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            self.fixture(directory)
            item, = benchmark.session_inputs(directory)
            solvers = {name: Mock() for name in ('ocaml', 'z3')}
            for solver in solvers.values():
                solver.memory.return_value = {}
                solver.run_warm.side_effect = [('sat', .001, [], []),
                                               ('unsat', .002, [], []),
                                               ('sat', .003, [], [])] * 2
            rows = list(benchmark.measure_session(
                0, item, solvers, 2, 10, 'unlimited'))
            self.assertEqual([row['id'] for row in rows], ['5', '7'])
            self.assertTrue(all(row['pass'] for row in rows))
            for solver in solvers.values():
                self.assertEqual(solver.close.call_count, 2)
                scripts = [call.args[0]
                           for call in solver.run_warm.call_args_list]
                self.assertEqual(scripts[:3], scripts[3:])
                self.assertIn(b'(get-model)', scripts[1])
                self.assertIn(b'(set-option :rlimit 0)', scripts[0])

    def test_original_responses_require_hash_and_complete_alignment(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            self.fixture(directory)
            outgoing = directory / 'sessions/one/stdout'
            manifest = directory / 'sessions.jsonl'
            captured = outgoing.read_bytes()
            metadata = json.loads(manifest.read_text())
            outgoing.write_bytes(captured + b'sat\n')
            with self.assertRaisesRegex(ValueError, 'stdout hash mismatch'):
                list(benchmark.session_inputs(directory))
            for data, message in [
                    (captured + b'sat\n', 'response alignment mismatch'),
                    (captured.replace(b'unsat', b'sat'),
                     'check response mismatch')]:
                outgoing.write_bytes(data)
                metadata['stdout_sha256'] = benchmark.hashlib.sha256(
                    data).hexdigest()
                manifest.write_text(json.dumps(metadata))
                with self.assertRaisesRegex(ValueError, message):
                    list(benchmark.session_inputs(directory, limit=1))

    def test_unselected_unknown_keeps_changed_answer_metadata(self):
        program = '''#!/usr/bin/env python3
import sys
for line in sys.stdin:
    line = line.strip()
    if line.startswith('(echo "'):
        print(line[7:-2], flush=True)
    elif line == '(get-info :reason-unknown)':
        print('(error "Reason unavailable without an unknown result")',
              flush=True)
    elif line == '(check-sat)':
        print('unsat', flush=True)
'''
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            session = directory / 'sessions/one'
            session.mkdir(parents=True)
            check = b'(check-sat)'
            prefix = (check + b'\n(set-option :rlimit 123456)\n'
                      b'(get-info :reason-unknown)\n')
            data = prefix + check + b'\n'
            outgoing = b'unknown\n(:reason-unknown "resource limit")\nunsat\n'
            (session / 'input.smt2').write_bytes(data)
            (session / 'stdout').write_bytes(outgoing)
            query = dict(id='selected', session='one', result='unsat',
                         command_start=len(prefix),
                         command_end=len(prefix) + len(check),
                         command_sha256=benchmark.hashlib.sha256(
                             check).hexdigest())
            (directory / 'queries.jsonl').write_text(json.dumps(query) + '\n')
            (directory / 'sessions.jsonl').write_text(json.dumps(dict(
                id='one', input_sha256=benchmark.hashlib.sha256(data).hexdigest(),
                stdout_sha256=benchmark.hashlib.sha256(outgoing).hexdigest())))
            executable = directory / 'solver'
            executable.write_text(program)
            executable.chmod(0o755)
            solvers = {name: benchmark.Solver(str(executable), 10)
                       for name in ('ocaml', 'z3')}
            for solver in solvers.values():
                self.addCleanup(solver.close)
            item, = benchmark.session_inputs(directory)
            self.assertIsNone(item[2][0][0])
            row, = benchmark.measure_session(
                0, item, solvers, 1, 10, 'unlimited')
            self.assertTrue(row['correct'])
            self.assertEqual(row['id'], 'selected')
            for name in solvers:
                sample, = row['runs'][name]
                self.assertEqual(sample['errors'], [])
                diagnostic, = sample['changed_result_errors']
                self.assertEqual(diagnostic['original_result'], 'unknown')
                self.assertEqual(diagnostic['replay_result'], 'unsat')
                self.assertEqual(diagnostic['command_start'],
                                 prefix.index(b'(get-info'))
                self.assertEqual(diagnostic['check_position'], 1)

    def test_timeout_does_not_restart_partway_through_a_session(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            self.fixture(directory)
            item, = benchmark.session_inputs(directory)
            ocaml, z3 = Mock(), Mock()
            ocaml.memory.return_value = {}
            z3.memory.return_value = {}
            ocaml.run_warm.return_value = ('timeout', 3, [], [])
            z3.run_warm.side_effect = [('sat', .001, [], []),
                                       ('unsat', .002, [], []),
                                       ('sat', .003, [], [])]
            rows = list(benchmark.measure_session(
                0, item, dict(ocaml=ocaml, z3=z3), 1, 10, 'unlimited'))
            self.assertEqual(ocaml.run_warm.call_count, 1)
            self.assertTrue(all(not row['correct'] for row in rows))
            self.assertTrue(all(row['runs']['ocaml'][0]['result']
                                == 'session-unavailable' for row in rows))

    def test_changed_answer_query_policy(self):
        query = b'(get-info :reason-unknown)'
        script = b'\n' + query + b'\n(reset)(check-sat)'
        eligible = benchmark.changed_result_queries(script, 'unknown', 'unsat')
        self.assertEqual(len(eligible), 1)
        self.assertEqual(script[eligible[0]['command_start']:
                                eligible[0]['command_end']], query)
        for original, replay in [('sat', 'sat'), ('unknown', 'unknown'),
                                  (None, 'unsat'), ('unsat', 'sat')]:
            self.assertEqual(benchmark.changed_result_queries(
                script, original, replay), [])
        for prefix in (b'(reset)', b'(assert true)', b'(push 1)', b'(pop 1)'):
            self.assertEqual(benchmark.changed_result_queries(
                prefix + script, 'unknown', 'unsat'), [])
        for command in (b'(get-model)', b'(get-value (x))',
                        b'(get-unsat-core)',
                        b'(get-info :reason-unknown extra)'):
            self.assertEqual(benchmark.changed_result_queries(
                command, 'unknown', 'unsat'), [])

    def test_response_errors_require_command_boundaries(self):
        error = b'(error "Reason unavailable without an unknown result")'
        query = dict(command='(get-info :reason-unknown)', command_start=0,
                     command_end=26, original_result='unknown',
                     replay_result='unsat')
        regions = [(b'before', b'after', query)]
        result, errors, changed = benchmark.protocol_result(
            b'before\n' + error + b'\nafter\nunsat\n', 1, regions)
        self.assertEqual((result, errors), ('unsat', []))
        self.assertEqual(changed, [dict(query, response=error.decode())])
        for output in [error + b'\nbefore\nafter\nunsat\n',
                       b'before\nafter\n' + error + b'\nunsat\n',
                       b'before\n(error "wrong model")\nafter\nunsat\n',
                       b'before\n' + error + b'\n' + error
                       + b'\nafter\nunsat\n',
                       b'before\n' + error + b'\nunsat\n',
                       b'before\n' + error + b'\nafter\n']:
            self.assertEqual(benchmark.protocol_result(output, 1, regions)[0],
                             'error')

    def test_changed_answer_error_does_not_poison_the_session(self):
        program = '''#!/usr/bin/env python3
import sys
for line in sys.stdin:
    line = line.strip()
    if line.startswith('(echo "'):
        print(line[7:-2], flush=True)
    elif line == '(get-info :reason-unknown)':
        print('(error "Reason unavailable without an unknown result")',
              flush=True)
    elif line == '(assert broken)':
        print('(error "Reason unavailable without an unknown result")',
              flush=True)
    elif line == '(check-sat)':
        print('unsat', flush=True)
'''
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / 'solver'
            executable.write_text(program)
            executable.chmod(0o755)
            solver = benchmark.Solver(str(executable), 10)
            self.addCleanup(solver.close)
            result, _, errors, changed = solver.run_warm(
                b'(get-info :reason-unknown)\n(check-sat)\n',
                'unknown', 'unsat')
            self.assertEqual((result, errors, len(changed)), ('unsat', [], 1))
            process = solver.process
            self.assertEqual(solver.run_warm(b'(check-sat)\n',
                                            'unsat', 'unsat')[0], 'unsat')
            self.assertIs(solver.process, process)
            self.assertEqual(solver.run_warm(
                b'(get-info :reason-unknown)\n(assert broken)\n(check-sat)\n',
                'unknown', 'unsat')[0], 'error')

    def test_quoted_echo_keeps_the_session(self):
        program = '''#!/usr/bin/env python3
import sys
for line in sys.stdin:
    line = line.strip()
    if line.startswith('(echo "'):
        print(line[6:-1], flush=True)
    elif line == '(get-info :reason-unknown)':
        print('(error "Reason unavailable without an unknown result")',
              flush=True)
    elif line == '(check-sat)':
        print('unsat', flush=True)
'''
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / 'solver'
            executable.write_text(program)
            executable.chmod(0o755)
            solver = benchmark.Solver(str(executable), 2)
            self.addCleanup(solver.close)
            result, _, errors, changed = solver.run_warm(
                b'(get-info :reason-unknown)\n(check-sat)\n',
                'unknown', 'unsat')
            self.assertEqual((result, errors, len(changed)), ('unsat', [], 1))
            process = solver.process
            self.assertEqual(solver.run(b'(check-sat)\n')[0], 'unsat')
            self.assertIs(solver.process, process)

    def test_nonreading_query_solver_cannot_bypass_timeout(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / 'solver'
            executable.write_text('#!/usr/bin/env python3\n'
                                  'import time\ntime.sleep(2)\n')
            executable.chmod(0o755)
            solver = benchmark.Solver(str(executable), .05)
            self.addCleanup(solver.close)
            result, elapsed, errors = solver.run(b' ' * (1024 * 1024))
            self.assertEqual((result, errors), ('timeout', []))
            self.assertLess(elapsed, 1)
            self.assertIsNone(solver.process)

    def test_unknown_does_not_poison_the_session(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            self.fixture(directory)
            item, = benchmark.session_inputs(directory)
            solvers = {name: Mock() for name in ('ocaml', 'z3')}
            for solver in solvers.values():
                solver.memory.return_value = {}
                solver.run_warm.side_effect = [('unknown', .001, [], []),
                                               ('unsat', .002, [], []),
                                               ('sat', .003, [], [])]
            rows = list(benchmark.measure_session(
                0, item, solvers, 1, 10, 'unlimited'))
            self.assertTrue(all(row['pass'] for row in rows))
            for solver in solvers.values():
                self.assertEqual(solver.run_warm.call_count, 3)

    def test_changed_result_diagnostics_keep_session_offsets(self):
        first = b'(check-sat)'
        second = b'\n(get-info :reason-unknown)\n(check-sat)'
        item = ('one', 'digest', [
            (dict(id='first', result='unknown'), first, 0, len(first), 'unknown'),
            (dict(id='second', result='unsat'), second, len(first),
             len(first) + len(second), 'unsat')])
        solvers = {name: Mock() for name in ('ocaml', 'z3')}
        for solver in solvers.values():
            solver.memory.return_value = {}
            diagnostic, = benchmark.changed_result_queries(
                b'(set-option :rlimit 0)\n' + second, 'unknown', 'unsat')
            solver.run_warm.side_effect = [
                ('unsat', .001, [], []), ('unsat', .001, [], [diagnostic])]
        rows = list(benchmark.measure_session(
            0, item, solvers, 1, 10, 'unlimited'))
        self.assertTrue(all(row['pass'] for row in rows))
        for name, solver in solvers.items():
            self.assertEqual(solver.run_warm.call_args.args[1:],
                             ('unknown', 'unsat'))
            diagnostic, = rows[1]['runs'][name][0]['changed_result_errors']
            self.assertEqual(diagnostic['command_start'], len(first) + 1)
            self.assertEqual(diagnostic['check_position'], 1)


class CompleteApiOperationTests(unittest.TestCase):
    @staticmethod
    def runs(status='sat', **changes):
        sample = dict(result=status, seconds=.001, stderr='',
                      model_requested=status == 'sat',
                      model_available=status == 'sat',
                      core_requested=False, core_available=False,
                      certificate_validation=dict(result='valid'))
        sample.update(changes)
        return {'ocaml': [dict(sample)], 'z3': [dict(sample)]}

    def test_empty_sessions_are_recorded_without_qualifying_calls(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            capture = directory / 'capture'
            capture.mkdir()
            (capture / 'one.vapi').write_bytes(b'call')
            (capture / 'empty.vapi').write_bytes(b'')
            output = directory / 'output'
            binary = directory / 'binary'
            binary.write_bytes(b'compiled')
            arguments = ['benchmark', '--driver', str(binary),
                         '--ocaml', str(binary), '--z3', str(binary),
                         '--compiler', str(binary), '--cpu', '0',
                         '--corpus', str(capture), '--output', str(output),
                         '--repeats', '1', '--modes', 'fresh']

            def replay(command, **_):
                sample = self.runs()['ocaml'][0]
                sample.update(ordinal=1)
                Path(command[-1]).write_text(json.dumps(sample) + '\n')
                return {'peak_rss_bytes': 4096}

            with patch.object(sys, 'argv', arguments), \
                    patch.object(api_benchmark.os, 'sched_setaffinity',
                                 create=True), \
                    patch.object(api_benchmark.subprocess, 'check_output',
                                 return_value='fresh\n'), \
                    patch.object(api_benchmark, 'run_driver', replay), \
                    patch.object(api_benchmark, 'validate_followups'), \
                    patch('builtins.print'):
                api_benchmark.main()
            configuration = json.loads(
                (output / 'configuration.json').read_text())
            summary = json.loads((output / 'summary.json').read_text())
            self.assertEqual(configuration['empty_sessions'], ['empty.vapi'])
            self.assertEqual(len(configuration['captures']), 2)
            self.assertEqual(summary['counts']['fresh_calls'], 1)
            self.assertTrue(summary['all_pass'])
            preserved = {path: path.read_bytes()
                         for path in output.rglob('*') if path.is_file()}
            with patch.object(sys, 'argv', arguments), \
                    patch.object(api_benchmark.os, 'sched_setaffinity',
                                 create=True), \
                    patch.object(api_benchmark.subprocess, 'check_output',
                                 return_value='fresh\n'), \
                    patch.object(api_benchmark, 'run_driver') as rerun, \
                    patch('sys.stderr', io.StringIO()) as errors, \
                    self.assertRaises(SystemExit) as stopped:
                api_benchmark.main()
            self.assertEqual(stopped.exception.code, 2)
            self.assertIn('Output directory already exists', errors.getvalue())
            rerun.assert_not_called()
            self.assertEqual(preserved,
                             {path: path.read_bytes()
                              for path in output.rglob('*') if path.is_file()})

    def test_sat_without_requested_model_fails(self):
        runs = self.runs()
        runs['ocaml'][0]['model_available'] = False
        result = api_benchmark.summarize_runs(runs)
        self.assertFalse(result['correct'])
        self.assertTrue(result['performance_pass'])
        self.assertFalse(result['pass'])

    def test_unchecked_or_invalid_certificate_fails(self):
        for kind in ('sat', 'unsat'):
            for validation in (None, 'invalid', 'unvalidated'):
                runs = self.runs(kind, core_requested=kind == 'unsat',
                                 core_available=kind == 'unsat')
                sample = runs['ocaml'][0]
                sample.pop('certificate_validation')
                if validation is not None:
                    sample['certificate_validation'] = dict(result=validation)
                result = api_benchmark.summarize_runs(runs)
                self.assertTrue(result['followups_complete'])
                self.assertFalse(result['followups_valid'])
                self.assertFalse(result['pass'])

    def test_certificate_preserves_signed_and_arbitrary_integers(self):
        script = (b'(declare-const v0 (_ BitVec 63))(declare-const v1 Int)'
                  b'(declare-const v2 Bool)(declare-sort S 0)'
                  b'(declare-const v3 S)(check-sat)')
        certificate = dict(kind='model', bindings=[
            [0, 'int63', '-1'], [1, 'int', '-123456789012345678901234567890'],
            [2, 'bool', 'true']])
        proof, expected = api_benchmark.certificate_script(script, certificate)
        self.assertEqual(expected, 'sat')
        self.assertIn(b'(_ bv9223372036854775807 63)', proof)
        self.assertIn(b'(- 123456789012345678901234567890)', proof)
        for bindings in (certificate['bindings'][:-1],
                         certificate['bindings'] + [[2, 'bool', 'true']],
                         [[0, 'int63', '4611686018427387904']],
                         [[0, 'int', '-1']], [[0, 'bool', 'true']],
                         [[False, 'int63', '-1']]):
            with self.assertRaises(ValueError):
                api_benchmark.certificate_script(
                    script, dict(kind='model', bindings=bindings))

    def test_certificate_preserves_exact_assumption_subset(self):
        script = (b'(declare-const v0 Bool)(declare-const v1 Bool)'
                  b'(assert (not v0))(check-sat-assuming (v0 v1))')
        proof, expected = api_benchmark.certificate_script(
            script, dict(kind='assumptions', indices=[0]))
        self.assertEqual(expected, 'unsat')
        self.assertIn(b'(check-sat-assuming (v0))', proof)
        for indices in ([2], [0, 0], [True], [-1]):
            with self.assertRaises(ValueError):
                api_benchmark.certificate_script(
                    script, dict(kind='assumptions', indices=indices))

    def test_every_returned_certificate_is_checked_after_measurement(self):
        script = '(declare-const v0 Int)(assert (= v0 7))(check-sat)'
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            args = SimpleNamespace(output=directory, corpus=directory,
                                      driver=directory / 'driver',
                                      resource_policy='unlimited',
                                      z3=directory / 'z3', evidence_timeout=1)
            good = dict(kind='model', bindings=[[0, 'int', '7']])
            bad = dict(kind='model', bindings=[[0, 'int', '8']])
            rows = [dict(session='a.vapi', ordinal=1, mode=mode,
                         runs=self.runs(certificate=good))
                    for mode in ('fresh', 'reset', 'incremental', 'automatic')]
            rows[-1]['runs']['ocaml'].append(
                self.runs(certificate=bad)['ocaml'][0])
            responses = [dict(statuses=[status], returncode=0, errors=[])
                         for status in ('sat', 'unsat')]
            with patch.object(api_benchmark, 'export_queries', return_value=[
                    dict(ordinal=1, script=script)]) as export, \
                    patch.object(api_benchmark, 'run_script',
                                 side_effect=responses) as oracle:
                api_benchmark.validate_followups(rows, args, {})
            export.assert_called_once()
            self.assertEqual(oracle.call_count, 2)
            self.assertEqual(len(list((directory / 'certificate-evidence')
                                      .glob('*.smt2'))), 2)
            self.assertTrue(all(api_benchmark.summarize_runs(row['runs'])['pass']
                                for row in rows[:-1]))
            self.assertFalse(api_benchmark.summarize_runs(
                rows[-1]['runs'])['pass'])
            self.assertEqual(rows[-1]['runs']['ocaml'][-1]['seconds'], .001)

    def test_warm_calls_accept_only_bound_independent_evidence(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            capture = directory / 'capture'
            capture.mkdir()
            (capture / 'one.vapi').write_bytes(b'call')
            binary = directory / 'binary'
            binary.write_bytes(b'compiled')
            reference_binary = directory / 'reference'
            reference_binary.write_bytes(b'oracle')
            script = b'(assert false)(check-sat)'
            key = corpus.digest(script)
            verifier = Mock()
            verifier.verify.return_value = dict(verified=True,
                                                query_sha256=key)
            captures = Mock()
            captures.lookup.return_value = dict(captured_results=['unsat'])

            def replay(command, **_):
                sample = self.runs('unsat')['ocaml'][0]
                if command[1] == str(reference_binary.resolve()):
                    sample['result'] = 'unknown'
                sample.update(ordinal=1)
                Path(command[-1]).write_text(json.dumps(sample) + '\n')
                return {'peak_rss_bytes': 4096}

            for mismatch in (False, True):
                output = directory / str(mismatch)
                arguments = ['benchmark', '--driver', str(binary),
                             '--ocaml', str(binary), '--z3',
                             str(reference_binary), '--compiler', str(binary),
                             '--cpu', '0', '--corpus', str(capture),
                             '--output', str(output), '--repeats', '1',
                             '--modes', 'incremental', 'automatic',
                             '--check-reference-cores', '--reference-corpus',
                             str(capture)]
                binding = dict(query_sha256='other' if mismatch else key)
                with patch.object(sys, 'argv', arguments), \
                        patch.object(api_benchmark.os, 'sched_setaffinity',
                                     create=True), \
                        patch.object(api_benchmark.subprocess, 'check_output',
                                     return_value='incremental\nautomatic\n'), \
                        patch.object(api_benchmark, 'run_driver', replay), \
                        patch.object(api_benchmark, 'ReferenceVerifier',
                                     return_value=verifier), \
                        patch.object(api_benchmark, 'CapturedResults',
                                     return_value=captures), \
                        patch.object(api_benchmark, 'export_query',
                                     return_value=(script, binding)), \
                        patch('builtins.print'):
                    api_benchmark.main()
                rows = [json.loads(line) for line in
                        (output / 'operations.jsonl').read_text().splitlines()]
                self.assertEqual(len(rows), 2)
                self.assertTrue(all(row['pass'] == (not mismatch)
                                    for row in rows))
                self.assertTrue(all(row['reference_input'] == binding
                                    for row in rows))
                memory = list((output / 'samples').glob('*.memory.json'))
                self.assertEqual(len(memory), 4)
                self.assertTrue(all(json.loads(path.read_text())
                                    == {'peak_rss_bytes': 4096}
                                    for path in memory))

    @unittest.skipUnless(hasattr(os, 'wait4'), 'requires wait4')
    def test_session_memory_does_not_hide_driver_failure(self):
        memory = api_benchmark.run_driver([sys.executable, '-c',
                                          'data = bytearray(1024 * 1024)'])
        self.assertGreater(memory['peak_rss_bytes'], 0)
        with self.assertRaises(subprocess.CalledProcessError) as raised:
            api_benchmark.run_driver([sys.executable, '-c',
                                      'raise SystemExit(7)'])
        self.assertEqual(raised.exception.returncode, 7)

    def test_missing_requested_assumption_core_fails(self):
        runs = self.runs('unsat', core_requested=True, core_available=True)
        runs['ocaml'][0]['core_available'] = False
        self.assertFalse(api_benchmark.summarize_runs(runs)['pass'])

    def test_empty_core_is_available(self):
        runs = self.runs('unsat', core_requested=True, core_available=True,
                         core_size=0)
        self.assertTrue(api_benchmark.summarize_runs(runs)['pass'])

    def test_model_not_requested_for_empty_symbol_list(self):
        runs = self.runs(model_requested=False, model_available=False)
        self.assertTrue(api_benchmark.summarize_runs(runs)['pass'])

    def test_legacy_observation_does_not_qualify(self):
        runs = self.runs()
        del runs['ocaml'][0]['model_available']
        self.assertFalse(api_benchmark.summarize_runs(runs)['pass'])

    def test_budget_observations_do_not_claim_timing_qualification(self):
        runs = self.runs('unknown')
        result = api_benchmark.summarize_runs(runs, 'preserve')
        self.assertIsNone(result['correct'])
        self.assertIsNone(result['performance_pass'])
        self.assertIsNone(result['pass'])


class ReferenceEvidenceTests(unittest.TestCase):
    @staticmethod
    def runs():
        return {'ocaml': [dict(result='unsat', seconds=.005, errors=[])],
                'z3': [dict(result='unknown', seconds=60, errors=[])]}

    @staticmethod
    def response(stdout, **changes):
        result = dict(returncode=0, stderr='', errors=[], stdout=stdout)
        result.update(changes)
        return result

    @staticmethod
    def prior(script, indices):
        named, commands, labels = reference.named_script(script)
        core = [list(labels)[index] for index in indices]
        return dict(query_sha256=reference.digest(script),
                    named_sha256=reference.digest(named),
                    assertions={label.decode(): reference.digest(command)
                                for label, command in labels.items()},
                    core=[label.decode() for label in core],
                    subset_sha256=reference.digest(
                        reference.subset_script(commands, labels, core)))

    def test_metadata_before_logic_is_preserved(self):
        script = (b'(reset)(set-info :smt-lib-version 2.6)'
                  b'(set-info :source |some ; source ":named"|)'
                  b'(set-logic QF_IDL)(set-info :status unsat)'
                  b'(declare-const x Int)(assert (= x 0))'
                  b'(set-info :category "industrial")'
                  b'(assert (distinct x 0))(check-sat)')
        named, commands, labels = reference.named_script(script)
        subset = reference.subset_script(commands, labels, list(labels))
        for item in (b'(set-info :smt-lib-version 2.6)',
                     b'(set-info :source |some ; source ":named"|)',
                     b'(set-info :status unsat)',
                     b'(set-info :category "industrial")'):
            self.assertIn(item, named)
            self.assertIn(item, subset)

    def test_malformed_or_unknown_metadata_is_rejected(self):
        for command in (b'(set-info :status)', b'(set-info :status yes)',
                        b'(set-info status unsat)',
                        b'(set-info :status unsat extra)',
                        b'(set-info :global-declarations true)',
                        b'(set-info :source)', b'(set-info :source a b)'):
            with self.subTest(command=command), self.assertRaises(ValueError):
                reference.named_script(command+b'(assert false)(check-sat)')

    def test_metadata_does_not_permit_resets_or_rechecks(self):
        for script in (b'(declare-const x Int)(set-info :status unsat)'
                       b'(reset)(check-sat)',
                       b'(check-sat)(set-info :status unsat)'):
            with self.subTest(script=script), self.assertRaises(ValueError):
                reference.named_script(script)

    def test_prior_core_is_bound_to_exact_query_and_assertions(self):
        script = b'(assert false)(assert true)(check-sat)'
        named, _, labels = reference.named_script(script)
        prior = self.prior(script, [0])
        self.assertEqual(reference.prior_core(prior, script, named, labels),
                         [next(iter(labels))])
        for changes in (dict(query_sha256='0' * 64),
                        dict(named_sha256='0' * 64), dict(assertions={}),
                        dict(core=['absent']), dict(core=[0]),
                        dict(core=prior['core'] * 2), dict(core='absent')):
            with self.subTest(changes=changes), self.assertRaises(ValueError):
                reference.prior_core(dict(prior, **changes), script,
                                     named, labels)

    @staticmethod
    def counting_fixture():
        script = (b'(declare-const x Int)(declare-const y Int)'
                  b'(assert (= x 0))(assert (= y 0))'
                  b'(assert (distinct x y))(check-sat)')
        _, commands, labels = reference.named_script(script)
        names = [label.decode() for label in labels]
        x = '(ite (= x 0) 1 0)'
        y = '(ite (= y 0) 1 0)'
        steps = [dict(id='x', dependencies=[names[0]],
                      formula=f'(= {x} 1)'),
                 dict(id='y', dependencies=[names[1]],
                      formula=f'(= {y} 1)'),
                 dict(id='column', dependencies=[names[2]],
                      formula=f'(<= (+ {x} {y}) 1)'),
                 dict(id='conclusion', dependencies=['x', 'y', 'column'],
                      formula='false')]
        prior = dict(method='independently-checked-counting-lemmas',
                     query_sha256=reference.digest(script), steps=steps)
        return script, commands, labels, prior

    def test_counting_lemmas_are_rechecked_before_integer_abstraction(self):
        script, _, _, prior = self.counting_fixture()
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver, oracle = directory / 'solver', directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            saved = directory / 'prior.json'
            saved.write_text(json.dumps(dict(prior, verified=False)))
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory / 'evidence', 5, [saved])
            with patch.object(reference, 'run_script',
                              return_value=self.response('unsat')) as run:
                evidence = verifier.verify(script, self.runs())
            self.assertTrue(evidence['verified'])
            self.assertEqual(evidence['method'], prior['method'])
            self.assertEqual(run.call_count, 4)
            self.assertTrue(all(call.args[0] == str(oracle.resolve())
                                for call in run.call_args_list))
            first = run.call_args_list[0].args[2]
            self.assertIn(b'(assert (= x 0))', first)
            self.assertNotIn(b'(assert (distinct x y))', first)
            last = run.call_args_list[-1].args[2]
            self.assertIn(b'(assert (= count_0 1))', last)
            self.assertIn(b'(assert (= count_1 1))', last)
            self.assertIn(b'(assert (<= (+ count_0 count_1) 1))', last)
            self.assertNotIn(b'ite', last)
            self.assertNotIn(b'(declare-const x Int)', last)
            self.assertEqual(evidence['steps'], prior['steps'])

    def test_counting_abstraction_shares_only_identical_indicators(self):
        declarations, formulas = reference.counting_abstraction([
            b'(= (+ (ite p 1 0) (ite q 1 0) (ite p 1 0)) 2)',
            b'(<= (ite (not (not p)) 1 0) 1)'])
        self.assertEqual(len(declarations), 3)
        self.assertEqual(formulas, [b'(= (+ count_0 count_1 count_0) 2)',
                                    b'(<= count_2 1)'])
        for formula in (b'(= x 1)', b'(= (ite p 0 1) 1)',
                        b'(= (* 2 (ite p 1 0)) 1)', b'(not false)',
                        b'(= (f (ite p 1 0)) 1)', b'(= (+) 1)'):
            with self.subTest(formula=formula), self.assertRaises(ValueError):
                reference.counting_abstraction([formula])

    def test_counting_proof_rejects_shadowed_operators_and_sorts(self):
        declarations = [
            b'(define-fun not ((p Bool)) Bool false)',
            b'(declare-fun |+| (Int Int) Int)',
            b'(declare-fun <= (Int Int) Bool)',
            b'(declare-fun = (Int Int) Bool)',
            b'(declare-fun ite (Bool Int Int) Int)',
            b'(declare-sort |Int| 0)', b'(define-sort Bool () Int)',
            b'(declare-datatype D ((|not|)))',
            b'(declare-datatype D ((C (+ Int))))',
            b'(declare-datatypes ((D 0)) (((C (|ite| Int)))))',
            b'(declare-datatypes ((Bool 0)) (((C))))']
        for declaration in declarations:
            with self.subTest(declaration=declaration), \
                    self.assertRaisesRegex(ValueError, 'shadowed'):
                reference.counting_declarations([declaration])
        allowed = [b'(declare-fun f (Int) Bool)',
                   b'(declare-datatypes ((L 1)) '
                   b'((par (A) ((nil) (cons (head A) (tail (L A)))))))']
        self.assertEqual(reference.counting_declarations(allowed), allowed)

    def test_counting_proof_rejects_declarations_after_assertions(self):
        script = (b'(declare-fun f (Real) Bool)(assert (f 0))'
                  b'(declare-fun f (Int) Bool)(assert (not (f 0)))'
                  b'(check-sat)')
        commands = reference.flat_commands(script)
        with self.assertRaisesRegex(ValueError, 'initial declarations'):
            reference.counting_declarations(commands)

    def test_counting_proof_rejects_invalid_dependencies_and_conclusions(self):
        _, commands, labels, prior = self.counting_fixture()
        variants = []
        for field, value in (
                ('id', next(iter(labels)).decode()),
                ('dependencies', ['column']), ('dependencies', [0]),
                ('formula', 'true false'), ('formula', 'false'),
                ('formula', '(= |escaped\\\\symbol| 0)'),
                ('formula', '(! true :named p)'),
                ('formula', '(= (ite (! true :named p) 1 0) 1)')):
            changed = json.loads(json.dumps(prior))
            changed['steps'][0][field] = value
            variants.append(changed)
        changed = json.loads(json.dumps(prior))
        changed['steps'][-1]['dependencies'] = [next(iter(labels)).decode()]
        variants.append(changed)
        variants.extend([dict(prior, steps=[]),
                         dict(prior, steps=prior['steps'][:-1])])
        for changed in variants:
            with self.subTest(proof=changed), \
                    tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                solver, oracle = directory / 'solver', directory / 'oracle'
                solver.write_bytes(b'candidate')
                oracle.write_bytes(b'independent')
                verifier = reference.ReferenceVerifier(
                    solver, oracle, directory, 5)
                with patch.object(reference, 'run_script',
                                  return_value=self.response('unsat')):
                    with self.assertRaises(ValueError):
                        verifier.prove_counting(commands, labels, changed,
                                                directory, {})

    def test_counting_proof_requires_every_oracle_check_to_succeed(self):
        _, commands, labels, prior = self.counting_fixture()
        failures = [self.response(answer) for answer in ('sat', 'unknown')]
        failures += [self.response('unsat', **change) for change in (
            dict(errors=['bad']), dict(stderr='bad'), dict(timeout=True),
            dict(output_limit=True), dict(failure='crash'), dict(returncode=1))]
        for failure in failures:
            with self.subTest(failure=failure), \
                    tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                solver, oracle = directory / 'solver', directory / 'oracle'
                solver.write_bytes(b'candidate')
                oracle.write_bytes(b'independent')
                verifier = reference.ReferenceVerifier(
                    solver, oracle, directory, 5)
                with patch.object(reference, 'run_script',
                                  return_value=failure) as run:
                    evidence = verifier.prove_counting(
                        commands, labels, prior, directory, {})
                self.assertFalse(evidence['verified'])
                self.assertEqual(run.call_count, 1)

    @staticmethod
    def split_fixture():
        script = (b'(reset)(set-logic QF_LIA)'
                  b'(set-option :rlimit 0)'
                  b'(declare-const x Int)(assert (>= x 1))'
                  b'(assert (<= x 0))(check-sat)')
        prior = dict(method='independently-checked-case-split',
                     query_sha256=reference.digest(script), formula='(<= x 0)')
        return script, prior

    def test_case_split_reconstructs_both_complete_original_queries(self):
        script, prior = self.split_fixture()
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver, oracle = directory / 'solver', directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            saved = directory / 'prior.json'
            saved.write_text(json.dumps(dict(prior, verified=False)))
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory / 'evidence', 5, [saved])
            with patch.object(reference, 'run_script',
                              return_value=self.response('unsat')) as run:
                evidence = verifier.verify(script, self.runs())
            self.assertTrue(evidence['verified'])
            self.assertEqual(evidence['method'], prior['method'])
            self.assertEqual(evidence['query_sha256'],
                             reference.digest(script))
            self.assertEqual(evidence['formula_sha256'],
                             reference.digest(prior['formula'].encode()))
            self.assertEqual(run.call_count, 2)
            for index, call in enumerate(run.call_args_list):
                self.assertEqual(call.args[0], str(oracle.resolve()))
                commands = [p for _, _, p in
                            suites.complete_expressions(call.args[2])]
                original = reference.flat_commands(script)[:-1]
                self.assertEqual(commands[:len(original)], original)
                self.assertEqual(commands[-2:], [b'(check-sat)', b'(exit)'])
                formula = prior['formula'].encode()
                if index:
                    formula = b'(not ' + formula + b')'
                self.assertEqual(commands[-3], b'(assert ' + formula + b')')
                self.assertTrue(evidence['split_checks'][index]['verified'])
                self.assertEqual(
                    evidence['split_checks'][index]['script_sha256'],
                    reference.digest(call.args[2]))

    def test_case_split_rejects_bad_formula_and_query_identity(self):
        script, prior = self.split_fixture()
        variants = [dict(prior, formula=formula) for formula in (
            None, {}, '', 'true false', '(not false', 'false ; trailing',
            '(! true :named p)', '(not (! false :named p))',
            '(= |escaped\\symbol| 0)')]
        variants.append(dict(prior, query_sha256='0' * 64))
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver, oracle = directory / 'solver', directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory, 5)
            for changed in variants:
                with self.subTest(changed=changed), \
                        patch.object(reference, 'run_script') as run:
                    with self.assertRaises(ValueError):
                        verifier.prove_case_split(
                            script, changed, directory, {})
                    run.assert_not_called()

    def test_case_split_rejects_scopes_assumptions_and_shadowed_negation(self):
        scripts = [
            b'(push 1)(assert false)(check-sat)',
            b'(assert false)(pop 0)(check-sat)',
            b'(assert false)(check-sat)(check-sat)',
            b'(assert false)(reset)(check-sat)',
            b'(assert false)(check-sat-assuming ())',
            b'(assert false)(check-sat-using smt)',
            b'(declare-const x Bool)(assert x)'
            b'(declare-const y Bool)(check-sat)',
            b'(define-fun not ((p Bool)) Bool false)(check-sat)',
            b'(declare-fun |not| (Bool) Bool)(check-sat)',
            b'(declare-datatype D ((not)))(check-sat)',
            b'(define-sort Bool () Int)(check-sat)',
            b'(assert (! false :named p))(check-sat)',
            b'(set-option :regular-output-channel "hidden")(check-sat)']
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver, oracle = directory / 'solver', directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory, 5)
            for script in scripts:
                with self.subTest(script=script), \
                        patch.object(reference, 'run_script') as run:
                    prior = dict(query_sha256=reference.digest(script),
                                 formula='false')
                    with self.assertRaises(ValueError):
                        verifier.prove_case_split(script, prior, directory, {})
                    run.assert_not_called()

    def test_case_split_requires_both_clean_independent_results(self):
        script, prior = self.split_fixture()
        failures = [self.response(answer) for answer in
                    ('sat', 'unknown', '', 'unsat\nsat', 'unsat\nunsat')]
        failures += [self.response('unsat', **change) for change in (
            dict(errors=['bad']), dict(stderr='bad'), dict(timeout=True),
            dict(output_limit=True), dict(failure='crash'),
            dict(returncode=1))]
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver, oracle = directory / 'solver', directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory, 5)
            for failed_branch in (0, 1):
                for failure in failures:
                    with self.subTest(branch=failed_branch, failure=failure):
                        responses = [self.response('unsat')] * failed_branch
                        with patch.object(reference, 'run_script',
                                          side_effect=responses + [failure]):
                            evidence = verifier.prove_case_split(
                                script, prior, directory, {})
                        self.assertFalse(evidence['verified'])
                        self.assertEqual(len(evidence['split_checks']),
                                         failed_branch + 1)

    def test_case_split_sat_branch_blocks_fallback_acceptance(self):
        script, prior = self.split_fixture()
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver, oracle = directory / 'solver', directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            saved = directory / 'prior.json'
            saved.write_text(json.dumps(dict(prior, verified=True)))
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory / 'evidence', 5, [saved])
            response = self.response('success\nsat')
            with patch.object(reference, 'run_script',
                              return_value=response) as run:
                evidence = verifier.verify(script, self.runs())
            self.assertFalse(evidence['verified'])
            self.assertTrue(evidence['reference_contradiction'])
            self.assertEqual(run.call_count, 1)

    def test_prior_result_is_reproved_without_trusting_saved_verified(self):
        script = b'(assert false)(assert true)(check-sat)'
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver = directory / 'solver'
            oracle = directory / 'oracle'
            solver.write_bytes(b'candidate')
            oracle.write_bytes(b'independent')
            prior = directory / 'prior.json'
            prior.write_text(json.dumps(dict(self.prior(script, [0]),
                                            verified=False)))
            verifier = reference.ReferenceVerifier(
                solver, oracle, directory / 'evidence', 5, [prior])
            with patch.object(reference, 'run_script',
                              return_value=self.response('unsat')) as run:
                evidence = verifier.verify(script, self.runs())
            self.assertTrue(evidence['verified'])
            self.assertEqual(run.call_count, 1)
            self.assertEqual(run.call_args.args[0], str(oracle.resolve()))
            self.assertNotIn(b'(assert true)', run.call_args.args[2])
            self.assertEqual(len(evidence['prior_attempts']), 1)
            self.assertFalse((directory / 'evidence' / reference.digest(script)
                              / 'extraction.json').exists())

    def test_bad_or_unproved_prior_falls_back_to_current_core(self):
        script = b'(assert false)(assert true)(check-sat)'
        _, _, labels = reference.named_script(script)
        extracted = self.response('unsat\n(' + next(iter(labels)).decode() + ')')
        for changed_hash, prior_answer in ((True, None), (False, 'sat'),
                                           (False, 'unknown'),
                                           (False, 'unsat\nsat')):
            with self.subTest(changed_hash=changed_hash, answer=prior_answer), \
                    tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                solver = directory / 'solver'
                oracle = directory / 'oracle'
                solver.write_bytes(b'candidate')
                oracle.write_bytes(b'independent')
                saved = dict(self.prior(script, [1]), verified=True)
                if changed_hash:
                    saved['subset_sha256'] = '0' * 64
                prior = directory / 'prior.json'
                prior.write_text(json.dumps(saved))
                results = [extracted, self.response('unsat')]
                if prior_answer is not None:
                    results.insert(0, self.response(prior_answer))
                verifier = reference.ReferenceVerifier(
                    solver, oracle, directory / 'evidence', 5, [prior])
                with patch.object(reference, 'run_script',
                                  side_effect=results) as run:
                    evidence = verifier.verify(script, self.runs())
                self.assertTrue(evidence['verified'])
                self.assertFalse(evidence['prior_attempts'][0]['verified'])
                self.assertEqual(run.call_count, len(results))
                self.assertIn(b'(assert false)', run.call_args.args[2])
                self.assertNotIn(b'(assert true)', run.call_args.args[2])

    def test_subset_retains_exact_assertions_and_assumptions(self):
        script = (b'(set-option :rlimit 0)\n(reset)\n'
                  b'(set-option :produce-unsat-cores false)\n'
                  b'(declare-const p Bool)\n'
                  b'(assert ; original comment\n (not p))\n'
                  b'(assert true)\n(check-sat-assuming (p))\n')
        named, commands, labels = reference.named_script(script)
        self.assertLess(named.index(b'(reset)'),
                        named.index(b':produce-unsat-cores true'))
        self.assertNotIn(b':produce-unsat-cores false', named)
        core = [next(iter(labels))]
        subset = reference.subset_script(commands, labels, core)
        self.assertIn(b'(assert ; original comment\n (not p))', subset)
        self.assertNotIn(b'(assert true)', subset)
        self.assertIn(b'(check-sat-assuming (p))', subset)
        self.assertIn(b':produce-unsat-cores false', subset)

    def test_scopes_named_dependencies_and_unmodeled_commands_rejected(self):
        prefixes = [b'(push 1)', b'(pop 0)', b'(reset-assertions)',
                    b'(declare-const p Bool)(reset)',
                    b'(assert (! true :named premise))',
                    b'(define-fun p () Bool (! true :named premise))',
                    b'(set-option :global-declarations true)',
                    b'(set-option :regular-output-channel "hidden")',
                    b'(echo "unsat")', b'(assert-soft false)',
                    b'(declare-const |es\\caped| Bool)']
        for prefix in prefixes:
            with self.subTest(prefix=prefix), self.assertRaises(ValueError):
                reference.named_script(prefix + b'(check-sat)')
        for tail in (b'(push', b'\nunknown', b'(reset)', b'(check-sat)'):
            with self.subTest(tail=tail), self.assertRaises(ValueError):
                reference.named_script(b'(assert false)(check-sat)' + tail)

    def test_strings_and_quoted_symbols_are_not_annotations(self):
        script = (b'(declare-const |:named| Bool)'
                  b'(assert (= ":named" ":named"))(check-sat)')
        reference.named_script(script)

    def test_bad_core_labels_and_extra_responses_are_rejected(self):
        _, _, labels = reference.named_script(b'(assert false)(check-sat)')
        label = next(iter(labels)).decode()
        for output in ('unsat\n(unknown)', f'unsat\n({label} {label})',
                       f'unsat\n({label} |{label}|)', 'unsat\n((not x))',
                       f'unsat\n({label})\nsat', f'unsat\n({label}',
                       'sat\n()', 'unknown\n()'):
            with self.subTest(output=output), self.assertRaises(ValueError):
                reference.checked_labels(self.response(output), labels)
        for changes in (dict(returncode=1), dict(stderr='diagnostic'),
                        dict(timeout=True), dict(failure='crash'),
                        dict(output_limit=True), dict(errors=['bad'])):
            with self.subTest(changes=changes), self.assertRaises(ValueError):
                reference.checked_labels(
                    self.response(f'unsat\n({label})', **changes), labels)
        self.assertEqual(reference.checked_labels(
            self.response(f'success\nunsat\n(|{label}|)'), labels),
            [label.encode()])

    def test_reference_must_cleanly_prove_unsat(self):
        self.assertTrue(reference.proved_unsat(self.response('unsat\n')))
        for output in ('sat', 'unknown', 'unsat\nsat',
                       'unsat\n(error "bad")', 'unsat\n()'):
            self.assertFalse(reference.proved_unsat(self.response(output)))
        self.assertFalse(reference.proved_unsat(
            self.response('unsat', returncode=1)))

    def test_any_definitive_sat_or_protocol_failure_blocks_fallback(self):
        self.assertTrue(reference.eligible(self.runs(), ['unknown', 'unsat']))
        self.assertFalse(reference.eligible(self.runs(), ['sat', 'unsat']))
        for solver in ('ocaml', 'z3'):
            runs = self.runs()
            runs[solver].append(dict(result='sat', seconds=.01))
            self.assertFalse(reference.eligible(runs))
            runs = self.runs()
            runs[solver][0]['errors'] = ['bad']
            self.assertFalse(reference.eligible(runs))
        for answer in ('error', 'crash', 'session-unavailable'):
            runs = self.runs()
            runs['z3'][0]['result'] = answer
            self.assertFalse(reference.eligible(runs))
        runs = self.runs()
        runs['z3'][0]['result'] = 'timeout'
        self.assertTrue(reference.eligible(runs))

    def test_query_gate_keeps_original_times_and_reference_answer(self):
        runs = self.runs()
        original = json.dumps(runs, sort_keys=True)
        evidence = dict(verified=True, query_sha256='script')
        row = dict(measurement_sha256='script', reference_evidence=evidence)
        result = benchmark.summarize(dict(result='unsat'), row, runs, 10)
        self.assertTrue(result['pass'])
        self.assertFalse(result['original_matches_replay'])
        self.assertEqual(result['independently_verified_result'], 'unsat')
        self.assertEqual(result['median_seconds']['z3'], 60)
        self.assertEqual(json.dumps(runs, sort_keys=True), original)
        result = benchmark.summarize(
            dict(result='unknown', captured_results=['sat', 'unknown']),
            row, runs, 10)
        self.assertFalse(result['correct'])
        row['measurement_sha256'] = 'different'
        self.assertFalse(benchmark.summarize({}, row, runs, 10)['pass'])

    def test_proof_work_does_not_replace_the_measured_query(self):
        script = b'(reset)(assert false)(check-sat)'
        measured = benchmark.measurement_script(script, 'unlimited')
        solvers = {name: Mock() for name in ('ocaml', 'z3')}
        solvers['ocaml'].run.return_value = ('unsat', .1, [])
        solvers['z3'].run.return_value = ('unknown', 60, [])
        for solver in solvers.values():
            solver.memory.return_value = {}
        verifier = Mock()
        verifier.verify.return_value = dict(
            verified=True, query_sha256=corpus.digest(measured))
        row = benchmark.measure(0, dict(result='unsat'), script, solvers,
                                1, 10, 'unlimited', verifier)
        self.assertTrue(row['pass'])
        for solver in solvers.values():
            solver.run.assert_called_once_with(measured)
        self.assertEqual(verifier.verify.call_args.args[0], measured)
        self.assertEqual(row['median_seconds'], {'ocaml': .1, 'z3': 60})

    def test_captured_query_memory_is_recorded_after_timing(self):
        order = []
        solvers = {name: Mock() for name in ('ocaml', 'z3')}
        for name, solver in solvers.items():
            def run(script, name=name):
                order.append((name, 'run'))
                return 'unsat', .012, []

            def memory(name=name):
                order.append((name, 'memory'))
                return {'rss_bytes': 4096, 'peak_rss_bytes': 8192}

            solver.run.side_effect = run
            solver.memory.side_effect = memory
        row = benchmark.measure(0, {'result': 'unsat'},
                                b'(assert false)(check-sat)', solvers, 1, 10)
        self.assertEqual(order, [('z3', 'run'), ('z3', 'memory'),
                                 ('ocaml', 'run'), ('ocaml', 'memory')])
        for samples in row['runs'].values():
            self.assertEqual(samples[0]['seconds'], .012)
            self.assertEqual(samples[0]['rss_bytes'], 4096)
            self.assertEqual(samples[0]['peak_rss_bytes'], 8192)

    def test_api_gate_keeps_followup_requirement_and_query_binding(self):
        runs = CompleteApiOperationTests.runs('unsat')
        runs['z3'][0]['result'] = 'timeout'
        runs['z3'][0]['seconds'] = 60
        evidence = dict(verified=True, query_sha256='script')
        result = api_benchmark.summarize_runs(
            runs, reference_evidence=evidence, query_sha256='script')
        self.assertTrue(result['pass'])
        self.assertEqual(result['median_seconds']['z3'], 60)
        self.assertFalse(api_benchmark.summarize_runs(
            runs, reference_evidence=evidence,
            query_sha256='different')['pass'])
        self.assertFalse(api_benchmark.summarize_runs(
            runs, reference_evidence=evidence, query_sha256='script',
            observed_results=['sat', 'unsat', 'timeout'])['pass'])
        runs['ocaml'][0].update(core_requested=True, core_available=False)
        self.assertFalse(api_benchmark.summarize_runs(
            runs, reference_evidence=evidence, query_sha256='script')['pass'])

    def test_export_binds_full_capture_driver_and_script_hashes(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            driver, capture = directory / 'driver', directory / 'one.vapi'
            driver.write_bytes(b'driver')
            capture.write_bytes(b'capture')
            configuration = dict(driver_sha256=api_benchmark.digest(driver),
                                 captures={capture.name:
                                           api_benchmark.digest(capture)})
            destination = directory / 'query.smt2'
            script = b'(assert false)(check-sat)'
            with patch.object(api_benchmark.subprocess, 'run') as run:
                run.side_effect = (
                    lambda *a, **k: destination.write_bytes(script))
                exported, binding = api_benchmark.export_query(
                    driver, capture, 17, destination, configuration)
                self.assertEqual(exported, script)
                self.assertEqual(binding['query_sha256'], corpus.digest(script))
                self.assertEqual(binding['capture_sha256'],
                                 corpus.digest(b'capture'))
                self.assertEqual(run.call_args.args[0][1], '--export-query')
                capture.write_bytes(b'changed')
                with self.assertRaisesRegex(ValueError, 'capture changed'):
                    api_benchmark.export_query(
                        driver, capture, 17, destination, configuration)

    def test_api_fallback_requires_matching_capture_without_sat(self):
        for captured in ('unsat', 'sat'):
            with self.subTest(captured=captured), \
                    tempfile.TemporaryDirectory() as temporary:
                directory = Path(temporary)
                typed, protocol = directory / 'typed', directory / 'protocol'
                typed.mkdir()
                (protocol / 'scripts').mkdir(parents=True)
                (typed / 'one.vapi').write_bytes(b'typed input')
                script = b'(reset)(assert false)(check-sat)'
                key = corpus.digest(script)
                (protocol / 'scripts' / (key + '.smt2')).write_bytes(script)
                (protocol / 'queries.jsonl').write_text(json.dumps(dict(
                    replay_script_sha256=key, result=captured)) + '\n')
                binaries = {}
                for name in ('driver', 'ocaml', 'z3'):
                    binaries[name] = directory / name
                    binaries[name].write_text(name)
                output = directory / 'output'
                arguments = ['benchmark', '--driver', str(binaries['driver']),
                             '--ocaml', str(binaries['ocaml']), '--z3',
                             str(binaries['z3']), '--compiler',
                             str(binaries['driver']), '--cpu', '0', '--corpus',
                             str(typed), '--output', str(output), '--repeats',
                             '1', '--modes', 'fresh', '--check-reference-cores',
                             '--reference-corpus', str(protocol)]

                def replay(command, **_):
                    if command[1] == '--export-query':
                        Path(command[-1]).write_bytes(script)
                    else:
                        sample = CompleteApiOperationTests.runs('unsat')
                        sample = sample['ocaml'][0]
                        if Path(command[1]).name == 'z3':
                            sample.update(result='timeout', seconds=60)
                        sample.update(ordinal=1)
                        Path(command[-1]).write_text(json.dumps(sample) + '\n')
                    return {}

                _, _, labels = reference.named_script(script)
                core = b'unsat\n(' + b' '.join(labels) + b')'
                with patch.object(sys, 'argv', arguments), \
                        patch.object(api_benchmark.os, 'sched_setaffinity',
                                     create=True), \
                        patch.object(api_benchmark.subprocess, 'check_output',
                                     return_value='fresh\n'), \
                        patch.object(api_benchmark.subprocess, 'run', replay), \
                        patch.object(api_benchmark, 'run_driver', replay), \
                        patch.object(reference, 'run_script', side_effect=[
                            self.response(core.decode()),
                            self.response('unsat')]) as proof, \
                        patch('builtins.print'):
                    api_benchmark.main()
                lines = (output / 'operations.jsonl').read_text().splitlines()
                row, = map(json.loads, lines)
                self.assertEqual(row['pass'], captured == 'unsat')
                self.assertEqual(row['median_seconds']['z3'], 60)
                self.assertEqual(row['reference_capture']['captured_results'],
                                 [captured])
                expected_proofs = 2 if captured == 'unsat' else 0
                self.assertEqual(proof.call_count, expected_proofs)

    def test_capture_mapping_checks_bytes_and_preserves_conflicts(self):
        original = (b'(reset)(set-option :rlimit 17)'
                    b'(assert false)(check-sat)')
        exported = (b'(reset)\n(set-option :rlimit 0)\n'
                    b'(assert false)\n(check-sat)\n')
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            scripts = directory / 'scripts'
            scripts.mkdir()
            key = corpus.digest(original)
            path = scripts / (key + '.smt2')
            path.write_bytes(original)
            (directory / 'queries.jsonl').write_text(json.dumps(
                dict(replay_script_sha256=key, result='unknown',
                     captured_results=['unsat', 'sat'])) + '\n')
            capture = reference.CapturedResults(directory).lookup(exported)
            self.assertEqual(capture['captured_results'],
                             ['sat', 'unknown', 'unsat'])
            self.assertEqual(capture['matching_replay_sha256'], [key])
            self.assertFalse(reference.eligible(self.runs(),
                                                capture['captured_results']))
            self.assertIsNone(reference.CapturedResults(directory).lookup(
                exported.replace(b'false', b'true')))
            path.write_bytes(exported)
            with self.assertRaisesRegex(ValueError, 'hash mismatch'):
                reference.CapturedResults(directory)

    @unittest.skipUnless(shutil.which('z3'), 'requires Z3')
    def test_bogus_unsat_core_cannot_override_reference(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver = directory / 'candidate'
            solver.write_text('#!' + sys.executable + '\n'
                              'import re, sys\n'
                              'script = sys.stdin.read()\n'
                              'labels = re.findall(r":named ([^ )]+)", '
                              'script)\n'
                              'print("unsat")\n'
                              'print("(" + " ".join(labels[:1]) + ")")\n')
            solver.chmod(0o755)
            verifier = reference.ReferenceVerifier(
                solver, shutil.which('z3'), directory / 'evidence', 5)
            bad = verifier.verify(b'(assert true)(check-sat)', self.runs())
            self.assertFalse(bad['verified'])
            good = verifier.verify(b'(assert false)(check-sat)', self.runs())
            self.assertTrue(good['verified'])
            evidence = Path(good['evidence_directory'])
            self.assertEqual(corpus.digest((evidence / 'original.smt2')
                                          .read_bytes()), good['query_sha256'])
            self.assertEqual(json.loads((evidence / 'verification.json')
                                        .read_text())['statuses'], ['unsat'])
            self.assertIsNone(verifier.verify(
                b'(assert false)(check-sat)', self.runs(), ['sat']))
            with self.assertRaisesRegex(ValueError, 'independent'):
                reference.ReferenceVerifier(solver, solver, directory, 5)

    @unittest.skipUnless(shutil.which('z3'), 'requires Z3')
    def test_empty_core_is_checked_with_original_assumptions(self):
        with tempfile.TemporaryDirectory() as temporary:
            directory = Path(temporary)
            solver = directory / 'candidate'
            solver.write_text('#!' + sys.executable + '\n'
                              'import sys\nsys.stdin.read()\n'
                              'print("unsat\\n()")\n')
            solver.chmod(0o755)
            verifier = reference.ReferenceVerifier(
                solver, shutil.which('z3'), directory / 'evidence', 5)
            script = (b'(declare-const p Bool)(assert true)'
                      b'(check-sat-assuming (p (not p)))')
            result = verifier.verify(script, self.runs())
            self.assertTrue(result['verified'])
            self.assertEqual(result['retained_assertions'], 0)
            subset = (Path(result['evidence_directory'])
                      / 'subset.smt2').read_bytes()
            self.assertNotIn(b'(assert true)', subset)
            self.assertIn(b'(check-sat-assuming (p (not p)))', subset)


if __name__ == "__main__":
    unittest.main()
