#!/usr/bin/env python3
import argparse
import contextlib
import io
import json
from pathlib import Path
import shlex
import sys
import tempfile
import unittest
from unittest.mock import patch

import smt_stream as stream


FAKE = r'''
import os
import sys
import time
sys.path.insert(0, sys.argv[1])
from smt_stream import Framer, parts
parser = Framer()
frames = [[]]
raw = 'raw' in sys.argv
wrong = 'wrong' in sys.argv
print_success = False
replied = False

def answer(value):
    global replied
    replied = True
    os.write(1, value + b'\n')

while data := os.read(0, 65536):
    for _, _, expression in parser.feed(data):
        replied = False
        items = parts(expression)
        name = items[0]
        if name == b'set-option' and items[1] == b':print-success':
            print_success = items[2] == b'true'
        elif name == b'echo':
            value = items[1]
            answer(value[1:-1].replace(b'""', b'"') if raw else value)
        elif name == b'push':
            frames.append([])
        elif name == b'pop':
            frames.pop()
        elif name == b'reset':
            frames = [[]]
            if 'retain-option' not in sys.argv:
                print_success = False
        elif name == b'assert':
            frames[-1].append(items[1])
        elif name in (b'check-sat', b'check-sat-assuming'):
            if len(items) > (1 if name == b'check-sat' else 2):
                answer(b'(error "invalid check")')
                continue
            bad = any(b'false' in frame for frame in frames)
            if 'missing-check' not in sys.argv:
                answer(b'unsat' if bad != wrong else b'sat')
            if 'extra-success' in sys.argv:
                answer(b'success')
            if 'extra-check' in sys.argv:
                answer(b'sat')
        elif name == b'get-model':
            answer(b'(model (define-fun x () String "vox-stream-fixed-2"))')
        elif name == b'get-value':
            answer(b'((x 0))')
        elif name == b'bad':
            answer(b'(error "invalid command")')
        elif name == b'crash':
            sys.exit(2)
        elif name == b'stall':
            time.sleep(5)
        elif name == b'diagnostics':
            os.write(2, b'x' * 200000)
        elif name == b'exit':
            if len(items) != 1:
                answer(b'(error "invalid exit")')
            else:
                if print_success:
                    answer(b'success')
                sys.exit(2 if 'exit-failure' in sys.argv else
                         1 if 'error-exit-status' in sys.argv else 0)
        if print_success and not replied and 'missing-success' not in sys.argv:
            answer(b'success')
'''


class FramerTests(unittest.TestCase):
    def test_all_chunk_boundaries(self):
        data = (b';initial\n(assert (= |x\\|y| "a"";()b"))\n'
                b'; between\n(check-sat) sat "x""y" |z|')
        expected = [b'(assert (= |x\\|y| "a"";()b"))', b'(check-sat)',
                    b'sat', b'"x""y"', b'|z|']
        for width in range(1, len(data) + 1):
            parser = stream.Framer()
            actual = []
            for index in range(0, len(data), width):
                actual.extend(parser.feed(data[index:index + width]))
            actual.extend(parser.feed(b'', True))
            self.assertEqual([part for _, _, part in actual], expected)
            for start, end, part in actual:
                self.assertEqual(data[start:end], part)

    def test_incomplete_and_oversized(self):
        for data in (b'(', b'(assert "x', b'|x', b'(a ; x'):
            with self.assertRaises(stream.FramingError):
                list(stream.Framer().feed(data, True))
        with self.assertRaises(stream.FramingError):
            list(stream.Framer(8).feed(b'(1234567890)'))

    def test_consumed_buffer_is_discarded(self):
        parser = stream.Framer()
        for _ in range(200):
            self.assertEqual(len(list(parser.feed(b'(check-sat)\n' * 100))),
                             100)
            self.assertEqual(len(parser.buffer), 0)

    def test_resource_limits(self):
        for option in stream.RESOURCE_LIMITS:
            self.assertEqual(stream.measured_command(
                b'(set-option ' + option + b' 17)'),
                (b'(set-option ' + option + b' 0)', True))
        timeout = b'(set-option :timeout 123)'
        self.assertEqual(stream.measured_command(timeout), (timeout, False))
        for value in (b'bad', b'-1', b'01'):
            invalid = b'(set-option :rlimit ' + value + b')'
            self.assertEqual(stream.measured_command(invalid), (invalid, False))


class SessionTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.fixture = Path(self.directory.name) / 'solver.py'
        self.fixture.write_text(FAKE)
        self.command = [sys.executable, str(self.fixture),
                        str(Path(stream.__file__).parent)]

    def session(self, *args, **kwargs):
        session = stream.Session([*self.command, *args], **kwargs)
        self.addCleanup(session.close)
        self.assertEqual(session.start()['result'], 'complete')
        return session

    def test_persistent_incremental_and_reset(self):
        session = self.session()
        pid = session.process.pid
        self.assertEqual(session.exchange(b'(check-sat)', 1)['result'], 'sat')
        self.assertEqual(session.exchange(
            b'(push 1)\n(assert false)\n(check-sat)', 1)['result'], 'unsat')
        self.assertEqual(session.exchange(
            b'(pop 1)\n(check-sat-assuming ())', 1)['result'], 'sat')
        self.assertEqual(session.exchange(
            b'(assert false)\n(reset)\n(check-sat)', 1)['result'], 'sat')
        self.assertEqual(session.process.pid, pid)

    def test_nested_marker_is_not_barrier(self):
        session = self.session()
        session.marker_prefix = b'vox-stream-fixed'
        result = session.exchange(b'(get-model)', data_responses=1)
        self.assertEqual(result['result'], 'complete')
        self.assertEqual(result['responses'], {'data': 1})

    def test_source_echo_isolated(self):
        for mode in ('raw', 'quoted'):
            session = self.session(mode)
            for echo in (b'"("', b'"sat"', b'"line\nline"', b'"a""b"',
                         b'""', b'"success"', b'"(error ""echo"")"'):
                result = session.exchange(b'(echo ' + echo + b')', echo=echo)
                self.assertEqual(result['result'], 'complete')
                self.assertEqual(result['responses'], {'echo': 1})

    def test_success_responses_follow_options_and_reset(self):
        for mode in ('standard', 'retain-option'):
            session = self.session(mode)
            self.assertEqual(session.exchange(
                b'(set-option :print-success true)(assert true)')['result'],
                'complete')
            self.assertEqual(session.exchange(b'(reset)')['result'], 'complete')
            self.assertEqual(session.exchange(b'(assert true)')['result'],
                             'complete')
            self.assertEqual(session.exchange(
                b'(set-option :print-success false)')['result'], 'complete')

    def test_extra_or_missing_responses_are_rejected(self):
        for mode in ('extra-success', 'extra-check', 'missing-check'):
            session = self.session(mode)
            self.assertEqual(session.exchange(b'(check-sat)', 1)['result'],
                             'protocol-error')
        session = self.session('missing-success')
        self.assertEqual(session.exchange(
            b'(set-option :print-success true)')['result'], 'protocol-error')

    def test_stderr_is_drained(self):
        session = self.session()
        result = session.exchange(b'(diagnostics)')
        self.assertEqual(result['result'], 'error')
        self.assertEqual(result['stderr_bytes'], 200000)
        self.assertEqual(len(result['stderr_sample']), 1024)

    def test_failure_does_not_restart(self):
        for command in (b'(crash)', b'(stall)'):
            session = self.session(timeout=0.2)
            expected = 'crash' if command == b'(crash)' else 'timeout'
            self.assertEqual(session.exchange(command)['result'], expected)
            self.assertEqual(session.exchange(b'(check-sat)', 1)['result'],
                             'unexecuted')
            self.assertIsNone(session.process)

    def run_script(self, source, **options):
        script = Path(self.directory.name) / 'source.smt2'
        script.write_bytes(source)
        output = Path(self.directory.name) / 'calls.jsonl'
        args = argparse.Namespace(
            script=str(script), output=str(output),
            ocaml=shlex.join(self.command), z3=shlex.join(self.command),
            oracle=None, timeout=1, output_max_mib=1, command_max_mib=1,
            batch_bytes=128, stop_after_checks=None, instant_ms=10, ratio=2,
            allow_matching_errors=False, expected_checks=None, flush_every=1)
        for key, value in options.items():
            setattr(args, key, value)
        with contextlib.redirect_stdout(io.StringIO()):
            status = stream.run(args)
        records = [json.loads(line)
                   for line in output.read_text().splitlines()]
        return status, records

    def test_complete_operations_and_linear_ingestion(self):
        script = (b'(set-info :status sat)\n'
                  + b'(declare-const x Int)\n' * 500
                  + b'(check-sat)\n(get-model)\n(get-value (x))\n'
                  + b'(push 1)\n(assert false)\n(set-info :status unsat)\n'
                  + b'(check-sat)\n(pop 1)\n(set-info :status sat)\n'
                  + b'(check-sat)\n(exit)')
        status, records = self.run_script(script)
        calls = [row for row in records if row['type'] == 'call']
        self.assertEqual(status, 1)
        self.assertFalse(calls[0]['operation_qualification']['pass'])
        self.assertTrue(calls[0]['operation_qualification']
                        ['response_accounting_pass'])
        self.assertEqual(records[-1]['counts']['operation_unvalidated'], 1)
        self.assertEqual([row['ocaml']['result'] for row in calls],
                         ['sat', 'unsat', 'sat'])
        self.assertGreater(calls[0]['ocaml']['seconds'],
                           calls[0]['ocaml']['final_exchange_seconds'])
        self.assertGreater(calls[0]['operation']['ocaml']['seconds'],
                           calls[0]['ocaml']['seconds'])
        self.assertEqual(
            calls[0]['operation']['ocaml']['responses']['data'], 2)
        self.assertEqual(records[-1]['counts']['checks'], 3)
        self.assertLess(records[-1]['solvers']['ocaml']['bytes_sent'],
                        len(script) * 2)

    def test_expected_errors_and_recovery(self):
        script = b'(bad)\n(check-sat)\n(assert false)\n(check-sat)'
        status, records = self.run_script(script, allow_matching_errors=True)
        self.assertEqual(status, 0)
        self.assertEqual(
            records[-1]['counts']['matching_rejected_commands'], 1)
        status, records = self.run_script(script)
        self.assertEqual(status, 1)
        self.assertFalse(records[-1]['all_calls_pass'])

    def test_rejected_check_and_recovery(self):
        script = b'(check-sat bad)\n(check-sat)'
        status, records = self.run_script(script, allow_matching_errors=True)
        self.assertEqual(status, 0)
        self.assertEqual(records[-1]['counts']['rejected_checks'], 1)
        self.assertEqual(records[-1]['counts']['query_pass'], 2)

    def test_rejected_check_cannot_override_expected_answer(self):
        status, records = self.run_script(
            b'(set-info :status sat)(check-sat bad)',
            allow_matching_errors=True)
        self.assertEqual(status, 1)
        self.assertEqual(records[-1]['counts']['query_fail'], 1)

    def test_exit_arity_and_unreachable_input(self):
        status, records = self.run_script(
            b'(exit 1)(check-sat)', allow_matching_errors=True,
            expected_checks=1)
        self.assertEqual(status, 0)
        self.assertEqual(records[-1]['counts']['matching_rejected_commands'], 1)
        for tail in (b'(check-sat)', b'('):
            status, records = self.run_script(
                b'(set-option :print-success true)(check-sat)(exit)' + tail,
                expected_checks=1)
            self.assertEqual(status, 0)
            self.assertTrue(records[-1]['execution_ended_by_exit'])
            exit_record = next(row for row in records if row['type'] == 'exit')
            self.assertEqual(exit_record['solvers']['ocaml']['responses'],
                             {'success': 1})

    def test_bad_exit_status_cannot_pass(self):
        status, records = self.run_script(
            b'(check-sat)(exit)',
            ocaml=shlex.join([*self.command, 'exit-failure']))
        self.assertEqual(status, 1)
        self.assertFalse(records[-1]['complete'])

    def test_exit_samples_memory_before_process_termination(self):
        session = self.session()
        sampled = []

        def memory():
            self.assertIsNone(session.process.poll())
            sampled.append(True)
            return 0

        with patch.object(session, 'memory', side_effect=memory):
            self.assertEqual(session.exchange(b'(exit)', terminal=True)
                             ['result'], 'complete')
        self.assertEqual(sampled, [True])

    def test_exit_status_preserves_expected_error_history(self):
        status, records = self.run_script(
            b'(bad)(check-sat)(exit)', allow_matching_errors=True,
            z3=shlex.join([*self.command, 'error-exit-status']))
        self.assertEqual(status, 0)
        exit_record = next(row for row in records if row['type'] == 'exit')
        self.assertEqual(exit_record['solvers']['z3']['returncode'], 1)

    def test_unvalidated_auxiliary_response_cannot_pass(self):
        status, records = self.run_script(b'(get-value (x))(check-sat)')
        self.assertEqual(status, 1)
        self.assertEqual(records[-1]['counts']['unvalidated_responses'], 1)
        self.assertFalse(records[-1]['all_calls_pass'])

    def test_prefix_smoke_is_incomplete(self):
        status, records = self.run_script(
            b'(check-sat)\n(get-model)\n(check-sat)', stop_after_checks=1)
        self.assertEqual(status, 1)
        self.assertEqual(records[-1]['counts']['checks'], 1)
        self.assertEqual(records[-1]['counts']['operations'], 1)
        self.assertFalse(records[-1]['source_scan_complete'])
        self.assertFalse(records[-1]['all_calls_pass'])

    def test_missing_data_response_is_rejected(self):
        session = self.session()
        self.assertEqual(session.exchange(
            b'(get-info :name)', data_responses=1)['result'],
            'protocol-error')

    def test_wrong_answers_and_missing_calls_fail(self):
        script = b'(assert false)\n(check-sat)'
        status, records = self.run_script(
            script, z3=shlex.join([*self.command, 'wrong']))
        self.assertEqual(status, 1)
        self.assertEqual(records[-1]['counts']['disagreements'], 1)
        status, records = self.run_script(script, expected_checks=2)
        self.assertEqual(status, 1)
        self.assertFalse(records[-1]['check_count_matches'])

    def test_reset_does_not_hide_unexpected_error(self):
        status, records = self.run_script(b'(bad)\n(reset)\n(check-sat)')
        self.assertEqual(status, 1)
        self.assertEqual(records[-1]['counts']['query_pass'], 1)
        self.assertGreater(records[-1]['counts']['protocol_failures'], 0)

    def test_truncation_fails(self):
        status, records = self.run_script(b'(check-sat)\n(assert')
        self.assertEqual(status, 1)
        self.assertFalse(records[-1]['complete'])

    def test_timeout_counts_remaining_calls(self):
        status, records = self.run_script(
            b'(stall)\n(check-sat)\n(check-sat)', timeout=0.2)
        self.assertEqual(status, 1)
        self.assertEqual(records[-1]['counts']['checks'], 2)
        self.assertFalse(records[-1]['complete'])


class QualificationTests(unittest.TestCase):
    def test_unknown_reference_needs_oracle(self):
        candidate = {'result': 'sat', 'seconds': 0.001}
        z3 = {'result': 'unknown', 'seconds': 0.1}
        self.assertFalse(stream.qualify(candidate, z3, None, None, False,
                                        0.01, 2)['pass'])
        self.assertTrue(stream.qualify(candidate, z3, candidate, None, False,
                                       0.01, 2)['pass'])
        candidate['seconds'] = 0.02
        self.assertIsNone(stream.qualify(candidate, z3, candidate, None,
                                        False, 0.01, 2)['performance'])

    def test_disagreement_never_adjudicated_away(self):
        candidate = {'result': 'sat', 'seconds': 0.001}
        z3 = {'result': 'unsat', 'seconds': 0.1}
        self.assertFalse(stream.qualify(candidate, z3, candidate, None, False,
                                        0.01, 2)['pass'])


if __name__ == '__main__':
    unittest.main()
