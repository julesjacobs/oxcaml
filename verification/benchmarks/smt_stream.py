#!/usr/bin/env python3
"""Qualify every SMT-LIB check using persistent solver sessions."""

import argparse
from collections import Counter
import gzip
import hashlib
import json
import os
from pathlib import Path
import re
import resource
import secrets
import selectors
import shlex
import shutil
import subprocess
import sys
import time


CHECKS = {'check-sat', 'check-sat-assuming', 'check-sat-using'}
FOLLOWUPS = {'get-model', 'get-value', 'get-assignment', 'get-unsat-core',
             'get-unsat-assumptions', 'get-assertions'}
QUERIES = FOLLOWUPS | {'get-info', 'get-option', 'echo'}
RESOURCE_LIMITS = {b':rlimit', b':rlimit-per',
                   b':reproducible-resource-limit'}
SPECIAL = re.compile(rb'[();"|]')
DELIMITER = re.compile(rb'[\s();"|]')
SYMBOL_END = re.compile(rb'[|\\]')
LINE_END = re.compile(rb'[\r\n]')
SPACE = re.compile(rb'\s+')


class FramingError(ValueError):
    pass


class Framer:
    """A linear-time framer; RAM is bounded by the largest expression."""

    def __init__(self, limit=64 * 1024 * 1024):
        self.buffer = bytearray()
        self.base = 0
        self.pos = 0
        self.start = None
        self.depth = 0
        self.mode = 'normal'
        self.limit = limit

    def feed(self, data, final=False):
        self.buffer.extend(data)
        buf = self.buffer
        i = self.pos
        n = len(buf)
        while i < n:
            if self.start is not None and i - self.start > self.limit:
                raise FramingError('expression exceeds configured byte limit')
            if self.mode == 'comment':
                match = LINE_END.search(buf, i)
                if match is None:
                    i = n
                    break
                i = match.end()
                self.mode = 'normal'
                continue
            if self.mode == 'string':
                end = buf.find(b'"', i)
                if end < 0:
                    i = n
                    break
                i = end + 1
                self.mode = 'string-quote'
                continue
            if self.mode == 'string-quote':
                if buf[i] == 34:
                    i += 1
                    self.mode = 'string'
                    continue
                self.mode = 'normal'
                if self.depth == 0:
                    yield self.base + self.start, self.base + i, bytes(
                        buf[self.start:i])
                    self.start = None
                continue
            if self.mode == 'symbol-escape':
                i += 1
                self.mode = 'symbol'
                continue
            if self.mode == 'symbol':
                match = SYMBOL_END.search(buf, i)
                if match is None:
                    i = n
                    break
                i = match.end()
                if buf[i - 1] == 92:
                    self.mode = 'symbol-escape'
                    continue
                self.mode = 'normal'
                if self.depth == 0:
                    yield self.base + self.start, self.base + i, bytes(
                        buf[self.start:i])
                    self.start = None
                continue
            if self.mode == 'atom':
                match = DELIMITER.search(buf, i)
                if match is None:
                    i = n
                    break
                i = match.start()
                yield self.base + self.start, self.base + i, bytes(
                    buf[self.start:i])
                self.start = None
                self.mode = 'normal'
                continue
            byte = buf[i]
            if self.start is None:
                match = SPACE.match(buf, i)
                if match:
                    i = match.end()
                    continue
                if byte == 59:
                    self.mode = 'comment'
                    i += 1
                    continue
                if byte == 41:
                    raise FramingError('unexpected closing parenthesis')
                self.start = i
                if byte not in (40, 34, 124):
                    self.mode = 'atom'
                    continue
            if byte == 40:
                self.depth += 1
            elif byte == 41:
                self.depth -= 1
                if self.depth == 0:
                    yield self.base + self.start, self.base + i + 1, bytes(
                        buf[self.start:i + 1])
                    self.start = None
            elif byte == 59:
                self.mode = 'comment'
            elif byte == 34:
                self.mode = 'string'
            elif byte == 124:
                self.mode = 'symbol'
            else:
                match = SPECIAL.search(buf, i)
                i = match.start() if match else n
                continue
            i += 1
        self.pos = i
        if self.start is not None and n - self.start > self.limit:
            raise FramingError('expression exceeds configured byte limit')
        if final:
            if self.mode in ('atom', 'string-quote') and self.depth == 0:
                yield self.base + self.start, self.base + n, bytes(
                    buf[self.start:n])
                self.start = None
                self.mode = 'normal'
            if self.start is not None:
                raise FramingError('incomplete final expression')
        discard = self.start if self.start is not None else self.pos
        if discard:
            del self.buffer[:discard]
            self.base += discard
            self.pos -= discard
            if self.start is not None:
                self.start = 0


def expressions(path, limit):
    parser = Framer(limit)
    with open(path, 'rb') as source:
        while data := source.read(65536):
            yield from parser.feed(data)
        yield from parser.feed(b'', final=True)


def parts(expression):
    if not expression.startswith(b'(') or not expression.endswith(b')'):
        return []
    return [part for _, _, part in Framer().feed(expression[1:-1], True)]


def command_name(expression):
    match = re.match(rb'\(\s*([^\s();]+)', expression)
    if match:
        return match[1].decode('ascii', 'replace')
    items = parts(expression)
    return items[0].decode('ascii', 'replace') if items else ''


def measured_command(expression):
    if command_name(expression) == 'set-option':
        items = parts(expression)
        if (len(items) == 3 and items[1] in RESOURCE_LIMITS
                and re.fullmatch(rb'(?:0|[1-9][0-9]*)', items[2])):
            return b'(set-option ' + items[1] + b' 0)', True
    return expression, False


def file_hash(path):
    digest = hashlib.sha256()
    with open(path, 'rb') as source:
        while data := source.read(1024 * 1024):
            digest.update(data)
    return digest.hexdigest()


def executable_info(command):
    executable = shutil.which(command[0])
    if executable is None:
        raise ValueError('executable not found: ' + command[0])
    return {'command': command, 'path': str(Path(executable).resolve()),
            'sha256': file_hash(executable)}


class Session:
    def __init__(self, command, timeout=60, output_limit=64 * 1024 * 1024):
        self.command = command
        self.timeout = timeout
        self.output_limit = output_limit
        self.process = None
        self.bytes_sent = 0
        self.exchanges = 0
        self.peak_rss_kib = 0
        self.failure = None
        self.print_success = {False}
        self.had_errors = False
        self.marker_prefix = b'vox-stream-' + secrets.token_hex(24).encode()

    def start(self):
        started = time.perf_counter()
        try:
            self.process = subprocess.Popen(
                self.command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, bufsize=0)
            for pipe in (self.process.stdin, self.process.stdout,
                         self.process.stderr):
                os.set_blocking(pipe.fileno(), False)
            response = self.exchange(b'(set-option :print-success false)\n')
        except OSError as error:
            response = {'result': 'crash', 'seconds': 0,
                        'errors': [str(error)]}
            self.failure = 'crash'
        response['seconds'] = time.perf_counter() - started
        return response

    def memory(self):
        if self.process is None:
            return self.peak_rss_kib
        try:
            status = Path(f'/proc/{self.process.pid}/status').read_text()
            match = re.search(r'^VmHWM:\s+(\d+)', status, re.MULTILINE)
            if match:
                self.peak_rss_kib = max(self.peak_rss_kib, int(match[1]))
        except OSError:
            pass
        return self.peak_rss_kib

    def close(self):
        if self.process is not None:
            self.memory()
            if self.process.poll() is None:
                self.process.kill()
            self.process.communicate()
            self.process = None

    def exchange(self, script, checks=0, echo=None, data_responses=0,
                 terminal=False):
        if self.failure or self.process is None:
            return {'result': 'unexecuted', 'seconds': 0,
                    'errors': ['session unavailable after '
                               + str(self.failure)]}
        if terminal:
            self.memory()
        self.exchanges += 1
        success_states = {(policy, 0) for policy in self.print_success}
        for _, _, expression in Framer().feed(script, final=True):
            name = command_name(expression)
            if name in CHECKS | QUERIES:
                continue
            items = parts(expression)
            next_states = set()
            for policy, count in success_states:
                if (len(items) == 3 and items[:2]
                        == [b'set-option', b':print-success']
                        and items[2] in (b'true', b'false')):
                    policy = items[2] == b'true'
                if items == [b'reset']:
                    # Z3 retains this option; the standard restores false.
                    next_states.add((False, count))
                next_states.add((policy, count + int(policy)))
            success_states = next_states
        marker = self.marker_prefix + b'-' + str(self.exchanges).encode()
        outgoing = memoryview(script + b'\n' + (
            b'' if terminal else b'(echo "' + marker + b'")\n'))
        self.bytes_sent += len(outgoing)
        framer = Framer(self.output_limit)
        output_hash = hashlib.sha256()
        stderr_hash = hashlib.sha256()
        output_bytes = 0
        stderr_bytes = 0
        stderr_sample = bytearray()
        answers = []
        kinds = Counter()
        errors = []
        complete = False
        echo_buffer = bytearray()
        echo_variants = None
        if echo is not None:
            echo_variants = (echo[1:-1].replace(b'""', b'"') + b'\n',
                             echo + b'\n')

        def consume(response):
            nonlocal complete
            if not terminal and response in (marker, b'"' + marker + b'"'):
                if outgoing:
                    raise FramingError('premature barrier')
                complete = True
                return
            if complete:
                raise FramingError('response after barrier')
            output_hash.update(response + b'\n')
            if response in (b'sat', b'unsat', b'unknown'):
                answers.append(response.decode())
                kinds['answer'] += 1
            elif response == b'success':
                kinds['success'] += 1
            elif response == b'unsupported':
                kinds['unsupported'] += 1
            elif command_name(response) == 'error':
                kinds['error'] += 1
                if len(errors) < 4:
                    errors.append(response[:512].decode(errors='replace'))
            else:
                kinds['data'] += 1

        started = time.perf_counter()
        try:
            with selectors.DefaultSelector() as selector:
                selector.register(self.process.stdin, selectors.EVENT_WRITE)
                selector.register(self.process.stdout, selectors.EVENT_READ)
                selector.register(self.process.stderr, selectors.EVENT_READ)
                while not complete:
                    remaining = self.timeout - (time.perf_counter() - started)
                    if remaining <= 0:
                        raise TimeoutError
                    events = selector.select(remaining)
                    if not events:
                        raise TimeoutError
                    for key, _ in events:
                        if key.fileobj is self.process.stdin:
                            count = key.fileobj.write(outgoing[:65536])
                            if count is None:
                                continue
                            if not count:
                                raise BrokenPipeError('solver closed stdin')
                            outgoing = outgoing[count:]
                            if not outgoing:
                                selector.unregister(key.fileobj)
                            continue
                        chunk = key.fileobj.read(65536)
                        if chunk is None:
                            continue
                        if not chunk:
                            if key.fileobj is self.process.stderr:
                                selector.unregister(key.fileobj)
                                continue
                            if terminal and not outgoing:
                                for _, _, response in framer.feed(b'', True):
                                    consume(response)
                                complete = True
                                selector.unregister(key.fileobj)
                                continue
                            raise BrokenPipeError('solver closed stdout')
                        if key.fileobj is self.process.stderr:
                            stderr_bytes += len(chunk)
                            stderr_hash.update(chunk)
                            stderr_sample.extend(chunk[:max(
                                0, 1024 - len(stderr_sample))])
                            if stderr_bytes > self.output_limit:
                                raise FramingError('stderr exceeds byte limit')
                            continue
                        output_bytes += len(chunk)
                        if output_bytes > self.output_limit:
                            raise FramingError('stdout exceeds byte limit')
                        if echo_variants is not None:
                            echo_buffer.extend(chunk)
                            matched = next((value for value in echo_variants
                                            if echo_buffer.startswith(value)),
                                           None)
                            if matched is None:
                                if any(value.startswith(echo_buffer)
                                       for value in echo_variants):
                                    continue
                                raise FramingError('source echo mismatch')
                            output_hash.update(matched)
                            kinds['echo'] += 1
                            chunk = bytes(echo_buffer[len(matched):])
                            echo_buffer.clear()
                            echo_variants = None
                        for _, _, response in framer.feed(chunk):
                            consume(response)
                if framer.start is not None:
                    raise FramingError('incomplete output after barrier')
                if terminal:
                    remaining = self.timeout - (time.perf_counter() - started)
                    allowed = (0, 1) if self.had_errors else (0,)
                    returncode = self.process.wait(timeout=max(0, remaining))
                    if returncode not in allowed:
                        raise BrokenPipeError('solver rejected exit')
                while chunk := self.process.stderr.read(65536):
                    stderr_bytes += len(chunk)
                    stderr_hash.update(chunk)
                    stderr_sample.extend(chunk[:max(
                        0, 1024 - len(stderr_sample))])
                    if stderr_bytes > self.output_limit:
                        raise FramingError('stderr exceeds byte limit')
            result = (answers[0] if len(answers) == checks == 1 else
                      'complete' if len(answers) == checks == 0 else
                      'protocol-error')
            if kinds['data'] != data_responses:
                result = 'protocol-error'
                errors.append('unexpected data response count')
            if kinds['error'] or kinds['unsupported'] or stderr_bytes:
                result = 'error'
                self.had_errors |= bool(kinds['error'])
            else:
                policies = {policy for policy, count in success_states
                            if count == kinds['success']}
                if not policies:
                    result = 'protocol-error'
                    errors.append('unexpected success response count')
                else:
                    self.print_success = policies
        except (TimeoutError, subprocess.TimeoutExpired):
            result = 'timeout'
            self.failure = result
        except (OSError, ValueError) as error:
            result = ('protocol-error' if isinstance(error, FramingError)
                      else 'crash')
            self.failure = result
            errors.append(str(error))
        elapsed = time.perf_counter() - started
        if self.failure:
            self.close()
        record = {'result': result, 'seconds': elapsed,
                  'responses': dict(kinds),
                  'response_sha256': output_hash.hexdigest()}
        if answers:
            record['answers'] = answers
        if errors:
            record['errors'] = errors
        if stderr_bytes:
            record['stderr_bytes'] = stderr_bytes
            record['stderr_sha256'] = stderr_hash.hexdigest()
            record['stderr_sample'] = stderr_sample.decode(errors='replace')
        if terminal and self.process is not None:
            record['returncode'] = self.process.returncode
        return record


def add_measurement(total, item):
    total['seconds'] += item['seconds']
    total['exchanges'] += 1
    if item['result'] not in ('complete', 'sat', 'unsat', 'unknown'):
        total['failures'].append(item['result'])
        del total['failures'][8:]
    for key, count in item.get('responses', {}).items():
        total['responses'][key] += count


def accumulator():
    return {'seconds': 0.0, 'exchanges': 0, 'failures': [],
            'responses': Counter()}


def speed_gate(candidate, reference, instant, ratio):
    if candidate['result'] not in ('sat', 'unsat'):
        return False
    if candidate['seconds'] <= instant:
        return True
    if reference['result'] in ('sat', 'unsat'):
        return candidate['seconds'] <= ratio * reference['seconds']
    if reference['result'] == 'timeout':
        if candidate['seconds'] <= ratio * reference['seconds']:
            return True
    return None


def qualify(candidate, reference, oracle, expected, tainted, instant, ratio):
    result = candidate['result']
    independent = reference['result']
    evidence = 'z3'
    if independent not in ('sat', 'unsat') and oracle is not None:
        independent = oracle['result']
        evidence = 'oracle'
    definitive = result in ('sat', 'unsat')
    disagreement = (definitive and any(
        item and item['result'] in ('sat', 'unsat')
        and item['result'] != result for item in (reference, oracle)))
    expected_matches = expected not in ('sat', 'unsat') or result == expected
    correct = (definitive and independent == result and not disagreement
               and expected_matches and not tainted)
    performance = speed_gate(candidate, reference, instant, ratio)
    return {'correct': correct, 'performance': performance,
            'pass': correct and performance is True,
            'reference': evidence, 'disagreement': disagreement}


def matching_errors(records):
    return all(
        row['result'] == 'error'
        and row.get('responses', {}).get('error') == 1
        and not row.get('answers') and not row.get('stderr_bytes')
        and not row.get('responses', {}).get('unsupported')
        and not row.get('responses', {}).get('data')
        and not row.get('responses', {}).get('success')
        and not row.get('responses', {}).get('echo')
        for row in records.values())


def run(args):
    source_path = Path(args.script).resolve()
    source_stat = source_path.stat()
    output_path = Path(args.output).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    commands = {'ocaml': shlex.split(args.ocaml), 'z3': shlex.split(args.z3)}
    if args.oracle:
        commands['oracle'] = shlex.split(args.oracle)
    provenance = {
        'schema': 'vox-smt-stream-v2', 'source': str(source_path),
        'source_bytes': source_stat.st_size,
        'source_sha256': file_hash(source_path),
        'runner_sha256': file_hash(__file__),
        'solvers': {key: executable_info(value)
                    for key, value in commands.items()},
        'cpu_affinity': sorted(os.sched_getaffinity(0))
        if hasattr(os, 'sched_getaffinity') else None,
        'python': sys.version, 'options': vars(args),
        'timing': 'persistent ingestion plus check; startup separate; '
                  'barrier wall time included; resource limits disabled',
        'certificate_validation': 'response accounting only; models and cores '
                                  'are not independently checked',
    }
    sessions = {key: Session(value, args.timeout,
                             args.output_max_mib * 1024 * 1024)
                for key, value in commands.items()}
    counts = Counter()
    startup = {}
    accumulated = {key: accumulator() for key in sessions}
    tainted = set()
    pending = None
    batch = bytearray()
    original_hash = hashlib.sha256()
    measured_hash = hashlib.sha256()
    expected = None
    disabled = 0
    command_index = 0
    ordinal = 0
    ended = False
    aborted = None
    prefix_only = False
    started = time.perf_counter()
    writer = gzip.open if output_path.suffix == '.gz' else open
    with writer(output_path, 'wt', encoding='utf-8') as output:
        def emit(item):
            output.write(json.dumps(item, separators=(',', ':')) + '\n')

        def exchange(script, checks=0, echo=None, data_responses=0,
                     terminal=False):
            keys = list(sessions)
            if ordinal % 2:
                keys[:2] = reversed(keys[:2])
            records = {}
            for key in keys:
                records[key] = sessions[key].exchange(
                    script, checks, echo, data_responses, terminal)
            return records

        def flush():
            if not batch:
                return
            records = exchange(bytes(batch))
            for key, record in records.items():
                add_measurement(accumulated[key], record)
                if record['result'] != 'complete':
                    counts['protocol_failures'] += 1
                    tainted.add(key)
            counts['ingestion_batches'] += 1
            batch.clear()

        def finish():
            nonlocal pending
            if pending is None:
                return
            if pending['followup_commands']:
                operation = {}
                for key in sessions:
                    query = pending[key]
                    tail = pending['operation'][key]
                    operation[key] = {
                        'result': query['result'],
                        'seconds': query['seconds'] + tail['seconds'],
                        'responses': dict(tail['responses']),
                        'failures': tail['failures'],
                    }
                gate = qualify(operation['ocaml'], operation['z3'],
                               operation.get('oracle'), pending['expected'],
                               pending['tainted'] or any(
                                   row['failures']
                                   for row in operation.values()),
                               args.instant_ms / 1000, args.ratio)
                unvalidated = pending['unvalidated_followups']
                if unvalidated:
                    gate['response_accounting_pass'] = gate['pass']
                    gate['pass'] = False
                    gate['unvalidated_commands'] = sorted(unvalidated)
                    counts['operation_unvalidated'] += 1
                counts['operations'] += 1
                counts['operation_pass' if gate['pass']
                       else 'operation_fail'] += 1
                pending['operation'] = operation
                pending['operation_qualification'] = gate
                pending['operation_certificate_validation'] = 'not-performed'
            else:
                del pending['operation']
                del pending['followup_commands']
            del pending['unvalidated_followups']
            emit(pending)
            pending = None
            if ordinal % args.flush_every == 0:
                output.flush()
                for session in sessions.values():
                    session.memory()

        emit({'type': 'provenance', **provenance})
        try:
            for key, session in sessions.items():
                startup[key] = session.start()
                if startup[key]['result'] != 'complete':
                    counts['protocol_failures'] += 1
                    tainted.add(key)
            emit({'type': 'startup', 'solvers': startup})
            for start, end, original in expressions(
                    source_path, args.command_max_mib * 1024 * 1024):
                if (args.stop_after_checks
                        and ordinal >= args.stop_after_checks
                        and command_name(original) not in QUERIES):
                    prefix_only = True
                    break
                command_index += 1
                name = command_name(original)
                counts['commands'] += 1
                if name not in QUERIES:
                    finish()
                original_hash.update(original + b'\n')
                expression, rewritten = measured_command(original)
                measured_hash.update(expression + b'\n')
                disabled += int(rewritten)
                if name == 'set-info':
                    items = parts(expression)
                    if len(items) == 3 and items[1] == b':status':
                        expected = items[2].decode('ascii', 'replace')
                if name == 'exit' and parts(expression) == [b'exit']:
                    flush()
                    records = exchange(expression, terminal=True)
                    emit({'type': 'exit', 'solvers': records})
                    for key, record in records.items():
                        if record['result'] != 'complete':
                            counts['protocol_failures'] += 1
                            tainted.add(key)
                    ended = True
                    break
                if name in CHECKS:
                    ordinal += 1
                    counts['checks'] += 1
                    script = bytes(batch) + expression + b'\n'
                    batch.clear()
                    records = exchange(script, checks=1)
                    rejected = (args.allow_matching_errors
                                and expected not in ('sat', 'unsat')
                                and matching_errors(records))
                    for key, record in records.items():
                        prior = accumulated[key]
                        record['final_exchange_seconds'] = record['seconds']
                        record['seconds'] += prior['seconds']
                        if prior['exchanges']:
                            record['prior_ingestion'] = prior
                        if (not rejected and record['result'] not in
                                ('sat', 'unsat', 'unknown')):
                            tainted.add(key)
                        accumulated[key] = accumulator()
                    gate = qualify(records['ocaml'], records['z3'],
                                   records.get('oracle'), expected,
                                   bool(tainted),
                                   args.instant_ms / 1000, args.ratio)
                    if rejected:
                        candidate = dict(records['ocaml'], result='sat')
                        reference = dict(records['z3'], result='sat')
                        performance = speed_gate(candidate, reference,
                                                 args.instant_ms / 1000,
                                                 args.ratio)
                        gate = {'correct': not tainted,
                                'performance': performance,
                                'pass': not tainted and performance is True,
                                'reference': 'matching-command-rejection',
                                'disagreement': False, 'expected_error': True}
                        counts['matching_rejected_commands'] += 1
                        counts['rejected_checks'] += 1
                    counts['query_pass' if gate['pass'] else 'query_fail'] += 1
                    counts['disagreements'] += int(gate['disagreement'])
                    for key, record in records.items():
                        counts[key + '_' + record['result']] += 1
                    pending = {
                        'type': 'call', 'ordinal': ordinal,
                        'command_index': command_index, 'start': start,
                        'end': end, 'command': name, 'expected': expected,
                        'history_sha256': original_hash.hexdigest(),
                        'measurement_sha256': measured_hash.hexdigest(),
                        'disabled_resource_limits': disabled,
                        **records, 'qualification': gate,
                        'tainted': sorted(tainted),
                        'followup_commands': Counter(),
                        'unvalidated_followups': [],
                        'operation': {key: accumulator() for key in sessions},
                    }
                    continue
                strict = (args.allow_matching_errors
                          or name in QUERIES | {'reset'})
                if strict:
                    flush()
                    items = parts(expression) if name == 'echo' else []
                    echo = (items[1] if len(items) == 2
                            and items[1].startswith(b'"') else None)
                    records = exchange(expression, echo=echo, data_responses=
                                       int(name in QUERIES and name != 'echo'))
                    matching_error = (args.allow_matching_errors
                                      and matching_errors(records))
                    if name in QUERIES - {'echo'} and not matching_error:
                        counts['unvalidated_responses'] += 1
                        if pending:
                            pending['unvalidated_followups'].append(name)
                    if matching_error:
                        counts['matching_rejected_commands'] += 1
                        emit({'type': 'expected-error',
                              'command_index': command_index,
                              'start': start, 'end': end,
                              'solvers': records})
                    for key, record in records.items():
                        accounted = dict(record)
                        if matching_error:
                            accounted['result'] = 'complete'
                        target = (pending['operation'][key] if pending
                                  else accumulated[key])
                        add_measurement(target, accounted)
                        if (not matching_error
                                and record['result'] != 'complete'):
                            counts['protocol_failures'] += 1
                            tainted.add(key)
                    if pending:
                        pending['followup_commands'][name] += 1
                    if name == 'reset' and all(
                            row['result'] == 'complete'
                            for row in records.values()):
                        tainted.clear()
                        expected = None
                    continue
                batch.extend(expression + b'\n')
                if len(batch) >= args.batch_bytes:
                    flush()
            finish()
            flush()
        except (OSError, ValueError) as error:
            aborted = str(error)
            finish()
        finally:
            for session in sessions.values():
                session.close()
        try:
            final_stat = source_path.stat()
            source_unchanged = (source_stat.st_ino == final_stat.st_ino
                                and source_stat.st_size == final_stat.st_size
                                and source_stat.st_mtime_ns
                                == final_stat.st_mtime_ns)
        except OSError:
            source_unchanged = False
        if not source_unchanged:
            aborted = 'source changed during replay'
        runner_memory = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
        if sys.platform == 'darwin':
            runner_memory //= 1024
        summary = {
            'type': 'summary', 'counts': dict(counts),
            'runner_peak_rss_kib': runner_memory,
            'complete': aborted is None and not prefix_only
                        and not any(session.failure
                                    for session in sessions.values()),
            'all_calls_pass': aborted is None and not prefix_only
                              and counts['query_fail'] == 0
                              and counts['operation_fail'] == 0
                              and counts['protocol_failures'] == 0
                              and counts['unvalidated_responses'] == 0
                              and not tainted,
            'aborted': aborted, 'seconds': time.perf_counter() - started,
            'source_sha256': provenance['source_sha256'],
            'source_unchanged': source_unchanged,
            'source_scan_complete': aborted is None and not prefix_only,
            'prefix_only': prefix_only,
            'execution_ended_by_exit': ended,
            'solvers': {key: {'bytes_sent': session.bytes_sent,
                              'exchanges': session.exchanges,
                              'peak_rss_kib': session.peak_rss_kib or None,
                              'failure': session.failure}
                        for key, session in sessions.items()},
            'certificate_validation': 'not-performed',
        }
        if args.expected_checks is not None:
            summary['expected_checks'] = args.expected_checks
            summary['check_count_matches'] = (
                counts['checks'] == args.expected_checks)
            if not summary['check_count_matches']:
                summary['complete'] = False
                summary['all_calls_pass'] = False
        emit(summary)
    summary_path = Path(str(output_path) + '.summary.json')
    summary_path.write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, sort_keys=True))
    return 0 if summary['complete'] and summary['all_calls_pass'] else 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('script')
    parser.add_argument('--ocaml', required=True,
                        help='shell-quoted executable and arguments')
    parser.add_argument('--z3', default='z3 -in -smt2')
    parser.add_argument('--oracle',
                        help='optional third persistent solver')
    parser.add_argument('--output', required=True,
                        help='JSONL records, optionally .gz')
    parser.add_argument('--timeout', type=float, default=60,
                        help='external watchdog per exchange, seconds')
    parser.add_argument('--instant-ms', type=float, default=10)
    parser.add_argument('--ratio', type=float, default=2)
    parser.add_argument('--batch-bytes', type=int, default=1024 * 1024)
    parser.add_argument('--command-max-mib', type=int, default=64)
    parser.add_argument('--output-max-mib', type=int, default=64)
    parser.add_argument('--flush-every', type=int, default=1000)
    parser.add_argument('--expected-checks', type=int)
    parser.add_argument('--allow-matching-errors', action='store_true',
                        help='require matching errors with command barriers')
    parser.add_argument('--stop-after-checks', type=int,
                        help='incomplete prefix smoke; never qualifies '
                             'the full script')
    args = parser.parse_args()
    for key in ('timeout', 'instant_ms', 'ratio', 'batch_bytes',
                'command_max_mib', 'output_max_mib', 'flush_every'):
        if getattr(args, key) <= 0:
            parser.error(key + ' must be positive')
    return run(args)


if __name__ == '__main__':
    sys.exit(main())
