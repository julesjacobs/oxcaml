#!/usr/bin/env python3
"""Compare installed compiler backends on the captured demo source inventory."""
import argparse
import collections
import hashlib
import json
import math
import os
from pathlib import Path
import signal
import shutil
import statistics
import subprocess
import time


PARITY_REPEATS = 15
PARITY_ALPHA = .05
PARITY_MARGIN = .01


def source_performance(backends, zero_calls):
    medians = {backend: statistics.median(row['seconds'] for row in rows)
               for backend, rows in backends.items()}
    faster = medians['ocaml'] < medians['z3']
    if not zero_calls:
        return dict(faster=faster, performance_pass=faster,
                    criterion='strict_speedup')
    paired = {backend: {row['repeat']: row['seconds'] for row in rows}
              for backend, rows in backends.items()}
    repeats = sorted(paired['ocaml'].keys() & paired['z3'].keys())
    ratios = [paired['ocaml'][r] / paired['z3'][r] for r in repeats]
    above = sum(r > 1 - PARITY_MARGIN for r in ratios)
    below = sum(r < 1 + PARITY_MARGIN for r in ratios)
    def upper_tail(count):
        return (sum(math.comb(len(ratios), i)
                    for i in range(count, len(ratios) + 1)) / 2 ** len(ratios))
    lower_p, upper_p = upper_tail(above), upper_tail(below)
    probability = max(lower_p, upper_p)
    enough = len(repeats) >= PARITY_REPEATS
    return dict(faster=faster, performance_pass=faster or
                (enough and probability < PARITY_ALPHA),
                criterion='strict_speedup_or_equivalence',
                parity=dict(pairs=len(repeats), margin=PARITY_MARGIN,
                            lower_p=lower_p, upper_p=upper_p,
                            p_value=probability,
                            sufficient_samples=enough))


def adjust_parity_family(results):
    family = sorted((row for row in results if row['zero_calls_verified']),
                    key=lambda row: row['parity']['p_value'])
    adjusted = 0.
    for rank, row in enumerate(family):
        parity = row['parity']
        adjusted = max(adjusted, min(1.,
            (len(family) - rank) * parity['p_value']))
        parity.update(adjusted_p_value=adjusted, family_size=len(family),
                      correction='Holm')
        parity['equivalent'] = (parity['sufficient_samples']
                                and adjusted < PARITY_ALPHA)
        row['performance_pass'] = (row['correct']
            and (row['faster'] or parity['equivalent']))


def digest(path):
    result = hashlib.sha256()
    with Path(path).open('rb') as source:
        for block in iter(lambda: source.read(1024 * 1024), b''):
            result.update(block)
    return result.hexdigest()


def dependency_hashes(directory):
    return {str(path.relative_to(directory)): digest(path)
            for path in sorted(directory.rglob('*'))
            if path.is_file() and path.suffix in
            ('.cmi', '.cmx', '.cmxa', '.cmo', '.cma', '.o', '.a')}


def cpu_configuration(cpu):
    directory = Path('/sys/devices/system/cpu') / f'cpu{cpu}'

    def read(path):
        return path.read_text().strip() if path.is_file() else None

    caches = [{name: read(cache / name) for name in
               ('level', 'type', 'size', 'shared_cpu_list')}
              for cache in sorted((directory / 'cache').glob('index*'))]
    return {
        'affinity': sorted(os.sched_getaffinity(0)),
        'core_id': read(directory / 'topology/core_id'),
        'thread_siblings': read(directory / 'topology/thread_siblings_list'),
        'governor': read(directory / 'cpufreq/scaling_governor'),
        'caches': caches,
    }


def compile_source(command, cwd, environment, timeout, output, name, stdin=None):
    with (output / (name + '.stdout')).open('wb') as stdout, \
            (output / (name + '.stderr')).open('wb') as stderr:
        started = time.monotonic()
        process = subprocess.Popen(command, cwd=cwd, env=environment,
                                   stdout=stdout, stderr=stderr,
                                   stdin=stdin,
                                   start_new_session=True)
        timed_out = False

        def expire(_signal, _frame):
            nonlocal timed_out
            timed_out = True
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass

        previous = signal.signal(signal.SIGALRM, expire)
        try:
            signal.setitimer(signal.ITIMER_REAL,
                             max(.000001, timeout - (time.monotonic() - started)))
            _, status, usage = os.wait4(process.pid, 0)
            seconds = time.monotonic() - started
            process.returncode = os.waitstatus_to_exitcode(status)
        finally:
            signal.setitimer(signal.ITIMER_REAL, 0)
            signal.signal(signal.SIGALRM, previous)
            if process.returncode is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
    return dict(seconds=seconds, returncode=process.returncode,
                timed_out=timed_out, peak_rss_bytes=usage.ru_maxrss * 1024)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--prefix', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--z3', type=Path, required=True)
    parser.add_argument('--scope', choices=['roots', 'all'], default='roots')
    parser.add_argument('--repeats', type=int, default=3)
    parser.add_argument('--cpu', type=int, required=True)
    parser.add_argument('--timeout', type=float, default=600)
    parser.add_argument('--source', action='append', default=[],
                        help='Restrict the selected inventory to these paths')
    args = parser.parse_args()
    if args.repeats < 1 or args.timeout <= 0:
        parser.error('Positive repetitions and timeout required')
    os.sched_setaffinity(0, {args.cpu})
    compiler = args.prefix.resolve() / 'bin/ocamlopt.opt'
    if not compiler.is_file():
        parser.error('Run make install before benchmarking')
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    corpus = args.corpus.resolve()
    inventory = json.loads((corpus / 'inventory.json').read_text())
    coverage = json.loads((corpus / 'coverage.json').read_text())
    roots = {name.lower() for names in inventory['census_roots'].values()
             for name in names}
    selected = {}
    missing = []
    for source, record in inventory['sources'].items():
        if args.source and source not in args.source:
            continue
        if args.scope == 'roots' and record['module'].lower() not in roots:
            continue
        captured = coverage.get(source, {})
        if 'command' not in captured:
            missing.append(source)
        else:
            selected[source] = captured
    if not selected:
        parser.error('No captured source commands selected')
    if set(args.source) - set(selected):
        parser.error('Requested source has no command in the selected scope')
    environment = {k: v for k, v in os.environ.items()
                   if not k.startswith(('VOX_SMT_', 'VOX_VERIFY_'))}
    environment['VOX_VERIFY_CACHE'] = ''
    environment.pop('OCAMLPARAM', None)
    configuration = {
        'compiler': str(compiler), 'compiler_sha256': digest(compiler),
        'harness_sha256': digest(__file__),
        'z3': str(args.z3.resolve()), 'z3_sha256': digest(args.z3),
        'inventory_sha256': digest(corpus / 'inventory.json'),
        'coverage_sha256': digest(corpus / 'coverage.json'),
        'scope': args.scope, 'cpu': args.cpu, 'repeats': args.repeats,
        'cpu_configuration': cpu_configuration(args.cpu),
        'runtime_parameters': {
            name: environment.get(name)
            for name in ('OCAMLRUNPARAM', 'CAMLRUNPARAM')},
        'timeout': args.timeout, 'selected': list(selected),
        'missing_source_commands': missing,
        'timed': 'Complete native compiler process, including verification',
        'excluded': 'Copying existing dependency interfaces and log analysis',
        'resource_limits': 'Disabled for both backends; timeouts preserved',
        'cache': 'Verification caches disabled for both backends',
        'compilation': 'Compile each source with frozen captured dependencies; '
                       'restore generated files after every timed invocation',
        'memory': 'Linux wait4 peak RSS in bytes, including waited descendants',
        'correctness': 'Expected exit code and identical backend diagnostics; '
                       'differences require review',
        'performance': 'Sources with solver calls require strict speedup. '
                       'Zero-call sources may instead establish equivalence.',
        'parity': {
            'eligibility': 'Zero calls in successful untimed probes of both '
                           'current backends; historical capture counts '
                           'are reported separately',
            'test': 'Two one-sided exact paired sign tests of median time '
                    'ratio, with null ratio <= 0.99 or >= 1.01',
            'family': 'Holm correction across every verified zero-call source '
                      'in the selected inventory',
            'alpha': PARITY_ALPHA, 'minimum_pairs': PARITY_REPEATS,
            'relative_margin': PARITY_MARGIN,
            'sampling': 'Fixed repetitions, alternating backend order; '
                        'no resampling based on observed performance',
        },
    }
    (output / 'configuration.json').write_text(
        json.dumps(configuration, indent=2) + '\n')
    shutil.copyfile(__file__, output / 'smt_compile_bench.py')
    builds = {}
    for captured in selected.values():
        original = Path(captured['cwd'])
        for backend in ('ocaml', 'z3'):
            if (original, backend) not in builds:
                destination = output / 'build' / backend / original.name
                shutil.copytree(original, destination)
                builds[original, backend] = destination
    dependency_inputs = {}
    for (original, backend), directory in builds.items():
        hashes = dependency_hashes(directory)
        key = str(original)
        if key in dependency_inputs and dependency_inputs[key] != hashes:
            raise ValueError('Backend dependencies differ: ' + key)
        dependency_inputs[key] = hashes
    (output / 'dependency-inputs.json').write_text(
        json.dumps(dependency_inputs, indent=2) + '\n')
    samples = collections.defaultdict(lambda: collections.defaultdict(list))
    zero_calls = {}
    with (output / 'samples.jsonl').open('x') as records:
        for index, (source, captured) in enumerate(selected.items()):
            original = list(captured['command'])
            generated = {}
            for backend in ('ocaml', 'z3'):
                source_path = builds[Path(captured['cwd']), backend] / original[-1]
                if digest(source_path) != captured['source_sha256']:
                    raise ValueError('Captured source changed: ' + source)
                generated[backend] = {
                    path: path.read_bytes() if path.exists() else None
                    for suffix in ('.cmi', '.cmx', '.o', '.cmt', '.cmti')
                    for path in [source_path.with_suffix(suffix)]}
            zero_calls[source] = True
            for repeat in range(-1, max(args.repeats, PARITY_REPEATS)):
                if repeat >= args.repeats and not zero_calls[source]:
                    break
                backends = ['ocaml', 'z3']
                if (index + repeat) % 2:
                    backends.reverse()
                for backend in backends:
                    cwd = builds[Path(captured['cwd']), backend]
                    command = [str(compiler)]
                    skip = False
                    for offset, argument in enumerate(original[1:], start=1):
                        if skip:
                            skip = False
                        elif argument == '-stop-after':
                            skip = True
                        elif argument == '-smt-solver':
                            command += [argument, 'ocaml' if backend == 'ocaml'
                                        else str(args.z3.resolve())]
                            skip = True
                        elif argument in ('-smt-resource-limit',
                                          '-smt-resource-warning'):
                            skip = True
                        elif argument == str(Path(captured['cwd'])):
                            command.append(str(cwd))
                        else:
                            command.append(argument)
                    command[1:1] = ['-smt-resource-limit', '0',
                                    '-smt-resource-warning', '0']
                    name = f'{index:04}-{repeat}-{backend}'
                    run_environment = environment
                    if repeat == -1:
                        capture = output / (name + '.capture')
                        capture.mkdir()
                        run_environment = dict(environment,
                            VOX_SMT_API_CORPUS_DIR=str(capture))
                    measurement = compile_source(
                        command, cwd, run_environment, args.timeout, output,
                        name)
                    diagnostics = [
                        (output / (name + '.' + stream)).read_bytes().replace(
                            str(cwd).encode(), b'<build>')
                        for stream in ('stdout', 'stderr')]
                    row = dict(source=source, backend=backend, repeat=repeat,
                               **measurement,
                               diagnostics_sha256=[hashlib.sha256(d).hexdigest()
                                                   for d in diagnostics],
                               expected_returncode=captured['returncode'],
                               command=command, log=name)
                    for path, contents in generated[backend].items():
                        if contents is None:
                            path.unlink(missing_ok=True)
                        else:
                            path.write_bytes(contents)
                    if repeat == -1:
                        nonempty = [p.name for p in capture.glob('*.vapi')
                                    if p.stat().st_size]
                        valid = (not measurement['timed_out'] and
                                 measurement['returncode'] ==
                                 captured['returncode'])
                        zero_calls[source] &= valid and not nonempty
                        row['nonempty_captures'] = nonempty
                        row['zero_calls_verified'] = valid and not nonempty
                        (output / (name + '.probe.json')).write_text(
                            json.dumps(row, indent=2) + '\n')
                        continue
                    samples[source][backend].append(row)
                    records.write(json.dumps(row) + '\n')
                    records.flush()
            medians = {backend: statistics.median(r['seconds'] for r in rows)
                       for backend, rows in samples[source].items()}
            print(json.dumps(dict(source=source, seconds=medians)), flush=True)
    results = []
    for source, backends in samples.items():
        medians = {backend: statistics.median(r['seconds'] for r in rows)
                   for backend, rows in backends.items()}
        same_diagnostics = len({tuple(row['diagnostics_sha256'])
                                for rows in backends.values()
                                for row in rows}) == 1
        correct = same_diagnostics and all(
                      not row['timed_out'] and
                      row['returncode'] == row['expected_returncode']
                      for rows in backends.values() for row in rows)
        performance = source_performance(backends, zero_calls[source])
        performance['performance_pass'] &= correct
        results.append(dict(source=source, seconds=medians, correct=correct,
                            identical_diagnostics=same_diagnostics,
                            captured_solver_calls=selected[source].get(
                                'query_count'),
                            zero_calls_verified=zero_calls[source],
                            **performance,
                            demos=inventory['sources'][source]['demos']))
    adjust_parity_family(results)
    demos = {}
    for demo in inventory['demos']:
        rows = [r for r in results if demo in r['demos']]
        totals = {b: sum(r['seconds'][b] for r in rows) for b in ['ocaml', 'z3']}
        demos[demo] = dict(sources=len(rows), seconds=totals,
                           correct=all(r['correct'] for r in rows),
                           faster=bool(rows) and totals['ocaml'] < totals['z3'])
    summary = dict(sources=results, demos=demos,
                   all_correct=not missing and all(r['correct'] for r in results),
                   all_sources_faster=not missing and all(
                       r['faster'] for r in results),
                   all_sources_pass=not missing and all(
                       r['performance_pass'] for r in results),
                   failing_sources=[r['source'] for r in results
                                    if not r['performance_pass']],
                   slower_sources=[r['source'] for r in results
                                   if not r['faster']],
                   all_demos_faster=not missing and all(
                       r['correct'] and r['faster'] for r in demos.values()))
    summary['all_pass'] = (summary['all_correct']
                           and summary['all_sources_pass']
                           and summary['all_demos_faster'])
    (output / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
    print(json.dumps(demos, indent=2), flush=True)
    if (digest(compiler) != configuration['compiler_sha256'] or
            digest(args.z3) != configuration['z3_sha256']):
        raise ValueError('Compiler or Z3 changed during measurement')
    for (original, _), directory in builds.items():
        if dependency_hashes(directory) != dependency_inputs[str(original)]:
            raise ValueError('Dependency inputs changed: ' + str(directory))
    if not summary['all_correct']:
        return 1
    return 0 if summary['all_pass'] else 2


if __name__ == '__main__':
    raise SystemExit(main())
