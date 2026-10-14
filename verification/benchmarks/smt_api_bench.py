#!/usr/bin/env python3
import argparse
import collections
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import sys

from smt_reference import CapturedResults, ReferenceVerifier, eligible
from smt_suites import (active_declarations, assumption_script, expressions,
                        model_script, run_script)


def digest(path):
    result = hashlib.sha256()
    with Path(path).open('rb') as source:
        for block in iter(lambda: source.read(1024 * 1024), b''):
            result.update(block)
    return result.hexdigest()


def run_driver(command):
    if not hasattr(os, 'wait4'):
        subprocess.run(command, check=True)
        return {}
    process = subprocess.Popen(command)
    try:
        _, status, usage = os.wait4(process.pid, 0)
        process.returncode = os.waitstatus_to_exitcode(status)
        if process.returncode:
            raise subprocess.CalledProcessError(process.returncode, command)
        scale = 1 if sys.platform == 'darwin' else 1024
        return {'peak_rss_bytes': int(usage.ru_maxrss) * scale}
    finally:
        if process.returncode is None:
            process.kill()
            process.wait()


def followups_complete(sample):
    for kind in ('model', 'core'):
        requested = sample.get(kind + '_requested')
        available = sample.get(kind + '_available')
        if not isinstance(requested, bool) or not isinstance(available, bool):
            return False
        if requested and not available:
            return False
    return True


def followups_valid(sample):
    if not followups_complete(sample):
        return False
    if ((sample['model_requested'] and sample['result'] != 'sat')
            or (sample['core_requested'] and sample['result'] != 'unsat')):
        return False
    if not (sample['model_requested'] or sample['core_requested']):
        return True
    return sample.get('certificate_validation', {}).get('result') == 'valid'


def certificate_script(script, certificate):
    if not isinstance(certificate, dict):
        raise ValueError('missing returned certificate')
    prefix = [part for _, _, part in expressions(script)]
    kind = certificate.get('kind')
    if kind == 'assumptions':
        indices = certificate.get('indices')
        if (not isinstance(indices, list)
                or any(type(i) is not int or i < 0 for i in indices)):
            raise ValueError('invalid assumption indices')
        answer = b'(' + b' '.join(f'v{i}'.encode() for i in indices) + b')'
        return assumption_script(prefix, answer), 'unsat'
    if kind != 'model':
        raise ValueError('invalid certificate kind')
    scalar_sorts = {b'Bool', b'Int', (b'_', b'BitVec', b'63')}
    declarations = {int(name[1:]): sort
                    for name, arguments, sort in active_declarations(prefix)
                    if re.fullmatch(rb'v[0-9]+', name) and not arguments
                    and sort in scalar_sorts}
    bindings = certificate.get('bindings')
    if not isinstance(bindings, list):
        raise ValueError('missing model bindings')
    indices, requested, values = [], [], []
    for binding in bindings:
        if (not isinstance(binding, list) or len(binding) != 3
                or type(binding[0]) is not int):
            raise ValueError('invalid model binding')
        index, kind, value = binding
        sort = declarations.get(index)
        if not isinstance(value, str):
            raise ValueError('invalid model value')
        if kind == 'bool' and sort == b'Bool' and value in ('true', 'false'):
            encoded = value.encode()
        elif (kind in ('int', 'int63') and sort in
              (b'Int', (b'_', b'BitVec', b'63'))
              and re.fullmatch(r'0|-?[1-9][0-9]*', value)):
            if kind == 'int63':
                if len(value) > 20 or not -(1 << 62) <= int(value) < 1 << 62:
                    raise ValueError('machine integer outside signed range')
            elif sort != b'Int':
                raise ValueError('mathematical integer in bitvector model')
            if sort == b'Int':
                encoded = (f'(- {value[1:]})' if value.startswith('-')
                           else value).encode()
            else:
                encoded = f'(_ bv{int(value) % (1 << 63)} 63)'.encode()
        else:
            raise ValueError('model binding has the wrong sort or value')
        indices.append(index)
        term = f'v{index}'.encode()
        requested.append(term)
        values.append(b'(' + term + b' ' + encoded + b')')
    if indices != sorted(declarations):
        raise ValueError('model does not cover the scalar API symbols')
    answer = b'(' + b' '.join(values) + b')'
    return model_script(prefix, answer, requested), 'sat'


def export_queries(driver, capture, resource_policy, configuration):
    expected = configuration['captures'][capture.name]
    if digest(capture) != expected:
        raise ValueError('Typed capture changed before certificate export')
    if digest(driver) != configuration['driver_sha256']:
        raise ValueError('API driver changed before certificate export')
    process = subprocess.Popen(
        [str(driver.resolve()), '--export-queries', str(capture.resolve()),
         resource_policy], stdout=subprocess.PIPE, text=True)
    try:
        for line in process.stdout:
            yield json.loads(line)
        if process.wait() != 0:
            raise ValueError('Certificate query export failed')
    finally:
        if process.poll() is None:
            process.kill()
        process.wait()
        process.stdout.close()
    if (digest(capture) != expected
            or digest(driver) != configuration['driver_sha256']):
        raise ValueError('Certificate input or exporter changed during export')


def validate_followups(rows, args, configuration):
    sessions = collections.defaultdict(lambda: collections.defaultdict(list))
    for row in rows:
        for samples in row['runs'].values():
            for sample in samples:
                sample.pop('certificate_validation', None)
                if (sample.get('model_requested')
                        or sample.get('core_requested')):
                    sessions[row['session']][row['ordinal']].append(sample)
    directory = args.output / 'certificate-evidence'
    directory.mkdir()
    cache = {}
    for session, operations in sessions.items():
        seen = set()
        for query in export_queries(args.driver, args.corpus / session,
                                    args.resource_policy, configuration):
            ordinal = query['ordinal']
            if ordinal in seen:
                raise ValueError('Duplicate exported query ordinal')
            seen.add(ordinal)
            if ordinal not in operations:
                continue
            script = query['script'].encode()
            query_hash = hashlib.sha256(script).hexdigest()
            for sample in operations[ordinal]:
                certificate = sample.get('certificate')
                serialized = json.dumps(certificate, sort_keys=True).encode()
                certificate_hash = hashlib.sha256(serialized).hexdigest()
                binding = dict(query_sha256=query_hash,
                               certificate_sha256=certificate_hash)
                wanted = ('model' if sample['model_requested']
                          else 'assumptions')
                key = (query_hash, certificate_hash, wanted)
                if key not in cache:
                    try:
                        if (not isinstance(certificate, dict)
                                or certificate.get('kind') != wanted):
                            raise ValueError('missing or wrong certificate')
                        proof, expected = certificate_script(
                            script, certificate)
                        proof_hash = hashlib.sha256(proof).hexdigest()
                        destination = directory / (proof_hash + '.smt2')
                        destination.write_bytes(proof)
                        oracle = run_script(str(args.z3.resolve()),
                                            ['-in', '-smt2'], proof,
                                            args.evidence_timeout, 0, 64)
                        statuses = oracle.get('statuses')
                        conclusive = (statuses in (['sat'], ['unsat'])
                                      and not oracle.get('errors')
                                      and oracle.get('returncode') == 0)
                        result = (('valid' if statuses == [expected]
                                   else 'invalid') if conclusive
                                  else 'unvalidated')
                        evidence = dict(result=result, expected=expected,
                                        script_sha256=proof_hash, oracle=oracle)
                        destination.with_suffix('.json').write_text(
                            json.dumps(evidence, indent=2) + '\n')
                        cache[key] = dict(result=result,
                                         evidence=str(destination.with_suffix(
                                             '.json').resolve()))
                    except (ValueError, IndexError, RecursionError) as error:
                        cache[key] = dict(result='invalid', reason=str(error))
                sample['certificate_validation'] = dict(cache[key], **binding)
        if set(operations) - seen:
            raise ValueError('Missing exported certificate queries')


def summarize_runs(runs, resource_policy='unlimited', reference_evidence=None,
                   query_sha256=None, observed_results=()):
    row = {'runs': runs}
    row['median_seconds'] = {
        solver: statistics.median(r['seconds'] for r in observations)
        for solver, observations in row['runs'].items()}
    answers = {solver: {r['result'] for r in observations}
               for solver, observations in row['runs'].items()}
    row['followups_complete'] = all(
        followups_complete(sample) for samples in row['runs'].values()
        for sample in samples)
    row['followups_valid'] = all(
        followups_valid(sample) for samples in row['runs'].values()
        for sample in samples)
    row['correct'] = (row['followups_valid'] and len(answers['z3']) == 1
                      and answers['z3'] <= {'sat', 'unsat'}
                      and answers['ocaml'] == answers['z3']
                      and not any(r['stderr'] for observations
                                  in row['runs'].values()
                                  for r in observations))
    evidence = reference_evidence or {}
    verified = (evidence.get('verified') is True
                and query_sha256 is not None
                and evidence.get('query_sha256') == query_sha256
                and eligible(runs, observed_results))
    row['independently_verified_result'] = 'unsat' if verified else None
    row['correct'] = (row['correct']
                      or verified and row['followups_valid'])
    definitive = set(observed_results) & {'sat', 'unsat'}
    if len(definitive) > 1:
        row['correct'] = False
    timing = row['median_seconds']
    row['performance_pass'] = (timing['ocaml'] <= .01
                              or timing['ocaml'] <= 2 * timing['z3'])
    if resource_policy == 'preserve':
        row['correct'] = None
        row['performance_pass'] = None
    row['pass'] = (row['correct'] and row['performance_pass']
                   if resource_policy == 'unlimited' else None)
    return row


def export_query(driver, capture, ordinal, destination, configuration):
    expected = configuration['captures'][capture.name]
    if digest(capture) != expected:
        raise ValueError('Typed capture changed before evidence export')
    if digest(driver) != configuration['driver_sha256']:
        raise ValueError('API driver changed before evidence export')
    destination.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(driver.resolve()), '--export-query',
                    str(capture.resolve()), str(ordinal), 'unlimited',
                    str(destination.resolve())], check=True)
    if (digest(capture) != expected
            or digest(driver) != configuration['driver_sha256']):
        raise ValueError('Evidence input or exporter changed during export')
    script = destination.read_bytes()
    binding = dict(capture_sha256=expected, ordinal=ordinal,
                   driver_sha256=configuration['driver_sha256'],
                   query_sha256=hashlib.sha256(script).hexdigest(),
                   exported_script=str(destination.resolve()),
                   serialization='Vox_smt_response.session_input '
                                 '(Vox_smt.to_smtlib decoded_call)',
                   resource_policy='unlimited',
                   captured_response_coverage='typed captures contain inputs '
                                              'without original responses')
    destination.with_suffix('.json').write_text(
        json.dumps(binding, indent=2) + '\n')
    return script, binding


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--driver', type=Path, required=True)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--ocaml', required=True,
                        help='solver executable, or ocaml for the embedded API')
    parser.add_argument('--ocaml-reference', type=Path,
                        help='standalone OCaml solver for untimed checked-core '
                             'fallback; required for embedded fallback')
    parser.add_argument('--z3', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--compiler', type=Path, required=True)
    parser.add_argument('--cpu', type=int, required=True)
    parser.add_argument('--repeats', type=int, default=3)
    parser.add_argument('--modes', nargs='+',
                        default=['fresh', 'reset', 'incremental'],
                        choices=['fresh', 'reset', 'incremental', 'automatic'])
    parser.add_argument('--resource-policy', choices=['unlimited', 'preserve'],
                        default='unlimited')
    parser.add_argument('--check-reference-cores', action='store_true',
                        help='independently prove assertion subsets outside '
                             'timing after inconclusive Z3 calls')
    parser.add_argument('--prior-reference-evidence', type=Path,
                        action='append', default=[],
                        help='reconstruct and recheck a previous evidence.json')
    parser.add_argument('--evidence-timeout', type=float, default=60)
    parser.add_argument('--reference-corpus', type=Path,
                        help='canonical protocol corpus with queries.jsonl '
                             'and hash-checked scripts for captured statuses')
    args = parser.parse_args()
    embedded = args.ocaml == 'ocaml'
    if args.evidence_timeout <= 0:
        parser.error('--evidence-timeout must be positive')
    if args.check_reference_cores and args.reference_corpus is None:
        parser.error('checked core fallback requires --reference-corpus')
    if args.check_reference_cores and args.resource_policy != 'unlimited':
        parser.error('checked core fallback requires unlimited timing')
    if (embedded and args.check_reference_cores
            and args.ocaml_reference is None):
        parser.error('embedded checked core fallback requires '
                     '--ocaml-reference')
    if args.prior_reference_evidence and not args.check_reference_cores:
        parser.error('prior evidence requires --check-reference-cores')
    if args.repeats < 1:
        parser.error('--repeats must be positive')
    available = subprocess.check_output(
        [str(args.driver.resolve()), '--list-modes'], text=True).splitlines()
    unsupported = set(args.modes) - set(available)
    if unsupported:
        parser.error('Driver does not provide modes: '
                     + ', '.join(sorted(unsupported)))
    os.sched_setaffinity(0, {args.cpu})
    captures = {p.name: digest(p)
                for p in sorted(args.corpus.glob('*.vapi'))}
    paths = [args.corpus / name for name in captures
             if (args.corpus / name).stat().st_size]
    if not paths:
        parser.error('No typed API calls found')
    try:
        args.output.mkdir(parents=True, exist_ok=False)
    except FileExistsError:
        parser.error('Output directory already exists; choose a fresh path '
                     'to preserve measurements and prevent concurrent writes')
    samples = args.output / 'samples'
    samples.mkdir(exist_ok=True)
    solvers = {'ocaml': ('ocaml' if embedded else Path(args.ocaml).resolve()),
               'z3': args.z3.resolve()}
    identities = dict(solvers, ocaml=args.driver.resolve()
                      if embedded else solvers['ocaml'])
    reference = (args.ocaml_reference.resolve() if args.ocaml_reference
                 else None if embedded else solvers['ocaml'])
    reference_hash = (digest(reference)
                      if args.check_reference_cores else None)
    external_timing = ['AST validation and SMT-LIB serialization',
                       'session selection and transformation',
                       'solver ingestion and check',
                       'requested model, core, reason and resource followups',
                       'response parsing']
    external_fresh = 'new solver process per call; includes process disposal'
    external_warm = ('one API session per capture file, including first-call '
                     'startup and actual policy recycling; disposal after '
                     'the session is outside per-call time')
    metadata = {
        'gate': ('10 ms or <= 2x Z3 for each complete API operation'
                 if args.resource_policy == 'unlimited' else None),
        'timed': {
            'ocaml': (['typed AST validation and translation',
                       'engine construction, assertion ingestion and check',
                       'requested typed model and core retrieval']
                      if embedded else external_timing),
            'z3': external_timing},
        'fresh': {
            'ocaml': ('new embedded engine per call; no solver process'
                      if embedded else external_fresh),
            'z3': external_fresh},
        'warm': {
            'ocaml': ('new embedded engine per call in every API session mode; '
                      'no retained engine state or solver process'
                      if embedded else external_warm),
            'z3': external_warm},
        'excluded': ['capture decoding', 'VC generation', 'benchmark output'],
        'memory_observation': 'wait4 peak RSS per session, in bytes; maximum '
                              'across the driver and its waited descendants '
                              'on Linux, including capture decoding; stored '
                              'beside each sample file; no acceptance limit',
        'certificate_validation': {
            'timing': 'after all measurements',
            'oracle': 'Z3',
            'models': 'exact scalar values returned by the compiler API; '
                      'opaque and datatype values are omitted by that API',
            'cores': 'exact returned assumption subset',
            'coverage': 'every requested followup in every mode and repeat',
            'reuse': 'identical query bytes and returned certificate',
            'timeout': args.evidence_timeout},
        'resource_policy': args.resource_policy,
        'checked_core_fallback': args.check_reference_cores,
        'checked_core_solver': (dict(path=str(reference),
                                     sha256=reference_hash)
                                if args.check_reference_cores else None),
        'prior_reference_evidence': {
            str(path.resolve()): digest(path)
            for path in args.prior_reference_evidence},
        'reference_corpus': (str(args.reference_corpus.resolve())
                             if args.reference_corpus else None),
        'checked_core_modes': ['fresh', 'reset', 'incremental', 'automatic'],
        'checked_core_semantics': 'the independent query specified by each '
                                  'API call, including its assumptions',
        'checked_core_timeout': args.evidence_timeout,
        'checked_core_timing': 'after all recorded measurements',
        'timeout': 'captured per-session timeout preserved',
        'affinity': [args.cpu], 'repeats': args.repeats, 'modes': args.modes,
        'compiler_sha256': digest(args.compiler),
        'driver_sha256': digest(args.driver),
        'solver_sha256': {k: digest(v) for k, v in identities.items()},
        'solver_identity': {
            'ocaml': dict(kind='embedded-driver' if embedded else 'executable',
                          path=str(identities['ocaml'])),
            'z3': dict(kind='executable', path=str(identities['z3']))},
        'harness_sha256': {
            name: digest(Path(__file__).with_name(name))
            for name in ('smt_api_bench.py', 'smt_reference.py',
                         'smt_suites.py', 'smt_corpus.py')},
        'captures': captures,
        'empty_sessions': sorted(set(captures) - {p.name for p in paths}),
    }
    (args.output / 'configuration.json').write_text(
        json.dumps(metadata, indent=2))
    rows = collections.OrderedDict()
    for session_number, path in enumerate(paths):
        source_path = Path(str(path) + '.source')
        source = (source_path.read_text().splitlines()
                  if source_path.exists() else [])
        expected_ordinals = None
        for repeat in range(args.repeats):
            variants = [(solver, mode) for mode in args.modes
                        for solver in solvers]
            if (session_number + repeat) % 2:
                variants.reverse()
            for solver, mode in variants:
                output = samples / f'{path.stem}-{repeat}-{solver}-{mode}.jsonl'
                memory = run_driver([str(args.driver.resolve()),
                                     str(solvers[solver]),
                                     mode, str(repeat), args.resource_policy,
                                     str(path.resolve()), str(output.resolve())])
                output.with_suffix('.memory.json').write_text(
                    json.dumps(memory) + '\n')
                observations = [json.loads(line)
                                for line in output.read_text().splitlines()]
                ordinals = [r['ordinal'] for r in observations]
                if (not ordinals
                        or ordinals != list(range(1, len(ordinals) + 1))):
                    raise ValueError(f'Missing or reordered calls: {output}')
                if expected_ordinals is None:
                    expected_ordinals = ordinals
                elif expected_ordinals != ordinals:
                    raise ValueError(f'Call count changed: {output}')
                for observation in observations:
                    key = (path.name, observation['ordinal'], mode)
                    if key not in rows:
                        rows[key] = dict(session=path.name,
                                         ordinal=observation['ordinal'],
                                         source=source[0] if source else None,
                                         mode=mode,
                                         runs={'ocaml': [], 'z3': []})
                    rows[key]['runs'][solver].append(observation)
        print(json.dumps({'session': path.name,
                          'calls': len(expected_ordinals)}), flush=True)
    after = {p.name: digest(p) for p in args.corpus.glob('*.vapi')}
    if captures != after:
        raise ValueError('Typed corpus changed during qualification')
    if (digest(args.driver) != metadata['driver_sha256']
            or {k: digest(v) for k, v in identities.items()}
            != metadata['solver_sha256']):
        raise ValueError('Benchmark executable changed during qualification')
    validate_followups(rows.values(), args, metadata)
    if (digest(args.z3) != metadata['solver_sha256']['z3']
            or digest(args.driver) != metadata['driver_sha256']):
        raise ValueError('Certificate executable changed during validation')
    if args.check_reference_cores and digest(reference) != reference_hash:
        raise ValueError('Checked core solver changed before validation')
    reference_verifier = (ReferenceVerifier(
        reference, solvers['z3'], args.output / 'reference-evidence',
        args.evidence_timeout, args.prior_reference_evidence)
        if args.check_reference_cores else None)
    exports = {}
    captured_results = None
    observed = collections.defaultdict(set)
    for row in rows.values():
        for samples in row['runs'].values():
            observed[row['session'], row['ordinal']].update(
                sample['result'] for sample in samples)
    counts = collections.Counter()
    with (args.output / 'operations.jsonl').open('w') as output:
        for row in rows.values():
            observed_results = sorted(observed[row['session'], row['ordinal']])
            row['query_replay_results'] = observed_results
            if (reference_verifier is not None
                    and eligible(row['runs'], observed_results)):
                key = (row['session'], row['ordinal'])
                if key not in exports:
                    destination = (args.output / 'reference-inputs'
                                   / captures[row['session']]
                                   / (str(row['ordinal']) + '.smt2'))
                    exports[key] = export_query(
                        args.driver, args.corpus / row['session'],
                        row['ordinal'], destination, metadata)
                script, binding = exports[key]
                row['reference_input'] = binding
                if captured_results is None:
                    captured_results = CapturedResults(
                        args.reference_corpus)
                capture = captured_results.lookup(script)
                row['reference_capture'] = capture
                if capture is None:
                    row['reference_evidence_unavailable'] = (
                        'no matching canonical captured query')
                else:
                    observed_results = sorted(
                        set(observed_results)
                        | set(capture['captured_results']))
                    row['query_and_capture_results'] = observed_results
                    row['reference_evidence'] = reference_verifier.verify(
                        script, row['runs'], observed_results)
            row.update(summarize_runs(
                row['runs'], args.resource_policy,
                row.get('reference_evidence'),
                row.get('reference_input', {}).get('query_sha256'),
                observed_results))
            counts[row['mode'] + '_calls'] += 1
            counts[row['mode'] + '_failed'] += row['pass'] is False
            output.write(json.dumps(row) + '\n')
    if args.check_reference_cores and digest(reference) != reference_hash:
        raise ValueError('Checked core solver changed during validation')
    if (digest(args.z3) != metadata['solver_sha256']['z3']
            or digest(args.driver) != metadata['driver_sha256']):
        raise ValueError('Evidence executable changed during validation')
    summary = {'counts': dict(counts),
               'all_pass': (all(row['pass'] for row in rows.values())
                            if args.resource_policy == 'unlimited' else None),
               'followup_failures': sum(not row['followups_complete']
                                        for row in rows.values()),
               'certificate_failures': sum(not row['followups_valid']
                                          for row in rows.values()),
               'gate_evaluated': args.resource_policy == 'unlimited',
               'correctness_failures': sum(
                   row['correct'] is False for row in rows.values()),
               'gate_failures': sum(
                   row['performance_pass'] is False for row in rows.values())}
    (args.output / 'summary.json').write_text(
        json.dumps(summary, indent=2) + '\n')
    print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
