"""Independently check assertion subsets after an inconclusive reference run."""

from collections import defaultdict
import hashlib
import json
from pathlib import Path
import shutil
import threading

from smt_corpus import command_name, write_json
from smt_suites import children, complete_expressions, run_script


HELPER_SOURCES = {Path(function.__code__.co_filename)
                  for function in (run_script, command_name)}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def eligible(runs, captured=()):
    answers = {name: {sample['result'] for sample in samples}
               for name, samples in runs.items()}
    return (answers.get('ocaml') == {'unsat'}
            and bool(answers.get('z3'))
            and answers['z3'] <= {'unsat', 'unknown', 'timeout'}
            and bool(answers['z3'] & {'unknown', 'timeout'})
            and 'sat' not in captured
            and not any(sample.get('errors') or sample.get('stderr')
                        for samples in runs.values() for sample in samples))


def flat_commands(script):
    commands = [part for _, _, part in complete_expressions(script)]
    declarations = {'declare-sort', 'define-sort', 'declare-fun',
                    'declare-const', 'define-fun', 'declare-datatype',
                    'declare-datatypes'}
    checks = {'check-sat', 'check-sat-assuming', 'check-sat-using'}
    options = {b':print-success', b':produce-models', b':produce-unsat-cores',
               b':produce-unsat-assumptions', b':timeout', b':rlimit'}
    semantic = False
    checked = False
    for command in commands:
        parts = children(command)
        name = command_name(command)
        if checked:
            raise ValueError('commands after the satisfiability check')
        if name == 'reset' and not semantic and parts == [b'reset']:
            continue
        if name == 'set-option' and len(parts) == 3 and parts[1] in options:
            continue
        if name == 'set-logic' and not semantic and len(parts) == 2:
            continue
        if (name == 'set-info' and len(parts) == 3
                and parts[1] in {b':smt-lib-version', b':source', b':license',
                                 b':category', b':status'}
                and (parts[1] != b':status'
                     or parts[2] in {b'sat', b'unsat', b'unknown'})):
            continue
        if name not in declarations | checks | {'assert'}:
            raise ValueError('unsupported evidence command: ' + str(name))
        semantic = True
        if name == 'assert' and len(parts) != 2:
            raise ValueError('malformed assertion')
        if name in checks:
            if name == 'check-sat' and len(parts) != 1:
                raise ValueError('malformed satisfiability check')
            if name != 'check-sat' and len(parts) != 2:
                raise ValueError('malformed satisfiability check')
            checked = True
        pending = list(parts)
        while pending:
            term = pending.pop()
            if term.startswith(b'('):
                pending.extend(children(term))
            elif term == b':named':
                raise ValueError('existing named-expression dependencies')
            elif term.startswith(b'|') and b'\\' in term:
                raise ValueError('escaped quoted symbols')
    if not checked:
        raise ValueError('missing satisfiability check')
    return commands


def named_script(script):
    commands = flat_commands(script)
    prefix = b'vox_reference_' + digest(script).encode()[:16] + b'_'
    while prefix in script:
        prefix += b'x'
    labels = {}
    replay = []
    # A leading reset would discard a produce-unsat-cores option before it.
    last_reset = max((i for i, command in enumerate(commands)
                      if command_name(command) == 'reset'), default=-1)
    for index, command in enumerate(commands):
        if index == last_reset + 1:
            replay.append(b'(set-option :produce-unsat-cores true)')
        parts = children(command)
        if parts[:2] == [b'set-option', b':produce-unsat-cores']:
            replay.append(b'(set-option :produce-unsat-cores true)')
        elif parts[0] == b'assert':
            label = prefix + str(len(labels)).encode()
            labels[label] = command
            replay.append(b'(assert (! ' + parts[1] + b' :named '
                          + label + b'))')
        else:
            replay.append(command)
    replay.extend([b'(get-unsat-core)', b'(exit)'])
    return b'\n'.join(replay) + b'\n', commands, labels


def checked_labels(result, labels):
    if (result.get('returncode') != 0 or result.get('stderr')
            or result.get('errors') or result.get('timeout')
            or result.get('failure') or result.get('output_limit')):
        raise ValueError('core extraction did not complete cleanly')
    responses = [part for _, _, part in complete_expressions(
        result['stdout'].encode()) if part != b'success']
    if len(responses) != 2 or responses[0] != b'unsat':
        raise ValueError('expected one unsat response and one core')
    core = children(responses[1])
    core = [label[1:-1] if label.startswith(b'|') and label.endswith(b'|')
            else label for label in core]
    if len(core) != len(set(core)):
        raise ValueError('duplicate core label')
    if any(label not in labels for label in core):
        raise ValueError('unknown core label')
    return core


def subset_script(commands, labels, core):
    retained = {labels[label] for label in core}
    return b'\n'.join(command for command in commands
                       if command_name(command) != 'assert'
                       or command in retained) + b'\n(exit)\n'


def prior_core(evidence, script, named, labels):
    if evidence.get('query_sha256') != digest(script):
        raise ValueError('prior evidence query mismatch')
    if evidence.get('named_sha256') != digest(named):
        raise ValueError('prior evidence named query mismatch')
    assertions = {label.decode(): digest(command)
                  for label, command in labels.items()}
    if evidence.get('assertions') != assertions:
        raise ValueError('prior evidence assertion mismatch')
    core = evidence.get('core')
    if (not isinstance(core, list)
            or any(not isinstance(label, str) for label in core)):
        raise ValueError('invalid prior core')
    core = [label.encode() for label in core]
    if len(core) != len(set(core)) or any(label not in labels for label in core):
        raise ValueError('invalid prior core labels')
    return core


def proved_answer(result, answer):
    if (result.get('returncode') != 0 or result.get('stderr')
            or result.get('errors') or result.get('timeout')
            or result.get('failure') or result.get('output_limit')):
        return False
    responses = [part for _, _, part in complete_expressions(
        result['stdout'].encode()) if part != b'success']
    return responses == [answer]


def proved_unsat(result):
    return proved_answer(result, b'unsat')


def counting_abstraction(formulas):
    indicators = {}

    def term(source):
        if source.isdigit():
            return source
        parts = children(source)
        if (len(parts) == 4 and parts[0] == b'ite'
                and parts[2:] == [b'1', b'0']):
            if source not in indicators:
                indicators[source] = f'count_{len(indicators)}'.encode()
            return indicators[source]
        if len(parts) >= 3 and parts[0] == b'+':
            return b'(+ ' + b' '.join(term(p) for p in parts[1:]) + b')'
        raise ValueError('unsupported counting term')

    def formula(source):
        parts = children(source)
        if len(parts) == 3 and parts[0] in (b'=', b'<='):
            return (b'(' + parts[0] + b' ' + term(parts[1]) + b' '
                    + term(parts[2]) + b')')
        raise ValueError('unsupported counting bound')

    premises = [formula(source) for source in formulas]
    declarations = [b'(declare-const ' + name + b' Int)'
                    for name in indicators.values()]
    return declarations, premises


def proof_declarations(commands, protected, purpose):
    def check_name(name):
        if name.startswith(b'|') and name.endswith(b'|'):
            name = name[1:-1]
        if name in protected:
            raise ValueError(purpose + ' operator or sort is shadowed')

    def datatype(body):
        constructors = children(body)
        if constructors and constructors[0] == b'par':
            if len(constructors) != 3:
                raise ValueError('malformed datatype parameters')
            constructors = children(constructors[2])
        for constructor in constructors:
            fields = children(constructor)
            if not fields:
                raise ValueError('missing datatype constructor')
            check_name(fields[0])
            for field in fields[1:]:
                selector = children(field)
                if len(selector) != 2:
                    raise ValueError('malformed datatype selector')
                check_name(selector[0])

    declarations = []
    asserted = False
    for command in commands:
        parts = children(command)
        name = command_name(command)
        if name == 'assert':
            asserted = True
        if asserted and name in {
                'declare-sort', 'define-sort', 'declare-fun', 'declare-const',
                'define-fun', 'declare-datatype', 'declare-datatypes'}:
            raise ValueError(purpose + ' requires initial declarations')
        if name in ('declare-sort', 'define-sort', 'declare-fun',
                    'declare-const', 'define-fun', 'declare-datatype'):
            if len(parts) < 2:
                raise ValueError('missing declaration name')
            check_name(parts[1])
            if name == 'declare-datatype':
                if len(parts) != 3:
                    raise ValueError('malformed datatype declaration')
                datatype(parts[2])
            declarations.append(command)
        elif name == 'declare-datatypes':
            if len(parts) != 3:
                raise ValueError('malformed datatype declarations')
            for sort in children(parts[1]):
                sort = children(sort)
                if len(sort) != 2:
                    raise ValueError('malformed datatype sort')
                check_name(sort[0])
            for body in children(parts[2]):
                datatype(body)
            declarations.append(command)
        elif name in ('check-sat-assuming', 'check-sat-using'):
            raise ValueError(purpose + ' requires plain check-sat')
    return declarations


def counting_declarations(commands):
    protected = {b'not', b'ite', b'+', b'=', b'<=', b'Int', b'Bool',
                 b'true', b'false'}
    return proof_declarations(commands, protected, 'counting proof')


def unlimited_query_identity(script):
    commands = []
    for _, _, command in complete_expressions(script):
        parts = children(command)
        if parts[:2] == [b'set-option', b':rlimit'] and len(parts) == 3:
            command = b'(set-option :rlimit 0)'
        commands.append(command)
    return digest(b'\n'.join(commands))


class CapturedResults:
    def __init__(self, corpus):
        index = (Path(corpus) / 'queries.jsonl').read_bytes()
        self.index_sha256 = digest(index)
        self.path = str(Path(corpus).resolve())
        self.queries = defaultdict(lambda: dict(results=set(), scripts=set()))
        seen = {}
        for line in index.splitlines():
            if not line.strip():
                continue
            row = json.loads(line)
            key = row.get('replay_script_sha256', '')
            if len(key) != 64 or any(c not in '0123456789abcdef' for c in key):
                raise ValueError('capture index lacks canonical script hashes')
            if key not in seen:
                path = Path(corpus) / 'scripts' / (key + '.smt2')
                script = path.read_bytes()
                if digest(script) != key:
                    raise ValueError('captured replay script hash mismatch')
                seen[key] = unlimited_query_identity(script)
            results = row.get('captured_results', [])
            if not isinstance(results, list):
                raise ValueError('invalid captured result list')
            results = [row.get('result'), *results]
            if any(result not in ('sat', 'unsat', 'unknown', None)
                   for result in results):
                raise ValueError('invalid captured result')
            entry = self.queries[seen[key]]
            entry['results'].update(result for result in results
                                    if result is not None)
            entry['scripts'].add(key)
        if self.index_sha256 != digest(
                (Path(corpus) / 'queries.jsonl').read_bytes()):
            raise ValueError('capture index changed while reading')

    def lookup(self, script):
        key = unlimited_query_identity(script)
        entry = self.queries.get(key)
        if entry is None:
            return None
        return dict(protocol_corpus=self.path,
                    protocol_index_sha256=self.index_sha256,
                    unlimited_query_identity=key,
                    matching_replay_sha256=sorted(entry['scripts']),
                    captured_results=sorted(entry['results']))


class ReferenceVerifier:
    def __init__(self, solver, reference, output, timeout, prior_evidence=()):
        self.executables = {
            name: Path(shutil.which(str(path)) or path).resolve()
            for name, path in (('ocaml', solver), ('z3', reference))}
        self.identities = {name: digest(path.read_bytes())
                           for name, path in self.executables.items()}
        if len(set(self.identities.values())) != 2:
            raise ValueError('core evidence requires independent executables')
        self.output = Path(output)
        self.timeout = timeout
        self.cache = {}
        self.lock = threading.Lock()
        self.prior = defaultdict(list)
        for path in prior_evidence:
            path = Path(path).resolve()
            data = path.read_bytes()
            evidence = json.loads(data)
            key = evidence.get('query_sha256')
            if (not isinstance(key, str) or len(key) != 64
                    or any(c not in '0123456789abcdef' for c in key)):
                raise ValueError('prior evidence lacks a query hash')
            self.prior[key].append((path, digest(data), evidence))

    def prove_subset(self, commands, labels, core, directory, evidence):
        subset = subset_script(commands, labels, core)
        (directory / 'subset.smt2').write_bytes(subset)
        evidence.update(subset_sha256=digest(subset),
                        original_assertions=len(labels),
                        retained_assertions=len(core),
                        core=[label.decode() for label in core],
                        assertions={label.decode(): digest(command)
                                    for label, command in labels.items()})
        verification = run_script(str(self.executables['z3']),
                                  ['-in', '-smt2'], subset, self.timeout)
        write_json(directory / 'verification.json', verification)
        evidence['verified'] = proved_unsat(verification)
        if not evidence['verified']:
            evidence['reason'] = 'reference did not prove the subset unsat'
        return evidence

    def prove_counting(self, commands, labels, prior, directory, evidence):
        steps = prior.get('steps')
        if not isinstance(steps, list) or not steps:
            raise ValueError('missing counting proof steps')
        declarations = counting_declarations(commands)
        formulas = {label.decode(): children(command)[1]
                    for label, command in labels.items()}
        proved = set()
        checks = []
        evidence.update(method='independently-checked-counting-lemmas',
                        steps=steps, counting_checks=checks, verified=False,
                        counting_abstraction='each identical integer indicator '
                        'ite(condition, 1, 0) becomes one unrestricted integer; '
                        'the final contradiction uses only verified lemmas')
        for index, step in enumerate(steps):
            if not isinstance(step, dict):
                raise ValueError('invalid counting proof step')
            name = step.get('id')
            dependencies = step.get('dependencies')
            formula = step.get('formula')
            if (not isinstance(name, str) or not name or name in formulas
                    or not isinstance(dependencies, list)
                    or any(not isinstance(d, str) or d not in formulas
                           for d in dependencies)
                    or not isinstance(formula, str)):
                raise ValueError('invalid counting proof dependency or formula')
            formula = formula.encode()
            if [p for _, _, p in complete_expressions(formula)] != [formula]:
                raise ValueError('counting proof requires one formula')
            pending = [formula]
            while pending:
                term = pending.pop()
                if term.startswith(b'('):
                    pending.extend(children(term))
                elif term == b':named':
                    raise ValueError('counting lemma declares a named term')
                elif term.startswith(b'|') and b'\\' in term:
                    raise ValueError('escaped quoted symbols')
            premises = [formulas[d] for d in dependencies]
            proof_declarations = declarations
            if formula == b'false':
                if index != len(steps) - 1 or not set(dependencies) <= proved:
                    raise ValueError('counting conclusion needs verified lemmas')
                proof_declarations, premises = counting_abstraction(premises)
            script = b'\n'.join([
                b'(set-logic ALL)', *proof_declarations,
                *[b'(assert ' + p + b')' for p in premises],
                b'(assert (not ' + formula + b'))', b'(check-sat)',
                b'(exit)']) + b'\n'
            target = directory / f'step-{index}'
            target.with_suffix('.smt2').write_bytes(script)
            result = run_script(str(self.executables['z3']),
                                ['-in', '-smt2'], script, self.timeout)
            write_json(target.with_suffix('.json'), result)
            passed = proved_unsat(result)
            checks.append(dict(step=name, script_sha256=digest(script),
                               verified=passed))
            if not passed:
                evidence['reason'] = 'reference did not prove counting lemma'
                return evidence
            formulas[name] = formula
            proved.add(name)
        if steps[-1]['formula'] != 'false':
            raise ValueError('counting proof lacks final contradiction')
        evidence['verified'] = True
        return evidence

    def prove_case_split(self, script, prior, directory, evidence):
        if prior.get('query_sha256') != digest(script):
            raise ValueError('case split query mismatch')
        formula = prior.get('formula')
        if not isinstance(formula, str):
            raise ValueError('case split requires a formula')
        formula = formula.encode()
        if [p for _, _, p in complete_expressions(formula)] != [formula]:
            raise ValueError('case split requires one formula')
        pending = [formula]
        while pending:
            term = pending.pop()
            if term.startswith(b'('):
                pending.extend(children(term))
            elif term == b':named':
                raise ValueError('case split declares a named term')
            elif term.startswith(b'|') and b'\\' in term:
                raise ValueError('escaped quoted symbols')
        commands = flat_commands(script)
        proof_declarations(commands, {b'not', b'Bool', b'true', b'false'},
                           'case split')
        prefix = [command for command in commands
                  if command_name(command) != 'check-sat']
        checks = []
        evidence.update(method='independently-checked-case-split',
                        formula=prior['formula'],
                        formula_sha256=digest(formula),
                        query_sha256=digest(script), split_checks=checks,
                        assertion_sha256=[
                            digest(command) for command in prefix
                            if command_name(command) == 'assert'],
                        verified=False)
        for index, branch in enumerate((formula, b'(not ' + formula + b')')):
            query = b'\n'.join([*prefix, b'(assert ' + branch + b')',
                                b'(check-sat)', b'(exit)']) + b'\n'
            target = directory / f'branch-{index}'
            target.with_suffix('.smt2').write_bytes(query)
            result = run_script(str(self.executables['z3']),
                                ['-in', '-smt2'], query, self.timeout)
            write_json(target.with_suffix('.json'), result)
            passed = proved_unsat(result)
            checks.append(dict(branch=index, script_sha256=digest(query),
                               verified=passed))
            if not passed:
                evidence['reason'] = 'reference did not prove both cases unsat'
                if proved_answer(result, b'sat'):
                    evidence['reference_contradiction'] = True
                return evidence
        evidence['verified'] = True
        return evidence

    def verify(self, script, runs, captured=()):
        with self.lock:
            return self._verify(script, runs, captured)

    def _verify(self, script, runs, captured):
        if not eligible(runs, captured):
            return None
        key = digest(script)
        if key in self.cache:
            return dict(self.cache[key])
        directory = self.output / key
        directory.mkdir(parents=True, exist_ok=True)
        (directory / 'original.smt2').write_bytes(script)
        evidence = dict(method='independently-checked-assertion-subset',
                        verified=False, query_sha256=key,
                        evidence_directory=str(directory.resolve()),
                        solver_sha256=self.identities,
                        solver_paths={name: str(path) for name, path
                                      in self.executables.items()},
                        verifier_sha256=digest(Path(__file__).read_bytes()),
                        helper_sha256={path.name: digest(path.read_bytes())
                                       for path in HELPER_SOURCES},
                        timeout_seconds=self.timeout,
                        outside_measurement=True)
        try:
            named, commands, labels = named_script(script)
            (directory / 'named.smt2').write_bytes(named)
            evidence['named_sha256'] = digest(named)
            attempts = []
            for index, (path, identity, prior) in enumerate(self.prior[key]):
                attempt = dict(path=str(path), sha256=identity, verified=False)
                attempts.append(attempt)
                try:
                    if (prior.get('method')
                            == 'independently-checked-case-split'):
                        target = directory / f'prior-{index}'
                        target.mkdir(exist_ok=True)
                        checked = self.prove_case_split(
                            script, prior, target, dict(evidence))
                        attempt.update(checked)
                        if (checked['verified']
                                or checked.get('reference_contradiction')):
                            evidence = checked
                            break
                        continue
                    if (prior.get('method')
                            == 'independently-checked-counting-lemmas'):
                        target = directory / f'prior-{index}'
                        target.mkdir(exist_ok=True)
                        checked = self.prove_counting(
                            commands, labels, prior, target, dict(evidence))
                        attempt.update(checked)
                        if checked['verified']:
                            evidence = checked
                            break
                        continue
                    core = prior_core(prior, script, named, labels)
                    subset = subset_script(commands, labels, core)
                    if digest(subset) != prior.get('subset_sha256'):
                        raise ValueError('prior evidence subset mismatch')
                    target = directory / f'prior-{index}'
                    target.mkdir(exist_ok=True)
                    checked = self.prove_subset(
                        commands, labels, core, target, dict(evidence))
                    attempt.update(checked)
                    if checked['verified']:
                        for name in ('subset.smt2', 'verification.json'):
                            shutil.copyfile(target / name, directory / name)
                        evidence = checked
                        break
                except ValueError as error:
                    attempt['reason'] = str(error)
            if attempts:
                evidence['prior_attempts'] = attempts
            if (not evidence['verified']
                    and not evidence.get('reference_contradiction')):
                extraction = run_script(str(self.executables['ocaml']),
                                        ['-in', '-smt2'], named, self.timeout)
                write_json(directory / 'extraction.json', extraction)
                core = checked_labels(extraction, labels)
                evidence = self.prove_subset(
                    commands, labels, core, directory, evidence)
        except ValueError as error:
            evidence['reason'] = str(error)
        if self.identities != {name: digest(path.read_bytes())
                               for name, path in self.executables.items()}:
            raise ValueError('evidence solver executable changed')
        write_json(directory / 'evidence.json', evidence)
        self.cache[key] = evidence
        return dict(evidence)
