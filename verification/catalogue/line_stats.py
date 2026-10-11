"""Physical-line counts for each demo: Spec, Implementation, Model, Proof
and Not reached.

A line is counted if it has code; comments, blank lines and an expect
test's expected output are not. Every code line of the demo's `.ml` files
is counted in exactly one column.

- Spec: the code lines the page quotes under its Interface heading.
- Implementation: code that runs. A declaration is reachable at run time
  if a root module exports it and it is not erased, if it is a root
  module's initialization, or if run-time code of a reachable declaration
  refers to it. Its lines count, less the proof regions below. A line with
  both run-time code and an annotation counts once, here.
- Model: ordinary declarations that never run and that the specification
  is stated in terms of: what the root modules' interfaces and the quoted
  lines refer to, and what those declarations refer to in turn, such as a
  declarative typing relation or a WebAssembly semantics. Lemmas are not
  followed.
- Proof: the proof regions, which erasure removes or only the verifier
  reads: `ghost_` expressions (with the `let ... in` that binds one), ghost
  declarations, all-ghost records, ghost fields, parameters of type
  `Ghost.t` or refined `unit` (with their arrows), refinements of a value
  already computed (`let x : {x : t | p} = y in`), refinement predicates
  (not the payload type `t` of `{x : t | p}`), lemmas (total functions
  whose result is a refined `unit` or ghost; catalogue.json's
  runtime_units lists the few that run) and termination measures. Also
  ordinary declarations that are used only in proofs, such as invariants.
- Not reached: declarations that nothing reachable from the exported
  values uses, at run time or logically.

Reachability is computed from typed trees. The demo's module closure (every
module the root modules mention, transitively) is type-checked with
verification skipped (`-smt-assume-verified -bin-annot -stop-after typing`),
and `source_inventory.ml -typed` reads each reference to a value, type or
exception with its resolved path and position. `source_inventory.ml` also
parses each file for its declarations and proof regions. A reference
inside a proof region or an erased declaration is logical; any other is
made at run time. Constructors, record fields and type annotations refer to
their type; paths are resolved through module aliases, includes and functor
applications. On a line, run-time code that is only punctuation or mode
annotations (`}) @ unique ->`) does not make it Implementation. Lines
outside every declaration (`open`, module aliases, `struct` and `end`) go
to the column with the most lines in their module.

`.mli` files are counted separately, as Interface files; the lines the page
quotes from them are Spec. Modules shared with other demos (by the census
in catalogue.json) are reported separately.
"""
from bisect import bisect_right
from concurrent.futures import ThreadPoolExecutor
from itertools import accumulate
from pathlib import Path
import hashlib, json, re, subprocess

HERE = Path(__file__).resolve().parent
SEARCHED = ('verification/library', 'testsuite/tests/vox')
BUILTIN = ('Ghost', 'Bigint', 'Stdlib', 'Iarray')
COLUMNS = ('impl', 'model', 'proof', 'unused')
EXPECT = re.compile(rb'\[%%expect\{\|.*?\|\}\]', re.S)  # an expect test's expected output


def code_mask(raw):
    """A byte mask of visible code, skipping nested OCaml comments."""
    visible = bytearray(c not in b' \t\r\n\v\f' for c in raw)
    i = depth = 0
    while i < len(raw):
        if raw.startswith(b'(*', i):
            depth += 1
            visible[i:i + 2] = b'\0\0'
            i += 2
            continue
        if depth and raw.startswith(b'*)', i):
            depth -= 1
            visible[i:i + 2] = b'\0\0'
            i += 2
            continue
        quoted = re.match(rb'\{([a-z_]*)\|', raw[i:])
        char = re.match(rb"'(?:[^'\\\r\n]|\\(?:[0-9]{3}|x[0-9a-fA-F]{2}|o[0-7]{3}|.))'", raw[i:])
        if quoted:
            closing = b'|' + quoted[1] + b'}'
            end = raw.find(closing, i + len(quoted[0]))
            assert end >= 0, 'Unclosed quoted string'
            end += len(closing)
        elif char:
            end = i + len(char[0])
        elif raw[i:i + 1] == b'"':
            end = i + 1
            while end < len(raw):
                if raw[end:end + 1] == b'\\':
                    end += 2
                elif raw[end:end + 1] == b'"':
                    end += 1
                    break
                else:
                    end += 1
        else:
            end = i + 1
        if depth:
            visible[i:end] = b'\0' * (end - i)
        i = end
    assert depth == 0, 'Unclosed source comment'
    return visible


def mask_lines(raw, mask):
    lines = set()
    offset = 0
    for n, line in enumerate(raw.splitlines(keepends=True), 1):
        if any(mask[offset:offset + len(line)]):
            lines.add(n)
        offset += len(line)
    return lines


def code_lines(source):
    raw = source.encode()
    return mask_lines(raw, code_mask(raw))


def owned(scope, path):
    name = Path(path).stem.lower()
    if 'shared_modules' in scope:
        return name not in scope['shared_modules']
    return any(name.startswith(prefix) for prefix in scope['owned'])


def module(path):
    stem = Path(path).stem
    return stem[:1].upper() + stem[1:]


class Parser:
    """source_inventory.ml, built once per compiler, with parse results
    cached by content hash and typed trees by closure."""

    def __init__(self, prefix, cache):
        self.cache = cache
        cache.mkdir(parents=True, exist_ok=True)
        self.prefix = Path(prefix).resolve()
        compiler = self.prefix / 'bin' / 'ocamlopt.opt'
        tool = (HERE / 'source_inventory.ml').read_bytes()
        key = hashlib.sha256(tool + compiler.read_bytes()).hexdigest()[:16]
        self.executable = cache / f'source_inventory-{key}'
        if not self.executable.exists():
            libs = self.prefix / 'lib' / 'ocaml' / 'compiler-libs'
            build = cache / 'parser-build'
            build.mkdir(exist_ok=True)
            (build / 'source_inventory.ml').write_bytes(tool)
            subprocess.run([str(compiler), '-I', str(libs), str(libs / 'ocamlcommon.cmxa'),
                            str(libs / 'ocamlfrontend.cmxa'), 'source_inventory.ml',
                            '-o', str(self.executable)], cwd=build, check=True)
        self.key = key

    def parse(self, files):
        """{sha256: syntax} for the given {sha256: (suffix, bytes)}."""
        results, missing = {}, []
        for sha, (suffix, raw) in files.items():
            cached = self.cache / f'{self.key}-{sha}{suffix}.json'
            if cached.exists():
                results[sha] = json.loads(cached.read_text())
            else:
                source = self.cache / f'{sha}{suffix}'
                source.write_bytes(raw)
                missing.append((sha, source, cached))
        if missing:
            out = subprocess.run([str(self.executable), *[str(s) for _, s, _ in missing]],
                                 check=True, capture_output=True, text=True).stdout
            by_file = {}
            for line in out.splitlines():
                data = json.loads(line)
                by_file[data.pop('file')] = data
            for sha, source, cached in missing:
                cached.write_text(json.dumps(by_file[str(source)]))
                results[sha] = by_file[str(source)]
        return results

    def typed(self, units, opens):
        """{path: typed} for a module closure [(unit, path, raw, syntax)]:
        the closure is type-checked with verification skipped, in a
        directory named by its contents, and its typed trees read. `opens`
        gives the units a file is compiled with `-open` of."""
        flags = {path: ['-extension', 'refinement_types']
                 + (['-vox-library'] if path.startswith('verification/library/') else [])
                 + [f for unit in opens.get(path, []) for f in ('-open', unit)]
                 for _, path, _, _ in units}
        digest = hashlib.sha256(json.dumps(sorted(
            [unit, path, hashlib.sha256(raw).hexdigest(), flags[path]] for unit, path, raw, _ in units)
            + [self.key]).encode()).hexdigest()[:20]
        build = self.cache / f'typed-{digest}'
        result = build / 'typed.json'
        if result.exists():
            return json.loads(result.read_text())
        build.mkdir(exist_ok=True)
        modules = {}
        for unit, path, raw, syntax in units:
            name = unit[:1].lower() + unit[1:] + Path(path).suffix
            # An expect test's expected output is blanked, keeping offsets.
            (build / name).write_bytes(EXPECT.sub(lambda m: re.sub(rb'[^\n]', b' ', m[0]), raw))
            entry = modules.setdefault(unit, {'files': [], 'deps': set()})
            entry['files'].append((name, path))
            entry['deps'] |= set(syntax['dependencies']) | set(opens.get(path, []))
        for entry in modules.values():
            entry['files'].sort(key=lambda f: f[0].endswith('.ml'))  # the .mli first
        compiler = self.prefix / 'bin' / 'ocamlc.opt'

        def check(unit):
            for name, path in modules[unit]['files']:
                done = subprocess.run([str(compiler), '-c', '-bin-annot', '-stop-after', 'typing',
                                       '-smt-assume-verified', '-w', '-a', *flags[path], '-I', '.', name],
                                      cwd=build, capture_output=True, text=True)
                assert done.returncode == 0, f'type-checking {path} failed:\n{done.stderr}'

        # In waves: a unit once every unit it mentions is done.
        remaining = {u: {d for d in e['deps'] if d in modules and d != u} for u, e in modules.items()}
        with ThreadPoolExecutor(max_workers=8) as pool:
            while remaining:
                ready = sorted(u for u, deps in remaining.items() if not deps)
                assert ready, ('dependency cycle', sorted(remaining))
                list(pool.map(check, ready))
                remaining = {u: deps - set(ready) for u, deps in remaining.items() if u not in ready}
        trees = {}
        for entry in modules.values():
            for name, path in entry['files']:
                suffix = '.cmti' if name.endswith('.mli') else '.cmt'
                trees[str(build / (Path(name).stem + suffix))] = path
        out = subprocess.run([str(self.executable), '-typed', *trees], check=True,
                             capture_output=True, text=True).stdout
        typed = {}
        for line in out.splitlines():
            data = json.loads(line)
            typed[trees[data.pop('file')]] = data
        result.write_text(json.dumps(typed))
        return typed


def closure(source, parser, scope):
    """The files reachable from the scope's roots: [(path, raw, syntax)]."""
    listing = (['ls-files', '--cached', '--others', '--exclude-standard']
               if source.working_tree
               else ['ls-tree', '-r', '--name-only', source.commit])
    listed = subprocess.run(['git', '-C', str(source.root), *listing, '--', *SEARCHED],
                            check=True, capture_output=True, text=True).stdout.splitlines()
    if source.working_tree:
        listed = [p for p in listed if (source.root / p).is_file()]
    paths = [p for p in listed if Path(p).suffix in ('.ml', '.mli') and str(Path(p).parent) in SEARCHED]
    index = {}
    for p in paths:
        extensions = index.setdefault(module(p), {})
        assert Path(p).suffix not in extensions, ('two sources for one module', p, extensions)
        extensions[Path(p).suffix] = p
    for name in BUILTIN:
        index.pop(name, None)
    for name, path in scope.get('module_sources', {}).items():
        assert path in paths, (name, path)
        index[name] = {Path(path).suffix: path}
    todo, seen, files = {module(p) for p in scope['roots']}, set(), []
    while todo:
        wave = sorted(todo - seen)
        todo = set()
        batch = []
        for name in wave:
            assert name in index, ('missing module', name)
            seen.add(name)
            for path in index[name].values():
                raw = source.read(path).encode()
                batch.append((path, raw, hashlib.sha256(raw).hexdigest()))
        syntax = parser.parse({sha: (Path(p).suffix, raw) for p, raw, sha in batch})
        for path, raw, sha in batch:
            files.append((path, raw, syntax[sha]))
            todo.update(d for d in syntax[sha]['dependencies'] if d in index and d not in seen)
    return files


class Spans:
    """Sorted, merged byte ranges, for membership tests."""

    def __init__(self, spans):
        merged = []
        for start, end in sorted(spans):
            if merged and start <= merged[-1][1]:
                merged[-1][1] = max(merged[-1][1], end)
            else:
                merged.append([start, end])
        self.starts = [s for s, _ in merged]
        self.spans = merged

    def __contains__(self, offset):
        i = bisect_right(self.starts, offset) - 1
        return i >= 0 and offset < self.spans[i][1]


class Declarations:
    """A file's declarations, found by position: the innermost one whose
    span contains an offset."""

    def __init__(self, declarations):
        self.items = sorted(declarations, key=lambda d: (d['span'][0], -d['span'][1]))
        self.starts = [d['span'][0] for d in self.items]

    def at(self, offset):
        i = bisect_right(self.starts, offset) - 1
        while i >= 0:
            d = self.items[i]
            if d['span'][0] <= offset < d['span'][1]:
                return d
            i -= 1
        return None


class Graph:
    """Declarations and the references between them, each made at run time
    or logically, resolved through module aliases, functor applications and
    includes. A declaration is a dict from the parse, annotated in place."""

    def __init__(self, units, typed, runtime_units):
        self.nodes = {}                  # (namespace, key) -> declaration
        self.aliases, self.instances, self.includes, self.params = {}, {}, {}, {}
        self.application_args = {}       # instance -> positional arguments
        self.arguments = {}              # (functor, position) -> actual modules
        self.edges = {}                  # id(declaration) -> [(declaration, logical)]
        self.refs = {}                   # path -> [(offset, declaration)]
        self.memo = {}
        found = {}
        for unit, path, raw, syntax in units:
            for d in syntax['declarations']:
                d['path'], d['unit'] = path, unit
                d['erased'] = (d['ghost'] and d['kind'] in ('value', 'type')
                               or d['kind'] in ('value', 'initialization') and d['erased_body']
                               or d['lemma'] and d['name'] not in runtime_units.get(path, {}))
            found[path] = Declarations(syntax['declarations'])
        for path, data in typed.items():
            for m, target in data['aliases']:
                self.aliases[m] = target
            for m, functor, arguments in data['instances']:
                self.instances[m] = functor
                self.application_args[m] = arguments
            for m, target in data['includes']:
                self.includes.setdefault(m, []).append(target)
            for p, functor, position in data['params']:
                self.params[p] = (functor, position)
            if path.endswith('.ml'):
                for kind, key, (start, _) in data['bindings']:
                    d = found[path].at(start)
                    if kind != 'm' and d is not None:
                        self.nodes[(kind, key)] = d
        for instance in self.instances:
            functor, arguments = self.application(instance)
            for position, argument in enumerate(arguments):
                if argument is not None:
                    actuals = self.arguments.setdefault((functor, position), set())
                    actuals.add(argument)
        for unit, path, raw, syntax in units:
            data = typed[path]
            proof = Spans(syntax['proof_spans'])
            if path.endswith('.ml'):
                owner = found[path].at
            else:
                # An interface's references belong to the implementation's
                # declaration of the same name.
                names = {}
                for kind, key, (start, _) in data['bindings']:
                    d = found[path].at(start)
                    if d is not None and kind in ('v', 't') and (kind, key) in self.nodes:
                        names[id(d)] = self.nodes[(kind, key)]
                owner = lambda start, f=found[path], names=names: names.get(id(f.at(start)))
            for kind, key, start, _ in data['refs']:
                targets = self.resolve(kind, key)
                self.refs.setdefault(path, []).extend((start, t) for t in targets)
                d = owner(start)
                if d is None:
                    continue
                logical = d['erased'] or start in proof
                for target in targets:
                    if target is not d:
                        self.edges.setdefault(id(d), []).append((target, logical))
            if path.endswith('.ml'):
                for name, start in syntax['measure_refs']:
                    d = owner(start)
                    if d is not None:
                        for target in self.measure(unit, d['context'], name):
                            self.edges.setdefault(id(d), []).append((target, True))

    def module(self, key, depth=0):
        """The canonical name of a module path."""
        assert depth < 50, key
        parts = key.split('.')
        current = parts[0]
        for part in parts[1:]:
            current = current + '.' + part
            while current in self.aliases or current in self.instances:
                current = self.module(self.aliases.get(current) or self.instances[current], depth + 1)
        return current

    def application(self, key, depth=0):
        """A functor's canonical path and any already-applied arguments."""
        assert depth < 50, key
        if key in self.aliases:
            return self.application(self.aliases[key], depth + 1)
        if key in self.instances:
            functor, arguments = self.application(self.instances[key], depth + 1)
            return functor, arguments + self.application_args[key]
        prefix, _, name = key.rpartition('.')
        if prefix:
            canonical = self.module(prefix) + '.' + name
            if canonical != key:
                return self.application(canonical, depth + 1)
        return key, []

    def resolve(self, kind, key, seen=()):
        """The declarations a path names. A functor parameter refers to
        that member of the actual modules at its argument position."""
        if (kind, key) in self.memo:
            return self.memo[(kind, key)]
        if '.' not in key or (kind, key) in seen:
            return []
        seen = seen + ((kind, key),)
        prefix, name = key.rsplit('.', 1)
        m = self.module(prefix)
        found = []
        if m in self.params:
            functor, position = self.params[m]
            actuals = self.arguments.get((self.module(functor), position), [])
            for argument in sorted(actuals):
                found += self.resolve(kind, argument + '.' + name, seen)
        elif (kind, m + '.' + name) in self.nodes:
            found = [self.nodes[(kind, m + '.' + name)]]
        else:
            for target in self.includes.get(m, []):
                found += self.resolve(kind, target + '.' + name, seen)
        self.memo[(kind, key)] = found
        return found

    def measure(self, unit, context, name):
        """A name in a termination measure, which the typed tree does not
        record: the innermost declaration in scope with that name."""
        head, _, rest = name.rpartition('.')
        for i in range(len(context), -1, -1):
            scope = '.'.join([unit] + context[:i] + ([head] if head else []))
            found = self.resolve('v', scope + '.' + rest)
            if found:
                return found
        return self.resolve('v', name) if head else []

    def reach(self, roots, logical):
        """The declarations reachable from the roots. Without logical
        references, erased declarations are not followed."""
        seen = {id(d): d for d in roots}
        todo = list(roots)
        while todo:
            d = todo.pop()
            if not logical and d['erased']:
                continue
            for target, is_logical in self.edges.get(id(d), []):
                if (logical or not is_logical) and id(target) not in seen:
                    seen[id(target)] = target
                    todo.append(target)
        return seen


# A line whose only run-time code is punctuation or mode annotations, such
# as `}) @ unique ->` after a refined parameter, goes to the other code on
# the line.
WEAK = frozenset(b'()[]{};,|:-<>=.@')
CODES = {'impl': 1, 'model': 2, 'unused': 3, 'proof': 4}


def classify(raw, syntax, runtime, used, model, quoted):
    """{line: column} for the code lines of an implementation file."""
    mask = code_mask(raw)
    for m in EXPECT.finditer(raw):
        mask[m.start():m.end()] = bytes(m.end() - m.start())
    kind = bytearray(len(raw))
    for d in syntax['declarations']:
        if id(d) not in used:
            column = 'unused'
        elif id(d) in runtime:
            column = 'impl'
        elif id(d) in model:
            column = 'model'
        else:
            column = 'proof'
        start, end = d['span']
        kind[start:end] = bytes([CODES[column]]) * (end - start)
    for start, end in Spans(syntax['proof_spans']).spans:
        kind[start:end] = bytes(CODES['proof'] if k in (1, 2) else k for k in kind[start:end])
    weak_bytes = bytearray(raw[i] in WEAK for i in range(len(raw)))
    for start, end in syntax['weak_spans']:
        weak_bytes[start:end] = b'\1' * (end - start)
    names = {v: k for k, v in CODES.items()}
    text = raw.splitlines(keepends=True)
    starts = [0] + list(accumulate(len(line) for line in text))
    lines, scaffold = {}, []
    for n, line in enumerate(text, 1):
        strong, weak = set(), set()
        for i in range(starts[n - 1], starts[n]):
            if mask[i]:
                (weak if weak_bytes[i] else strong).add(kind[i])
        if not strong and not weak:
            continue
        if n in quoted:
            lines[n] = 'spec'
        elif strong & {1, 2, 3}:
            lines[n] = names[min(strong & {1, 2, 3})]
        elif 4 in strong or 4 in weak:
            lines[n] = 'proof'
        elif weak - {0}:
            lines[n] = names[min(weak - {0})]
        else:
            scaffold.append(n)

    def majority(first, last):
        counts = {}
        for n, column in lines.items():
            if first <= n <= last and column != 'spec':
                counts[column] = counts.get(column, 0) + 1
        return max(sorted(counts), key=counts.get) if counts else None

    # Scaffolding goes to the column with most lines in its innermost module.
    modules = sorted(syntax['modules'], key=lambda m: m[1][1] - m[1][0])
    for n in scaffold:
        column = None
        for _, (a, b) in modules:
            if a <= starts[n - 1] < b:
                column = majority(bisect_right(starts, a), bisect_right(starts, b - 1))
                if column:
                    break
        lines[n] = column or majority(1, len(text)) or 'unused'
    return lines


def wrapper_opens(scope, units):
    """{path: [unit]}: the `-open` flags of each file. Files that tests
    compose with `#use` (the census names them in module_sources) each
    define one module at top level, such as `module Dfa_proof = struct ...
    end` in dfa_equivalence_proof.ml; the boundary tests compile them as
    units with `-open` of the units whose modules they mention, and so
    does the line count."""
    if 'module_sources' not in scope:
        return {}
    names = {unit for unit, _, _, _ in units}
    inner = {}
    for unit, path, raw, syntax in units:
        top = [m for m in syntax['modules'] if len(m[0]) == 1]
        if path.endswith('.ml') and top and not any(not d['context'] for d in syntax['declarations']):
            for (name,), _ in top:
                if name not in names or name == unit:
                    inner[name] = unit
    order = {}
    def visit(unit, stack=()):
        if unit in order or unit in stack:
            return
        for u, p, r, s in units:
            if u == unit:
                for d in s['dependencies']:
                    if d in inner:
                        visit(inner[d], stack + (unit,))
        order[unit] = len(order)
    for unit in inner.values():
        visit(unit)
    return {path: sorted({inner[d] for d in syntax['dependencies'] if d in inner} - {unit}, key=order.get)
            for unit, path, raw, syntax in units}


def reachability(source, parser, scope, spec_lines, runtime_units):
    """The demo's files [(unit, path, raw, syntax)], its Graph, and three
    sets of declarations, each as {id: declaration}: those reachable from
    the root modules' exported values at run time, those reachable at all,
    and the model: the ordinary declarations the specification is stated
    in terms of."""
    # Copies, since declarations are annotated in place.
    units = [(module(path), path, raw, json.loads(json.dumps(syntax)))
             for path, raw, syntax in sorted(closure(source, parser, scope))]
    typed = parser.typed(units, wrapper_opens(scope, units))
    graph = Graph(units, typed, runtime_units)
    roots = {module(p) for p in scope['roots']}
    with_interface = {u for u, p, _, _ in units if p.endswith('.mli')}
    exported = {}
    for unit, path, raw, syntax in units:
        if unit in roots and (path.endswith('.mli') or unit not in with_interface):
            for kind, key, _ in typed[path]['bindings']:
                if kind == 'v':
                    exported.update((id(d), d) for d in graph.resolve('v', key))
                elif kind == 'export':
                    # A module whose signature is a named module type.
                    for other, k, _ in typed[path[:-1]]['bindings']:
                        if other == 'v' and k.startswith(key + '.'):
                            exported.update((id(d), d) for d in graph.resolve('v', k))
    def initialization(d):
        return (d['kind'] == 'initialization' or d['kind'] == 'value' and not d['name']) and not d['erased']
    # A root module's initialization runs, and so does that of any module
    # that runs.
    runtime = graph.reach([d for d in exported.values() if not d['erased']]
                          + [d for u, _, _, syntax in units if u in roots
                             for d in syntax['declarations'] if initialization(d)], logical=False)
    while True:
        running = {d['path'] for d in runtime.values() if not d['erased']}
        extra = [d for _, path, _, syntax in units if path in running for d in syntax['declarations']
                 if initialization(d) and id(d) not in runtime]
        if not extra:
            break
        runtime = graph.reach(list(runtime.values()) + extra, logical=False)
    runtime = {i: d for i, d in runtime.items() if not d['erased']}
    used = graph.reach(list(exported.values()) + list(runtime.values())
                       + [d for u, _, _, syntax in units if u in roots for d in syntax['declarations']
                          if d['kind'] in ('initialization', 'value') and not d['name']], logical=True)
    # The specification: the root modules' interfaces (or, without one, the
    # annotations of the exported values) and the lines the page quotes.
    quoted = {}
    for path, line in spec_lines:
        quoted.setdefault(path, set()).add(line)
    stated = []
    for unit, path, raw, syntax in units:
        if unit in roots and path.endswith('.mli'):
            stated += [t for _, t in graph.refs.get(path, [])]
        if path in quoted:
            starts = [0] + list(accumulate(len(l) for l in raw.splitlines(keepends=True)))
            stated += [t for offset, t in graph.refs.get(path, [])
                       if bisect_right(starts, offset) in quoted[path]]
    for d in exported.values():
        if not d['erased']:
            stated += [t for t, logical in graph.edges.get(id(d), []) if logical]
    # The model is what the specification reaches through ordinary
    # declarations that do not run; a run-time type contributes the
    # predicates in its definition. Lemma bodies are not followed.
    model, todo = {}, [d for d in stated if not d['erased']]
    while todo:
        d = todo.pop()
        if id(d) in model:
            continue
        model[id(d)] = d
        for target, logical in graph.edges.get(id(d), []):
            if not target['erased'] and (id(d) not in runtime or d['kind'] == 'type' and logical):
                todo.append(target)
    model = {i: d for i, d in model.items() if i not in runtime and i in used}
    return units, graph, runtime, used, model


def count(source, parser, scope, spec_lines, runtime_units):
    """Spec, Impl, Model, Proof and Unused counts, for the demo and for
    shared dependencies, with a per-file breakdown."""
    units, _, runtime, used, model = reachability(source, parser, scope, spec_lines, runtime_units)
    quoted = {}
    for path, line in spec_lines:
        quoted.setdefault(path, set()).add(line)
    totals = {group: dict({k: 0 for k in COLUMNS}, spec=0, interfaces=0) for group in ('own', 'shared')}
    for path, line in spec_lines:
        totals['own' if owned(scope, path) else 'shared']['spec'] += 1
    records = []
    for unit, path, raw, syntax in units:
        group = 'own' if owned(scope, path) else 'shared'
        record = {'path': path, 'group': group}
        if path.endswith('.mli'):
            record['interfaces'] = len(mask_lines(raw, code_mask(raw)))
            totals[group]['interfaces'] += record['interfaces']
        else:
            lines = classify(raw, syntax, runtime, used, model, quoted.get(path, set()))
            for column in COLUMNS:
                record[column] = sum(1 for c in lines.values() if c == column)
                totals[group][column] += record[column]
        records.append(record)
    return dict(totals['own'], shared=totals['shared'], files=records)
