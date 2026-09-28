"""Physical-line counts for each demo: Spec, Impl and Proof.

Spec is the code lines a page quotes under its Interface heading. Impl and
Proof are counted over the demo's module dependency closure: whole modules
reachable from the demo's root modules, found by parsing with
`source_inventory.ml` (built with the installed compiler's compiler-libs).
Proof is syntactic: lines inside `ghost_` expressions or refinement types,
plus whole `total` declarations whose result is a refined `unit` or ghost
value. It is not an erasure analysis. A line with both counts in both.
"""
from pathlib import Path
import hashlib, json, re, subprocess

HERE = Path(__file__).resolve().parent
SEARCHED = ('verification/library', 'testsuite/tests/vox')
BUILTIN = ('Ghost', 'Bigint', 'Stdlib', 'Iarray')


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
    cached by content hash."""

    def __init__(self, prefix, cache):
        self.cache = cache
        cache.mkdir(parents=True, exist_ok=True)
        compiler = Path(prefix) / 'bin' / 'ocamlopt.opt'
        tool = (HERE / 'source_inventory.ml').read_bytes()
        key = hashlib.sha256(tool + compiler.read_bytes()).hexdigest()[:16]
        self.executable = cache / f'source_inventory-{key}'
        if not self.executable.exists():
            libs = Path(prefix) / 'lib' / 'ocaml' / 'compiler-libs'
            build = cache / 'parser-build'
            build.mkdir(exist_ok=True)
            (build / 'source_inventory.ml').write_bytes(tool)
            subprocess.run([str(compiler), '-I', str(libs), str(libs / 'ocamlcommon.cmxa'),
                            'source_inventory.ml', '-o', str(self.executable)], cwd=build, check=True)
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


def closure(source, parser, scope):
    """The files reachable from the scope's roots: [(path, raw, syntax)]."""
    listed = subprocess.run(['git', '-C', str(source.root), 'ls-tree', '-r', '--name-only',
                             source.commit, '--', *SEARCHED],
                            check=True, capture_output=True, text=True).stdout.splitlines()
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


def count(source, parser, scope, spec_lines, runtime_units):
    """Spec, Impl, Proof and Mixed counts, for the demo and for shared
    dependencies, with a per-file breakdown."""
    totals = {group: {'spec': 0, 'impl': 0, 'proof': 0, 'mixed': 0, 'interfaces': 0}
              for group in ('own', 'shared')}
    for path, line in spec_lines:
        totals['own' if owned(scope, path) else 'shared']['spec'] += 1
    files = []
    for path, raw, syntax in sorted(closure(source, parser, scope)):
        group = 'own' if owned(scope, path) else 'shared'
        mask = code_mask(raw)
        record = {'path': path, 'group': group}
        if path.endswith('.mli'):
            record['interfaces'] = len(mask_lines(raw, mask))
            totals[group]['interfaces'] += record['interfaces']
            files.append(record)
            continue
        spans = list(syntax['explicit_proof_spans'])
        for d in syntax['declarations']:
            name = '.'.join(d['context'] + [d['name']])
            runtime = name in runtime_units.get(path, {})
            if d['kind'] == 'value' and d['proof_signature'] and d['declared_total'] and not runtime:
                spans.append(d['span'])
            else:
                spans.extend(d['proof_spans'])
        proof_mask = bytearray(len(raw))
        for start, end in spans:
            proof_mask[start:end] = mask[start:end]
        impl_mask = bytearray(a and not b for a, b in zip(mask, proof_mask))
        proof, impl = mask_lines(raw, proof_mask), mask_lines(raw, impl_mask)
        record.update(impl=len(impl), proof=len(proof), mixed=len(impl & proof))
        for key in ('impl', 'proof', 'mixed'):
            totals[group][key] += record[key]
        files.append(record)
    return dict(totals['own'], shared=totals['shared'], files=files)
