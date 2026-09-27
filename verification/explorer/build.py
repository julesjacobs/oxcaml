"""Build the Vox source explorer.

    python3 verification/explorer/build.py [--revision REV] [--output DIR]
        [--prefix PREFIX] [--catalogue DIR] [--catalogue-url URL]

Writes a static site to DIR (default _build/explorer): the app (app/), the
data it reads (data/) and every tracked source file at REV (src/<path>.txt,
served compressed by the host). Nothing runs on the server.

The data describe commit REV (default HEAD), read with git, never the
checkout:
- the tree: every tracked file outside the exclusions of config.json, with
  its line count and language;
- Vox provenance: the diff of each file against the upstream merge base
  (config.json "base"): upstream, Vox-modified or Vox-new, lines added and
  removed, and the changed line ranges;
- the demos: for each demo of verification/catalogue/catalogue.json, the
  files its page lists and quotes, and the modules its line counts cover
  (the catalogue's census, parsed with the compiler-libs of the compiler
  in PREFIX; without one, dependencies are found by a coarser scan);
- the content of content/ (descriptions and tours), checked against REV:
  a path, pattern or link that does not resolve fails the build.

--catalogue names a built catalogue (verification/catalogue/build.py),
whose rendered sources the file view links to; without it they are
computed from the pages. --catalogue-url is where the catalogue is served
relative to the explorer (default ../catalogue/).

Serve with `python3 -m http.server -d _build/explorer`.
"""
from pathlib import Path
import argparse, gzip, hashlib, html, json, re, shlex, shutil, subprocess, sys, time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
CATALOGUE = HERE.parent / 'catalogue'
sys.path.insert(0, str(CATALOGUE))
sys.path.insert(0, str(HERE))

import content as C  # noqa: E402


def git(*args, binary=False):
    out = subprocess.run(['git', '-C', str(ROOT), *args], check=True, capture_output=True).stdout
    return out if binary else out.decode()


def excluded(path, config):
    return any(path == p or (p.endswith('/') and path.startswith(p)) for p in config['exclude'])


def language(path, config):
    suffix = Path(path).suffix
    for name, suffixes in config['languages'].items():
        if suffix in suffixes:
            return name
    return 'other'


def read_blobs(entries):
    """{path: bytes} for [(path, sha)], through one `git cat-file --batch`."""
    shas = '\n'.join(sha for _, sha in entries) + '\n'
    out = subprocess.run(['git', '-C', str(ROOT), 'cat-file', '--batch'], input=shas.encode(),
                         check=True, capture_output=True).stdout
    blobs, offset = {}, 0
    for path, sha in entries:
        header_end = out.index(b'\n', offset)
        header = out[offset:header_end].split()
        assert header[0].decode() == sha and header[1] == b'blob', header
        size = int(header[2])
        blobs[path] = out[header_end + 1:header_end + 1 + size]
        offset = header_end + 1 + size + 1
    return blobs


def line_count(raw):
    if not raw:
        return 0
    return raw.count(b'\n') + (0 if raw.endswith(b'\n') else 1)


def unquote(path):
    """A path as git prints it in a diff header: C-quoted when unusual."""
    if not path.startswith('"'):
        return path
    body = path[1:-1].encode('latin-1', 'backslashreplace').decode('unicode_escape')
    return body.encode('latin-1').decode('utf-8', 'replace')


def diff(base, revision, config):
    """{path: {'status', 'added', 'removed', 'ranges'}} for every file that
    differs between base and revision. ranges is compact text:
    `12+3` three lines added at 12, `40~2-1` lines 40-41 replace one old
    line, `55-4` four lines removed after line 55 (0: before line 1)."""
    exclude = [f':(exclude){p}' for p in config['exclude']]
    out = git('-c', 'core.quotePath=false', 'diff', '-U0', '--no-color', '--no-renames', '--no-ext-diff',
              base, revision, '--', '.', *exclude)
    files, current = {}, None
    for line in out.splitlines():
        if line.startswith('diff --git '):
            current = {'status': 'modified', 'added': 0, 'removed': 0, 'ranges': []}
            path = None
        elif line.startswith('new file mode'):
            current['status'] = 'new'
        elif line.startswith('deleted file mode'):
            current['status'] = 'deleted'
        elif line.startswith('+++ '):
            target = line[4:]
            if target != '/dev/null':
                path = unquote(target)[2:] if not target.startswith('"') else unquote(target)[2:]
                files[path] = current
        elif line.startswith('--- '):
            source = line[4:]
            if current['status'] == 'deleted':
                files[unquote(source)[2:]] = current
        elif line.startswith('Binary files'):
            m = re.match(r'Binary files (?:/dev/null|a/(.*)) and (?:/dev/null|b/(.*)) differ', line)
            if m and m[2]:
                files[unquote(m[2])] = current
        elif line.startswith('@@'):
            m = re.match(r'@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@', line)
            old_n = int(m[2]) if m[2] is not None else 1
            new_start, new_n = int(m[3]), int(m[4]) if m[4] is not None else 1
            current['added'] += new_n
            current['removed'] += old_n
            if old_n == 0:
                current['ranges'].append(f'{new_start}+{new_n}')
            elif new_n == 0:
                current['ranges'].append(f'{new_start}-{old_n}')
            else:
                current['ranges'].append(f'{new_start}~{new_n}-{old_n}')
    return {p: f for p, f in files.items() if f['status'] != 'deleted'}


class RegexParser:
    """A stand-in for line_stats.Parser without a compiler: a module's
    dependencies are the capitalised names it mentions."""

    def parse(self, files):
        found = {}
        for sha, (_, raw) in files.items():
            text = re.sub(rb'\(\*.*?\*\)', b'', raw, flags=re.S)
            found[sha] = {'dependencies': sorted({m.decode() for m in re.findall(rb"\b([A-Z][A-Za-z0-9_']*)", text)})}
        return found


def demos(revision, prefix, paths):
    """For each demo: its page's metadata, its source list (path, role),
    the files its page quotes, and the census closure split into own and
    shared modules. Also the set of files the catalogue renders."""
    import pages as P
    import line_stats as L
    catalogue = json.loads(git('show', f'{revision}:verification/catalogue/catalogue.json'))
    source = P.Source(ROOT, revision)
    compiler = Path(prefix) / 'bin' / 'ocamlopt.opt'
    if compiler.exists():
        parser = L.Parser(prefix, ROOT / '_build' / 'explorer-cache')
        how = f'parsed with {compiler}'
    else:
        parser = RegexParser()
        how = f'no compiler at {compiler}: dependencies found by name scan'
    rendered = set()
    result = []
    for ident in catalogue['demos']:
        text = git('show', f'{revision}:verification/catalogue/pages/{ident}.md')
        meta, body = parse_page(text)
        sources = []
        for entry in meta.get('sources', []):
            path, _, role = entry.partition(' — ')
            sources.append({'path': path, 'role': role})
        quoted = []
        for line in body.splitlines():
            s = line.strip()
            if s.startswith(('@code ', '@text ')):
                path = shlex.split(s)[1]
                if path not in quoted:
                    quoted.append(path)
            elif s.startswith('@performance '):
                spec = json.loads(git('show', f'{revision}:verification/catalogue/pages/{shlex.split(s)[1]}'))
                quoted.append(spec['source'])
        scope = catalogue['census'][ident]
        own, shared = [], []
        for path, _, _ in sorted(L.closure(source, parser, scope)):
            (own if L.owned(scope, path) else shared).append(path)
        for p in [s['path'] for s in sources] + quoted + own + shared:
            assert p in paths, f'{ident}: {p} is not a file at {revision}'
        rendered.update([s['path'] for s in sources] + quoted + own + shared)
        result.append({'id': ident, 'title': meta['title'], 'blurb': meta.get('blurb', ''),
                       'status': meta.get('status', ''), 'sources': sources,
                       'quoted': quoted, 'own': own, 'shared': shared})
    rendered.add('verification/catalogue/compiler-example/original_run_smoke.ml')
    return result, rendered, how


def parse_page(text):
    head, _, body = text.partition('\n---\n')
    meta, key = {}, None
    for line in head.splitlines():
        if line.startswith('  - ') and key:
            meta.setdefault(key, []).append(line[4:].strip())
        elif ':' in line:
            key, value = line.split(':', 1)
            key = key.strip()
            if value.strip():
                meta[key] = value.strip()
    return meta, body


def human(n):
    return f'{n / 1e6:.1f} MB' if n >= 1e6 else f'{n / 1e3:.0f} kB'


def build(args):
    started = time.time()
    config = json.loads((HERE / 'config.json').read_text())
    commit = git('rev-parse', args.revision).strip()
    base = git('rev-parse', config['base']).strip()
    output = Path(args.output)
    shutil.rmtree(output, ignore_errors=True)
    (output / 'data').mkdir(parents=True)

    listing = git('ls-tree', '-r', '-z', commit).split('\0')
    entries = []
    for item in listing:
        if not item:
            continue
        meta, path = item.split('\t', 1)
        mode, kind, sha = meta.split()
        if kind != 'blob' or excluded(path, config):
            continue
        entries.append((path, sha, mode))
    blobs = read_blobs([(p, s) for p, s, m in entries if m != '120000'])
    changes = diff(base, commit, config)
    print(f'{len(entries)} files at {commit[:10]}, {len(changes)} differ from {base[:10]}')

    demo_list, rendered, how = demos(commit, args.prefix, {p for p, _, _ in entries})
    print(f'{len(demo_list)} demos; census {how}')
    if args.catalogue:
        views = Path(args.catalogue) / 'sources'
        rendered = {str(p.relative_to(views))[:-len('.html')] for p in views.rglob('*.html')}

    files, source_bytes = [], 0
    for path, sha, mode in sorted(entries):
        raw = blobs.get(path, b'')
        binary = mode == '120000' or b'\0' in raw[:8000]
        lines = 0 if binary else line_count(raw)
        change = changes.get(path)
        status = 'u' if change is None else ('n' if change['status'] == 'new' else 'm')
        record = [path, lines, 'binary' if binary else language(path, config), status]
        if status == 'm':
            record += [change['added'], change['removed'], ','.join(change['ranges'])]
        elif status == 'n':
            record += [lines, 0, '']
        if path in rendered:
            record += [0] * (7 - len(record)) + [1]
        files.append(record)
        if not binary:
            target = output / 'src' / (path + '.txt')
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(raw)
            source_bytes += len(raw)

    repo = C.Repo(ROOT, commit)
    problems = C.Problems()
    content = C.load(repo, [d['id'] for d in demo_list], problems, args.content)
    for w in problems.warnings:
        print('warning:', w)
    if problems.errors:
        sys.exit('content errors:\n  ' + '\n  '.join(problems.errors))

    tree = {'revision': commit, 'base': base, 'repository': config['repository'],
            'catalogue': args.catalogue_url, 'scopes': config['scopes'],
            'default_scope': config.get('default_scope', config['scopes'][0]['id']),
            'excluded': config['exclude'], 'built': time.strftime('%Y-%m-%d'),
            'columns': ['path', 'lines', 'language', 'status', 'added', 'removed', 'ranges', 'catalogue'],
            'files': files}
    data = {
        'tree': tree,
        'demos': {'demos': demo_list},
        'content': content,
    }
    # Every asset's name carries a hash of its content, so an index.html
    # only ever loads the files it was built with; version.txt lets a
    # cached index.html notice that a newer build is deployed.
    def hashed(folder, stem, suffix, raw):
        name = f'{stem}.{hashlib.sha256(raw).hexdigest()[:10]}{suffix}'
        (output / folder / name).write_bytes(raw)
        return f'{folder}/{name}' if folder != '.' else name

    names = {}
    for name, value in data.items():
        raw = json.dumps(value, separators=(',', ':'), ensure_ascii=False).encode()
        names[name] = hashed('data', name, '.json', raw)
    names['app'] = hashed('.', 'app', '.js', (HERE / 'app' / 'app.js').read_bytes())
    names['style'] = hashed('.', 'style', '.css', (HERE / 'app' / 'style.css').read_bytes())
    version = hashlib.sha256(json.dumps(names, sort_keys=True).encode()).hexdigest()[:10]
    page = (HERE / 'app' / 'index.html').read_text()
    for key, value in {'version': version, 'app': names['app'], 'style': names['style'],
                       'data': json.dumps({k: names[k] for k in data}), 'revision': commit}.items():
        page = page.replace('{{' + key + '}}', html.escape(value))
    assert '{{' not in page, 'every placeholder of index.html is filled'
    (output / 'index.html').write_text(page)
    (output / 'version.txt').write_text(version + '\n')
    for item in (HERE / 'app').iterdir():
        if item.name not in ('index.html', 'app.js', 'style.css'):
            (shutil.copytree if item.is_dir() else shutil.copy)(item, output / item.name)

    total = sum(f.stat().st_size for f in output.rglob('*') if f.is_file())
    data_bytes = sum(f.stat().st_size for f in (output / 'data').iterdir())
    data_gz = sum(len(gzip.compress(f.read_bytes(), 6)) for f in (output / 'data').iterdir())
    print(f'content: {len(content["descriptions"])} descriptions, {len(content["tours"])} tours, '
          f'{len(problems.warnings)} warnings')
    print(f'size: {human(total)} in all; data {human(data_bytes)} ({human(data_gz)} gzipped, loaded at start); '
          f'sources {human(source_bytes)} in {sum(1 for f in files if f[2] != "binary")} files, fetched one at a time')
    if args.measure:
        sources_gz = sum(len(gzip.compress(f.read_bytes(), 6)) for f in (output / 'src').rglob('*.txt'))
        print(f'sources gzipped: {human(sources_gz)}')
    print(f'built {output} from {commit[:10]} in {time.time() - started:.1f}s')


if __name__ == '__main__':
    arguments = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    arguments.add_argument('--revision', default='HEAD')
    arguments.add_argument('--output', default=str(ROOT / '_build' / 'explorer'))
    arguments.add_argument('--prefix', default=str(ROOT / '_install'),
                           help='an installed compiler whose compiler-libs parse the demos (default _install)')
    arguments.add_argument('--catalogue', help='a built catalogue, whose rendered sources the file view links to')
    arguments.add_argument('--catalogue-url', default='../catalogue/')
    arguments.add_argument('--content', default=str(HERE / 'content'),
                           help='the directory of descriptions and tours (default: content/ in the checkout)')
    arguments.add_argument('--measure', action='store_true', help='also report the gzipped size of the sources')
    build(arguments.parse_args())
