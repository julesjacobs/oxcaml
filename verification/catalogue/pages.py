"""Demo pages: text files that quote the repository at one commit.

A page is `pages/<id>.md`: a header of `key: value` lines (a key with an
empty value takes the `  - item` lines after it as a list) ending at `---`,
then a body in a small Markdown subset (paragraphs, `## ` headings, `- `
bullets, fenced blocks, `code`, **bold**, [links](url)) plus directives that
quote the repository:

    @code PATH              the whole file
    @code PATH 12-40        lines 12 to 40
    @code PATH "from" "to"  from the line containing `from` to the next line
                            containing `to` (`from` must occur once)
    @code PATH "from" "to" after
                            the same, starting just after `from`
    @text ...               the same, shown without highlighting
    @performance FILE       a table of median times; FILE (in pages/) names
                            a benchmark CSV and the rows and columns to show

PATH is relative to the repository root. Quotes are read with `git show` at
the commit being built, so a page always shows exactly that commit.
"""
from pathlib import Path
import csv, html, json, os, re, shlex, statistics, subprocess

from highlight_code import highlight

esc = html.escape

# The home page of the site the catalogue is part of (build.py --home-url).
# When set, every page's navigation starts with a link to it.
HOME = None


def home_link(separator=' · '):
    return f'<a href="{esc(HOME)}">Vox</a>{separator}' if HOME else ''
STATUS = {'reviewed': 'Reviewed', 'owner-review': 'Ready for owner review',
          'review-pending': 'Review pending', 'in-progress': 'In progress'}
HERE = Path(__file__).resolve().parent


def inline(s):
    out, cursor = [], 0
    for m in re.finditer(r'`([^`]+)`|\*\*([^*]+)\*\*|\[([^\]]+)\]\(([^)\s]+)\)', s):
        out.append(esc(s[cursor:m.start()]))
        if m[1] is not None:
            out.append('<code>' + esc(m[1]) + '</code>')
        elif m[2] is not None:
            out.append('<strong>' + inline(m[2]) + '</strong>')
        else:
            out.append('<a href="' + esc(m[4]) + '">' + inline(m[3]) + '</a>')
        cursor = m.end()
    return ''.join(out) + esc(s[cursor:])


class Source:
    """The repository at one commit. `working_tree` reads the checkout
    instead, for drafts."""

    repository = 'https://github.com/julesjacobs/oxcaml'

    def __init__(self, root, revision, working_tree=False):
        self.root = Path(root)
        self.working_tree = working_tree
        self.commit = subprocess.run(['git', '-C', str(self.root), 'rev-parse', revision],
                                     check=True, capture_output=True, text=True).stdout.strip()
        self.short = 'working tree' if working_tree else self.commit[:10]
        self.files = {}

    def read(self, path):
        if path not in self.files:
            if self.working_tree:
                file = self.root / path
                assert file.is_file(), f'{path} does not exist'
                self.files[path] = file.read_bytes()
            else:
                result = subprocess.run(['git', '-C', str(self.root), 'show', f'{self.commit}:{path}'],
                                        capture_output=True)
                assert result.returncode == 0, f'{path} is not in commit {self.short}'
                self.files[path] = result.stdout
        return self.files[path].decode()

    def view(self, path):
        return 'sources/' + path + '.html'

    def github(self, path):
        return f'{self.repository}/blob/{self.commit}/{path}'

    def write_views(self, output, css):
        for path in sorted(self.files):
            source = self.read(path)
            raw = output / 'sources' / (path + '.txt')
            raw.parent.mkdir(parents=True, exist_ok=True)
            raw.write_text(source)
            target = output / self.view(path)
            rel = lambda p: os.path.relpath(output / p, target.parent)
            n = len(source.splitlines())
            gutter = ''.join(f'<a id="L{i}" href="#L{i}">{i}</a>\n' for i in range(1, n + 1))
            code = highlight(source) if path.endswith(('.ml', '.mli')) else esc(source)
            target.write_text(
                '<!doctype html><html lang="en"><head><meta charset="utf-8">'
                '<meta name="viewport" content="width=device-width,initial-scale=1">'
                f'<title>{esc(Path(path).name)}</title><link rel="stylesheet" href="{rel("style.css")}?v={css}"></head>'
                f'<body><main class="source-page"><p>{home_link()}<a href="{rel("index.html")}">Catalogue</a> · '
                f'<a href="{esc(raw.name)}">Raw</a> · <a href="{esc(self.github(path))}">GitHub</a></p>'
                f'<h1>{esc(path)}</h1><p>Commit {self.short}</p>'
                f'<div class="numbered"><pre class="gutter">{gutter}</pre><pre><code>{code}</code></pre></div>'
                '</main></body></html>')


def select(source, spec, where):
    lines = source.splitlines()
    if not spec:
        return 1, lines
    if len(spec) == 1:
        m = re.fullmatch(r'(\d+)-(\d+)', spec[0])
        assert m, (where, spec)
        first, last = int(m[1]), int(m[2])
        assert 1 <= first <= last <= len(lines), (where, spec)
        return first, lines[first - 1:last]
    after = spec[-1] == 'after'
    start, end = spec[:2]
    hits = [i for i, line in enumerate(lines) if start in line]
    assert len(hits) == 1, (where, start, f'occurs {len(hits)} times')
    first = hits[0]
    last = next((i for i in range(first, len(lines)) if end in lines[i]), None)
    assert last is not None, (where, end)
    excerpt = lines[first:last + 1]
    if after:
        excerpt[0] = excerpt[0].split(start, 1)[1]
    return first + 1, excerpt


def performance(source, spec, prefix, used):
    """A table of median times from a benchmark CSV."""
    used.add(spec['source'])
    rows = csv.DictReader(line for line in source.read(spec['source']).splitlines() if not line.startswith('#'))
    samples = {}
    for row in rows:
        if row['payload'] == spec['payload']:
            key = (row['implementation'], int(row['entries']), row['operation'])
            samples.setdefault(key, []).append(float(row['ns_per_op']))
    labels = [label for _, label in spec['columns']] + [label for _, _, label in spec['ratios']]
    head = '<tr><th>Entries</th><th>Operation</th>' + ''.join('<th>' + esc(l) + '</th>' for l in labels) + '</tr>'
    body = ''
    for entries in spec['entries']:
        for operation in spec['operations']:
            median = {name: statistics.median(samples[(name, entries, operation)]) for name, _ in spec['columns']}
            body += (f'<tr><th scope="row">{entries:,}</th><td>{esc(operation)}</td>'
                     + ''.join(f'<td>{median[name]:.1f}</td>' for name, _ in spec['columns'])
                     + ''.join(f'<td>{median[a] / median[b]:.1f}×</td>' for a, b, _ in spec['ratios']) + '</tr>')
    return ('<div class="table-scroll"><table class="stats-table perf-table"><thead>' + head + '</thead><tbody>'
            + body + f'</tbody></table></div><p class="excerpt-source">Medians of <a href="{prefix}{esc(source.view(spec["source"]))}">'
            + esc(spec['source']) + '</a></p>')


def parse(path):
    text = path.read_text()
    head, sep, body = text.partition('\n---\n')
    assert sep, f'{path}: missing header'
    meta = {}
    key = None
    for line in head.splitlines():
        if line.startswith('  - ') and key:
            meta.setdefault(key, []).append(line[4:].strip())
        elif ':' in line:
            key, value = line.split(':', 1)
            key = key.strip()
            if value.strip():
                meta[key] = value.strip()
    return meta, body


def render_body(body, source, prefix, where, used):
    out, para, items = [], [], []

    def flush():
        if para:
            out.append('<p>' + inline(' '.join(para)) + '</p>')
            para.clear()
        if items:
            out.append('<ul>' + ''.join('<li>' + inline(i) + '</li>' for i in items) + '</ul>')
            items.clear()

    lines = body.splitlines()
    i = 0
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()
        if stripped.startswith('```'):
            flush()
            lang = stripped[3:].strip()
            block = []
            i += 1
            while not lines[i].strip().startswith('```'):
                block.append(lines[i])
                i += 1
            code = '\n'.join(block) + '\n'
            out.append('<pre><code>' + (highlight(code) if lang == 'ocaml' else esc(code)) + '</code></pre>')
        elif stripped.startswith('@'):
            flush()
            words = shlex.split(stripped)
            kind, path, spec = words[0], words[1], words[2:]
            if kind == '@performance':
                out.append(performance(source, json.loads((HERE / 'pages' / path).read_text()), prefix, used))
                i += 1
                continue
            assert kind in ('@code', '@text'), (where, kind)
            first, excerpt = select(source.read(path), spec, where)
            used.add(path)
            code = '\n'.join(excerpt) + '\n'
            shown = highlight(code) if kind == '@code' and path.endswith(('.ml', '.mli')) else esc(code)
            span = '' if not spec else (f', line {first}' if len(excerpt) == 1 else f', lines {first}–{first + len(excerpt) - 1}')
            anchor = '' if not spec else f'#L{first}'
            out.append(f'<pre><code>{shown}</code></pre><p class="excerpt-source"><a href="{prefix}{esc(source.view(path))}{anchor}">{esc(path)}</a>{span}</p>')
        elif stripped.startswith('## '):
            flush()
            out.append('<h2>' + inline(stripped[3:]) + '</h2>')
        elif stripped.startswith('- '):
            if para:
                flush()
            items.append(stripped[2:])
        elif not stripped:
            flush()
        elif items and line.startswith('  '):
            items[-1] += ' ' + stripped
        else:
            if items:
                flush()
            para.append(stripped)
        i += 1
    flush()
    return ''.join(out)


def page_shell(title, css, prefix, body, script=''):
    return ('<!doctype html><html lang="en"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width,initial-scale=1">'
            f'<title>{esc(title)}</title><link rel="stylesheet" href="{prefix}style.css?v={css}"></head>'
            f'<body><main class="spec-page">{body}</main>{script}</body></html>')


def load(only=None):
    """The pages, or those named in `only`."""
    pages = {}
    for path in sorted((HERE / 'pages').glob('*.md')):
        if only and path.stem not in only:
            continue
        meta, body = parse(path)
        for key in ('title', 'blurb', 'status', 'date'):
            assert key in meta, (path.name, key)
        if not path.stem.startswith('_'):
            assert meta['status'] in STATUS, (path.name, meta['status'])
            assert '\n## Trusted base\n' in body, f'{path.stem}: a page states its trusted base'
            assert '\n## Interface\n' in body, f'{path.stem}: a page quotes its interface'
        pages[path.stem] = (meta, body)
    return pages


def summary(body):
    """The paragraphs before the first heading, for the catalogue index."""
    return body.split('\n## ', 1)[0].strip()


def interface_lines(body, source):
    """(path, line) for each code line quoted under `## Interface`, which is
    what the Spec line count measures."""
    from line_stats import code_lines
    section = body.split('\n## Interface\n', 1)[1].split('\n## ', 1)[0]
    lines = set()
    for line in section.splitlines():
        if line.strip().startswith(('@code ', '@text ')):
            words = shlex.split(line.strip())
            first, excerpt = select(source.read(words[1]), words[2:], 'interface')
            for n in code_lines('\n'.join(excerpt)):
                lines.add((words[1], first + n - 1))
    return lines


def commit_line(source):
    if source.working_tree:
        return 'Sources: the working tree'
    return (f'Sources: <a href="{esc(source.repository)}/tree/{source.commit}">'
            f'julesjacobs/oxcaml</a> at commit {source.short}')


def build_page(ident, meta, body, source, output, css, stats_html=''):
    """Write specs/<ident>.html and its source list; `_name` pages go to
    <name>.html at the top level."""
    used = set()
    if ident.startswith('_'):
        content = (f'<p>{home_link()}<a href="index.html">← Catalogue</a></p><h1>' + inline(meta['title']) + '</h1>'
                   f'<p class="page-meta">{esc(meta["date"])} · {commit_line(source)}</p>'
                   + render_body(body, source, '', ident, used))
        (output / (ident[1:] + '.html')).write_text(page_shell(meta['title'], css, '', content))
        return used
    body = body.replace('\n## Trusted base\n',
                        '\n## Trusted base\n\nBeyond [what every demo trusts](../trust.html):\n\n', 1)
    content = (f'<p>{home_link()}<a href="../index.html#{ident}">← Catalogue</a></p>'
               f'<h1>{inline(meta["title"])}</h1>'
               f'<p class="page-meta">{esc(meta["date"])} · {commit_line(source)}</p>'
               + render_body(body, source, '../', ident, used) + stats_html)
    listed = []
    for entry in meta.get('sources', []):
        path, _, role = entry.partition(' — ')
        source.read(path)
        used.add(path)
        listed.append((path, role))
    listed += [(p, '') for p in sorted(used) if p not in {q for q, _ in listed}]
    files = ('<ul class="source-list">'
             + ''.join(f'<li><a href="../{esc(source.view(p))}">{esc(p)}</a>'
                       + (f' <span>{inline(role)}</span>' if role else '') + '</li>' for p, role in listed)
             + '</ul>')
    content += '<section><h2>Source files</h2>' + files + '</section>'
    (output / 'specs').mkdir(exist_ok=True)
    (output / 'specs' / f'{ident}.html').write_text(page_shell(meta['title'], css, '../', content))
    return used
