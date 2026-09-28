"""The explorer's hand-written content: descriptions and tours.

    python3 verification/explorer/content.py [--revision REV] [--working-tree]
        [--show]

checks every file in verification/explorer/content/ against the repository
at REV (default HEAD) and prints the errors and warnings; with --show it
also prints where every note and tour stop resolves. The format is in
content/SCHEMA.md. build.py uses `load` to compile the content into the
site's data.
"""
from pathlib import Path, PurePosixPath
import argparse, html, json, re, subprocess, sys

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
CONTENT = HERE / 'content'
esc = html.escape

SHORT_MAX = 200
TOUR_TITLE_MAX = 70
RANGE_WARN = 150


class Repo:
    """The tracked files of the repository at one commit (or the working
    tree), read lazily."""

    def __init__(self, root, revision='HEAD', working_tree=False):
        self.root = Path(root)
        self.working_tree = working_tree
        self.commit = git(self.root, 'rev-parse', revision).strip()
        listing = git(self.root, 'ls-tree', '-r', '-z', '--name-only', self.commit)
        self.paths = set(p for p in listing.split('\0') if p)
        self.dirs = {'/'}
        for p in self.paths:
            parts = p.split('/')
            for i in range(1, len(parts)):
                self.dirs.add('/'.join(parts[:i]) + '/')
        self.cache = {}

    def lines(self, path):
        if path not in self.cache:
            if self.working_tree:
                raw = (self.root / path).read_bytes()
            else:
                raw = subprocess.run(['git', '-C', str(self.root), 'show', f'{self.commit}:{path}'],
                                     check=True, capture_output=True).stdout
            self.cache[path] = raw.decode('utf-8', 'replace').splitlines()
        return self.cache[path]


def git(root, *args):
    return subprocess.run(['git', '-C', str(root), *args], check=True, capture_output=True,
                          text=True).stdout


class Problems:
    def __init__(self):
        self.errors, self.warnings = [], []

    def error(self, where, message):
        self.errors.append(f'{where}: {message}')

    def warn(self, where, message):
        self.warnings.append(f'{where}: {message}')


def patterns(value):
    if value is None:
        return []
    if isinstance(value, str):
        return [value]
    if isinstance(value, list) and value and all(isinstance(v, str) and v for v in value):
        return value
    raise ValueError(f'a pattern is a non-empty string or a list of them, not {value!r}')


def resolve(repo, path, spec, where, problems):
    """(first, last), 1-based and inclusive, for a range written with
    `from`/`to`/`span`/`lines`; None if it does not resolve."""
    try:
        froms, tos = patterns(spec.get('from')), patterns(spec.get('to'))
    except ValueError as e:
        problems.error(where, str(e))
        return None
    if not froms:
        problems.error(where, 'a line range needs a "from" pattern, so that it survives edits elsewhere '
                       'in the file')
        return None
    if tos and 'span' in spec:
        problems.error(where, 'give "to" or "span", not both')
        return None
    lines = repo.lines(path)
    hits = [i for i, line in enumerate(lines) if froms[0] in line]
    if len(hits) != 1:
        where_hits = ', '.join(str(h + 1) for h in hits[:8])
        problems.error(where, f'"from" pattern {froms[0]!r} must occur on exactly one line of {path}; '
                       f'it occurs on {len(hits)}' + (f' (lines {where_hits})' if hits else ''))
        return None
    first = hits[0]
    for p in froms[1:]:
        nxt = next((i for i in range(first + 1, len(lines)) if p in lines[i]), None)
        if nxt is None:
            problems.error(where, f'"from" pattern {p!r} does not occur after line {first + 1} of {path}')
            return None
        first = nxt
    last = first
    if tos:
        start = first
        for k, p in enumerate(tos):
            nxt = next((i for i in range(start if k == 0 else start + 1, len(lines)) if p in lines[i]), None)
            if nxt is None:
                problems.error(where, f'"to" pattern {p!r} does not occur at or after line {start + 1} of {path}')
                return None
            start = last = nxt
    elif 'span' in spec:
        span = spec['span']
        if not isinstance(span, int) or span < 1:
            problems.error(where, '"span" is a positive number of lines')
            return None
        last = first + span - 1
        if last >= len(lines):
            problems.error(where, f'"span" runs past the end of {path} ({len(lines)} lines)')
            return None
    first, last = first + 1, last + 1
    if 'lines' in spec:
        m = re.fullmatch(r'(\d+)(?:-(\d+))?', str(spec['lines']))
        if not m:
            problems.error(where, '"lines" is "N" or "N-M"')
        else:
            a, b = int(m[1]), int(m[2] or m[1])
            if (a, b) != (first, last):
                problems.warn(where, f'"lines" says {spec["lines"]} but the patterns give {first}-{last} '
                              f'at {repo.commit[:10]}')
    if last - first + 1 > RANGE_WARN:
        problems.warn(where, f'range {first}-{last} is {last - first + 1} lines; a reader sees about 40')
    return first, last


class Markdown:
    """A small Markdown subset rendered to HTML: paragraphs, `- ` lists,
    fenced code, `code`, **bold**, *emphasis* and links. Internal links:
    src:PATH (a file or directory at REV), tour:ID or tour:ID/N, demo:ID."""

    def __init__(self, repo, demos, tours):
        self.repo, self.demos, self.tours = repo, demos, tours
        self.pending = []   # (where, tour, stop) checked once every tour is loaded

    def text(self, value, where, problems):
        if isinstance(value, list) and all(isinstance(v, str) for v in value):
            return '\n\n'.join(value)
        if isinstance(value, str):
            return value
        problems.error(where, 'text is a string or a list of paragraph strings')
        return ''

    def inline(self, s, where, problems):
        out, cursor = [], 0
        pattern = r'`([^`]+)`|\*\*([^*]+)\*\*|\*([^*\s][^*]*)\*|\[([^\]]+)\]\(([^)\s]+)\)'
        for m in re.finditer(pattern, s):
            out.append(esc(s[cursor:m.start()]))
            if m[1] is not None:
                out.append('<code>' + esc(m[1]) + '</code>')
            elif m[2] is not None:
                out.append('<strong>' + self.inline(m[2], where, problems) + '</strong>')
            elif m[3] is not None:
                out.append('<em>' + self.inline(m[3], where, problems) + '</em>')
            else:
                out.append(self.link(m[4], m[5], where, problems))
            cursor = m.end()
        return ''.join(out) + esc(s[cursor:])

    def link(self, label, target, where, problems):
        body = self.inline(label, where, problems)
        if target.startswith('src:'):
            path = target[4:]
            if path not in self.repo.paths and path not in self.repo.dirs:
                problems.error(where, f'link to {path}, which is not a file or directory at '
                               f'{self.repo.commit[:10]} (directories end in "/")')
            return f'<a href="#p/{esc(path)}" data-src="{esc(path)}">{body}</a>'
        if target.startswith('tour:'):
            ident, _, stop = target[5:].partition('/')
            self.pending.append((where, ident, stop))
            return f'<a href="#tour/{esc(target[5:])}">{body}</a>'
        if target.startswith('demo:'):
            ident = target[5:]
            if ident not in self.demos:
                problems.error(where, f'link to demo {ident}, which is not in catalogue.json')
            return f'<a href="{{catalogue}}specs/{esc(ident)}.html" data-demo="{esc(ident)}">{body}</a>'
        if re.match(r'https?://', target):
            return f'<a href="{esc(target)}" rel="noopener">{body}</a>'
        problems.error(where, f'link target {target!r}: use https://, src:, tour: or demo:')
        return body

    def render(self, value, where, problems):
        source = self.text(value, where, problems)
        out, para, items = [], [], []

        def flush():
            if para:
                out.append('<p>' + self.inline(' '.join(para), where, problems) + '</p>')
                para.clear()
            if items:
                out.append('<ul>' + ''.join('<li>' + self.inline(i, where, problems) + '</li>'
                                            for i in items) + '</ul>')
                items.clear()

        lines = source.splitlines()
        i = 0
        while i < len(lines):
            line = lines[i]
            stripped = line.strip()
            if stripped.startswith('```'):
                flush()
                block = []
                i += 1
                while i < len(lines) and not lines[i].strip().startswith('```'):
                    block.append(lines[i])
                    i += 1
                out.append('<pre><code>' + esc('\n'.join(block)) + '</code></pre>')
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

    def check_pending(self, tours, problems):
        for where, ident, stop in self.pending:
            if ident not in tours:
                problems.error(where, f'link to tour {ident}, which does not exist')
            elif stop and not (stop.isdigit() and 1 <= int(stop) <= len(tours[ident]['stops'])):
                problems.error(where, f'link to stop {stop} of tour {ident}, which has '
                               f'{len(tours[ident]["stops"])} stops')


def sentences(text):
    plain = re.sub(r'`[^`]*`', 'x', text)
    return len(re.findall(r'[.!?](?:\s|$)', plain))


def check_keys(obj, allowed, where, problems):
    for k in obj:
        if k not in allowed and not k.startswith('_'):
            problems.error(where, f'unknown key {k!r} (allowed: {", ".join(sorted(allowed))}; '
                           'keys starting with "_" are ignored)')


def short_text(value, where, problems):
    if not isinstance(value, str) or not value.strip():
        problems.error(where, '"short" is a non-empty string')
        return ''
    if len(value) > SHORT_MAX:
        problems.warn(where, f'"short" is {len(value)} characters; keep it under {SHORT_MAX}')
    if '\n' in value:
        problems.error(where, '"short" is one line')
    return value


def load(repo, demos, problems, content=CONTENT):
    """The compiled content: {'descriptions': {path: ...}, 'tours': [...]}.
    Markdown is rendered to HTML; `{catalogue}` in links is left for the
    caller to replace."""
    tour_files = sorted((Path(content) / 'tours').glob('*.json'))
    tour_ids = {p.stem for p in tour_files}
    md = Markdown(repo, demos, tour_ids)

    def block(entry, key, where):
        value = entry.get(key)
        if value is None:
            return None
        if not isinstance(value, dict):
            problems.error(where, f'"{key}" is an object with "short" and optionally "long"')
            return None
        check_keys(value, {'short', 'long'}, f'{where}.{key}', problems)
        out = {'short': short_text(value.get('short'), f'{where}.{key}.short', problems)}
        if 'long' in value:
            out['long'] = md.render(value['long'], f'{where}.{key}.long', problems)
        return out

    descriptions, origin = {}, {}
    for file in sorted((Path(content) / 'descriptions').glob('*.json')):
        name = f'descriptions/{file.name}'
        try:
            data = json.loads(file.read_text())
        except json.JSONDecodeError as e:
            problems.error(name, f'invalid JSON: {e}')
            continue
        if not isinstance(data, dict):
            problems.error(name, 'a descriptions file is an object mapping paths to entries')
            continue
        for path, entry in data.items():
            if path.startswith('_'):
                continue
            where = f'{name}: {path}'
            if path in origin:
                problems.error(where, f'also described in {origin[path]}')
                continue
            origin[path] = name
            is_dir = path == '/' or path.endswith('/')
            if (is_dir and path not in repo.dirs) or (not is_dir and path not in repo.paths):
                hint = ' (directories end in "/")' if not is_dir and path + '/' in repo.dirs else ''
                problems.error(where, f'no such {"directory" if is_dir else "file"} at '
                               f'{repo.commit[:10]}{hint}')
                continue
            if not isinstance(entry, dict):
                problems.error(where, 'an entry is an object')
                continue
            check_keys(entry, {'what', 'vox', 'notes'}, where, problems)
            out = {}
            for key in ('what', 'vox'):
                b = block(entry, key, where)
                if b:
                    out[key] = b
            notes = []
            for n, note in enumerate(entry.get('notes', []), 1):
                w = f'{where} note {n}'
                if is_dir:
                    problems.error(w, 'notes belong to files')
                    break
                check_keys(note, {'from', 'to', 'span', 'lines', 'kind', 'title', 'text'}, w, problems)
                span = resolve(repo, path, note, w, problems)
                kind = note.get('kind', 'vox')
                if kind not in ('vox', 'what'):
                    problems.error(w, '"kind" is "vox" or "what"')
                if 'text' not in note:
                    problems.error(w, 'a note has "text"')
                if span:
                    notes.append({'lines': list(span), 'kind': kind, 'title': note.get('title', ''),
                                  'html': md.render(note.get('text', ''), w, problems)})
            if notes:
                out['notes'] = notes
            if not out:
                problems.warn(where, 'empty entry')
            descriptions[path] = out

    tours = {}
    for file in tour_files:
        name = f'tours/{file.name}'
        try:
            data = json.loads(file.read_text())
        except json.JSONDecodeError as e:
            problems.error(name, f'invalid JSON: {e}')
            continue
        check_keys(data, {'id', 'title', 'kind', 'demo', 'order', 'summary', 'stops'}, name, problems)
        if data.get('id') != file.stem:
            problems.error(name, f'"id" must be the file name, {file.stem!r}')
        kind = data.get('kind')
        if kind not in ('vox', 'demo'):
            problems.error(name, '"kind" is "vox" or "demo"')
        if kind == 'demo' and data.get('demo') not in demos:
            problems.error(name, f'"demo" must be a demo of catalogue.json, not {data.get("demo")!r}')
        if not isinstance(data.get('title'), str) or not data['title']:
            problems.error(name, 'a tour has a "title"')
        stops = []
        if not isinstance(data.get('stops'), list) or not data['stops']:
            problems.error(name, 'a tour has a non-empty list of "stops"')
            data['stops'] = []
        for n, stop in enumerate(data['stops'], 1):
            w = f'{name} stop {n}'
            check_keys(stop, {'title', 'path', 'from', 'to', 'span', 'lines', 'focus', 'text'}, w, problems)
            path = stop.get('path', '')
            is_dir = path == '/' or path.endswith('/')
            if (is_dir and path not in repo.dirs) or (not is_dir and path not in repo.paths):
                problems.error(w, f'no such {"directory" if is_dir else "file"} {path!r} at {repo.commit[:10]}')
                continue
            title = stop.get('title', '')
            if not title:
                problems.error(w, 'a stop has a "title"')
            elif len(title) > TOUR_TITLE_MAX:
                problems.warn(w, f'title is {len(title)} characters; keep it under {TOUR_TITLE_MAX}')
            span = None
            if 'from' in stop or 'lines' in stop or 'to' in stop:
                if is_dir:
                    problems.error(w, 'a directory stop has no line range')
                else:
                    span = resolve(repo, path, stop, w, problems)
            focus = stop.get('focus')
            if focus is not None and focus not in repo.dirs:
                problems.error(w, f'"focus" {focus!r} is not a directory (directories end in "/")')
            text = md.text(stop.get('text', ''), w, problems)
            count = sentences(text)
            if not 2 <= count <= 6:
                problems.warn(w, f'the narrative has about {count} sentences; aim for 2 to 6')
            stops.append({'title': title, 'path': path, 'lines': list(span) if span else None,
                          'focus': focus, 'html': md.render(text, w, problems)})
        tours[file.stem] = {'id': file.stem, 'title': data.get('title', file.stem), 'kind': kind,
                            'demo': data.get('demo'), 'order': data.get('order', 1000),
                            'summary': md.render(data.get('summary', ''), f'{name} summary', problems),
                            'stops': stops}
    md.check_pending(tours, problems)
    ordered = sorted(tours.values(), key=lambda t: (t['kind'] != 'vox', t['order'], t['title']))
    return {'descriptions': descriptions, 'tours': ordered}


def demo_ids():
    return json.loads((HERE.parent / 'catalogue' / 'catalogue.json').read_text())['demos']


def main():
    arguments = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    arguments.add_argument('--revision', default='HEAD')
    arguments.add_argument('--working-tree', action='store_true',
                           help='check paths and patterns against the checkout instead of REV')
    arguments.add_argument('--show', action='store_true', help='print where each note and stop resolves')
    args = arguments.parse_args()
    repo = Repo(ROOT, args.revision, args.working_tree)
    problems = Problems()
    content = load(repo, demo_ids(), problems)
    if args.show:
        for path, entry in sorted(content['descriptions'].items()):
            for note in entry.get('notes', []):
                a, b = note['lines']
                print(f'note  {path}:{a}-{b}  {note["title"]}')
        for tour in content['tours']:
            for n, stop in enumerate(tour['stops'], 1):
                where = stop['path'] + (f':{stop["lines"][0]}-{stop["lines"][1]}' if stop['lines'] else '')
                print(f'{tour["id"]} {n:2}  {where}  {stop["title"]}')
    for w in problems.warnings:
        print('warning:', w)
    for e in problems.errors:
        print('error:', e)
    print(f'{len(content["descriptions"])} descriptions, {len(content["tours"])} tours, '
          f'{sum(len(t["stops"]) for t in content["tours"])} stops at {repo.commit[:10]}'
          f'{" (working tree)" if repo.working_tree else ""}: '
          f'{len(problems.errors)} errors, {len(problems.warnings)} warnings')
    sys.exit(1 if problems.errors else 0)


if __name__ == '__main__':
    main()
