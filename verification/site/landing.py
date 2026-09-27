#!/usr/bin/env python3
"""Writes the landing page of the Vox site.

    landing.py REVISION SITE_DIR

Fills verification/site/index.html with the demo count from the catalogue
and the `clamp` example from the playground, both read with `git show` at
REVISION like the catalogue's quotes, with the size of the playground
already built in SITE_DIR/playground, and with the size of the compiler
from the source explorer's data in SITE_DIR/source. Writes SITE_DIR/index.html and copies
the catalogue's style.css next to it.
"""
import datetime
import gzip
import hashlib
import json
import os
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
sys.path.insert(0, str(ROOT / 'verification' / 'catalogue'))
from highlight_code import highlight  # noqa: E402


def show(revision, path):
    return subprocess.run(['git', '-C', str(ROOT), 'show', f'{revision}:{path}'],
                          check=True, capture_output=True, text=True).stdout


def between(text, start, end):
    """The lines from the one starting with `start` to the one starting
    with `end`, inclusive."""
    lines = text.splitlines()
    first = next(i for i, line in enumerate(lines) if line.startswith(start))
    last = next(i for i in range(first, len(lines)) if lines[i].startswith(end))
    return '\n'.join(lines[first:last + 1])


def first_visit_bytes(playground):
    """What the playground transfers before its first check: every file
    except the interface sources (fetched when a message points into them),
    compressed as the host compresses them. Caddy's `encode` compresses
    text, JavaScript and WebAssembly but not application/octet-stream, so
    lib/bundle.bin travels as it is."""
    total = 0
    for path in playground.rglob('*'):
        relative = path.relative_to(playground).as_posix()
        if not path.is_file() or relative.startswith('lib/src/'):
            continue
        data = path.read_bytes()
        total += len(data) if path.suffix == '.bin' else len(gzip.compress(data, 6))
    return total


def source_lines(explorer):
    """The lines of the compiler proper that the explorer shows, rounded
    to thousands (its tree.json, already built in SITE_DIR/source)."""
    tree = json.loads((explorer / 'data' / 'tree.json').read_text())
    scope = next(s for s in tree['scopes'] if s['id'] == 'proper')
    lines = sum(f[1] for f in tree['files'] if any(f[0].startswith(p) for p in scope['prefixes']))
    return f'{round(lines / 1000):,}k'


def main():
    revision, site = sys.argv[1], Path(sys.argv[2])
    full = subprocess.run(['git', '-C', str(ROOT), 'rev-parse', revision],
                          check=True, capture_output=True, text=True).stdout.strip()
    demos = json.loads(show(full, 'verification/catalogue/catalogue.json'))['demos']
    example = between(show(full, 'verification/playground/examples/refinements.ml'),
                      'let (clamp', 'let bounded')
    css = (ROOT / 'verification' / 'catalogue' / 'style.css').read_bytes()
    values = {
        'demos': str(len(demos)),
        'revision': full,
        'revision_short': full[:10],
        'example': highlight(example),
        'playground_mb': str(round(first_visit_bytes(site / 'playground') / 1e6)),
        'source_lines': source_lines(site / 'source'),
        'date': datetime.date.today().strftime('%-d %B %Y'),
        'css': hashlib.sha256(css).hexdigest()[:10],
    }
    page = (HERE / 'index.html').read_text()
    for key, value in values.items():
        page = page.replace('{{' + key + '}}', value)
    assert '{{' not in page, 'every placeholder is filled'
    (site / 'index.html').write_text(page)
    (site / 'style.css').write_bytes(css)


if __name__ == '__main__':
    main()
