"""Build the demo catalogue.

    python3 verification/catalogue/build.py [--revision REV] [--working-tree]
        [--prefix _install] [--output _build/catalogue] [--pages ID,...]

Quotes and line counts come from commit REV (default HEAD), read with
`git show`, so the site describes exactly that commit; with --working-tree
they come from the checkout instead, for drafts. The line counts parse
sources with the compiler-libs of the compiler installed in --prefix. Serve
the output with `python3 -m http.server -d _build/catalogue`.
"""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import unquote, urlsplit
import argparse, hashlib, html, json, shutil

import pages as P
from line_stats import Parser, count

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
CSS = 'catalogue-1'
esc = html.escape

# (page, minutes, what to show). The claims themselves are on the pages.
ROUTE = [
    ('flat-hash-table', 6,
     'Open with the client example: the refinement on the result is the whole specification of the call, and the ghost block is erased. '
     'Then the interface: the abstract map and its laws, views and tokens, and what a mutation promises about the rest of the heap. '
     'End with the native code, where views and tokens have disappeared, and the trusted SIMD and storage code.'),
    ('myers-diff', 4,
     'A pure algorithm with an optimality theorem: the patch reconstructs the target and no patch has lower insertion and deletion cost.'),
    ('one-shot-channels', 4,
     'Ownership across domains: a send transfers the payload together with the ownership its refinement mentions. '
     'Point out what is trusted (the atomic and unique-cell primitives) and what is not claimed (progress, linearizability).'),
    ('sat-solver', 3,
     'A result type that carries its own evidence: SAT returns a model, UNSAT a proof that no assignment satisfies the formula, both erased.'),
]

LEGEND = ('<p><strong>Reviewed</strong>: two independent reviews found no false claim, and the page states every gap '
          'they found. <strong>Review pending</strong>: the page is current, but a review found something to fix or has '
          'not been redone. <strong>In progress</strong>: a public contract is weaker than the demo needs, and the page '
          'says how.</p>')

COUNTS = ('<p>Line counts are physical lines with code, comments excluded. <strong>Spec</strong> is the interface the '
          'page quotes. <strong>Impl</strong> and <strong>Proof</strong> cover every module reachable from the demo\'s root '
          'modules; Proof is the lines inside <code>ghost_</code> expressions or refinement types, plus whole '
          '<code>total</code> declarations whose result is a refined <code>unit</code> or a ghost value. A line with '
          'both counts in both, so the columns do not add up. Modules shared with other demos are counted separately. '
          'This is a syntactic count, not a measure of compiled code.</p>')


def shell(title, body, prefix='', script=''):
    return P.page_shell(title, CSS, prefix, body, script)


def header(prefix=''):
    return (f'<p class="site-nav"><a href="{prefix}index.html">Vox demonstrations</a> · '
            f'<a href="{prefix}trust.html">What every demo trusts</a> · '
            f'<a href="{prefix}presentation.html">Presentation guide</a> · '
            f'<a href="{prefix}statistics/index.html">Line counts</a></p>')


def counts_line(stats, prefix):
    return (f'<span class="line-stats">Spec {stats["spec"]:,} · Impl {stats["impl"]:,} · Proof {stats["proof"]:,}</span>'
            f' <a class="line-stats" href="{prefix}statistics/{stats["id"]}.html">Line counts →</a>')


def build(args):
    source = P.Source(ROOT, args.revision, args.working_tree)
    catalogue = json.loads((HERE / 'catalogue.json').read_text())
    only = set(args.pages.split(',')) if args.pages else None
    pages = P.load(only)
    demos = [d for d in catalogue['demos'] if d in pages]
    assert only or set(demos) == {p for p in pages if not p.startswith('_')}, 'catalogue.json lists every page'
    output = Path(args.output)
    shutil.rmtree(output, ignore_errors=True)
    output.mkdir(parents=True)

    parser = Parser(args.prefix, ROOT / '_build' / 'catalogue-cache')
    stats = {}
    for ident in demos:
        meta, body = pages[ident]
        stats[ident] = dict(count(source, parser, catalogue['census'][ident], P.interface_lines(body, source),
                                  catalogue['runtime_units']), id=ident)

    for ident, (meta, body) in pages.items():
        extra = ('<section><h2>Line counts</h2><p>' + counts_line(stats[ident], '../') + '</p></section>'
                 if ident in stats else '')
        P.build_page(ident, meta, body, source, output, CSS, extra)

    by_status = {s: sum(pages[d][0]['status'] == s for d in demos) for s in P.STATUS}
    rows = ''.join(
        f'<li id="{d}"><h3><a href="specs/{d}.html">{P.inline(pages[d][0]["title"])}</a> '
        f'<span class="status status-{pages[d][0]["status"]}">{P.STATUS[pages[d][0]["status"]]}</span></h3>'
        f'<p>{P.inline(pages[d][0]["blurb"])}</p><p>{counts_line(stats[d], "")}</p></li>' for d in demos)
    mechanisms = ''.join(
        f'<li><h3>{esc(m["name"])}</h3><p>{P.inline(m["summary"])}</p>'
        + (('<p class="related">Used in, for example: '
            + ', '.join(f'<a href="specs/{d}.html">{P.inline(pages[d][0]["title"])}</a>' for d in m['demos'] if d in pages)
            + '</p>') if any(d in pages for d in m['demos']) else '') + '</li>'
        for m in catalogue['mechanisms'])
    summary = ' · '.join(f'{n} {P.STATUS[s].lower()}' for s, n in by_status.items() if n)
    (output / 'index.html').write_text(shell('Vox demonstrations',
        header() + '<h1>Vox demonstrations</h1>'
        '<p>Vox extends OxCaml with refinement types checked by Z3 at compile time, erased ghost code and affine '
        'ghost ownership. Each demo below is ordinary OxCaml code whose specification the compiler checks. Its page '
        'states what is proved, what the demo trusts and what it does not claim, and quotes the code at '
        f'{P.commit_line(source).removeprefix("Sources: ")}.</p>'
        f'<p><strong>{len(demos)} demos</strong> · {summary}</p>' + LEGEND
        + f'<h2>Demos</h2><ul class="demo-list">{rows}</ul>'
        + f'<h2>Language mechanisms</h2><ul class="demo-list">{mechanisms}</ul>'))

    route = ''.join(
        f'<section class="tour-step"><p class="eyebrow">{n} · {minutes} min</p>'
        f'<h2><a href="specs/{d}.html">{P.inline(pages[d][0]["title"])}</a></h2>'
        f'<p>{P.inline(pages[d][0]["blurb"])}</p><p>{esc(show)}</p></section>'
        for n, (d, minutes, show) in enumerate(ROUTE, 1) if d in pages)
    table = ''.join(f'<tr><th scope="row"><a href="specs/{d}.html">{P.inline(pages[d][0]["title"])}</a></th>'
                    f'<td>{P.STATUS[pages[d][0]["status"]]}</td><td>{P.inline(pages[d][0]["blurb"])}</td></tr>' for d in demos)
    (output / 'presentation.html').write_text(shell('Presentation guide',
        header() + '<h1>Presentation guide</h1>'
        '<p>A route through four demos for a 20-minute talk, then the state of every demo.</p>' + route
        + compiler_example(output, source, 'hm-wasm-compiler' in pages)
        + '<section class="tour-step"><h2>All demos</h2>' + LEGEND
        + f'<div class="table-scroll"><table class="stats-table"><thead><tr><th>Demo</th><th>Status</th><th>Claim</th>'
          f'</tr></thead><tbody>{table}</tbody></table></div></section>',
        script='<script src="compiler-demo.js"></script>'))

    statistics(output, pages, demos, stats, source)
    source.write_views(output, CSS)
    shutil.copy(HERE / 'style.css', output / 'style.css')
    shutil.copy(HERE / 'compiler-demo.js', output / 'compiler-demo.js')
    check(output)
    print(f'Built {len(demos)} demo pages from {source.short} in {output}.')


def compiler_example(output, source, linked):
    """Two WebAssembly modules emitted by the HM-to-Wasm compiler for the
    design document's id/map example, run in the browser."""
    example = HERE / 'compiler-example'
    target = output / 'demos' / 'compiler'
    target.mkdir(parents=True)
    cases = []
    for n, result in ((4, '12'), (8, '0')):
        entry = {'input': n, 'output': result, 'status': 1, 'tag': '1'}
        for kind, name in (('wasm', f'input-{n}.wasm'), ('memory', f'input-{n}.memory.bin')):
            raw = (example / name).read_bytes()
            (target / name).write_bytes(raw)
            entry[kind] = {'url': f'demos/compiler/{name}', 'sha256': hashlib.sha256(raw).hexdigest()}
        cases.append(entry)
    (target / 'manifest.json').write_text(json.dumps({'cases': cases}, indent=1) + '\n')
    fixture = 'verification/catalogue/compiler-example/original_run_smoke.ml'
    source.read(fixture)
    return ('<section class="tour-step compiler-example" id="compiler-example" data-state="idle">'
            '<p class="eyebrow">Compiler example · In progress</p><h2>Polymorphic id and map, compiled to WebAssembly</h2>'
            '<p>The worked example of the compiler\'s design combines polymorphic <code>id</code> and <code>map</code>, '
            'captured closures, recursion and a final branch. The buttons run the two modules the verified compiler '
            'emits for the inputs 4 and 8, and check the result and every byte of final memory against what the '
            'compiler\'s WebAssembly model predicts. The modules are saved outputs of '
            f'<a href="{esc(source.view(fixture))}">original_run_smoke.ml</a>, which compiles the example with the '
            'compiler on this commit and gives the same bytes; this page does not compile anything.</p>'
            '<div class="demo-actions"><button type="button" data-wasm-case="0">Run input 4</button>'
            '<button type="button" data-wasm-case="1">Run input 8</button></div>'
            '<div class="demo-result" role="status" aria-live="polite"><strong data-wasm-result>Choose an input to run '
            'its emitted program.</strong><p data-wasm-detail>Expected results: 4 → 12 and 8 → 0.</p></div>'
            '<p class="tour-limit">The compiler\'s theorems are conditional'
            + ('; see <a href="specs/hm-wasm-compiler.html">its page</a>' if linked else '')
            + '.</p></section>')


def statistics(output, pages, demos, stats, source):
    target = output / 'statistics'
    target.mkdir()
    rows = ''
    for d in demos:
        s = stats[d]
        title = P.inline(pages[d][0]['title'])
        rows += (f'<tr><th scope="row"><a href="{d}.html">{title}</a></th>'
                 + ''.join(f'<td>{s[k]:,}</td>' for k in ('spec', 'impl', 'proof')) + '</tr>')
        body = (header('../') + f'<h1>{title}: line counts</h1><p>At {P.commit_line(source).removeprefix("Sources: ")}. '
                f'<a href="../specs/{d}.html">Demo page</a>.</p>' + COUNTS
                + '<div class="table-scroll"><table class="stats-table"><thead><tr><th></th><th>Spec</th><th>Impl</th>'
                  '<th>Proof</th><th>Both</th><th>Interface files</th></tr></thead><tbody>')
        for label, data in (('Demo', s), ('Shared modules', s['shared'])):
            body += f'<tr><th scope="row">{label}</th>' + ''.join(
                f'<td>{data[k]:,}</td>' for k in ('spec', 'impl', 'proof', 'mixed', 'interfaces')) + '</tr>'
        body += ('</tbody></table></div><div class="table-scroll"><table class="stats-table"><thead><tr><th>File</th>'
                 '<th></th><th>Impl</th><th>Proof</th><th>Both</th><th>Interface</th></tr></thead><tbody>')
        for f in s['files']:
            source.read(f['path'])
            body += (f'<tr><th scope="row"><a href="../{esc(source.view(f["path"]))}">{esc(f["path"])}</a></th>'
                     f'<td>{"Demo" if f["group"] == "own" else "Shared"}</td>'
                     + ''.join(f'<td>{f[k]:,}</td>' if k in f else '<td>—</td>'
                               for k in ('impl', 'proof', 'mixed', 'interfaces')) + '</tr>')
        body += '</tbody></table></div>'
        (target / f'{d}.html').write_text(shell(f'{pages[d][0]["title"]}: line counts', body, '../'))
    (target / 'index.html').write_text(shell('Line counts',
        header('../') + '<h1>Line counts</h1>' + COUNTS
        + '<div class="table-scroll"><table class="stats-table"><thead><tr><th>Demo</th><th>Spec</th><th>Impl</th>'
          f'<th>Proof</th></tr></thead><tbody>{rows}</tbody></table></div>', '../'))


class Links(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.ids, self.links = set(), []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if 'id' in a:
            self.ids.add(a['id'])
        self.links += [a[k] for k in ('href', 'src') if k in a]


def check(output):
    """Every local link and anchor resolves."""
    pages = {p: Links(p.read_text()) for p in output.rglob('*.html')}
    errors = []
    for path, page in pages.items():
        for link in page.links:
            u = urlsplit(link)
            if u.scheme or u.netloc:
                continue
            target = (path.parent / unquote(u.path)).resolve() if u.path else path
            if not target.exists():
                errors.append((str(path.relative_to(output)), link))
            elif u.fragment and target in pages and unquote(u.fragment) not in pages[target].ids:
                errors.append((str(path.relative_to(output)), link))
    assert not errors, errors[:20]


if __name__ == '__main__':
    arguments = argparse.ArgumentParser()
    arguments.add_argument('--revision', default='HEAD')
    arguments.add_argument('--working-tree', action='store_true')
    arguments.add_argument('--prefix', default=str(ROOT / '_install'))
    arguments.add_argument('--output', default=str(ROOT / '_build' / 'catalogue'))
    arguments.add_argument('--pages')
    build(arguments.parse_args())
