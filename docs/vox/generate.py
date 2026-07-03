#!/usr/bin/env python3
"""Generate docs/vox/index.html from the verified test suite.

Every code snippet on the page is extracted VERBATIM from
testsuite/tests/vox (anchored slices below), so the page cannot claim
anything the suite does not enforce; the failure output is the expect
block of mechanics/lean_wrong.ml, which pins it byte for byte.

Usage, from the repository root:

    python3 docs/vox/generate.py [--ocamlc PATH --lean PATH]

With --ocamlc/--lean, the "what the solver sees" tab is re-captured by
compiling demo/lean_fib.ml through a wrapper that saves the generated
Lean input; without them, the copy already embedded in index.html is
reused (generation then never needs a built compiler).
"""

import argparse, html, os, re, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SUITE = os.path.join(ROOT, 'testsuite', 'tests', 'vox')


SHOWN = set()

def read(rel):
    SHOWN.add(rel)
    with open(os.path.join(SUITE, rel)) as f:
        return f.read()


# Demos deliberately not on the page, with the reason.  A new
# demo/lean_*.ml must either be sliced into a card or listed here --
# otherwise generation fails, so the page cannot silently lag the
# suite.
UNSHOWN = {
    'demo/lean_verify.ml': 'first-contact basics; lean_overview is the page version',
    'demo/lean_embed.ml': 'embedded blocks appear in the fib and flip cards',
    'demo/lean_embedclient.ml': 'client side of lean_embed',
    'demo/lean_sig.ml': 'specced .mli; the cross-module card uses reflectclient',
    'demo/lean_sigclient.ml': 'client side of lean_sig',
    'demo/lean_spec.ml': 'the -vox-prelude FILE spelling of lean_reflect',
    'demo/lean_borrow_elem.ml': 'slot borrows; the borrow and quicksort cards cover the idea',
    'demo/lean_flip_proph.ml': 'prophecy-flavored variant of the flip card',
    'demo/lean_qsort_run.ml': 'expect-block runner for the quicksort card',
}


def check_demo_coverage():
    demos = {'demo/' + f for f in os.listdir(os.path.join(SUITE, 'demo'))
             if re.fullmatch(r'lean_\w+\.ml', f)}
    missing = demos - SHOWN - set(UNSHOWN)
    stale = set(UNSHOWN) - demos
    if missing:
        sys.exit('demos neither shown nor listed as unshown: %s'
                 % ', '.join(sorted(missing)))
    if stale:
        sys.exit('UNSHOWN entries for demos that no longer exist: %s'
                 % ', '.join(sorted(stale)))


def slice_between(text, start, stop, *, include_stop=True, what=''):
    """The lines from the one matching [start] through the one matching
    [stop] (anchors are regexes matched against whole lines)."""
    lines = text.split('\n')
    i = next(k for k, l in enumerate(lines) if re.search(start, l))
    j = next(k for k, l in enumerate(lines[i:], i) if re.search(stop, l))
    return '\n'.join(lines[i:j + 1 if include_stop else j])


def strip_test_header(text):
    return re.sub(r'\(\* TEST.*?\*\)\n\n', '', text, count=1, flags=re.S)


def strip_leading_comment(text):
    return re.sub(r'\A\(\*.*?\*\)\n\n', '', text, count=1, flags=re.S)


def capture_generated_lean(ocamlc, lean):
    with tempfile.TemporaryDirectory() as d:
        save = os.path.join(d, 'generated.lean')
        wrapper = os.path.join(d, 'leansave')
        with open(wrapper, 'w') as f:
            f.write('#!/bin/sh\ncp "$1" %s\nexec %s "$@"\n' % (save, lean))
        os.chmod(wrapper, 0o755)
        src = os.path.join(d, 'lean_fib.ml')
        with open(src, 'w') as f:
            f.write(read('demo/lean_fib.ml'))
        subprocess.run([ocamlc, '-vox-solver-path', wrapper, '-c', 'lean_fib.ml'],
                       cwd=d, check=True)
        with open(save) as f:
            return f.read()


def previous_generated_lean(out_path):
    """Reuse the generated-Lean tab of the existing page."""
    with open(out_path) as f:
        prev = f.read()
    m = re.search(r'<code class="lean" id="genlean">(.*?)</code>', prev, re.S)
    if not m:
        sys.exit('no embedded generated.lean to reuse; pass --ocamlc/--lean')
    return html.unescape(m.group(1))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--ocamlc')
    ap.add_argument('--lean')
    args = ap.parse_args()

    here = os.path.dirname(os.path.abspath(__file__))
    out_path = os.path.join(here, 'index.html')

    fib = read('demo/lean_fib.ml')
    reflect = read('demo/lean_reflect.ml')
    wrong = read('mechanics/lean_wrong.ml')

    snippets = {
        '@SIXTY@': slice_between(read('demo/lean_overview.ml'),
                                 r'^let div', r'^let safe .* else 0'),
        '@FIB_SRC@': strip_test_header(fib),
        '@WRONG_SRC@': slice_between(wrong, r'^let rec total_ fib',
                                     r'^let wrong'),
        '@WRONG_OUT@': slice_between(wrong, r'^Error: vox: verification failed',
                                     r'^\(lean: '),
        '@REFLECT@': slice_between(reflect, r'^let rec total_ len',
                                   r'Cons \(_, t\) -> 1 \+ len t')
                     + '\n\n'
                     + slice_between(reflect,
                                     r'\(\* The textbook inductive proof',
                                     r'Cons \(h, r\)'),
        '@ADT@': slice_between(read('demo/lean_adt.ml'), r'^let head',
                               r'Nil -> 0'),
        '@RECORDS@': slice_between(read('demo/lean_records.ml'), r'^let origin',
                                   r'\{ px = p.py; py = p.px \}'),
        '@CONTRACTS@': slice_between(fib, r'^let rec fib_loop',
                                     r'fun n -> fib_loop n 0 0 1'),
        '@CROSSMOD@': slice_between(read('demo/lean_reflectclient.ml'),
                                    r'^let l2', r'^let f5'),
        '@EMBEDDED@': '[%%vox.lean {lean|\n...\n\n'
                      + slice_between(fib,
                                      r'@\[grind =\] theorem fib_double \(',
                                      r':= by$')
                      + '\n  ...\n|lean}]',
        '@BINSEARCH@': slice_between(read('demo/lean_binsearch.ml'),
                                     r'^let lower_bound', r'^  =')
                       + '\n  fun a x -> ...',
        '@MUTABLE@': slice_between(read('demo/lean_mutable.ml'),
                                   r'^let iota', r'^  refine_ x$'),
        '@IMP_BINSEARCH@': slice_between(read('demo/lean_imp_binsearch.ml'),
                                         r'^let search2',
                                         r'^  \{ lo = l; hi = h \}$'),
        '@PCELL@': slice_between(read('demo/lean_pcell.ml'), r'^let swap_sum',
                                 r'read c1 a t1 in') + '\n    ...',
        '@TUPLES@': slice_between(read('demo/lean_tuples.ml'), r'^let swap',
                                  r'\| \(x, y\) -> \(y, x\)')
                    + '\n\n'
                    + slice_between(read('demo/lean_tuples.ml'),
                                    r'^let first_pos', r'\| \(x, _\) -> x'),
        '@QUANT@': slice_between(read('demo/lean_quant.ml'), r'^let max2',
                                 r'fun x y -> if x < y then y else x'),
        '@ISQRT@': slice_between(read('demo/lean_isqrt.ml'),
                                 r'^let total_ sq', r'^let total_ sq')
                   + '\n\n'
                   + slice_between(read('demo/lean_isqrt.ml'), r'^let isqrt',
                                   r'go 0 \(x \+ 1\)'),
        '@BORROW@': slice_between(read('demo/lean_borrow.ml'), r'^let bump',
                                  r'^  s$'),
        '@FLIP@': slice_between(read('demo/lean_flip.ml'), r'^let rec flip',
                                r'^  else h1')
                  + '\n\n'
                  + slice_between(read('demo/lean_flip.ml'),
                                  r'^let roundtrip', r'^  r$'),
        '@SEP@': slice_between(read('demo/lean_sep.ml'), r'^let swap',
                               r'^  t$'),
        '@QSORT@': slice_between(read('demo/lean_qsort.ml'),
                                 r'^let rec qsort', r'@ local unique =$')
                   + '\n  fun m -> ...\n\n'
                   + slice_between(read('demo/lean_qsort.ml'),
                                   r'Par_lib.fork_join2', r'ignore ur;'),
        '@BST_MLI@': strip_leading_comment(read('demo/bst.mli')),
        '@BST_ML@': strip_leading_comment(read('demo/bst.ml')),
        '@BST_CLIENT@': slice_between(read('demo/lean_bst.ml'), r'^let demo',
                                      r'\(hit, miss\)'),
        '@BST_FAIL@': slice_between(read('mechanics/lean_bst_fail.ml'),
                                    r'^let rec member_wrong', r'^\[%%expect',
                                    include_stop=False).rstrip('\n'),
        '@BST_FAIL_OUT@': slice_between(read('mechanics/lean_bst_fail.ml'),
                                        r'^Error: vox: verification failed',
                                        r'^\(lean: '),
        '@KERNEL@': slice_between(read('demo/lean_kernel.ml'),
                                  r'^let get', r'^    go 0 0'),
        '@BST_ALT@': slice_between(read('demo/lean_bst_alt.ml'),
                                   r'^module Set', r'^end')
                     + '\n\n'
                     + slice_between(read('demo/lean_bst_alt.ml'),
                                     r'^let probe', r'^    b$'),
    }

    if args.ocamlc and args.lean:
        snippets['@GEN_LEAN@'] = capture_generated_lean(args.ocamlc, args.lean)
    else:
        snippets['@GEN_LEAN@'] = previous_generated_lean(out_path)

    tpl = open(os.path.join(here, 'template.html')).read()
    for k, v in snippets.items():
        assert k in tpl, k
        tpl = tpl.replace(k, html.escape(v.rstrip('\n'), quote=False))
    leftover = re.findall(r'@[A-Z_]+@', tpl)
    assert not leftover, leftover
    check_demo_coverage()
    with open(out_path, 'w') as f:
        f.write(tpl)
    print('wrote %s (%d bytes)' % (out_path, len(tpl)))


if __name__ == '__main__':
    main()
