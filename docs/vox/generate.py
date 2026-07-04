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
    'demo/lean_overview.ml':
        'first-contact basics; the example walkthrough covers them',
    'demo/lean_reflect.ml':
        'reflection overview; the walkthrough shows the same len/append',
    'demo/lean_verify.ml': 'first-contact basics; the walkthrough is the page version',
    'demo/lean_embed.ml': 'embedded blocks appear in the fib and flip cards',
    'demo/lean_embedclient.ml': 'client side of lean_embed',
    'demo/lean_sig.ml': 'specced .mli; the cross-module card uses reflectclient',
    'demo/lean_sigclient.ml': 'client side of lean_sig',
    'demo/lean_spec.ml': 'the -vox-prelude FILE spelling of lean_reflect',
    'demo/lean_borrow_elem.ml': 'slot borrows; the borrow and quicksort cards cover the idea',
    'demo/lean_flip_proph.ml': 'prophecy-flavored variant of the flip card',
    'demo/lean_qsort_run.ml': 'expect-block runner for the quicksort card',
    'demo/lean_slice_sort.ml':
        'proof-style merge sort; quicksort is the page\'s sorting development',
    'demo/lean_slice_sort_run.ml':
        'expect-block runner for the merge sorts, mentioned on the quicksort card',
    'demo/lean_ptrie_packed.ml':
        'client of lib/ptrie_packed; the packed subsection slices the lib itself',
    'demo/lean_reverse.ml':
        'McCarthy-array methodology note; kernel and quicksort are the page\'s array cards',
    'demo/lean_seal.ml':
        'sealed interfaces are linked, not sliced, in the signatures section',
    'demo/lean_seal_alt.ml':
        'the identical client body of lean_seal.ml, against step_double',
    'demo/lean_oset.ml':
        'full abstraction is linked, not sliced, in the signatures section',
    'demo/lean_bst.ml':
        'the BST is linked, not sliced, in the signatures section',
    'demo/lean_bst_alt.ml':
        'the quantified alt interface is linked from the signatures section',
    'demo/lean_ptrie.ml':
        'the trie is linked, not sliced, in the signatures section',
    'demo/lean_borrow.ml':
        'the smallest borrow; the quicksort card is the page\'s borrow demo',
    'demo/lean_kernel.ml':
        'bounds-check elimination is noted on the quicksort card',
    'demo/lean_htbl.ml': 'verified hash table; page card pending',
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


def strip_ml_comments(text):
    """Remove (non-nested) OCaml comments and the blank lines they
    leave behind."""
    text = re.sub(r'[ \t]*\(\*.*?\*\)\n?', '', text, flags=re.S)
    return re.sub(r'\n{3,}', '\n\n', text).strip('\n')


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

    snippets = {
        '@FIB_IMPLS@': strip_ml_comments(
            slice_between(fib, r'^let rec total_ fib',
                          r'^\[@@vox\.decreases n\]')
            + '\n\n'
            + slice_between(fib, r'^let fib_slow',
                            r'^      else \(y, x \+ y\)$')
            + '\n  end'),
        '@FIB_LEMMAS@': slice_between(fib, r'^\[%%vox\.lean \{lean\|',
                                      r'^\|lean\}\]'),
        '@ADT@': slice_between(read('demo/lean_adt.ml'), r'^let head',
                               r'Nil -> 0'),
        '@RECORDS@': slice_between(read('demo/lean_records.ml'), r'^let origin',
                                   r'\{ px = p.py; py = p.px \}'),
        '@CONTRACTS@': slice_between(fib, r'^let rec fib_loop',
                                     r'^let fib_iter .* = fib_loop n 0 0 1'),
        '@CROSSMOD@': slice_between(read('demo/lean_reflectclient.ml'),
                                    r'^let l2', r'^let f5'),
        '@EMBEDDED@': '[%%vox.lean {lean|\n...\n\n'
                      + slice_between(fib,
                                      r'@\[grind =\] theorem fib_double \(',
                                      r':= by$')
                      + '\n  ...\n|lean}]',
        '@BINSEARCH@': slice_between(read('demo/lean_binsearch.ml'),
                                     r'^let lower_bound', r'^  =')
                       + '\n  ...',
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
                                 r'^  if x < y then y else x'),
        '@ISQRT@': slice_between(read('demo/lean_isqrt.ml'),
                                 r'^let total_ sq', r'^let total_ sq')
                   + '\n\n'
                   + slice_between(read('demo/lean_isqrt.ml'), r'^let isqrt',
                                   r'go 0 \(x \+ 1\)'),
        '@FLIP@': slice_between(read('demo/lean_flip.ml'), r'^let rec flip',
                                r'^  else h1')
                  + '\n\n'
                  + slice_between(read('demo/lean_flip.ml'),
                                  r'^let roundtrip', r'^  r$'),
        '@SEP@': slice_between(read('demo/lean_sep.ml'), r'^let swap',
                               r'^  t$'),
        '@SEPLIST@': slice_between(read('demo/lean_seplist.ml'),
                                   r'^let rec total_ rev_append',
                                   r'rev_append vs\' \(ICons \(v, ws\)\)')
                     + '\n\n'
                     + slice_between(read('demo/lean_seplist.ml'),
                                     r'^let reverse',
                                     r'^    @ unique =')
                     + '\n  fun l vs t -> ...',
        '@HERO_QSORT@': slice_between(read('demo/lean_qsort.ml'),
                                      r'^let rec psort',
                                      r'^\(\* Sorting an ARRAY',
                                      include_stop=False).rstrip('\n'),
        '@QSORT_ARRAY@': slice_between(read('demo/lean_qsort.ml'),
                                       r'^let sort_array', r"^  x'$"),
    }

    nth = read('demo/lean_nth.ml')
    nth_fail = read('mechanics/lean_nth_fail.ml')
    snippets['@EX_LEN@'] = (
        slice_between(nth, r'^type ilist', r'^  \| Cons of int \* ilist')
        + '\n\n'
        + slice_between(nth, r'^let rec total_ len',
                        r'Cons \(_, t\) -> 1 \+ len t'))
    snippets['@EX_APPEND@'] = slice_between(
        nth, r'^\(\* Each obligation', r'^    Cons \(h, r\)$')
    snippets['@EX_NTH@'] = slice_between(
        nth, r'^let rec nth',
        r'if i = 0 then h else nth t \(i - 1\)')
    assume = read('demo/lean_assume.ml')
    snippets['@ASSUME_INLINE@'] = slice_between(
        assume, r'^\(\* Inline, at the boundary', r'nth l \(assume_ i\)')
    snippets['@ASSUME_FAIL_OUT@'] = slice_between(
        assume, r'^Exception:', r'^Failure "vox: assume_ check')
    snippets['@ASSUME_LEMMA@'] = (
        slice_between(assume, r'^let rec total_ rev',
                      r'append \(rev t\) \(Cons \(h, Nil\)\)')
        + '\n\n'
        + slice_between(assume, r'^\(\* Lemma-style',
                        r'rev \(rev l\) = l \} = assume_ \(\)'))
    snippets['@HERO_FAIL@'] = slice_between(
        nth_fail, r'^let rec nth', r'if i = 0 then h else nth t \(i - 1\)')
    snippets['@HERO_FAIL_OUT@'] = slice_between(
        nth_fail, r'^Line \d+, characters', r'^\(lean: ')

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
