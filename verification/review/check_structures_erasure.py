#!/usr/bin/env python3
"""Check that list/tree/ring runtime bodies contain no model or proof calls.

Usage: check_structures_erasure.py COMPILER_PREFIX

Ported from the erasure part of the lists/trees v3 and rings v3 structures
gate. Compiles the modules with ocamlopt -dlambda and inspects the named
functions' bodies.
"""
from pathlib import Path
import json, re, shutil, subprocess, sys

root = Path(__file__).resolve().parents[2]
prefix = Path(sys.argv[1]).resolve()
out = root / '_build/structures-erasure-check'
out.mkdir(parents=True, exist_ok=True)
fixtures = root / 'testsuite/tests/vox'
library = root / 'verification/library'
flags = ['-nostdlib', '-I', str(prefix / 'lib/ocaml'), '-I', '.',
         '-extension', 'refinement_types']
order = ['pref', 'vox_pref_semantics', 'pref_list', 'pref_tree', 'pref_ring',
         'pref_ring_proofs', 'pref_ring_checks', 'pref_ring_alloc',
         'pref_ring_splice_model', 'pref_ring_splice_setup', 'pref_ring_splice',
         'pref_ring_reverse_model', 'pref_ring_reverse_setup', 'pref_ring_reverse',
         'pref_ring_general', 'pref_ring_splice_general']

def run(args, name):
    p = subprocess.run([str(a) for a in args], cwd=out, capture_output=True, text=True)
    (out / name).write_text(p.stdout + p.stderr)
    assert p.returncode == 0, p.stderr
    return p.stdout + p.stderr

lambdas = {}
for module in order:
    folder = library if module in ('pref', 'vox_pref_semantics') else fixtures
    for ext in ('.mli', '.ml'):
        if (folder / (module + ext)).exists():
            shutil.copy(folder / (module + ext), out)
    if (out / (module + '.mli')).exists():
        run([prefix / 'bin/ocamlopt', *flags, '-c', module + '.mli'], module + '.mli.log')
    lambdas[module] = run([prefix / 'bin/ocamlopt', *flags, '-dlambda', '-c', module + '.ml'],
                          module + '.lambda')

proofs = ('heap|valid|rev_append|flipped|flipped_all|reversed|append|spliced|'
          'last|chain|fold_correct|add_correct')
checked = {}
for module, names in {
    'pref_list': ['reverse_into', 'reverse', 'observe_framed', 'observe_read', 'observe'],
    'pref_tree': ['mirror', 'mirror_with_frame', 'observe_framed', 'observe_read', 'observe'],
    'pref_ring': ['splice_range', 'reverse_nodes'],
    'pref_ring_splice': ['splice_demo'],
    'pref_ring_reverse': ['reverse_demo'],
    'pref_ring_general': ['reverse', 'adopt', 'release'],
    'pref_ring_splice_general': ['splice', 'adopt', 'release', 'swap'],
}.items():
    text = lambdas[module]
    for name in names:
        matches = list(re.finditer(r'\b' + name + r'/\d+(?:\s*=)?\s*\(function', text))
        assert matches, (module, name)
        for index, match in enumerate(matches):
            start = text.index('(function', match.start())
            depth = 0
            for end in range(start, len(text)):
                depth += (text[end] == '(') - (text[end] == ')')
                if depth == 0:
                    break
            body = text[start:end + 1]
            assert not re.search(r'\(apply (?:' + proofs + r')/', body), (module, name)
            for model in ('Pref_ring_checks', 'Pref_ring_reverse_model',
                          'Pref_ring_splice_model', 'Pref_ring_proofs',
                          'Vox_pref_semantics'):
                assert '(global ' + model + '!)' not in body, (module, name, model)
            if name.startswith('observe'):
                assert 'caml_pref_split' not in body and 'caml_pref_join' not in body, (module, name)
            checked[module + '.' + name + '#' + str(index + 1)] = 'no model/proof call'
(out / 'erasure.json').write_text(json.dumps(checked, indent=2) + '\n')
print(f'{len(checked)} runtime bodies checked; evidence in {out}')
