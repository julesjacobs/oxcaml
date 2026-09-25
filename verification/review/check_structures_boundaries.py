#!/usr/bin/env python3
"""Check historical structure demos using a read-only compatible installation."""
from pathlib import Path
import subprocess, shutil, re, sys, json
root = Path(__file__).resolve().parents[2]
prefix = Path(sys.argv[1]).resolve()
out = root/'_build/structures-boundary-check'
lib = out/'library'
lib.mkdir(parents=True, exist_ok=True)
fixtures = root/'testsuite/tests/vox'
flags = ['-nostdlib', '-I', str(prefix/'lib/ocaml'), '-I', '.', '-extension', 'refinement_types']
def run(args, cwd, name, reject=None):
    p = subprocess.run([str(x) for x in args], cwd=cwd, capture_output=True, text=True)
    (out/name).write_text(p.stdout+p.stderr)
    if reject:
        def normalize(s): return s.replace('unboxed ', '').replace('"', '')
        assert p.returncode and normalize(reject) in normalize(p.stderr), p.stderr
    else:
        assert not p.returncode, p.stderr
    return p.stdout
modules = ['pref', 'pref_list', 'pref_tree', 'pref_ring',
           'expression_folding', 'clamp_api', 'int_list_laws']
for module in modules:
    folder = root/'verification/library' if module == 'pref' else fixtures
    for ext in ['.mli', '.ml']:
        source = folder/(module+ext)
        if source.exists():
            shutil.copy(source, lib)
            run([prefix/'bin/ocamlc', *flags, '-c', source.name], lib, module+ext+'.log')
    run([prefix/'bin/ocamlopt', *flags, '-dlambda', '-c', module+'.ml'], lib, module+'.lambda')
print('Public interfaces and implementations verified bytecode/native', flush=True)
for client, visible in {
    'pref_list_client': ['pref', 'pref_list'],
    'pref_tree_client': ['pref', 'pref_tree'],
    'pref_ring_public_client': ['pref', 'pref_ring'],
}.items():
    directory = out/client
    directory.mkdir(exist_ok=True)
    for module in visible: shutil.copy(lib/(module+'.cmi'), directory)
    shutil.copy(fixtures/(client+'.ml'), directory)
    for compiler, ext in [('ocamlc', '.cmo'), ('ocamlopt', '.cmx')]:
        run([prefix/'bin'/compiler, *flags, '-dlambda', '-c', client+'.ml'], directory, client+'.'+compiler+'.lambda')
        exe = directory/(client+'.'+compiler)
        run([prefix/'bin'/compiler, *flags, '-o', exe, *[lib/(m+ext) for m in modules], client+ext], directory, client+'.link.log')
        run([prefix/'bin/ocamlrun', exe] if compiler=='ocamlc' else [exe], directory, client+'.'+compiler+'.output')
    print(client+': public-only compiled client passed', flush=True)
# Compile each successful expect phrase into the subsequent phrase's context;
# failing phrases do not introduce bindings. Run the final successful program.
counts = {}
for fixture in ['expressions', 'clamp', 'int_lists', 'definitions',
                'pref_list_rejected', 'pref_tree_rejected', 'pref_ring_rejected']:
    directory = out/fixture
    directory.mkdir(exist_ok=True)
    visible = {
        'expressions': ['expression_folding'], 'clamp': ['clamp_api'],
        'int_lists': ['int_list_laws'], 'definitions': [],
        'pref_list_rejected': ['pref', 'pref_list'],
        'pref_tree_rejected': ['pref', 'pref_tree'],
        'pref_ring_rejected': ['pref', 'pref_ring'],
    }[fixture]
    for module in visible: shutil.copy(lib/(module+'.cmi'), directory)
    source = (fixtures/(fixture+'.ml')).read_text().split('*)', 1)[1]
    successful = ('open '+fixture.removesuffix('_rejected').capitalize()+'\n') if fixture.endswith('_rejected') else ''
    rejected = 0
    for number, (code, expected) in enumerate(re.findall(r'(.*?)\[%%expect\{\|(.*?)\|\}\]', source, re.S)):
        test = directory/(fixture+'_case.ml')
        test.write_text(successful+code)
        marker = expected.split('Error:', 1)[1].strip().splitlines()[0] if 'Error:' in expected else None
        for compiler in ['ocamlc', 'ocamlopt']:
            run([prefix/'bin'/compiler, *flags, '-c', test.name], directory,
                fixture+'.'+str(number)+'.'+compiler+'.log', reject=marker)
        if marker: rejected += 1
        else: successful += code
    test = directory/(fixture+'_positive.ml')
    test.write_text(successful)
    for compiler, ext in [('ocamlc', '.cmo'), ('ocamlopt', '.cmx')]:
        run([prefix/'bin'/compiler, *flags, '-dlambda', '-c', test.name], directory, fixture+'.'+compiler+'.lambda')
        exe = directory/(fixture+'.'+compiler)
        run([prefix/'bin'/compiler, *flags, '-o', exe, *[lib/(m+ext) for m in modules], test.stem+ext], directory, fixture+'.link.log')
        run([prefix/'bin/ocamlrun', exe] if compiler=='ocamlc' else [exe], directory, fixture+'.'+compiler+'.output')
    counts[fixture] = rejected
    print(f'{fixture}: successful phrases executed; {rejected} rejections passed in both modes', flush=True)
for fixture in ['pref_ring_insert_remove_demo', 'pref_ring_splice_demo', 'pref_ring_reverse_demo']:
    text = (fixtures/(fixture+'.ml')).read_text()
    files = re.search(r'all_modules = "([^"]+)"', text).group(1).split()
    link_modules = []
    for filename in files:
        source = (root/'verification/library'/filename) if filename.startswith('pref.') else fixtures/filename
        shutil.copy(source, lib)
        if filename.endswith('.ml'): link_modules.append(filename[:-3])
        for compiler in (['ocamlc'] if filename.endswith('.mli') else ['ocamlc', 'ocamlopt']):
            run([prefix/'bin'/compiler, *flags, '-dlambda', '-c', filename], lib, filename+'.'+compiler+'.lambda')
    for compiler, ext in [('ocamlc', '.cmo'), ('ocamlopt', '.cmx')]:
        exe = lib/(fixture+'.'+compiler)
        run([prefix/'bin'/compiler, *flags, '-o', exe, *[m+ext for m in link_modules]], lib, fixture+'.link.log')
        run([prefix/'bin/ocamlrun', exe] if compiler=='ocamlc' else [exe], lib, fixture+'.'+compiler+'.output')
    print(fixture+': concrete whole-ring regression passed in both modes', flush=True)
directory = out/'pref_ring_examples_client'
directory.mkdir(exist_ok=True)
for module in ['pref', 'pref_ring', 'pref_ring_splice', 'pref_ring_reverse']:
    shutil.copy(lib/(module+'.cmi'), directory)
shutil.copy(fixtures/'pref_ring_examples_client.ml', directory)
for compiler in ['ocamlc', 'ocamlopt']:
    run([prefix/'bin'/compiler, *flags, '-c', 'pref_ring_examples_client.ml'],
        directory, 'pref_ring_examples_client.'+compiler+'.log')
print('Concrete ring results: public-only semantic client passed in both modes', flush=True)
for module, hidden in [('pref_list', 'reverse_into'), ('pref_tree', 'set_links'),
                       ('pref_ring', 'owns_put'), ('expression_folding', 'add_correct')]:
    directory = out/'hidden'
    directory.mkdir(exist_ok=True)
    shutil.copy(lib/(module+'.cmi'), directory)
    shutil.copy(lib/'pref.cmi', directory)
    source = directory/'hidden.ml'
    source.write_text('let hidden = '+module.capitalize()+'.'+hidden+'\n')
    run([prefix/'bin/ocamlc', *flags, '-c', source.name], directory,
        module+'.hidden.log', reject='Unbound value '+module.capitalize()+'.'+hidden)
erasure = {}
for module, names in {
    'pref_list': ['reverse_into', 'reverse'],
    'pref_tree': ['mirror', 'mirror_with_frame'],
    'pref_ring': ['splice_range', 'reverse_nodes'],
    'expression_folding': ['fold', 'eval_folded'],
    'pref_ring_splice': ['splice_demo'],
    'pref_ring_reverse': ['reverse_demo'],
}.items():
    path = out/(module+'.lambda')
    if not path.exists(): path = out/(module+'.ml.ocamlopt.lambda')
    text = path.read_text()
    for name in names:
        match = re.search(r'\b'+name+r'/\d+(?:\s*=)?\s*\(function', text)
        assert match, (module, name)
        start = text.index('(function', match.start())
        depth = 0
        for end in range(start, len(text)):
            depth += (text[end]=='(') - (text[end]==')')
            if depth == 0: break
        body = text[start:end+1]
        assert not re.search(r'\(apply (?:heap|valid|rev_append|flipped|flipped_all|fold_correct|add_correct)/', body), (module, name)
        assert '(global Pref_ring_checks!)' not in body, (module, name)
        assert '(global Pref_ring_reverse_model!)' not in body, (module, name)
        assert '(global Pref_ring_splice_model!)' not in body, (module, name)
        erasure[module+'.'+name] = 'No runtime model/proof calls or final traversal checker'
(out/'erasure.json').write_text(json.dumps(erasure, indent=2)+'\n')
(out/'rejections.json').write_text(json.dumps(counts, indent=2)+'\n')
print('Evidence: '+str(out), flush=True)
