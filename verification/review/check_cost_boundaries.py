#!/usr/bin/env python3
"""Compile cost demos without modifying the supplied compiler installation."""
import pathlib, shutil, subprocess, sys, re, json
root = pathlib.Path(__file__).resolve().parents[2]
prefix = pathlib.Path(sys.argv[1]).resolve()
out = root / '_build/cost-boundary-check'
lib = out / 'library'
lib.mkdir(parents=True, exist_ok=True)
modules = '''vox_sequence vox_credits vox_ordered_sequence vox_merge_proofs
vox_sort_cost vox_merge_sort pref ghost_pref vox_big_credits vox_ackermann
vox_union_find_potential vox_union_find_levels vox_union_find_path_cost
vox_union_find_model vox_union_find_forest vox_union_find_rank vox_union_find_mass
vox_union_find_link vox_union_find_worker vox_union_find_amortized vox_union_find_bank
vox_union_find_spec vox_union_find vox_union_find_complexity vox_union_find_simple
vox_union_find_online vox_connectivity vox_union_find_online_cost'''.split()
flags = ['-nostdlib', '-I', str(prefix / 'lib/ocaml'), '-I', '.', '-extension', 'refinement_types', '-principal']
def run(args, cwd, log=None, reject=None):
    p = subprocess.run([str(x) for x in args], cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    if log:
        (out / log).write_text(p.stdout)
    if reject:
        assert p.returncode != 0 and reject.replace('unboxed ', '').replace(chr(34), '') in p.stdout.replace('unboxed ', '').replace(chr(34), ''), p.stdout
    elif p.returncode:
        raise RuntimeError(p.stdout)
    return p.stdout
for module in modules:
    for ext in ['.mli', '.ml']:
        source = root / 'verification/library' / (module + ext)
        if source.exists():
            shutil.copy(source, lib)
            run([prefix/'bin/ocamlc', *flags, '-c', source.name], lib)
    run([prefix/'bin/ocamlopt', *flags, '-dlambda', '-c', module+'.ml'], lib, module+'.lambda')
print('Fresh bytecode/native module verification passed', flush=True)
public = {
 'merge_sort': ['vox_sequence', 'vox_credits', 'vox_sort_cost', 'vox_merge_sort'],
 'connectivity': ['vox_big_credits', 'vox_ackermann', 'vox_connectivity', 'vox_union_find_online_cost'],
}
for client, visible in public.items():
    directory = out / client
    directory.mkdir(exist_ok=True)
    for module in visible:
        shutil.copy(lib/(module+'.cmi'), directory)
    shutil.copy(root/'testsuite/tests/vox'/(client+'.ml'), directory)
    for compiler, extension in [('ocamlc', '.cmo'), ('ocamlopt', '.cmx')]:
        run([prefix/'bin'/compiler, *flags, '-dlambda', '-c', client+'.ml'], directory, client+'.'+compiler+'.lambda')
        executable = directory / (client+'.'+compiler)
        run([prefix/'bin'/compiler, *flags, '-o', executable, *[lib/(m+extension) for m in modules], client+extension], directory)
        result = run([prefix/'bin/ocamlrun', executable] if compiler == 'ocamlc' else [executable], directory)
        (out/(client+'.'+compiler+'.output')).write_text(result)
    print(client+': isolated public-only client passed in bytecode/native', flush=True)
for client in ['union_find_online', 'time_credits']:
    shutil.copy(root/'testsuite/tests/vox'/(client+'.ml'), lib)
    for compiler, extension in [('ocamlc', '.cmo'), ('ocamlopt', '.cmx')]:
        run([prefix/'bin'/compiler, *flags, '-c', client+'.ml'], lib)
        executable = lib/(client+'.'+compiler)
        run([prefix/'bin'/compiler, *flags, '-o', executable,
             *[lib/(m+extension) for m in modules], client+extension], lib)
        result = run([prefix/'bin/ocamlrun', executable] if compiler == 'ocamlc' else [executable], lib)
        (out/(client+'.'+compiler+'.output')).write_text(result)
    print(client+': existing regression passed in bytecode/native', flush=True)
for fixture, directory in [
    ('merge_sort_rejected', out/'merge_sort'),
    ('connectivity_rejected', out/'connectivity'),
    ('time_credits_rejected', out/'merge_sort'),
    ('union_find_online_rejected', lib),
]:
    source = (root/'testsuite/tests/vox'/(fixture+'.ml')).read_text().split('*)', 1)[1]
    prelude = ''
    count = 0
    for code, expected in re.findall(r'(.*?)\[%%expect\{\|(.*?)\|\}\]', source, re.S):
        test = directory / (fixture+'_case.ml')
        test.write_text(prelude+code)
        for compiler in ['ocamlc', 'ocamlopt']:
            marker = None
            if 'Error:' in expected:
                marker = expected.split('Error:', 1)[1].strip().splitlines()[0]
            run([prefix/'bin'/compiler, *flags, '-c', test.name], directory,
                fixture+'.'+str(count)+'.'+compiler+'.log', reject=marker)
        if 'Error:' not in expected:
            prelude += code
        else:
            count += 1
    print(f'{fixture}: {count} expected rejections in bytecode/native', flush=True)
hidden = out/'merge_sort'/'hidden_sort_proof.ml'
hidden.write_text('open Merge_sort\nlet hidden = Sort.P.count_nonnegative\n')
run([prefix/'bin/ocamlc', *flags, '-c', hidden.name], hidden.parent,
    'hidden_sort_proof.log', reject='Unbound value Sort.P.count_nonnegative')
erasure = {}
for module, names in {
    'vox_merge_sort': ['sort', 'merge', 'split'],
    'vox_union_find_online': ['create', 'make_set', 'find', 'union'],
    'vox_union_find': ['create', 'make_set', 'find', 'union', 'link'],
    'vox_union_find_worker': ['allocate', 'find', 'link'],
}.items():
    text = (out/(module+'.lambda')).read_text()
    for name in names:
        match = re.search(r'\b'+name+r'/\d+(?:\s*=)?\s*\(function', text)
        assert match, (module, name)
        start = text.index('(function', match.start())
        depth = 0
        end = start
        for end in range(start, len(text)):
            depth += (text[end] == '(') - (text[end] == ')')
            if depth == 0:
                break
        body = text[start:end+1]
        for forbidden in ['caml_bigint_', '(global Vox_ackermann!',
                          '(global Vox_union_find_forest!', '(global Vox_sort_cost!',
                          '(apply snapshot/', '(apply added_law/', '(apply joined_law/']:
            assert forbidden not in body, (module, name, forbidden)
        erasure[module+'.'+name] = 'No runtime hierarchy, model, budget or transition-proof calls'
(out/'erasure.json').write_text(json.dumps(erasure, indent=2)+'\n')
print('Focused execution, rejection and emitted-operation erasure checks passed', flush=True)
print('Evidence in '+str(out), flush=True)
