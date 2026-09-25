#!/usr/bin/env python3
"""Compile the public boundary and an isolated client; retain erasure dumps."""
import argparse
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output', type=Path)
parser.add_argument('--compiler', type=Path)
args = parser.parse_args()
root = Path(__file__).resolve().parents[3]
source = Path(__file__).resolve().parent
output = args.output or Path(tempfile.mkdtemp(prefix='vox-dfa-boundary-'))
output.mkdir(parents=True, exist_ok=True)
output = output.resolve()
private = output / 'implementation'
public = output / 'public'
private.mkdir(exist_ok=True)
public.mkdir(exist_ok=True)
compiler = (args.compiler or root / '_install/bin/ocamlopt.opt').resolve()
flags = ['-opaque', '-principal', '-extension', 'refinement_types']


def compile_file(path, destination, includes, opens=(), dump=False, reject=None):
    cmd = [str(compiler), *flags]
    for directory in includes:
        cmd += ['-I', str(directory)]
    for module in opens:
        cmd += ['-open', module]
    if dump:
        cmd += ['-drawlambda', '-dcanonical-ids']
    cmd += ['-c', str(path), '-o', str(destination)]
    result = subprocess.run(cmd, cwd=output, text=True, capture_output=True)
    log = output / (path.stem + ('.lambda' if dump else '.log'))
    log.write_text(result.stdout + result.stderr)
    if reject:
        if result.returncode == 0 or reject not in result.stderr:
            raise RuntimeError(f'Expected rejection {reject!r}: {log}')
    elif result.returncode:
        raise RuntimeError(f'Compilation failed: {log}')
    return result.stderr


units = [
    ('dfa_semantics', ()),
    ('regex_semantics', ()),
    ('dfa_equivalence_proof', ('Dfa_semantics',)),
    ('regex_core', ('Regex_semantics',)),
    ('regex_dfa_bridge_core', ('Dfa_semantics', 'Dfa_equivalence_proof',
                               'Regex_semantics', 'Regex_core')),
    ('dfa_equivalence_core', ('Dfa_semantics', 'Dfa_equivalence_proof')),
    ('regex_language', ('Dfa_semantics', 'Dfa_equivalence_proof',
                        'Regex_semantics', 'Regex_core', 'Regex_dfa_bridge_core')),
]
dumps = {}
for unit, opens in units:
    interface = source / (unit + '.mli')
    if interface.exists():
        semantic_opens = tuple(name for name in opens
                               if name in ('Dfa_semantics', 'Regex_semantics'))
        compile_file(interface, private / (unit + '.cmi'), [private], semantic_opens)
    dumps[unit] = compile_file(source / (unit + '.ml'), private / (unit + '.cmx'),
                               [private], opens, dump=True)

for unit in ['dfa_semantics', 'regex_semantics', 'dfa_equivalence_core', 'regex_language']:
    shutil.copyfile(private / (unit + '.cmi'), public / (unit + '.cmi'))
compile_file(source / 'dfa_public_client.ml', public / 'dfa_public_client.cmx', [public], dump=True)

executable = output / 'public_client.exe'
subprocess.run([str(compiler), '-I', str(private),
                *(str(private / (unit + '.cmx')) for unit, _ in units),
                str(public / 'dfa_public_client.cmx'), '-o', str(executable)],
               cwd=output, check=True)
subprocess.run([str(executable)], cwd=output, check=True)

hidden = output / 'hidden_certificate.ml'
hidden.write_text('open Dfa_equivalence_core\nlet hidden = Dfa_equivalence.check_reduction\n')
compile_file(hidden, public / 'hidden_certificate.cmx', [public], reject='Unbound value')
hidden_module = output / 'hidden_proof_module.ml'
hidden_module.write_text('module Hidden = Dfa_equivalence_proof.Dfa_proof\n')
compile_file(hidden_module, public / 'hidden_proof_module.cmx', [public],
             reject='Unbound module')
false_claim = output / 'false_equality.ml'
false_claim.write_text('''open Dfa_semantics
let (false_equality @ total) (left : Dfa_semantics.machine)
    (right : Dfa_semantics.machine) (word : int list) :
    {u : unit | Dfa_semantics.run left word === Dfa_semantics.run right word} =
  let u = () in u
''')
compile_file(false_claim, public / 'false_equality.cmx', [public],
             reject='Refinement could not be proved')


def function_bodies(dump):
    bodies = {}
    for match in re.finditer(r'\b(\w+)/\d+\s*(?:=\s*)?\(function', dump):
        start = dump.index('(function', match.start())
        depth = 0
        for end in range(start, len(dump)):
            depth += (dump[end] == '(') - (dump[end] == ')')
            if depth == 0:
                break
        bodies[match[1]] = dump[start:end + 1]
    return bodies


bodies = function_bodies(dumps['dfa_equivalence_proof'])
forbidden = {'append_word', 'quotient_relation', 'quotient_access', 'cover_rows',
             'copy_access', 'copy_separations', 'same_class_pairs', 'candidate',
             'search_product'}
for entry in ['compare', 'reduce']:
    pending, visited = [entry], set()
    while pending:
        name = pending.pop()
        if name in visited:
            continue
        visited.add(name)
        if name not in bodies:
            continue
        pending.extend(re.findall(r'\(apply\s+(\w+)/\d+', bodies[name]))
    if entry not in bodies or visited & forbidden:
        raise RuntimeError(f'Unexpected ordinary call graph: {entry}: {visited}')
    (output / (entry + '-calls.txt')).write_text('\n'.join(sorted(visited)) + '\n')
for unit, names in [
    ('dfa_equivalence_core', ['compare_complete', 'compare_equal', 'comparison_witness',
                              'reduce_complete', 'reduce_preserves', 'reduce_minimum']),
    ('regex_language', ['sound', 'complete', 'lower_matches']),
]:
    functions = function_bodies(dumps[unit])
    for name in names:
        if name not in functions or re.search(r'\(apply\b', functions[name]):
            raise RuntimeError(f'Runtime call in public proof: {unit}.{name}')
print(f'Public client, rejection checks and DFA erasure call graphs passed: {output}')
print('Lambda dumps retain indirect semantic calls for manual inspection.')
