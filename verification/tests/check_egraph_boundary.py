#!/usr/bin/env python3
"""Check an e-graph build using an existing compiler and fresh public-only CMIs."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--compiler', required=True, type=Path)
parser.add_argument('--stdlib', required=True, type=Path)
parser.add_argument('--compiled', type=Path,
                    help='optional complete directory of freshly compiled dependency CMIs')
parser.add_argument('--output', type=Path)
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
inventory = json.loads((root / 'verification/library/vox_egraph_rule_handle.spec.json').read_text())
for entry in inventory['files'] + inventory['primitive_code']:
    source = (root / entry['path']).read_text()
    if hashlib.sha256(source.encode()).hexdigest() != entry['sha256']:
        raise SystemExit(f"Stale specification inventory: {entry['path']}")
    for declaration in entry['declarations']:
        lines = source.splitlines()[declaration['start_line'] - 1:declaration['end_line']]
        if lines != declaration['code'].splitlines():
            raise SystemExit(f"Stale declaration: {entry['path']}:{declaration['name']}")
print('Exact semantic inventory: passed', flush=True)
out = (args.output or Path(tempfile.mkdtemp(prefix='vox-egraph-boundary-'))).resolve()
out.mkdir(parents=True, exist_ok=True)
public = out / 'public'
public.mkdir(exist_ok=True)
modules = [
    'language_spec', 'rule_spec', 'derivation_spec', 'match_spec',
    'snapshot_spec', 'preservation_spec', 'saturation_spec',
    'congruence_spec', 'fixedpoint_spec', 'interpret_wrapping', 'rule_handle',
]
base = [str(args.compiler.resolve()), '-nostdlib', '-I', str(args.stdlib.resolve()),
        '-extension', 'refinement_types']

def compile_file(source, directory, *, include, extra=()):
    result = subprocess.run(base + ['-I', str(include), *extra, '-c', str(source),
                            '-o', str(directory / (source.stem + '.cmo'))],
                            text=True, capture_output=True, cwd=directory)
    (directory / (source.stem + '.log')).write_text(result.stdout + result.stderr)
    return result

client = root / 'testsuite/tests/vox/egraph_rule_public.ml'
if args.compiled is None:
    args.compiled = out / 'compiled'
    args.compiled.mkdir(exist_ok=True)
    files = re.search(r'all_modules = "([^"]+)"', client.read_text()).group(1).split()
    for file in files[:-1]:
        source = root / 'verification/library' / file
        target = args.compiled / (source.stem + ('.cmi' if source.suffix == '.mli' else '.cmo'))
        result = subprocess.run(base + ['-I', str(args.compiled.resolve()), '-c', str(source),
                                       '-o', str(target.resolve())], text=True, capture_output=True)
        if result.returncode:
            raise SystemExit(result.stdout + result.stderr)
    print('Fresh dependency verification: passed', flush=True)
for name in modules:
    file = f'vox_egraph_{name}.cmi'
    shutil.copy2(args.compiled / file, public / file)
result = compile_file(client, public, include=public)
if result.returncode:
    raise SystemExit(result.stdout + result.stderr)
print('Public-only client: passed', flush=True)
for name, expected in [
    ('abstract_state', 'Vox_egraph_rule_handle.t'),
    ('invalid_rules', 'Refinement could not be proved'),
    ('limit_is_not_fixed', 'Refinement could not be proved'),
]:
    result = compile_file(root / f'verification/tests/egraph_boundary/{name}.ml',
                          public, include=public)
    diagnostic = result.stdout + result.stderr
    if not result.returncode or expected not in diagnostic:
        raise SystemExit(f'Unexpected rejection result for {name}:\n{diagnostic}')
    print(f'Rejection {name}: passed', flush=True)

# A fresh output directory avoids overwriting the verified build's artifacts.
lambda_dir = out / 'lambda'
lambda_dir.mkdir(exist_ok=True)
for name in ['match_subst', 'rule_rewrite', 'rule_saturate', 'model_evidence', 'rule_handle']:
    source = root / f'verification/library/vox_egraph_{name}.ml'
    cmi = args.compiled / (source.stem + '.cmi')
    shutil.copy2(cmi, lambda_dir / cmi.name)
    result = compile_file(source, lambda_dir, include=args.compiled.resolve(),
                          extra=('-dlambda',))
    if result.returncode:
        raise SystemExit(result.stdout + result.stderr)
    emitted = result.stderr
    (lambda_dir / (source.stem + '.lambda')).write_text(emitted)
    forbidden = r'\(apply\s+\(field_imm \d+ \(global Vox_egraph_(?:derivation|match_evidence|match_observation|snapshot_proof|preservation_proof)\b'
    if re.search(forbidden, emitted, re.I):
        raise SystemExit(f'Runtime proof call in {name}; inspect emitted Lambda')
    print(f'Emitted proof-call check {name}: passed', flush=True)
print(f'Evidence: {out}')
