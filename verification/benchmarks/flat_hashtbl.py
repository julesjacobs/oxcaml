#!/usr/bin/env python3
"""Benchmark the verified flat table using the make-install compiler."""
import argparse
import hashlib
import json
import platform
import shlex
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
PROGRAM = (ROOT / 'verification/benchmarks/verified_flat_hashtbl.ml').read_text()
MODULES = [
    'vox_sequence', 'vox_table_model', 'vox_table_model_proofs',
    'vox_table_bits', 'vox_table_probe', 'vox_table_wrap', 'vox_table_mask',
    'vox_table_map', 'vox_table_invariant', 'vox_table_initial',
    'vox_table_update_proofs', 'vox_table_insert_proofs',
    'vox_table_migration_proofs', 'vox_table_read_proofs',
    'vox_table_search_spec', 'vox_table_stop_proof', 'pref', 'ghost_pref',
    'vox_table_storage', 'vox_table_search', 'vox_table_mutation',
    'vox_table_coverage', 'vox_table_occupancy', 'vox_table_vacancy_progress',
    'vox_table_vacancy', 'vox_table_insert', 'vox_table_migrate',
    'vox_table_resize', 'vox_verified_flat_hashtbl',
]



def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--sizes', nargs='+', type=int,
                        default=[1024, 57344, 65536, 917504, 1048576])
    parser.add_argument('--repeats', type=int, default=3)
    args = parser.parse_args()
    if args.repeats < 1 or any(n <= 0 for n in args.sizes):
        parser.error('sizes and repeats must be positive')
    compiler = ROOT / '_install/bin/ocamlopt'
    c_compiler = subprocess.check_output(
        [str(compiler), '-config-var', 'c_compiler'], text=True).strip()
    c_version = subprocess.check_output(
        [*shlex.split(c_compiler), '--version'], text=True)
    scalar_flags = (['-fno-vectorize', '-fno-slp-vectorize']
                    if 'clang' in c_version.lower() else
                    ['-fno-tree-vectorize', '-fno-tree-slp-vectorize'])
    print('# ' + json.dumps({'machine': platform.machine(),
          'platform': platform.platform(),
          'compiler_version': subprocess.check_output(
              [str(compiler), '-version'], text=True).strip(),
          'compiler': str(compiler), 'flags': ['-O3'],
          'c_compiler': c_compiler, 'scalar_flags': scalar_flags,
          'initial_capacity': 16,
          'sources': {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest()
                      for p in ['runtime/vox_control.c', 'runtime/pref.c',
                                'verification/benchmarks/verified_flat_hashtbl.ml'] +
                      ['verification/library/' + m + '.ml' for m in MODULES]},
          'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'],
                                             cwd=ROOT, text=True).strip()}))
    with tempfile.TemporaryDirectory(prefix='vox-flat-') as directory:
        directory = Path(directory)
        executables = {}
        for module in MODULES:
            for suffix in ['.mli', '.ml']:
                source = ROOT / 'verification/library' / (module + suffix)
                if source.exists():
                    shutil.copy(source, directory)
                    subprocess.run([str(compiler), '-O3', '-extension',
                                    'refinement_types', '-c', module + suffix],
                                   cwd=directory, check=True)
        (directory / 'measure.ml').write_text(PROGRAM)
        subprocess.run([str(compiler), '-O3', '-extension', 'refinement_types',
                        '-c', 'measure.ml'], cwd=directory, check=True)
        for backend in ['simd', 'scalar']:
            extra = []
            if backend == 'scalar':
                shutil.copy(ROOT / 'runtime/vox_control.c',
                            directory / 'scalar_control.c')
                subprocess.run([str(compiler), '-ccopt', '-O3',
                                '-ccopt', '-DVOX_CONTROL_SCALAR',
                                *[arg for flag in scalar_flags
                                  for arg in ['-ccopt', flag]],
                                '-c', 'scalar_control.c'],
                               cwd=directory, check=True)
                extra = ['scalar_control.o']
            exe = directory / ('measure-' + backend)
            subprocess.run([str(compiler), '-O3',
                            *[module + '.cmx' for module in MODULES],
                            'measure.cmx', *extra, '-o', str(exe)],
                           cwd=directory, check=True)
            executables[backend] = exe
        print('repeat,implementation,payload,entries,operation,'
              'ns_per_op,bytes_per_op',
              flush=True)
        for repeat in range(args.repeats):
            order = ['stdlib', 'simd', 'scalar']
            if repeat % 2:
                order.reverse()
            for n in args.sizes:
                for name in order:
                    exe = executables['scalar' if name == 'scalar' else 'simd']
                    result = subprocess.check_output([str(exe), name, str(n)],
                                                     text=True, timeout=300)
                    for line in result.splitlines():
                        print(f'{repeat},{line}', flush=True)


if __name__ == '__main__':
    main()
