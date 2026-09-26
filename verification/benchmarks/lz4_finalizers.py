"""Check explicit release, GC liveness and OOM recovery in both codec paths."""
from pathlib import Path
import shutil
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
prefix = root / '_install'
library = root / '_build/vox-library'
fixtures = root / 'testsuite/tests/vox'
flags = ['-nostdlib', '-I', str(prefix / 'lib/ocaml'), '-I', str(library),
         '-extension', 'refinement_types', '-principal', '-g']
with tempfile.TemporaryDirectory(prefix='lz4-finalizers-') as name:
    work = Path(name)
    for fixture in ['lz4_finalizer_stubs.c', 'lz4_finalizers.ml', 'raw_memory_demo.ml']:
        shutil.copy(fixtures / fixture, work)
    subprocess.run([str(prefix / 'bin/ocamlc'), '-c', 'lz4_finalizer_stubs.c'],
                   cwd=work, check=True)
    for compiler, archive in [('ocamlc', 'cma'), ('ocamlopt', 'cmxa')]:
        command = [str(prefix / 'bin' / compiler), *flags]
        extra = ['-custom'] if compiler == 'ocamlc' else ['-O3']
        subprocess.run([*command, *extra, str(library / ('vox_borrow.' + archive)),
                        'lz4_finalizer_stubs.o', 'lz4_finalizers.ml', '-o', 'finalizers'],
                       cwd=work, check=True)
        subprocess.run([str(work / 'finalizers')], cwd=work, check=True)
        subprocess.run([*command, str(library / ('vox_borrow.' + archive)),
                        'raw_memory_demo.ml', '-o', 'raw_memory'], cwd=work, check=True)
        subprocess.run([str(work / 'raw_memory')], cwd=work, check=True)
        print(compiler + ': finalizers, OOM paths and raw-memory GC liveness passed', flush=True)
