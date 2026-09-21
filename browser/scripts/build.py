"""Build the real Vox bytecode compiler and its 63-bit WebAssembly runtime."""
import argparse
import os
from pathlib import Path
import shutil
import subprocess

root = Path(__file__).resolve().parents[2]
browser = root / 'browser'
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--skip-native', action='store_true', help='Reuse an up-to-date native installation')
args = parser.parse_args()

def copy(src, dest):
    if dest.exists():
        dest.chmod(dest.stat().st_mode | 0o200)
    shutil.copy2(src, dest)

def run(*command, cwd=root, env=None):
    print('+', ' '.join(map(str, command)), flush=True)
    subprocess.run(list(map(str, command)), cwd=cwd, env=env, check=True)

emcc = shutil.which(os.environ.get('EMCC', 'emcc'))
if not emcc:
    raise SystemExit('Install Emscripten 6.0.9, activate its environment, or set EMCC to its emcc executable.')
emscripten = Path(emcc).parent
if not args.skip_native:
    if not (root / 'configure').exists():
        run('autoconf')
    run('./configure', f'--prefix={root / "_install"}')
    run('make', '-s', 'install')

build = browser / '.build'
source = browser / '.runtime-source'
build.mkdir(exist_ok=True)
source.mkdir(exist_ok=True)
(source / 'package.json').write_text('{"type":"commonjs"}\n')
(browser / 'public/assets').mkdir(parents=True, exist_ok=True)
# Copy tracked working files, including uncommitted runtime portability changes.
files = subprocess.check_output(['git', 'ls-files', '-z'], cwd=root).decode().split('\0')
for filename in files:
    if not filename or filename.startswith('browser/'):
        continue
    src = root / filename
    if not src.is_file():
        continue
    dest = source / filename
    dest.parent.mkdir(parents=True, exist_ok=True)
    copy(src, dest)
copy(root / 'configure', source / 'configure')
env = dict(os.environ, CC=f'{emcc} -sMEMORY64=2', AR=str(emscripten / 'emar'),
           RANLIB=str(emscripten / 'emranlib'), ac_cv_func_sigaction='no',
           ac_cv_func_sigprocmask='no')
run('./configure', '--host=wasm64-unknown-emscripten', f'--prefix={source / "_install"}',
    '--disable-native-compiler', '--disable-multidomain', '--disable-systhreads',
    '--disable-shared', '--disable-debug-runtime', '--disable-instrumented-runtime',
    '--disable-ocamldoc', '--disable-ocamltest', '--disable-ocamldebug', cwd=source, env=env)
for name in ['primitives', 'prims.c']:
    copy(root / '_build/main/runtime' / name, source / 'runtime' / name)
for header in (root / '_build/main/runtime/caml').glob('*.h'):
    dest = source / 'runtime/caml' / header.name
    if not dest.exists():
        copy(header, dest)
run('make', '-f', 'Makefile.upstream', '-j8', 'runtime/libcamlrun.a',
    'COMPUTE_DEPS=false', 'SAK_CC=cc', 'SAK_CFLAGS=-O2 -Iruntime',
    'SAK_LINK=cc -o $(1) $(2)', cwd=source, env=env)
cc = root / '_install/bin/ocamlc'
includes = ['-I', str(root / '_install/lib/ocaml/compiler-libs'), '-I', str(build)]
for name in ['vox_verify.mli', 'vox_verify.ml', 'browser_main.ml']:
    copy(browser / 'compiler' / name, build / name)
    run(cc, *includes, '-c', build / name)
run(cc, '-noautolink', '-no-check-prims', *includes, '-o', build / 'compiler.byte',
    root / '_build/main/middle_end/flambda2/numbers/floats/flambda2_floats.cma',
    'ocamlcommon.cma', 'ocamlfrontend.cma', 'vox_smt.cma', 'vox_vc.cma',
    build / 'vox_verify.cmo', 'ocamlbytecomp.cma', build / 'browser_main.cmo')
run('python3', browser / 'scripts/primitives.py')
run('sh', browser / 'scripts/link-runtime.sh', env=dict(os.environ, EMCC=emcc))
run('python3', browser / 'scripts/package-assets.py')
run('npm', 'run', 'build:workers', cwd=browser)
