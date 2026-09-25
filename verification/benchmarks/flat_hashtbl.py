#!/usr/bin/env python3
"""Benchmark the verified flat table using the make-install compiler."""
import argparse
import contextlib
import hashlib
import json
import os
import platform
import shlex
from pathlib import Path
import shutil
import subprocess
import sys
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
    'vox_table_resize', 'vox_table_implementation', 'vox_verified_flat_hashtbl',
]



def prepare_base(directory, compiler):
    """Build released Base with OxCaml; only adapt dependency compatibility."""
    directory = directory.resolve()
    directory.mkdir(parents=True, exist_ok=True)
    for name, version in [('base', 'v0.17.3'), ('sexplib0', 'v0.17.0'),
                          ('ocaml_intrinsics_kernel', 'v0.17.1')]:
        if not (directory / name).exists():
            subprocess.run(['opam', 'source', name + '.' + version,
                            '--dir=' + str(directory / name)],
                           stdout=sys.stderr, check=True)

    def replace(path, before, after):
        target = directory / path
        source = target.read_text()
        if after in source:
            return
        if before not in source:
            raise RuntimeError('unexpected dependency source: ' + path)
        target.write_text(source.replace(before, after))

    # The upstream discover program also emits () on macOS. Elsewhere it
    # enables popcnt only on x86_64; leave discovery intact on those hosts.
    if platform.system() == 'Darwin':
        replace('base/src/dune', '(run ./discover/discover.exe -o %{targets})',
                '(write-file %{targets} "()")')
    replace('base/shadow-stdlib/gen/dune', 'compiler-libs.common',
            'compiler-libs.frontend')
    replace('base/shadow-stdlib/gen/gen.ml',
            'Printtyp.signature cmi.Cmi_format.cmi_sign',
            'Printtyp.signature (fst cmi.Cmi_format.cmi_sign)')
    replace('base/shadow-stdlib/gen/gen.ml',
            'let repl = Mapper.line (Lexing.from_string line) in',
            '''let line = Str.global_replace (Str.regexp " : value mod [a-z ]+") "" line in
      let line = Str.global_replace (Str.regexp_string "('a : value_or_null)") "'a" line in
      let repl = Mapper.line (Lexing.from_string line) in''')
    for name in ['int32', 'int64', 'nativeint']:
        path = 'ocaml_intrinsics_kernel/src/' + name + '.ml'
        for op in ['clz', 'ctz']:
            target = directory / path
            target.write_text(target.read_text().replace(
                '_' + op + '_nonzero_unboxed_to_untagged',
                '_' + op + '_unboxed_to_untagged'))
    for suffix in ['ml', 'mli']:
        path = directory / ('ocaml_intrinsics_kernel/src/float.' + suffix)
        path.write_text(path.read_text().replace('[@@builtin]', ''))
    for name, parameters in [('iter', 'f t'), ('fold', 'f x t')]:
        replace('base/src/linked_queue0.ml', '(Stdlib.Queue.' + name + ' :',
                '((fun ' + parameters + ' -> Stdlib.Queue.' + name +
                ' ' + parameters + ') :')
    replace('base/src/buffer.ml', '(Stdlib.Buffer.blit :',
            '((fun b i d j n -> Stdlib.Buffer.blit b i d j n) :')
    wrappers = ['add_string', 'add_bytes', 'add_buffer', 'add_char',
                'add_utf_8_uchar', 'add_utf_16le_uchar', 'add_utf_16be_uchar']
    replace('base/src/buffer.ml', 'include Stdlib.Buffer',
            'include Stdlib.Buffer\n' + '\n'.join(
                'let ' + name + ' b x = Stdlib.Buffer.' + name + ' b x'
                for name in wrappers))
    (directory / 'dune-project').write_text('(lang dune 3.23)\n')
    (directory / 'dune-workspace').write_text(f'''(lang dune 3.23)
(context (default (name vox) (profile release)
 (paths (PATH ("{compiler.parent}" :standard))
        (OCAMLLIB ("{compiler.parent.parent / 'lib/ocaml'}")))
 (env (_ (ocamlopt_flags (:standard -O3))))))
''')
    subprocess.run(['dune', 'build', '--root=' + str(directory),
                    'base/src/base.cmxa',
                    'base/hash_types/src/base_internalhash_types.cmxa',
                    'base/shadow-stdlib/src/shadow_stdlib.cmxa',
                    'sexplib0/src/sexplib0.cmxa',
                    'ocaml_intrinsics_kernel/src/ocaml_intrinsics_kernel.cmxa',
                    'base/src/libbase_stubs.a',
                    'base/hash_types/src/libbase_internalhash_types_stubs.a',
                    'ocaml_intrinsics_kernel/src/libocaml_intrinsics_kernel_stubs.a'],
                   stdout=sys.stderr, check=True)
    return directory / '_build/vox'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--sizes', nargs='+', type=int,
                        default=[1, 8, 16, 64, 1024, 57344, 65536, 917504, 1048576])
    parser.add_argument('--work-factor', type=int, default=10)
    parser.add_argument('--repeats', type=int, default=3)
    parser.add_argument('--build-dir', type=Path)
    parser.add_argument('--library-dir', type=Path,
                        default=ROOT / 'verification/library')
    parser.add_argument('--prepare-base', type=Path,
                        help='download and build Base in this directory')
    parser.add_argument('--base-build', type=Path,
                        help='Dune context directory containing Base v0.17.3')
    parser.add_argument('--implementations', nargs='+',
                        choices=['stdlib', 'simd', 'scalar', 'base'],
                        default=['stdlib', 'simd', 'scalar'])
    args = parser.parse_args()
    if args.work_factor < 1 or args.repeats < 1 or any(n <= 0 for n in args.sizes):
        parser.error('sizes and repeats must be positive')
    if 'base' in args.implementations and not (args.base_build or args.prepare_base):
        parser.error('base requires --base-build')
    compiler = ROOT / '_install/bin/ocamlopt'
    if args.prepare_base:
        args.base_build = prepare_base(args.prepare_base, compiler)
    c_compiler = subprocess.check_output(
        [str(compiler), '-config-var', 'c_compiler'], text=True).strip()
    c_version = subprocess.check_output(
        [*shlex.split(c_compiler), '--version'], text=True)
    scalar_flags = (['-fno-vectorize', '-fno-slp-vectorize']
                    if 'clang' in c_version.lower() else
                    ['-fno-tree-vectorize', '-fno-tree-slp-vectorize'])
    print('# ' + json.dumps({'machine': platform.machine(),
          'platform': platform.platform(),
          'cpu': (subprocess.check_output(
              ['sysctl', '-n', 'machdep.cpu.brand_string'], text=True).strip()
              if platform.system() == 'Darwin' else platform.processor()),
          'ocamlrunparam': os.environ.get('OCAMLRUNPARAM', ''),
          'compiler_version': subprocess.check_output(
              [str(compiler), '-version'], text=True).strip(),
          'compiler': str(compiler), 'flags': ['-O3'],
          'c_compiler': c_compiler, 'scalar_flags': scalar_flags,
          'initial_capacity': 16, 'work_factor': args.work_factor,
          'query_order': 'shuffled',
          'library_dir': str(args.library_dir),
          'base': ({'version': 'v0.17.3',
                    'build': str(args.base_build),
                    'hashtbl_sha256': hashlib.sha256((args.base_build /
                        'base/src/hashtbl.ml').read_bytes()).hexdigest()}
                   if 'base' in args.implementations else None),
          'sources': {p: hashlib.sha256((
              args.library_dir / Path(p).name if p.startswith('verification/library/')
              else ROOT / p).read_bytes()).hexdigest()
                      for p in ['backend/cmm_builtins.ml', 'runtime/vox_control.c', 'runtime/pref.c',
                                'verification/benchmarks/verified_flat_hashtbl.ml'] +
                      ['verification/library/' + m + '.ml' for m in MODULES]},
          'revision': subprocess.check_output(['git', 'rev-parse', 'HEAD'],
                                             cwd=ROOT, text=True).strip()}))
    if args.build_dir:
        args.build_dir.mkdir(parents=True, exist_ok=True)
    context = (contextlib.nullcontext(args.build_dir.resolve()) if args.build_dir
               else tempfile.TemporaryDirectory(prefix='vox-flat-'))
    with context as directory:
        directory = Path(directory)
        executables = {}
        backends = ['simd'] + (['scalar'] if 'scalar' in args.implementations else [])
        for backend in backends:
            target = directory if backend == 'simd' else directory / 'scalar'
            target.mkdir(parents=True, exist_ok=True)
            for module in MODULES:
                for suffix in ['.mli', '.ml']:
                    source = args.library_dir / (module + suffix)
                    if not source.exists():
                        continue
                    contents = source.read_text()
                    if backend == 'scalar' and module == 'vox_table_storage':
                        for name in ['match16', 'match16_empty']:
                            marker = 'external ' + name + ' :'
                            if marker not in contents:
                                continue
                            start = contents.index(marker)
                            end = contents.find('\nexternal ', start + 1)
                            if end < 0:
                                end = len(contents)
                            contents = (contents[:start] +
                                contents[start:end].replace(' [@@builtin]', '') +
                                contents[end:])
                    (target / (module + suffix)).write_text(contents)
                    subprocess.run([str(compiler), '-O3', '-S', '-extension',
                                    'refinement_types', '-c', module + suffix],
                                   cwd=target, check=True)
            (target / 'measure.ml').write_text(PROGRAM)
            subprocess.run([str(compiler), '-O3', '-S', '-extension',
                            'refinement_types', '-c', 'measure.ml'],
                           cwd=target, check=True)
            extra = []
            if backend == 'scalar':
                shutil.copy(ROOT / 'runtime/vox_control.c',
                            target / 'scalar_control.c')
                subprocess.run([str(compiler), '-ccopt', '-O3',
                                '-ccopt', '-DVOX_CONTROL_SCALAR',
                                *[arg for flag in scalar_flags
                                  for arg in ['-ccopt', flag]],
                                '-c', 'scalar_control.c'],
                               cwd=target, check=True)
                extra = ['scalar_control.o']
            exe = target / ('measure-' + backend)
            subprocess.run([str(compiler), '-O3',
                            *[module + '.cmx' for module in MODULES],
                            'measure.cmx', *extra, '-o', str(exe)],
                           cwd=target, check=True)
            executables[backend] = exe
        if 'base' in args.implementations:
            base = args.base_build.resolve()
            dependencies = [
                ('sexplib0/src', 'sexplib0'),
                ('ocaml_intrinsics_kernel/src', 'ocaml_intrinsics_kernel'),
                ('base/hash_types/src', 'base_internalhash_types'),
                ('base/shadow-stdlib/src', 'shadow_stdlib'),
                ('base/src', 'base'),
            ]
            includes, archives = [], []
            for path, name in dependencies:
                folder = base / path
                for sub in [folder, folder / ('.' + name + '.objs/byte'),
                            folder / ('.' + name + '.objs/native')]:
                    includes += ['-I', str(sub)]
                archives.append(str(folder / (name + '.cmxa')))
            adapter = """
module Standard = struct
  module K = struct
    type t = int
    let hash = Key.hash
    let compare = Int.compare
    let sexp_of_t = Base.Int.sexp_of_t
  end
  let create n = Base.Hashtbl.create ~size:n (module K)
  let replace t k v = Base.Hashtbl.set t ~key:k ~data:v
  let find t k = Base.Hashtbl.find_exn t k
  let mem t k = Base.Hashtbl.mem t k
  let remove t k = Base.Hashtbl.remove t k
end
"""
            source = (PROGRAM[:PROGRAM.index('module V =')] + adapter +
                      PROGRAM[PROGRAM.index('let work_factor'):PROGRAM.index('let rec fill')] +
                      PROGRAM[PROGRAM.index('let standard'):PROGRAM.index('let () =')] +
                      """let () =
  let n = int_of_string Sys.argv.(2) in
  let ints = Iarray.init n Fun.id in
  let strings = Iarray.init n string_of_int in
  standard "int" ints;
  standard "string" strings
""").replace('"stdlib"', '"base"')
            (directory / 'measure_base.ml').write_text(source)
            exe = directory / 'measure-base'
            subprocess.run([str(compiler), '-O3', '-S', '-extension',
                            'refinement_types', *includes, *archives,
                            'measure_base.ml', '-o', str(exe)],
                           cwd=directory, check=True)
            executables['base'] = exe
        print('repeat,implementation,payload,entries,operation,'
              'ns_per_op,bytes_per_op',
              flush=True)
        for repeat in range(args.repeats):
            order = list(args.implementations)
            if repeat % 2:
                order.reverse()
            for n in args.sizes:
                for name in order:
                    exe = executables[name if name in ('base', 'scalar') else 'simd']
                    result = subprocess.check_output([str(exe), name, str(n), str(args.work_factor)],
                                                     text=True, timeout=300)
                    for line in result.splitlines():
                        print(f'{repeat},{line}', flush=True)


if __name__ == '__main__':
    main()
