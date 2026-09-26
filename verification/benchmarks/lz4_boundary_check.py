"""Compile the public client without any codec implementation/proof CMIs."""
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
prefix = root / '_install'
library = root / '_build/vox-library'
public = ['vox_lz4', 'vox_string_view', 'vox_iarray', 'vox_sequence']
public += ['vox_lz4_spec', 'vox_lz4_spec_parse', 'vox_lz4_spec_decode_bytes',
           'vox_lz4_spec_bytes', 'vox_lz4_spec_match', 'vox_lz4_spec_plan',
           'vox_lz4_spec_token', 'vox_lz4_spec_wire', 'vox_lz4_spec_hashes',
           'vox_lz4_spec_scan']
with tempfile.TemporaryDirectory(prefix='lz4-public-') as name:
    work = Path(name)
    for module in public:
        shutil.copy(library / (module + '.cmi'), work)
    shutil.copy(root / 'testsuite/tests/vox/vox_lz4_public_client.ml', work)
    flags = ['-nostdlib', '-I', str(prefix / 'lib/ocaml'), '-I', str(work),
             '-extension', 'refinement_types', '-principal']
    for compiler, archive in [('ocamlc', 'cma'), ('ocamlopt', 'cmxa')]:
        command = [str(prefix / 'bin' / compiler), *flags]
        result = subprocess.run([*command, '-dlambda', '-c', 'vox_lz4_public_client.ml'],
                                cwd=work, text=True, capture_output=True)
        dump = result.stderr
        assert result.returncode == 0, dump
        assert 'Vox_lz4_spec' not in dump, dump
        calls = re.findall(r'apply \(field_imm \d+ \(global Vox_lz4!\)\)', dump)
        assert len(calls) == 2, dump
        obj = 'vox_lz4_public_client.' + ('cmo' if compiler == 'ocamlc' else 'cmx')
        subprocess.run([*command, str(library / ('vox_borrow.' + archive)), obj,
                        '-o', 'client'], cwd=work, check=True)
        subprocess.run([str(work / 'client')], check=True)
        print(compiler + ': public-only client and ghost erasure passed')
        print(dump)
    failures = {
        'heap_model': ('let f = Vox_lz4_spec_decode.decode_model\n', 'Unbound module'),
        'hidden': ('let f = Vox_lz4.C.compress_string\n', 'Unbound module'),
        'ghost': ('let f (s : string) : int = Iarray.length (Vox_string_view.contents s)\n', 'ghost'),
        'false_claim': ('let f (s : string) : {w : string | Vox_string_view.contents w === Vox_string_view.contents s} = Vox_lz4.compress s\n', 'refinement'),
        'false_decode_status': ('let f (wire : string) : {d : Vox_lz4_spec.decoded | match d with Ok _ -> true | Error Vox_lz4_spec.Output_limit -> true | Error _ -> false} = Vox_lz4.decompress_verified wire 0\n', 'refinement'),
    }
    for name, (source, diagnostic) in failures.items():
        (work / (name + '.ml')).write_text(source)
        for compiler in ['ocamlc', 'ocamlopt']:
            result = subprocess.run([str(prefix / 'bin' / compiler), *flags, '-c', name + '.ml'],
                                    cwd=work, text=True, capture_output=True)
            assert result.returncode != 0 and diagnostic.lower() in result.stderr.lower(), result.stderr
            print(compiler + ': ' + name + ': rejected as expected')
