from pathlib import Path
import json,shutil
root=Path(__file__).resolve().parents[2];out=root/'browser/public/assets';out.mkdir(exist_ok=True)
manifest=[];offset=0
with (out/'stdlib.data').open('wb') as packed:
 files=[(p.name,p.read_bytes()) for p in sorted((root/'_install/lib/ocaml').glob('*.cmi'))]
 files += [('runtime-launch-info',(root/'_install/lib/ocaml/runtime-launch-info').read_bytes()),('std_exit.cmo',(root/'_install/lib/ocaml/std_exit.cmo').read_bytes()),('stdlib.cma',(root/'_install/lib/ocaml/stdlib.cma').read_bytes()),('primitives',(root/'browser/.runtime-source/runtime/primitives').read_bytes()),('camlheader',b''),('camlheader_ur',b'')]
 for name,data in files:
  manifest.append(dict(name=name,offset=offset,length=len(data)));packed.write(data);offset+=len(data)
(out/'stdlib-manifest.json').write_text(json.dumps(manifest))
shutil.copyfile(root/'browser/.build/compiler.byte',out/'compiler.byte')
for name in ['z3-built.js','z3-built.wasm']:
 shutil.copyfile(root/'browser/node_modules/z3-solver/build'/name,out/name)
print('Packaged',len(manifest),'standard-library files;',offset,'bytes')

(out/'LICENSE-OCaml.txt').write_bytes((root/'LICENSE').read_bytes())
(out/'LICENSE-Z3.txt').write_bytes((root/'browser/node_modules/z3-solver/LICENSE.txt').read_bytes())
