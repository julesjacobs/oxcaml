#!/usr/bin/env python3
"""Verify the library using an existing installation without modifying it."""
from pathlib import Path
import re,sys,subprocess,shutil
root=Path(__file__).resolve().parents[2];prefix=Path(sys.argv[1]).resolve();out=root/'_build/refine-library';out.mkdir(exist_ok=True)
modules=re.search(r'modules=\((.*?)\)',(root/'verification/library/build.sh').read_text(),re.S).group(1).split()+['vox_traversal']
for m in modules:
 for ext in ['.mli','.ml']:
  src=root/'verification/library'/(m+ext)
  if not src.exists():continue
  shutil.copy(src,out)
  for compiler in (['ocamlc'] if ext=='.mli' else ['ocamlc','ocamlopt']):
   cmd=[str(prefix/'bin'/compiler),'-nostdlib','-I',str(prefix/'lib/ocaml'),'-I','.', '-extension','refinement_types']
   if not (m.startswith('vox_table_') or m=='vox_verified_flat_hashtbl'):cmd+=['-principal']
   p=subprocess.run(cmd+['-c',src.name],cwd=out,capture_output=True,text=True)
   (out/(src.name+'.'+compiler+'.log')).write_text(p.stdout+p.stderr)
   if p.returncode:print(m,p.stderr,flush=True);sys.exit(p.returncode)
 print(m,flush=True)
