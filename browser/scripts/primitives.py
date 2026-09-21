from pathlib import Path
import struct
root = Path(__file__).resolve().parents[1]
blob = (root/'.build/compiler.byte').read_bytes()
count = struct.unpack('>I', blob[-16:-12])[0]
entries = [(blob[-16-count*8+i*8:-16-count*8+i*8+4].decode(),struct.unpack('>I',blob[-16-count*8+i*8+4:-16-count*8+i*8+8])[0]) for i in range(count)]
pos = len(blob)-16-count*8-sum(size for _,size in entries)
for name,size in entries:
 if name=='PRIM': names=blob[pos:pos+size].decode().strip('\0').split('\0')
 pos+=size
native=(root/'.runtime-source/runtime/primitives').read_text().splitlines()
extra=sorted(set(names)-set(native))
print('Additional compiler primitives:',extra)
s=(root/'.runtime-source/runtime/prims.c').read_text()
start=s.index('const c_primitive caml_builtin_cprim')
s=s[:start]+''.join('CAMLextern value '+n+'();\n' for n in extra)+s[start:]
a=s.index('c_primitive caml_builtin_cprim');end=s.index('  0 };',a)
s=s[:end]+''.join('  (c_primitive)'+n+',\n' for n in extra)+s[end:]
a=s.index('char * caml_names_of_builtin_cprim') if 'char * caml_names_of_builtin_cprim' in s else s.index('caml_names_of_builtin_cprim');end=s.index('  0 };',a)
s=s[:end]+''.join('  "'+n+'",\n' for n in extra)+s[end:]
(root/'.build/prims.c').write_text(s)
