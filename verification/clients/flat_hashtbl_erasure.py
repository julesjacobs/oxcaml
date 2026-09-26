import json
import pathlib
import re
import sys

output = pathlib.Path(sys.argv[1])
reports = {}
for compiler in ('ocamlc', 'ocamlopt'):
    emitted = (output / (compiler + '.lambda')).read_text()
    exercise = emitted.split('\n   Key/', 1)[0]
    assert 'replace_lookup/' in exercise and 'replace_length/' in exercise
    assert 'update_framed/' in exercise
    # Public signature fields 0..4 are Map and the ghost observers.
    assert not re.search(r'\(field(?:_imm)? [0-4] V/\d+\)', exercise), exercise
    assert not re.search(r'put_get|erase_get|count_put|count_erase|lookup_empty|routes|certificate', exercise)
    reports[compiler] = {'public_semantic_observations_and_lemmas_erased': True}
    if compiler == 'ocamlopt':
        assert re.search(r'before/\d+\[#\(\)\]', exercise)
        assert re.search(r'token/\d+\[#\(\)\]', exercise)
        assert '#(#(), #())' in exercise
        reports[compiler]['snapshot_and_token_have_zero_native_layout'] = True
cmm = (output / 'ocamlopt.cmm').read_text()
assert 'find_after_replace' in cmm
# Native code calls no ownership primitive: tokens and heaps are erased.
assert 'caml_pref' not in cmm, 'ghost ownership primitive survives in Cmm'
assert not re.search(r'put_get|erase_get|count_put|count_erase|lookup_empty', cmm)
reports['ocamlopt']['native_cmm_calls_no_ownership_primitive_or_lemma'] = True
(output / 'erasure.json').write_text(json.dumps(reports, indent=2) + '\n')
print('Public-client erasure checks passed.')
