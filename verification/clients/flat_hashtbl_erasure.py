import json
import pathlib
import re
import sys

output = pathlib.Path(sys.argv[1])
reports = {}
for compiler in ('ocamlc', 'ocamlopt'):
    emitted = (output / (compiler + '.lambda')).read_text()
    exercise = emitted.split('\n   Key/', 1)[0]
    assert 'replace_lookup/' in exercise and 'update_framed/' in exercise
    # Public signature fields 0..6 are Map, count, count_def and ghost observers.
    assert not re.search(r'\(field(?:_imm)? [0-6] V/\d+\)', exercise), exercise
    assert not re.search(r'same_get|put_get|erase_get|routes|certificate', exercise)
    reports[compiler] = {'public_semantic_observations_and_lemmas_erased': True}
    if compiler == 'ocamlopt':
        assert re.search(r'before/\d+\[#\(\)\]', exercise)
        assert re.search(r'token/\d+\[#\(\)\]', exercise)
        assert '#(#(), #())' in exercise
        reports[compiler]['snapshot_and_token_have_zero_native_layout'] = True
(output / 'erasure.json').write_text(json.dumps(reports, indent=2) + '\n')
print('Public-client erasure checks passed.')
