import json
import pathlib
import re
import sys

source = pathlib.Path(sys.argv[1])
emitted = source.read_text()
marker = emitted.index('\n camlVox_table_vacancy__scan_')
start = emitted.rfind('(function', 0, marker)
end = emitted.index('\n(function', start + 1)
body = emitted[start:end]
checks = {
    'unboxed_two_scalar_result': ': val*val\n' in body,
    'no_scan_allocation': '(alloc' not in body,
    'rank_is_unused_ghost_placeholder': len(re.findall(r'\brank/\d+', body)) == 1,
    'no_exhaustion_raise': 'raise' not in body,
    'no_compaction': 'compact' not in body,
}
assert all(checks.values()), checks
source.with_suffix('.json').write_text(json.dumps(checks, indent=2) + '\n')
print('Vacancy-scan Cmm erasure checks passed.')
