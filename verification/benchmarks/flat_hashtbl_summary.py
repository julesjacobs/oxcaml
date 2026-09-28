#!/usr/bin/env python3
"""Summarize flat_hashtbl.py output: medians, spread, ratios, checks, load."""
import csv
import statistics
import sys
from collections import defaultdict

rows = defaultdict(list)
checks = defaultdict(set)
loads = []
header = None
for line in open(sys.argv[1]):
    line = line.rstrip('\n')
    if line.startswith('# load,'):
        loads.append(line[len('# load,'):].split(','))
        continue
    if line.startswith('#') or line.startswith('repeat,') or not line:
        continue
    repeat, impl, payload, n, op, a, b = line.split(',')
    if op == 'check':
        checks[(payload, int(n))].add((impl, a, b))
    else:
        rows[(impl, payload, int(n), op)].append(float(a))

impls = [i for i in ['simd', 'scalar', 'stdlib', 'base']
         if any(k[0] == i for k in rows)]
ops = ['build', 'hit', 'miss', 'replace', 'churn', 'mixed']
sizes = sorted({k[2] for k in rows})
payloads = sorted({k[1] for k in rows})

print('## Checks\n')
bad = 0
for key in sorted(checks):
    values = {(a, b) for _, a, b in checks[key]}
    implset = sorted({i for i, _, _ in checks[key]})
    if len(values) != 1:
        bad += 1
        print(f'MISMATCH {key}: {sorted(checks[key])}')
print(f'{len(checks)} (payload, size) pairs checked across {", ".join(impls)}; '
      f'{bad} mismatches.\n')

for payload in payloads:
    print(f'## {payload} payload: median ns/op (min-max over repeats)\n')
    head = '| entries | op | ' + ' | '.join(impls) + ' | ' + ' | '.join(
        f'stdlib/{i}' for i in impls if i != 'stdlib') + ' |'
    print(head)
    print('|' + '---|' * (2 + len(impls) + len(impls) - 1))
    for n in sizes:
        for op in ops:
            cells, med = [], {}
            for impl in impls:
                xs = rows.get((impl, payload, n, op))
                if not xs:
                    cells.append('')
                    continue
                m = statistics.median(xs)
                med[impl] = m
                cells.append(f'{m:.2f} ({min(xs):.2f}-{max(xs):.2f})')
            ratios = [f'{med["stdlib"] / med[i]:.2f}' if i in med and 'stdlib' in med
                      else '' for i in impls if i != 'stdlib']
            print(f'| {n} | {op} | ' + ' | '.join(cells) + ' | ' +
                  ' | '.join(ratios) + ' |')
    print()

if loads:
    busy = [float(l[3]) for l in loads]
    before = [float(l[4].split()[0]) for l in loads]
    after = [float(l[5].split()[0]) for l in loads]
    print('## Load\n')
    print(f'{len(loads)} runs. Busy CPUs in the second before each run: '
          f'median {statistics.median(busy):.2f}, max {max(busy):.2f}. '
          f'1-minute load average before: median {statistics.median(before):.2f}, '
          f'max {max(before):.2f}; after: median {statistics.median(after):.2f}, '
          f'max {max(after):.2f}.')
