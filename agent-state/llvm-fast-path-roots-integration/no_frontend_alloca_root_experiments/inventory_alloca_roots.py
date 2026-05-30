#!/usr/bin/env python3
"""Inventory ptr addrspace(1) allocas used in gc-live bundles."""

import re
import sys
from pathlib import Path


ALLOCA_RE = re.compile(r"(%[-.\w]+) = alloca ptr addrspace\(1\)")
VALUE_RE = re.compile(r"%[-.\w]+")


def find_function(lines, name):
    start = next(
        (
            index
            for index, line in enumerate(lines)
            if line.startswith("define ") and name in line
        ),
        None,
    )
    if start is None:
        raise RuntimeError(f"function not found: {name}")
    end = next(
        (
            index
            for index in range(start + 1, len(lines))
            if lines[index].startswith("define ")
        ),
        len(lines),
    )
    return start, end, lines[start:end]


def main(argv):
    if len(argv) != 3:
        print(f"usage: {Path(argv[0]).name} FILE.ll FUNCTION_SUBSTRING", file=sys.stderr)
        return 2

    path = Path(argv[1])
    function = argv[2]
    lines = path.read_text().splitlines()
    start, end, body = find_function(lines, function)

    allocas = {}
    for offset, line in enumerate(body, start + 1):
        match = ALLOCA_RE.search(line)
        if match:
            allocas[match.group(1)] = offset

    gc_live_lines = [
        (offset, line)
        for offset, line in enumerate(body, start + 1)
        if '"gc-live"' in line
    ]

    slots_in_gc_live = set()
    for _offset, line in gc_live_lines:
        slots_in_gc_live.update(VALUE_RE.findall(line) & allocas.keys())

    print(f"{path}")
    print(f"function: {function}")
    print(f"lines: {start + 1}-{end}")
    print(f"ptr addrspace(1) allocas: {len(allocas)}")
    print(f"gc-live bundle lines: {len(gc_live_lines)}")
    print(f"ptr addrspace(1) allocas directly in gc-live: {len(slots_in_gc_live)}")

    if "%21" in allocas:
        hits = [
            (offset, line.strip())
            for offset, line in gc_live_lines
            if "%21" in VALUE_RE.findall(line)
        ]
        print(f"%21 alloca line: {allocas['%21']}")
        print(f"gc-live bundles mentioning %21: {len(hits)}")
        for offset, line in hits[:8]:
            print(f"  {offset}: {line}")
        if len(hits) > 8:
            print(f"  ... {len(hits) - 8} more")

    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
