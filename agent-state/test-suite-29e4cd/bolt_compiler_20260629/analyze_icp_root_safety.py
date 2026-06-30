#!/usr/bin/env python3
"""Estimate how much BOLT ICP opportunity is safe for OCaml frame descriptors.

The conservative safety proxy here is: the original indirect-call return frame
descriptor has zero live OCaml roots.  BOLT-created promoted direct-call return
PCs still need frame descriptors, but a zero-root descriptor is much less likely
to corrupt moving-GC state than cloning a descriptor with live root maps.
"""

from __future__ import annotations

import argparse
import bisect
import importlib.util
import sys
from collections import Counter, defaultdict
from dataclasses import dataclass
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
PATCHER_PATH = SCRIPT_DIR / "patch_ocaml_frametables.py"


def load_patcher():
    spec = importlib.util.spec_from_file_location("patch_ocaml_frametables", PATCHER_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {PATCHER_PATH}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


patcher = load_patcher()


@dataclass(frozen=True)
class DescriptorInfo:
    num_live: int
    has_alloc: bool
    has_debug: bool


def descriptor_info(elf, addr: int) -> DescriptorInfo:
    frame_data = elf.u16_at_addr(addr + 4)
    if frame_data == patcher.FRAME_RETURN_TO_C:
        return DescriptorInfo(num_live=elf.u16_at_addr(addr + 6), has_alloc=False, has_debug=False)
    if frame_data == patcher.FRAME_LONG_MARKER:
        actual_frame_data = elf.u32_at_addr(addr + 8)
        num_live = elf.u32_at_addr(addr + 12)
    else:
        actual_frame_data = frame_data
        num_live = elf.u16_at_addr(addr + 6)
    return DescriptorInfo(
        num_live=num_live,
        has_alloc=bool(actual_frame_data & patcher.FRAME_DESCRIPTOR_ALLOC),
        has_debug=bool(actual_frame_data & patcher.FRAME_DESCRIPTOR_DEBUG),
    )


def descriptors_by_pc(elf) -> dict[int, DescriptorInfo]:
    out: dict[int, DescriptorInfo] = {}
    for table in patcher.frametable_symbols(elf):
        count = elf.u64_at_addr(table.value)
        addr = table.value + 8
        for _ in range(count):
            pc = addr + elf.i32_at_addr(addr)
            out[pc] = descriptor_info(elf, addr)
            addr = patcher.descriptor_end(elf, addr)
    return out


def function_ranges(elf) -> list[tuple[int, int, str]]:
    ranges = []
    for sym, end in patcher.symbol_ranges(patcher.executable_symbols(elf)).values():
        ranges.append((sym.value, end, sym.name))
    return sorted(ranges)


def symbol_for_addr(ranges: list[tuple[int, int, str]], addr: int) -> tuple[str, int] | None:
    lo = 0
    hi = len(ranges)
    while lo < hi:
        mid = (lo + hi) // 2
        if ranges[mid][0] <= addr:
            lo = mid + 1
        else:
            hi = mid
    idx = lo - 1
    if idx < 0:
        return None
    start, end, name = ranges[idx]
    if end and addr >= end:
        return None
    return name, addr - start


def parse_fdata_counts(path: Path) -> dict[tuple[str, int], int]:
    counts: dict[tuple[str, int], int] = defaultdict(int)
    with path.open() as f:
        for line in f:
            parts = line.split()
            if len(parts) < 7:
                continue
            try:
                source_func = parts[1]
                source_off = int(parts[2], 16)
                count = int(parts[-1])
            except ValueError:
                continue
            counts[(source_func, source_off)] += count
    return counts


def fast_promoted_icp_old_calls(input_elf, output_elf, bat) -> list[int]:
    input_calls = sorted(patcher.collect_call_sites(input_elf.path), key=lambda c: c.call_addr)
    output_calls = sorted(patcher.collect_call_sites(output_elf.path), key=lambda c: c.call_addr)
    output_jump_targets = patcher.collect_unconditional_jump_targets(output_elf.path)

    input_ranges = patcher.symbol_ranges(patcher.executable_symbols(input_elf))
    output_ranges = patcher.symbol_ranges(patcher.executable_symbols(output_elf))
    output_by_name = patcher.unique_by_name(patcher.executable_symbols(output_elf))
    output_ranges_by_value = patcher.symbol_ranges_by_value(patcher.executable_symbols(output_elf))
    cold_by_hot: dict[int, list[int]] = {}
    for cold, hot in bat.cold_to_hot.items():
        cold_by_hot.setdefault(hot, []).append(cold)

    old_indirect_by_func: dict[str, list[int]] = defaultdict(list)
    ordered_input_ranges = sorted(
        (input_sym.value, input_end, name)
        for name, (input_sym, input_end) in input_ranges.items()
    )
    range_index = 0
    for call in input_calls:
        while (
            range_index + 1 < len(ordered_input_ranges)
            and ordered_input_ranges[range_index + 1][0] <= call.call_addr
        ):
            range_index += 1
        if range_index >= len(ordered_input_ranges):
            continue
        start, end, name = ordered_input_ranges[range_index]
        if call.call_addr < start or (end and call.call_addr >= end):
            continue
        if call.target is None:
            old_indirect_by_func[name].append(call.call_addr)

    fragments: list[tuple[int, int, str, int, list[tuple[int, int]]]] = []
    for name, (input_sym, _input_end) in input_ranges.items():
        output_sym = output_by_name.get(name)
        if output_sym is None or output_sym.value not in bat.maps:
            continue
        hot_range = output_ranges.get(name)
        if hot_range is not None:
            hot_sym, hot_end = hot_range
            fragments.append((hot_sym.value, hot_end, name, input_sym.value, bat.maps[hot_sym.value]))
        for cold_addr in cold_by_hot.get(output_sym.value, []):
            cold_range = output_ranges_by_value.get(cold_addr)
            if cold_range is not None and cold_addr in bat.maps:
                cold_sym, cold_end = cold_range
                fragments.append((cold_sym.value, cold_end, name, input_sym.value, bat.maps[cold_sym.value]))
    fragments.sort()

    out: list[int] = []
    seen: set[tuple[str, int]] = set()
    fragment_index = 0
    for call in output_calls:
        if call.target is None:
            continue
        merge = output_jump_targets.get(call.return_addr)
        if merge is None:
            continue
        while fragment_index + 1 < len(fragments) and fragments[fragment_index + 1][0] <= call.call_addr:
            fragment_index += 1
        if fragment_index >= len(fragments):
            continue
        start, end, name, input_start, entries = fragments[fragment_index]
        if call.call_addr < start or (end and call.call_addr >= end):
            continue
        approx_old = input_start + patcher.Translator.translate_output_offset(
            entries, call.call_addr - start
        )
        old_indirects = old_indirect_by_func.get(name, [])
        if not old_indirects:
            continue
        old_index = bisect.bisect_left(old_indirects, approx_old)
        if old_index >= len(old_indirects):
            continue
        candidate = old_indirects[old_index]
        key = (name, candidate)
        if key in seen:
            continue
        seen.add(key)
        out.append(candidate)
    return out


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input", type=Path, help="pre-BOLT executable")
    parser.add_argument("bolted", type=Path, help="BOLT ICP output with BAT")
    parser.add_argument("--fdata", type=Path, help="optional BOLT profile data")
    args = parser.parse_args()

    input_elf = patcher.Elf64(args.input)
    output_elf = patcher.Elf64(args.bolted)
    bat = patcher.parse_bat(output_elf)
    descs = descriptors_by_pc(input_elf)
    ranges = function_ranges(input_elf)
    fdata_counts = parse_fdata_counts(args.fdata) if args.fdata else {}
    calls_by_addr = {
        call.call_addr: call for call in patcher.collect_call_sites(input_elf.path)
    }
    old_call_addrs = fast_promoted_icp_old_calls(input_elf, output_elf, bat)

    by_roots = Counter()
    by_exact_roots = Counter()
    weighted_by_roots = Counter()
    top_weighted: list[tuple[int, str, int, int, bool, bool]] = []

    for old_call_addr in old_call_addrs:
        old_return = old_call_addr
        call = calls_by_addr.get(old_call_addr)
        if call is not None:
            old_return = call.return_addr
        info = descs.get(old_return)
        if info is None:
            by_roots["missing-descriptor"] += 1
            continue
        root_bucket = "zero-live" if info.num_live == 0 else "nonzero-live"
        by_roots[root_bucket] += 1
        by_exact_roots[info.num_live] += 1
        sym = symbol_for_addr(ranges, old_call_addr)
        weight = fdata_counts.get(sym, 0) if sym is not None else 0
        weighted_by_roots[root_bucket] += weight
        if weight:
            top_weighted.append(
                (weight, sym[0], sym[1], info.num_live, info.has_alloc, info.has_debug)
            )

    print(f"approximated promoted direct-call source sites: {len(old_call_addrs)}")
    print("by live-root bucket:")
    for key, value in by_roots.most_common():
        print(f"  {key}: {value}")
    print("weighted by fdata call count:")
    for key, value in weighted_by_roots.most_common():
        print(f"  {key}: {value}")
    print("top exact live-root counts:")
    for roots, value in by_exact_roots.most_common(20):
        print(f"  {roots}: {value}")
    print("top weighted unsupported callsites:")
    for weight, func, off, live, has_alloc, has_debug in sorted(top_weighted, reverse=True)[:25]:
        print(
            f"  count={weight} live={live} alloc={int(has_alloc)} debug={int(has_debug)} "
            f"{func}+0x{off:x}"
        )


if __name__ == "__main__":
    main()
