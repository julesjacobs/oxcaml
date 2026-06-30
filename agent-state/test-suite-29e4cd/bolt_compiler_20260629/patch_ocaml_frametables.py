#!/usr/bin/env python3
"""Patch OCaml native frame tables after BOLT rewriting.

BOLT's BAT note maps output text addresses back to input text addresses.
OCaml frame descriptors store PC-relative return addresses in data, and this
local BOLT build does not rewrite those data entries.  This script inverts BAT
per function and rewrites every caml*__frametable retaddr_rel field in a BOLTed
ELF64/x86-64 executable.
"""

from __future__ import annotations

import argparse
import bisect
import os
import re
import shutil
import struct
import subprocess
from dataclasses import dataclass
from pathlib import Path


SHT_SYMTAB = 2
SHT_DYNSYM = 11
SHF_ALLOC = 0x2
SHF_EXECINSTR = 0x4
PT_LOAD = 1
PF_W = 0x2

FRAME_DESCRIPTOR_DEBUG = 1
FRAME_DESCRIPTOR_ALLOC = 2
FRAME_RETURN_TO_C = 0xFFFF
FRAME_LONG_MARKER = 0x7FFF
BRANCHENTRY = 0x80000000

INSTRUCTION_RE = re.compile(r"^\s*([0-9a-fA-F]+):\s*(.*)$")
CALL_TARGET_RE = re.compile(r"<([^>]+)>")
PUSH_IMMEDIATE_RE = re.compile(r"^push[q]?\s+\$(0x[0-9a-fA-F]+|[0-9]+)")
JUMP_ADDRESS_RE = re.compile(r"^jmp[q]?\s+([0-9a-fA-F]+)")


def align(value: int, by: int) -> int:
    return (value + by - 1) & -by


@dataclass(frozen=True)
class Section:
    name: str
    sh_type: int
    flags: int
    addr: int
    offset: int
    size: int
    link: int
    entsize: int


@dataclass(frozen=True)
class Symbol:
    name: str
    value: int
    size: int
    info: int
    shndx: int


@dataclass(frozen=True)
class BatInfo:
    maps: dict[int, list[tuple[int, int]]]
    cold_to_hot: dict[int, int]


@dataclass(frozen=True)
class ProgramHeader:
    index: int
    offset: int
    p_type: int
    flags: int
    file_offset: int
    vaddr: int
    filesz: int
    memsz: int
    align: int


class Elf64:
    def __init__(self, path: Path) -> None:
        self.path = path
        self.data = bytearray(path.read_bytes())
        if self.data[:4] != b"\x7fELF" or self.data[4] != 2 or self.data[5] != 1:
            raise ValueError(f"{path}: expected little-endian ELF64")
        phoff = self.u64_at(0x20)
        phentsize = self.u16_at(0x36)
        phnum = self.u16_at(0x38)
        shoff = self.u64_at(0x28)
        shentsize = self.u16_at(0x3A)
        shnum = self.u16_at(0x3C)
        shstrndx = self.u16_at(0x3E)
        self.program_headers: list[ProgramHeader] = []
        for i in range(phnum):
            off = phoff + i * phentsize
            p_type, flags, file_offset, vaddr, _paddr, filesz, memsz, align = struct.unpack_from(
                "<IIQQQQQQ", self.data, off
            )
            self.program_headers.append(
                ProgramHeader(
                    index=i,
                    offset=off,
                    p_type=p_type,
                    flags=flags,
                    file_offset=file_offset,
                    vaddr=vaddr,
                    filesz=filesz,
                    memsz=memsz,
                    align=align,
                )
            )
        raw = []
        for i in range(shnum):
            off = shoff + i * shentsize
            raw.append(struct.unpack_from("<IIQQQQIIQQ", self.data, off))
        shstr = raw[shstrndx]
        shstrtab = self.data[shstr[4] : shstr[4] + shstr[5]]
        self.sections: list[Section] = []
        for entry in raw:
            name_off, sh_type, flags, addr, offset, size, link, _info, _align, entsize = entry
            self.sections.append(
                Section(
                    name=self.cstring(shstrtab, name_off),
                    sh_type=sh_type,
                    flags=flags,
                    addr=addr,
                    offset=offset,
                    size=size,
                    link=link,
                    entsize=entsize,
                )
            )

    @staticmethod
    def cstring(buf: bytes | bytearray, off: int) -> str:
        end = buf.find(b"\0", off)
        if end < 0:
            end = len(buf)
        return bytes(buf[off:end]).decode("utf-8", "replace")

    def section(self, name: str) -> Section:
        for sec in self.sections:
            if sec.name == name:
                return sec
        raise KeyError(name)

    def symbols(self) -> list[Symbol]:
        out: list[Symbol] = []
        for sec in self.sections:
            if sec.sh_type not in (SHT_SYMTAB, SHT_DYNSYM) or sec.entsize == 0:
                continue
            strtab_sec = self.sections[sec.link]
            strtab = self.data[strtab_sec.offset : strtab_sec.offset + strtab_sec.size]
            for off in range(sec.offset, sec.offset + sec.size, sec.entsize):
                st_name, st_info, _st_other, st_shndx, st_value, st_size = struct.unpack_from(
                    "<IBBHQQ", self.data, off
                )
                if st_name == 0:
                    continue
                out.append(Symbol(self.cstring(strtab, st_name), st_value, st_size, st_info, st_shndx))
        return out

    def section_for_addr(self, addr: int) -> Section:
        for sec in self.sections:
            if (sec.flags & SHF_ALLOC) and sec.addr <= addr < sec.addr + sec.size:
                return sec
        raise KeyError(f"no section for address 0x{addr:x}")

    def off_for_addr(self, addr: int) -> int:
        sec = self.section_for_addr(addr)
        return sec.offset + (addr - sec.addr)

    def u8_at_addr(self, addr: int) -> int:
        return self.data[self.off_for_addr(addr)]

    def u16_at_addr(self, addr: int) -> int:
        return struct.unpack_from("<H", self.data, self.off_for_addr(addr))[0]

    def u32_at_addr(self, addr: int) -> int:
        return struct.unpack_from("<I", self.data, self.off_for_addr(addr))[0]

    def u64_at_addr(self, addr: int) -> int:
        return struct.unpack_from("<Q", self.data, self.off_for_addr(addr))[0]

    def i32_at_addr(self, addr: int) -> int:
        return struct.unpack_from("<i", self.data, self.off_for_addr(addr))[0]

    def put_i32_at_addr(self, addr: int, value: int) -> None:
        if value < -(1 << 31) or value >= (1 << 31):
            raise OverflowError(f"0x{addr:x}: relative address {value} does not fit int32")
        struct.pack_into("<i", self.data, self.off_for_addr(addr), value)

    def put_u64_at_addr(self, addr: int, value: int) -> None:
        struct.pack_into("<Q", self.data, self.off_for_addr(addr), value)

    def u16_at(self, off: int) -> int:
        return struct.unpack_from("<H", self.data, off)[0]

    def u64_at(self, off: int) -> int:
        return struct.unpack_from("<Q", self.data, off)[0]

    def put_u64_at(self, off: int, value: int) -> None:
        struct.pack_into("<Q", self.data, off, value)

    def write(self, path: Path) -> None:
        path.write_bytes(self.data)


def parse_bat(output: Elf64) -> BatInfo:
    sec = output.section(".note.bolt_bat")
    buf = bytes(output.data[sec.offset : sec.offset + sec.size])
    off = 0
    if len(buf) < 12:
        raise ValueError(".note.bolt_bat is too short")
    name_sz, desc_sz, note_type = struct.unpack_from("<III", buf, off)
    off += 12
    name = buf[off : off + name_sz].rstrip(b"\0")
    off = align(off + name_sz, 4)
    if name != b"BOLT":
        raise ValueError(f"unexpected BAT note name {name!r}")
    desc_end = off + desc_sz
    if desc_end > len(buf):
        raise ValueError(".note.bolt_bat descriptor overruns section")
    num_functions = struct.unpack_from("<I", buf, off)[0]
    off += 4
    maps: dict[int, list[tuple[int, int]]] = {}
    for _ in range(num_functions):
        address, num_entries = struct.unpack_from("<QI", buf, off)
        off += 12
        entries: list[tuple[int, int]] = []
        for _ in range(num_entries):
            out_off, in_off = struct.unpack_from("<II", buf, off)
            off += 8
            entries.append((out_off, in_off & ~BRANCHENTRY))
        entries.sort()
        maps[address] = entries
    num_cold = struct.unpack_from("<I", buf, off)[0]
    off += 4
    cold_to_hot: dict[int, int] = {}
    for _ in range(num_cold):
        cold, hot = struct.unpack_from("<QQ", buf, off)
        off += 16
        cold_to_hot[cold] = hot
    if off > desc_end:
        raise ValueError(".note.bolt_bat cold map overruns descriptor")
    return BatInfo(maps=maps, cold_to_hot=cold_to_hot)


def unique_by_name(symbols: list[Symbol]) -> dict[str, Symbol]:
    out: dict[str, Symbol] = {}
    for sym in sorted(symbols, key=lambda s: (s.name, s.size, s.value)):
        out.setdefault(sym.name, sym)
    return out


def executable_symbols(elf: Elf64) -> list[Symbol]:
    syms = []
    seen = set()
    for sym in elf.symbols():
        if not sym.name or sym.value == 0 or sym.shndx >= len(elf.sections):
            continue
        sec = elf.sections[sym.shndx]
        if not (sec.flags & SHF_EXECINSTR):
            continue
        key = (sym.name, sym.value)
        if key in seen:
            continue
        seen.add(key)
        syms.append(sym)
    syms.sort(key=lambda s: (s.value, s.name))
    return syms


def frametable_symbols(elf: Elf64) -> list[Symbol]:
    syms = []
    seen = set()
    for sym in elf.symbols():
        if not sym.name.endswith("__frametable") or sym.value == 0 or sym.shndx >= len(elf.sections):
            continue
        sec = elf.sections[sym.shndx]
        if sec.flags & SHF_EXECINSTR:
            continue
        key = (sym.name, sym.value)
        if key in seen:
            continue
        seen.add(key)
        syms.append(sym)
    syms.sort(key=lambda s: s.value)
    return syms


@dataclass(frozen=True)
class CallSite:
    call_addr: int
    return_addr: int
    target: str | None


@dataclass(frozen=True)
class SharedReturnSite:
    push_addr: int
    jmp_addr: int
    return_addr: int


@dataclass(frozen=True)
class UnsupportedICPReturn:
    function: str
    old_call_addr: int
    direct_call_addr: int
    return_addr: int
    shared_return_addr: int

    def describe(self) -> str:
        return (
            f"{self.function}: promoted direct call 0x{self.direct_call_addr:x} "
            f"from old indirect 0x{self.old_call_addr:x} returns to "
            f"0x{self.return_addr:x}, which needs a synthesized frame descriptor "
            f"distinct from shared return 0x{self.shared_return_addr:x}"
        )


def normalize_call_target(target: str | None) -> str | None:
    if target is None:
        return None
    return target.split("+", 1)[0]


def collect_call_sites(path: Path) -> list[CallSite]:
    proc = subprocess.run(
        ["objdump", "-d", "--no-show-raw-insn", str(path)],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    calls: list[tuple[int, str | None]] = []
    previous_call: tuple[int, str | None] | None = None
    out: list[CallSite] = []
    for line in proc.stdout.splitlines():
        match = INSTRUCTION_RE.match(line)
        if match is None:
            continue
        addr = int(match.group(1), 16)
        text = match.group(2).strip()
        if previous_call is not None:
            call_addr, target = previous_call
            out.append(CallSite(call_addr, addr, target))
            previous_call = None
        if text.startswith("call"):
            target_match = CALL_TARGET_RE.search(text)
            target = normalize_call_target(target_match.group(1) if target_match else None)
            previous_call = (addr, target)
            calls.append((addr, target))
    return out


def collect_shared_return_sites(path: Path) -> list[SharedReturnSite]:
    proc = subprocess.run(
        ["objdump", "-d", "--no-show-raw-insn", str(path)],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    pending: tuple[int, int] | None = None
    out: list[SharedReturnSite] = []
    for line in proc.stdout.splitlines():
        match = INSTRUCTION_RE.match(line)
        if match is None:
            continue
        addr = int(match.group(1), 16)
        text = match.group(2).strip()
        if pending is not None:
            push_addr, return_addr = pending
            if text.startswith("jmp") and "*" in text:
                out.append(SharedReturnSite(push_addr, addr, return_addr))
            pending = None
        push_match = PUSH_IMMEDIATE_RE.match(text)
        if push_match is not None:
            pending = (addr, int(push_match.group(1), 0))
    return out


def collect_unconditional_jump_targets(path: Path) -> dict[int, int]:
    proc = subprocess.run(
        ["objdump", "-d", "--no-show-raw-insn", str(path)],
        check=True,
        text=True,
        stdout=subprocess.PIPE,
    )
    out: dict[int, int] = {}
    for line in proc.stdout.splitlines():
        match = INSTRUCTION_RE.match(line)
        if match is None:
            continue
        addr = int(match.group(1), 16)
        text = match.group(2).strip()
        jump_match = JUMP_ADDRESS_RE.match(text)
        if jump_match is not None:
            out[addr] = int(jump_match.group(1), 16)
    return out


def symbol_ranges(symbols: list[Symbol]) -> dict[str, tuple[Symbol, int]]:
    ranges: dict[str, tuple[Symbol, int]] = {}
    ordered = sorted(symbols, key=lambda s: (s.value, s.name))
    for i, sym in enumerate(ordered):
        if sym.size:
            end = sym.value + sym.size
        elif i + 1 < len(ordered):
            end = ordered[i + 1].value
        else:
            end = sym.value
        ranges.setdefault(sym.name, (sym, end))
    return ranges


def symbol_ranges_by_value(symbols: list[Symbol]) -> dict[int, tuple[Symbol, int]]:
    ranges: dict[int, tuple[Symbol, int]] = {}
    ordered = sorted(symbols, key=lambda s: (s.value, s.name))
    for i, sym in enumerate(ordered):
        if sym.size:
            end = sym.value + sym.size
        elif i + 1 < len(ordered):
            end = ordered[i + 1].value
        else:
            end = sym.value
        ranges.setdefault(sym.value, (sym, end))
    return ranges


def calls_in_range(calls: list[CallSite], call_addrs: list[int], start: int, end: int) -> list[CallSite]:
    left = bisect.bisect_left(call_addrs, start)
    selected: list[CallSite] = []
    for call in calls[left:]:
        if end and call.call_addr >= end:
            break
        selected.append(call)
    return selected


def shared_returns_in_range(
    sites: list[SharedReturnSite], site_addrs: list[int], start: int, end: int
) -> list[SharedReturnSite]:
    left = bisect.bisect_left(site_addrs, start)
    selected: list[SharedReturnSite] = []
    for site in sites[left:]:
        if end and site.push_addr >= end:
            break
        selected.append(site)
    return selected


def build_call_return_map(
    input_elf: Elf64, output_elf: Elf64, bat: BatInfo
) -> tuple[dict[int, int], list[UnsupportedICPReturn]]:
    input_calls = sorted(collect_call_sites(input_elf.path), key=lambda c: c.call_addr)
    output_calls = sorted(collect_call_sites(output_elf.path), key=lambda c: c.call_addr)
    output_shared_returns = sorted(
        collect_shared_return_sites(output_elf.path), key=lambda site: site.push_addr
    )
    output_jump_targets = collect_unconditional_jump_targets(output_elf.path)
    input_call_addrs = [call.call_addr for call in input_calls]
    output_call_addrs = [call.call_addr for call in output_calls]
    output_shared_return_addrs = [site.push_addr for site in output_shared_returns]
    input_exec = executable_symbols(input_elf)
    output_exec = executable_symbols(output_elf)
    input_ranges = symbol_ranges(input_exec)
    output_ranges = symbol_ranges(output_exec)
    output_ranges_by_value = symbol_ranges_by_value(output_exec)
    output_by_name = unique_by_name(output_exec)
    cold_by_hot: dict[int, list[int]] = {}
    for cold, hot in bat.cold_to_hot.items():
        cold_by_hot.setdefault(hot, []).append(cold)
    mapping: dict[int, int] = {}
    unsupported_icp_returns: list[UnsupportedICPReturn] = []
    unsupported_seen: set[tuple[str, int, int, int]] = set()

    for name, (input_sym, input_end) in input_ranges.items():
        output_sym = output_by_name.get(name)
        if output_sym is None or output_sym.value not in bat.maps:
            continue
        output_fragments: list[tuple[Symbol, int]] = []
        hot_range = output_ranges.get(name)
        if hot_range is not None:
            output_fragments.append(hot_range)
        for cold_addr in cold_by_hot.get(output_sym.value, []):
            cold_range = output_ranges_by_value.get(cold_addr)
            if cold_range is not None:
                output_fragments.append(cold_range)
        old_calls = calls_in_range(input_calls, input_call_addrs, input_sym.value, input_end)
        old_indirect_calls = [call for call in old_calls if call.target is None]
        old_indirect_by_addr = {call.call_addr: call for call in old_indirect_calls}
        old_calls_by_addr = sorted(old_calls, key=lambda call: call.call_addr)
        old_call_addrs_in_func = [call.call_addr for call in old_calls_by_addr]
        old_by_target: dict[str | None, list[CallSite]] = {}
        new_by_target: dict[str | None, list[tuple[CallSite, int]]] = {}
        new_direct_approx_pairs: list[tuple[int, int, CallSite]] = []
        for call in old_calls:
            old_by_target.setdefault(call.target, []).append(call)
        for fragment_sym, fragment_end in output_fragments:
            bat_entries = bat.maps.get(fragment_sym.value)
            if bat_entries is None:
                continue
            new_calls = calls_in_range(output_calls, output_call_addrs, fragment_sym.value, fragment_end)
            for call in new_calls:
                approx_old_offset = Translator.translate_output_offset(
                    bat_entries, call.call_addr - fragment_sym.value
                )
                approx_old_call = input_sym.value + approx_old_offset
                new_by_target.setdefault(call.target, []).append((call, approx_old_call))
                if call.target is not None:
                    approx_old_return_offset = Translator.translate_output_offset(
                        bat_entries, call.return_addr - fragment_sym.value
                    )
                    new_direct_approx_pairs.append(
                        (approx_old_call, input_sym.value + approx_old_return_offset, call)
                    )
            shared_return_sites = shared_returns_in_range(
                output_shared_returns,
                output_shared_return_addrs,
                fragment_sym.value,
                fragment_end,
            )
            for site in shared_return_sites:
                old_call = None
                for site_addr in (site.jmp_addr, site.push_addr):
                    approx_old_offset = Translator.translate_output_offset(
                        bat_entries, site_addr - fragment_sym.value
                    )
                    approx_old_call = input_sym.value + approx_old_offset
                    old_call = old_indirect_by_addr.get(approx_old_call)
                    if old_call is not None:
                        break
                if old_call is not None:
                    mapping[old_call.return_addr] = site.return_addr
                    idx = bisect.bisect_left(old_call_addrs_in_func, old_call.call_addr)
                    previous_old_call = (
                        old_call_addrs_in_func[idx - 1] if idx > 0 else input_sym.value
                    )
                    for direct_approx_call, _direct_approx_return, direct_call in new_direct_approx_pairs:
                        if direct_call.return_addr == site.return_addr:
                            continue
                        if output_jump_targets.get(direct_call.return_addr) != site.return_addr:
                            continue
                        if not (previous_old_call < direct_approx_call <= old_call.call_addr):
                            continue
                        key = (name, old_call.call_addr, direct_call.call_addr, direct_call.return_addr)
                        if key in unsupported_seen:
                            continue
                        unsupported_seen.add(key)
                        unsupported_icp_returns.append(
                            UnsupportedICPReturn(
                                function=name,
                                old_call_addr=old_call.call_addr,
                                direct_call_addr=direct_call.call_addr,
                                return_addr=direct_call.return_addr,
                                shared_return_addr=site.return_addr,
                            )
                        )
        for target, old_group in old_by_target.items():
            new_group = new_by_target.get(target)
            if new_group is None or len(new_group) != len(old_group):
                continue
            old_group = sorted(old_group, key=lambda call: call.call_addr)
            new_group = sorted(new_group, key=lambda item: item[1])
            for old_call, (new_call, _approx_old_call) in zip(old_group, new_group, strict=True):
                mapping[old_call.return_addr] = new_call.return_addr
    return mapping, unsupported_icp_returns


class Translator:
    def __init__(self, input_elf: Elf64, output_elf: Elf64, bat: BatInfo) -> None:
        output_by_name = unique_by_name(executable_symbols(output_elf))
        input_candidates = []
        for sym in executable_symbols(input_elf):
            out_sym = output_by_name.get(sym.name)
            if out_sym is None or out_sym.value not in bat.maps:
                continue
            input_candidates.append(sym)
        self.output_by_name = output_by_name
        self.bat_maps = bat.maps
        self.input_candidates = input_candidates
        self.input_values = [sym.value for sym in input_candidates]

    def translate_pc(self, old_pc: int) -> tuple[int, str] | None:
        idx = bisect.bisect_right(self.input_values, old_pc) - 1
        while idx >= 0:
            sym = self.input_candidates[idx]
            old_off = old_pc - sym.value
            out_sym = self.output_by_name[sym.name]
            translated_off = self.invert_map(self.bat_maps[out_sym.value], old_off)
            if translated_off is not None:
                return out_sym.value + translated_off, sym.name
            idx -= 1
        return None

    @staticmethod
    def invert_map(entries: list[tuple[int, int]], input_off: int) -> int | None:
        for i, (out_start, in_start) in enumerate(entries):
            if i + 1 < len(entries):
                out_end = entries[i + 1][0]
                length = max(0, out_end - out_start)
                if in_start <= input_off < in_start + length:
                    return out_start + (input_off - in_start)
            elif input_off >= in_start:
                return out_start + (input_off - in_start)
        for out_start, in_start in entries:
            if input_off == in_start:
                return out_start
        return None

    @staticmethod
    def translate_output_offset(entries: list[tuple[int, int]], output_off: int) -> int:
        lo = 0
        hi = len(entries)
        while lo < hi:
            mid = (lo + hi) // 2
            if entries[mid][0] <= output_off:
                lo = mid + 1
            else:
                hi = mid
        idx = lo - 1
        if idx < 0:
            return output_off
        out_start, in_start = entries[idx]
        return in_start + (output_off - out_start)


def descriptor_end(elf: Elf64, addr: int) -> int:
    frame_data = elf.u16_at_addr(addr + 4)
    if frame_data == FRAME_RETURN_TO_C:
        num_live = elf.u16_at_addr(addr + 6)
        if num_live != 0:
            raise ValueError(f"0x{addr:x}: return-to-C frame has {num_live} live values")
        return align(addr + 8, 8)

    if frame_data == FRAME_LONG_MARKER:
        actual_frame_data = elf.u32_at_addr(addr + 8)
        num_live = elf.u32_at_addr(addr + 12)
        p = addr + 16 + 4 * num_live
    else:
        actual_frame_data = frame_data
        num_live = elf.u16_at_addr(addr + 6)
        p = addr + 8 + 2 * num_live

    num_allocs = 0
    if actual_frame_data & FRAME_DESCRIPTOR_ALLOC:
        num_allocs = elf.u8_at_addr(p)
        p += 1 + num_allocs
    if actual_frame_data & FRAME_DESCRIPTOR_DEBUG:
        p = align(p, 4)
        p += 4 * (num_allocs if (actual_frame_data & FRAME_DESCRIPTOR_ALLOC) else 1)
    return align(p, 8)


def find_master_frametable(elf: Elf64) -> tuple[int, int]:
    for sym in elf.symbols():
        if sym.name == "caml_frametable":
            return sym.value, sym.size
    raise ValueError("could not find caml_frametable")


def writable_load_for_addr(elf: Elf64, addr: int) -> ProgramHeader:
    for ph in elf.program_headers:
        if ph.p_type != PT_LOAD or not (ph.flags & PF_W):
            continue
        if ph.vaddr <= addr < ph.vaddr + ph.memsz:
            return ph
    raise ValueError(f"0x{addr:x}: no writable LOAD segment")


def next_load_file_offset(elf: Elf64, ph: ProgramHeader) -> int:
    later = [
        other.file_offset
        for other in elf.program_headers
        if other.p_type == PT_LOAD and other.file_offset > ph.file_offset
    ]
    return min(later) if later else len(elf.data)


def synthesize_icp_frametable(
    output_elf: Elf64,
    descriptor_by_pc: dict[int, bytes],
    unsupported_icp_returns: list[UnsupportedICPReturn],
) -> tuple[int, list[str]]:
    if not unsupported_icp_returns:
        return 0, []

    master_addr, master_size = find_master_frametable(output_elf)
    master_entries = master_size // 8 if master_size else 100_000
    terminator_addr = None
    for i in range(master_entries):
        entry_addr = master_addr + 8 * i
        if output_elf.u64_at_addr(entry_addr) == 0:
            terminator_addr = entry_addr
            break
    if terminator_addr is None:
        raise ValueError("could not find caml_frametable terminator")
    if output_elf.u64_at_addr(terminator_addr + 8) != 0:
        raise ValueError(
            f"caml_frametable terminator at 0x{terminator_addr:x} has no spare zero slot"
        )

    master_load = writable_load_for_addr(output_elf, master_addr)
    synth_addr = align(master_load.vaddr + master_load.memsz, 8)
    synth_file_offset = master_load.file_offset + (synth_addr - master_load.vaddr)

    synthetic_descriptors: list[tuple[UnsupportedICPReturn, bytes]] = []
    missing: list[str] = []
    for item in unsupported_icp_returns:
        source = descriptor_by_pc.get(item.shared_return_addr)
        if source is None:
            if len(missing) < 20:
                missing.append(
                    f"{item.function}: no source descriptor for shared return "
                    f"0x{item.shared_return_addr:x}"
                )
            continue
        synthetic_descriptors.append((item, source))

    if missing:
        return 0, missing

    table = bytearray(struct.pack("<Q", len(synthetic_descriptors)))
    for item, source in synthetic_descriptors:
        desc_addr = synth_addr + len(table)
        desc = bytearray(source)
        rel = item.return_addr - desc_addr
        if rel < -(1 << 31) or rel >= (1 << 31):
            raise OverflowError(
                f"0x{item.return_addr:x}: synthetic descriptor at 0x{desc_addr:x} "
                "does not fit int32 relative retaddr"
            )
        struct.pack_into("<i", desc, 0, rel)
        table.extend(desc)

    synth_end = synth_file_offset + len(table)
    next_load = next_load_file_offset(output_elf, master_load)
    if synth_end > next_load:
        raise ValueError(
            f"synthetic frametable ending at file offset 0x{synth_end:x} "
            f"would overlap next LOAD at 0x{next_load:x}"
        )
    if synth_end > len(output_elf.data):
        output_elf.data.extend(b"\0" * (synth_end - len(output_elf.data)))
    output_elf.data[synth_file_offset:synth_end] = table

    new_filesz = synth_end - master_load.file_offset
    new_memsz = synth_addr + len(table) - master_load.vaddr
    output_elf.put_u64_at(master_load.offset + 32, new_filesz)
    output_elf.put_u64_at(master_load.offset + 40, new_memsz)
    output_elf.put_u64_at_addr(terminator_addr, synth_addr)
    output_elf.put_u64_at_addr(terminator_addr + 8, 0)
    return len(synthetic_descriptors), [
        f"synthesized {len(synthetic_descriptors)} ICP frame descriptors "
        f"at 0x{synth_addr:x}"
    ]


def patch_frametables(
    input_elf: Elf64, output_elf: Elf64, synthesize_icp_descriptors: bool
) -> tuple[int, int, list[str]]:
    bat = parse_bat(output_elf)
    translator = Translator(input_elf, output_elf, bat)
    call_return_map, unsupported_icp_returns = build_call_return_map(input_elf, output_elf, bat)
    input_tables_by_name = unique_by_name(frametable_symbols(input_elf))
    output_tables = frametable_symbols(output_elf)
    patched = 0
    call_mapped = 0
    bat_mapped = 0
    unresolved = 0
    samples: list[str] = []
    descriptor_by_pc: dict[int, bytes] = {}

    for out_table in output_tables:
        in_table = input_tables_by_name.get(out_table.name)
        if in_table is None:
            continue
        count = output_elf.u64_at_addr(out_table.value)
        if count > 2_000_000:
            raise ValueError(f"{out_table.name}: implausible descriptor count {count}")
        out_addr = out_table.value + 8
        in_addr = in_table.value + 8
        for i in range(count):
            old_rel = output_elf.i32_at_addr(out_addr)
            old_pc = in_addr + old_rel
            new_pc = call_return_map.get(old_pc)
            if new_pc is not None:
                call_mapped += 1
            else:
                translated = translator.translate_pc(old_pc)
                if translated is None:
                    unresolved += 1
                    if len(samples) < 20:
                        samples.append(f"{out_table.name}[{i}]: cannot translate old PC 0x{old_pc:x}")
                else:
                    new_pc, _name = translated
                    bat_mapped += 1
            if new_pc is not None:
                output_elf.put_i32_at_addr(out_addr, new_pc - out_addr)
                patched += 1
            next_out = descriptor_end(output_elf, out_addr)
            if new_pc is not None:
                descriptor_by_pc[new_pc] = bytes(
                    output_elf.data[output_elf.off_for_addr(out_addr) : output_elf.off_for_addr(next_out)]
                )
            in_addr += next_out - out_addr
            out_addr = next_out
    if unsupported_icp_returns:
        if synthesize_icp_descriptors:
            synthesized, synth_samples = synthesize_icp_frametable(
                output_elf, descriptor_by_pc, unsupported_icp_returns
            )
            samples.extend(synth_samples)
            missing = len(unsupported_icp_returns) - synthesized
            if missing:
                unresolved += missing
        else:
            unresolved += len(unsupported_icp_returns)
            samples.extend(item.describe() for item in unsupported_icp_returns[:20])
    samples.insert(0, f"call-site mapped {call_mapped}; BAT fallback mapped {bat_mapped}")
    return patched, unresolved, samples


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input", type=Path, help="pre-BOLT executable with old text addresses")
    parser.add_argument("bolted", type=Path, help="BOLT output produced with --enable-bat")
    parser.add_argument("output", type=Path, help="patched executable to write")
    parser.add_argument(
        "--synthesize-icp-descriptors",
        action="store_true",
        help="append a synthetic frametable for BOLT-created ICP direct-call return PCs",
    )
    args = parser.parse_args()

    shutil.copy2(args.bolted, args.output)
    input_elf = Elf64(args.input)
    output_elf = Elf64(args.output)
    patched, unresolved, samples = patch_frametables(
        input_elf, output_elf, args.synthesize_icp_descriptors
    )
    output_elf.write(args.output)
    mode = os.stat(args.bolted).st_mode
    os.chmod(args.output, mode | 0o111)
    print(f"patched {patched} frame descriptor return addresses")
    print(f"unresolved {unresolved} frame descriptor return addresses")
    for sample in samples:
        print(sample)
    if unresolved:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
