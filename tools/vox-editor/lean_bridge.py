#!/usr/bin/env python3
"""Layer 2: goals and hypotheses inside [%%vox.lean] blocks.

The vox compiler splices each embedded ``[%%vox.lean {lean| ... |lean}]``
block VERBATIM into the Lean solver input.  We recover that input with a
solver-path WRAPPER (the same trick docs/vox/generate.py uses: the
compiler invokes the solver as ``solver <leanfile>``, so a wrapper that
copies ``$1`` captures the generated Lean).  Because the block text is
copied verbatim, a substring search locates it in the generated file and
maps a cursor in block source to a cursor in the generated Lean.

Two ways to report the proof state at that cursor:

  * LIVE (``goal_at_source_pos``): drive ``lean --server`` and ask
    ``$/lean/plainGoal`` at the mapped position.  This is the true
    intermediate proof state (it reflects ``have`` steps, rewrites, ...).
    The generated file is module-mode (``import VoxCore``); the LSP wants
    an IR data file the batch build discards, so we instead rewrite the
    file into a self-contained one, inlining VoxCore's (small, stable)
    base theory in place of the import.  Blocks that import ANOTHER
    unit's sig module are out of scope for the prototype (single-file
    editor) and reported as such.

  * STATIC (``enclosing_theorem``): with no Lean process, parse the
    theorem enclosing the cursor and present its binders as hypotheses
    and its stated type as the goal.  Deterministic and instant; used by
    the front end for immediate feedback and as a fallback.

Positions here are 0-based line and character (LSP/CodeMirror
convention), unlike the 1-based lines vc_index reports.
"""

import io
import json
import os
import re
import subprocess
import time
from typing import Dict, List, Optional, Tuple, cast

# VoxCore's base theory, inlined (without the module header and `public`
# markers) to make a module-mode solver input self-contained.  Kept in
# sync with typing/vox_module.ml (lean_iarray_theory, lean_tuple_decl,
# core_text); if that drifts, test_lean_bridge's VoxU test fails loudly.
_MAX_TUPLE_ARITY = 8


def _voxcore_body() -> str:
    lines = [
        "opaque VoxU : Type",
        "opaque VoxIA : Type",
        "opaque Vox_ia_len : VoxIA -> Int",
        "opaque Vox_ia_get : VoxIA -> Int -> Int",
        "axiom Vox_ia_len_nonneg (a : VoxIA) : 0 <= Vox_ia_len a",
        "grind_pattern Vox_ia_len_nonneg => Vox_ia_len a",
    ]
    for n in range(2, _MAX_TUPLE_ARITY + 1):
        us = ", ".join("u%d" % i for i in range(1, n + 1))
        ts = "".join(" (t%d : Sort u%d)" % (i, i) for i in range(1, n + 1))
        univ = "1"
        for i in range(n, 0, -1):
            univ = "max u%d (%s)" % (i, univ)
        ps = "".join(" (p%d : t%d)" % (i, i) for i in range(1, n + 1))
        lines.append(
            "structure VoxT%d.{%s}%s : Sort (%s) where%s" % (n, us, ts, univ, ps)
        )
    return "\n".join(lines) + "\n"


# --- block extraction ------------------------------------------------------

_BLOCK_OPEN = "{lean|"
_BLOCK_CLOSE = "|lean}"


class Block:
    def __init__(
        self, content: str, content_offset: int, start_line: int, start_col: int
    ) -> None:
        self.content = content
        self.content_offset = content_offset  # char offset in source
        self.start_line = start_line  # 0-based
        self.start_col = start_col  # 0-based


def find_lean_blocks(source: str) -> List[Block]:
    """All [%%vox.lean {lean| ... |lean}] block contents, in order."""
    blocks: List[Block] = []
    i = 0
    while True:
        j = source.find(_BLOCK_OPEN, i)
        if j < 0:
            break
        cstart = j + len(_BLOCK_OPEN)
        k = source.find(_BLOCK_CLOSE, cstart)
        if k < 0:
            break
        content = source[cstart:k]
        line, col = offset_to_linecol(source, cstart)
        blocks.append(Block(content, cstart, line, col))
        i = k + len(_BLOCK_CLOSE)
    return blocks


def offset_to_linecol(text: str, offset: int) -> Tuple[int, int]:
    """0-based (line, col) of a character offset."""
    prefix = text[:offset]
    line = prefix.count("\n")
    last_nl = prefix.rfind("\n")
    col = offset - (last_nl + 1)
    return line, col


def linecol_to_offset(text: str, line: int, col: int) -> int:
    """Char offset of a 0-based (line, col)."""
    lines = text.split("\n")
    off = 0
    for i in range(min(line, len(lines))):
        off += len(lines[i]) + 1
    return off + col


def block_at(source: str, line: int, col: int) -> Optional[Block]:
    """The block whose content contains the 0-based cursor, if any."""
    off = linecol_to_offset(source, line, col)
    for b in find_lean_blocks(source):
        if b.content_offset <= off <= b.content_offset + len(b.content):
            return b
    return None


# --- generated-Lean capture and self-containment ---------------------------


def capture_generated(
    source_path: str,
    ocamlc: str,
    lean: str,
    cwd: Optional[str] = None,
) -> Optional[str]:
    """Compile ``source_path`` with a wrapper solver that saves the
    generated Lean, and return that text (or None if none was produced)."""
    work = cwd or os.path.dirname(os.path.abspath(source_path))
    saved = os.path.join(work, "_vox_generated.lean")
    wrapper = os.path.join(work, "_vox_leansave.sh")
    with open(wrapper, "w") as fh:
        fh.write('#!/bin/sh\ncp "$1" %s\nexec %s "$@"\n' % (saved, lean))
    os.chmod(wrapper, 0o755)
    subprocess.run(
        [ocamlc, "-c", "-vox-solver-path", wrapper, source_path],
        cwd=work,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        universal_newlines=True,
    )
    if not os.path.exists(saved):
        return None
    with open(saved) as fh:
        return fh.read()


def imports_sig_module(generated: str) -> bool:
    """Does the generated file import another unit's sig module?"""
    return bool(_sig_imports(generated))


def _sig_imports(text: str) -> List[str]:
    """The VoxSig_* modules ``text`` imports, in file order (each name
    including the ``VoxSig_`` prefix, e.g. ``VoxSig_Vhof``)."""
    return re.findall(r"^\s*public import (VoxSig_\w+)", text, flags=re.MULTILINE)


def _read_sig_source(sig_dir: str, module: str) -> Optional[str]:
    """The captured Lean source of sig ``module`` (``VoxSig_<M>``) staged in
    ``sig_dir`` as ``<module>.leansrc``, or None if it was not staged."""
    path = os.path.join(sig_dir, module + ".leansrc")
    if os.path.isfile(path):
        with open(path) as fh:
            return fh.read()
    return None


def _strip_public(line: str) -> str:
    """Drop a ``public`` declaration modifier, whether it starts the line
    (``public def ...``) or follows an attribute (``@[grind] public def
    ...``).  ``public`` is invalid outside a module."""
    line = re.sub(r"^(\s*)public ", r"\1", line)
    line = re.sub(r"(\]\s*)public ", r"\1", line)
    return line


def _strip_module_scaffolding(text: str) -> str:
    """Turn a captured module-mode body (a client's or a sig's generated
    Lean) into inline declarations: drop the ``module`` line, drop every
    ``import`` (VoxCore and the sig imports are inlined separately by the
    caller; ``import Lean`` cannot appear after declarations), and strip
    ``public`` markers."""
    out: List[str] = []
    for line in text.split("\n"):
        stripped = line.strip()
        if stripped == "module":
            continue
        if re.match(r"(public )?import ", stripped):
            continue
        out.append(_strip_public(line))
    return "\n".join(out)


def _collect_sig_bodies(generated: str, sig_dir: str) -> str:
    """Concatenated inline bodies of every VoxSig module ``generated``
    imports, transitively, dependencies before dependents and each at most
    once.  A sig whose ``.leansrc`` is not staged in ``sig_dir`` is skipped
    (its dependents may then fail to elaborate, which the pane surfaces as
    no goal -- better than crashing)."""
    order: List[str] = []
    seen: set = set()

    def visit(module: str) -> None:
        if module in seen:
            return
        seen.add(module)
        raw = _read_sig_source(sig_dir, module)
        if raw is None:
            return
        for dep in _sig_imports(raw):
            visit(dep)
        order.append(_strip_module_scaffolding(raw))

    for module in _sig_imports(generated):
        visit(module)
    return "\n".join(order)


def to_self_contained(generated: str, sig_dir: Optional[str] = None) -> str:
    """Rewrite a module-mode solver input into a self-contained Lean file
    the LSP server can elaborate: drop the ``module`` line, inline VoxCore
    in place of its import, inline each imported ``VoxSig_*`` module's body
    (recursively, dependencies first) from ``sig_dir``, and strip ``public``
    markers (invalid outside a module).

    The interactive server cannot ``import`` the batch-built VoxCore/VoxSig
    oleans -- they carry no IR data, so an import fails with "missing IR
    data file" -- so their content is inlined instead.  ``sig_dir`` is the
    scratch/work dir where /check and /goal stage the dependencies'
    ``VoxSig_<M>.leansrc`` sources; with ``sig_dir=None`` (or a sig not
    staged) the imports are dropped, as before."""
    inlined_sigs = _collect_sig_bodies(generated, sig_dir) if sig_dir else ""
    out: List[str] = []
    for line in generated.split("\n"):
        stripped = line.strip()
        if stripped == "module":
            continue
        if re.match(r"public import VoxCore\b", stripped):
            out.append(_voxcore_body().rstrip("\n"))
            if inlined_sigs:
                out.append(inlined_sigs)
            continue
        if stripped.startswith("public import "):
            # VoxSig imports: content is inlined above (or dropped when its
            # source was not staged).  Any other public import is
            # unsupported here; drop and let elaboration surface the gap.
            continue
        out.append(_strip_public(line))
    return "\n".join(out)


def locate_content(content: str, text: str) -> int:
    """Char offset where ``content`` first appears in ``text`` (-1)."""
    return text.find(content)


def map_source_to_generated(
    source: str,
    generated_self_contained: str,
    line: int,
    col: int,
) -> Optional[Tuple[int, int]]:
    """Map a 0-based cursor in block source to a 0-based cursor in the
    self-contained generated Lean, or None if not inside a block or the
    block cannot be located."""
    b = block_at(source, line, col)
    if b is None:
        return None
    off = linecol_to_offset(source, line, col)
    rel = off - b.content_offset
    gstart = locate_content(b.content, generated_self_contained)
    if gstart < 0:
        return None
    return offset_to_linecol(generated_self_contained, gstart + rel)


# --- static tier: parse the enclosing theorem ------------------------------

_THEOREM_RE = re.compile(
    r"^\s*(?:@\[[^\]]*\]\s*)*(?:public\s+)?"
    r"(theorem|lemma|example|def)\b"
)


def enclosing_theorem(content: str, rel_line: int) -> Optional[Dict[str, object]]:
    """Given block content and a 0-based line within it, return the
    enclosing declaration's binders (as hypotheses) and stated goal,
    parsed statically.  Returns {"name", "hypotheses", "goal"} or None."""
    lines = content.split("\n")
    # Find the declaration header at or above rel_line.
    start = None
    for i in range(min(rel_line, len(lines) - 1), -1, -1):
        if _THEOREM_RE.match(lines[i]):
            start = i
            break
    if start is None:
        return None
    # Accumulate the header until ':=' (proof start) or end.
    header_parts: List[str] = []
    for i in range(start, len(lines)):
        header_parts.append(lines[i])
        if ":=" in lines[i]:
            break
    header = "\n".join(header_parts)
    header = header.split(":=", 1)[0]
    m = re.match(
        r"\s*(?:@\[[^\]]*\]\s*)*(?:public\s+)?"
        r"(?:theorem|lemma|example|def)\s+([A-Za-z_][\w'.]*)",
        header,
    )
    name = m.group(1) if m else "<anonymous>"
    rest = header[m.end() :] if m else header
    # Split binders (parenthesised / braced / bracketed groups) from the
    # goal (everything after the top-level ':').
    binders, goal = _split_binders_and_goal(rest)
    return {"name": name, "hypotheses": binders, "goal": goal.strip()}


def theorems_in_source(source: str) -> List[Dict[str, object]]:
    """Every declaration in every [%%vox.lean] block, with its static
    hypotheses/goal and its 0-based source range.  Used to make block
    theorems selectable client-side (no Lean process)."""
    result: List[Dict[str, object]] = []
    for b in find_lean_blocks(source):
        lines = b.content.split("\n")
        matches = [
            (i, m.group(1))
            for i, l in enumerate(lines)
            for m in [_THEOREM_RE.match(l)]
            if m
        ]
        headers = [i for i, _ in matches]
        # ``def``s bound the regions but are DEFINITIONS, not statements:
        # rendering one as "theorem" with a goal that swallows the
        # following declarations (pattern-match defs have no ``:=`` on
        # the header line) was pure confusion.
        stmt = {i for i, kw in matches if kw != "def"}
        # Prefix char offsets of each content line.
        prefix = [0]
        for l in lines:
            prefix.append(prefix[-1] + len(l) + 1)
        for idx, h in enumerate(headers):
            if h not in stmt:
                continue
            end_line = (
                headers[idx + 1] - 1 if idx + 1 < len(headers) else len(lines) - 1
            )
            info = enclosing_theorem(b.content, h)
            if info is None:
                continue
            start_off = b.content_offset + prefix[h]
            end_off = b.content_offset + prefix[end_line] + len(lines[end_line])
            sline, scol = offset_to_linecol(source, start_off)
            eline, ecol = offset_to_linecol(source, end_off)
            result.append(
                {
                    "name": info["name"],
                    "hypotheses": info["hypotheses"],
                    "goal": info["goal"],
                    "start": {"line": sline, "col": scol},
                    "end": {"line": eline, "col": ecol},
                }
            )
    return result


def _split_binders_and_goal(text: str) -> Tuple[List[str], str]:
    binders: List[str] = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch.isspace():
            i += 1
            continue
        if ch in "({[":
            close = {"(": ")", "{": "}", "[": "]"}[ch]
            depth = 0
            j = i
            while j < n:
                if text[j] == ch:
                    depth += 1
                elif text[j] == close:
                    depth -= 1
                    if depth == 0:
                        break
                j += 1
            binders.append(text[i : j + 1].strip())
            i = j + 1
        elif ch == ":":
            return binders, text[i + 1 :]
        else:
            # A non-binder token before ':' (e.g. an implicit-free goal);
            # stop and treat the remainder as the goal.
            return binders, text[i:]
    return binders, ""


# --- LSP client ------------------------------------------------------------


class LeanServer:
    """A minimal LSP client for ``lean --server`` over stdio."""

    def __init__(
        self,
        lean_bin: str,
        env: Optional[Dict[str, str]] = None,
        cwd: Optional[str] = None,
    ) -> None:
        self._proc = subprocess.Popen(
            [lean_bin, "--server"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env,
            cwd=cwd,
        )
        self._id = 0
        self._buf = b""

    @property
    def _stdin(self):
        assert self._proc.stdin is not None
        return self._proc.stdin

    @property
    def _stdout(self) -> "io.BufferedReader":
        assert self._proc.stdout is not None
        return cast("io.BufferedReader", self._proc.stdout)

    def _send(self, obj: Dict[str, object]) -> None:
        body = json.dumps(obj).encode("utf-8")
        self._stdin.write(b"Content-Length: %d\r\n\r\n%s" % (len(body), body))
        self._stdin.flush()

    def _request(self, method: str, params: Dict[str, object]) -> int:
        self._id += 1
        self._send(
            {"jsonrpc": "2.0", "id": self._id, "method": method, "params": params}
        )
        return self._id

    def _notify(self, method: str, params: Dict[str, object]) -> None:
        self._send({"jsonrpc": "2.0", "method": method, "params": params})

    def _read(self, timeout: float) -> Optional[Dict[str, object]]:
        deadline = time.time() + timeout
        while b"\r\n\r\n" not in self._buf:
            if time.time() > deadline:
                return None
            chunk = self._stdout.read1(65536)
            if not chunk:
                return None
            self._buf += chunk
        header, rest = self._buf.split(b"\r\n\r\n", 1)
        length = 0
        for hl in header.split(b"\r\n"):
            if hl.lower().startswith(b"content-length:"):
                length = int(hl.split(b":")[1].strip())
        while len(rest) < length:
            if time.time() > deadline:
                return None
            chunk = self._stdout.read1(65536)
            if not chunk:
                return None
            rest += chunk
        self._buf = rest[length:]
        return json.loads(rest[:length].decode("utf-8"))

    def _await(self, rid: int, timeout: float) -> Optional[Dict[str, object]]:
        deadline = time.time() + timeout
        while time.time() < deadline:
            msg = self._read(deadline - time.time())
            if msg is None:
                return None
            if msg.get("id") == rid and ("result" in msg or "error" in msg):
                return msg
        return None

    def initialize(self, root: str) -> None:
        rid = self._request(
            "initialize",
            {
                "processId": os.getpid(),
                "rootUri": "file://" + root,
                "capabilities": {},
            },
        )
        self._await(rid, 60)
        self._notify("initialized", {})

    def open_wait(self, uri: str, text: str, timeout: float = 60) -> None:
        self._notify(
            "textDocument/didOpen",
            {
                "textDocument": {
                    "uri": uri,
                    "languageId": "lean4",
                    "version": 1,
                    "text": text,
                }
            },
        )
        deadline = time.time() + timeout
        while time.time() < deadline:
            msg = self._read(deadline - time.time())
            if msg is None:
                break
            if msg.get("method") == "$/lean/fileProgress":
                params = msg.get("params")
                if isinstance(params, dict) and not params.get("processing"):
                    return

    def plain_goal(
        self, uri: str, line: int, col: int, timeout: float = 30
    ) -> Optional[List[str]]:
        rid = self._request(
            "$/lean/plainGoal",
            {
                "textDocument": {"uri": uri},
                "position": {"line": line, "character": col},
            },
        )
        resp = self._await(rid, timeout)
        if resp is None:
            return None
        result = resp.get("result")
        if isinstance(result, dict):
            goals = result.get("goals")
            if isinstance(goals, list):
                return [str(g) for g in goals]
        return None

    def close(self) -> None:
        for stream in (self._proc.stdin, self._proc.stdout, self._proc.stderr):
            try:
                if stream is not None:
                    stream.close()
            except Exception:
                pass
        try:
            self._proc.terminate()
            self._proc.wait(timeout=5)
        except Exception:
            pass


# --- end-to-end live query -------------------------------------------------


class BlockGoal:
    def __init__(
        self, status: str, goals: Optional[List[str]] = None, detail: str = ""
    ) -> None:
        self.status = status  # "ok" | "not_in_block" | "unsupported" | "no_goal"
        self.goals = goals or []
        self.detail = detail

    def to_json(self) -> Dict[str, object]:
        return {"status": self.status, "goals": self.goals, "detail": self.detail}


def goal_at_source_pos(
    source_path: str,
    ocamlc: str,
    lean: str,
    line: int,
    col: int,
    cwd: Optional[str] = None,
) -> BlockGoal:
    """Live proof state inside a [%%vox.lean] block at a 0-based cursor."""
    with open(source_path) as fh:
        source = fh.read()
    if block_at(source, line, col) is None:
        return BlockGoal("not_in_block")
    generated = capture_generated(source_path, ocamlc, lean, cwd=cwd)
    if generated is None:
        return BlockGoal("unsupported", detail="no solver input produced")
    work = cwd or os.path.dirname(os.path.abspath(source_path))
    # Imported VoxSig modules are inlined from the sig sources staged in the
    # work dir (VoxSig_<M>.leansrc); a multi-module buffer now renders.
    self_contained = to_self_contained(generated, sig_dir=work)
    mapped = map_source_to_generated(source, self_contained, line, col)
    if mapped is None:
        return BlockGoal("unsupported", detail="could not locate block text")
    gline, gcol = mapped
    lean_path = os.path.join(work, "_vox_self_contained.lean")
    with open(lean_path, "w") as fh:
        fh.write(self_contained)
    server = LeanServer(lean, cwd=work)
    try:
        server.initialize(work)
        uri = "file://" + os.path.abspath(lean_path)
        server.open_wait(uri, self_contained)
        goals = server.plain_goal(uri, gline, gcol)
    finally:
        server.close()
    if goals is None:
        return BlockGoal("no_goal", detail="no proof state at this position")
    return BlockGoal("ok", goals=goals)


def main() -> None:
    import argparse

    ap = argparse.ArgumentParser()
    ap.add_argument("source")
    ap.add_argument("line", type=int, help="0-based line")
    ap.add_argument("col", type=int, help="0-based column")
    ap.add_argument("--ocamlc", required=True)
    ap.add_argument("--lean", required=True)
    args = ap.parse_args()
    bg = goal_at_source_pos(args.source, args.ocamlc, args.lean, args.line, args.col)
    print(json.dumps(bg.to_json(), indent=2))


if __name__ == "__main__":
    main()
