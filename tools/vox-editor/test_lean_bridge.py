#!/usr/bin/env python3
"""Tests for lean_bridge.

Pure-logic tests (block extraction, offset mapping, static theorem parse,
self-containment) run everywhere.  Live LSP tests run only when the
pinned Lean is available; the full source->goal test also needs a built
ocamlc.
"""

import os
import tempfile
import unittest

import lean_bridge  # pyright: ignore[reportImplicitRelativeImport]

SOURCE = """\
let x = 1

[%%vox.lean {lean|
theorem foo (n : Int) (h : 2 <= n) : n + 1 >= 3 := by
  have k : n >= 2 := h
  omega
|lean}]

let y = 2
"""


class TestBlocks(unittest.TestCase):
    def test_find(self):
        blocks = lean_bridge.find_lean_blocks(SOURCE)
        self.assertEqual(len(blocks), 1)
        b = blocks[0]
        self.assertIn("theorem foo", b.content)
        self.assertTrue(b.content.startswith("\ntheorem foo"))
        # The content offset points just after "{lean|".
        self.assertEqual(SOURCE[b.content_offset : b.content_offset + 1], "\n")

    def test_block_at(self):
        # A cursor on the "omega" line is inside the block.
        omega_line = None
        for i, line in enumerate(SOURCE.split("\n")):
            if "omega" in line:
                omega_line = i
        assert omega_line is not None
        self.assertIsNotNone(lean_bridge.block_at(SOURCE, omega_line, 2))
        # A cursor on "let x = 1" is not.
        self.assertIsNone(lean_bridge.block_at(SOURCE, 0, 4))

    def test_offset_roundtrip(self):
        for off in (0, 5, 20, len(SOURCE) - 1):
            line, col = lean_bridge.offset_to_linecol(SOURCE, off)
            self.assertEqual(lean_bridge.linecol_to_offset(SOURCE, line, col), off)


class TestSelfContained(unittest.TestCase):
    GEN = (
        "module\n"
        "public import VoxCore\n"
        "public inductive Ilist where | INil | ICons\n"
        "@[grind] def fib (n : Int) : Int := n\n"
        "theorem t : fib 0 = 0 := by grind\n"
        "set_option maxHeartbeats 400000\n"
        "theorem vc_0 : True := by trivial\n"
    )

    def test_strips_and_inlines(self):
        sc = lean_bridge.to_self_contained(self.GEN)
        self.assertNotIn("module\n", "\n" + sc + "\n")
        self.assertNotIn("public import VoxCore", sc)
        self.assertNotIn("public ", sc)  # markers stripped
        self.assertIn("opaque VoxU : Type", sc)  # VoxCore inlined
        self.assertIn("structure VoxT2", sc)
        self.assertIn("inductive Ilist", sc)

    def test_sig_module_detection(self):
        self.assertFalse(lean_bridge.imports_sig_module(self.GEN))
        self.assertTrue(
            lean_bridge.imports_sig_module("module\npublic import VoxSig_Foo\n")
        )


class TestStaticTheorem(unittest.TestCase):
    def test_parse(self):
        content = SOURCE[lean_bridge.find_lean_blocks(SOURCE)[0].content_offset :]
        # rel line of "omega" within the block content:
        rel = None
        for i, line in enumerate(content.split("\n")):
            if "omega" in line:
                rel = i
        assert rel is not None
        info = lean_bridge.enclosing_theorem(content, rel)
        assert info is not None
        self.assertEqual(info["name"], "foo")
        self.assertEqual(info["hypotheses"], ["(n : Int)", "(h : 2 <= n)"])
        self.assertEqual(info["goal"], "n + 1 >= 3")

    def test_none_outside(self):
        self.assertIsNone(lean_bridge.enclosing_theorem("-- just a comment\n", 0))

    def test_theorems_in_source(self):
        thms = lean_bridge.theorems_in_source(SOURCE)
        self.assertEqual(len(thms), 1)
        t = thms[0]
        self.assertEqual(t["name"], "foo")
        self.assertEqual(t["goal"], "n + 1 >= 3")
        # The range covers the "theorem foo" line in the source.
        start = t["start"]
        assert isinstance(start, dict)
        line = start["line"]
        assert isinstance(line, int)
        self.assertTrue(SOURCE.split("\n")[line].startswith("theorem foo"))


class TestMapping(unittest.TestCase):
    def test_map_into_generated(self):
        # Fabricate a self-contained file where the block content appears
        # verbatim; map a cursor in source to the generated position.
        block = lean_bridge.find_lean_blocks(SOURCE)[0]
        generated = "opaque VoxU : Type\n" + block.content + "\n"
        # cursor at start of "theorem foo" in the source
        src_line, src_col = block.start_line + 1, 0
        mapped = lean_bridge.map_source_to_generated(
            SOURCE, generated, src_line, src_col
        )
        assert mapped is not None
        gline, gcol = mapped
        gen_lines = generated.split("\n")
        self.assertTrue(gen_lines[gline].startswith("theorem foo"))
        self.assertEqual(gcol, 0)


# --- live LSP tests --------------------------------------------------------


def _find_lean():
    env = os.environ.get("VOX_LEAN")
    if env and os.path.exists(env):
        return env
    pinned = "/nix/store/h6z4nr52r2x6v7ygqg59cl8nzjg0yxcy-lean4-4.31.0/bin/lean"
    return pinned if os.path.exists(pinned) else None


def _find_ocamlc():
    env = os.environ.get("VOX_OCAMLC")
    if env and os.path.exists(env):
        return env
    here = os.path.dirname(os.path.abspath(__file__))
    root = os.path.dirname(os.path.dirname(here))
    cand = os.path.join(root, "_build", "_bootinstall", "bin", "ocamlc.opt")
    return cand if os.path.exists(cand) else None


LEAN = _find_lean()
OCAMLC = _find_ocamlc()


@unittest.skipUnless(LEAN, "no lean (set VOX_LEAN)")
class TestLiveVoxCore(unittest.TestCase):
    def test_inlined_voxcore_elaborates(self):
        # A generated-style file that USES VoxCore (VoxU); after
        # self-containment the LSP must still give a goal at the cursor.
        assert LEAN is not None
        gen = (
            "module\n"
            "public import VoxCore\n"
            "opaque g : VoxU -> Int\n"
            "axiom gpos (x : VoxU) : g x >= 0\n"
            "theorem t (x : VoxU) : g x >= 0 := by\n"
            "  grind [gpos]\n"
        )
        sc = lean_bridge.to_self_contained(gen)
        d = tempfile.mkdtemp(prefix="voxbridge")
        path = os.path.join(d, "sc.lean")
        with open(path, "w") as fh:
            fh.write(sc)
        # find the "grind [gpos]" line in sc
        gline = None
        for i, line in enumerate(sc.split("\n")):
            if "grind [gpos]" in line:
                gline = i
        assert gline is not None
        server = lean_bridge.LeanServer(LEAN, cwd=d)
        try:
            server.initialize(d)
            uri = "file://" + path
            server.open_wait(uri, sc)
            goals = server.plain_goal(uri, gline, 2)
        finally:
            server.close()
        assert goals is not None
        joined = "\n".join(goals)
        self.assertIn("g x", joined)
        self.assertIn("VoxU", joined)


@unittest.skipUnless(LEAN and OCAMLC, "need lean + ocamlc")
class TestLiveEndToEnd(unittest.TestCase):
    FIB = """\
let rec total_ fib n =
  if n <= 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)
[@@vox.decreases n]

[%%vox.lean {lean|
theorem fib_rec (n : Int) (h : 2 <= n) : fib n = fib (n - 1) + fib (n - 2) := by
  grind
|lean}]

let use () =
  let refine_ ok = (fib 2 : int{ _ = 1 }) in
  ok
"""

    def test_goal_in_block(self):
        assert LEAN is not None and OCAMLC is not None
        d = tempfile.mkdtemp(prefix="voxbridge")
        path = os.path.join(d, "emb.ml")
        with open(path, "w") as fh:
            fh.write(self.FIB)
        # cursor on the "grind" line inside the block (source line index 6)
        grind_line = None
        for i, line in enumerate(self.FIB.split("\n")):
            if line.strip() == "grind":
                grind_line = i
        assert grind_line is not None
        bg = lean_bridge.goal_at_source_pos(path, OCAMLC, LEAN, grind_line, 2, cwd=d)
        self.assertEqual(bg.status, "ok", msg=bg.detail)
        joined = "\n".join(bg.goals)
        self.assertIn("fib n", joined)
        self.assertIn("n : Int", joined)

    def test_not_in_block(self):
        assert LEAN is not None and OCAMLC is not None
        d = tempfile.mkdtemp(prefix="voxbridge")
        path = os.path.join(d, "emb.ml")
        with open(path, "w") as fh:
            fh.write(self.FIB)
        bg = lean_bridge.goal_at_source_pos(path, OCAMLC, LEAN, 0, 4, cwd=d)
        self.assertEqual(bg.status, "not_in_block")


if __name__ == "__main__":
    unittest.main()
