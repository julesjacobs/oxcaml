#!/usr/bin/env python3
"""Tests for workspace: the file-explorer tree, safe path resolution, and
the stdlib dependency/manifest logic.  These touch only committed source
files (no compiler / Lean), so they run unconditionally."""

import os
import unittest

import workspace as w  # pyright: ignore[reportImplicitRelativeImport]


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


def _files(node):
    """All file nodes under a tree node, flattened."""
    if node.get("type") == "file":
        return [node]
    out = []
    for c in node.get("children", []):
        out.extend(_files(c))
    return out


class TestTree(unittest.TestCase):
    def setUp(self):
        self.tree = w.list_tree()
        self.roots = {r["id"]: r for r in self.tree["roots"]}

    def test_two_roots(self):
        self.assertEqual(set(self.roots), {"examples", "stdlib"})

    def test_examples_have_titles_and_default(self):
        files = _files(self.roots["examples"])
        self.assertTrue(files)
        self.assertTrue(all(f["path"].startswith("examples/") for f in files))
        self.assertTrue(all("title" in f for f in files))
        self.assertEqual(sum(1 for f in files if f.get("default")), 1)

    def test_stdlib_lists_sources_notes_and_clients(self):
        paths = [f["path"] for f in _files(self.roots["stdlib"])]
        self.assertIn("stdlib/voption.ml", paths)
        self.assertIn("stdlib/voption.mli", paths)
        # notes/*.md docs and client smokes are reachable subdirs.
        self.assertTrue(
            any(p.startswith("stdlib/notes/") and p.endswith(".md") for p in paths)
        )
        self.assertTrue(any(p.startswith("stdlib/clients/") for p in paths))

    def test_interface_before_impl(self):
        # A module's .mli lists before its .ml (grouped, interface first).
        top = [c for c in self.roots["stdlib"]["children"] if c["type"] == "file"]
        names = [c["name"] for c in top]
        self.assertIn("voption.mli", names)
        self.assertLess(names.index("voption.mli"), names.index("voption.ml"))

    def test_note_kind_is_doc(self):
        for f in _files(self.roots["stdlib"]):
            if f["path"].endswith(".md"):
                self.assertEqual(f["kind"], "doc")


class TestResolve(unittest.TestCase):
    def test_allowlisted_files(self):
        self.assertTrue(w.resolve("stdlib/voption.ml"))
        self.assertTrue(w.resolve("stdlib/voption.mli"))
        self.assertTrue(w.resolve("stdlib/notes/voption.md"))
        self.assertTrue(w.resolve("examples/fib.ml"))

    def test_resolved_path_is_real_file(self):
        target = w.resolve("stdlib/voption.ml")
        self.assertTrue(target and os.path.isfile(target))

    def test_rejects_traversal(self):
        for bad in [
            "stdlib/../../etc/passwd",
            "stdlib/../server.py",
            "examples/../../server.py",
            "stdlib//etc/passwd",
            "stdlib/notes/../../../server.py",
        ]:
            self.assertIsNone(w.resolve(bad), bad)

    def test_rejects_unknown_root(self):
        self.assertIsNone(w.resolve("secret/x.ml"))
        self.assertIsNone(w.resolve("/etc/passwd"))
        self.assertIsNone(w.resolve("server.py"))

    def test_rejects_non_servable_extension(self):
        # A real file in a root, but not an .ml/.mli/.md.
        self.assertIsNone(w.resolve("stdlib/MODULES.manifest"))
        self.assertIsNone(w.resolve("stdlib/check_poly.sh"))

    def test_rejects_empty_and_dir(self):
        self.assertIsNone(w.resolve(""))
        self.assertIsNone(w.resolve("stdlib"))
        self.assertIsNone(w.resolve("stdlib/notes"))


class TestModuleMapping(unittest.TestCase):
    def test_top_level_ml_is_a_module(self):
        self.assertEqual(
            w.module_of_path("stdlib/voption.ml"), ("Voption", "voption.ml")
        )
        self.assertEqual(w.module_of_path("stdlib/vmap.ml"), ("Vmap", "vmap.ml"))
        # Capitalised-on-disk names keep their casing.
        self.assertEqual(w.module_of_path("stdlib/Vlist.ml"), ("Vlist", "Vlist.ml"))

    def test_mli_is_a_module_too(self):
        self.assertEqual(
            w.module_of_path("stdlib/voption.mli"), ("Voption", "voption.mli")
        )

    def test_non_units_are_not_modules(self):
        self.assertIsNone(w.module_of_path("stdlib/notes/voption.md"))
        self.assertIsNone(w.module_of_path("stdlib/clients/smoke_voption.ml"))
        self.assertIsNone(w.module_of_path("examples/fib.ml"))
        self.assertIsNone(w.module_of_path(None))


class TestManifest(unittest.TestCase):
    def test_direct_deps(self):
        # Structural, not exact-list: the manifest legitimately grows as the
        # stdlib gains modules (Vhof, Voption riders); the invariants are
        # membership and shape, not a frozen snapshot.
        m = w._parse_manifest()
        self.assertIn("Vlist", m.get("vmap", []))
        self.assertIn("Vset_bst", m.get("vset", []))
        self.assertIn("Vlist", m.get("vset", []))
        for deps in m.values():
            self.assertEqual(len(deps), len(set(d.lower() for d in deps)))

    def test_transitive_deps_unique_and_ordered(self):
        # Unique (case-insensitive) and dependency-ordered: every dep's own
        # deps appear before it.
        m = w._parse_manifest()
        for mod in ("Vmap", "Vset", "Vrel"):
            deps = w._transitive_deps(mod)
            self.assertEqual(len(deps), len(set(d.lower() for d in deps)))
            seen = set()
            for d in deps:
                for dd in m.get(d.lower(), []):
                    self.assertIn(dd.lower(), seen, "%s before its dep %s" % (d, dd))
                seen.add(d.lower())
        self.assertIn("Vlist", w._transitive_deps("Vmap"))

    def test_find_source_case_insensitive(self):
        # voption.mli is lowercase on disk; the manifest name is Voption.
        self.assertTrue(w._find_source("Voption", ".mli"))
        self.assertTrue(w._find_source("Vlist", ".ml"))
        self.assertIsNone(w._find_source("NoSuchModule", ".mli"))


@unittest.skipUnless(LEAN and OCAMLC, "need lean + ocamlc")
class TestSigSourceStaging(unittest.TestCase):
    """ensure_artifacts captures each block-bearing interface's VoxSig Lean
    SOURCE, and stage_for_check drops it (with the cmi/olean) into the
    scratch so the goal pane can inline it."""

    def test_ensure_captures_leansrc(self):
        assert LEAN is not None and OCAMLC is not None
        w.ensure_artifacts("Vhof", OCAMLC, LEAN)
        leansrc = w._leansrc_path("Vhof")
        self.assertTrue(os.path.isfile(leansrc), leansrc)
        with open(leansrc) as fh:
            body = fh.read()
        # It is the sig module's Lean, importing VoxCore and declaring the
        # shared HOF substrate.
        self.assertIn("public import VoxCore", body)
        self.assertIn("IntRel", body)

    def test_stage_for_check_stages_dep_leansrc(self):
        assert LEAN is not None and OCAMLC is not None
        vlist_ml = os.path.join(w.STDLIB_DIR, "Vlist.ml")
        if not os.path.isfile(vlist_ml):
            self.skipTest("no Vlist.ml")
        with open(vlist_ml) as fh:
            source = fh.read()
        dest = w.stage_for_check("Vlist", source, "Vlist.ml", OCAMLC, LEAN)
        scratch = os.path.dirname(dest)
        # Vlist depends on Vhof and Voption: both sig sources are staged.
        self.assertTrue(os.path.isfile(os.path.join(scratch, "VoxSig_Vhof.leansrc")))
        self.assertTrue(os.path.isfile(os.path.join(scratch, "VoxSig_Voption.leansrc")))


if __name__ == "__main__":
    unittest.main()
