#!/usr/bin/env python3
"""Tests for workspace: the file-explorer tree, safe path resolution, and
the stdlib dependency/manifest logic.  These touch only committed source
files (no compiler / Lean), so they run unconditionally."""

import os
import unittest

import workspace as w  # pyright: ignore[reportImplicitRelativeImport]


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
        m = w._parse_manifest()
        self.assertEqual(m.get("vmap"), ["Vlist"])
        self.assertEqual(m.get("vset"), ["Vset_bst", "Vlist"])
        self.assertEqual(m.get("voption"), [])

    def test_transitive_deps_unique_and_ordered(self):
        # Vmap depends on Vlist; Vset on Vset_bst and Vlist.
        self.assertEqual(w._transitive_deps("Vmap"), ["Vlist"])
        deps = w._transitive_deps("Vset")
        self.assertEqual(set(deps), {"Vset_bst", "Vlist"})
        self.assertEqual(len(deps), len(set(d.lower() for d in deps)))

    def test_find_source_case_insensitive(self):
        # voption.mli is lowercase on disk; the manifest name is Voption.
        self.assertTrue(w._find_source("Voption", ".mli"))
        self.assertTrue(w._find_source("Vlist", ".ml"))
        self.assertIsNone(w._find_source("NoSuchModule", ".mli"))


if __name__ == "__main__":
    unittest.main()
