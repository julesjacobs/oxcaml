import os
import tempfile
import unittest
from typing import Any, cast
from unittest import mock

import workspace  # pyright: ignore[reportImplicitRelativeImport]


class ResolveTests(unittest.TestCase):
    def test_resolves_a_curated_example(self):
        target = workspace.resolve("examples/overview.ml")
        self.assertIsNotNone(target)
        assert target is not None
        self.assertTrue(target.endswith(os.path.join("examples", "overview.ml")))
        self.assertTrue(os.path.isfile(target))

    def test_resolves_a_curated_doc(self):
        target = workspace.resolve("docs/welcome.md")
        self.assertIsNotNone(target)
        assert target is not None
        self.assertTrue(target.endswith(os.path.join("docs", "welcome.md")))
        self.assertTrue(os.path.isfile(target))

    def test_rejects_doc_root_traversal(self):
        # The generic containment check must cover the docs root too.
        self.assertIsNone(workspace.resolve("docs/../server.py"))
        self.assertIsNone(workspace.resolve("docs/../workspace.py"))
        self.assertIsNone(workspace.resolve("docs/\x00.md"))

    def test_rejects_traversal(self):
        self.assertIsNone(workspace.resolve("examples/../server.py"))
        self.assertIsNone(workspace.resolve("examples/../../etc/passwd"))
        self.assertIsNone(workspace.resolve("examples/bst/../overview.ml"))

    def test_rejects_absolute_and_empty_path_components(self):
        absolute = os.path.join(workspace.EXAMPLES_DIR, "bst", "bst.ml")
        self.assertIsNone(workspace.resolve("examples/" + absolute))
        self.assertIsNone(workspace.resolve("examples/bst//bst.ml"))
        self.assertIsNone(workspace.resolve("examples/./overview.ml"))

    def test_resolves_a_nested_curated_workspace_file(self):
        target = workspace.resolve("examples/bst/bst.mli")
        self.assertIsNotNone(target)
        assert target is not None
        self.assertTrue(target.endswith(os.path.join("bst", "bst.mli")))

    def test_rejects_unknown_root(self):
        self.assertIsNone(workspace.resolve("stdlib/foo.ml"))
        self.assertIsNone(workspace.resolve("overview.ml"))

    def test_rejects_non_servable_extension(self):
        # index.json lives in the examples dir but is not a servable source.
        self.assertIsNone(workspace.resolve("examples/index.json"))

    def test_rejects_embedded_nul_without_raising(self):
        # A crafted ?path=examples/%00.ml decodes to a NUL, which makes
        # os.path.realpath raise ValueError; resolve must swallow it and
        # report None rather than let the request handler crash.
        self.assertIsNone(workspace.resolve("examples/\x00.ml"))
        self.assertIsNone(workspace.resolve("examples/foo\x00bar.ml"))


class TreeTests(unittest.TestCase):
    def test_tree_carries_titles_and_expected_states(self):
        tree = cast(Any, workspace.list_tree())
        children = tree["roots"][0]["children"]
        by_name = {c["name"]: c for c in children}
        self.assertIn("counterexample.ml", by_name)
        self.assertEqual(by_name["counterexample.ml"]["expected_state"], "disproved")
        self.assertEqual(by_name["unproved.ml"]["expected_state"], "unproved")
        self.assertEqual(by_name["overview.ml"]["expected_state"], "verified")
        self.assertNotIn("verifies", by_name["overview.ml"])
        self.assertTrue(by_name["overview.ml"]["default"])

    def test_tree_has_a_docs_root_with_doc_kind_children(self):
        tree = cast(Any, workspace.list_tree())
        roots = {root["id"]: root for root in tree["roots"]}
        self.assertIn("docs", roots)
        docs = {child["name"]: child for child in roots["docs"]["children"]}
        self.assertIn("welcome.md", docs)
        self.assertEqual(docs["welcome.md"]["kind"], "doc")
        self.assertEqual(docs["welcome.md"]["path"], "docs/welcome.md")
        # Docs carry no verification outcome (they are never compiled).
        self.assertNotIn("expected_state", docs["welcome.md"])

    def test_doc_children_lists_only_markdown_sorted(self):
        docs = cast(Any, workspace._doc_children())
        names = [doc["name"] for doc in docs]
        self.assertEqual(names, sorted(names))
        self.assertTrue(all(name.endswith(".md") for name in names))

    def test_doc_children_skips_outward_symlink(self):
        # _doc_children must agree with resolve(): a .md symlink whose target
        # escapes the docs root would 404 on /file, so it must not be listed
        # (otherwise the tree shows a listed-but-unopenable entry).  Nested
        # `with` (not parenthesized) to stay Python 3.6-compatible.
        with tempfile.TemporaryDirectory() as outside:
            with tempfile.TemporaryDirectory() as docs:
                with open(os.path.join(docs, "real.md"), "w") as fh:
                    fh.write("# real\n")
                target = os.path.join(outside, "secret.md")
                with open(target, "w") as fh:
                    fh.write("# outside\n")
                os.symlink(target, os.path.join(docs, "escape.md"))
                patch_dir = mock.patch.object(workspace, "DOCS_DIR", docs)
                patch_roots = mock.patch.dict(workspace.ROOTS, {"docs": (docs, "Docs")})
                with patch_dir, patch_roots:
                    names = [
                        doc["name"] for doc in cast(Any, workspace._doc_children())
                    ]
                    self.assertIn("real.md", names)
                    self.assertNotIn("escape.md", names)
                    # resolve rejects it too, so tree and /file stay consistent.
                    self.assertIsNone(workspace.resolve("docs/escape.md"))
                    self.assertIsNotNone(workspace.resolve("docs/real.md"))


class MalformedIndexTests(unittest.TestCase):
    """A malformed (but valid-JSON) examples index must degrade to an empty
    example list rather than raise an uncaught error that closes /ls."""

    def _example_children_with_index(self, contents: str):
        with tempfile.TemporaryDirectory() as tmp:
            with open(os.path.join(tmp, "index.json"), "w") as fh:
                fh.write(contents)
            with mock.patch.object(workspace, "EXAMPLES_DIR", tmp):
                return workspace._example_children()

    def test_top_level_array_is_tolerated(self):
        self.assertEqual(self._example_children_with_index("[]"), [])
        self.assertEqual(self._example_children_with_index('[{"name": "x"}]'), [])

    def test_non_list_examples_member_is_tolerated(self):
        self.assertEqual(
            self._example_children_with_index('{"examples": {"name": "x"}}'), []
        )

    def test_non_dict_example_entries_are_skipped(self):
        self.assertEqual(
            self._example_children_with_index('{"examples": ["x", 3, null]}'), []
        )

    def test_invalid_json_is_tolerated(self):
        self.assertEqual(self._example_children_with_index("not json"), [])


if __name__ == "__main__":
    unittest.main()
