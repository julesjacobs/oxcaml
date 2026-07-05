#!/usr/bin/env python3
"""Tests for the example-curation transform (make_examples).

The transform is pure (no compiler), so these run unconditionally.  The
end-to-end strip is pinned against a real suite file, mirroring the
byte-for-byte discipline of test_vc_index.
"""

import os
import unittest

import make_examples  # pyright: ignore[reportImplicitRelativeImport]

SUITE = make_examples.SUITE


class TestStripTestHeader(unittest.TestCase):
    def test_removes_leading_test_block(self):
        src = "(* TEST\n foo;\n*)\n\nlet x = 1\n"
        self.assertEqual(make_examples.strip_test_header(src), "\n\nlet x = 1\n")

    def test_handles_nested_comments_in_header(self):
        src = "(* TEST\n a (* nested *) b;\n*)\nlet x = 1\n"
        self.assertEqual(make_examples.strip_test_header(src), "\nlet x = 1\n")

    def test_no_header_is_a_noop(self):
        src = "let x = 1\n(* not a test header *)\n"
        self.assertEqual(make_examples.strip_test_header(src), src)


class TestStripExpect(unittest.TestCase):
    def test_removes_every_block(self):
        src = (
            "let a = 1\n"
            "[%%expect{|\nval a : int = 1\n|}]\n"
            "let b = 2\n"
            "[%%expect{|\nval b : int = 2\n|}]\n"
        )
        out = make_examples.strip_expect_blocks(src)
        self.assertNotIn("[%%expect", out)
        self.assertIn("let a = 1", out)
        self.assertIn("let b = 2", out)

    def test_block_with_error_payload(self):
        src = (
            "let bad = refine_ 0\n"
            "[%%expect{|\nError: vox: verification failed (lean).\n"
            "       Goal: 0 > 0\n|}]\n"
        )
        self.assertNotIn("Error", make_examples.strip_expect_blocks(src))


class TestNormalize(unittest.TestCase):
    def test_collapses_blank_runs_and_trailing_ws(self):
        self.assertEqual(make_examples.normalize("a  \n\n\n\nb\n\n"), "a\n\nb\n")


class TestTransformPinned(unittest.TestCase):
    """The whole transform, pinned against a real expect-mode suite file."""

    def test_lean_wrong(self):
        path = os.path.join(SUITE, "mechanics", "lean_wrong.ml")
        if not os.path.exists(path):
            self.skipTest("suite file missing")
        with open(path) as fh:
            raw = fh.read()
        expected = (
            "(* The docs/vox demo page's \"when you're wrong\" example, "
            "kept here so\n"
            "   CI verifies exactly the failure output the page shows: goal,\n"
            "   hypotheses, counterexample. *)\n"
            "\n"
            "let rec total_ fib n =\n"
            "  if n <= 0 then 0 else if n = 1 then 1 else "
            "fib (n - 1) + fib (n - 2)\n"
            "[@@vox.decreases n]\n"
            "\n"
            "(* Off by one: fib is not n+1.  The compiler says so, with a "
            "witness. *)\n"
            "let wrong : (n : int) -> int{ _ = fib n } = fun n -> "
            "refine_ (n + 1)\n"
        )
        self.assertEqual(make_examples.transform(raw), expected)


if __name__ == "__main__":
    unittest.main()
