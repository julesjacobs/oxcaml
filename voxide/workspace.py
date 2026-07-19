#!/usr/bin/env python3
"""Read-only file explorer behind the sidebar (slice 3).

A single allowlisted root -- the curated ``examples/`` directory -- is
exposed as a small tree (``/ls``) whose files are served one at a time
(``/file``).  Path ids are ``"examples/<relpath>"``; [resolve] maps one
back to an absolute path with strict traversal protection so neither
``..`` segments nor a symlink can escape the root, and only files with a
servable extension are ever reachable.  Nothing here writes into the
source tree.

Multi-root browsing (a vox_stdlib root with dependency-artifact staging,
as vox1 has) is deliberately out of scope for this slice: it depends on
the vox2 build model and lands with the module-context work.
"""

import json
import os
from typing import Dict, List, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
EXAMPLES_DIR = os.path.join(HERE, "examples")
DOCS_DIR = os.path.join(HERE, "docs")

# id -> (absolute dir, display label).  A path id is "<root>/<relpath>".
ROOTS: Dict[str, Tuple[str, str]] = {
    "examples": (EXAMPLES_DIR, "Examples"),
    "docs": (DOCS_DIR, "Docs"),
}

# Extensions the tree lists and /file will serve.  .md files are read-only
# docs, opened in the rendered doc viewer with no compile attempted.
_SERVABLE_EXT = {".ml", ".mli", ".md"}


def resolve(path_id: str) -> Optional[str]:
    """Map a client path id ("<root>/<relpath>") to an absolute file path,
    or None if it is not an allowlisted, servable, real file.

    Traversal protection: the real (symlink-resolved) target must stay
    within the real root dir, so neither ``..`` segments nor a symlink
    pointing outside can escape."""
    if not path_id or "/" not in path_id:
        return None
    root_id, rel = path_id.split("/", 1)
    entry = ROOTS.get(root_id)
    if entry is None or not rel:
        return None
    # Path ids are canonical relative spellings, not filesystem paths supplied
    # by the client.  Reject absolute tails and all empty/dot/parent components
    # before joining: realpath containment alone would otherwise accept inputs
    # such as ``examples/bst/../overview.ml`` or ``examples//<absolute>`` when
    # their final target happened to land back inside the allowlisted root.
    if os.path.isabs(rel):
        return None
    components = rel.split("/")
    if any(component in {"", ".", ".."} for component in components):
        return None
    # An embedded NUL (or other bad path byte) makes os.path.realpath /
    # os.path.isfile raise; treat any such path as simply not found, matching
    # resolve_static_path's discipline, so a crafted ``?path=examples/%00.ml``
    # cannot crash the request handler.
    try:
        base = os.path.realpath(entry[0])
        target = os.path.realpath(os.path.join(base, rel))
        if target != base and not target.startswith(base + os.sep):
            return None
        if not os.path.isfile(target):
            return None
    except (OSError, ValueError):
        return None
    if os.path.splitext(target)[1] not in _SERVABLE_EXT:
        return None
    return target


def _kind(name: str) -> str:
    ext = os.path.splitext(name)[1]
    return {".ml": "ml", ".mli": "mli", ".md": "doc"}.get(ext, "file")


def _example_children() -> List[Dict[str, object]]:
    """The curated examples, in index.json order, with teaching metadata."""
    children: List[Dict[str, object]] = []
    index = os.path.join(EXAMPLES_DIR, "index.json")
    if not os.path.isfile(index):
        return children
    try:
        with open(index) as fh:
            data = json.load(fh)
    except (OSError, ValueError):
        return children
    # index.json is repo-controlled, but a malformed shape (a top-level
    # array, or a non-list "examples") must degrade to an empty tree rather
    # than raise an uncaught AttributeError that closes the /ls request.
    if not isinstance(data, dict):
        return children
    examples = data.get("examples", [])
    if not isinstance(examples, list):
        return children
    for example in examples:
        if not isinstance(example, dict):
            continue
        name = example.get("name")
        filename = str(name) + ".ml"
        if not name or not os.path.isfile(os.path.join(EXAMPLES_DIR, filename)):
            continue
        expected_state = example.get("expected_state")
        if expected_state not in {"verified", "disproved", "unproved"}:
            expected_state = "verified"
        children.append(
            {
                "name": filename,
                "path": "examples/" + filename,
                "type": "file",
                "kind": "ml",
                "title": example.get("title", name),
                "expected_state": expected_state,
                "default": bool(example.get("default", False)),
            }
        )
    return children


def _doc_children() -> List[Dict[str, object]]:
    """The curated read-only docs (``docs/*.md``), sorted by filename.  A
    doc has no verification outcome; the client opens it read-only in the
    rendered doc viewer, never the compiler."""
    children: List[Dict[str, object]] = []
    if not os.path.isdir(DOCS_DIR):
        return children
    try:
        names = sorted(os.listdir(DOCS_DIR))
    except OSError:
        return children
    for filename in names:
        if os.path.splitext(filename)[1] != ".md":
            continue
        # Gate on resolve (not a bare isfile): a symlink whose target escapes
        # the root, or a dead symlink, must not be listed, since /file would
        # then 404 it -- the tree and the server must agree on what is servable.
        if resolve("docs/" + filename) is None:
            continue
        children.append(
            {
                "name": filename,
                "path": "docs/" + filename,
                "type": "file",
                "kind": "doc",
                "title": filename,
            }
        )
    return children


def list_tree() -> Dict[str, object]:
    """The full browsable tree (small enough to send at once; the client
    renders it collapsibly)."""
    roots: List[Dict[str, object]] = [
        {
            "name": ROOTS["examples"][1],
            "id": "examples",
            "type": "dir",
            "children": _example_children(),
        },
    ]
    docs = _doc_children()
    if docs:
        roots.append(
            {
                "name": ROOTS["docs"][1],
                "id": "docs",
                "type": "dir",
                "children": docs,
            }
        )
    return {"roots": roots}


def examples_index_path() -> Optional[str]:
    """Absolute path of the examples index.json, or None if absent."""
    index = os.path.join(EXAMPLES_DIR, "index.json")
    return index if os.path.isfile(index) else None
