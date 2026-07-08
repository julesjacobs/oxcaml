"""The browsable workspace behind the file-explorer sidebar (task #76).

Two concerns, kept together because they share the same allowlist of
read-only roots:

  * a safe file tree -- [list_tree] enumerates the curated examples and
    the vox stdlib (sources + notes/*.md docs + client smokes) as a
    nested tree, and [resolve] turns a client-supplied path id back into
    an absolute path with strict traversal protection (used by /ls and
    /file);

  * stdlib dependency staging -- a vox stdlib unit does not verify in
    isolation: its .ml checks against its own compiled interface (.cmi +
    the VoxSig_<M>.olean the seal re-elaborates) and, per
    MODULES.manifest, against each dependency's interface artifacts.
    [stage_for_check] materialises all of those into a scratch build dir,
    building each interface's artifacts ONCE and caching them keyed by
    the .mli's mtime (BUILD.md's recipe, automated).

Only files under the two roots, with a servable extension, are ever
reachable; nothing here writes into the source tree.
"""

import json
import os
import shutil
import subprocess
import tempfile
import threading
from typing import Dict, List, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
# tools/vox-editor -> tools -> clone root.
CLONE_ROOT = os.path.dirname(os.path.dirname(HERE))
EXAMPLES_DIR = os.path.join(HERE, "examples")
STDLIB_DIR = os.path.join(CLONE_ROOT, "vox_stdlib")

# Read-only roots the sidebar exposes, id -> (absolute dir, display label).
# A path id is "<root>/<relpath>"; the root id selects the base dir.
ROOTS: Dict[str, Tuple[str, str]] = {
    "examples": (EXAMPLES_DIR, "Examples"),
    "stdlib": (STDLIB_DIR, "vox_stdlib"),
}

# Extensions the tree lists and /file will serve.  .md are read-only docs.
_SERVABLE_EXT = {".ml", ".mli", ".md"}
# Subdirectories of the stdlib root worth showing (others -- _artifacts,
# scratch, __pycache__ -- are build/cache noise).
_STDLIB_SUBDIRS = ("notes", "clients")

# Where built interface artifacts are cached across checks (process- and
# run-independent; invalidated by source mtime).  Kept out of the source
# tree so browsing stays read-only.
_CACHE_ROOT = os.path.join(tempfile.gettempdir(), "vox-editor-depcache")
# RLock: ensure_artifacts recurses into itself for manifest deps while
# holding the lock; a non-reentrant Lock self-deadlocks on any dep-bearing
# module (Vmap, Vset) and every other stdlib check then hangs behind it.
_cache_lock = threading.RLock()


# --------------------------------------------------------------------------
# Safe path resolution
# --------------------------------------------------------------------------


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
    base = os.path.realpath(entry[0])
    target = os.path.realpath(os.path.join(base, rel))
    if target != base and not target.startswith(base + os.sep):
        return None
    if not os.path.isfile(target):
        return None
    if os.path.splitext(target)[1] not in _SERVABLE_EXT:
        return None
    return target


def _kind(name: str) -> str:
    ext = os.path.splitext(name)[1]
    return {".ml": "ml", ".mli": "mli", ".md": "doc"}.get(ext, "file")


def _stdlib_children() -> List[Dict[str, object]]:
    """The stdlib root's tree: top-level .ml/.mli (interface before impl,
    grouped by module), then the notes/ and clients/ subdirs."""
    children: List[Dict[str, object]] = []
    try:
        names = os.listdir(STDLIB_DIR)
    except OSError:
        return children
    tops = [
        n
        for n in names
        if os.path.isfile(os.path.join(STDLIB_DIR, n))
        and os.path.splitext(n)[1] in _SERVABLE_EXT
    ]
    # Group a module's .mli/.ml adjacently, interface first, modules in
    # case-insensitive name order.
    tops.sort(
        key=lambda n: (os.path.splitext(n)[0].lower(), os.path.splitext(n)[1] != ".mli")
    )
    for n in tops:
        children.append(
            {"name": n, "path": "stdlib/" + n, "type": "file", "kind": _kind(n)}
        )
    for sub in _STDLIB_SUBDIRS:
        d = os.path.join(STDLIB_DIR, sub)
        if not os.path.isdir(d):
            continue
        kids: List[Dict[str, object]] = []
        for n in sorted(os.listdir(d), key=str.lower):
            if (
                os.path.isfile(os.path.join(d, n))
                and os.path.splitext(n)[1] in _SERVABLE_EXT
            ):
                kids.append(
                    {
                        "name": n,
                        "path": "stdlib/%s/%s" % (sub, n),
                        "type": "file",
                        "kind": _kind(n),
                    }
                )
        if kids:
            children.append({"name": sub, "type": "dir", "children": kids})
    return children


def _example_children() -> List[Dict[str, object]]:
    """The curated examples, in the index.json order (title/verifies carried
    through so the tree can label them the way the old dropdown did)."""
    children: List[Dict[str, object]] = []
    index = os.path.join(EXAMPLES_DIR, "index.json")
    listed = set()
    if os.path.isfile(index):
        try:
            with open(index) as fh:
                data = json.load(fh)
            for ex in data.get("examples", []):
                name = ex.get("name")
                fn = name + ".ml"
                if not name or not os.path.isfile(os.path.join(EXAMPLES_DIR, fn)):
                    continue
                listed.add(fn)
                children.append(
                    {
                        "name": fn,
                        "path": "examples/" + fn,
                        "type": "file",
                        "kind": "ml",
                        "title": ex.get("title", name),
                        "verifies": ex.get("verifies", True),
                        "default": bool(ex.get("default", False)),
                    }
                )
        except (OSError, ValueError):
            pass
    return children


def list_tree() -> Dict[str, object]:
    """The full browsable tree of both roots (small enough to send at once;
    the client renders it collapsibly)."""
    return {
        "roots": [
            {
                "name": ROOTS["examples"][1],
                "id": "examples",
                "type": "dir",
                "children": _example_children(),
            },
            {
                "name": ROOTS["stdlib"][1],
                "id": "stdlib",
                "type": "dir",
                "children": _stdlib_children(),
            },
        ]
    }


# --------------------------------------------------------------------------
# Stdlib dependency staging
# --------------------------------------------------------------------------


def _parse_manifest() -> Dict[str, List[str]]:
    """MODULES.manifest -> {module_lower: [dep_module, ...]}.  Lines look
    like ``Vmap : Vlist : 1`` (module : space-separated deps : wave);
    comments and blanks ignored."""
    out: Dict[str, List[str]] = {}
    path = os.path.join(STDLIB_DIR, "MODULES.manifest")
    try:
        with open(path) as fh:
            lines = fh.readlines()
    except OSError:
        return out
    for line in lines:
        line = line.split("#", 1)[0].strip()
        if not line:
            continue
        parts = [p.strip() for p in line.split(":")]
        if len(parts) < 2 or not parts[0]:
            continue
        deps = parts[1].split() if parts[1] else []
        out[parts[0].lower()] = deps
    return out


def _find_source(module: str, ext: str) -> Optional[str]:
    """Locate a stdlib source file for ``module`` (case-insensitively on
    the stem, since the manifest capitalises names the files do not, e.g.
    ``Voption`` vs ``voption.ml``).  Returns the absolute path, preserving
    the on-disk filename casing (the produced .cmi tracks it)."""
    try:
        names = os.listdir(STDLIB_DIR)
    except OSError:
        return None
    want = module.lower()
    for n in names:
        stem, e = os.path.splitext(n)
        if e == ext and stem.lower() == want:
            return os.path.join(STDLIB_DIR, n)
    return None


def module_of_path(path_id: str) -> Optional[Tuple[str, str]]:
    """If ``path_id`` is a top-level stdlib source file, return
    (module_name, source_filename); else None.  The module name is the
    OCaml module (stem with a capital initial); the filename keeps its
    on-disk casing so the scratch copy compiles to the matching module."""
    if not path_id or not path_id.startswith("stdlib/"):
        return None
    rel = path_id[len("stdlib/") :]
    if "/" in rel:  # notes/ and clients/ are not checkable units
        return None
    stem, ext = os.path.splitext(rel)
    if ext not in (".ml", ".mli"):
        return None
    module = stem[:1].upper() + stem[1:]
    return module, rel


def _artifact_paths(module: str) -> Tuple[str, str, str]:
    """Cache slot for ``module``: (dir, cmi_path, olean_path).  The cmi
    keeps the source filename casing; the VoxSig olean is always
    capitalised (it derives from the module name)."""
    src = _find_source(module, ".mli")
    cmi_name = (os.path.splitext(os.path.basename(src))[0] if src else module) + ".cmi"
    d = os.path.join(_CACHE_ROOT, module)
    return d, os.path.join(d, cmi_name), os.path.join(d, "VoxSig_%s.olean" % module)


def _cache_fresh(module: str, mli_path: str) -> bool:
    d, cmi, olean = _artifact_paths(module)
    stamp = os.path.join(d, ".mli.mtime")
    # The olean is absent for a block-less interface (e.g. viarray);
    # the stamp is written only after a completed build, so cmi+stamp
    # suffice for freshness.
    if not (os.path.isfile(cmi) and os.path.isfile(stamp)):
        return False
    try:
        with open(stamp) as fh:
            return fh.read().strip() == str(int(os.path.getmtime(mli_path)))
    except (OSError, ValueError):
        return False


def ensure_artifacts(module: str, ocamlc: str, lean: str) -> Tuple[str, str]:
    """Build (once, cached) and return the (cmi, VoxSig olean) for
    ``module``'s interface, recursively ensuring its manifest deps first.
    Raises RuntimeError with the compiler output if the .mli fails to
    build.  Cache keyed by the .mli mtime, so a source edit rebuilds."""
    mli = _find_source(module, ".mli")
    if mli is None:
        raise RuntimeError("no interface for stdlib module %r" % module)
    with _cache_lock:
        d, cmi, olean = _artifact_paths(module)
        if _cache_fresh(module, mli):
            return cmi, olean
        deps = _parse_manifest().get(module.lower(), [])
        build = tempfile.mkdtemp(prefix="voxdep")
        try:
            # A dep's interface must be present to compile this .mli.
            for dep in deps:
                dcmi, dolean = ensure_artifacts(dep, ocamlc, lean)
                shutil.copy(dcmi, build)
                if os.path.isfile(dolean):
                    shutil.copy(dolean, build)
            shutil.copy(mli, build)
            mli_name = os.path.basename(mli)
            proc = subprocess.run(
                [ocamlc, "-c", "-vox-solver-path", lean, mli_name],
                cwd=build,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
            )
            base = os.path.splitext(mli_name)[0]
            built_cmi = os.path.join(build, base + ".cmi")
            built_olean = os.path.join(build, "VoxSig_%s.olean" % module)
            if proc.returncode != 0 or not os.path.isfile(built_cmi):
                raise RuntimeError(
                    "building %s interface failed:\n%s" % (module, proc.stdout)
                )
            os.makedirs(d, exist_ok=True)
            shutil.copy(built_cmi, cmi)
            # A block-less interface has no VoxSig olean; cache without it.
            if os.path.isfile(built_olean):
                shutil.copy(built_olean, olean)
            elif os.path.isfile(olean):
                os.remove(olean)
            with open(os.path.join(d, ".mli.mtime"), "w") as fh:
                fh.write(str(int(os.path.getmtime(mli))))
            return cmi, olean
        finally:
            shutil.rmtree(build, ignore_errors=True)


def _transitive_deps(module: str, seen: Optional[set] = None) -> List[str]:
    """``module``'s dependency modules, transitively, each once."""
    if seen is None:
        seen = set()
    order: List[str] = []
    manifest = _parse_manifest()
    for dep in manifest.get(module.lower(), []):
        if dep.lower() in seen:
            continue
        seen.add(dep.lower())
        order.extend(_transitive_deps(dep, seen))
        order.append(dep)
    return order


def stage_for_check(
    module: str, source: str, filename: str, ocamlc: str, lean: Optional[str]
) -> str:
    """Prepare a private scratch dir to /check a stdlib unit's .ml: write
    the (possibly edited) buffer under its real module filename, and stage
    the module's OWN interface artifacts plus every transitive dependency's
    -- so both the fast dry-run (no solver) and the full solve see the
    .cmi/olean they need without recompiling interfaces per keystroke.

    A Lean path is required to build interface artifacts; without one
    (fast pass before any full check has warmed the cache) we still stage
    whatever the cache already holds and fall back gracefully.  Returns the
    absolute path of the written source file."""
    scratch = tempfile.mkdtemp(prefix="voxstdlib")
    dest = os.path.join(scratch, filename)
    with open(dest, "w") as fh:
        fh.write(source)
    if lean is not None:
        needed = _transitive_deps(module)
        # Checking the .ml needs the module's own interface too; checking
        # the .mli itself only needs its deps.
        if filename.endswith(".ml"):
            needed = needed + [module]
        for m in needed:
            try:
                cmi, olean = ensure_artifacts(m, ocamlc, lean)
                shutil.copy(cmi, scratch)
                if os.path.isfile(olean):
                    shutil.copy(olean, scratch)
            except (RuntimeError, OSError):
                # Missing/failed dep: let the compile surface the real
                # error (unbound module) rather than masking it here.
                pass
    else:
        for m in _transitive_deps(module) + [module]:
            d, cmi, olean = _artifact_paths(m)
            for art in (cmi, olean):
                if os.path.isfile(art):
                    try:
                        shutil.copy(art, scratch)
                    except OSError:
                        pass
    return dest
