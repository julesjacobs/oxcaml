#!/usr/bin/env python3
"""Extract checked public declarations for the six collection specification pages."""
import hashlib
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
OUT = Path(__file__).with_name("collections-specs.json")
LIB = "verification/library/"
TEST = "testsuite/tests/vox/"
DECL = re.compile(r"^(?:val|external|type|module|include|open)\b|^@@")
entries = {}


def extract(path, name, *, parent=None):
    text = (ROOT / path).read_text()
    lines = text.splitlines(keepends=True)
    lower, upper, indent = 0, len(lines), ""
    context = []
    if parent:
        lower = next(i for i, line in enumerate(lines)
                     if re.match(r"module " + re.escape(parent) + r"\s*:", line))
        context = [lines[lower].rstrip()]
        upper = next(i for i in range(lower + 1, len(lines)) if lines[i].startswith("end"))
        lower += 1
        indent = "  "
    starts = [i for i in range(lower, upper)
              if lines[i].startswith(indent) and DECL.match(lines[i][len(indent):])]
    if name == "*":
        return [extract(path, "@" + str(i + 1), parent=parent) for i in starts]
    if name.startswith("@") and name != "@@":
        start = int(name[1:]) - 1
    else:
        pattern = re.compile(r"(?:val|external) " + re.escape(name) + r"(?:\s|:)|"
                             r"module (?:type )?" + re.escape(name) + r"(?:\s|:)|"
                             r"type .*\b" + re.escape(name) + r"\s*(?::|=)|"
                             r"type " + re.escape(name) + r"\s*(?::|=)|"
                             r"(?:open|include) " + re.escape(name) + r"(?:\s|$)")
        matches = [i for i in starts if pattern.match(lines[i].lstrip())]
        if name == "@@":
            matches = [i for i in starts if lines[i].lstrip().startswith("@@")]
        if len(matches) != 1:
            raise ValueError((path, parent, name, matches))
        start = matches[0]
    stop = next((i for i in starts if i > start), upper)
    # Documentation after a declaration is not needed for a code-only view.
    # Keep the original source text and source coordinates for the declaration.
    for i in range(start + 1, stop):
        if lines[i].startswith(indent + "(**"):
            stop = i
            break
    while stop > start and not lines[stop - 1].strip():
        stop -= 1
    code = "".join(lines[start:stop]).rstrip()
    label = code.splitlines()[0].strip()
    key = path + ("#" + parent if parent else "") + ":" + str(start + 1)
    entries[key] = {
        "file": path, "context": context, "declaration": label,
        "start_line": start + 1, "end_line": stop, "code": code,
        "source_sha256": hashlib.sha256(text.encode()).hexdigest(),
        "code_sha256": hashlib.sha256(code.encode()).hexdigest(),
    }
    return key


def pick(path, names, **kwargs):
    result = []
    for name in names:
        selected = extract(path, name, **kwargs)
        result.extend(selected if isinstance(selected, list) else [selected])
    return result


common = pick("stdlib/stdlib.mli", ["( = )", "( <> )", "( < )", "( > )",
    "( <= )", "( >= )", "not", "( && )", "( || )", "( + )", "( - )", "( ~- )"])
common += pick("stdlib/ghost.mli", ["t"])
bigint = pick("stdlib/bigint.mli", ["@@", "t", "of_int", "add", "sub", "equal", "compare"])
iarray = pick("stdlib/iarray.mli", ["length"])
iarray += pick("stdlib/iarray.mli", ["get"], parent="Refined")
sequence = pick(LIB + "vox_sequence.mli", ["t", "length", "length_def", "append", "append_def",
    "at", "at_def", "at_outside", "set", "set_def", "take", "take_def", "drop", "drop_def",
    "sub", "sub_def", "swap", "swap_def", "iarray_get", "of_iarray", "of_iarray_length",
    "of_iarray_at", "extensional"])
integer_sequence = pick(LIB + "vox_int_sequence.mli", ["Vox_sequence", "accepts", "accepts_def",
    "all", "all_def", "sorted", "sorted_def", "permutation", "count", "count_def",
    "permutation_count", "count_extensional"])
borrow = pick(LIB + "borrow.mli", ["@@", "Model", "step"])
borrow += pick(LIB + "borrow.mli", ["t", "current", "final", "length", "get", "set", "swap",
    "split_at", "split3", "with_range", "finish"], parent="Slice")
borrow += pick(LIB + "borrow.mli", ["*"], parent="Owned_array")
parallel = pick(LIB + "borrow.mli", ["parallel"], parent="Slice")
quick_common = common + bigint + iarray + sequence + integer_sequence + borrow
pages = [
    {"id": "sequential-quicksort", "declarations": quick_common +
     pick(TEST + "quicksort.mli", ["@@", "Spec", "sort", "sort_array"])},
    {"id": "parallel-quicksort", "declarations": quick_common + parallel +
     pick(TEST + "quicksort.mli", ["@@", "Spec", "parallel_sort", "parallel_sort_array"])},
    {"id": "avl-sets", "declarations": common + bigint +
     pick(TEST + "int_set_intf.mli", ["Operations", "Extensional"]) +
     pick(TEST + "avl_sets.mli", ["*"])},
    {"id": "binary-search-sorted-updates", "declarations": common + bigint +
     pick(LIB + "vox_sequence.mli", ["t", "length", "length_def", "at", "at_def",
         "at_outside", "extensional"]) + pick(TEST + "sorted_array.mli", ["*"])},
    {"id": "functional-queue", "declarations": common +
     pick(LIB + "vox_sequence.mli", ["t", "append", "append_def"]) +
     pick(TEST + "functional_queue.mli", ["*"])},
    {"id": "sparse-array-overlays", "declarations": common + iarray +
     pick(LIB + "vox_iarray.mli", ["get", "at", "at_get", "at_outside"]) +
     pick(TEST + "sparse_overlay.mli", ["*"])},
]
for page in pages:
    page["declarations"] = list(dict.fromkeys(page["declarations"]))
    page["opaque_semantic_gaps"] = []
    page["checked_source_only"] = True
manifest = {
    "schema_version": 1,
    "compiler_baseline": "d143961f173da258a7c4220a8f140c81871a2c60",
    "source_baseline": "78b8b9e2ac7122817251d983a83e0cbb1ace5eac",
    "pages": pages,
    "declarations": entries,
    "format": "Ordered source fragments, grouped by file and enclosing signature; not a synthetic compilation unit.",
    "limitations": {
        "parallel-quicksort": "Normal return; no parallel termination or cost theorem.",
        "avl-sets": "Size is characterized only at zero; no general cardinality or cost theorem.",
        "binary-search-sorted-updates": "Start from empty; allocating updates are not total; no comparator-count theorem.",
        "functional-queue": "Immutable-data values; no amortized-cost theorem.",
        "sparse-array-overlays": "Ordinary-mode verification; principal generic writable-value inference fails. Association-map implementation; historical balanced-map fixture is separate.",
    },
}
OUT.write_text(json.dumps(manifest, indent=2) + "\n")
print(f"{len(pages)} pages, {len(entries)} exact checked declarations: {OUT}")
