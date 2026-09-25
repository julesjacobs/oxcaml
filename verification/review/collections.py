#!/usr/bin/env python3
"""Compile the collection demos and isolated clients with a stable installation."""
import argparse
import json
import re
import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MODULES = [
    "vox_sequence", "vox_int_sequence", "vox_iarray", "borrow",
    "functional_queue", "int_set_intf", "avl_sets", "sorted_array_proofs",
    "sorted_array", "quicksort_model", "quicksort", "sparse_overlay",
]
PUBLIC = [m for m in MODULES if m not in {"sorted_array_proofs", "quicksort_model"}]
CLIENTS = [
    "queue_client", "avl_set_client", "sorted_array_client", "quicksort_client",
    "quicksort_frame_client", "sparse_overlay_client", "collections_boundary_client",
    "borrow_parallel",
]
REJECT = {
    "queue_empty": """let () =
  let q = Functional_queue.empty in
  let nonempty : {q : int Functional_queue.t |
    (Functional_queue.contents q === []) === false} = q in
  let _ = Functional_queue.dequeue nonempty in ()
""",
    "queue_representation": "let invalid (q : int Functional_queue.t) = q.front\n",
    "avl_representation": "let invalid = Avl_sets.Core.Leaf\n",
    "avl_structural_equality": """let invalid (a : Avl_sets.t) (b : Avl_sets.t)
  (same : {u : unit | Avl_sets.equal a b}) : {u : unit | a === b} =
  let u = same in u
""",
    "sorted_representation": "let invalid (a : Sorted_array.t) = Iarray.length a\n",
    "sorted_empty_removal": """let () =
  let a = Sorted_array.empty in
  let zero = 0 in
  let u = () in
  let _ = Sorted_array.remove_at a zero (u) in ()
""",
    "sorted_proofs": "let invalid = Sorted_array_proofs.Arrays.at\n",
    "quicksort_proofs": "let invalid = Quicksort_model.swap_partition\n",
    "quicksort_false_count": """let invalid (input : int iarray) =
  let a = Borrow.Owned_array.of_iarray input in
  let b = Quicksort.sort_array a in
  let result = Borrow.Owned_array.into_iarray b in
  let before = ghost_ (Vox_sequence.of_iarray input) in
  let after = ghost_ (Vox_sequence.of_iarray result) in
  let target = 7 in
  ghost_ (
    Quicksort.Spec.permutation_count before after target;
    let u = () in (u : {u : unit |
      Quicksort.Spec.count after target ===
        Bigint.add 1Z (Quicksort.Spec.count before target)}))
""",
    "sparse_representation": "let invalid (a : int Sparse_overlay.t) = a.updates\n",
    "sparse_bounds": """module L = Sparse_overlay.Laws(struct type t = int end)
let () =
  let values = [: :] in
  let a = Sparse_overlay.empty values in
  let zero = 0 in
  ghost_ (L.empty_base values; L.length_equation a);
  let _ = Sparse_overlay.get a (zero) in ()
""",
    "sparse_false_update": """module L = Sparse_overlay.Laws(struct type t = int end)
let invalid : (a : int Sparse_overlay.t) -> (index : int) -> (value : int) ->
    {u : unit | 0 <= index && index < Sparse_overlay.length a} -> unit =
  fun a index value bound ->
  let bounds = bound in
  let b = Sparse_overlay.set index value a in
  ghost_ (L.set_lookup a index value index);
  let u = () in
  let false_claim : {u : unit |
    Sparse_overlay.lookup index b === Some (value + 1)} = u in
  let proof = false_claim in ()
""",
}


def run(command, cwd, log, expected=0):
    result = subprocess.run(command, cwd=cwd, text=True, capture_output=True)
    log.write_text("$ " + " ".join(map(str, command)) + "\n" + result.stdout + result.stderr)
    if result.returncode != expected:
        raise RuntimeError(f"{log}: expected exit {expected}, got {result.returncode}")
    return result


def source(module, suffix):
    for directory in [ROOT / "verification/library", ROOT / "testsuite/tests/vox"]:
        path = directory / (module + suffix)
        if path.exists():
            return path
    return None


def binding(text, name, last=False):
    matches = list(re.finditer(r"\b" + re.escape(name) + r"/\d+(?:\s*=[^\n]*|\s*)\s*\(function", text))
    if not matches:
        raise RuntimeError(f"Missing emitted function: {name}")
    match = matches[-1] if last else matches[0]
    start = text.index("(function", match.start())
    depth = 0
    for pos in range(start, len(text)):
        if text[pos] == "(":
            depth += 1
        elif text[pos] == ")":
            depth -= 1
            if depth == 0:
                return text[start:pos + 1]
    raise RuntimeError(f"Unterminated emitted function: {name}")


def audit_additional(logs, mode, report):
    for backend in ["ocamlc", "ocamlopt"]:
        for module, names, forbidden in [
            ("avl_sets", ["add", "union", "lookup", "add_tree", "lookup_tree",
                          "make_node", "balance", "add_elements"],
             r"(?:Validity_proofs|Insertion_model_proofs|Element_proofs|List_proofs)/"
             r"|apply[^)]*\b(?:valid|all_less|all_greater|\w+_def)/"),
            ("sorted_array", ["mem", "equal_range", "insert", "remove_at",
                              "find_first", "find_last", "remove_one"],
             r"apply[^)]*\b(?:contents|\w+_def)/"),
            ("sorted_array_proofs", ["search", "bounds", "equal_range", "insert",
                                     "remove_at", "find_first", "find_last", "remove_one"],
             r"apply[^)]*\b(?:occurs|occurs_range|edited|edited_at|range_at|ordered|partition|\w+_def)/"),
        ]:
            emitted = (logs / (module + "." + backend + ".log")).read_text()
            for name in names:
                for last in ([False, True] if module == "avl_sets" and name in {"add", "union"} else [False]):
                    body = binding(emitted, name, last=last)
                    if re.search(forbidden, body):
                        raise RuntimeError(f"{module}.{name}: unexpected proof execution")
                    report["erasure"].append([mode, backend, module, name, last])


def check_legacy_avl(prefix, flags, public, impl, logs, mode, report):
    shutil.copyfile(source("avl_stdlib_set", ".ml"), public / "avl_stdlib_set.ml")
    for backend, suffix in [("ocamlc", ".cmo"), ("ocamlopt", ".cmx")]:
        run([str(prefix / "bin" / backend), *flags, "-c", "avl_stdlib_set.ml"],
            public, logs / ("avl_stdlib_set." + backend + ".compile.log"))
        exe = public / ("avl_legacy." + backend + ".exe")
        run([str(prefix / "bin" / backend), *flags, "-o", str(exe),
             str(impl / ("avl_sets" + suffix)), "avl_set_client" + suffix,
             "avl_stdlib_set" + suffix], public, logs / ("avl_legacy." + backend + ".link.log"))
        output = run([str(exe)], public, logs / ("avl_legacy." + backend + ".run.log"))
        if output.stdout != source("avl_sets", ".reference").read_text():
            raise RuntimeError("Original AVL reference output changed")
        report["legacy_runs"].append([mode, backend, "avl_sets.reference"])


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument("prefix", type=Path)
    args = parser.parse_args()
    prefix = args.prefix.resolve()
    out = ROOT / "_build/collections-review"
    out.mkdir(parents=True, exist_ok=True)
    config = run([str(prefix / "bin/ocamlopt"), "-config"], out, out / "compiler.log").stdout
    if "multidomain: true" not in config or "poll_insertion: true" not in config:
        raise RuntimeError("Parallel callback tests require multidomain and poll insertion")
    report = {"prefix": str(prefix), "runs": [], "rejections": [], "erasure": [], "legacy_runs": []}
    for principal in [False, True]:
        mode = "principal" if principal else "default"
        work = out / mode
        impl = work / "implementation"
        public = work / "public"
        logs = work / "logs"
        for path in [impl, public, logs]:
            path.mkdir(parents=True, exist_ok=True)
        flags = ["-nostdlib", "-I", str(prefix / "lib/ocaml"), "-I", ".",
                 "-extension", "refinement_types"]
        if principal:
            flags.append("-principal")
        modules = [m for m in MODULES if not (principal and m == "sparse_overlay")]
        for module in modules:
            for suffix in [".mli", ".ml"]:
                original = source(module, suffix)
                if original:
                    shutil.copyfile(original, impl / original.name)
            if (impl / (module + ".mli")).exists():
                run([str(prefix / "bin/ocamlc"), *flags, "-c", module + ".mli"],
                    impl, logs / (module + ".mli.log"))
            if (impl / (module + ".ml")).exists():
                for backend in ["ocamlc", "ocamlopt"]:
                    print(mode, backend, module, flush=True)
                    run([str(prefix / "bin" / backend), *flags, "-dlambda", "-c", module + ".ml"],
                        impl, logs / (module + "." + backend + ".log"))
        for module in PUBLIC:
            if module not in modules:
                continue
            shutil.copyfile(impl / (module + ".cmi"), public / (module + ".cmi"))
        objects = [m for m in modules if (impl / (m + ".ml")).exists()]
        for client in CLIENTS:
            if principal and client == "sparse_overlay_client":
                continue
            shutil.copyfile(source(client, ".ml"), public / (client + ".ml"))
            for backend, suffix in [("ocamlc", ".cmo"), ("ocamlopt", ".cmx")]:
                print(mode, backend, client, flush=True)
                run([str(prefix / "bin" / backend), *flags, "-c", client + ".ml"],
                    public, logs / (client + "." + backend + ".compile.log"))
                exe = public / (client + "." + backend + ".exe")
                run([str(prefix / "bin" / backend), *flags, "-o", str(exe),
                     *[str(impl / (m + suffix)) for m in objects], client + suffix],
                    public, logs / (client + "." + backend + ".link.log"))
                run([str(exe)], public, logs / (client + "." + backend + ".run.log"))
                report["runs"].append([mode, backend, client])
        check_legacy_avl(prefix, flags, public, impl, logs, mode, report)
        for name, text in REJECT.items():
            if principal and name.startswith("sparse_"):
                continue
            (public / (name + ".ml")).write_text(text)
            result = run([str(prefix / "bin/ocamlc"), *flags, "-c", name + ".ml"],
                         public, logs / (name + ".reject.log"), expected=2)
            expected = "Refinement could not be proved" if name in {
                "queue_empty", "avl_structural_equality", "sorted_empty_removal",
                "quicksort_false_count", "sparse_bounds", "sparse_false_update",
            } else "Error:"
            if expected not in result.stderr:
                raise RuntimeError(f"{name}: unexpected rejection reason, see log")
            report["rejections"].append([mode, name])
        for backend in ["ocamlc", "ocamlopt"]:
            for module, names, forbidden in [
                ("quicksort", ["partition", "sort_sized", "sort", "parallel_sort"],
                 r"Quicksort_model|Vox_int_sequence|Vox_sequence|caml_borrow_(?:current|final|contents)"),
                ("functional_queue", ["normalize", "enqueue", "dequeue"],
                 r"(?:apply[^\n]*\s)(?:contents|reverse|reverse_append_correct|\w+_def)/"),
                ("sparse_overlay", ["empty", "set", "clear", "lookup", "get"],
                 r"find_remove|(?:apply[^\n]*\s)\w+_def/|Vox_iarray"),
            ]:
                if module not in modules:
                    continue
                emitted = (logs / (module + "." + backend + ".log")).read_text()
                for name in names:
                    body = binding(emitted, name)
                    if re.search(forbidden, body):
                        raise RuntimeError(f"{module}.{name}: unexpected proof/model execution")
                    report["erasure"].append([mode, backend, module, name])
    for mode in ["default", "principal"]:
        audit_additional(out / mode / "logs", mode, report)
    (out / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({key: len(report[key]) for key in ["runs", "rejections", "erasure"]}))


if __name__ == "__main__":
    main()
