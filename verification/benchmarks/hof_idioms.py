#!/usr/bin/env python3
"""Reproduce the HOF idiom study with an installed Vox compiler."""
import argparse
import json
import re
from pathlib import Path
import shutil
import subprocess
import time


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("prefix", type=Path)
    parser.add_argument("--repetitions", type=int, default=3)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    prefix = args.prefix.resolve()
    output = root / "_build" / "hof-study"
    output.mkdir(parents=True, exist_ok=True)
    library = root / "verification" / "library"
    tests = root / "testsuite" / "tests" / "vox"
    units = [
        (library, "vox_sequence.mli"), (library, "vox_sequence.ml"),
        (library, "vox_int_sequence.mli"), (library, "vox_int_sequence.ml"),
        (library, "vox_iarray.mli"), (library, "vox_iarray.ml"),
        (library, "vox_traversal.mli"), (library, "vox_traversal.ml"),
        (tests, "hof_candidates.ml"), (tests, "collection_functions.ml"),
        (tests, "hof_challenges.ml"), (tests, "hof_array_clients.ml"),
    ]
    flags = ["-nostdlib", "-I", str(prefix / "lib" / "ocaml"),
             "-I", str(output), "-extension", "refinement_types", "-principal"]

    def run(command):
        started = time.perf_counter()
        completed = subprocess.run(command, cwd=output, text=True,
                                   capture_output=True)
        if completed.returncode:
            raise RuntimeError(completed.stdout + completed.stderr)
        return time.perf_counter() - started, completed.stdout

    results = {"prefix": str(prefix), "compile_seconds": {},
               "count_client_seconds": {}}
    clients = {}
    source = (tests / "collection_functions.ml").read_text()
    common = ["count_relation", "increment_relation", "increment",
              "count_preserved"]
    dependencies = {
        "count": ["count_relation", "count_step"],
        "count_separate": common,
        "count_by_model": common + ["next_count", "count_exact",
                                    "count_model_preserved"],
        "count_by_trace": common,
    }
    results["count_client_source_lines"] = {}
    for name, helpers in dependencies.items():
        bodies = []
        for binding in [*helpers, name]:
            match = re.search(r"^let(?:\[@def\])? \(?" + binding +
                              r"\b.*?(?=^let|\Z)", source,
                              re.MULTILINE | re.DOTALL)
            if match is None:
                raise ValueError("Missing count binding: " + binding)
            bodies.append(match.group())
        filename = "client_" + name + ".ml"
        body = ("open Vox_sequence\nopen Vox_traversal\nopen Hof_candidates\n\n"
                + "".join(bodies))
        (output / filename).write_text(body)
        results["count_client_source_lines"][name] = len(body.splitlines())
        clients[name] = filename
    for directory, name in units:
        shutil.copyfile(directory / name, output / name)
    for compiler, suffix in [("ocamlc", "cmo"), ("ocamlopt", "cmx")]:
        executable = str(prefix / "bin" / compiler)
        timings = {}
        for _, name in units:
            timings[name] = [run([executable, *flags, "-c", name])[0]]
        for _ in range(args.repetitions - 1):
            for name in ("vox_traversal.ml", "hof_candidates.ml",
                         "collection_functions.ml", "hof_challenges.ml"):
                timings[name].append(run([executable, *flags, "-c", name])[0])
        objects = [Path(name).with_suffix("." + suffix).name
                   for _, name in units if name.endswith(".ml")]
        program = "clients-" + compiler
        run([executable, *flags, "-o", program, *objects])
        run([str(output / program)])
        results["compile_seconds"][compiler] = timings
        results["count_client_seconds"][compiler] = {
            name: [run([executable, *flags, "-c", filename])[0]
                   for _ in range(args.repetitions)]
            for name, filename in clients.items()
        }
    allocation = output / "alloc.ml"
    allocation.write_text('''
let measure name f =
  let xs = List.init 1000 (fun _ -> 1) in
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to 100 do ignore (Sys.opaque_identity (f xs)) done;
  let words = ((Gc.quick_stat ()).minor_words -. before) /. 100. in
  Printf.printf "%s %.2f\\n" name words
let () =
  measure "ih" (fun xs -> let refine_ n = Collection_functions.count xs in n);
  measure "preservation" (fun xs ->
    let refine_ n = Collection_functions.count_separate xs in n);
  measure "model" (fun xs ->
    let refine_ n = Collection_functions.count_by_model xs in n);
  measure "trace" (fun xs ->
    let refine_ n = Collection_functions.count_by_trace xs in n)
''')
    results["minor_words_per_1000_elements"] = {}
    for compiler, suffix in [("ocamlc", "cmo"), ("ocamlopt", "cmx")]:
        executable = str(prefix / "bin" / compiler)
        run([executable, *flags, "-c", "alloc.ml"])
        objects = [Path(name).with_suffix("." + suffix).name
                   for _, name in units if name.endswith(".ml")]
        program = "alloc-" + compiler
        run([executable, *flags, "-o", program, *objects, "alloc." + suffix])
        _, allocation_output = run([str(output / program)])
        results["minor_words_per_1000_elements"][compiler] = {
            name: float(words) for name, words in
            (line.split() for line in allocation_output.splitlines())
        }
    destination = output / "results.json"
    destination.write_text(json.dumps(results, indent=2) + "\n")
    print(destination)
    print(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
