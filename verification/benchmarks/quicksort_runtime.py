#!/usr/bin/env python3
"""Compare sequential quicksort implementations with the installed compiler."""

import argparse
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
PROGRAM = r'''
open Borrow

let sort values =
  let refine_ owned = Owned_array.of_iarray values in
  let refine_ sorted = Quicksort.sort_array owned in
  let refine_ result = Owned_array.into_iarray sorted in
  result

let measure name input =
  let expected = List.sort compare (Iarray.to_list input) in
  let seconds = ref 0. and bytes = ref 0. in
  for _ = 1 to 20 do
    Gc.full_major ();
    let allocated = Gc.allocated_bytes () in
    let start = Sys.time () in
    let result = sort input in
    seconds := !seconds +. Sys.time () -. start;
    bytes := !bytes +. Gc.allocated_bytes () -. allocated;
    assert (Iarray.to_list result = expected)
  done;
  Printf.printf "%s,%.3f,%.0f\n%!" name (!seconds *. 50.) (!bytes /. 20.)

let () =
  let random = Random.State.make [|0x51ce|] in
  List.iter (fun (name, input) -> measure name input)
    ["ordered", Iarray.init 4096 Fun.id;
     "reverse", Iarray.init 4096 (fun i -> 4095 - i);
     "duplicates", Iarray.init 4096 (fun i -> i mod 5);
     "random", Iarray.init 4096 (fun _ -> Random.State.int random 65536)]
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", default="13c52c9fe7")
    parser.add_argument("--repeats", type=int, default=5)
    parser.add_argument("--iarray-model", action="store_true")
    args = parser.parse_args()
    compiler = ROOT / "_install/bin/ocamlopt"
    if not compiler.is_file():
        parser.error("run make install before benchmarking")
    modules = [Path("verification/library") / (module + suffix)
               for module in ["vox_sequence", "vox_int_sequence", "borrow"]
               for suffix in [".mli", ".ml"]]
    modules += [Path("testsuite/tests/vox") / name for name in
                ["quicksort_model.ml", "quicksort.mli", "quicksort.ml"]]
    with tempfile.TemporaryDirectory(prefix="vox-quicksort-") as directory:
        executables = {}
        for version in ["baseline", "candidate"]:
            output = Path(directory, version)
            output.mkdir()
            selected = modules
            program = PROGRAM
            if args.iarray_model and version == "candidate":
                selected = [Path("verification/library") / (name + suffix)
                            for name in ["vox_sequence", "vox_int_sequence",
                                         "vox_iarray", "borrow_iarray"]
                            for suffix in [".mli", ".ml"]]
                selected += [Path("testsuite/tests/vox") / name for name in
                             ["quicksort_iarray_model.ml", "quicksort_iarray.mli",
                              "quicksort_iarray.ml"]]
                program = PROGRAM.replace("open Borrow", "open Borrow_iarray")
                program = program.replace("Quicksort.", "Quicksort_iarray.")
            for module in selected:
                source = ((ROOT / module).read_text() if version == "candidate"
                          else subprocess.check_output(
                              ["git", "show", f"{args.baseline}:{module}"],
                              cwd=ROOT, text=True))
                (output / module.name).write_text(source)
            (output / "measure.ml").write_text(program)
            executable = output / "measure"
            subprocess.run([str(compiler), "-extension", "refinement_types",
                            "-principal", "-alert", "-unsafe_multidomain",
                            "-alert", "-do_not_spawn_domains",
                            *[module.name for module in selected],
                            "measure.ml", "-o", str(executable)],
                           cwd=output, check=True, timeout=180)
            executables[version] = executable
        print("version,workload,cpu_ms_per_sort,bytes_per_sort", flush=True)
        for repeat in range(args.repeats):
            order = ["baseline", "candidate"]
            if repeat % 2:
                order.reverse()
            for version in order:
                result = subprocess.check_output(
                    [str(executables[version])], text=True, timeout=180)
                for line in result.splitlines():
                    print(f"{version},{line}", flush=True)


if __name__ == "__main__":
    main()
