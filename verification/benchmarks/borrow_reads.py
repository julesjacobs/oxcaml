#!/usr/bin/env python3
"""Measure borrowed reads against the previous result-pair primitives."""

import argparse
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
PROGRAM = r'''
type handle
external copy : int iarray -> handle = "caml_borrow_of_iarray"
external length : handle -> int = "caml_borrow_length"
external get : handle -> int -> int = "caml_borrow_get"
external snapshot : handle -> int iarray = "caml_borrow_snapshot"
external old_length : handle -> int * handle = "caml_baseline_borrow_length"
external old_get : handle -> int -> int * handle = "caml_baseline_borrow_get"
external old_snapshot : handle -> int iarray * handle
  = "caml_baseline_borrow_snapshot"

let rec lengths remaining state sum =
  if remaining = 0 then sum else
    lengths (remaining - 1) state (sum + length state)
let rec old_lengths remaining state sum =
  if remaining = 0 then sum else
    let value, state = old_length state in
    old_lengths (remaining - 1) state (sum + value)
let rec gets remaining state sum =
  if remaining = 0 then sum else
    gets (remaining - 1) state (sum + get state (remaining land 15))
let rec old_gets remaining state sum =
  if remaining = 0 then sum else
    let value, state = old_get state (remaining land 15) in
    old_gets (remaining - 1) state (sum + value)
let rec snapshots remaining state sum =
  if remaining = 0 then sum else
    let values = snapshot state in
    snapshots (remaining - 1) state (sum + Iarray.length values)
let rec old_snapshots remaining state sum =
  if remaining = 0 then sum else
    let values, state = old_snapshot state in
    old_snapshots (remaining - 1) state (sum + Iarray.length values)

let measure name version iterations expected run state =
  Gc.full_major ();
  let allocated = Gc.allocated_bytes () in
  let start = Sys.time () in
  let result = run iterations state 0 in
  let seconds = Sys.time () -. start in
  let bytes = Gc.allocated_bytes () -. allocated in
  assert (result = expected);
  Printf.printf "%s,%s,%d,%.3f,%.3f\n%!" name version iterations
    (seconds *. 1000.) (bytes /. float_of_int iterations)

let () =
  let state = copy (Iarray.init 16 Fun.id) in
  let repeats = int_of_string Sys.argv.(1) in
  print_endline "operation,version,iterations,cpu_ms,bytes_per_operation";
  for repeat = 1 to repeats do
    List.iter (fun (name, iterations, expected, old_run, run) ->
      let variants =
        if repeat mod 2 = 0 then ["candidate", run; "baseline", old_run]
        else ["baseline", old_run; "candidate", run] in
      List.iter (fun (version, run) ->
        measure name version iterations expected run state) variants)
      ["length", 2_000_000, 32_000_000, old_lengths, lengths;
       "get", 2_000_000, 15_000_000, old_gets, gets;
       "snapshot", 200_000, 3_200_000, old_snapshots, snapshots]
  done
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", default="d9d6a7f6d3")
    parser.add_argument("--repeats", type=int, default=5)
    args = parser.parse_args()
    compiler = ROOT / "_install/bin/ocamlopt"
    if not compiler.is_file():
        parser.error("run make install before benchmarking")
    baseline = subprocess.check_output(
        ["git", "show", f"{args.baseline}:runtime/borrow.c"],
        cwd=ROOT, text=True)
    baseline = baseline.replace("caml_borrow_", "caml_baseline_borrow_")
    baseline = baseline.replace("caml_vox_sequence_length",
                                "caml_baseline_vox_sequence_length")
    with tempfile.TemporaryDirectory(prefix="vox-borrow-reads-") as directory:
        output = Path(directory)
        (output / "baseline.c").write_text(baseline)
        (output / "reads.ml").write_text(PROGRAM)
        subprocess.run([str(compiler), "-c", "baseline.c"],
                       cwd=output, check=True)
        subprocess.run([str(compiler), "baseline.o", "reads.ml", "-o", "reads"],
                       cwd=output, check=True)
        subprocess.run([str(output / "reads"), str(args.repeats)], check=True)


if __name__ == "__main__":
    main()
