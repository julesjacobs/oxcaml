(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml vox_sat.mli vox_sat.ml sat_total_probe.ml";
 { bytecode; }
*)

open Vox_sat_spec

type reason = Decision | Original of int | Learned of int [@@inductive]
type binding : immutable_data mod total = {
  value : bool;
  level : int;
  reason : reason;
}
type trail_item : immutable_data mod total = {
  literal : Vox_sat_spec.literal;
  level : int;
}
type state : immutable_data mod total = {
  bindings : binding option list;
  trail : trail_item list;
  level : int;
  decisions : int;
  conflicts : int;
  learned : int;
  backjumps : int;
  steps : int;
}
let (bindings @ total) (state : state @ immutable total) =
  state.bindings

let (first_reason @ total) (state : state @ immutable total) =
  match bindings state with
  | Some binding :: _ ->
    (match binding.reason with
     | Decision -> 0
     | Original index -> index + 1
     | Learned ordinal -> ordinal + 1)
  | None :: _ | [] -> 0
