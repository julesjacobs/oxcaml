(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml vox_sat.mli vox_sat.ml vox_cdcl_total_proof.mli vox_cdcl_total_proof.ml vox_cdcl_total.mli vox_cdcl_total.ml";
 readonly_files = "talk_sat_unknown.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* Talk, section 4f ("totality and certificates: CDCL SAT"). A client of the
   public interface proves that [Unknown] is impossible for the total,
   fuel-free [solve_complete]: its [unreachable_ ()] arm verifies. The same
   client against [solve fuel] is rejected, because a fuelled search may run
   out and answer [Unknown]. *)

#load "vox_sequence.cmo";;
#load "vox_sat_spec.cmo";;
#load "vox_sat_proof.cmo";;
#load "vox_sat.cmo";;
#load "vox_cdcl_total_proof.cmo";;
#load "vox_cdcl_total.cmo";;

let (decide @ total) (n : int) (f : Vox_sat_spec.formula) : bool =
  match Vox_cdcl_total.solve_complete n f with
  | Ok { Vox_cdcl_total.answer = Vox_cdcl_total.Unknown; _ } -> unreachable_ ()
  | Ok _ -> true
  | Error _ -> false;;
[%%expect{|
val decide : int -> Vox_sat_spec.formula -> bool = <fun>
|}]

let (decide_fuel @ total) (fuel : int) (n : int) (f : Vox_sat_spec.formula) : bool =
  match Vox_cdcl_total.solve fuel n f with
  | Ok { Vox_cdcl_total.answer = Vox_cdcl_total.Unknown; _ } -> unreachable_ ()
  | Ok _ -> true
  | Error _ -> false;;
[%%expect{|
Line 3, characters 64-79:
3 |   | Ok { Vox_cdcl_total.answer = Vox_cdcl_total.Unknown; _ } -> unreachable_ ()
                                                                    ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
