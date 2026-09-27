(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml";
 {
   reference = "${test_source_directory}/pref_layout.byte.reference";
   bytecode;
 }
 {
   reference = "${test_source_directory}/pref_layout.reference";
   native;
 }
*)

let () =
  let value = 7 in
  let refine_ t = Pref.empty () in
  let refine_ r = Pref.alloc value t in
  Printf.printf "%d\n" (Obj.size (Obj.repr r))
