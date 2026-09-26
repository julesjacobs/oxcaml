(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_int_sequence.mli vox_iarray.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "vox_int_sequence.mli";
 ocamlc.opt;
 module = "vox_iarray.mli";
 ocamlc.opt;
 expect;
*)

#directory "ocamlc.opt";;

let wrong_update () =
  let values = [: 1; 2 :] in
  let index = 0 in
  let value = 3 in
  let query = 1 in
  let changed = Vox_iarray.updated values index value in
  let _ = Vox_iarray.updated_read values index value query in
  let (_ : {a : int iarray | Vox_iarray.at a 1 === Some 3}) = changed in
  ();;
[%%expect{|
Line 8, characters 62-69:
8 |   let (_ : {a : int iarray | Vox_iarray.at a 1 === Some 3}) = changed in
                                                                  ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_extensional () =
  let left = [: 1 :] in
  let right = [: 2 :] in
  let _ = Vox_iarray.extensional left right
    (fun index -> let u = () in u) in
  ();;
[%%expect{|
Line 5, characters 32-33:
5 |     (fun index -> let u = () in u) in
                                    ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_length () =
  let left = [: 1 :] in
  let right = [: 1; 1 :] in
  let _ = Vox_iarray.extensional left right
    (fun index -> let u = () in u) in
  ();;
[%%expect{|
Line 5, characters 32-33:
5 |     (fun index -> let u = () in u) in
                                    ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_predicate_intro () =
  let module P = struct
    type element = int
    let[@def] test (value : int) = value >= 0
  end in
  let module All = Vox_iarray.For_all (P) in
  let values = [: -1 :] in
  let _ = All.intro values
    (fun index -> let u = () in u) in
  ();;
[%%expect{|
Line 9, characters 32-33:
9 |     (fun index -> let u = () in u) in
                                    ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_list_predicate_intro () =
  let module P = struct
    type element = int
    let[@def] test (value : int) = value >= 0
  end in
  let module All = Vox_sequence.For_all (P) in
  let values = [-1] in
  let _ = All.intro values
    (fun index -> let u = () in u) in
  ();;
[%%expect{|
Line 9, characters 32-33:
9 |     (fun index -> let u = () in u) in
                                    ^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_map_length (values : int list) =
  let module F = struct
    type input = int
    type output = int
    let[@def] apply (value : int) = value
  end in
  let module M = Vox_sequence.Map (F) in
  let result = M.map values in
  let _ = M.map_length values in
  let (_ : {u : unit | Vox_sequence.length result < Vox_sequence.length values}) =
    let u = () in u in
  ();;
[%%expect{|
Line 11, characters 18-19:
11 |     let u = () in u in
                       ^
Error: Refinement could not be proved (counterexample)
|}]
