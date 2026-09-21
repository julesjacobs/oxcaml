(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml";
 readonly_files = "pref_equal_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

let wrong (p : int Pref.t @ immutable) : {b : bool | not b} =
  let refine_ b = Pref.equal p p in refine_ b;;
[%%expect{|
Line 2, characters 36-45:
2 |   let refine_ b = Pref.equal p p in refine_ b;;
                                        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_branch (p : int Pref.t @ immutable) (q : int Pref.t @ immutable) =
  let refine_ equal = Pref.equal p q in
  if equal then
    let u = () in
    let refine_ impossible = (refine_ u : {u : unit | not (p === q)}) in ()
  else ();;
[%%expect{|
Line 5, characters 30-39:
5 |     let refine_ impossible = (refine_ u : {u : unit | not (p === q)}) in ()
                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let wrong_other_branch (p : int Pref.t @ immutable) (q : int Pref.t @ immutable) =
  let refine_ equal = Pref.equal p q in
  if equal then () else
    let u = () in
    let refine_ impossible = (refine_ u : {u : unit | p === q}) in ();;
[%%expect{|
Line 5, characters 30-39:
5 |     let refine_ impossible = (refine_ u : {u : unit | p === q}) in ();;
                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
