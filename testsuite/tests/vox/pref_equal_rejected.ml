(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "pref.mli pref.ml";
 readonly_files = "pref_equal_rejected.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

let wrong (p : int Pref.t @ immutable) : {b : bool | not b} =
  let b = Pref.equal p p in b;;
[%%expect{|
Line 2, characters 28-29:
2 |   let b = Pref.equal p p in b;;
                                ^
Error: Refinement could not be proved (counterexample)
Line 1, characters 53-58:
1 | let wrong (p : int Pref.t @ immutable) : {b : bool | not b} =
                                                         ^^^^^
  The refinement is stated here.
|}]

let wrong_branch (p : int Pref.t @ immutable) (q : int Pref.t @ immutable) =
  let equal = Pref.equal p q in
  if equal then
    let u = () in
    let _ = (u : {u : unit | not (p === q)}) in ()
  else ();;
[%%expect{|
Line 5, characters 13-14:
5 |     let _ = (u : {u : unit | not (p === q)}) in ()
                 ^
Error: Refinement could not be proved (counterexample)
Line 5, characters 29-42:
5 |     let _ = (u : {u : unit | not (p === q)}) in ()
                                 ^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let wrong_other_branch (p : int Pref.t @ immutable) (q : int Pref.t @ immutable) =
  let equal = Pref.equal p q in
  if equal then () else
    let u = () in
    let _ = (u : {u : unit | p === q}) in ();;
[%%expect{|
Line 5, characters 13-14:
5 |     let _ = (u : {u : unit | p === q}) in ();;
                 ^
Error: Refinement could not be proved (counterexample)
Line 5, characters 29-36:
5 |     let _ = (u : {u : unit | p === q}) in ();;
                                 ^^^^^^^
  The refinement is stated here.
|}]
