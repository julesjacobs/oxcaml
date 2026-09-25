(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_ordered_sequence.ml vox_credits.mli vox_credits.ml vox_merge_proofs.ml vox_sort_cost.ml vox_merge_sort.mli vox_merge_sort.ml merge_sort.ml";
 readonly_files = "merge_sort_rejected.ml";
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
 {
   flags += " -principal";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

open Merge_sort;;
[%%expect{|
|}]

let third () =
  let amount = 2 in
  let initial : {n : int | n >= 0} = amount in
  let token = C.Budget.create initial in
  let input : {t : C.token | C.credits t >= 2} = token in
  let result = two 3 2 1 input in
  let #{ Compare.before = _; state } = result in
  Compare.compare 1 0 (state);;
[%%expect{|
Line 8, characters 22-29:
8 |   Compare.compare 1 0 (state);;
                          ^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let inserted () = ghost_ (
  let left = [] in let right = [1] in
  Sort.P.permutation_def left right;
  Sort.P.same_counts_def right left right;
  Sort.P.count_def left 1; Sort.P.count_def right 1;
  let u = () in
  (u : {u : unit | Sort.P.permutation left right}));;
[%%expect{|
Line 7, characters 3-4:
7 |   (u : {u : unit | Sort.P.permutation left right}));;
       ^
Error: Refinement could not be proved (counterexample)
|}]

let multiplicity () = ghost_ (
  let left = [1; 1] in let right = [1] in
  Sort.P.permutation_def left right;
  Sort.P.same_counts_def left left right;
  Sort.P.count_def left 1; Sort.P.count_def right 1;
  Sort.P.count_def [] 1;
  let u = () in
  (u : {u : unit | Sort.P.permutation left right}));;
[%%expect{|
Line 8, characters 3-4:
8 |   (u : {u : unit | Sort.P.permutation left right}));;
       ^
Error: Refinement could not be proved (counterexample)
|}]

let payload () = ghost_ (
  let a = { Ranked.rank = 1; payload = 10 } in
  let b = { Ranked.rank = 1; payload = 20 } in
  let left = [a; b] in let right = [a; a] in
  Rank_sort.P.permutation_def left right;
  Rank_sort.P.same_counts_def left left right;
  Rank_sort.P.count_def left a; Rank_sort.P.count_def right a;
  Rank_sort.P.count_def [b] a; Rank_sort.P.count_def [a] a;
  Rank_sort.P.count_def [] a;
  let u = () in
  (u : {u : unit | Rank_sort.P.permutation left right}));;
[%%expect{|
Line 11, characters 3-4:
11 |   (u : {u : unit | Rank_sort.P.permutation left right}));;
        ^
Error: Refinement could not be proved (counterexample)
|}]

let false_after_roundtrip () =
  let amount = 2 in
  let initial : {n : int | n >= 0} = amount in
  let token = C.Budget.create initial in
  let one = 1 in
  let input : {t : C.token | 0 <= one && one <= C.credits t} =
    token in
  let parts = C.split one input in
  let { C.left; right } = parts in
  let admissible : {t : C.token | 0 <= C.credits left &&
    0 <= C.credits t && 0 <= C.credits left + C.credits t} = right in
  let joined = C.merge left admissible in
  let u = () in (u : {u : unit | false});;
[%%expect{|
Line 12, characters 6-12:
12 |   let joined = C.merge left admissible in
           ^^^^^^
Warning 26 [unused-var]: unused variable "joined".

Line 13, characters 17-18:
13 |   let u = () in (u : {u : unit | false});;
                      ^
Error: Refinement could not be proved (counterexample)
|}]

let false_after_height_bound () = ghost_ (
  Vox_sort_cost.height_bound 3Z;
  let u = () in (u : {u : unit | false}));;
[%%expect{|
Line 3, characters 17-18:
3 |   let u = () in (u : {u : unit | false}));;
                     ^
Error: Refinement could not be proved (counterexample)
|}]

let false_after_height_minimal () = ghost_ (
  Vox_sort_cost.height_minimal 3Z;
  let u = () in (u : {u : unit | false}));;
[%%expect{|
Line 3, characters 17-18:
3 |   let u = () in (u : {u : unit | false}));;
                     ^
Error: Refinement could not be proved (counterexample)
|}]

let false_after_sort () =
  let values = [2; 1] in
  ghost_ (
    Vox_sequence.length_def values;
    Vox_sequence.length_def [1]; Vox_sequence.length_def [];
    Vox_sort_cost.budget_def 2Z;
    Vox_sort_cost.height_def 2Z; Vox_sort_cost.height_def 1Z);
  let amount = 2 in
  let initial : {n : int | n >= 0} = amount in
  let token = C.Budget.create initial in
  let input : {t : C.token | Vox_sort_cost.budget
    (Vox_sequence.length values) <= Bigint.of_int (C.credits t)} =
    token in
  let result = Sort.sort values input in
  let u = () in (u : {u : unit | false});;
[%%expect{|
Line 14, characters 6-12:
14 |   let result = Sort.sort values input in
           ^^^^^^
Warning 26 [unused-var]: unused variable "result".

Line 15, characters 17-18:
15 |   let u = () in (u : {u : unit | false});;
                      ^
Error: Refinement could not be proved (counterexample)
|}]
