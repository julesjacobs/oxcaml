(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_ordered_sequence.ml vox_credits.mli vox_credits.ml vox_merge_proofs.ml vox_sort_cost.mli vox_sort_cost.ml vox_merge_sort.mli vox_merge_sort.ml merge_sort.ml";
 readonly_files = "merge_sort_rejected.ml";
 {
   setup-ocamlc.opt-build-env;
   binary_modules = "prebuilt/vox_sequence prebuilt/vox_ordered_sequence prebuilt/vox_credits prebuilt/vox_merge_proofs prebuilt/vox_sort_cost prebuilt/vox_merge_sort prebuilt/merge_sort";
   run-expect;
   check-program-output;
 }
*)

open Merge_sort;;
[%%expect{|
|}]

let third () =
  let amount = 2 in
  let token = C.Budget.create amount in
  let result = two 3 2 1 token in
  let #{ Compare.before = _; state } = result in
  Compare.compare 1 0 (state);;
[%%expect{|
Line 6, characters 22-29:
6 |   Compare.compare 1 0 (state);;
                          ^^^^^^^
Error: Refinement could not be proved (counterexample)
File "merge_sort.ml", line 26, characters 30-45:
  The refinement is stated here.
|}]

let inserted () = ghost_ (
  let left = [] in let right = [1] in
  Sort.P.permutation_count left right 1;
  Sort.P.count_def left 1; Sort.P.count_def right 1;
  let u = () in
  (u : {u : unit | Sort.P.permutation left right}));;
[%%expect{|
Line 6, characters 3-4:
6 |   (u : {u : unit | Sort.P.permutation left right}));;
       ^
Error: Refinement could not be proved (counterexample)
Line 6, characters 19-48:
6 |   (u : {u : unit | Sort.P.permutation left right}));;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let multiplicity () = ghost_ (
  let left = [1; 1] in let right = [1] in
  Sort.P.permutation_count left right 1;
  Sort.P.count_def left 1; Sort.P.count_def right 1;
  Sort.P.count_def [] 1;
  let u = () in
  (u : {u : unit | Sort.P.permutation left right}));;
[%%expect{|
Line 7, characters 3-4:
7 |   (u : {u : unit | Sort.P.permutation left right}));;
       ^
Error: Refinement could not be proved (counterexample)
Line 7, characters 19-48:
7 |   (u : {u : unit | Sort.P.permutation left right}));;
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let payload () = ghost_ (
  let a = { Ranked.rank = 1; payload = 10 } in
  let b = { Ranked.rank = 1; payload = 20 } in
  let left = [a; b] in let right = [a; a] in
  Rank_sort.P.permutation_count left right a;
  Rank_sort.P.count_def left a; Rank_sort.P.count_def right a;
  Rank_sort.P.count_def [b] a; Rank_sort.P.count_def [a] a;
  Rank_sort.P.count_def [] a;
  let u = () in
  (u : {u : unit | Rank_sort.P.permutation left right}));;
[%%expect{|
Line 10, characters 3-4:
10 |   (u : {u : unit | Rank_sort.P.permutation left right}));;
        ^
Error: Refinement could not be proved (counterexample)
Line 10, characters 19-53:
10 |   (u : {u : unit | Rank_sort.P.permutation left right}));;
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
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
Line 13, characters 33-38:
13 |   let u = () in (u : {u : unit | false});;
                                      ^^^^^
  The refinement is stated here.
|}]

let false_after_height_bound () = ghost_ (
  Vox_sort_cost.height_bound 3Z;
  let u = () in (u : {u : unit | false}));;
[%%expect{|
Line 3, characters 17-18:
3 |   let u = () in (u : {u : unit | false}));;
                     ^
Error: Refinement could not be proved (counterexample)
Line 3, characters 33-38:
3 |   let u = () in (u : {u : unit | false}));;
                                     ^^^^^
  The refinement is stated here.
|}]

let false_after_height_minimal () = ghost_ (
  Vox_sort_cost.height_minimal 3Z;
  let u = () in (u : {u : unit | false}));;
[%%expect{|
Line 3, characters 17-18:
3 |   let u = () in (u : {u : unit | false}));;
                     ^
Error: Refinement could not be proved (counterexample)
Line 3, characters 33-38:
3 |   let u = () in (u : {u : unit | false}));;
                                     ^^^^^
  The refinement is stated here.
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
Line 15, characters 33-38:
15 |   let u = () in (u : {u : unit | false});;
                                      ^^^^^
  The refinement is stated here.
|}]

(* [sort] of two elements needs budget 2 = 2 * height 2 credits; one credit
   is rejected at the call, and the same call with two credits is
   accepted. *)
let underfunded_sort () =
  let values = [2; 1] in
  ghost_ (
    Vox_sequence.length_def values;
    Vox_sequence.length_def [1]; Vox_sequence.length_def [];
    Vox_sort_cost.budget_def 2Z;
    Vox_sort_cost.height_def 2Z; Vox_sort_cost.height_def 1Z);
  let amount = 1 in
  let initial : {n : int | n >= 0} = amount in
  let token = C.Budget.create initial in
  let #{ Sort.values = sorted; state = _ } = Sort.sort values token in
  sorted;;
[%%expect{|
Line 11, characters 62-67:
11 |   let #{ Sort.values = sorted; state = _ } = Sort.sort values token in
                                                                   ^^^^^
Error: Refinement could not be proved (counterexample)
File "vox_merge_sort.mli", lines 58-59, characters 30-35:
  The refinement is stated here.
|}]

let exactly_funded_sort () =
  let values = [2; 1] in
  ghost_ (
    Vox_sequence.length_def values;
    Vox_sequence.length_def [1]; Vox_sequence.length_def [];
    Vox_sort_cost.budget_def 2Z;
    Vox_sort_cost.height_def 2Z; Vox_sort_cost.height_def 1Z);
  let amount = 2 in
  let initial : {n : int | n >= 0} = amount in
  let token = C.Budget.create initial in
  let #{ Sort.values = sorted; state = _ } = Sort.sort values token in
  sorted;;
[%%expect{|
val exactly_funded_sort : unit -> Merge_sort.O.elt list = <fun>
|}]
