(* TEST
 readonly_files = "proof_erasure_check.py proof_erasure_quotation.ml";
 {
   setup-ocamlc.byte-build-env;
   flags = "-keywords 5.3 -vox-backend z3 -drawlambda \
            -dno-locations -dno-unique-ids";
   compiler_output = "verified.lambda";
   ocamlc.byte;
   script = "python3 ${test_source_directory}/proof_erasure_check.py \
             lambda-verified verified.lambda";
   script;
   run;
   check-program-output;
 } {
   setup-ocamlc.byte-build-env;
   flags = "-keywords 5.3 -vox-backend z3 -dtypedtree \
            -dno-locations -dno-unique-ids -stop-after typing";
   compiler_output = "verified.typedtree";
   ocamlc.byte;
   script = "python3 ${test_source_directory}/proof_erasure_check.py \
             typedtree verified.typedtree";
   script;
 } {
   setup-ocamlc.byte-build-env;
   flags = "-keywords 5.3 -vox-no-verify -drawlambda \
            -dno-locations -dno-unique-ids";
   compiler_output = "noverify.lambda";
   ocamlc.byte;
   script = "python3 ${test_source_directory}/proof_erasure_check.py \
             lambda-noverify noverify.lambda";
   script;
   run;
   check-program-output;
 } {
   setup-ocamlopt.byte-build-env;
   flags = "-keywords 5.3 -vox-backend z3";
   ocamlopt.byte;
   run;
   check-program-output;
 } {
   setup-ocamlc.byte-build-env;
   flags = "-keywords 5.3 -vox-type-only";
   ocamlc.byte;
 } {
   setup-ocamlc.byte-build-env;
   module = "proof_erasure_quotation.ml";
   flags = "-keywords 5.3 -extension runtime_metaprogramming \
            -vox-backend z3 -dtypedtree -dno-locations \
            -dno-unique-ids -stop-after typing";
   compiler_output = "quotation.typedtree";
   ocamlc.byte;
   script = "python3 ${test_source_directory}/proof_erasure_check.py \
             typedtree-quotation quotation.typedtree";
   script;
 }
*)

type nat = Zero | Succ of nat

let effect_count = ref 0

let (erase_direct @ total) (x : int @ logical) : unit{ x = x } = ()

let rec (pure_expensive @ total) (n : nat) =
  match n with
  | Zero -> 17
  | Succ rest -> pure_expensive rest
;;

let[@vox.def] identity_nat (n : nat @ logical) = n

let erased_direct_site : unit = erase_direct 1
let erased_generated_site : unit = identity_nat_def Zero
let erased_pipe_site : unit = 2 |> erase_direct
let erased_revapply_site : unit = erase_direct @@ 3

let erased_expensive_site : unit =
  erase_direct (pure_expensive (Succ (Succ (Succ Zero))))
;;

let retained_tailcall_site () : unit =
  (erase_direct [@tailcall]) 17
;;

let retained_inlined_site : unit =
  (erase_direct [@inlined]) 18
;;

let retained_unrolled_site : unit =
  (erase_direct [@unrolled 1]) 19
;;

let retained_specialised_site : unit =
  (erase_direct [@specialised]) 20
;;

let retained_zero_alloc_site : unit =
  (erase_direct [@zero_alloc assume]) 21
;;

let retained_nested_attribute_argument_site : unit =
  erase_direct
    (let _nested = (erase_direct [@inlined]) 22 in
     23)
;;

module Shadowed_unit = struct
  type unit = Unit

  let (keep_shadowed_unit @ total) (_ : int @ logical) : unit{ true } =
    Unit
  ;;

  let retained_shadowed_unit_site : unit =
    keep_shadowed_unit 24
  ;;
end

let keep_effectful (x : int @ logical) : unit{ x = x } =
  incr effect_count
;;

let retained_effectful_body_site : unit = keep_effectful 4

let rec keep_unproved_recursion (x : int @ logical) : unit{ true } =
  keep_unproved_recursion x
;;

let retained_unproved_recursion_site () : unit =
  keep_unproved_recursion 0
;;

let retained_effectful_argument_site : unit =
  erase_direct (incr effect_count; 5)
;;

let retained_pipe_argument_site : unit =
  (incr effect_count; 6) |> erase_direct
;;

let retained_revapply_argument_site : unit =
  erase_direct @@ (incr effect_count; 7)
;;

let (keep_alias_target @ total) (x : int @ logical) : unit{ x = x } = ()
let keep_alias = keep_alias_target
let retained_alias_site : unit = keep_alias 8

let keep_higher_order
    (function_ : ((x : int) @ logical -> unit{ x = x }) @ total)
    (argument : int @ logical) =
  function_ argument
;;

let retained_higher_order_site : unit =
  keep_higher_order keep_alias_target 9
;;

let (keep_partial_target @ total)
    (left : int @ logical)
    (right : int @ logical)
    : unit{ left = left && right = right } =
  ()
;;

let keep_partial = keep_partial_target 10
let retained_partial_site : unit = keep_partial 11

let (keep_bare @ total) (_ : int @ logical) = ()
let retained_bare_site : unit = keep_bare 12

let (keep_nonunit @ total) (x : int @ logical) : int{ _ = x } = x
let retained_nonunit_site : int = keep_nonunit 13

let (assert_trap @ total) () =
  assert false
;;

let (keep_assert @ total) (_ : int @ logical) : unit{ true } =
  assert_trap ();
  ()
;;

let rec (rollback_safe @ total) (_ : int @ logical) : unit{ true } = ()
and (rollback_trapping @ total) (_ : int @ logical) : unit{ true } =
  assert_trap ();
  ()
;;

let retained_recursive_rollback_site : unit = rollback_safe 0

let (return_parameter @ total)
    (function_ : ((x : int) @ logical -> unit{ true }) @ total)
    : ((y : int) @ logical -> unit{ true }) @ total =
  function_
;;

let keep_assert_via_return_parameter () =
  return_parameter keep_assert 14
;;

let retained_overapplication_site =
  try
    keep_assert_via_return_parameter ();
    false
  with
  | Assert_failure _ -> true
;;

let retained_assert_site =
  try
    keep_assert 14;
    false
  with
  | Assert_failure _ -> true
;;

let (partial_match_trap @ total) value =
  match value with
  | true -> ()
;;

let (keep_partial_match @ total) (value : bool @ logical) : unit{ true } =
  partial_match_trap value;
  ()
;;

let retained_partial_match_site =
  try
    keep_partial_match false;
    false
  with
  | Match_failure _ -> true
;;

let (keep_transitive @ total) (x : int @ logical) : unit{ true } =
  keep_assert x
;;

let retained_transitive_site =
  try
    keep_transitive 15;
    false
  with
  | Assert_failure _ -> true
;;

let (trapping_value @ total) value =
  match value with
  | true -> 16
;;

let retained_trapping_argument_site =
  try
    erase_direct (trapping_value false);
    false
  with
  | Match_failure _ -> true
;;

let () =
  Printf.printf
    "proof-erasure-effects:%d traps:%b/%b/%b/%b/%b\n"
    !effect_count
    retained_overapplication_site
    retained_assert_site
    retained_partial_match_site
    retained_transitive_site
    retained_trapping_argument_site
;;
