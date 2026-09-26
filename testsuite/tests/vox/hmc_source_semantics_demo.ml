(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml hmc_word64.ml hm_declarative.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_elaboration_check.ml hm_checked_elaboration.mli hm_checked_elaboration.ml hm_interpreter_typing.ml hm_interpreter_proofs.ml hm_interpreter_substitution.ml hm_interpreter_substitution_proofs.ml hmc_source_semantics.ml hm_evaluation.ml hm_evaluation_machine.ml hm_evaluation_continuation.ml hm_evaluation_reverse.ml hm_interpreter.mli hm_interpreter.ml hmc_source_safety.ml hmc_source_semantics_demo.ml";
 { bytecode; }
*)
module D = Hm_declarative
module V = Hm_interpreter_typing
module S = Hmc_source_semantics
module P = Hmc_source_safety
module C = Hm_checked_elaboration
module I = Hm_interpreter

let rec fuel n = if n <= 0 then D.Z else D.S (fuel (n - 1))
let budget = fuel 2000
let zero = {Hmc_word64.lo = 0; hi = 0}
let one = {Hmc_word64.lo = 1; hi = 0}
let two = {Hmc_word64.lo = 2; hi = 0}
let maximum = {Hmc_word64.lo = 4294967295; hi = 4294967295}
let variable = D.Variable D.No_arguments
let literal = D.Word_constant
let list_ty = D.List_type D.Word64
let nil_d = D.Empty_list D.Word64
let list = D.Cons (D.Word one, D.Cons (D.Word one, D.Nil))
let list_d = D.List_cons (D.Word64, literal, D.List_cons (D.Word64, literal, nil_d))
let diverge = D.Apply (D.Recursive (D.Apply (D.Bound (D.S D.Z), D.Bound D.Z)), D.Word zero)
let diverge_d = D.Application (D.Word64,
  D.Recursion (D.Word64, D.Word64, D.Application (D.Word64, variable, variable)), literal)

let expect term ty proof expected =
  match C.check term ty proof with
  | None -> failwith "invalid source machine fixture"
  | Some checked ->
    let d = C.derivation checked in
    ghost_ (P.source_safe budget term ty d ());
    let direct = I.run term #{I.ty = ghost_ ty; derivation = ghost_ d} in
    ghost_ (
      let steps = Hm_evaluation_machine.closed term direct.#value direct.#evidence.execution () in
      let execution = Hm_evaluation_reverse.closed steps term direct.#value () in
      let _ = execution in ());
    if direct.#value <> expected then failwith "direct evaluator mismatch";
    match S.observe budget (S.initial term) with
    | S.Finished v when v = expected -> ()
    | S.Finished _ -> failwith "source machine result mismatch"
    | S.Invalid -> failwith "typed source machine stuck"
    | S.Suspended _ -> failwith "source machine fuel exhausted"

let expect_suspended term =
  match S.observe budget (S.initial term) with
  | S.Suspended _ -> () | _ -> failwith "expected suspended computation"

let expect_invalid term =
  match S.observe budget (S.initial term) with
  | S.Invalid -> () | _ -> failwith "expected invalid computation"

let () =
  expect D.Truth D.Boolean D.Constant V.True;
  expect D.False D.Boolean D.Constant V.False;
  expect (D.Word maximum) D.Word64 literal (V.Word maximum);
  expect D.Nil list_ty nil_d V.Nil;
  expect list list_ty list_d (V.Cons (V.Word one, V.Cons (V.Word one, V.Nil)));
  expect (D.Apply (D.Lambda (D.Bound D.Z), D.Word one)) D.Word64
    (D.Application (D.Word64, D.Abstraction (D.Word64, variable), literal)) (V.Word one);
  let word_scheme = D.Forall (D.Z, D.Word64) in
  expect (D.Let (D.Word one, D.Apply (D.Lambda (D.Bound (D.S D.Z)), D.Word zero)))
    D.Word64 (D.Let_binding (word_scheme, literal,
      D.Application (D.Word64, D.Abstraction (D.Word64, variable), literal))) (V.Word one);
  expect (D.CaseList (list, D.Word zero, D.Bound D.Z)) D.Word64
    (D.List_case (D.Word64, list_d, literal, variable)) (V.Word one);
  expect (D.CaseList (list, D.Nil, D.Bound (D.S D.Z))) list_ty
    (D.List_case (D.Word64, list_d, nil_d, variable)) (V.Cons (V.Word one, V.Nil));
  expect (D.CaseList (D.Nil, D.Word one, diverge)) D.Word64
    (D.List_case (D.Word64, nil_d, literal, diverge_d)) (V.Word one);
  expect (D.If (D.Truth, D.Word one, diverge)) D.Word64
    (D.Conditional (D.Constant, literal, diverge_d)) (V.Word one);
  expect (D.If (D.False, diverge, D.Word one)) D.Word64
    (D.Conditional (D.Constant, diverge_d, literal)) (V.Word one);
  let op_d = D.Word_primitive (literal, literal) in
  expect (D.Primitive (D.Add, D.Word maximum, D.Word one)) D.Word64 op_d (V.Word zero);
  expect (D.Primitive (D.Subtract, D.Word zero, D.Word one)) D.Word64 op_d (V.Word maximum);
  expect (D.Primitive (D.Equal_word, D.Word maximum, D.Word maximum)) D.Boolean op_d V.True;
  expect (D.Primitive (D.Unsigned_less, D.Word maximum, D.Word zero)) D.Boolean op_d V.False;
  let sum = D.Recursive (D.CaseList (D.Bound D.Z, D.Word zero,
    D.Primitive (D.Add, D.Bound D.Z,
      D.Apply (D.Bound (D.S (D.S (D.S D.Z))), D.Bound (D.S D.Z))))) in
  let sum_d = D.Recursion (list_ty, D.Word64, D.List_case (D.Word64, variable, literal,
    D.Word_primitive (variable, D.Application (list_ty, variable, variable)))) in
  expect (D.Apply (sum, list)) D.Word64 (D.Application (list_ty, sum_d, list_d)) (V.Word two);
  let scheme = D.Forall (D.S D.Z, D.List_type (D.Parameter D.Z)) in
  expect (D.Let (D.Nil, D.Cons (D.Word one, D.Bound D.Z))) list_ty
    (D.Let_binding (scheme, D.Empty_list (D.Parameter D.Z),
      D.List_cons (D.Word64, literal, D.Variable (D.Argument (D.Word64, D.No_arguments)))))
    (V.Cons (V.Word one, V.Nil));
  expect_suspended diverge;
  expect_invalid (D.Primitive (D.Add, D.Bound D.Z, diverge));
  expect_suspended (D.Primitive (D.Add, diverge, D.Bound D.Z));
  expect_invalid (D.Cons (D.Bound D.Z, diverge));
  expect_suspended (D.Cons (diverge, D.Bound D.Z));
  expect_invalid (D.Apply (D.Truth, D.Word zero));
  expect_invalid (D.If (D.Word one, D.Truth, D.False));
  (match S.observe (fuel 1) (S.initial (D.Word one)) with
  | S.Suspended rest -> (match S.observe (fuel 1) rest with
    | S.Finished (V.Word w) when Hmc_word64.equal w one -> ()
    | _ -> failwith "resume result")
  | _ -> failwith "one-step suspension");
  (match C.check (D.Bound D.Z) D.Word64 variable with
  | None -> () | Some _ -> failwith "unbound variable admitted")
