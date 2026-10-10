(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_machine_semantics.mli vox_machine_semantics.ml expression_folding.mli expression_folding.ml";
 { bytecode; }
 { native; }
*)
open Expression_folding
module M = Vox_machine_semantics

let add_folded (left : int) (right : int) :
    {result : int | Bigint.of_int result =
      M.signed (Bigint.add (Bigint.of_int left) (Bigint.of_int right))} =
  let a = Lit left in
  let b = Lit right in
  let expression = Add (a, b) in
  ghost_ (eval_def a 0; eval_def b 0; eval_def expression 0; M.add left right);
  eval_folded expression 0

let (preserves @ total) (expression : t) input :
    {result : int | result === eval expression input} =
  let folded = fold expression in
  ghost_ (fold_correct expression input);
  eval folded input

let (simplify @ total) (expression : t) : {result : t | folded result} =
  ghost_ (fold_is_folded expression);
  fold expression

let () =
  assert (add_folded max_int 1 = min_int);
  assert (add_folded min_int (-1) = max_int);
  assert (add_folded 19 23 = 42);
  let expression = Add (Lit 0, Add (Input, Add (Lit 0, Lit 3))) in
  assert (eval (simplify expression) 39 = 42);
  assert (preserves expression 39 = 42);
  assert (eval_folded expression 39 = 42)

let (folded_alternatives @ total) input :
    {u : unit | folded (Add (Input, Lit 1))
      && folded (Add (Lit 1, Input))
      && eval (Add (Input, Lit 1)) input ===
         eval (Add (Lit 1, Input)) input
      && not (Add (Input, Lit 1) === Add (Lit 1, Input))} =
  ghost_ (
    folded_def Input; folded_def (Lit 1);
    folded_def (Add (Input, Lit 1)); folded_def (Add (Lit 1, Input));
    eval_def Input input; eval_def (Lit 1) input;
    eval_def (Add (Input, Lit 1)) input;
    eval_def (Add (Lit 1, Input)) input);
  ()
