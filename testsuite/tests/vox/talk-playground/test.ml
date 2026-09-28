(* TEST
 has-z3;
 arguments = "${test_source_directory}/run.sh ${ocamlc_opt} ${ocamlsrcdir}/stdlib ${test_source_directory}/../../../../verification/playground/examples refinements.ml counterexample.ml lemma.ml ghost_token.ml";
 {
   setup-ocamlc.opt-build-env;
   ocamlc.opt;
   run;
   check-program-output;
 }
*)

(* Talk, section 1 ("the language in 150 seconds"): the four scripted
   playground examples, compiled by the native compiler from
   verification/playground/examples at this commit. The playground, the
   compiler compiled to JavaScript, must agree with these verdicts
   (verification/playground/differential/run.py); this test pins the native
   side:
   - refinements.ml: a dependent parameter; accepted.
   - counterexample.ml: [succ] is rejected with
     "counterexample: x = 4611686018427387903" ([x + 1 > x] fails at
     max_int); [next], which assumes [x < max_int], is accepted.
   - lemma.ml: recursion is induction; accepted.
   - ghost_token.ml: reusing a unique ghost token is an ordinary
     uniqueness error. *)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  exit (Sys.command (Filename.quote_command "sh" arguments))
