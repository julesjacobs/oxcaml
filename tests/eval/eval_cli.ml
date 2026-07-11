open Oxsmt_core
open Oxsmt_eval

(* CLI: eval <file.smt2> <file.model> exit 0 MODEL-SATISFIES exit 1 MODEL-FAILS <index>
   (subterm values on the failing path -> stderr) exit 2 MALFORMED / UNSUPPORTED (detail
   -> stderr) Digest to stdout; all detail to stderr (DESIGN.md §11 context-frugal). *)

let usage () =
  prerr_endline "usage: eval <file.smt2> <file.model>";
  exit 2
;;

let () =
  let smt2, model_path =
    match Sys.argv with
    | [| _; a; b |] -> a, b
    | _ -> usage ()
  in
  let fail_2 kind detail =
    Printf.eprintf "%s: %s\n" kind detail;
    print_endline kind;
    exit 2
  in
  match
    let query = Reader.read_file smt2 in
    let model = Model.of_file query.Reader.decls model_path in
    Eval.check model query.Reader.assertions
  with
  | Eval.Satisfies ->
    print_endline "MODEL-SATISFIES";
    exit 0
  | Eval.Fails { index; trace } ->
    prerr_string trace;
    Printf.printf "MODEL-FAILS %d\n" index;
    exit 1
  | exception Reader.Unsupported msg -> fail_2 "UNSUPPORTED" msg
  | exception Term.Unsupported msg -> fail_2 "UNSUPPORTED" ("term: " ^ msg)
  | exception Reader.Malformed msg -> fail_2 "MALFORMED" msg
  | exception Model.Malformed msg -> fail_2 "MALFORMED" ("model: " ^ msg)
  | exception Sexp.Parse_error msg -> fail_2 "MALFORMED" ("syntax: " ^ msg)
  | exception Term.Sort_error msg -> fail_2 "MALFORMED" ("sort: " ^ msg)
  | exception Term.Overflow ->
    fail_2 "UNSUPPORTED" "arithmetic overflow during construction"
  | exception Eval.Eval_error msg -> fail_2 "MALFORMED" ("evaluation: " ^ msg)
  | exception Sys_error msg -> fail_2 "MALFORMED" ("io: " ^ msg)
;;
