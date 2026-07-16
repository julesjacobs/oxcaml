(* chc_cli <file.smt2>: parse a HORN problem, solve it, print one verdict line.

   Output is the SMT-LIB HORN convention: [sat] (SAFE — an interpretation / inductive
   invariant exists), [unsat] (UNSAFE — a derivation of false exists), or [unknown]. With
   [-v] the detail/provenance line is printed to stderr. *)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let () =
  let verbose = ref false in
  let file = ref None in
  Array.iteri
    (fun i a ->
      if i > 0 then if String.equal a "-v" then verbose := true else file := Some a)
    Sys.argv;
  match !file with
  | None ->
    prerr_endline "usage: chc_cli [-v] <file.smt2>";
    exit 2
  | Some path ->
    let src = read_file path in
    let result =
      match Oxsmt_chc.Chc_parse.parse src with
      | sys -> Oxsmt_chc.Chc_engine.solve sys
      | exception Oxsmt_chc.Chc_parse.Unsupported m ->
        { Oxsmt_chc.Chc_engine.verdict = Oxsmt_chc.Chc_engine.Unknown ("unsupported: " ^ m); detail = m }
      | exception Oxsmt_chc.Chc_parse.Malformed m ->
        { Oxsmt_chc.Chc_engine.verdict = Oxsmt_chc.Chc_engine.Unknown ("malformed: " ^ m); detail = m }
    in
    print_endline (Oxsmt_chc.Chc_engine.verdict_to_smtlib result.Oxsmt_chc.Chc_engine.verdict);
    if !verbose then prerr_endline ("; " ^ result.Oxsmt_chc.Chc_engine.detail)
;;
