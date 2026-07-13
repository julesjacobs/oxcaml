(* End-to-end acceptance for the QF_BV lane THROUGH THE REAL DISPATCH: parse each
   tests/bv-goldens/*.smt2 with the (test-only) SMT-LIB parser, load it into the shipped
   Session, and run Session.check_sat — which routes a pure-QF_BV assertion set to the
   eager bit-blaster (Bv_dispatch/Bv_solve) and leaves anything mixed on the combinator's
   fail-closed degrade path. The produced verdict must equal the file's declared
   [:status]; a sat additionally must surface a model (Bv_solve has already re-checked
   that model with the independent evaluator before returning Sat, so a surfaced sat is
   self-certified).

   Coverage: two sat files (bvadd inverse, concat/extract), two unsat files (bvand-zero,
   bvult-irreflexive), and one DOOR TEST — a QF_UFBV file with a genuine uninterpreted
   function over bitvectors, which the conservative pure-BV gate must reject so it
   degrades to a sound unknown (never a fabricated verdict). Self-contained: no z3, no
   external oracle. Nonzero exit on any mismatch. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser

let failures = ref 0

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

(* pull the token after "(set-info :status " — sat | unsat | unknown *)
let status_of src =
  let marker = ":status" in
  match
    let re_len = String.length marker in
    let rec find i =
      if i + re_len > String.length src
      then None
      else if String.sub src i re_len = marker
      then Some (i + re_len)
      else find (i + 1)
    in
    find 0
  with
  | None -> "unknown"
  | Some j ->
    let n = String.length src in
    let k = ref j in
    while !k < n && (src.[!k] = ' ' || src.[!k] = '\t') do
      incr k
    done;
    let start = !k in
    while !k < n && src.[!k] <> ' ' && src.[!k] <> ')' && src.[!k] <> '\n' do
      incr k
    done;
    String.sub src start (!k - start)
;;

let solve src =
  let s = Session.create () in
  match Parser.parse_into (Session.env s) (Session.context s) src with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> "unknown", None
  | parsed ->
    if not (Oxsmt_query_loader.assert_all ~presolve:true s parsed)
    then "unknown", None
    else (
      match Session.check_sat s with
      | Session.Sat -> "sat", Session.get_model s
      | Session.Unsat -> "unsat", None
      | Session.Unknown -> "unknown", None)
;;

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/bv-goldens" in
  let files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
  in
  print_endline "bv-goldens self-test (real dispatch):";
  List.iter
    (fun f ->
       let path = Filename.concat dir f in
       let src = read_file path in
       let expected = status_of src in
       let got, model = solve src in
       let ok = String.equal got expected in
       (* a sat must also surface a (self-checked) model *)
       let model_ok =
         (not (String.equal got "sat"))
         ||
         match model with
         | Some (_, (_ :: _ as _binds)) -> true
         | _ -> false
       in
       if not ok
       then (
         incr failures;
         Printf.printf "  FAIL %-40s expected %s, got %s\n" f expected got)
       else if not model_ok
       then (
         incr failures;
         Printf.printf "  FAIL %-40s sat but no surfaced model\n" f)
       else Printf.printf "  ok   %-40s %s\n" f got)
    files;
  Printf.printf
    "\nbv-goldens self-test: %d file(s), %d failure(s)\n"
    (List.length files)
    !failures;
  if !failures > 0 then exit 1
;;
