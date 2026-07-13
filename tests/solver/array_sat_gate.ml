(* Sat-direction gate for the arrays model-construction lane (task #14), the arrays
   analogue of {!Dt_sat_gate}. Three parts:

   1. GOLDENS — every [*_sat.smt2] under tests/arr-goldens-sat/ must be a CHECKED [Sat]:
      check_sat returns [Sat] only after {!Array_model_check} validated the array model
      against the original assertions.

   2. SOUNDNESS DISCRIMINATOR — the storeinv-shape UNSAT file the arrays theory does not
      refute (it reaches a Final "Sat"). The array checker rejects it (no single model
      satisfies the store-equality and the array disequality together) => [Unknown], never
      a wrong sat. A bypass of the checker would report [Sat] here — so this being
      [unknown] is exactly the checker earning its keep.

   These two run through a SUBPROCESS of the shipped CLI (one Session per OS process, the
   production path): the arrays theory mints reserved symbols into the process-global
   intern table, so multiple array Sessions in one process interfere — a known limitation
   of in-process multi-session use, irrelevant to the one-query-per-process product/corpus
   path.

   3. WIRING (fault injection) — a SINGLE in-process Session, re-checked with the
   {!Session.For_test.set_array_checker} override flipped between calls: a reject-all stub
   forces [Unknown] on a genuinely-sat query (a commit that bypassed the checker would
   report [Sat] — RED), the stub is observed invoked, and restoring the real checker gives
   [Sat]. One Session, so no cross-session interference. *)

module Session = Oxsmt_interface.Session
module Parser = Oxsmt_smtlib_parser.Parser

let checks = ref 0
let failures = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
       incr failures;
       print_string ("  FAIL " ^ s ^ "\n"))
    fmt
;;

let expect_bool name got want =
  incr checks;
  if Bool.equal got want then () else fail "%s: got %b, want %b" name got want
;;

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

(* CLI sibling of this gate in the same _build dir; run one file, one Session per process. *)
let cli = Filename.concat (Filename.dirname Sys.executable_name) "oxsmt_cli.exe"

(* substring occurrence test (stdlib-only; no Str) *)
let contains hay needle =
  let nh = String.length hay
  and nn = String.length needle in
  let rec at i =
    if i + nn > nh
    then false
    else if String.sub hay i nn = needle
    then true
    else at (i + 1)
  in
  nn = 0 || at 0
;;

let cli_verdict file =
  let ic =
    Unix.open_process_in
      (Printf.sprintf "%s %s 2>/dev/null" (Filename.quote cli) (Filename.quote file))
  in
  let out =
    try input_line ic with
    | End_of_file -> ""
  in
  ignore (Unix.close_process_in ic);
  if contains out "verdict sat"
  then "sat"
  else if contains out "verdict unsat"
  then "unsat"
  else "unknown"
;;

let run_goldens dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f "_sat.smt2")
  |> List.sort String.compare
  |> List.iter (fun f ->
    incr checks;
    let v = cli_verdict (Filename.concat dir f) in
    if not (String.equal v "sat") then fail "golden %s: got %s, want sat" f v)
;;

let run_soundness file =
  incr checks;
  let v = cli_verdict file in
  (* the checker must NOT certify this unsatisfiable query as sat *)
  if String.equal v "sat"
  then
    fail
      "soundness: storeinv-unsat reported SAT (WRONG-SAT — the checker failed to reject \
       an unsatisfiable array query)"
;;

(* One in-process Session, re-checked with the override flipped: proves the commit is
   GATED on the checker verdict (RED against a bypass). *)
let run_fault_injection src =
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception e ->
    fail "fault-injection: golden failed to parse: %s" (Printexc.to_string e)
  | parsed ->
    if not (Oxsmt_query_loader.assert_all s parsed)
    then fail "fault-injection: golden failed to load"
    else (
      let rejects = ref 0 in
      Session.For_test.set_array_checker
        (Some
           (fun _ _ _ ->
             incr rejects;
             false));
      let v_reject = Session.check_sat s in
      incr checks;
      (match v_reject with
       | Session.Unknown -> ()
       | Session.Sat ->
         fail
           "fault-injection: reject-all stub did NOT force unknown — commit bypasses the \
            array checker verdict"
       | Session.Unsat -> fail "fault-injection: reject-all stub produced unsat");
      expect_bool
        "fault-injection: reject-all stub was invoked by commit"
        (!rejects > 0)
        true;
      Session.For_test.set_array_checker None;
      incr checks;
      (match Session.check_sat s with
       | Session.Sat -> ()
       | _ ->
         fail "fault-injection: real checker on a genuinely-sat query did not give sat");
      Session.For_test.set_array_checker None)
;;

let () =
  let dir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/arr-goldens-sat" in
  run_goldens dir;
  run_soundness (Filename.concat dir "arr_storeinv_unsat_stays_unknown.smt2");
  run_fault_injection (read_file (Filename.concat dir "arr_select_over_store_sat.smt2"));
  Printf.printf "Array sat-gate: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
