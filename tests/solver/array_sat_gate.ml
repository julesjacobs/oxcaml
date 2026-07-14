(* Sat-direction gate for the arrays model-construction lane (task #14), the arrays
   analogue of {!Dt_sat_gate}. Three parts, all in-process through the real {!Session}:

   1. GOLDENS — every [*_sat.smt2] under tests/arr-goldens-sat/ must be a CHECKED [Sat]:
      check_sat returns [Sat] only after {!Array_model_check} validated the array model
      against the original assertions.

   2. SOUNDNESS — the storeinv-shape UNSAT file must NEVER be reported [Sat]. Since upward
      read propagation landed (ensure_store_reads in arr.ml) the arrays theory refutes
      this shape directly and answers [unsat]; before that it saturated to a Final "Sat"
      that the array checker rejected, giving a sound [unknown]. run_soundness accepts
      unsat OR unknown, never sat, so it stays valid across that behaviour change. (The
      checker-bypass discrimination — that a commit ignoring [Array_model_check] would
      wrongly report [Sat] on a genuinely-sat query — now lives entirely in part 3, since
      the theory no longer leaves this file at a Final "Sat" for the checker to catch.)

   3. WIRING (fault injection) — a Session re-checked with the
      {!Session.For_test.set_array_checker} override flipped between calls: a reject-all
      stub forces [Unknown] on a genuinely-sat query (a commit that bypassed the checker
      would report [Sat] — RED), the stub is observed invoked, and restoring the real
      checker gives [Sat]. *)

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

(* Parse (with the session's cap-backed minter, needed for the reserved array op symbols),
   load, and solve one .smt2 source in a fresh Session. *)
let solve src =
  let s = Session.create () in
  match
    Parser.parse_into
      ~internal_mint:(Session.parse_minter s)
      (Session.env s)
      (Session.context s)
      src
  with
  | exception (Parser.Malformed _ | Parser.Unsupported _) -> Session.Unknown
  | parsed ->
    if Oxsmt_query_loader.assert_all s parsed
    then Session.check_sat s
    else Session.Unknown
;;

let run_goldens dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f "_sat.smt2")
  |> List.sort String.compare
  |> List.iter (fun f ->
    incr checks;
    match solve (read_file (Filename.concat dir f)) with
    | Session.Sat -> ()
    | Session.Unsat -> fail "golden %s: got unsat, want sat" f
    | Session.Unknown -> fail "golden %s: got unknown, want sat" f)
;;

(* the checker must NOT certify this unsatisfiable query as sat *)
let run_soundness src =
  incr checks;
  match solve src with
  | Session.Sat ->
    fail
      "soundness: storeinv-unsat reported SAT (WRONG-SAT — the checker failed to reject \
       an unsatisfiable array query)"
  | Session.Unsat | Session.Unknown -> ()
;;

(* Re-check one Session with the override flipped: proves the commit is GATED on the
   checker verdict (RED against a bypass). *)
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
  run_soundness (read_file (Filename.concat dir "arr_storeinv_unsat_stays_unknown.smt2"));
  run_fault_injection (read_file (Filename.concat dir "arr_select_over_store_sat.smt2"));
  Printf.printf "Array sat-gate: %d checks, %d failures\n" !checks !failures;
  if !failures > 0 then exit 1
;;
