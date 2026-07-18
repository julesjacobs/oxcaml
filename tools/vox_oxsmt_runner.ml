(* A strict, ground-only SMT-LIB2 runner for the Vox oxsmt backend.  The
   oxsmt parser can represent quantified input, but Vox instantiates every
   quantifier manually.  Never submit parser lemmas or clauses to Session. *)

module Parser = Oxsmt_smtlib_parser.Parser
module Session = Oxsmt_interface.Session

exception Rejected_input of string

let unsupported_input_exit_code = 3

let read_all channel =
  let buffer = Buffer.create 4096 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match input channel bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
      Buffer.add_subbytes buffer bytes 0 count;
      loop ()
  in
  loop ()
;;

let read_source () =
  match Array.length Sys.argv with
  | 1 -> read_all stdin
  | 2 ->
    let channel = open_in_bin Sys.argv.(1) in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> read_all channel)
  | _ ->
    invalid_arg
      "usage: vox_oxsmt_runner [FILE] (read SMT-LIB2 from FILE or stdin)"
;;

let reject_non_ground (parsed : Parser.t) =
  let dropped = parsed.Parser.dropped in
  let lemmas = List.length parsed.Parser.lemmas in
  let existentials = List.length parsed.Parser.existentials in
  let clauses = List.length parsed.Parser.clauses in
  if dropped <> 0 || lemmas <> 0 || existentials <> 0 || clauses <> 0 then
    raise
      (Rejected_input
         (Printf.sprintf
            "rejected non-ground or dropped input \
             (dropped=%d, lemmas=%d, existentials=%d, clauses=%d)"
            dropped
            lemmas
            existentials
            clauses))
;;

let solve source =
  let session = Session.create () in
  let parsed =
    Parser.parse_into
      ~internal_mint:(Session.parse_minter session)
      (Session.env session)
      (Session.context session)
      source
  in
  reject_non_ground parsed;
  Session.set_datatypes session parsed.Parser.datatypes;
  Session.set_arrays session parsed.Parser.arrays;
  Session.assert_presolved session parsed.Parser.assertions;
  let verdict = Session.check_sat session in
  (match verdict with
   | Session.Unknown ->
     let reason = Session.last_unknown_reason session in
     if reason <> ""
     then
       Printf.eprintf
         "vox_oxsmt_runner: solver returned unknown (%s)\n%!"
         reason
   | Session.Sat | Session.Unsat -> ());
  verdict
;;

let report_exception exception_ =
  let detail =
    match exception_ with
    | Rejected_input message -> message
    | Parser.Malformed message -> "malformed SMT-LIB2: " ^ message
    | Parser.Unsupported message -> "unsupported SMT-LIB2: " ^ message
    | exception_ -> Printexc.to_string exception_
  in
  Printf.eprintf "vox_oxsmt_runner: %s\n%!" detail
;;

let () =
  match solve (read_source ()) with
  | verdict ->
    print_endline
      (match verdict with
       | Session.Sat -> "sat"
       | Session.Unsat -> "unsat"
       | Session.Unknown -> "unknown")
  | exception (Parser.Unsupported _ as exception_) ->
    report_exception exception_;
    exit unsupported_input_exit_code
  | exception exception_ ->
    report_exception exception_;
    exit 2
;;
