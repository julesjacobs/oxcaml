(* A strict, ground-only SMT-LIB2 runner for the Vox oxsmt backend.  The
   oxsmt parser can represent quantified input, but Vox instantiates every
   quantifier manually.  Never submit parser lemmas or clauses to Session. *)

module Parser = Oxsmt_smtlib_parser.Parser
module Sexp = Oxsmt_smtlib_parser.Sexp
module Session = Oxsmt_interface.Session
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Term = Oxsmt_core.Term

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

let assertion_name = function
  | Sexp.List
      [ command;
        Sexp.List
          [ bang;
            _body;
            Sexp.Atom (Oxsmt_lexical.Lexer.Keyword "named");
            name;
          ];
      ]
    when Sexp.simple command = Some "assert"
         && String.equal (Sexp.to_string bang) "!" ->
    Some (Sexp.symbol_name name)
  | Sexp.List (command :: _) when Sexp.simple command = Some "assert" ->
    Some None
  | _ -> None
;;

let wants_unsat_core sexps =
  List.exists
    (function
      | Sexp.List [ command ] ->
        Sexp.simple command = Some "get-unsat-core"
      | _ -> false)
    sexps
;;

let named_core session assertions names =
  if List.length assertions <> List.length names
  then raise (Rejected_input "assertion/name count mismatch");
  Session.preselect_arithmetic session assertions;
  let ctx = Session.context session in
  let named_assumptions = ref [] in
  List.iter2
    (fun assertion name ->
      match name with
      | None -> Session.assert_term session assertion
      | Some name ->
        let symbol = Session.declare_const session name Sort.bool in
        let selector = Context.const ctx symbol in
        Session.assert_term session (Context.implies ctx selector assertion);
        named_assumptions := (name, selector) :: !named_assumptions)
    assertions
    names;
  let named_assumptions = List.rev !named_assumptions in
  let assumptions =
    List.map (fun (_, selector) -> selector, true) named_assumptions
  in
  let result = Session.check_sat_assuming session assumptions in
  let core_names =
    match result.Session.verdict, result.Session.unsat_core with
    | Session.Unsat, Some core ->
      Some
        (List.map
           (fun (term, polarity) ->
             if not polarity
             then raise (Rejected_input "negative core selector");
             match
               List.find_opt
                 (fun (_, selector) -> Term.equal term selector)
                 named_assumptions
             with
             | Some (name, _) -> name
             | None -> raise (Rejected_input "unknown core selector"))
           core)
    | Session.Unsat, None ->
      raise (Rejected_input "oxsmt returned unsat without an assumption core")
    | (Session.Sat | Session.Unknown), _ -> None
  in
  result.Session.verdict, core_names
;;

let solve source =
  let session = Session.create () in
  let sexps = Sexp.parse_many source in
  let parsed =
    Parser.parse_into_sexps
      ~internal_mint:(Session.parse_minter session)
      (Session.env session)
      (Session.context session)
      sexps
  in
  reject_non_ground parsed;
  Session.set_datatypes session parsed.Parser.datatypes;
  Session.set_arrays session parsed.Parser.arrays;
  let verdict, core =
    if wants_unsat_core sexps
    then
      let names = List.filter_map assertion_name sexps in
      named_core session parsed.Parser.assertions names
    else (
      Session.assert_presolved session parsed.Parser.assertions;
      Session.check_sat session, None)
  in
  (match verdict with
   | Session.Unknown ->
     let reason = Session.last_unknown_reason session in
     if reason <> ""
     then
       Printf.eprintf
         "vox_oxsmt_runner: solver returned unknown (%s)\n%!"
         reason
   | Session.Sat | Session.Unsat -> ());
  verdict, core
;;

let report_exception exception_ =
  let detail =
    match exception_ with
    | Rejected_input message -> message
    | Sexp.Malformed message -> "malformed SMT-LIB2: " ^ message
    | Parser.Malformed message -> "malformed SMT-LIB2: " ^ message
    | Parser.Unsupported message -> "unsupported SMT-LIB2: " ^ message
    | exception_ -> Printexc.to_string exception_
  in
  Printf.eprintf "vox_oxsmt_runner: %s\n%!" detail
;;

let () =
  match solve (read_source ()) with
  | verdict, core ->
    print_endline
      (match verdict with
       | Session.Sat -> "sat"
       | Session.Unsat -> "unsat"
       | Session.Unknown -> "unknown");
    Option.iter
      (fun names -> Printf.printf "(%s)\n" (String.concat " " names))
      core
  | exception (Parser.Unsupported _ as exception_) ->
    report_exception exception_;
    exit unsupported_input_exit_code
  | exception exception_ ->
    report_exception exception_;
    exit 2
;;
