(* Test-only performance runner for Vox2 VC files. Unlike [vc_corpus_test], this invokes
   [Session.check_sat_assuming] exactly once per input and performs no independent core
   minimality probes. *)

module Session = Oxsmt_interface.Session
module Sexp = Oxsmt_smtlib_parser.Sexp
module Parser = Oxsmt_smtlib_parser.Parser
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort

type selector_name =
  { internal : string }

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let split_assumptions (sexps : Sexp.t list) : selector_name list * Sexp.t list =
  let assumptions = ref None in
  let kept =
    List.filter
      (fun sx ->
        match sx with
        | Sexp.List (head :: rest) ->
          (match Sexp.simple head with
           | Some "check-sat-assuming" ->
             if !assumptions = None
             then (
               let names =
                 match rest with
                 | [ Sexp.List sels ] ->
                   List.filter_map
                     (fun sel ->
                       Option.map
                         (fun name -> { internal = name })
                         (Sexp.symbol_name sel))
                     sels
                 | _ -> []
               in
               assumptions := Some names);
             false
           | _ -> true)
        | _ -> true)
      sexps
  in
  Option.value ~default:[] !assumptions, kept
;;

let named_assertion = function
  | Sexp.List
      [ head; Sexp.List (Sexp.Atom (Oxsmt_lexical.Lexer.Reserved "!") :: body :: attrs) ]
    when Sexp.simple head = Some "assert" ->
    let rec named = function
      | Sexp.Atom (Oxsmt_lexical.Lexer.Keyword "named") :: value :: _ ->
        Option.map (fun _name -> body) (Sexp.symbol_name value)
      | _ :: rest -> named rest
      | [] -> None
    in
    named attrs
  | _ -> None
;;

let rec record_symbol_names names = function
  | Sexp.Atom _ as atom ->
    (match Sexp.symbol_name atom with
     | Some name -> Hashtbl.replace names name ()
     | None -> ())
  | Sexp.List xs -> List.iter (record_symbol_names names) xs
;;

let symbol text =
  Sexp.Atom (Oxsmt_lexical.Lexer.Symbol { text; quoted = false })
;;

let gate_named_assertions (sexps : Sexp.t list) : selector_name list * Sexp.t list =
  let used_names = Hashtbl.create 128 in
  List.iter (record_symbol_names used_names) sexps;
  let next_selector = ref 0 in
  let fresh_selector () =
    let rec loop () =
      let name = Printf.sprintf "oxsmt_vc_core_selector_%d" !next_selector in
      incr next_selector;
      if Hashtbl.mem used_names name
      then loop ()
      else (
        Hashtbl.add used_names name ();
        name)
    in
    loop ()
  in
  let names = ref [] in
  let transformed =
    List.concat_map
      (fun sx ->
        match named_assertion sx with
        | None -> [ sx ]
        | Some body ->
          let internal = fresh_selector () in
          names := { internal } :: !names;
          [ Sexp.List [ symbol "declare-const"; symbol internal; symbol "Bool" ]
          ; Sexp.List
              [ symbol "assert"
              ; Sexp.List [ symbol "=>"; symbol internal; body ]
              ]
          ])
      sexps
  in
  List.rev !names, transformed
;;

let prepare_document sexps =
  let assumptions, kept = split_assumptions sexps in
  if assumptions = [] then gate_named_assertions kept else assumptions, kept
;;

let verdict_string = function
  | Session.Sat -> "sat"
  | Session.Unsat -> "unsat"
  | Session.Unknown -> "unknown"
;;

let rec count_matches = function
  | Sexp.Atom _ -> 0
  | Sexp.List xs ->
    let here =
      match xs with
      | head :: _ when String.equal (Sexp.to_string head) "match" -> 1
      | _ -> 0
    in
    here + List.fold_left (fun total sx -> total + count_matches sx) 0 xs
;;

let run path =
  let total_wall_started = Unix.gettimeofday () in
  let total_cpu_started = Sys.time () in
  let read_wall_started = Unix.gettimeofday () in
  let src = read_file path in
  let read_wall_s = Unix.gettimeofday () -. read_wall_started in
  let parse_wall_started = Unix.gettimeofday () in
  let parse_started = Sys.time () in
  let raw_sexps = Sexp.parse_many src in
  let match_count = List.fold_left (fun total sx -> total + count_matches sx) 0 raw_sexps in
  let selector_names, sexps = prepare_document raw_sexps in
  let session = Session.create () in
  let parsed =
    Parser.parse_into_sexps
      ~internal_mint:(Session.parse_minter session)
      (Session.env session)
      (Session.context session)
      sexps
  in
  let parse_wall_s = Unix.gettimeofday () -. parse_wall_started in
  let parse_cpu_s = Sys.time () -. parse_started in
  let load_wall_started = Unix.gettimeofday () in
  let load_started = Sys.time () in
  let presolve =
    match Sys.getenv_opt "OXSMT_PERF_DIRECT" with
    | Some ("1" | "true" | "yes") -> false
    | Some _ | None -> true
  in
  if not (Oxsmt_query_loader.assert_all ~presolve session parsed)
  then failwith "query loader declined the document";
  let load_wall_s = Unix.gettimeofday () -. load_wall_started in
  let load_cpu_s = Sys.time () -. load_started in
  let assumption_setup_wall_started = Unix.gettimeofday () in
  let ctx = Session.context session in
  let assumptions =
    List.map
      (fun { internal; _ } ->
        Context.const ctx (Session.declare_const session internal Sort.bool), true)
      selector_names
  in
  let assumption_setup_wall_s =
    Unix.gettimeofday () -. assumption_setup_wall_started
  in
  let solve_wall_started = Unix.gettimeofday () in
  let { Session.verdict; unsat_core } =
    Session.check_sat_assuming session assumptions
  in
  let solve_wall_s = Unix.gettimeofday () -. solve_wall_started in
  let p = Session.assumption_profile session in
  let dt = Session.dt_ground_simplify_stats session in
  let vc = Filename.remove_extension (Filename.basename path) in
  let core_size = Option.fold ~none:0 ~some:List.length unsat_core in
  let total_wall_s = Unix.gettimeofday () -. total_wall_started in
  let total_cpu_s = Sys.time () -. total_cpu_started in
  let reason = Session.last_unknown_reason session in
  let json =
    match Sys.getenv_opt "OXSMT_PERF_JSON" with
    | Some ("1" | "true" | "yes") -> true
    | Some _ | None -> false
  in
  if json
  then
    Printf.printf
      ("{\"vc\":%S,\"verdict\":%S,\"facts\":%d,\"matches\":%d,\"core\":%d,"
       ^^ "\"total_wall_s\":%.6f,\"read_wall_s\":%.6f,\"parse_wall_s\":%.6f,"
       ^^ "\"load_wall_s\":%.6f,\"assumption_setup_wall_s\":%.6f,"
       ^^ "\"solve_wall_s\":%.6f,\"total_cpu_s\":%.6f,\"parse_cpu_s\":%.6f,"
       ^^ "\"load_cpu_s\":%.6f,\"prepare_cpu_s\":%.6f,\"initial_cpu_s\":%.6f,"
       ^^ "\"deletion_cpu_s\":%.6f,\"replay_cpu_s\":%.6f,"
       ^^ "\"initial_effort\":%d,\"deletion_effort\":%d,\"replay_effort\":%d,"
       ^^ "\"initial_decisions\":%d,\"deletion_decisions\":%d,"
       ^^ "\"replay_decisions\":%d,\"initial_conflicts\":%d,"
       ^^ "\"deletion_conflicts\":%d,\"replay_conflicts\":%d,"
       ^^ "\"deletion_probes\":%d,\"deletion_sat\":%d,\"deletion_unsat\":%d,"
       ^^ "\"deletion_unknown\":%d,\"total_probes\":%d,\"initial_core\":%d,"
       ^^ "\"final_core\":%d,\"dt_tester_folds\":%d,\"dt_selector_folds\":%d,"
       ^^ "\"reason\":%S}\n%!")
      vc
      (verdict_string verdict)
      (List.length assumptions)
      match_count
      core_size
      total_wall_s
      read_wall_s
      parse_wall_s
      load_wall_s
      assumption_setup_wall_s
      solve_wall_s
      total_cpu_s
      parse_cpu_s
      load_cpu_s
      p.prepare_cpu_s
      p.initial_cpu_s
      p.deletion_cpu_s
      p.replay_cpu_s
      p.initial_effort
      p.deletion_effort
      p.replay_effort
      p.initial_decisions
      p.deletion_decisions
      p.replay_decisions
      p.initial_conflicts
      p.deletion_conflicts
      p.replay_conflicts
      p.deletion_probes
      p.deletion_sat
      p.deletion_unsat
      p.deletion_unknown
      (Session.minimize_probes session)
      p.initial_core_size
      p.final_core_size
      dt.tester_folds
      dt.selector_folds
      reason
  else
    Printf.printf
    ("vc=%s verdict=%s assumptions=%d core=%d total_wall_s=%.6f read_wall_s=%.6f "
     ^^ "parse_wall_s=%.6f load_wall_s=%.6f assumption_setup_wall_s=%.6f "
     ^^ "solve_wall_s=%.6f total_cpu_s=%.6f "
     ^^ "parse_cpu_s=%.6f load_cpu_s=%.6f prepare_cpu_s=%.6f initial_cpu_s=%.6f "
     ^^ "deletion_cpu_s=%.6f replay_cpu_s=%.6f initial_effort=%d deletion_effort=%d "
     ^^ "replay_effort=%d deletion_probes=%d deletion_sat=%d deletion_unsat=%d "
     ^^ "deletion_unknown=%d total_probes=%d initial_core=%d final_core=%d "
     ^^ "dt_tester_folds=%d dt_selector_folds=%d reason=%s\n%!")
    vc
    (verdict_string verdict)
    (List.length assumptions)
    core_size
    total_wall_s
    read_wall_s
    parse_wall_s
    load_wall_s
    assumption_setup_wall_s
    solve_wall_s
    total_cpu_s
    parse_cpu_s
    load_cpu_s
    p.prepare_cpu_s
    p.initial_cpu_s
    p.deletion_cpu_s
    p.replay_cpu_s
    p.initial_effort
    p.deletion_effort
    p.replay_effort
    p.deletion_probes
    p.deletion_sat
    p.deletion_unsat
    p.deletion_unknown
    (Session.minimize_probes session)
    p.initial_core_size
    p.final_core_size
    dt.tester_folds
    dt.selector_folds
    (Session.last_unknown_reason session)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _ ] ->
    prerr_endline "intset_perf_runner: expected one or more .smt2 paths";
    exit 2
  | _ :: paths ->
    List.iter
      (fun path ->
        match run path with
        | () -> ()
        | exception exn ->
          Printf.eprintf
            "vc=%s error=%s\n%!"
            (Filename.remove_extension (Filename.basename path))
            (Printexc.to_string exn);
          exit 1)
      paths
  | [] -> assert false
;;
