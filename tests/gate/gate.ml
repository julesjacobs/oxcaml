(* The gate driver: certify .smt2 files against the Lean oracle, with the
   content-addressed cache and day-one honeypots (DESIGN.md §8, §10).

   Subcommands: gate selftest sha256 + sexp self-checks (no Lean) gate certify FILE.smt2
   certify one file, print its outcome gate run [opts] honeypots first, then the case
   corpus

   Digest to stdout; full per-query detail (.lean/.out) and a summary land under
   ../logs/gate-<timestamp>/ (AGENTS.md digest-first rule). *)

type ctx = {
  logdir : string;
  lean_version : string;
  timeout : float;
  mutable counter : int; (* uniquifies per-query log file names *)
  log : Buffer.t;
}

let logf ctx fmt =
  Printf.ksprintf (fun s -> Buffer.add_string ctx.log (s ^ "\n")) fmt

let uniq ctx tag =
  ctx.counter <- ctx.counter + 1;
  Printf.sprintf "%03d-%s" ctx.counter tag

(* ---- Lean attempts ---- *)

type attempt =
  [ `Proved of string (* tactic that succeeded *)
  | `Gaveup of string
  | `Timeout
  | `Encode of string ]

let try_unsat ctx (q : Ast.query) : attempt =
  match Encoder.encode_unsat q with
  | exception Encoder.Encode_error m -> `Encode m
  | src -> (
      match
        Lean_runner.run ~logdir:ctx.logdir ~timeout:ctx.timeout
          ~tag:(uniq ctx "unsat") ~src
      with
      | Proved -> `Proved Encoder.grind_tactic
      | Timed_out -> `Timeout
      | Tactic_failed s -> `Gaveup s
      | Elab_error s -> `Encode s)

let try_sat ctx (q : Ast.query) (m : Model.t) : attempt =
  let attempt tactic =
    match Encoder.encode_sat q m ~tactic with
    | exception Encoder.Encode_error msg -> `Encode msg
    | exception Model.Bad_model msg -> `Encode ("bad model: " ^ msg)
    | src -> (
        match
          Lean_runner.run ~logdir:ctx.logdir ~timeout:ctx.timeout
            ~tag:(uniq ctx ("sat-" ^ tactic))
            ~src
        with
        | Proved -> `Proved tactic
        | Timed_out -> `Timeout
        | Tactic_failed s -> `Gaveup s
        | Elab_error s -> `Encode s)
  in
  match attempt "decide" with
  | `Proved t -> `Proved t
  | `Gaveup _ ->
      attempt "native_decide" (* NOTES.md: decide can blow up; fall back *)
  | other -> other

(* ---- certification per claim ---- *)

let refute_with_witness ctx q model_opt ~default : Outcome.t =
  match model_opt with
  | Some m -> (
      match try_sat ctx q m with
      | `Proved tac ->
          Outcome.Refuted
            (Printf.sprintf
               "assertions satisfiable, witness kernel-checked by %s" tac)
      | _ -> default)
  | None -> default

let certify_unsat ctx q model_opt : Outcome.t =
  match try_unsat ctx q with
  | `Proved _ -> Certified
  | `Encode m -> Encode_error m
  | `Timeout ->
      refute_with_witness ctx q model_opt
        ~default:(Inconclusive "grind timed out")
  | `Gaveup s ->
      refute_with_witness ctx q model_opt
        ~default:(Inconclusive ("grind gave up: " ^ s))

let certify_sat ctx q model_opt : Outcome.t =
  match model_opt with
  | None -> Inconclusive "sat claim but no .model sidecar supplied"
  | Some m -> (
      match try_sat ctx q m with
      | `Proved _ -> Certified
      | `Encode s -> Encode_error s
      | `Timeout -> Inconclusive "model check (decide) timed out"
      | `Gaveup s -> (
          (* the supplied model did not check; is the query actually unsat? *)
          match try_unsat ctx q with
          | `Proved _ ->
              Refuted "sat claim but assertions proved unsat by grind"
          | _ -> Inconclusive ("model did not check: " ^ s)))

(* ---- file wrapper + cache ---- *)

let model_path smt2 = Filename.remove_extension smt2 ^ ".model"

let load_model smt2 : Model.t option * string =
  let p = model_path smt2 in
  if Sys.file_exists p then
    let text = Lean_runner.read_file p in
    (Some (Model.of_string text), text)
  else (None, "")

(* A timeout-driven Inconclusive must not be cached (DESIGN.md §8). *)
let should_cache = function
  | Outcome.Inconclusive d ->
      not (Lean_runner.substring_mem ~needle:"timed out" d)
  | _ -> true

type disposition = [ `Hit | `Miss | `Uncached ]

let certify_file ctx ~cache_dir ~allow_cache smt2 : Outcome.t * disposition =
  match Lean_runner.read_file smt2 with
  | exception e -> (Encode_error (Printexc.to_string e), `Uncached)
  | text -> (
      match Reader.of_string text with
      | exception Reader.Malformed m -> (Malformed m, `Uncached)
      | exception Reader.Unsupported m -> (Unsupported m, `Uncached)
      | exception Sexp.Malformed m -> (Malformed ("sexp: " ^ m), `Uncached)
      | q -> (
          match q.status with
          | None -> (No_status, `Uncached)
          | Some Ast.Unknown -> (No_status, `Uncached)
          | Some claim -> (
              match load_model smt2 with
              | exception Model.Bad_model m ->
                  (Encode_error ("bad model: " ^ m), `Uncached)
              | model_opt, model_str -> (
                  let claim_str = Ast.verdict_to_string claim in
                  let key =
                    Cache.compose
                      ~canonical:(Canonical.canonical_query q)
                      ~claim:claim_str ~model:model_str
                      ~lean_version:ctx.lean_version
                  in
                  let cached =
                    if allow_cache then Cache.lookup ~dir:cache_dir key
                    else None
                  in
                  match cached with
                  | Some o -> (o, `Hit)
                  | None ->
                      let o =
                        match claim with
                        | Ast.Unsat -> certify_unsat ctx q model_opt
                        | Ast.Sat -> certify_sat ctx q model_opt
                        | Ast.Unknown -> No_status
                      in
                      if allow_cache && should_cache o then
                        Cache.store ~dir:cache_dir key ~claim:claim_str o;
                      (o, if allow_cache then `Miss else `Uncached)))))

(* ---- corpus listing ---- *)

let smt2_files dir =
  if not (Sys.file_exists dir && Sys.is_directory dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
    |> List.map (Filename.concat dir)

(* ---- reporting ---- *)

let short p = Filename.basename p

let print_line ctx label file (o : Outcome.t) disp =
  let d =
    match disp with `Hit -> "cache" | `Miss -> "lean" | `Uncached -> "lean"
  in
  let det = Outcome.detail o in
  let det =
    if String.length det > 80 then String.sub det 0 77 ^ "..." else det
  in
  logf ctx "  [%s] %-13s %-28s (%s) %s" label (Outcome.tag o) (short file) d det

(* ---- run subcommand ---- *)

let run_dir_now logs_root =
  let t = Unix.localtime (Unix.time ()) in
  let stamp =
    Printf.sprintf "%04d%02d%02d-%02d%02d%02d" (t.tm_year + 1900) (t.tm_mon + 1)
      t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in
  Filename.concat logs_root ("gate-" ^ stamp)

let honeypots_phase ctx ~cache_dir dir : bool * (string * Outcome.t) list =
  (* Honeypots run with the cache DISABLED and are never cached (DESIGN.md §10). Any
     honeypot that gets CERTIFIED means the gate is broken -> red. *)
  let files = smt2_files dir in
  logf ctx "HONEYPOTS (%d) — must NOT certify:" (List.length files);
  let results =
    List.map
      (fun f ->
        let o, disp = certify_file ctx ~cache_dir ~allow_cache:false f in
        print_line ctx "honeypot" f o disp;
        (f, o))
      files
  in
  let breached = List.filter (fun (_, o) -> Outcome.is_certified o) results in
  (match breached with
  | [] -> logf ctx "  -> all honeypots correctly refused certification"
  | bs ->
      logf ctx "  -> BREACH: %d honeypot(s) were CERTIFIED (gate is broken):"
        (List.length bs);
      List.iter (fun (f, _) -> logf ctx "       %s" (short f)) bs);
  (breached = [], results)

let cases_phase ctx ~cache_dir dir =
  let files = smt2_files dir in
  logf ctx "CASES (%d):" (List.length files);
  let tally = Hashtbl.create 8 in
  let bump t =
    Hashtbl.replace tally t
      (1 + Option.value (Hashtbl.find_opt tally t) ~default:0)
  in
  let hits = ref 0 and misses = ref 0 in
  let ship_stoppers = ref [] and encode_errors = ref [] in
  List.iter
    (fun f ->
      let o, disp = certify_file ctx ~cache_dir ~allow_cache:true f in
      (match disp with
      | `Hit -> incr hits
      | `Miss -> incr misses
      | `Uncached -> ());
      bump (Outcome.tag o);
      if Outcome.is_ship_stopper o then
        ship_stoppers := (f, o) :: !ship_stoppers;
      (match o with
      | Encode_error _ -> encode_errors := (f, o) :: !encode_errors
      | _ -> ());
      print_line ctx "case" f o disp)
    files;
  (tally, !hits, !misses, List.rev !ship_stoppers, List.rev !encode_errors)

let cmd_run ~cases_dir ~honeypots_dir ~cache_dir ~logs_root ~timeout
    ~allow_cache =
  let logdir = run_dir_now logs_root in
  Cache.mkdir_p logdir;
  let lean_version = Lean_runner.version () in
  let ctx =
    { logdir; lean_version; timeout; counter = 0; log = Buffer.create 4096 }
  in
  logf ctx "oxsmt gate run";
  logf ctx "  lean: %s" lean_version;
  logf ctx "  encoding: %s   grind: %s" Encoder.encoding_version
    Encoder.grind_config;
  logf ctx "  cache: %s (%s)" cache_dir
    (if allow_cache then "enabled" else "disabled");
  logf ctx "  logdir: %s" logdir;
  logf ctx "";
  let honeypots_ok, _hres = honeypots_phase ctx ~cache_dir honeypots_dir in
  logf ctx "";
  (* If a honeypot certified, abort red BEFORE trusting any case result. *)
  let tally, hits, misses, ship_stoppers, encode_errors =
    if honeypots_ok then cases_phase ctx ~cache_dir cases_dir
    else (Hashtbl.create 1, 0, 0, [], [])
  in
  logf ctx "";
  (* Write full log. *)
  let logfile = Filename.concat logdir "gate.log" in
  Lean_runner.write_file logfile (Buffer.contents ctx.log);
  (* Digest to stdout. *)
  let get t = Option.value (Hashtbl.find_opt tally t) ~default:0 in
  let green = honeypots_ok && ship_stoppers = [] && encode_errors = [] in
  Printf.printf "oxsmt gate: %s\n" (if green then "GREEN" else "RED");
  if not honeypots_ok then
    Printf.printf
      "  HONEYPOT BREACH — a known-wrong verdict was CERTIFIED (see log)\n";
  Printf.printf
    "  cases: %d CERTIFIED, %d REFUTED, %d INCONCLUSIVE, %d ENCODE_ERROR, %d \
     UNSUPPORTED, %d MALFORMED, %d NO_STATUS\n"
    (get "CERTIFIED") (get "REFUTED") (get "INCONCLUSIVE") (get "ENCODE_ERROR")
    (get "UNSUPPORTED") (get "MALFORMED") (get "NO_STATUS");
  Printf.printf "  cache: %d hit, %d miss\n" hits misses;
  if ship_stoppers <> [] then (
    Printf.printf "  SHIP-STOPPERS (REFUTED):\n";
    List.iter
      (fun (f, o) -> Printf.printf "    %s: %s\n" (short f) (Outcome.detail o))
      ship_stoppers);
  if encode_errors <> [] then (
    Printf.printf "  ENCODE ERRORS:\n";
    List.iter
      (fun (f, o) -> Printf.printf "    %s: %s\n" (short f) (Outcome.detail o))
      encode_errors);
  Printf.printf "  full log: %s\n" logfile;
  if green then 0 else 1

(* ---- certify subcommand (single file, verbose) ---- *)

let cmd_certify ~cache_dir ~logs_root ~timeout ~allow_cache file =
  let logdir = run_dir_now logs_root in
  Cache.mkdir_p logdir;
  let lean_version = Lean_runner.version () in
  let ctx =
    { logdir; lean_version; timeout; counter = 0; log = Buffer.create 1024 }
  in
  let o, disp = certify_file ctx ~cache_dir ~allow_cache file in
  Lean_runner.write_file
    (Filename.concat logdir "gate.log")
    (Buffer.contents ctx.log);
  let d =
    match disp with
    | `Hit -> "cache hit"
    | `Miss -> "lean (cached)"
    | `Uncached -> "lean"
  in
  Printf.printf "%s: %s [%s]\n" (short file) (Outcome.tag o) d;
  if Outcome.detail o <> "" then Printf.printf "  %s\n" (Outcome.detail o);
  Printf.printf "  logdir: %s\n" logdir;
  Outcome.exit_code o

(* ---- selftest ---- *)

let cmd_selftest () =
  let ok = ref true in
  (match Sha256.selftest () with
  | Ok () -> print_endline "sha256: OK (FIPS 180-4 vectors)"
  | Error fs ->
      ok := false;
      print_endline "sha256: FAIL";
      List.iter (fun f -> print_endline ("  " ^ f)) fs);
  (* sexp round-trip *)
  let sample = "(a (b c) |quoted sym| 42) ; comment\n(assert (= x 1))" in
  (match Sexp.parse_many sample with
  | exception e ->
      ok := false;
      Printf.printf "sexp: FAIL (%s)\n" (Printexc.to_string e)
  | _ -> print_endline "sexp: OK");
  (* reader smoke + encoder smoke (no Lean) *)
  (match
     Reader.of_string
       "(set-logic QF_UFLIA)(declare-sort S 0)(declare-fun f (S) \
        S)(declare-const a S)(declare-const b S)(assert (= a b))(assert \
        (distinct (f a) (f b)))(set-info :status unsat)(check-sat)"
   with
  | exception e ->
      ok := false;
      Printf.printf "reader: FAIL (%s)\n" (Printexc.to_string e)
  | q -> (
      match Encoder.encode_unsat q with
      | exception e ->
          ok := false;
          Printf.printf "encoder: FAIL (%s)\n" (Printexc.to_string e)
      | src ->
          if
            Lean_runner.substring_mem ~needle:"by grind" src
            && Lean_runner.substring_mem ~needle:"open Classical" src
          then print_endline "reader+encoder: OK"
          else (
            ok := false;
            print_endline "encoder: FAIL (unexpected output)")));
  if !ok then 0 else 1

(* ---- argument parsing ---- *)

let usage () =
  prerr_endline
    "usage:\n\
    \  gate selftest\n\
    \  gate certify FILE.smt2 [--no-cache] [--timeout SECS]\n\
    \  gate run [--cases DIR] [--honeypots DIR] [--cache DIR] [--logs DIR] \
     [--no-cache] [--timeout SECS]";
  2

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let cache_dir = ref (Cache.default_dir ()) in
  let logs_root =
    ref
      (match Sys.getenv_opt "OXSMT_LOGS" with
      | Some d -> d
      | None -> "/usr/local/home/jujacobs/oxsmt/logs")
  in
  let cases_dir = ref "tests/cases" in
  let honeypots_dir = ref "tests/gate/honeypots" in
  let timeout = ref 30.0 in
  let allow_cache = ref true in
  let rec parse_opts = function
    | [] -> ()
    | "--no-cache" :: tl ->
        allow_cache := false;
        parse_opts tl
    | "--cache" :: d :: tl ->
        cache_dir := d;
        parse_opts tl
    | "--logs" :: d :: tl ->
        logs_root := d;
        parse_opts tl
    | "--cases" :: d :: tl ->
        cases_dir := d;
        parse_opts tl
    | "--honeypots" :: d :: tl ->
        honeypots_dir := d;
        parse_opts tl
    | "--timeout" :: s :: tl ->
        timeout := float_of_string s;
        parse_opts tl
    | other :: _ ->
        Printf.eprintf "unknown option: %s\n" other;
        exit (usage ())
  in
  let code =
    match args with
    | "selftest" :: _ -> cmd_selftest ()
    | "certify" :: file :: rest ->
        parse_opts rest;
        cmd_certify ~cache_dir:!cache_dir ~logs_root:!logs_root
          ~timeout:!timeout ~allow_cache:!allow_cache file
    | "run" :: rest ->
        parse_opts rest;
        cmd_run ~cases_dir:!cases_dir ~honeypots_dir:!honeypots_dir
          ~cache_dir:!cache_dir ~logs_root:!logs_root ~timeout:!timeout
          ~allow_cache:!allow_cache
    | _ -> usage ()
  in
  exit code
