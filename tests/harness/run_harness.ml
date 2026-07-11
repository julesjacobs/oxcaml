(* The .smt2 golden/expect regression runner (DESIGN.md §8, §11).

   Usage: run_harness --solver PATH [--dir DIR]... [--logs DIR] [--stats DIR] [--promote]
   [--max-failures N]

   Scans each --dir for *.smt2 files (sorted, deterministic), runs the solver on each, and
   compares produced golden text against the committed sidecar FILE.smt2.expected. Full
   detail (produced output, diffs, solver stderr) is written under --logs; only a digest
   goes to stdout (context-frugal, §11). Exact counters and wall-clock go to an
   uncommitted JSONL stats sidecar under --stats (never committed — I5). Exits non-zero on
   any failure (or, without --promote, on any missing golden). *)

open Harness_lib

let default_logs = "../logs"
let default_max_failures = 10

type config =
  { solver : string
  ; dirs : string list
  ; logs : string
  ; stats : string
  ; promote : bool
  ; max_failures : int
  }

let usage () =
  prerr_endline
    "usage: run_harness --solver PATH [--dir DIR]... [--logs DIR] [--stats DIR] \
     [--promote] [--max-failures N]";
  exit 2
;;

let parse_args argv =
  let solver = ref "" in
  let dirs = ref [] in
  let logs = ref default_logs in
  let stats = ref "" in
  let promote = ref false in
  let max_failures = ref default_max_failures in
  let rec go = function
    | "--solver" :: v :: r ->
      solver := v;
      go r
    | "--dir" :: v :: r ->
      dirs := v :: !dirs;
      go r
    | "--logs" :: v :: r ->
      logs := v;
      go r
    | "--stats" :: v :: r ->
      stats := v;
      go r
    | "--promote" :: r ->
      promote := true;
      go r
    | "--max-failures" :: v :: r ->
      (match int_of_string_opt v with
       | Some n -> max_failures := n
       | None -> usage ());
      go r
    | [] -> ()
    | _ -> usage ()
  in
  go (List.tl (Array.to_list argv));
  if String.equal !solver "" then usage ();
  { solver = !solver
  ; dirs = List.rev !dirs
  ; logs = !logs
  ; stats = (if String.equal !stats "" then Filename.concat !logs "stats" else !stats)
  ; promote = !promote
  ; max_failures = !max_failures
  }
;;

(* --- filesystem helpers --- *)

let rec mkdir_p dir =
  if not (Sys.file_exists dir)
  then (
    let parent = Filename.dirname dir in
    if not (String.equal parent dir) then mkdir_p parent;
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
;;

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc
;;

let smt2_files_in dir =
  if not (Sys.file_exists dir)
  then []
  else
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".smt2")
    |> List.sort String.compare
    |> List.map (fun f -> Filename.concat dir f)
;;

let golden_path smt2 = smt2 ^ ".expected"

(* --- one-line diffstat for the promote summary (multiset line delta) --- *)

let lines s = if String.equal s "" then [] else String.split_on_char '\n' s

let multiset_delta ~old_text ~new_text =
  let module SM = Map.Make (String) in
  let counts ls =
    List.fold_left
      (fun m l ->
         SM.update
           l
           (function
             | None -> Some 1
             | Some n -> Some (n + 1))
           m)
      SM.empty
      ls
  in
  let o = counts (lines old_text)
  and n = counts (lines new_text) in
  let added = ref 0
  and removed = ref 0 in
  SM.iter
    (fun l cn ->
       let co =
         match SM.find_opt l o with
         | Some x -> x
         | None -> 0
       in
       if cn > co then added := !added + (cn - co))
    n;
  SM.iter
    (fun l co ->
       let cn =
         match SM.find_opt l n with
         | Some x -> x
         | None -> 0
       in
       if co > cn then removed := !removed + (co - cn))
    o;
  !added, !removed
;;

(* --- stats sidecar (JSONL; uncommitted) --- *)

let json_escape s =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
       match c with
       | '"' -> Buffer.add_string b "\\\""
       | '\\' -> Buffer.add_string b "\\\\"
       | '\n' -> Buffer.add_string b "\\n"
       | c -> Buffer.add_char b c)
    s;
  Buffer.contents b
;;

let write_stats cfg ~run_id (rows : (Harness.file_eval * float) list) =
  mkdir_p cfg.stats;
  let path = Filename.concat cfg.stats (Printf.sprintf "run-%s.jsonl" run_id) in
  let oc = open_out_bin path in
  List.iter
    (fun (fe, wall_ms) ->
       List.iteri
         (fun i (g : Harness.goal_result) ->
            Printf.fprintf
              oc
              "{\"file\":\"%s\",\"goal\":%d,\"verdict\":\"%s\",\"conflicts\":%d,\"decisions\":%d,\"propagations\":%d,\"wall_ms\":%.3f}\n"
              (json_escape fe.Harness.path)
              (i + 1)
              (Harness.verdict_to_string g.Harness.verdict)
              g.Harness.counters.conflicts
              g.Harness.counters.decisions
              g.Harness.counters.propagations
              wall_ms)
         fe.Harness.output)
    rows;
  close_out oc;
  path
;;

(* --- digest --- *)

let category = function
  | Harness.Pass -> "pass"
  | Fail_missing_golden -> "missing golden"
  | Fail_golden_mismatch -> "golden mismatch"
  | Fail_label_mismatch _ -> "label mismatch (soundness)"
  | Fail_error _ -> "solver error"
;;

let outcome_detail = function
  | Harness.Fail_label_mismatch m | Fail_error m -> Some m
  | _ -> None
;;

let () =
  let cfg = parse_args Sys.argv in
  let run_id = Printf.sprintf "%d-%d" (int_of_float (Unix.time ())) (Unix.getpid ()) in
  let log_dir = Filename.concat (Filename.concat cfg.logs "harness") run_id in
  mkdir_p log_dir;
  (* Collect (dir, file) pairs, de-duplicated, deterministic order. *)
  let files =
    List.concat_map (fun d -> List.map (fun f -> d, f) (smt2_files_in d)) cfg.dirs
  in
  let per_dir_counts = List.map (fun d -> d, List.length (smt2_files_in d)) cfg.dirs in
  (* Evaluate every file. *)
  let results =
    List.map
      (fun (_dir, path) ->
         let sexps =
           match Sexp.parse_all (read_file path) with
           | s -> Ok s
           | exception Sexp.Parse_error m -> Error m
         in
         match sexps with
         | Error m ->
           let fe =
             { Harness.path
             ; produced = ""
             ; golden = None
             ; outcome = Fail_error ("smt2 parse error: " ^ m)
             ; output = []
             }
           in
           fe, 0.0
         | Ok sexps ->
           let expected_statuses = Harness.expected_statuses sexps in
           let golden =
             let gp = golden_path path in
             if Sys.file_exists gp then Some (read_file gp) else None
           in
           let t0 = Unix.gettimeofday () in
           let solver_result, errs = Harness.run_solver cfg.solver path in
           let wall_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
           let fe = Harness.evaluate ~path ~expected_statuses ~golden ~solver_result in
           if not (String.equal errs "")
           then
             write_file
               (Filename.concat log_dir (Filename.basename path ^ ".solver-stderr"))
               errs;
           fe, wall_ms)
      files
  in
  let stats_path = write_stats cfg ~run_id results in
  (* Promote, if requested. *)
  let promote_summary = ref [] in
  if cfg.promote
  then
    List.iter
      (fun (fe, _) ->
         if Harness.promotable fe.Harness.outcome
         then (
           let old_text =
             match fe.Harness.golden with
             | Some g -> g
             | None -> ""
           in
           let added, removed = multiset_delta ~old_text ~new_text:fe.Harness.produced in
           write_file (golden_path fe.Harness.path) fe.Harness.produced;
           promote_summary := (fe.Harness.path, added, removed) :: !promote_summary))
      results;
  let promote_summary = List.rev !promote_summary in
  (* Recompute outcomes after promotion: a promoted file now passes; anything
     non-promotable (label mismatch, solver error) still fails. *)
  let effective_outcome (fe : Harness.file_eval) =
    if cfg.promote && Harness.promotable fe.outcome then Harness.Pass else fe.outcome
  in
  let fails =
    List.filter (fun (fe, _) -> Harness.is_fail (effective_outcome fe)) results
  in
  (* Write per-failure detail to the log dir. *)
  List.iter
    (fun (fe, _) ->
       let base = Filename.basename fe.Harness.path in
       let detail =
         Printf.sprintf
           "path: %s\ncategory: %s\n%s\n--- expected ---\n%s\n--- produced ---\n%s\n"
           fe.Harness.path
           (category (effective_outcome fe))
           (match outcome_detail fe.Harness.outcome with
            | Some m -> "detail: " ^ m
            | None -> "")
           (match fe.Harness.golden with
            | Some g -> g
            | None -> "<none>")
           fe.Harness.produced
       in
       write_file (Filename.concat log_dir (base ^ ".diff")) detail)
    fails;
  (* Digest to stdout. *)
  let total = List.length results in
  let n_fail = List.length fails in
  let n_pass = total - n_fail in
  let dir_desc =
    String.concat
      ", "
      (List.map (fun (d, n) -> Printf.sprintf "%s: %d" d n) per_dir_counts)
  in
  Printf.printf
    "harness: %d file(s) [%s] | PASS %d | FAIL %d%s\n"
    total
    dir_desc
    n_pass
    n_fail
    (if cfg.promote then " | promote" else "");
  let shown = ref 0 in
  List.iter
    (fun (fe, _) ->
       if !shown < cfg.max_failures
       then (
         incr shown;
         Printf.printf
           "  FAIL %s  [%s]  -> %s\n"
           fe.Harness.path
           (category (effective_outcome fe))
           (Filename.concat log_dir (Filename.basename fe.Harness.path ^ ".diff"))))
    fails;
  if n_fail > cfg.max_failures
  then Printf.printf "  ... and %d more (see %s)\n" (n_fail - cfg.max_failures) log_dir;
  if cfg.promote
  then (
    Printf.printf "promote: wrote %d golden(s)\n" (List.length promote_summary);
    List.iter
      (fun (path, added, removed) -> Printf.printf "  %s (+%d/-%d)\n" path added removed)
      promote_summary);
  Printf.printf "stats: %s\n" stats_path;
  if n_fail > 0 then exit 1 else exit 0
;;
