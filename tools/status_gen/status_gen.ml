(* STATUS.md generator (DESIGN.md §8.4, §11).

   STATUS.md is the master's empirical view of the world: outcome metrics first
   (goal-displacement defense — §11), process metrics after. This tool ONLY AGGREGATES
   existing artifacts; it runs nothing (no harness, no Lean) and re-derives no product
   state, so its output is a pure function of the inputs on disk:

   - TASKS.md -> milestone completion (parse the M-rows)
   - git -> generated-at HEAD, days-since-last-outcome-improvement, worktree/branch
     hygiene
   - the last captured harness digest -> live pass/fail (written by `make status-fresh` /
     the fast suite; this reads that file, it does not run the harness)
   - latest gate log -> gate outcome counts, honeypot floor, cache hit-rate
   - most recent stats JSONL -> counter-bucket distribution + solved-rate (buckets and
     verdicts are deterministic; per-goal wall_ms is deliberately NOT emitted — it is
     nondeterministic and stays in the uncommitted sidecar)
   - tools/line_budgets.txt + smt/ -> per-module line counts vs budget

   Byte-stable given the same (repo, logs): the only per-run-varying line is "generated at
   <HEAD>" (git HEAD, never wall-clock). This is what lets the committed STATUS.md have a
   meaningful diff (a dashboard that is always dirty trains readers to ignore its diffs —
   I5's spirit applied to every committed artifact). `make status` never writes a stats
   file; `make status-fresh` refreshes inputs first, for nightly. Every input is optional:
   a missing one degrades to "n/a", never a crash. Digest-first: a ~5-line summary to
   stdout, full document to --out. *)

(* ------------------------------------------------------------------ *)
(* Small IO / process helpers *)
(* ------------------------------------------------------------------ *)

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let read_file_opt path = if Sys.file_exists path then Some (read_file path) else None
let lines s = if String.equal s "" then [] else String.split_on_char '\n' s
let trim = String.trim

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.equal (String.sub s 0 lp) prefix
;;

let contains_sub haystack needle =
  let hn = String.length haystack
  and nn = String.length needle in
  if nn = 0
  then true
  else (
    let rec at i =
      if i + nn > hn
      then false
      else if String.equal (String.sub haystack i nn) needle
      then true
      else at (i + 1)
    in
    at 0)
;;

(* Read all of a file descriptor to a string. *)
let read_all_fd fd =
  let ic = Unix.in_channel_of_descr fd in
  let b = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  let rec loop () =
    let n = input ic chunk 0 (Bytes.length chunk) in
    if n > 0
    then (
      Buffer.add_subbytes b chunk 0 n;
      loop ())
  in
  loop ();
  Buffer.contents b
;;

(* Run [argv], capture stdout (stderr -> /dev/null). Returns (stdout, ok). *)
let run_capture argv =
  match
    let r, w = Unix.pipe () in
    let devnull = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
    let pid = Unix.create_process argv.(0) argv Unix.stdin w devnull in
    Unix.close w;
    Unix.close devnull;
    let out = read_all_fd r in
    Unix.close r;
    let _, status = Unix.waitpid [] pid in
    out, status
  with
  | out, Unix.WEXITED 0 -> out, true
  | out, _ -> out, false
  | exception _ -> "", false
;;

let git repo args =
  let argv = Array.of_list ("git" :: "-C" :: repo :: args) in
  let out, ok = run_capture argv in
  if ok then Some (trim out) else None
;;

(* Recursively count regular files under [dir] (tolerant of permission errors). *)
let count_files_rec dir =
  let n = ref 0 in
  let rec walk d =
    match Sys.readdir d with
    | entries ->
      Array.iter
        (fun e ->
           let p = Filename.concat d e in
           match Sys.is_directory p with
           | true -> walk p
           | false -> incr n
           | exception _ -> ())
        entries
    | exception _ -> ()
  in
  if Sys.file_exists dir then walk dir;
  !n
;;

let count_lines path =
  match read_file_opt path with
  | None -> 0
  | Some s ->
    let n = ref 0 in
    String.iter (fun c -> if c = '\n' then incr n) s;
    (* count a final unterminated line too *)
    if String.length s > 0 && s.[String.length s - 1] <> '\n' then incr n;
    !n
;;

let dir_entries_ml dir =
  match Sys.readdir dir with
  | a ->
    Array.to_list a
    |> List.filter (fun f ->
      Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli")
  | exception _ -> []
;;

(* Total .ml + .mli lines directly under [repo]/[path] (non-recursive, so a test/
   subdirectory is excluded — its files live one level down). *)
let count_lines_in_module repo path =
  let dir = Filename.concat repo path in
  if not (Sys.file_exists dir)
  then 0
  else
    dir_entries_ml dir
    |> List.fold_left (fun acc f -> acc + count_lines (Filename.concat dir f)) 0
;;

(* ------------------------------------------------------------------ *)
(* Log-scale buckets (mirror tests/harness bucketing, DESIGN.md §8) *)
(* ------------------------------------------------------------------ *)

let bucket v =
  if v < 10
  then "<10"
  else if v < 100
  then "<100"
  else if v < 1000
  then "<1k"
  else if v < 10000
  then "<10k"
  else ">=10k"
;;

let bucket_order = [ "<10"; "<100"; "<1k"; "<10k"; ">=10k" ]

(* ------------------------------------------------------------------ *)
(* Minimal JSONL reader for the harness stats sidecar *)
(*   one flat object per line: {"k":"str", "k":num, ...}              *)
(* ------------------------------------------------------------------ *)

let parse_json_object line : (string * string) list =
  (* Tolerant flat-object parser: keys and string values are quoted, numbers are bare.
     Good enough for the harness's own well-formed output. *)
  let n = String.length line in
  let i = ref 0 in
  let res = ref [] in
  let skip_ws () =
    while !i < n && (line.[!i] = ' ' || line.[!i] = '\t') do
      incr i
    done
  in
  let read_string () =
    (* assumes line.[!i] = '"' *)
    incr i;
    let b = Buffer.create 16 in
    let rec loop () =
      if !i >= n
      then ()
      else (
        match line.[!i] with
        | '"' -> incr i
        | '\\' when !i + 1 < n ->
          (match line.[!i + 1] with
           | 'n' -> Buffer.add_char b '\n'
           | c -> Buffer.add_char b c);
          i := !i + 2;
          loop ()
        | c ->
          Buffer.add_char b c;
          incr i;
          loop ())
    in
    loop ();
    Buffer.contents b
  in
  let read_bare () =
    let start = !i in
    while !i < n && line.[!i] <> ',' && line.[!i] <> '}' do
      incr i
    done;
    trim (String.sub line start (!i - start))
  in
  (* find opening brace *)
  while !i < n && line.[!i] <> '{' do
    incr i
  done;
  if !i < n then incr i;
  let rec loop () =
    skip_ws ();
    if !i >= n || line.[!i] = '}'
    then ()
    else if line.[!i] = '"'
    then (
      let key = read_string () in
      skip_ws ();
      if !i < n && line.[!i] = ':' then incr i;
      skip_ws ();
      let value = if !i < n && line.[!i] = '"' then read_string () else read_bare () in
      res := (key, value) :: !res;
      skip_ws ();
      if !i < n && line.[!i] = ',' then incr i;
      loop ())
    else (* unexpected; bail *)
      ()
  in
  loop ();
  List.rev !res
;;

(* Only the deterministic fields are kept; wall_ms is intentionally ignored (it is
   nondeterministic and never enters the committed STATUS.md). *)
type stat_row =
  { file : string
  ; goal : int
  ; verdict : string
  ; conflicts : int
  ; decisions : int
  ; propagations : int
  }

let stat_row_of_line line : stat_row option =
  match parse_json_object line with
  | [] -> None
  | kvs ->
    let get k = List.assoc_opt k kvs in
    let geti k =
      match get k with
      | Some v -> int_of_string_opt v
      | None -> None
    in
    (match get "file", geti "goal" with
     | Some file, Some goal ->
       Some
         { file
         ; goal
         ; verdict =
             (match get "verdict" with
              | Some v -> v
              | None -> "?")
         ; conflicts = Option.value ~default:0 (geti "conflicts")
         ; decisions = Option.value ~default:0 (geti "decisions")
         ; propagations = Option.value ~default:0 (geti "propagations")
         }
     | _ -> None)
;;

(* ------------------------------------------------------------------ *)
(* Directory listing helpers *)
(* ------------------------------------------------------------------ *)

let dir_entries dir =
  match Sys.readdir dir with
  | a ->
    Array.sort String.compare a;
    Array.to_list a
  | exception _ -> []
;;

(* stats JSONL files, most-recent first (by mtime). *)
let recent_stats_files stats_dir k =
  if not (Sys.file_exists stats_dir)
  then []
  else
    dir_entries stats_dir
    |> List.filter (fun f -> Filename.check_suffix f ".jsonl")
    |> List.map (fun f -> Filename.concat stats_dir f)
    |> List.map (fun p -> p, (Unix.stat p).Unix.st_mtime)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.filteri (fun idx _ -> idx < k)
    |> List.map fst
;;

(* Gate logs, most-recent first. Prefer a full `gate run` (has case results) over an
   honeypot-only `gate selftest`: concurrent gate agents may leave a selftest as the
   newest log, and reporting "0 cases" off that would be misleading. Falls back to the
   newest overall when no full run is present. *)
let latest_gate_log logs_dir =
  if not (Sys.file_exists logs_dir)
  then None
  else (
    let by_recency =
      dir_entries logs_dir
      |> List.filter (fun f -> starts_with ~prefix:"gate-" f)
      |> List.map (fun f -> Filename.concat (Filename.concat logs_dir f) "gate.log")
      |> List.filter Sys.file_exists
      |> List.map (fun p -> p, (Unix.stat p).Unix.st_mtime)
      |> List.sort (fun (_, a) (_, b) -> compare b a)
      |> List.map fst
    in
    let has_cases p =
      match read_file_opt p with
      | Some s -> contains_sub s "[case]"
      | None -> false
    in
    match List.find_opt has_cases by_recency with
    | Some p -> Some p
    | None ->
      (match by_recency with
       | [] -> None
       | p :: _ -> Some p))
;;

(* ------------------------------------------------------------------ *)
(* TASKS.md milestone parsing *)
(* ------------------------------------------------------------------ *)

type task_row =
  { id : string
  ; status : string
  }

let parse_tasks path : task_row list =
  match read_file_opt path with
  | None -> []
  | Some s ->
    lines s
    |> List.filter_map (fun line ->
      let line = trim line in
      if not (starts_with ~prefix:"|" line)
      then None
      else (
        let cells = String.split_on_char '|' line |> List.map trim in
        (* leading/trailing empties from the outer pipes *)
        match cells with
        | _ :: id :: _title :: status :: _ ->
          if String.equal id "id" || contains_sub id "---" || String.equal id ""
          then None
          else Some { id; status }
        | _ -> None))
;;

let milestone_of_id id =
  (* "M0-harness" -> Some "M0"; "ADR-0003" -> None *)
  if String.length id >= 2 && id.[0] = 'M' && id.[1] >= '0' && id.[1] <= '9'
  then Some (String.sub id 0 2)
  else None
;;

let is_done status = starts_with ~prefix:"done" (String.lowercase_ascii (trim status))
let is_in_progress status = contains_sub (String.lowercase_ascii status) "in progress"

(* ------------------------------------------------------------------ *)
(* Gate log parsing *)
(* ------------------------------------------------------------------ *)

type gate_summary =
  { lean : string
  ; encoding : string
  ; case_outcomes : (string * int) list (* outcome -> count *)
  ; cache_hits : int
  ; cache_total : int
  ; honeypots : int
  ; honeypot_floor : string
  ; honeypots_ok : bool
  }

let word_after tok line =
  (* returns the whitespace-delimited word following [tok] in [line] *)
  match String.split_on_char ' ' (trim line) |> List.filter (fun w -> w <> "") with
  | ws ->
    let rec find = function
      | a :: b :: _ when String.equal a tok -> Some b
      | _ :: rest -> find rest
      | [] -> None
    in
    find ws
;;

let parse_gate_log path : gate_summary option =
  match read_file_opt path with
  | None -> None
  | Some s ->
    let ls = lines s in
    let lean =
      List.find_map (fun l -> if contains_sub l "lean:" then Some (trim l) else None) ls
      |> Option.value ~default:"n/a"
    in
    let encoding =
      List.find_map
        (fun l -> if contains_sub l "encoding:" then Some (trim l) else None)
        ls
      |> Option.value ~default:"n/a"
    in
    let outcomes = Hashtbl.create 8 in
    let hits = ref 0
    and total = ref 0 in
    List.iter
      (fun l ->
         let t = trim l in
         if starts_with ~prefix:"[case]" t
         then (
           incr total;
           (match word_after "[case]" t with
            | Some outcome ->
              let cur =
                try Hashtbl.find outcomes outcome with
                | Not_found -> 0
              in
              Hashtbl.replace outcomes outcome (cur + 1)
            | None -> ());
           if contains_sub t "(cache)" then incr hits))
      ls;
    let honeypots = ref 0
    and honeypot_certified = ref false in
    List.iter
      (fun l ->
         let t = trim l in
         if starts_with ~prefix:"[honeypot]" t
         then (
           incr honeypots;
           (* a honeypot that CERTIFIES is always a breach (DESIGN.md §10) *)
           if contains_sub t "CERTIFIED" then honeypot_certified := true))
      ls;
    let floor =
      List.find_map
        (fun l ->
           if contains_sub l "floor"
           then (
             (* e.g. "HONEYPOTS (4; floor 4) ..." *)
             match
               word_after "floor" (String.map (fun c -> if c = ')' then ' ' else c) l)
             with
             | Some f -> Some f
             | None -> None)
           else None)
        ls
      |> Option.value ~default:"?"
    in
    (* Robust to summary-line wording changes across gate versions: OK iff no honeypot
       certified and the count meets the declared floor. *)
    let honeypots_ok =
      (not !honeypot_certified)
      &&
      match int_of_string_opt floor with
      | Some f -> !honeypots >= f
      | None -> !honeypots > 0
    in
    Some
      { lean
      ; encoding
      ; case_outcomes =
          Hashtbl.fold (fun k v acc -> (k, v) :: acc) outcomes []
          |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      ; cache_hits = !hits
      ; cache_total = !total
      ; honeypots = !honeypots
      ; honeypot_floor = floor
      ; honeypots_ok
      }
;;

(* ------------------------------------------------------------------ *)
(* Config *)
(* ------------------------------------------------------------------ *)

type config =
  { repo : string
  ; logs : string
  ; stats : string
  ; tasks : string
  ; budgets : string
  ; harness_digest : string option
  ; out : string
  }

let usage () =
  prerr_endline
    "usage: status_gen --repo DIR [--logs DIR] [--stats DIR] [--tasks FILE] [--budgets \
     FILE] [--harness-digest FILE] [--out FILE]";
  exit 2
;;

let parse_args () =
  let repo = ref "." in
  let logs = ref "" in
  let stats = ref "" in
  let tasks = ref "" in
  let budgets = ref "" in
  let harness_digest = ref None in
  let out = ref "STATUS.md" in
  let rec go = function
    | "--repo" :: v :: r ->
      repo := v;
      go r
    | "--logs" :: v :: r ->
      logs := v;
      go r
    | "--stats" :: v :: r ->
      stats := v;
      go r
    | "--tasks" :: v :: r ->
      tasks := v;
      go r
    | "--budgets" :: v :: r ->
      budgets := v;
      go r
    | "--harness-digest" :: v :: r ->
      harness_digest := Some v;
      go r
    | "--out" :: v :: r ->
      out := v;
      go r
    | [] -> ()
    | _ -> usage ()
  in
  go (List.tl (Array.to_list Sys.argv));
  let default rel r = if String.equal !r "" then rel else !r in
  { repo = !repo
  ; logs = default (Filename.concat !repo "../logs") logs
  ; stats =
      (if String.equal !stats ""
       then Filename.concat (default (Filename.concat !repo "../logs") logs) "stats"
       else !stats)
  ; tasks = default (Filename.concat !repo "TASKS.md") tasks
  ; budgets = default (Filename.concat !repo "tools/line_budgets.txt") budgets
  ; harness_digest = !harness_digest
  ; out = !out
  }
;;

(* ------------------------------------------------------------------ *)
(* Main *)
(* ------------------------------------------------------------------ *)

let () =
  let cfg = parse_args () in
  let b = Buffer.create 8192 in
  let out fmt = Printf.ksprintf (fun s -> Buffer.add_string b s) fmt in
  (* --- git basics --- *)
  let head =
    match git cfg.repo [ "rev-parse"; "--short"; "HEAD" ] with
    | Some h -> h
    | None -> "unknown"
  in
  let head_time =
    match git cfg.repo [ "log"; "-1"; "--format=%ct" ] with
    | Some s -> int_of_string_opt (trim s)
    | None -> None
  in
  out "# STATUS\n\n";
  out "**GENERATED by `make status` — do not edit by hand.** Regenerate after\n";
  out "changes land; the master reads this file, not the territory (DESIGN.md §11).\n\n";
  out "generated at %s\n\n" head;
  (* ================= OUTCOME METRICS ================= *)
  out "## Outcome metrics\n\n";
  (* Milestones from TASKS.md *)
  let rows = parse_tasks cfg.tasks in
  let milestones =
    List.filter_map (fun r -> Option.map (fun m -> m, r) (milestone_of_id r.id)) rows
  in
  let ms_names = List.sort_uniq String.compare (List.map fst milestones) in
  let current =
    List.find_opt
      (fun m ->
         let these = List.filter (fun (mm, _) -> mm = m) milestones in
         List.exists (fun (_, r) -> not (is_done r.status)) these)
      ms_names
  in
  out
    "- **Current milestone:** %s\n"
    (match current with
     | Some m -> Printf.sprintf "%s (first milestone with open rows)" m
     | None -> "all parsed milestones complete");
  out "  | milestone | done / total |\n  |---|---|\n";
  List.iter
    (fun m ->
       let these = List.filter (fun (mm, _) -> mm = m) milestones in
       let d = List.length (List.filter (fun (_, r) -> is_done r.status) these) in
       out "  | %s | %d / %d |\n" m d (List.length these))
    ms_names;
  out "\n";
  (* Harness pass/fail from the captured digest *)
  let harness_line =
    match cfg.harness_digest with
    | Some f ->
      (match read_file_opt f with
       | Some s ->
         List.find_opt (fun l -> starts_with ~prefix:"harness:" (trim l)) (lines s)
       | None -> None)
    | None -> None
  in
  out
    "- **Harness (fast regression suite):** %s\n"
    (match harness_line with
     | Some l -> trim l
     | None -> "n/a (no digest captured this run)");
  (* Gate outcomes from the latest gate log *)
  let gate = Option.bind (latest_gate_log cfg.logs) parse_gate_log in
  (match gate with
   | None -> out "- **Gate (Lean oracle):** n/a (no gate log found under %s)\n" cfg.logs
   | Some g ->
     let outc =
       String.concat
         ", "
         (List.map (fun (k, v) -> Printf.sprintf "%s %d" k v) g.case_outcomes)
     in
     out
       "- **Gate (Lean oracle):** %d case(s) [%s]; honeypots %d/floor %s %s\n"
       g.cache_total
       (if outc = "" then "none" else outc)
       g.honeypots
       g.honeypot_floor
       (if g.honeypots_ok then "(all matched)" else "(CHECK: no match-confirmation line)"));
  (* Corpus solved-rate from stats over tests/cases. Use only the single most recent stats
     file: `make status` never writes a new one, so this is stable between back-to-back
     runs, and it reflects one coherent run rather than a mix. `make status-fresh`
     refreshes it before generating. *)
  let stat_files = recent_stats_files cfg.stats 1 in
  let all_rows =
    List.concat_map
      (fun f ->
         match read_file_opt f with
         | Some s -> List.filter_map stat_row_of_line (lines s)
         | None -> [])
      stat_files
  in
  let case_rows = List.filter (fun r -> contains_sub r.file "tests/cases/") all_rows in
  (* dedupe by (file,goal), keeping the most recent (first, since files are recent-first) *)
  let seen = Hashtbl.create 64 in
  let case_rows_dedup =
    List.filter
      (fun r ->
         let key = r.file, r.goal in
         if Hashtbl.mem seen key
         then false
         else (
           Hashtbl.add seen key ();
           true))
      case_rows
  in
  let solved =
    List.length
      (List.filter (fun r -> r.verdict = "sat" || r.verdict = "unsat") case_rows_dedup)
  in
  let total_cases = List.length case_rows_dedup in
  out
    "- **Corpus solved-rate (tests/cases, by our solver):** %s\n"
    (if total_cases = 0
     then
       "0% — no solver verdicts yet (solver is a stub; THIS is the number that must move)"
     else
       Printf.sprintf
         "%d%% (%d/%d) — solver is a stub until M1+; THIS is the number that must move"
         (100 * solved / total_cases)
         solved
         total_cases);
  (* Days since last outcome improvement (heuristic) *)
  let improvement_paths = [ "smt"; "tests/cases" ] in
  let last_improvement =
    match git cfg.repo ([ "log"; "-1"; "--format=%ct"; "--" ] @ improvement_paths) with
    | Some s -> int_of_string_opt (trim s)
    | None -> None
  in
  out
    "- **Days since last outcome improvement:** %s\n"
    (match head_time, last_improvement with
     | Some h, Some l ->
       Printf.sprintf
         "%d (commits touching %s; measured to HEAD's commit time, not wall-clock)"
         (max 0 ((h - l) / 86400))
         (String.concat " / " improvement_paths)
     | _ -> "n/a");
  out "\n";
  (* ================= PROCESS METRICS ================= *)
  out "## Process metrics\n\n";
  (* Per-module line counts vs budget *)
  out "### Module line counts vs budget (`tools/line_budgets.txt`)\n\n";
  (match read_file_opt cfg.budgets with
   | None -> out "n/a (no budgets config at %s)\n" cfg.budgets
   | Some s ->
     out "| module | lines | budget | |\n|---|---|---|---|\n";
     List.iter
       (fun line ->
          let line = trim line in
          if line <> "" && not (starts_with ~prefix:"#" line)
          then (
            match String.split_on_char ' ' line |> List.filter (fun w -> w <> "") with
            | [ path; budget ] ->
              let n = count_lines_in_module cfg.repo path in
              let bud =
                match int_of_string_opt budget with
                | Some b -> b
                | None -> 0
              in
              let flag = if bud > 0 && n > bud then "OVER" else "ok" in
              out "| %s | %d | %d | %s |\n" path n bud flag
            | _ -> ()))
       (lines s));
  out "\n";
  (* Search-counter distribution (log-scale buckets — deterministic, so safe in the
     committed file). Exact per-goal wall_ms is intentionally NOT reported here: it is
     nondeterministic and lives only in the uncommitted stats sidecar (I5's spirit — no
     nondeterministic values in a committed artifact). *)
  out
    "### Search-counter distribution (most recent stats run, %d goal(s))\n\n"
    (List.length all_rows);
  if all_rows = []
  then out "n/a (no stats JSONL under %s)\n" cfg.stats
  else (
    let hist sel =
      List.map
        (fun bk -> bk, List.length (List.filter (fun r -> bucket (sel r) = bk) all_rows))
        bucket_order
    in
    let show name sel =
      out
        "- %s: %s\n"
        name
        (String.concat
           "  "
           (List.filter_map
              (fun (bk, c) -> if c > 0 then Some (Printf.sprintf "%s=%d" bk c) else None)
              (hist sel)))
    in
    show "conflicts" (fun r -> r.conflicts);
    show "decisions" (fun r -> r.decisions);
    show "propagations" (fun r -> r.propagations);
    out
      "\n\
       _Per-goal wall_ms and exact counts: uncommitted stats sidecar under `%s` (nightly \
       aggregation)._\n"
      cfg.stats);
  out "\n";
  (* Gate cache *)
  out "### Oracle cache & triage\n\n";
  (match gate with
   | None -> out "- Cache hit-rate: n/a (no gate log)\n"
   | Some g ->
     out
       "- Cache hit-rate (last gate run): %s (%d/%d cases from cache)\n"
       (if g.cache_total = 0
        then "n/a"
        else Printf.sprintf "%d%%" (100 * g.cache_hits / g.cache_total))
       g.cache_hits
       g.cache_total;
     out "- %s\n" g.lean;
     out "- %s\n" g.encoding);
  (* cache and logs are siblings of main/; derive cache from --logs so it resolves the
     same regardless of the invocation directory. *)
  let cache_dir = Filename.concat (Filename.dirname cfg.logs) "cache" in
  out
    "- Cache entries on disk: %s\n"
    (if Sys.file_exists cache_dir
     then string_of_int (count_files_rec cache_dir)
     else "n/a");
  out
    "- Triage / nightly queue depths: n/a (no CI/nightly scheduler wired yet — M0-gate \
     is manual `make gate`)\n\n";
  (* Hygiene *)
  out "### Repository hygiene\n\n";
  let strip_ref b =
    if starts_with ~prefix:"refs/heads/" b
    then String.sub b 11 (String.length b - 11)
    else b
  in
  let all_branches =
    match git cfg.repo [ "worktree"; "list"; "--porcelain" ] with
    | Some s ->
      lines s
      |> List.filter_map (fun l ->
        if starts_with ~prefix:"branch " l
        then Some (strip_ref (trim (String.sub l 7 (String.length l - 7))))
        else None)
    | None -> []
  in
  (* Exclude the trunk (`oxsmt`, the main/ checkout); count task worktrees only. *)
  let task_worktrees = List.filter (fun b -> not (String.equal b "oxsmt")) all_branches in
  let in_progress = List.length (List.filter (fun r -> is_in_progress r.status) rows) in
  out
    "- Task worktrees: %d  |  in-progress TASKS.md rows: %d %s\n"
    (List.length task_worktrees)
    in_progress
    (if List.length task_worktrees > in_progress
     then
       "(worktrees exceed in-progress board rows — worktrees may be ahead of the board, \
        or orphaned)"
     else "");
  out
    "- Worktree branches: %s\n"
    (if task_worktrees = [] then "n/a (trunk only)" else String.concat ", " task_worktrees);
  out "\n";
  out "---\n";
  out
    "_Heuristics: milestone = first M-row group in TASKS.md with any non-`done` row; \
     solved-rate counts `tests/cases` goals with a definite verdict in recent stats; \
     days-since-improvement = git commits touching %s, measured to HEAD's commit \
     timestamp. All best-effort and documented in tests/README.md._\n"
    (String.concat " / " improvement_paths);
  (* Write the file *)
  let oc = open_out_bin cfg.out in
  output_string oc (Buffer.contents b);
  close_out oc;
  (* Digest to stdout (~5 lines) *)
  Printf.printf "status: wrote %s @ %s\n" cfg.out head;
  Printf.printf
    "  milestone: %s\n"
    (match current with
     | Some m -> m
     | None -> "all complete");
  Printf.printf
    "  harness: %s\n"
    (match harness_line with
     | Some l -> trim l
     | None -> "n/a");
  Printf.printf
    "  gate: %s\n"
    (match gate with
     | Some g ->
       Printf.sprintf
         "%d cases, cache %d/%d, honeypots %d"
         g.cache_total
         g.cache_hits
         g.cache_total
         g.honeypots
     | None -> "n/a");
  Printf.printf
    "  corpus solved-rate: %s\n"
    (if total_cases = 0 then "0%" else Printf.sprintf "%d%%" (100 * solved / total_cases))
;;
