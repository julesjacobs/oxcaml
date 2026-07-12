(* STATUS.md generator (DESIGN.md §8.4, §11).

   STATUS.md is the master's empirical view of the world: outcome metrics first
   (goal-displacement defense — §11), process metrics after. This tool ONLY AGGREGATES
   existing artifacts; it runs nothing (no harness, no Lean) and re-derives no product
   state, so its output is a pure function of the inputs on disk:

   - the COMMITTED tests/corpus/baseline_summary.json (schema oxsmt-corpus-baseline/v1) ->
     the HEADLINE per-logic corpus solved-rate (solved-sat+solved-unsat)/scanned, a loud
     `‼ CORPUS SOUNDNESS BREACH` line if mismatch_count>0, and a staleness caption if the
     baseline's trunk hash isn't a prefix of the summarized HEAD. Read from the fixed repo
     path ONLY — never the ad-hoc run JSONs `make corpus-run` writes to ../logs (task
     #124/#133).
   - TASKS.md -> milestone completion (parse the M-rows)
   - git -> generated-at HEAD, days-since-last-outcome-improvement, worktree/branch
     hygiene
   - the last captured harness digest -> live pass/fail (written by `make status-fresh` /
     the fast suite; this reads that file, it does not run the harness)
   - the latest gate log WHOSE PROVENANCE HEAD MATCHES the summarized tree (task #133):
     all worktrees share ../logs, so a log is trusted only if the gate dir name
     (gate-<stamp>-<pid>-<HEAD>) records the same HEAD we are summarizing — otherwise loud
     absence, never a foreign/stale verdict -> gate outcome counts, honeypot floor, cache
   - most recent stats JSONL -> counter-bucket distribution + the tests/cases suite-health
     sub-metric (demoted from headline once the corpus baseline landed; buckets and
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

(* Run [argv], capture stdout (stderr -> /dev/null). Returns (stdout, ok). fds are opened
   inside the protected region and closed on every path incl. a create_process raise
   (status-guard review S3: no pipe/devnull leak). git here is our own trusted repo, so —
   unlike the gate's hostile-git path — no read timeout is needed. *)
let run_capture argv =
  let r = ref None
  and w = ref None
  and dn = ref None in
  let close_opt rf =
    match !rf with
    | Some fd ->
      (try Unix.close fd with
       | _ -> ());
      rf := None
    | None -> ()
  in
  match
    Fun.protect
      ~finally:(fun () ->
        close_opt r;
        close_opt w;
        close_opt dn)
      (fun () ->
         let rr, ww = Unix.pipe () in
         r := Some rr;
         w := Some ww;
         let d = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
         dn := Some d;
         let pid = Unix.create_process argv.(0) argv Unix.stdin ww d in
         close_opt w;
         close_opt dn;
         let out = read_all_fd rr in
         let _, status = Unix.waitpid [] pid in
         out, status)
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
(* Committed corpus baseline (tests/corpus/baseline_summary.json). *)
(* The JSONL reader above is FLAT; this artifact is nested *)
(* (logics.<L>.outcomes.<k>), so a small recursive JSON reader — stdlib *)
(* only. Any parse failure degrades to "no committed corpus baseline",  *)
(* never a crash. status_gen reads ONLY this committed path, never the *)
(* ad-hoc run JSONs `make corpus-run` writes under ../logs. *)
(* ------------------------------------------------------------------ *)

type json =
  | JNull
  | JBool of bool
  | JNum of float
  | JStr of string
  | JArr of json list
  | JObj of (string * json) list

exception Json_error of string

let json_parse (s : string) : json =
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then s.[!pos] else '\000' in
  let adv () = incr pos in
  let rec skip_ws () =
    if !pos < n
    then (
      match s.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
        adv ();
        skip_ws ()
      | _ -> ())
  in
  let expect c =
    if peek () = c then adv () else raise (Json_error (Printf.sprintf "expected %c" c))
  in
  let rec value () =
    skip_ws ();
    match peek () with
    | '{' -> obj ()
    | '[' -> arr ()
    | '"' -> JStr (str ())
    | 't' -> lit "true" (JBool true)
    | 'f' -> lit "false" (JBool false)
    | 'n' -> lit "null" JNull
    | c when c = '-' || (c >= '0' && c <= '9') -> num ()
    | c -> raise (Json_error (Printf.sprintf "unexpected %c" c))
  and lit word v =
    String.iter
      (fun c -> if peek () = c then adv () else raise (Json_error "bad literal"))
      word;
    v
  and str () =
    expect '"';
    let b = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | '\000' -> raise (Json_error "unterminated string")
      | '"' -> adv ()
      | '\\' ->
        adv ();
        (match peek () with
         | 'n' -> Buffer.add_char b '\n'
         | 't' -> Buffer.add_char b '\t'
         | 'r' -> Buffer.add_char b '\r'
         | c -> Buffer.add_char b c);
        adv ();
        loop ()
      | c ->
        Buffer.add_char b c;
        adv ();
        loop ()
    in
    loop ();
    Buffer.contents b
  and num () =
    let start = !pos in
    if peek () = '-' then adv ();
    while
      match peek () with
      | '0' .. '9' | '.' | 'e' | 'E' | '+' | '-' -> true
      | _ -> false
    do
      adv ()
    done;
    let sub = String.sub s start (!pos - start) in
    match float_of_string_opt sub with
    | Some f -> JNum f
    | None -> raise (Json_error ("bad number " ^ sub))
  and arr () =
    expect '[';
    skip_ws ();
    if peek () = ']'
    then (
      adv ();
      JArr [])
    else (
      let items = ref [] in
      let rec loop () =
        let v = value () in
        items := v :: !items;
        skip_ws ();
        match peek () with
        | ',' ->
          adv ();
          loop ()
        | ']' -> adv ()
        | _ -> raise (Json_error "expected , or ]")
      in
      loop ();
      JArr (List.rev !items))
  and obj () =
    expect '{';
    skip_ws ();
    if peek () = '}'
    then (
      adv ();
      JObj [])
    else (
      let items = ref [] in
      let rec loop () =
        skip_ws ();
        let k = str () in
        skip_ws ();
        expect ':';
        let v = value () in
        items := (k, v) :: !items;
        skip_ws ();
        match peek () with
        | ',' ->
          adv ();
          loop ()
        | '}' -> adv ()
        | _ -> raise (Json_error "expected , or }")
      in
      loop ();
      JObj (List.rev !items))
  in
  let v = value () in
  skip_ws ();
  v
;;

let jmember k = function
  | JObj kvs -> List.assoc_opt k kvs
  | _ -> None
;;

let jint = function
  | JNum f -> Some (int_of_float f)
  | _ -> None
;;

let jstr = function
  | JStr s -> Some s
  | _ -> None
;;

let jbool = function
  | JBool b -> Some b
  | _ -> None
;;

type logic_stat =
  { logic : string
  ; total_available : int
  ; scanned : int
  ; solved : int (* solved-sat + solved-unsat *)
  ; mismatches : int
  }

(* Board #69 binary-provenance stamp, if the baseline carries one. A baseline promoted
   before #69 has no stamp -> [None] -> rendered as UNVERIFIED provenance (never crash). *)
type corpus_stamp =
  { build_commit : string
  ; dirty : bool
  ; assertions : string
  ; euf_self_check : string
  ; release_config : bool
  }

type corpus_summary =
  { schema : string
  ; c_trunk : string (* the trunk hash/label the baseline measured *)
  ; logics : logic_stat list
  ; mismatch_count : int
  ; stamp : corpus_stamp option
  }

(* Pure parse of the committed baseline. Returns None on any structural surprise or an
   unrecognized schema (forward-safety: an unknown shape must degrade, not mis-report). *)
let parse_corpus_summary (s : string) : corpus_summary option =
  match json_parse s with
  | exception _ -> None
  | j ->
    let top_str k = Option.value ~default:"?" (Option.bind (jmember k j) jstr) in
    let top_int k = Option.value ~default:0 (Option.bind (jmember k j) jint) in
    let schema = top_str "schema" in
    if not (starts_with ~prefix:"oxsmt-corpus-baseline/" schema)
    then None
    else (
      match jmember "logics" j with
      | Some (JObj entries) ->
        let logics =
          List.map
            (fun (name, lj) ->
               let li k = Option.value ~default:0 (Option.bind (jmember k lj) jint) in
               let oi k =
                 Option.value
                   ~default:0
                   (Option.bind (Option.bind (jmember "outcomes" lj) (jmember k)) jint)
               in
               { logic = name
               ; total_available = li "total_available"
               ; scanned = li "scanned"
               ; solved = oi "solved-sat" + oi "solved-unsat"
               ; mismatches = li "mismatches"
               })
            entries
        in
        let stamp =
          match jmember "stamp" j with
          | Some (JObj _ as sj) ->
            let ss k d = Option.value ~default:d (Option.bind (jmember k sj) jstr) in
            let sb k = Option.value ~default:false (Option.bind (jmember k sj) jbool) in
            Some
              { build_commit = ss "build_commit" "?"
              ; dirty = sb "dirty"
              ; assertions = ss "assertions" "?"
              ; euf_self_check = ss "euf_self_check" "?"
              ; release_config = sb "release_config"
              }
          | _ -> None
        in
        Some
          { schema
          ; c_trunk = top_str "trunk"
          ; logics
          ; mismatch_count = top_int "mismatch_count"
          ; stamp
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

(* Provenance HEAD embedded in a gate log dir name by the gate runner (task #133): dir
   names are "gate-<stamp>-<pid>-<HEAD>", where HEAD is the FINAL '-'-component and 40 hex
   chars. Returns the HEAD, or None for a legacy "gate-<stamp>" dir or a "nohead" run
   (both treated as unmatched). Taking the last component keeps this robust to the extra
   <pid> field the gate fix round added. *)
let provenance_of_dirname f =
  match List.rev (String.split_on_char '-' f) with
  | last :: _ ->
    if
      String.length last = 40
      && String.for_all (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) last
    then Some last
    else None
  | [] -> None
;;

type gate_pick =
  | Matched of string (* path to the chosen gate.log produced by THIS tree's HEAD *)
  | No_match_foreign of
      int (* no log for this HEAD; N gate logs exist for other checkouts *)
  | No_logs

(* Pick the gate log to summarize, GUARDED BY PROVENANCE (task #133): all worktrees share
   ../logs, so status_gen must read only a log produced by the very tree it is summarizing
   — otherwise a concurrent worktree's gate run contaminates trunk's STATUS. Among logs
   whose embedded HEAD equals [head], prefer a full `gate run` (has case results) over an
   honeypot-only selftest, newest first. A log with no matching provenance is never used:
   the result is loud absence, never a foreign or stale number. *)
let pick_gate_log logs_dir ~(head : string option) : gate_pick =
  if not (Sys.file_exists logs_dir)
  then No_logs
  else (
    let all =
      dir_entries logs_dir
      |> List.filter (fun f -> starts_with ~prefix:"gate-" f)
      |> List.filter_map (fun f ->
        let p = Filename.concat (Filename.concat logs_dir f) "gate.log" in
        if Sys.file_exists p
        then Some (provenance_of_dirname f, p, (Unix.stat p).Unix.st_mtime)
        else None)
    in
    if all = []
    then No_logs
    else (
      let matches_head prov =
        match head, prov with
        | Some h, Some p -> String.equal h p
        | _ -> false
      in
      let matching =
        all
        |> List.filter (fun (prov, _, _) -> matches_head prov)
        |> List.sort (fun (_, _, a) (_, _, b) -> compare b a)
        |> List.map (fun (_, p, _) -> p)
      in
      match matching with
      | [] -> No_match_foreign (List.length all)
      | _ ->
        let has_cases p =
          match read_file_opt p with
          | Some s -> contains_sub s "[case]"
          | None -> false
        in
        (match List.find_opt has_cases matching with
         | Some p -> Matched p
         | None -> Matched (List.hd matching))))
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
  ; (* CERTIFIED trust-tier mix (#86 / AP2b): [decide]/[grind]/[omega] are kernel-checked,
       [native_decide] is compiler-trusted (Lean.ofReduceBool axiom + compiler). Parsed
       from the trailing tactic on each [case] CERTIFIED log line. [cert_untagged] counts
       CERTIFIED lines carrying no recognised tactic (e.g. a pre-#86 log). *)
    cert_kernel : int
  ; cert_compiler : int
  ; cert_untagged : int
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
    let cert_kernel = ref 0
    and cert_compiler = ref 0
    and cert_untagged = ref 0 in
    (* Trailing tactic on a "[case] CERTIFIED … (disp) <tactic>" line is the trust tier
       (#86 / AP2b). A line with no recognised tactic (e.g. a pre-#86 blank detail) is
       counted [untagged] rather than silently attributed to the kernel default. *)
    let classify_tier t =
      match List.rev (String.split_on_char ' ' t |> List.filter (fun w -> w <> "")) with
      | "native_decide" :: _ -> incr cert_compiler
      | ("decide" | "grind" | "omega") :: _ -> incr cert_kernel
      | _ -> incr cert_untagged
    in
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
              Hashtbl.replace outcomes outcome (cur + 1);
              if String.equal outcome "CERTIFIED" then classify_tier t
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
      ; cert_kernel = !cert_kernel
      ; cert_compiler = !cert_compiler
      ; cert_untagged = !cert_untagged
      ; cache_hits = !hits
      ; cache_total = !total
      ; honeypots = !honeypots
      ; honeypot_floor = floor
      ; honeypots_ok
      }
;;

(* ------------------------------------------------------------------ *)
(* Harness-digest staleness guard (task #25). *)
(* *)
(* `make status` AGGREGATES the last captured harness digest; it does *)
(* NOT re-run the harness (that is `make status-fresh`). So if the *)
(* digest predates a tests/cases change — cases added/removed by *)
(* another task — the committed STATUS would silently carry a stale *)
(* pass/fail line (the demonstrated 24-vs-27 drift). We refuse to do *)
(* that: STATUS is "generated, never gameable" (DESIGN §11). *)
(* *)
(* Detection is by the per-dir file COUNTS the harness embeds in its *)
(* own digest line — "[tests/cases: 27, tests/harness/fixtures: 4]" — *)
(* compared against the live .smt2 count in each dir. Chosen over a *)
(* raw mtime compare (a fresh `git worktree add` resets every file's *)
(* mtime, which would make the guard cry stale on every new worktree) *)
(* and over a new recorded-sha sidecar (the counts are already in the *)
(* digest; no extra plumbing). It catches the count-drift class that *)
(* actually bit us; a same-count content edit is a documented residual *)
(* — status-fresh (the nightly path) always regenerates and is exact. *)
(* *)
(* FAIL CLOSED on an unparseable digest: if a digest is present but its *)
(* per-dir count block is absent/empty or ANY entry is malformed, the *)
(* guard refuses rather than treating "zero parsed counts" as no-drift *)
(* — otherwise a future digest-wording change would silently DISARM the *)
(* guard (the cache-reader lesson: an unrecognized shape must be loud, *)
(* not benign). Partial tolerance is deliberately rejected: one bad *)
(* entry poisons the whole check, so a malformed `tests/cases` entry *)
(* can't slip past by leaving the other dirs well-formed. *)
(* ------------------------------------------------------------------ *)

(* Parse the per-dir file counts embedded in a run_harness "harness:" digest line, e.g.
   "harness: 31 file(s) [tests/cases: 27, tests/harness/fixtures: 4] | PASS 31 | FAIL 0"
   -> Ok [("tests/cases", 27); ("tests/harness/fixtures", 4)]. STRICT: the "[...]" block
   must be present, non-empty, and every comma-separated entry must be exactly "<dir>:
   <int>" (dir non-empty); anything else is [Error why] (fail-closed), never a
   partial/empty success. *)
let parse_harness_dir_counts (harness_line : string)
  : ((string * int) list, string) result
  =
  match String.index_opt harness_line '[' with
  | None -> Error "no per-dir count block ([...]) in the harness digest line"
  | Some lb ->
    (match String.index_from_opt harness_line lb ']' with
     | None -> Error "unterminated per-dir count block ('[' with no ']')"
     | Some rb ->
       let inner = trim (String.sub harness_line (lb + 1) (rb - lb - 1)) in
       if inner = ""
       then Error "empty per-dir count block ([])"
       else (
         let segs = String.split_on_char ',' inner in
         let rec go acc = function
           | [] -> Ok (List.rev acc)
           | seg :: rest ->
             (match String.rindex_opt seg ':' with
              | None ->
                Error (Printf.sprintf "malformed per-dir entry %S (no ':')" (trim seg))
              | Some ci ->
                let dir = trim (String.sub seg 0 ci) in
                let cnt = trim (String.sub seg (ci + 1) (String.length seg - ci - 1)) in
                (match dir, int_of_string_opt cnt with
                 | "", _ ->
                   Error
                     (Printf.sprintf "malformed per-dir entry %S (empty dir)" (trim seg))
                 | _, None ->
                   Error
                     (Printf.sprintf
                        "malformed per-dir entry %S (non-integer count)"
                        (trim seg))
                 | _, Some n -> go ((dir, n) :: acc) rest))
         in
         go [] segs))
;;

(* Count .smt2 files directly under [dir] (non-recursive), matching the harness's own
   [smt2_files_in]. Tolerant of a missing/unreadable dir (-> 0). *)
let count_smt2_in dir =
  match Sys.readdir dir with
  | a ->
    Array.fold_left
      (fun acc f -> if Filename.check_suffix f ".smt2" then acc + 1 else acc)
      0
      a
  | exception _ -> 0
;;

(* Verdict of checking a present harness digest against the live tree. *)
type digest_check =
  | Fresh (* every recorded per-dir count matches the tree *)
  | Drifted of (string * int * int) list (* (dir, recorded, actual) for each mismatch *)
  | Unparseable of string (* the digest is present but its count block can't be trusted *)

(* Compare the digest's recorded per-dir counts against the live tree. Dirs are resolved
   relative to [repo] (the harness is invoked from the repo root), absolute dirs used
   as-is. Unparseable counts are [Unparseable] (fail-closed), NOT silently "fresh". *)
let digest_check ~repo (harness_line : string) : digest_check =
  match parse_harness_dir_counts harness_line with
  | Error why -> Unparseable why
  | Ok dirs ->
    let drift =
      List.filter_map
        (fun (dir, recorded) ->
           let path =
             if Filename.is_relative dir then Filename.concat repo dir else dir
           in
           let actual = count_smt2_in path in
           if actual = recorded then None else Some (dir, recorded, actual))
        dirs
    in
    if drift = [] then Fresh else Drifted drift
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
(* Self-test: the gate-log provenance parse (task #133). *)
(* Guards the CRITICAL invariant that the provenance HEAD is the FINAL *)
(* '-'-component of the dir name, so the `gate-<stamp>-<pid>-<HEAD>` *)
(* naming introduced by the gate fix round still parses. *)
(* ------------------------------------------------------------------ *)

let selftest () =
  let failures = ref 0 in
  let check name got expected =
    if got = expected
    then Printf.printf "  ok   %s\n" name
    else (
      incr failures;
      Printf.printf "  FAIL %s\n" name)
  in
  let h = String.make 40 'a' in
  (* legacy pre-provenance dir: no HEAD component *)
  check "legacy gate-<stamp> -> None" (provenance_of_dirname "gate-20260101-000000") None;
  (* nohead sentinel (git unavailable) -> unmatched *)
  check "nohead -> None" (provenance_of_dirname "gate-20260101-000000-nohead") None;
  (* pre-fix head form (still valid: HEAD is last) *)
  check
    "gate-<stamp>-<HEAD> -> Some head"
    (provenance_of_dirname ("gate-20260101-000000-" ^ h))
    (Some h);
  (* NEW pid form from the fix round: HEAD is STILL the final component *)
  check
    "gate-<stamp>-<pid>-<HEAD> -> Some head"
    (provenance_of_dirname ("gate-20260101-000000-12345-" ^ h))
    (Some h);
  (* degenerate: wrong-length / non-hex final component -> None *)
  check
    "39-char final -> None"
    (provenance_of_dirname ("gate-20260101-000000-" ^ String.make 39 'a'))
    None;
  check
    "41-char final -> None"
    (provenance_of_dirname ("gate-20260101-000000-" ^ String.make 41 'a'))
    None;
  check
    "non-hex final -> None"
    (provenance_of_dirname ("gate-20260101-000000-" ^ String.make 40 'g'))
    None;
  (* Corpus baseline parse (task #124): nested JSON, per-logic solved math, mismatch
     detection, schema/robustness. *)
  let cfix =
    {|{"schema":"oxsmt-corpus-baseline/v1","trunk":"abc1234",
       "logics":{"QF_UF":{"total_available":100,"scanned":80,
         "outcomes":{"solved-sat":3,"solved-unsat":7,"unknown":70},"mismatches":0}},
       "mismatch_count":0}|}
  in
  (match parse_corpus_summary cfix with
   | Some cs ->
     check "corpus schema" cs.schema "oxsmt-corpus-baseline/v1";
     check "corpus trunk" cs.c_trunk "abc1234";
     check "corpus mismatch_count 0" cs.mismatch_count 0;
     (match cs.logics with
      | [ l ] ->
        check "corpus logic name" l.logic "QF_UF";
        check "corpus solved = sat+unsat" l.solved 10;
        check "corpus scanned" l.scanned 80;
        check "corpus total_available" l.total_available 100
      | _ -> check "corpus exactly one logic" false true)
   | None -> check "corpus fixture parses" false true);
  check
    "corpus mismatch>0 surfaced"
    (match
       parse_corpus_summary
         {|{"schema":"oxsmt-corpus-baseline/v1","trunk":"x","logics":{},"mismatch_count":3}|}
     with
     | Some cs -> cs.mismatch_count
     | None -> -1)
    3;
  check
    "corpus bad schema -> None"
    (parse_corpus_summary {|{"schema":"other/v9","logics":{},"mismatch_count":0}|} = None)
    true;
  check "corpus malformed json -> None" (parse_corpus_summary "{not json" = None) true;
  (* Board #69: binary-provenance stamp parse (release / non-release / absent). *)
  check
    "corpus stamp: release_config=true parsed"
    (match
       parse_corpus_summary
         {|{"schema":"oxsmt-corpus-baseline/v1","trunk":"x","logics":{},"mismatch_count":0,"stamp":{"build_commit":"abc","dirty":false,"assertions":"off","euf_self_check":"off","release_config":true}}|}
     with
     | Some { stamp = Some st; _ } -> st.release_config
     | _ -> false)
    true;
  check
    "corpus stamp: absent -> None (pre-#69 baseline)"
    (match
       parse_corpus_summary
         {|{"schema":"oxsmt-corpus-baseline/v1","trunk":"x","logics":{},"mismatch_count":0}|}
     with
     | Some { stamp; _ } -> stamp = None
     | None -> false)
    true;
  check
    "corpus stamp: non-release fields parsed"
    (match
       parse_corpus_summary
         {|{"schema":"oxsmt-corpus-baseline/v1","trunk":"x","logics":{},"mismatch_count":0,"stamp":{"build_commit":"abc","dirty":true,"assertions":"on","euf_self_check":"off","release_config":false}}|}
     with
     | Some { stamp = Some st; _ } ->
       (not st.release_config) && st.dirty && st.assertions = "on"
     | _ -> false)
    true;
  (* Harness-digest per-dir count parse (task #25 staleness guard). Happy paths return
     [Ok]; the three degenerate shapes fail CLOSED as [Error] so an unparseable digest can
     never silently disarm the guard. *)
  let is_err = function
    | Error _ -> true
    | Ok _ -> false
  in
  check
    "digest dir-counts: two dirs"
    (parse_harness_dir_counts
       "harness: 31 file(s) [tests/cases: 27, tests/harness/fixtures: 4] | PASS 31 | \
        FAIL 0")
    (Ok [ "tests/cases", 27; "tests/harness/fixtures", 4 ]);
  check
    "digest dir-counts: single dir"
    (parse_harness_dir_counts "harness: 5 file(s) [tests/cases: 5] | PASS 5 | FAIL 0")
    (Ok [ "tests/cases", 5 ]);
  check
    "digest dir-counts: promote suffix tolerated"
    (parse_harness_dir_counts
       "harness: 27 file(s) [tests/cases: 27] | PASS 27 | FAIL 0 | promote")
    (Ok [ "tests/cases", 27 ]);
  check
    "digest dir-counts: no bracket -> fail-closed"
    (is_err (parse_harness_dir_counts "harness: something without a bracket"))
    true;
  check
    "digest dir-counts: empty bracket -> fail-closed"
    (is_err (parse_harness_dir_counts "harness: 0 file(s) [] | PASS 0 | FAIL 0"))
    true;
  check
    "digest dir-counts: malformed entry -> fail-closed (poisons whole check)"
    (is_err
       (parse_harness_dir_counts
          "harness: 31 file(s) [tests/cases: abc, tests/harness/fixtures: 4] | PASS 31 | \
           FAIL 0"))
    true;
  if !failures = 0
  then print_endline "status_gen selftest: all checks passed"
  else (
    Printf.printf "status_gen selftest: %d check(s) FAILED\n" !failures;
    exit 1)
;;

(* ------------------------------------------------------------------ *)
(* Main *)
(* ------------------------------------------------------------------ *)

let () =
  (match Array.to_list Sys.argv with
   | _ :: "selftest" :: _ ->
     selftest ();
     exit 0
   | _ -> ());
  let cfg = parse_args () in
  let b = Buffer.create 8192 in
  let out fmt = Printf.ksprintf (fun s -> Buffer.add_string b s) fmt in
  (* --- git basics --- *)
  let head =
    match git cfg.repo [ "rev-parse"; "--short"; "HEAD" ] with
    | Some h -> h
    | None -> "unknown"
  in
  (* Full 40-char HEAD of the tree being summarized, for gate-log provenance matching
     (task #133). None if not a git repo — then no gate log can be trusted as this tree's. *)
  let full_head = git cfg.repo [ "rev-parse"; "HEAD" ] in
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
  (* Corpus solved-rate — THE headline (task #124/#133): per-logic
     (solved-sat+solved-unsat)/scanned from the COMMITTED baseline snapshot, never the
     ../logs run JSONs. Read once; reused by the stdout digest below. *)
  let corpus =
    match
      read_file_opt (Filename.concat cfg.repo "tests/corpus/baseline_summary.json")
    with
    | Some s -> parse_corpus_summary s
    | None -> None
  in
  let corpus_stale cs =
    (* fresh iff the baseline's trunk hash is a prefix of the summarized HEAD *)
    match full_head with
    | Some h -> not (String.length cs.c_trunk > 0 && starts_with ~prefix:cs.c_trunk h)
    | None -> true
  in
  (match corpus with
   | None ->
     out
       "- **Corpus solved-rate (committed baseline):** n/a (no committed \
        tests/corpus/baseline_summary.json)\n"
   | Some cs ->
     (* Soundness alarm FIRST: a verdict mismatch vs the pre-labeled corpus is a
        ship-stopping breach, honeypot-severity (DESIGN.md §8). *)
     if cs.mismatch_count > 0
     then
       out
         "- **‼ CORPUS SOUNDNESS BREACH:** %d verdict mismatch(es) vs the pre-labeled \
          corpus — ship-stopping (DESIGN.md §8).\n"
         cs.mismatch_count;
     let baseline_kind =
       if starts_with ~prefix:"79fd661" cs.c_trunk
       then "pre-adapter baseline"
       else "post-adapter measurement"
     in
     out
       "- **Corpus solved-rate (committed %s, trunk %s) — the headline:**\n"
       baseline_kind
       cs.c_trunk;
     List.iter
       (fun l ->
          let pct = if l.scanned = 0 then 0 else 100 * l.solved / l.scanned in
          out
            "  - %s: %d%% (%d/%d solved; %d/%d scanned)%s\n"
            l.logic
            pct
            l.solved
            l.scanned
            l.scanned
            l.total_available
            (if l.mismatches > 0
             then Printf.sprintf "  ‼ %d mismatch(es)" l.mismatches
             else ""))
       cs.logics;
     out
       "  - _baseline measured at %s; tree at %s — %s_\n"
       cs.c_trunk
       head
       (if corpus_stale cs
        then "STALE, re-run `make corpus-run` + promote for a current number"
        else "current");
     (* Board #69: provenance of the measuring binary. A promotable headline must come
        from a release-config binary (assertions off, debug oracles off) on a clean tree;
        surface the stamp and flag non-release / unverified provenance loudly. *)
     (match cs.stamp with
      | None ->
        out
          "  - _⚠ provenance UNVERIFIED — baseline predates the #69 stamp; re-measure \
           via `make corpus-run-release` then `make promote-baseline`_\n"
      | Some st ->
        if not st.release_config
        then
          out
            "- **⚠ NON-RELEASE MEASUREMENT:** headline came from a non-release-config \
             binary — NOT promotable (board #69).\n";
        out
          "  - _provenance (#69): build %s, assertions %s, euf_self_check %s%s — %s_\n"
          (let c = st.build_commit in
           if String.length c > 10 then String.sub c 0 10 else c)
          st.assertions
          st.euf_self_check
          (if st.dirty then ", DIRTY tree" else "")
          (if st.release_config then "release-config" else "NON-release")));
  out "\n";
  (* Milestones from TASKS.md *)
  let rows = parse_tasks cfg.tasks in
  let milestones =
    List.filter_map (fun r -> Option.map (fun m -> m, r) (milestone_of_id r.id)) rows
  in
  (* Sort by the numeric suffix so M10 sorts after M2, not between M1 and M2. *)
  let ms_num m =
    try int_of_string (String.sub m 1 (String.length m - 1)) with
    | _ -> max_int
  in
  let ms_names =
    List.sort_uniq (fun a b -> compare (ms_num a) (ms_num b)) (List.map fst milestones)
  in
  let current =
    List.find_opt
      (fun m ->
         let these = List.filter (fun (mm, _) -> mm = m) milestones in
         List.exists (fun (_, r) -> not (is_done r.status)) these)
      ms_names
  in
  (* Three distinct states: no milestone rows parsed at all (TASKS.md unreadable or its
     table shape changed) is "unknown" — NOT "all complete", which would falsely imply the
     project is finished. *)
  let milestone_summary =
    if milestones = []
    then "unknown (no M-milestone rows parsed from TASKS.md — check the board's shape)"
    else (
      match current with
      | Some m -> Printf.sprintf "%s (first milestone with open rows)" m
      | None -> "all parsed milestones complete")
  in
  out "- **Current milestone:** %s\n" milestone_summary;
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
  (* Staleness guard (task #25). If a harness digest is PRESENT, it must validate against
     the live tree before we aggregate it into STATUS.md; otherwise `make status` would
     commit stale (or, worse, silently unchecked) pass/fail numbers. Fail LOUDLY —
     nonzero, BEFORE writing STATUS.md — on any of:
     - the per-dir count block is absent/empty or has a malformed entry (fail CLOSED: a
       future wording change must not silently disarm the guard by parsing to nothing);
     - a recorded per-dir count no longer matches the tree (the stale-count drift). `make
       status` deliberately does NOT re-run the harness (that is `make status-fresh`); the
       separation is what keeps the committed diff meaningful, so the guard refuses rather
       than regenerating. A MISSING digest FILE (or no --harness-digest) is NOT a failure:
       it is honest absence, reported as "n/a" in the body below — never a stale or
       unchecked number. *)
  let digest_file =
    match cfg.harness_digest with
    | Some f when Sys.file_exists f -> Some f
    | _ -> None
  in
  let fail_closed heading details =
    prerr_endline ("status: " ^ heading);
    List.iter prerr_endline details;
    prerr_endline
      (Printf.sprintf
         "  digest: %s"
         (match cfg.harness_digest with
          | Some f -> f
          | None -> "<none>"));
    prerr_endline
      "  run `make status-fresh` to re-run the harness and refresh the digest, then \
       `make status`.";
    exit 1
  in
  (match digest_file with
   | None -> () (* no digest FILE: honest n/a below (behavior unchanged) *)
   | Some _ ->
     (match harness_line with
      | None ->
        fail_closed
          "harness digest present but unparseable — refusing to aggregate."
          [ "  no 'harness:' summary line in the digest (harness crash, or a wording \
             change that would disarm this guard)"
          ]
      | Some l ->
        (match digest_check ~repo:cfg.repo l with
         | Fresh -> ()
         | Unparseable why ->
           fail_closed
             "harness digest present but unparseable — refusing to aggregate."
             [ Printf.sprintf "  %s" why ]
         | Drifted drift ->
           fail_closed
             "STALE harness digest — refusing to aggregate stale pass/fail counts."
             (List.map
                (fun (dir, recorded, actual) ->
                   Printf.sprintf
                     "  %s: digest recorded %d .smt2 file(s), tree now has %d"
                     dir
                     recorded
                     actual)
                drift))));
  out
    "- **Harness (fast regression suite):** %s\n"
    (match harness_line with
     | Some l -> trim l
     | None -> "n/a (no digest captured this run)");
  (* Gate outcomes from the latest gate log PRODUCED BY THIS TREE'S HEAD (task #133). *)
  let gate_pick = pick_gate_log cfg.logs ~head:full_head in
  let gate =
    match gate_pick with
    | Matched p -> parse_gate_log p
    | No_match_foreign _ | No_logs -> None
  in
  (* Loud-absence message shared by the gate and cache lines when there is no gate run at
     this HEAD — never a foreign/stale number. *)
  let gate_absence () =
    match gate_pick with
    | No_logs -> Printf.sprintf "n/a (no gate log found under %s)" cfg.logs
    | No_match_foreign n ->
      Printf.sprintf
        "no gate run at this HEAD (%d gate log(s) exist for other checkouts; run `make \
         gate` in this tree)"
        n
    | Matched _ -> "n/a (gate log present but unreadable)"
  in
  (match gate with
   | None -> out "- **Gate (Lean oracle):** %s\n" (gate_absence ())
   | Some g ->
     let outc =
       String.concat
         ", "
         (List.map (fun (k, v) -> Printf.sprintf "%s %d" k v) g.case_outcomes)
     in
     let count_of k =
       match List.assoc_opt k g.case_outcomes with
       | Some n -> n
       | None -> 0
     in
     (* A REFUTED case = Lean kernel-checked our verdict WRONG (a ship-stopping soundness
        breach, DESIGN.md §8); a honeypot that fails its floor / gets CERTIFIED = the gate
        itself is unaudited. Either turns this into a LOUD leading line — outcome metrics
        must scream a soundness breach, not bury it. *)
     let refuted = count_of "REFUTED" in
     let breach =
       if refuted > 0
       then
         Some
           (Printf.sprintf
              "%d gate case(s) REFUTED — Lean proved our verdict WRONG"
              refuted)
       else if not g.honeypots_ok
       then Some "gate honeypots did not all fire (a honeypot CERTIFIED, or below floor)"
       else None
     in
     (match breach with
      | Some why ->
        out "- **‼ GATE RED — SOUNDNESS BREACH:** %s. Ship-stopping (DESIGN.md §8).\n" why
      | None -> ());
     out
       "- **Gate (Lean oracle):** %d case(s) [%s]; honeypots %d/floor %s %s\n"
       g.cache_total
       (if outc = "" then "none" else outc)
       g.honeypots
       g.honeypot_floor
       (if g.honeypots_ok then "(all fired)" else "(BREACH)");
     (* Trust-tier mix of the CERTIFIED cases (#86 / AP2b): kernel-checked
        (decide/grind/omega) vs compiler-trusted (native_decide, +Lean.ofReduceBool
        axiom). Not a breach either way — CERTIFIED is sound in both tiers — but the mix
        is reported so the compiler-trusted surface is visible, never silently folded into
        the kernel. *)
     out
       "  - certification tiers: %d kernel-checked, %d compiler-trusted (native_decide)%s\n"
       g.cert_kernel
       g.cert_compiler
       (if g.cert_untagged > 0
        then Printf.sprintf ", %d untagged (pre-#86 log)" g.cert_untagged
        else ""));
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
  (* Demoted (task #124): the tests/cases number is now a SUITE-HEALTH sub-metric, not the
     headline — the committed corpus baseline above is the headline. This tracks that the
     small committed regression corpus is being solved, not overall progress. *)
  out
    "- **Suite health (tests/cases regression, by our solver):** %s\n"
    (if total_cases = 0
     then "0% — no solver verdicts in the latest stats run"
     else
       Printf.sprintf
         "%d%% (%d/%d definite sat/unsat)"
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
              (match int_of_string_opt budget with
               | Some bud ->
                 let flag = if n > bud then "OVER" else "ok" in
                 out "| %s | %d | %d | %s |\n" path n bud flag
               | None ->
                 (* A non-numeric budget is a config bug, not a silent 0. Surface it. *)
                 out "| %s | %d | %s | ⚠ non-numeric budget |\n" path n budget)
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
   | None -> out "- Cache hit-rate: %s\n" (gate_absence ())
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
  out
    "_Volatile snapshot: live worktrees/branches change as tasks start and land, so this \
     section legitimately differs run-to-run and its diff is not a regression signal \
     (unlike the outcome metrics above)._\n\n";
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
    (if milestones = []
     then "unknown"
     else (
       match current with
       | Some m -> m
       | None -> "all complete"));
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
    "  corpus baseline: %s\n"
    (match corpus with
     | None -> "n/a (no committed baseline)"
     | Some cs ->
       let tot_scanned = List.fold_left (fun a l -> a + l.scanned) 0 cs.logics in
       let tot_solved = List.fold_left (fun a l -> a + l.solved) 0 cs.logics in
       Printf.sprintf
         "%s%d%% (%d/%d)%s"
         (if cs.mismatch_count > 0
          then Printf.sprintf "MISMATCH x%d! " cs.mismatch_count
          else "")
         (if tot_scanned = 0 then 0 else 100 * tot_solved / tot_scanned)
         tot_solved
         tot_scanned
         (if corpus_stale cs then " [stale]" else ""));
  Printf.printf
    "  suite health (tests/cases): %s\n"
    (if total_cases = 0 then "0%" else Printf.sprintf "%d%%" (100 * solved / total_cases))
;;
