(* Perf-bench runner for the adversarial performance corpus (DESIGN.md §8.4).

   Usage: perf_bench <solver-exe> <cases-dir> [--log FILE]

   Runs the solver CLI (the harness SOLVER contract) on each cases-dir/*.smt2 (sorted,
   deterministic order) and prints a per-case [{ verdict, counters, wall_ms }] table to
   stdout, plus a full log to --log. This is a VISIBILITY tool, not a gate (DESIGN §8:
   performance is surfaced, not gated) — so it always exits 0 on a clean run, and its
   stdout is NOT a committed golden (wall-clock is allowed here, unlike §I5 goldens).

   Reuses harness_lib to spawn the solver and parse its (result ...) blocks. *)

open Harness_lib

let walk_cnf dir =
  let entries =
    try Sys.readdir dir with
    | Sys_error _ -> [||]
  in
  Array.sort compare entries;
  Array.to_list entries
  |> List.filter (fun n -> Filename.check_suffix n ".smt2")
  |> List.map (Filename.concat dir)
;;

type row =
  { name : string
  ; verdict : string
  ; conflicts : int
  ; decisions : int
  ; propagations : int
  ; wall_ms : float
  }

(* Summarise a solver_output (one goal_result per check-sat): a single verdict when all
   goals agree ("unknown x5" when repeated), and summed counters. *)
let summarize goals =
  let n = List.length goals in
  let vs =
    List.sort_uniq
      compare
      (List.map (fun g -> Harness.verdict_to_string g.Harness.verdict) goals)
  in
  let verdict =
    match vs with
    | [] -> "no-goals"
    | [ one ] -> if n <= 1 then one else Printf.sprintf "%s x%d" one n
    | many -> String.concat "," many
  in
  let sum sel = List.fold_left (fun a g -> a + sel g.Harness.counters) 0 goals in
  ( verdict
  , sum (fun c -> c.Harness.conflicts)
  , sum (fun c -> c.Harness.decisions)
  , sum (fun c -> c.Harness.propagations) )
;;

let () =
  let args = Array.to_list Sys.argv in
  let solver = ref ""
  and dir = ref ""
  and log = ref "" in
  let rec parse = function
    | [] -> ()
    | "--log" :: f :: rest ->
      log := f;
      parse rest
    | a :: rest ->
      if !solver = "" then solver := a else if !dir = "" then dir := a;
      parse rest
  in
  parse (List.tl args);
  if !solver = "" || !dir = ""
  then (
    prerr_endline "usage: perf_bench <solver-exe> <cases-dir> [--log FILE]";
    exit 2);
  let files = walk_cnf !dir in
  if files = []
  then (
    Printf.printf "perf-bench: no *.smt2 under %s — nothing to run.\n" !dir;
    exit 0);
  let rows =
    List.map
      (fun path ->
         let t0 = Unix.gettimeofday () in
         let res, _errs = Harness.run_solver !solver path in
         let wall_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
         let verdict, conflicts, decisions, propagations =
           match res with
           | Ok goals -> summarize goals
           | Error m -> "ERROR:" ^ m, 0, 0, 0
         in
         { name = Filename.basename path
         ; verdict
         ; conflicts
         ; decisions
         ; propagations
         ; wall_ms
         })
      files
  in
  (* Full log (uncommitted; wall-clock allowed). *)
  if !log <> ""
  then (
    (try Unix.mkdir (Filename.dirname !log) 0o755 with
     | Unix.Unix_error (Unix.EEXIST, _, _) | Unix.Unix_error (Unix.ENOENT, _, _) -> ());
    let oc = open_out !log in
    List.iter
      (fun r ->
         Printf.fprintf
           oc
           "%s\t%s\tconflicts=%d decisions=%d propagations=%d wall_ms=%.1f\n"
           r.name
           r.verdict
           r.conflicts
           r.decisions
           r.propagations
           r.wall_ms)
      rows;
    close_out oc);
  (* Digest table to stdout. *)
  let namew = List.fold_left (fun w r -> max w (String.length r.name)) 4 rows in
  Printf.printf
    "%-*s  %-12s  %9s  %9s  %12s  %9s\n"
    namew
    "case"
    "verdict"
    "conflicts"
    "decisions"
    "propagations"
    "wall_ms";
  List.iter
    (fun r ->
       Printf.printf
         "%-*s  %-12s  %9d  %9d  %12d  %9.1f\n"
         namew
         r.name
         r.verdict
         r.conflicts
         r.decisions
         r.propagations
         r.wall_ms)
    rows;
  let slowest = List.sort (fun a b -> compare b.wall_ms a.wall_ms) rows in
  (match slowest with
   | top :: _ ->
     Printf.printf
       "\nslowest: %s at %.1f ms (%d cases total)\n"
       top.name
       top.wall_ms
       (List.length rows)
   | [] -> ());
  if !log <> "" then Printf.printf "full log: %s\n" !log
;;
