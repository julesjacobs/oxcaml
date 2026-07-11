module Sat = Oxsmt_solver.Sat
module Dimacs = Oxsmt_dimacs.Dimacs

(* SAT benchmark runner over a DIMACS corpus (TASKS.md M1-sat, DESIGN.md §8).

   Usage: sat_bench <corpus-dir> [--log FILE]

   Globs <corpus-dir>/**/*.cnf at runtime (sorted, deterministic). Tolerates an absent or
   empty corpus with a clear message and exit 0 — the SAT corpus is fetched out-of-band
   and may not be present. For families whose name encodes the verdict (SATLIB: uf* = sat,
   uuf* = unsat) the result is label-checked; a mismatch is a failure. Every sat verdict
   is additionally self-checked by evaluating the model. A digest (counts, failures,
   slowest-by-conflicts) prints to stdout; the full per-file log goes to --log if given
   (context-frugal, §11).

   Deterministic: the "slowest" ranking is by conflict count, never wall-clock. *)

let rec walk dir acc =
  let entries =
    try Sys.readdir dir with
    | Sys_error _ -> [||]
  in
  Array.sort compare entries;
  Array.fold_left
    (fun acc name ->
       let p = Filename.concat dir name in
       if
         try Sys.is_directory p with
         | Sys_error _ -> false
       then walk p acc
       else if Filename.check_suffix p ".cnf"
       then p :: acc
       else acc)
    acc
    entries
;;

(* SATLIB naming: "uuf*" is unsat, "uf*" is sat. Returns the expected verdict, or None
   when the family does not encode it. *)
let label_of path =
  let base = Filename.basename path in
  let has sub =
    let ls = String.length sub
    and lb = String.length base in
    let rec go i = i + ls <= lb && (String.sub base i ls = sub || go (i + 1)) in
    go 0
  in
  if has "uuf" then Some false else if has "uf" then Some true else None
;;

let model_satisfies clauses model =
  List.for_all
    (fun cl ->
       List.exists
         (fun l ->
            let b = model.(abs l - 1) in
            if l > 0 then b else not b)
         cl)
    clauses
;;

type outcome =
  { path : string
  ; verdict : string
  ; conflicts : int
  ; decisions : int
  ; propagations : int
  ; failure : string option
  }

let () =
  let dirs = ref [] in
  let log = ref "" in
  let rec parse = function
    | [] -> ()
    | "--log" :: f :: rest ->
      log := f;
      parse rest
    | d :: rest ->
      dirs := d :: !dirs;
      parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  let dirs = List.rev !dirs in
  if dirs = []
  then (
    prerr_endline "usage: sat_bench <corpus-dir>... [--log FILE]";
    exit 2);
  let files = List.sort compare (List.concat_map (fun d -> walk d []) dirs) in
  if files = []
  then (
    Printf.printf
      "sat-bench: no corpus under %s (no *.cnf found) — nothing to run.\n"
      (String.concat ", " dirs);
    exit 0);
  let outcomes = ref [] in
  let n_sat = ref 0
  and n_unsat = ref 0
  and n_fail = ref 0 in
  List.iter
    (fun path ->
       let o =
         match Dimacs.parse_file path with
         | exception e ->
           incr n_fail;
           { path
           ; verdict = "parse-error"
           ; conflicts = 0
           ; decisions = 0
           ; propagations = 0
           ; failure = Some (Printexc.to_string e)
           }
         | problem ->
           let s = Dimacs.to_sat problem in
           let r = Sat.solve s in
           let st = Sat.stats s in
           let verdict =
             match r with
             | Sat.Sat -> "sat"
             | Sat.Unsat -> "unsat"
           in
           (match r with
            | Sat.Sat -> incr n_sat
            | Sat.Unsat -> incr n_unsat);
           let failure =
             match r with
             | Sat.Sat when not (model_satisfies problem.Dimacs.clauses (Sat.model s)) ->
               Some "model does not satisfy the formula"
             | _ ->
               (match label_of path, r with
                | Some true, Sat.Unsat -> Some "label says sat, got unsat"
                | Some false, Sat.Sat -> Some "label says unsat, got sat"
                | _ -> None)
           in
           if failure <> None then incr n_fail;
           { path
           ; verdict
           ; conflicts = st.Sat.Stats.conflicts
           ; decisions = st.Sat.Stats.decisions
           ; propagations = st.Sat.Stats.propagations
           ; failure
           }
       in
       outcomes := o :: !outcomes)
    files;
  let outcomes = List.rev !outcomes in
  (* Full log. *)
  if !log <> ""
  then (
    let oc = open_out !log in
    List.iter
      (fun o ->
         Printf.fprintf
           oc
           "%s\t%s\tconflicts=%d decisions=%d propagations=%d%s\n"
           o.path
           o.verdict
           o.conflicts
           o.decisions
           o.propagations
           (match o.failure with
            | Some m -> "\tFAIL: " ^ m
            | None -> ""))
      outcomes;
    close_out oc);
  (* Digest. *)
  let total = List.length outcomes in
  Printf.printf
    "sat-bench: %d files | %d sat | %d unsat | %d failures\n"
    total
    !n_sat
    !n_unsat
    !n_fail;
  let failures = List.filter (fun o -> o.failure <> None) outcomes in
  if failures <> []
  then (
    Printf.printf "first failures:\n";
    List.iteri
      (fun i o ->
         if i < 10
         then
           Printf.printf
             "  %s\t%s\t%s\n"
             o.path
             o.verdict
             (match o.failure with
              | Some m -> m
              | None -> ""))
      failures);
  let slowest = List.sort (fun a b -> compare b.conflicts a.conflicts) outcomes in
  Printf.printf "slowest by conflicts:\n";
  List.iteri
    (fun i o ->
       if i < 5 then Printf.printf "  %s\t%s\tconflicts=%d\n" o.path o.verdict o.conflicts)
    slowest;
  if !log <> "" then Printf.printf "full log: %s\n" !log;
  if !n_fail > 0 then exit 1
;;
