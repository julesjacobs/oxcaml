module Sat = Oxsmt_solver.Sat

(* Test-only DIMACS CNF parser. Lives under smt/solver/test and is compiled only into the
   test/bench executables — it is NEVER linked into shipped solver code, the same split
   discipline the SMT-LIB parser follows (DESIGN.md §3). It exists to ingest
   SAT-competition / SATLIB benchmarks and to feed the property tests; it is not on the
   dependency-firewall-critical path.

   Format: comment lines start with 'c'; an optional header line "p cnf V C"; then clauses
   as whitespace-separated nonzero ints, each clause terminated by 0. A clause may span
   lines and a line may hold several clauses. Literal [i] (i>0) is DIMACS variable [i];
   [-i] its negation.

   Strict, so a truncated file is a loud reject rather than a silently-shorter formula
   (which can flip unsat->sat undetected — sat-review item 11, the dubois100 corruption
   shape). When a "p cnf V C" header is present we reject a parsed-clause-count ≠ C, and
   we always reject a nonempty unterminated trailing clause (a run of literals with no
   closing 0). The SATLIB "%"-footer early-stop is kept: a "%" line ends the formula, and
   its trailing lone "0" is never read (so it is not a phantom empty clause) — see
   corpora/SAT/README.md. *)

exception Parse_error of string

type problem =
  { num_vars : int (* max variable index that appears (or the header's V) *)
  ; clauses : int list list (* DIMACS literals, per clause *)
  }

(* [source] is a label (a path, usually) used only in error messages. *)
let parse_channel ?(source = "<channel>") ic =
  let header_vars = ref 0 in
  let header_clauses =
    ref None
    (* Some C when a "p cnf V C" header was seen *)
  in
  let max_var = ref 0 in
  let clauses = ref [] in
  let n_clauses = ref 0 in
  let cur = ref [] in
  let fail fmt = Printf.ksprintf (fun m -> raise (Parse_error (source ^ ": " ^ m))) fmt in
  (try
     while true do
       let line = input_line ic in
       let n = String.length line in
       (* first non-space char *)
       let i = ref 0 in
       while !i < n && (line.[!i] = ' ' || line.[!i] = '\t' || line.[!i] = '\r') do
         incr i
       done;
       if !i >= n
       then () (* blank *)
       else if line.[!i] = 'c'
       then () (* comment *)
       else if line.[!i] = '%'
       then raise End_of_file (* SATLIB end-of-formula marker *)
       else if line.[!i] = 'p'
       then (
         (* "p cnf V C" — record V and C. *)
         match String.split_on_char ' ' (String.trim line) |> List.filter (( <> ) "") with
         | "p" :: "cnf" :: v :: c :: _ ->
           header_vars := int_of_string v;
           header_clauses := Some (int_of_string c)
         | "p" :: "cnf" :: v :: _ -> header_vars := int_of_string v
         | _ -> ())
       else
         (* literal line *)
         String.split_on_char ' ' line
         |> List.concat_map (String.split_on_char '\t')
         |> List.iter (fun tok ->
           let tok = String.trim tok in
           if tok <> ""
           then (
             let l = int_of_string tok in
             if l = 0
             then (
               clauses := List.rev !cur :: !clauses;
               incr n_clauses;
               cur := [])
             else (
               if abs l > !max_var then max_var := abs l;
               cur := l :: !cur)))
     done
   with
   | End_of_file -> ());
  if !cur <> []
  then
    fail
      "unterminated trailing clause (%d literals with no closing 0) — file is likely \
       truncated"
      (List.length !cur);
  (match !header_clauses with
   | Some c when c <> !n_clauses ->
     fail
       "clause-count mismatch vs header: expected %d, got %d — file is likely truncated \
        or corrupt"
       c
       !n_clauses
   | _ -> ());
  { num_vars = max !header_vars !max_var; clauses = List.rev !clauses }
;;

let parse_file path =
  let ic = open_in path in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> parse_channel ~source:path ic)
;;

(* DIMACS literal (±v, 1-based) -> Sat literal (0-based var). *)
let to_lit l = if l > 0 then Sat.pos (l - 1) else Sat.neg (-l - 1)

(* Load a problem into a fresh solver. *)
let to_sat { num_vars; clauses } =
  let s = Sat.create () in
  for _ = 1 to num_vars do
    ignore (Sat.new_var s : Sat.var)
  done;
  List.iter (fun cl -> Sat.add_clause s (List.map to_lit cl)) clauses;
  s
;;
