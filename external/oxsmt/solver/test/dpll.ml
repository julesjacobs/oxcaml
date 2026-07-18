(* A deliberately naive, independent SAT oracle: DPLL-style chronological backtracking
   search with conflict pruning (no watched literals, no learning, no heuristics). Written
   from the definition of satisfiability so that it shares as little as possible with the
   CDCL core it cross-checks (DESIGN.md §8: an independent oracle until benchmarks
   arrive). Test-only.

   Works directly on DIMACS literals (±v, 1-based). Correct — not fast — and used only on
   formulas with a handful of variables. *)

let solve num_vars clauses =
  (* assign.(v): 0 unknown, 1 true, -1 false; index 0 unused (vars are 1-based). *)
  let assign = Array.make (num_vars + 1) 0 in
  (* A literal is false iff its variable is assigned to the opposite polarity. *)
  let lit_false l =
    let a = assign.(abs l) in
    a <> 0 && l > 0 <> (a = 1)
  in
  let clause_all_false cl = List.for_all lit_false cl in
  let some_clause_falsified () = List.exists clause_all_false clauses in
  let rec go v =
    if some_clause_falsified ()
    then false
    else if v > num_vars
    then true (* every clause has a satisfied literal *)
    else (
      assign.(v) <- 1;
      if go (v + 1)
      then true
      else (
        assign.(v) <- -1;
        let r = go (v + 1) in
        if not r then assign.(v) <- 0;
        r))
  in
  go 1
;;
