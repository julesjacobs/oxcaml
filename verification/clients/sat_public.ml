open Vox_sat_spec

let (decide @ total) :
    (n : {n : int | 0 <= n && n <= 256}) ->
    (formula : {f : formula |
      valid_formula n f && clauses_fit 4096 f && literals_fit 65536 f}) ->
    (candidate : bool list) ->
    {r : (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result |
      match r with
      | Error _ -> false
      | Ok report -> match report.answer with
        | Vox_cdcl_total.Sat assignment ->
          well_sized n assignment && eval_formula assignment formula
        | Vox_cdcl_total.Unsat -> not (eval_formula candidate formula)
        | Vox_cdcl_total.Unknown -> false} =
  fun n formula candidate ->
  ghost_ (classify_input_def n formula);
  let result = Vox_cdcl_total.solve_with_fallback 0 (n + 1) n formula in
  (match result with
   | Error _ -> ()
   | Ok report -> match report.answer with
     | Vox_cdcl_total.Sat assignment ->
       ghost_ (check_def n formula assignment)
     | Vox_cdcl_total.Unsat ->
       ghost_ (Vox_sat.unsat_at n formula candidate)
     | Vox_cdcl_total.Unknown -> ());
  result

let (decide_cdcl @ total) :
    (n : {n : int | 0 <= n && n <= 256}) ->
    (formula : {f : formula |
      valid_formula n f && clauses_fit 4096 f && literals_fit 65536 f}) ->
    (candidate : bool list) ->
    {r : (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result |
      match r with
      | Error _ -> false
      | Ok report -> match report.answer with
        | Vox_cdcl_total.Sat assignment ->
          well_sized n assignment && eval_formula assignment formula
        | Vox_cdcl_total.Unsat -> not (eval_formula candidate formula)
        | Vox_cdcl_total.Unknown -> false} =
  fun n formula candidate ->
  ghost_ (classify_input_def n formula);
  let result = Vox_cdcl_total.solve_complete n formula in
  (match result with
   | Error _ -> ()
   | Ok report -> match report.answer with
     | Vox_cdcl_total.Sat assignment ->
       ghost_ (check_def n formula assignment)
     | Vox_cdcl_total.Unsat ->
       ghost_ (Vox_sat.unsat_at n formula candidate)
     | Vox_cdcl_total.Unknown -> ());
  result

let (input_classification @ total) : (n : int) -> (formula : formula) ->
    {u : unit | match classify_input n formula with
      | Some Unsupported_variable_count -> n < 0 || n > 256
      | Some Too_many_clauses ->
        0 <= n && n <= 256 && not (clauses_fit 4096 formula)
      | Some Too_many_literals ->
        0 <= n && n <= 256 && clauses_fit 4096 formula
        && not (literals_fit 65536 formula)
      | Some Invalid_formula ->
        0 <= n && n <= 256 && clauses_fit 4096 formula
        && literals_fit 65536 formula && not (valid_formula n formula)
      | None ->
        0 <= n && n <= 256 && clauses_fit 4096 formula
        && literals_fit 65536 formula && valid_formula n formula} =
  fun n formula -> classify_input_def n formula

let (unknown_exhausts_fuel @ total) :
    (fuel : int) -> (n : int) -> (formula : formula) ->
    {r : (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result |
      match r with
      | Error _ -> true
      | Ok report -> match report.answer with
        | Vox_cdcl_total.Unknown -> report.statistics.steps = fuel
        | Vox_cdcl_total.Sat _ | Vox_cdcl_total.Unsat -> true} =
  fun fuel n formula -> Vox_cdcl_total.solve fuel n formula

let () =
  let impossible = [[Positive 0]; [Negative 0]] in
  (match Vox_cdcl_total.solve_with_fallback 0 2 1 impossible with
   | Ok {answer = Vox_cdcl_total.Unsat; _} ->
     ghost_ (Vox_sat.unsat_at 1 impossible []);
     ghost_ (Vox_sat.unsat_at 1 impossible [true; false])
   | _ -> assert false);
  (match Vox_sat.solve 10 1 [[Positive 0]] with
   | Ok {answer = Vox_sat.Sat [true]; _} -> ()
   | _ -> assert false);
  (match Vox_cdcl.solve 100 1 impossible with
   | Ok {answer = Vox_cdcl.Unsat; _} ->
     ghost_ (Vox_sat.unsat_at 1 impossible [false])
   | _ -> assert false);
  ()

let () =
  let impossible = [[Positive 0]; [Negative 0]] in
  ghost_ (
    valid_literal_def 1 (Positive 0);
    valid_literal_def 1 (Negative 0);
    valid_clause_def 1 [];
    valid_clause_def 1 [Positive 0];
    valid_clause_def 1 [Negative 0];
    valid_formula_def 1 [];
    valid_formula_def 1 [[Negative 0]];
    valid_formula_def 1 impossible;
    clauses_fit_def 4094 [];
    clauses_fit_def 4095 [[Negative 0]];
    clauses_fit_def 4096 impossible;
    consume_literals_def 65534 [];
    consume_literals_def 65535 [Negative 0];
    consume_literals_def 65535 [];
    consume_literals_def 65536 [Positive 0];
    literals_fit_def 65534 [];
    literals_fit_def 65535 [[Negative 0]];
    literals_fit_def 65536 impossible);
  match decide_cdcl 1 impossible [false] with
  | Ok {answer = Vox_cdcl_total.Unsat; _} -> ()
  | _ -> assert false
