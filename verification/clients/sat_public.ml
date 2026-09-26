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
