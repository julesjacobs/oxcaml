(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml vox_sat.mli vox_sat.ml vox_cdcl_total_proof.mli vox_cdcl_total_proof.ml vox_cdcl_total.mli vox_cdcl_total.ml";
 { bytecode; }
*)

open Vox_sat_spec

let (unknown_exhausts_fuel @ total) :
    (fuel : int) -> (n : int) -> (formula : formula) ->
    {r : (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result |
      match r with
      | Error _ -> true
      | Ok report -> match report.answer with
        | Vox_cdcl_total.Unknown -> report.statistics.steps = fuel
        | Vox_cdcl_total.Sat _ | Vox_cdcl_total.Unsat -> true} =
  fun fuel n formula -> Vox_cdcl_total.solve fuel n formula

let (complete_cdcl @ total) :
    (n : {n : int | 0 <= n && n <= 256}) ->
    (formula : {f : formula | valid_formula n f && clauses_fit 4096 f
      && literals_fit 65536 f}) ->
    {r : (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result |
      match r with
      | Error _ -> false
      | Ok report -> match report.answer with
        | Vox_cdcl_total.Unknown -> false
        | Vox_cdcl_total.Sat _ | Vox_cdcl_total.Unsat -> true} =
  fun n formula ->
  ghost_ (classify_input_def n formula);
  Vox_cdcl_total.solve_complete n formula

let () =
  let open Vox_sat in
  let impossible =
    [ [Positive 0; Positive 1]; [Positive 0; Negative 1];
      [Negative 0; Positive 1]; [Negative 0; Negative 1] ]
  in
  (match Vox_cdcl_total.solve 10000 2 impossible with
   | Ok {answer = Vox_cdcl_total.Unsat; statistics} ->
     assert (statistics.learned > 0);
     ghost_ (Vox_sat.unsat_at 2 impossible [false; false])
   | _ -> assert false);
  (match Vox_cdcl_total.solve_complete 2 impossible with
   | Ok {answer = Vox_cdcl_total.Unsat; _} ->
     ghost_ (Vox_sat.unsat_at 2 impossible []);
     ghost_ (Vox_sat.unsat_at 2 impossible [true; false; true])
   | _ -> assert false);
  let chain = [Positive 0] :: List.init 255 (fun v ->
    [Negative v; Positive (v + 1)]) in
  (match Vox_cdcl_total.solve 1 256 chain with
   | Ok {answer = Vox_cdcl_total.Sat assignment; statistics} ->
     assert (check 256 chain assignment);
     assert (statistics.steps = 1 && statistics.decisions = 0)
   | _ -> assert false);
  (match Vox_cdcl_total.solve 1 256 (chain @ [[Negative 255]]) with
   | Ok {answer = Vox_cdcl_total.Unsat; statistics} ->
     assert (statistics.steps = 1 && statistics.conflicts = 1);
     assert (statistics.decisions = 0)
   | _ -> assert false);
  List.iter (fun formula ->
    match Vox_cdcl_total.solve_complete 0 formula with
    | Ok {answer = Vox_cdcl_total.Sat assignment; _} ->
      assert (formula = [] && assignment = [])
    | Ok {answer = Vox_cdcl_total.Unsat; _} -> assert (formula = [[]])
    | _ -> assert false) [[]; [[]]];
  List.iter (fun (n, formula, expected) ->
    match Vox_cdcl_total.solve_complete n formula with
    | Error (Vox_cdcl_total.Invalid_input error) -> assert (error = expected)
    | _ -> assert false)
    [257, [], Unsupported_variable_count;
     2, [[Positive 2]], Invalid_formula;
     2, List.init 4097 (fun _ -> []), Too_many_clauses;
     2, [List.init 65537 (fun _ -> Positive 0)], Too_many_literals];
  let satisfiable = [[Negative 0; Positive 1];
                     [Negative 0; Negative 1]] in
  (match Vox_cdcl_total.solve 10000 2 satisfiable with
   | Ok {answer = Vox_cdcl_total.Sat assignment; _} ->
     assert (check 2 satisfiable assignment)
   | _ -> assert false);
  (match Vox_cdcl_total.solve 10000 2
    [[Positive 1]; [Negative 1]] with
   | Ok {answer = Vox_cdcl_total.Unsat; _} ->
     ()
   | _ -> assert false);
  (match Vox_cdcl_total.solve 0 2 impossible with
   | Ok {answer = Vox_cdcl_total.Unknown; _} -> ()
   | _ -> assert false);
  (match Vox_cdcl_total.solve (-1) 2 impossible with
   | Error Vox_cdcl_total.Invalid_fuel -> ()
   | _ -> assert false);
  let random = Random.State.make [|42|] in
  let hard = List.init 218 (fun _ ->
    List.init 3 (fun _ ->
      let variable = Random.State.int random 50 in
      if Random.State.bool random then Positive variable
      else Negative variable)) in
  (match Vox_cdcl_total.solve 1_000_000 50 hard with
   | Ok {answer = Vox_cdcl_total.Unsat; statistics} ->
     assert (statistics.learned > 0);
     assert (statistics.backjumps > 0);
     ()
   | _ -> assert false);
  let clauses =
    [ []; [Positive 0]; [Negative 0]; [Positive 1]; [Negative 1];
      [Positive 0; Positive 1]; [Positive 0; Negative 1];
      [Negative 0; Positive 1]; [Negative 0; Negative 1] ]
  in
  let assignments =
    [[false; false]; [false; true]; [true; false]; [true; true]]
  in
  List.iter (fun first ->
    List.iter (fun second ->
      List.iter (fun third ->
        let formula = [first; second; third] in
        let oracle = List.exists (check 2 formula) assignments in
        List.iter (fun (result :
          (Vox_cdcl_total.report, Vox_cdcl_total.input_error) result) ->
          match result with
          | Ok {answer = Vox_cdcl_total.Sat assignment; _} ->
            assert (oracle && check 2 formula assignment)
          | Ok {answer = Vox_cdcl_total.Unsat; _} ->
            assert (not oracle)
          | Ok {answer = Vox_cdcl_total.Unknown; _} | Error _ -> assert false)
          [Vox_cdcl_total.solve_complete 2 formula;
           Vox_cdcl_total.solve 10000 2 formula])
        clauses)
      clauses)
    clauses

let () =
  let many_invalid = List.init 4097 (fun _ -> [Positive 256]) in
  let long_invalid = [List.init 65537 (fun _ -> Positive 256)] in
  List.iter (fun (n, formula, expected) ->
    assert (classify_input n formula = Some expected);
    List.iter (fun result ->
      match result with
      | Error (Vox_cdcl_total.Invalid_input error) -> assert (error = expected)
      | _ -> assert false)
      [Vox_cdcl_total.solve_complete n formula;
       Vox_cdcl_total.solve 0 n formula])
    [257, many_invalid, Unsupported_variable_count;
     256, many_invalid, Too_many_clauses;
     256, long_invalid, Too_many_literals;
     256, [[Positive 256]], Invalid_formula];
  (match Vox_cdcl_total.solve (-1) 257 many_invalid with
   | Error Vox_cdcl_total.Invalid_fuel -> ()
   | _ -> assert false)

let () =
  (match Vox_cdcl_total.solve 1 1 [[Negative 0; Negative 0]] with
   | Ok {answer = Vox_cdcl_total.Sat [false]; statistics} ->
     assert (statistics.steps = 1 && statistics.decisions = 0)
   | _ -> assert false);
  (match Vox_cdcl_total.solve 1 1
      [[Positive 0; Positive 0]; [Negative 0; Negative 0]] with
   | Ok {answer = Vox_cdcl_total.Unsat; statistics} ->
     assert (statistics.steps = 1 && statistics.decisions = 0)
   | _ -> assert false)

let () =
  (match Vox_cdcl_total.solve_complete 256 [] with
   | Ok {answer = Vox_cdcl_total.Sat assignment; statistics} ->
     assert (List.length assignment = 256 && statistics.decisions = 256)
   | _ -> assert false);
  (match Vox_cdcl_total.solve_complete 0 [[]] with
   | Ok {answer = Vox_cdcl_total.Unsat; _} -> ()
   | _ -> assert false);
  (match Vox_cdcl_total.solve_complete 1
      [[Positive 0; Positive 0]; [Negative 0; Negative 0]] with
   | Ok {answer = Vox_cdcl_total.Unsat; statistics} ->
     assert (statistics.steps = 1 && statistics.decisions = 0)
   | _ -> assert false)
