(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sat.mli vox_sat.ml vox_cdcl.mli vox_cdcl.ml sat_cdcl.ml";
 { bytecode; }
 { native; }
*)

let () =
  let open Vox_sat in
  let units = [[Positive 0]; [Negative 0]] in
  ghost_ (
    clause_at_def units 0;
    clause_at_def units 1;
    clause_at_def [[Negative 0]] 0;
    let positive = input_proof units 0 in
    let negative = input_proof units 1 in
    let resolved = resolve_clause 0 [Positive 0] [Negative 0] in
    let learned = resolve_proof units 0 [Positive 0] [Negative 0]
      positive negative in
    (match resolved with
     | [] -> empty_proof_at units learned [false]
     | _ :: _ -> ());
    let empty_formula = [[]] in
    clause_at_def empty_formula 0;
    (match clause_at empty_formula 0 with
     | Some [] ->
       let empty = input_proof empty_formula 0 in
       empty_proof_at empty_formula empty [false]
     | Some (_ :: _) | None -> ());
    ());
  (match original_result units 0, original_result units 1 with
   | Some positive, Some negative ->
     let learned = resolve_result units 0 positive negative in
     (match learned.clause with
      | [] ->
        let database = database_empty units in
        let database = database_cons units learned database in
        (match database_at units database 0 with
         | Some empty ->
           (match empty.clause with
            | [] -> ghost_ (empty_result_at units empty [false])
            | _ :: _ -> assert false)
         | None -> assert false)
      | _ :: _ -> assert false)
   | _ -> assert false);
  let plan =
    [{pivot = 0; source = Original_clause 1; current_positive = true}]
  in
  (match execute_resolution units (database_empty units)
    (Original_clause 0) plan with
   | Some result ->
     (match result.clause with
      | [] -> ghost_ (empty_result_at units result [true])
      | _ :: _ -> assert false)
   | None -> assert false);
  let impossible =
    [ [Positive 0; Positive 1]; [Positive 0; Negative 1];
      [Negative 0; Positive 1]; [Negative 0; Negative 1] ]
  in
  (match Vox_cdcl.solve 10000 2 impossible with
   | Ok ({answer = Vox_cdcl.Unsat proof; statistics} as report) ->
     assert (statistics.learned > 0);
     assert (proof.clause = []);
     ghost_ (Vox_cdcl.unsat_at impossible report [false; false])
   | _ -> assert false);
  let satisfiable = [[Negative 0; Positive 1];
                     [Negative 0; Negative 1]] in
  (match Vox_cdcl.solve 10000 2 satisfiable with
   | Ok {answer = Vox_cdcl.Sat assignment; _} ->
     assert (check 2 satisfiable assignment)
   | _ -> assert false);
  (match Vox_cdcl.solve 10000 2
    [[Positive 1]; [Negative 1]] with
   | Ok {answer = Vox_cdcl.Unsat proof; _} ->
     assert (proof.clause = [])
   | _ -> assert false);
  (match Vox_cdcl.solve 0 2 impossible with
   | Ok {answer = Vox_cdcl.Unknown; _} -> ()
   | _ -> assert false);
  (match Vox_cdcl.solve (-1) 2 impossible with
   | Error Vox_cdcl.Invalid_fuel -> ()
   | _ -> assert false);
  let random = Random.State.make [|42|] in
  let hard = List.init 218 (fun _ ->
    List.init 3 (fun _ ->
      let variable = Random.State.int random 50 in
      if Random.State.bool random then Positive variable
      else Negative variable)) in
  (match Vox_cdcl.solve 1_000_000 50 hard with
   | Ok {answer = Vox_cdcl.Unsat proof; statistics} ->
     assert (statistics.learned > 0);
     assert (statistics.backjumps > 0);
     assert (proof.clause = [])
   | Ok {answer = Vox_cdcl.Unknown; _} -> assert false
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
        match Vox_cdcl.solve 10000 2 formula with
        | Ok {answer = Vox_cdcl.Sat assignment; _} ->
          assert (oracle && check 2 formula assignment)
        | Ok {answer = Vox_cdcl.Unsat proof; _} ->
          assert (not oracle && proof.clause = [])
        | Ok {answer = Vox_cdcl.Unknown; _} | Error _ -> assert false)
        clauses)
      clauses)
    clauses
