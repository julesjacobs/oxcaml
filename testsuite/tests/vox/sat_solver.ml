(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml vox_sat.mli vox_sat.ml sat_solver.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Vox_sat_spec

let () =
  let open Vox_sat in
  let formula =
    [ [Positive 0; Positive 1]; [Negative 0; Positive 1];
      [Positive 0; Negative 1] ]
  in
  assert (check 2 formula [true; true]);
  assert (not (check 2 formula [false; false]));
  (match solve 30 2 formula with
   | Error _ -> assert false
   | Ok result ->
     match result.answer with
     | Sat assignment -> assert (check 2 formula assignment)
     | Unsat | Unknown -> assert false);
  let impossible =
    [ [Positive 0; Positive 1]; [Positive 0; Negative 1];
      [Negative 0; Positive 1]; [Negative 0; Negative 1] ]
  in
  assert (valid_formula 2 impossible);
  (match solve 50 2 impossible with
   | Error _ -> assert false
   | Ok result ->
     match result.answer with
     | Unsat -> ghost_ (unsat_at 2 impossible [false; true])
     | Sat _ | Unknown -> assert false);
  (match solve 0 2 impossible with
   | Error _ -> assert false
   | Ok result ->
     match result.answer with
     | Unknown -> ()
     | Sat _ | Unsat -> assert false);
  (match solve 1 0 [[]] with
   | Error _ -> assert false
   | Ok result ->
     match result.answer with
     | Unsat -> ()
     | Sat _ | Unknown -> assert false);
  (match solve 1 256 [] with
   | Error _ -> assert false
   | Ok result ->
     match result.answer with
     | Sat assignment -> assert (check 256 [] assignment)
     | Unsat | Unknown -> assert false);
  (match solve 1 2 [[Positive 2]] with
   | Error Invalid_formula -> ()
   | Error (Unsupported_variable_count | Too_many_clauses | Too_many_literals)
   | Ok _ -> assert false);
  (match solve 1 257 [] with
   | Error Unsupported_variable_count -> ()
   | Error (Invalid_formula | Too_many_clauses | Too_many_literals)
   | Ok _ -> assert false);
  (match solve 2 1 [[Negative 0]] with
   | Ok {answer = Sat [false]; _} -> ()
   | Error _ | Ok _ -> assert false);
  (match solve 2 1 [[Positive 0]; [Negative 0]] with
   | Ok {answer = Unsat; _} -> ()
   | Error _ | Ok _ -> assert false);
  (match solve 2 2 [[Positive 1]; [Negative 1]] with
   | Ok {answer = Unsat; _} -> ()
   | Error _ | Ok _ -> assert false);
  (match solve 1 1 (List.init 4097 (fun _ -> [])) with
   | Error Too_many_clauses -> ()
   | Error _ | Ok _ -> assert false);
  (match solve 1 1 [List.init 65537 (fun _ -> Positive 0)] with
   | Error Too_many_literals -> ()
   | Error _ | Ok _ -> assert false);
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
        let oracle = List.exists (fun assignment -> check 2 formula assignment)
          assignments in
        match solve 30 2 formula with
        | Error _ -> assert false
        | Ok {answer = Sat assignment; _} ->
          assert (oracle && check 2 formula assignment)
        | Ok {answer = Unsat; _} -> assert (not oracle)
        | Ok {answer = Unknown; _} -> assert false)
        clauses)
      clauses)
    clauses
