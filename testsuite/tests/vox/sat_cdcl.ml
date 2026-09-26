(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml vox_sat.mli vox_sat.ml vox_cdcl_proof.mli vox_cdcl_proof.ml vox_cdcl.mli vox_cdcl.ml sat_cdcl.ml";
 { bytecode; }
*)

open Vox_sat_spec

let () =
  let open Vox_sat in
  let impossible =
    [ [Positive 0; Positive 1]; [Positive 0; Negative 1];
      [Negative 0; Positive 1]; [Negative 0; Negative 1] ]
  in
  (match Vox_cdcl.solve 10000 2 impossible with
   | Ok {answer = Vox_cdcl.Unsat; statistics} ->
     assert (statistics.learned > 0);
     ghost_ (Vox_sat.unsat_at 2 impossible [false; false])
   | _ -> assert false);
  let satisfiable = [[Negative 0; Positive 1];
                     [Negative 0; Negative 1]] in
  (match Vox_cdcl.solve 10000 2 satisfiable with
   | Ok {answer = Vox_cdcl.Sat assignment; _} ->
     assert (check 2 satisfiable assignment)
   | _ -> assert false);
  (match Vox_cdcl.solve 10000 2
    [[Positive 1]; [Negative 1]] with
   | Ok {answer = Vox_cdcl.Unsat; _} ->
     ()
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
   | Ok {answer = Vox_cdcl.Unsat; statistics} ->
     assert (statistics.learned > 0);
     assert (statistics.backjumps > 0);
     ()
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
        | Ok {answer = Vox_cdcl.Unsat; _} ->
          assert (not oracle)
        | Ok {answer = Vox_cdcl.Unknown; _} | Error _ -> assert false)
        clauses)
      clauses)
    clauses

let () =
  (match Vox_cdcl.solve 100 1 [[Negative 0; Negative 0]] with
   | Ok {answer = Vox_cdcl.Sat [false]; statistics} ->
     assert (statistics.decisions = 0)
   | _ -> assert false)
