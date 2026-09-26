(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sat_spec.mli vox_sat_spec.ml vox_sat_proof.mli vox_sat_proof.ml vox_sat.mli vox_sat.ml sat_kernel.ml";
 { bytecode; }
*)

open Vox_sat_spec

open Vox_sat_spec

let () =
  let open Vox_sat_proof in
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
  ()
