(* Checks the SAT solver's emitted code for proof code; see sat_boundary.ml.
   Arguments: the library's build directory, then, for native code, the
   object of Vox_cdcl_total_proof. *)
open Emitted_code

let () =
  let dir = Sys.argv.(1) in
  let proofs =
    [ "empty_unsatisfiable"; "semantic_unsat_at"; "exhaustive_result";
      "rejects_extensions"; "derivation_valid"; "classify_input" ]
  in
  List.iter
    (fun (unit, calls) ->
      let lambda = read (Filename.concat dir (unit ^ ".lambda")) in
      check
        (not (List.exists (fun name -> occurs name lambda) proofs))
        (unit ^ " names no proof function");
      check (applications lambda = calls)
        (Printf.sprintf "%s makes %d calls" unit calls))
    [ "vox_sat", 1; "vox_cdcl", 1; "vox_cdcl_total", 6 ];
  if Array.length Sys.argv > 2 then begin
    let obj = read Sys.argv.(2) in
    let measures =
      [ "clause_rank"; "resolve_rank"; "earlier_clause_rank";
        "variable_source_member"; "prefix_"; "no_current_false";
        "resolve_preserves_current"; "clause_universe"; "literal_universe";
        "count_absent"; "progress_measure"; "progress_learning";
        "progress_decision"; "trail_levels"; "decision_level_bound" ]
    in
    List.iter
      (fun name ->
        check
          (not (occurs ("Vox_cdcl_total_proof__" ^ name) obj))
          ("no symbol for " ^ name ^ " in the proof module's object"))
      measures;
    check (occurs "Vox_cdcl_total_proof" obj)
      "the proof module's object has symbols"
  end;
  finish ()
