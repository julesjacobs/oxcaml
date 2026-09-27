(* Checks the Lambda of the DFA and regex units; see dfa_boundary.ml.
   Argument: the directory of the dumps. *)
open Emitted_code

let dump unit = read (Filename.concat Sys.argv.(1) (unit ^ ".lambda"))

let () =
  let semantics = dump "dfa_semantics" in
  let state_size = function_body semantics "state_size" in
  check
    (direct_calls state_size = [ "big_length" ]
     && not (occurs "makeblock" state_size))
    "state_size counts rows without constructing state IDs";
  let proof = dump "dfa_equivalence_proof" in
  let certificates =
    [ "append_word"; "quotient_relation"; "quotient_access"; "cover_row";
      "cover_rows"; "same_class_pairs"; "compare_states";
      "distinguish_classes"; "minimization_certificate" ]
  in
  List.iter
    (fun entry ->
      let reached = reachable proof entry in
      check
        (function_bodies proof entry <> []
         && not (List.exists (fun f -> List.mem f certificates) reached))
        (entry ^ " reaches no certificate-building function"))
    [ "compare"; "reduce" ];
  List.iter
    (fun (unit, names) ->
      let text = dump unit in
      List.iter
        (fun name ->
          check
            (match function_bodies text name with
             | [] -> false
             | bodies -> List.for_all (fun b -> applications b = 0) bodies)
            (Printf.sprintf "%s.%s makes no call" unit name))
        names)
    [ "dfa_equivalence_core",
      [ "compare_complete"; "compare_equal"; "comparison_witness";
        "reduce_complete"; "reduce_preserves"; "reduce_minimum" ];
      "regex_language",
      [ "sound"; "complete"; "lower_matches"; "lower_valid" ] ];
  finish ()
