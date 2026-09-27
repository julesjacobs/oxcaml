(* Checks the Lambda of the list, tree and ring modules; see
   structures_erasure.ml. Argument: the directory of the dumps. *)
open Emitted_code

let proofs =
  [ "heap"; "valid"; "rev_append"; "flipped"; "flipped_all"; "reversed";
    "append"; "spliced"; "last"; "chain"; "inserted"; "removed";
    "fold_correct"; "add_correct" ]

let models =
  [ "Pref_ring_checks"; "Pref_ring_reverse_model"; "Pref_ring_splice_model";
    "Pref_ring_proofs"; "Vox_pref_semantics" ]

let () =
  List.iter
    (fun (unit, names) ->
      let text = read (Filename.concat Sys.argv.(1) (unit ^ ".lambda")) in
      List.iter
        (fun name ->
          let bodies = function_bodies text name in
          check (bodies <> []) (Printf.sprintf "%s.%s is defined" unit name);
          List.iteri
            (fun i body ->
              let what = Printf.sprintf "%s.%s#%d" unit name (i + 1) in
              check
                (not
                   (List.exists
                      (fun p -> occurs ("(apply%s" ^ p ^ "/") body)
                      proofs))
                (what ^ " calls no model or proof function");
              check
                (not
                   (List.exists
                      (fun m -> occurs ("(global%s" ^ m ^ "!)") body)
                      models))
                (what ^ " uses no model module");
              if String.length name >= 7 && String.sub name 0 7 = "observe"
              then
                check
                  (not (occurs "caml_pref_split" body
                        || occurs "caml_pref_join" body))
                  (what ^ " neither splits nor joins tokens"))
            bodies)
        names)
    [ "pref_list",
      [ "reverse_into"; "reverse"; "observe_framed"; "observe_read";
        "observe" ];
      "pref_tree",
      [ "mirror"; "mirror_with_frame"; "observe_framed"; "observe_read";
        "observe" ];
      "pref_ring", [ "splice_range"; "reverse_nodes" ];
      "pref_ring_splice", [ "splice_demo" ];
      "pref_ring_reverse", [ "reverse_demo" ];
      "pref_ring_general",
      [ "reverse"; "insert"; "remove"; "adopt"; "release"; "sentinel_node" ];
      "pref_ring_splice_general", [ "splice"; "adopt"; "release"; "swap" ] ];
  finish ()
