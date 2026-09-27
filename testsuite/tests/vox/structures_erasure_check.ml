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

(* The functions of these modules that run: every function called by name
   from an audited function must be one of them. Model functions, lemmas
   and [_def] equations are not; [Pref_ring.field], which selects a node's
   [prev] or [next] cell, is both. *)
let runtime =
  [ "reverse_into"; "reverse"; "observe_framed"; "observe_read"; "observe";
    "mirror"; "mirror_with_frame"; "set_links"; "connect"; "insert_between";
    "remove"; "read_link"; "walk"; "traverse"; "make_node"; "splice_range";
    "reverse_nodes"; "read_node"; "insert"; "splice"; "adopt"; "release";
    "sentinel_node"; "swap"; "observe_source"; "observe_destination";
    "field" ]

(* The functions a body calls by name, including calls marked
   [(apply[yielding] ...)], which [Emitted_code.direct_calls] does not
   match. *)
let calls body =
  List.sort_uniq String.compare
    (direct_calls body
     @ List.map
         (fun (_, _, captures) -> List.nth captures 1)
         (find_all "(apply[%w]%s%w/%d" body))

(* The functions reachable from [entry] through calls by name. *)
let reached text entry =
  let rec go pending visited =
    match pending with
    | [] -> List.sort String.compare visited
    | name :: pending when List.mem name visited -> go pending visited
    | name :: pending ->
      go (List.concat_map calls (function_bodies text name) @ pending)
        (name :: visited)
  in
  go [ entry ] []

(* A body that calls no model or proof function: none of [proofs], only
   [runtime] functions by name, nothing in a [Proofs] submodule and no heap
   law. *)
let clean body =
  not (List.exists (fun p -> occurs ("(apply%s" ^ p ^ "/") body) proofs)
  && List.for_all (fun f -> List.mem f runtime) (calls body)
  && not (occurs "Proofs/" body)
  && not (occurs "caml_pref_heap_law" body)

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
              check (clean body) (what ^ " calls no model or proof function");
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
            bodies;
          (* The functions [name] reaches through calls by name in this
             module; each of their bodies must be clean too. *)
          let reached = reached text name in
          check
            (List.for_all
               (fun f -> List.for_all clean (function_bodies text f))
               reached)
            (Printf.sprintf "%s.%s reaches only runtime functions: %s" unit
               name (words reached)))
        names)
    [ "pref_list",
      [ "reverse_into"; "reverse"; "observe_framed"; "observe_read";
        "observe" ];
      "pref_tree",
      [ "mirror"; "mirror_with_frame"; "observe_framed"; "observe_read";
        "observe" ];
      "pref_ring",
      [ "connect"; "insert_between"; "remove"; "make_node"; "traverse";
        "splice_range"; "reverse_nodes" ];
      "pref_ring_splice", [ "splice_demo" ];
      "pref_ring_reverse", [ "reverse_demo" ];
      "pref_ring_general",
      [ "reverse"; "insert"; "remove"; "adopt"; "release"; "observe";
        "sentinel_node" ];
      "pref_ring_splice_general",
      [ "splice"; "adopt"; "release"; "observe_source";
        "observe_destination"; "swap" ] ];
  finish ()
