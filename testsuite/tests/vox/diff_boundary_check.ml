(* Checks the Lambda of vox_diff.ml and of the public client; see
   diff_boundary.ml. *)
open Emitted_code

let () =
  let library = read Sys.argv.(1) and client = read Sys.argv.(2) in
  (* The functions each executable diff function may call directly. *)
  let expected =
    [ "snake", [ "snake" ];
      "step_delete", [];
      "step_insert", [];
      "choose", [ "snake" ];
      "advance", [ "advance"; "choose"; "step_delete"; "step_insert" ];
      "finished", [ "finished" ];
      "search", [ "advance"; "finished"; "search" ];
      "bounded_length", [ "bounded_length" ];
      "diff", [ "bounded_length"; "search"; "snake" ] ]
  in
  (* The Proof module's fields, in order, from its block. *)
  let exports =
    match find_all "(makeblock 0 apply_source/" library with
    | (start, _, _) :: _ ->
      let stop = String.index_from library start ')' in
      List.concat_map (fun (_, _, c) -> c)
        (find_all "%w/%d" (String.sub library start (stop - start)))
    | [] -> failwith "no Proof block"
  in
  let reverse_into =
    let rec index i = function
      | [] -> failwith "reverse_into is not exported"
      | "reverse_into" :: _ -> i
      | _ :: rest -> index (i + 1) rest
    in
    string_of_int (index 0 exports)
  in
  List.iter
    (fun (name, allowed) ->
      let body = function_body library name in
      let calls = direct_calls body in
      check (calls = allowed)
        (Printf.sprintf "%s calls exactly [%s]" name (words allowed));
      let proof_calls =
        all_captures "(apply%s(field_imm%s%w%sProof/%d)" body
      in
      let wanted = if name = "finished" then [ reverse_into ] else [] in
      check (proof_calls = wanted)
        (Printf.sprintf "%s calls %d proof functions" name
           (List.length wanted));
      check
        (applications body
         = count "(apply%s%w/%d" body
           + count "(apply%s(field_imm%s%d%sProof/%d)" body)
        (name ^ " makes no other calls");
      check
        (not (occurs "minimum_cost" body
              || occurs "bigint" (String.lowercase_ascii body)))
        (name ^ " computes no metric or big integer"))
    expected;
  let search = function_body library "search" in
  check
    (not
       (List.exists (fun op -> occurs op search)
          [ "(%%int_"; "(+ "; "(- "; "(* "; "(/ "; "(<= "; "(>= "; "(< ";
            "(> "; "(== "; "(!= "; "(raise"; "(exit" ]))
    "search does no arithmetic, comparison, raise or exit";
  check
    (not (occurs "fuel/%d" search || occurs "remaining/%d" search))
    "search has no runtime fuel";
  let diff = function_body library "diff" in
  check (not (occurs "(%%int_add " diff || occurs "(+ " diff))
    "diff does no addition";
  let reverse = function_body library "reverse_into" in
  check (all_captures "(apply%s%w/%d" reverse = [ "reverse_into" ]
         && not (occurs "global" reverse
                || occurs "bigint" (String.lowercase_ascii reverse)))
    "reverse_into only calls itself";
  List.iter
    (fun name ->
      let body = function_body client name in
      check
        (count "(apply%s(field_imm%s0%s(global%sVox_diff!))" body = 1
         && applications body = 1
         && not (occurs "Vox_diff_spec" body))
        (name ^ " makes one call, to Vox_diff.diff"))
    [ "verified_client"; "accepted" ];
  finish ()
