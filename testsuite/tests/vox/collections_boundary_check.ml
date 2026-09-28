(* Checks that the collections' runtime functions call no model or proof
   code; see collections_boundary.ml. Argument: the directory of the
   Lambda dumps. *)
open Emitted_code

(* The identifiers [name/N] in the text of each call, from [apply] to the
   end of the line or to the first closing parenthesis. *)
let called_identifiers ~to_line_end body =
  List.concat_map
    (fun (_, stop, _) ->
      let limit =
        try String.index_from body stop (if to_line_end then '\n' else ')')
        with Not_found -> String.length body
      in
      let segment = String.sub body stop (limit - stop) in
      List.filter_map
        (fun (start, _, captures) ->
          if start > 0 && is_word segment.[start - 1] then None
          else Some (List.hd captures))
        (find_all "%w/%d" segment))
    (find_all "apply" body)

let is_def name =
  String.length name > 4
  && String.sub name (String.length name - 4) 4 = "_def"

let () =
  let dir = Sys.argv.(1) in
  let dump unit = read (Filename.concat dir (unit ^ ".lambda")) in
  (* Only the sparse overlay is left out, under -principal. *)
  let audit ?(both = false) ?(optional = false) unit names ~forbidden ~called
      ~to_line_end =
    match dump unit with
    | exception Sys_error _ when optional -> ()
    | text ->
      List.iter
        (fun name ->
          let bodies = function_bodies text name in
          let bodies =
            match bodies with
            | [] -> []
            | first :: _ when not both -> [ first ]
            | first :: rest ->
              [ first; List.nth (first :: rest) (List.length rest) ]
          in
          check
            (bodies <> []
             && List.for_all
                  (fun body ->
                    not (List.exists (fun s -> occurs s body) forbidden)
                    && not
                         (List.exists
                            (fun f -> is_def f || List.mem f called)
                            (called_identifiers ~to_line_end body)))
                  bodies)
            (Printf.sprintf "%s.%s runs no model or proof code" unit name))
        names
  in
  audit "quicksort"
    [ "partition"; "partition_middle"; "sort_sized"; "sort"; "sort_owned" ]
    ~forbidden:
      [ "Quicksort_model"; "Vox_int_sequence"; "Vox_sequence";
        "caml_borrow_current"; "caml_borrow_final"; "caml_borrow_contents" ]
    ~called:[] ~to_line_end:true;
  audit "functional_queue" [ "enqueue"; "dequeue" ] ~forbidden:[]
    ~called:[ "contents"; "reverse"; "reverse_append_correct" ]
    ~to_line_end:true;
  audit ~optional:true "sparse_overlay"
    [ "empty"; "set"; "clear"; "lookup"; "get" ]
    ~forbidden:[ "find_remove"; "Vox_iarray" ] ~called:[] ~to_line_end:true;
  audit ~both:true "avl_sets"
    [ "add"; "union"; "lookup"; "add_tree"; "lookup_tree"; "make_node";
      "balance"; "add_elements" ]
    ~forbidden:
      [ "Validity_proofs/"; "Insertion_model_proofs/"; "Element_proofs/";
        "List_proofs/" ]
    ~called:[ "valid"; "all_less"; "all_greater" ] ~to_line_end:false;
  audit "sorted_array"
    [ "mem"; "equal_range"; "insert"; "remove_at"; "find_first"; "find_last";
      "remove_one" ]
    ~forbidden:[] ~called:[ "contents" ] ~to_line_end:false;
  audit "sorted_array_proofs"
    [ "bounds"; "equal_range"; "insert"; "remove_at"; "find_first";
      "find_last"; "remove_one" ]
    ~forbidden:[]
    ~called:
      [ "occurs"; "occurs_range"; "edited"; "edited_at"; "range_at";
        "ordered"; "partition" ]
    ~to_line_end:false;
  finish ()
