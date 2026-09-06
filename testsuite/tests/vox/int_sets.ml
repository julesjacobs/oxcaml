(* TEST
 flags = "-extension refinement_types";
 has-z3;
 timeout = "900";
 all_modules = "int_set_intf.mli list_int_set.mli list_int_set.ml int_sets.ml";
 { bytecode; }
 { flags += " -principal"; bytecode; }
*)

let () =
  let open List_int_set in
  let one = 1 in
  let two = 2 in
  let three = 3 in
  let empty_set = empty in
  let left_start = add one empty_set in
  let left = add three left_start in
  let right = add two empty_set in
  let result = union left right in
  let _proofs = ghost_ (
    let refine_ empty_law = lookup_empty one in
    let refine_ add_law = lookup_add three three left_start in
    let refine_ union_law = lookup_union two left right in
    let refine_ size_law = size_zero empty_set in
    let (same_lookup @ total) :
        (element : int) ->
        {u : unit | lookup element result === lookup element result} =
      fun _element ->
      let u = () in
      refine_ u
    in
    let refine_ extensional_law = extensional result result same_lookup in
    ())
  in
  Format.printf "members = %b,%b,%b; size = %s@."
    (lookup one result) (lookup two result) (lookup three result)
    (Bigint.to_string (size result))
