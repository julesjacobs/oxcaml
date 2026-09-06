let () =
  let open Avl_sets in
  let one = 1 in
  let two = 2 in
  let three = 3 in
  let four = 4 in
  let empty_set = empty in
  let ascending_one = add one empty_set in
  let ascending_two = add two ascending_one in
  let ascending_three = add three ascending_two in
  let ascending = add four ascending_three in
  let descending_four = add four empty_set in
  let descending_three = add three descending_four in
  let descending_two = add two descending_three in
  let descending = add one descending_two in
  let _union_result = union ascending descending in
  let _proofs = ghost_ (
    let refine_ empty_law = lookup_empty one in
    let refine_ add_law = lookup_add four four ascending_three in
    let refine_ union_law = lookup_union two ascending descending in
    let refine_ size_law = size_zero empty_set in
    let refine_ equal_lookup_law = equal_lookup ascending descending two in
    let (same_lookup @ total) :
        (element : int) ->
        {u : unit |
          lookup element ascending === lookup element ascending} =
      fun _element ->
      let u = () in
      refine_ u
    in
    let refine_ extensional_law =
      extensional ascending ascending same_lookup
    in
    ())
  in
  Format.printf "semantic equal = %b; representation equal = %b@."
    (equal ascending descending)
    (ascending = descending)
