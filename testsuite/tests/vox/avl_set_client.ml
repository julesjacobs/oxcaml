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
    lookup_empty one;
    lookup_add four four ascending_three;
    lookup_union two ascending descending;
    size_zero empty_set;
    equal_lookup ascending descending two;
    let (same_lookup @ total) :
        (element : int) ->
        {u : unit |
          lookup element ascending === lookup element ascending} =
      fun _element ->
      let u = () in
      u
    in
    extensional ascending ascending same_lookup;
    ())
  in
  Format.printf "semantic equal = %b; representation equal = %b@."
    (equal ascending descending)
    (ascending = descending)

let () =
  List.iter (fun values ->
    let set = List.fold_left (fun set value -> Avl_sets.add value set)
      Avl_sets.empty values in
    List.iter (fun value -> assert (Avl_sets.lookup value set)) values;
    assert (Bigint.to_int_opt (Avl_sets.size set) =
      Some (List.length (List.sort_uniq Int.compare values))))
    [[1; 2; 3]; [3; 2; 1]; [3; 1; 2]; [1; 3; 2];
     [min_int; max_int; 0; min_int]; List.init 200 Fun.id;
     List.init 200 (fun i -> 199 - i)]
