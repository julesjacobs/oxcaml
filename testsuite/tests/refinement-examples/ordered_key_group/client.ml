(* A client that knows nothing about the key type: it has two keys and the
   laws, and no comparison of its own.  Everything it does with the
   invariant it does abstractly, transporting it from [empty] along
   [insert]. *)
module Client (S : Key_intf.SET) = struct
  let empty_is_absent ~(query : S.key @ logical)
      : unit{ S.member query S.empty = false } =
    S.empty_law ~query

  let singleton_well_formed ~(one : S.key @ logical)
      : unit{ S.invariant (S.insert one S.empty) = true } =
    S.insert_invariant ~inserted:one ~set:S.empty
      ~well_formed:S.empty_invariant

  let pointwise_for_reordered ~(one : S.key @ logical)
      ~(other : S.key @ logical) ~(query : S.key @ logical)
      : unit{
        S.member query (S.insert one (S.insert other S.empty))
        = S.member query (S.insert other (S.insert one S.empty))
      } =
    S.insert_law ~inserted:one ~set:(S.insert other S.empty) ~query
      ~well_formed:(singleton_well_formed ~one:other);
    S.insert_law ~inserted:other ~set:S.empty ~query
      ~well_formed:S.empty_invariant;
    S.insert_law ~inserted:other ~set:(S.insert one S.empty) ~query
      ~well_formed:(singleton_well_formed ~one);
    S.insert_law ~inserted:one ~set:S.empty ~query
      ~well_formed:S.empty_invariant;
    ()

  let reordered_equal ~(one : S.key @ logical) ~(other : S.key @ logical)
      : unit{
        S.equal
          (S.insert one (S.insert other S.empty))
          (S.insert other (S.insert one S.empty))
        = true
      } =
    S.equal_backward_law
      ~left:(S.insert one (S.insert other S.empty))
      ~right:(S.insert other (S.insert one S.empty))
      ~pointwise:(pointwise_for_reordered ~one ~other)

  let reordered_round_trip ~(one : S.key @ logical)
      ~(other : S.key @ logical) ~(query : S.key @ logical)
      : unit{
        S.member query (S.insert one (S.insert other S.empty))
        = S.member query (S.insert other (S.insert one S.empty))
      } =
    let equal_sets = reordered_equal ~one ~other in
    S.equal_forward_law
      ~left:(S.insert one (S.insert other S.empty))
      ~right:(S.insert other (S.insert one S.empty))
      ~equal_sets ~query
end

module Ulist_set = Gen_ulist.Make (Int_key)
module Bst_set = Gen_bst.Make (Int_key)
module Avl_set = Gen_avl.Make (Int_key)
module Sorted_set = Gen_sorted.Make (Int_key)

module Ulist_client = Client (Ulist_set)
module Bst_client = Client (Bst_set)
module Avl_client = Client (Avl_set)
module Sorted_client = Client (Sorted_set)

(* And a client that does know the keys, to check the functor result behaves
   at a concrete instance. *)
let bst_absent_from_empty =
  Bst_set.empty_law ~query:41;
  (Bst_set.member 41 Bst_set.empty : bool{ _ = false })

let bst_insert_is_found =
  let set = Bst_set.insert 17 Bst_set.empty in
  let well_formed =
    Bst_set.insert_invariant ~inserted:17 ~set:Bst_set.empty
      ~well_formed:Bst_set.empty_invariant
  in
  Bst_set.insert_law ~inserted:41 ~set ~query:41 ~well_formed;
  (Bst_set.member 41 (Bst_set.insert 41 set) : bool{ _ = true })

let ulist_insert_is_found =
  let set = Ulist_set.insert 17 Ulist_set.empty in
  let well_formed =
    Ulist_set.insert_invariant ~inserted:17 ~set:Ulist_set.empty
      ~well_formed:Ulist_set.empty_invariant
  in
  Ulist_set.insert_law ~inserted:41 ~set ~query:41 ~well_formed;
  (Ulist_set.member 41 (Ulist_set.insert 41 set) : bool{ _ = true })

let avl_insert_is_found =
  let set = Avl_set.insert 17 Avl_set.empty in
  let well_formed =
    Avl_set.insert_invariant ~inserted:17 ~set:Avl_set.empty
      ~well_formed:Avl_set.empty_invariant
  in
  Avl_set.insert_law ~inserted:41 ~set ~query:41 ~well_formed;
  (Avl_set.member 41 (Avl_set.insert 41 set) : bool{ _ = true })

let sorted_insert_is_found =
  let set = Sorted_set.insert 17 Sorted_set.empty in
  let well_formed =
    Sorted_set.insert_invariant ~inserted:17 ~set:Sorted_set.empty
      ~well_formed:Sorted_set.empty_invariant
  in
  Sorted_set.insert_law ~inserted:41 ~set ~query:41 ~well_formed;
  (Sorted_set.member 41 (Sorted_set.insert 41 set) : bool{ _ = true })
