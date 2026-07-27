module Client (S : Set_intf.SET) = struct
  let empty_is_absent ~(query : int)
      : unit{ S.member query S.empty = false } =
    S.empty_law ~query

  (* The invariant is abstract here: a client only ever transports it from
     [empty] along [insert]. *)
  let five : unit{ S.invariant (S.insert 5 S.empty) = true } =
    S.insert_invariant ~inserted:5 ~tree:S.empty
      ~well_formed:S.empty_invariant

  let eleven : unit{ S.invariant (S.insert 11 S.empty) = true } =
    S.insert_invariant ~inserted:11 ~tree:S.empty
      ~well_formed:S.empty_invariant

  let pointwise_for_reordered_insertions ~(query : int)
      : unit{
        S.member query (S.insert 5 (S.insert 11 S.empty))
        = S.member query (S.insert 11 (S.insert 5 S.empty))
      } =
    S.insert_law ~inserted:5 ~tree:(S.insert 11 S.empty) ~query
      ~well_formed:eleven;
    S.insert_law ~inserted:11 ~tree:S.empty ~query
      ~well_formed:S.empty_invariant;
    S.insert_law ~inserted:11 ~tree:(S.insert 5 S.empty) ~query
      ~well_formed:five;
    S.insert_law ~inserted:5 ~tree:S.empty ~query
      ~well_formed:S.empty_invariant;
    ()

  let reordered_insertions_equal ()
      : unit{
        S.equal
          (S.insert 5 (S.insert 11 S.empty))
          (S.insert 11 (S.insert 5 S.empty))
        = true
      } =
    S.equal_backward_law
      ~t1:(S.insert 5 (S.insert 11 S.empty))
      ~t2:(S.insert 11 (S.insert 5 S.empty))
      ~pointwise:pointwise_for_reordered_insertions

  let reordered_insertions_compare : bool{ _ = true } =
    reordered_insertions_equal ();
    S.equal
      (S.insert 5 (S.insert 11 S.empty))
      (S.insert 11 (S.insert 5 S.empty))

  let reordered_insertions_round_trip ~(query : int) =
    let equal_trees = reordered_insertions_equal () in
    S.equal_forward_law
      ~t1:(S.insert 5 (S.insert 11 S.empty))
      ~t2:(S.insert 11 (S.insert 5 S.empty))
      ~equal_trees ~query

  let equal_membership ~(t1 : S.t @ logical) ~(t2 : S.t @ logical)
      ~(equal_trees : unit{ S.equal t1 t2 = true }) ~(query : int) =
    S.equal_forward_law ~t1 ~t2 ~equal_trees ~query
end

module Bst_client = Client (Bst)
module Rbt_client = Client (Rbt)
module Avl_client = Client (Avl)
module Ulist_client = Client (Ulist)
