module Client (S : Set_intf.SET) = struct
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
end

module Iarray_set_client = Client (Iarray_set)
