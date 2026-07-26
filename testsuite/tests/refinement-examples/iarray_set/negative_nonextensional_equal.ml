let prove_unequal (inserted : int)
    : unit{
      Iarray_set.equal Iarray_set.empty
        (Iarray_set.insert inserted Iarray_set.empty)
      = false
    } =
  let singleton = Iarray_set.insert inserted Iarray_set.empty in
  let same = Iarray_set.equal Iarray_set.empty singleton in
  match same with
  | true ->
    Iarray_set.empty_law ~query:inserted;
    Iarray_set.insert_law
      ~inserted ~tree:Iarray_set.empty ~query:inserted
      ~well_formed:Iarray_set.empty_invariant;
    Iarray_set.equal_forward_law
      ~t1:Iarray_set.empty ~t2:singleton
      ~equal_trees:() ~query:inserted;
    ()
  | false -> ()

let wrong_equal (inserted : int)
    : unit{
      Iarray_set.equal Iarray_set.empty
        (Iarray_set.insert inserted Iarray_set.empty)
      = true
    } =
  prove_unequal inserted;
  ()
