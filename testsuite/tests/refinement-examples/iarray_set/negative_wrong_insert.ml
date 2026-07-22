let wrong_insert (inserted : int)
    : unit{
      Iarray_set.member inserted
        (Iarray_set.insert inserted Iarray_set.empty)
      = false
    } =
  Iarray_set.empty_law ~query:inserted;
  Iarray_set.insert_law
    ~inserted ~tree:Iarray_set.empty ~query:inserted;
  ()
