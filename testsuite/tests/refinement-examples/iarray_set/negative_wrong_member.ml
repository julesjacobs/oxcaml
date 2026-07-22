let wrong_member (query : int)
    : unit{ Iarray_set.member query Iarray_set.empty = true } =
  Iarray_set.empty_law ~query;
  ()
