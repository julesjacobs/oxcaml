val len : int list @ local logical -> int @@ total
val elem : int list @ local logical -> int -> int @@ total
val update : int list @ logical -> int -> int -> int list @@ total
val take : int -> int list @ logical -> int list @@ total
val drop : int -> int list @ logical -> int list @@ total
val append :
  int list @ logical -> int list @ logical -> int list @@ total
val singleton : int -> int list @@ total
val segment : int -> int -> int list @ logical -> int list @@ total
val all_le : int list @ local logical -> int -> bool @@ total
val all_ge : int list @ local logical -> int -> bool @@ total
val sorted : int list @ local logical -> bool @@ total
val insert_bag : int -> int list @ logical -> int list @@ total
val bag : int list @ local logical -> int list @@ total
val bag_union :
  int list @ logical -> int list @ logical -> int list @@ total
val perm :
  int list @ local logical -> int list @ local logical -> bool @@ total

val len_update_law :
  values:int list @ logical -> index:int -> value:int ->
  unit{ len (update values index value) = len values } @@ total

val len_take_law :
  count:int{ 0 <= _ } -> values:int list @ logical ->
  bounded:unit{ count <= len values } ->
  unit{ len (take count values) = count } @@ total

val len_segment_law :
  values:int list @ logical ->
  first:int{ 0 <= _ } ->
  last:int{ first <= _ && _ <= len values } ->
  unit{ len (segment first last values) = last - first } @@ total

val insert_commute_law :
  first:int -> second:int -> values:int list @ logical ->
  unit{
    insert_bag first (insert_bag second values)
    = insert_bag second (insert_bag first values)
  } @@ total

val bag_exchange_head_law :
  head:int ->
  values:int list @ logical ->
  index:int{ 0 <= _ && _ < len values } ->
  unit{
    bag (elem values index :: update values index head)
    = bag (head :: values)
  } @@ total

val bag_swap_law :
  values:int list @ logical ->
  first:int{ 0 <= _ && _ < len values } ->
  second:int{ 0 <= _ && _ < len values } ->
  unit{
    bag
      (update
         (update values first (elem values second))
         second (elem values first))
    = bag values
  } @@ total

val perm_swap_law :
  values:int list @ logical ->
  first:int{ 0 <= _ && _ < len values } ->
  second:int{ 0 <= _ && _ < len values } ->
  unit{
    perm values
      (update
         (update values first (elem values second))
         second (elem values first))
    = true
  } @@ total

val take_update_ge_law :
  count:int -> index:int -> value:int -> values:int list @ logical ->
  proof:unit{ count <= index } ->
  unit{ take count (update values index value) = take count values }
  @@ total

val take_nonpositive_law :
  count:int -> values:int list @ logical ->
  proof:unit{ count <= 0 } ->
  unit{ take count values = [] } @@ total

val elem_update_law :
  values:int list @ logical ->
  update_index:int{ 0 <= _ && _ < len values } ->
  query:int -> value:int ->
  unit{
    elem (update values update_index value) query
    = if query = update_index then value else elem values query
  } @@ total

val all_le_append_law :
  left:int list @ logical -> right:int list @ logical -> bound:int ->
  unit{
    all_le (append left right) bound
    = (all_le left bound && all_le right bound)
  } @@ total

val all_ge_append_law :
  left:int list @ logical -> right:int list @ logical -> bound:int ->
  unit{
    all_ge (append left right) bound
    = (all_ge left bound && all_ge right bound)
  } @@ total

val empty_bounds_law :
  bound:int -> unit{ all_le [] bound = true && all_ge [] bound = true }
  @@ total

val take_snoc_law :
  values:int list @ logical ->
  index:int{ 0 <= _ && _ < len values } ->
  unit{
    take (index + 1) values
    = append (take index values) (singleton (elem values index))
  } @@ total

val take_all_law :
  values:int list @ logical ->
  unit{ take (len values) values = values } @@ total

val swap_le_law :
  values:int list @ logical -> pivot:int ->
  lower:int{ 0 <= _ } ->
  scan:int{ lower <= _ && _ < len values } ->
  prefix:unit{ all_le (take lower values) pivot = true } ->
  scanned:unit{ elem values scan <= pivot } ->
  unit{
    all_le
      (take (lower + 1)
         (update
            (update values lower (elem values scan))
            scan (elem values lower)))
      pivot
    = true
  } @@ total

val perm_refl_law :
  values:int list @ logical -> unit{ perm values values = true } @@ total

val perm_trans_law :
  first:int list @ logical ->
  second:int list @ logical ->
  third:int list @ logical ->
  left:unit{ perm first second = true } ->
  right:unit{ perm second third = true } ->
  unit{ perm first third = true } @@ total

val len_drop_law :
  count:int{ 0 <= _ } -> values:int list @ logical ->
  bounded:unit{ count <= len values } ->
  unit{ len (drop count values) = len values - count } @@ total

val elem_drop_law :
  count:int{ 0 <= _ } -> index:int{ 0 <= _ } ->
  values:int list @ logical ->
  unit{ elem (drop count values) index = elem values (count + index) }
  @@ total

val drop_update_lt_law :
  count:int ->
  index:int{ 0 <= _ && _ < count } ->
  value:int -> values:int list @ logical ->
  unit{ drop count (update values index value) = drop count values }
  @@ total

val drop_update_ge_law :
  count:int{ 0 <= _ } ->
  index:int{ count <= _ } ->
  value:int -> values:int list @ logical ->
  unit{
    drop count (update values index value)
    = update (drop count values) (index - count) value
  } @@ total

val drop_cons_law :
  values:int list @ logical ->
  index:int{ 0 <= _ && _ < len values } ->
  unit{
    drop index values
    = append
        (singleton (elem values index))
        (drop (index + 1) values)
  } @@ total

val segment_snoc_law :
  values:int list @ logical ->
  first:int{ 0 <= _ } ->
  last:int{ first <= _ && _ < len values } ->
  unit{
    segment first (last + 1) values
    = append
        (segment first last values)
        (singleton (elem values last))
  } @@ total

val segment_empty_law :
  values:int list @ logical -> first:int{ 0 <= _ } ->
  unit{ segment first first values = [] } @@ total

val segment_one_law :
  values:int list @ logical ->
  index:int{ 0 <= _ && _ < len values } ->
  unit{
    segment index (index + 1) values
    = singleton (elem values index)
  } @@ total

val segment_grow_ge_law :
  values:int list @ logical -> pivot:int ->
  first:int{ 0 <= _ } ->
  last:int{ first <= _ && _ < len values } ->
  middle:unit{ all_ge (segment first last values) pivot = true } ->
  next:unit{ pivot <= elem values last } ->
  unit{ all_ge (segment first (last + 1) values) pivot = true }
  @@ total

val segment_update_out_law :
  values:int list @ logical ->
  first:int{ 0 <= _ } ->
  last:int{ first <= _ } ->
  index:int{ 0 <= _ && _ < len values } ->
  value:int ->
  outside:unit{ index < first || last <= index } ->
  unit{
    segment first last (update values index value)
    = segment first last values
  } @@ total

val segment_cons_law :
  values:int list @ logical ->
  first:int{ 0 <= _ } ->
  last:int{ first < _ && _ <= len values } ->
  unit{
    segment first last values
    = append
        (singleton (elem values first))
        (segment (first + 1) last values)
  } @@ total

val swap_mid_law :
  values:int list @ logical -> pivot:int ->
  lower:int{ 0 <= _ } ->
  scan:int{ lower <= _ && _ < len values } ->
  middle:unit{ all_ge (segment lower scan values) pivot = true } ->
  unit{
    all_ge
      (segment (lower + 1) (scan + 1)
         (update
            (update values lower (elem values scan))
            scan (elem values lower)))
      pivot
    = true
  } @@ total

val segment_to_end_law :
  values:int list @ logical ->
  first:int{ 0 <= _ && _ <= len values } ->
  unit{ segment first (len values) values = drop first values }
  @@ total

val swap_pivot_law :
  values:int list @ logical -> pivot:int ->
  lower:int{ 0 <= _ } ->
  scan:int{ lower <= _ && _ < len values } ->
  parked:unit{ elem values scan = pivot } ->
  unit{
    elem
      (update
         (update values lower (elem values scan))
         scan (elem values lower))
      lower
    = pivot
  } @@ total

val swap_final_ge_law :
  values:int list @ logical -> pivot:int ->
  lower:int{ 0 <= _ } ->
  scan:int{ lower <= _ && _ < len values } ->
  final_index:unit{ scan = len values - 1 } ->
  middle:unit{ all_ge (segment lower scan values) pivot = true } ->
  unit{
    all_ge
      (drop (lower + 1)
         (update
            (update values lower (elem values scan))
            scan (elem values lower)))
      pivot
    = true
  } @@ total

val sorted_short_law :
  values:int list @ logical ->
  proof:unit{ len values <= 1 } ->
  unit{ sorted values = true } @@ total

val take_drop_append_law :
  count:int -> values:int list @ logical ->
  unit{ append (take count values) (drop count values) = values }
  @@ total

val drop_drop_law :
  first:int{ 0 <= _ } -> second:int{ 0 <= _ } ->
  values:int list @ logical ->
  unit{
    drop first (drop second values) = drop (first + second) values
  } @@ total

val append3_decomp_law :
  values:int list @ logical ->
  first:int{ 0 <= _ } ->
  last:int{ first <= _ } ->
  unit{
    append
      (take first values)
      (append (segment first last values) (drop last values))
    = values
  } @@ total

val decomposition_perm_law :
  values:int list @ logical ->
  first:int{ 0 <= _ } ->
  last:int{ first <= _ } ->
  unit{
    perm
      values
      (append
         (take first values)
         (append (segment first last values) (drop last values)))
    = true
  } @@ total

val bag_union_insert_law :
  value:int -> left:int list @ logical -> right:int list @ logical ->
  unit{
    bag_union (insert_bag value left) right
    = insert_bag value (bag_union left right)
  } @@ total

val bag_append_law :
  left:int list @ logical -> right:int list @ logical ->
  unit{
    bag (append left right) = bag_union (bag left) (bag right)
  } @@ total

val perm_append_law :
  left:int list @ logical -> left_result:int list @ logical ->
  right:int list @ logical -> right_result:int list @ logical ->
  left_perm:unit{ perm left left_result = true } ->
  right_perm:unit{ perm right right_result = true } ->
  unit{
    perm
      (append left right)
      (append left_result right_result)
    = true
  } @@ total

val perm_glue3_law :
  original:int list @ logical ->
  left:int list @ logical -> middle:int list @ logical ->
  right:int list @ logical ->
  left_result:int list @ logical ->
  middle_result:int list @ logical ->
  right_result:int list @ logical ->
  decomposed:unit{
    perm original (append left (append middle right)) = true
  } ->
  left_perm:unit{ perm left left_result = true } ->
  middle_perm:unit{ perm middle middle_result = true } ->
  right_perm:unit{ perm right right_result = true } ->
  unit{
    perm
      original
      (append left_result (append middle_result right_result))
    = true
  } @@ total

val all_le_perm_law :
  source:int list @ logical -> target:int list @ logical -> bound:int ->
  permutation:unit{ perm source target = true } ->
  source_bound:unit{ all_le source bound = true } ->
  unit{ all_le target bound = true } @@ total

val all_ge_perm_law :
  source:int list @ logical -> target:int list @ logical -> bound:int ->
  permutation:unit{ perm source target = true } ->
  source_bound:unit{ all_ge source bound = true } ->
  unit{ all_ge target bound = true } @@ total

val sorted_pivot_glue_law :
  left:int list @ logical -> right:int list @ logical -> pivot:int ->
  left_sorted:unit{ sorted left = true } ->
  right_sorted:unit{ sorted right = true } ->
  left_bound:unit{ all_le left pivot = true } ->
  right_bound:unit{ all_ge right pivot = true } ->
  unit{
    sorted (append left (append (singleton pivot) right)) = true
  } @@ total
