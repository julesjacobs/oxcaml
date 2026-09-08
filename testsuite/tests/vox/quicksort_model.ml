open Vox_sequence
open Vox_int_sequence

let (swap_partition @ total) : (values : int list) -> (pivot : int) ->
    (lower : Bigint.t) -> (scan : Bigint.t) ->
    {u : unit | if 0Z <= lower && lower <= scan && scan < length values
      && range values pivot true 0Z lower
      && range values pivot false lower scan
      && element values scan <= pivot then
      range (swap values lower scan) pivot true 0Z ((Bigint.add lower 1Z))
      && range (swap values lower scan) pivot false ((Bigint.add lower 1Z)) ((Bigint.add scan 1Z))
      else true} = fun values pivot lower scan ->
  let zero = 0Z in
  let low_side = true in
  let high_side = false in
  let next_lower = (Bigint.add lower 1Z) in
  let next_scan = (Bigint.add scan 1Z) in
  let x = element values lower in
  let y = element values scan in
  let intermediate = set values lower y in
  let swapped = swap values lower scan in
  swap_equation values lower scan;
  set_length values lower y;
  set_length intermediate scan x;
  element_swap values lower scan lower;
  element_swap values lower scan scan;
  range_set values pivot low_side zero lower lower y;
  range_set intermediate pivot low_side zero lower scan x;
  let final_low = element swapped lower in
  accepts_def final_low pivot low_side;
  range_grow swapped pivot low_side zero lower;
  if lower = scan then
    (range_empty swapped pivot high_side next_lower next_scan;
    let u = () in refine_ u)
  else
    (range_get values pivot high_side lower scan lower;
    range_shrink values pivot high_side lower scan next_lower scan;
    range_set values pivot high_side next_lower scan lower y;
    range_set intermediate pivot high_side next_lower scan scan x;
    range_grow swapped pivot high_side next_lower scan;
    let u = () in refine_ u)

let (partition_bounds @ total) : (values : int list) -> (pivot : int) -> (index : Bigint.t) ->
    {u : unit | if 0Z <= index && index < length values
      && range values pivot true 0Z index
      && range values pivot false ((Bigint.add index 1Z)) (length values) then
      all (take index values) pivot true && all (drop ((Bigint.add index 1Z)) values) pivot false
      else true} = fun values pivot index ->
  let zero = 0Z in
  let next = (Bigint.add index 1Z) in
  let size = length values in
  let low_side = true in
  let high_side = false in
  range_sub values pivot low_side zero index;
  sub_prefix values index;
  range_sub values pivot high_side next size;
  sub_suffix values next;
  let u = () in refine_ u

let (glue_partition @ total) : (before : int list) -> (pivot : int) -> (index : Bigint.t) ->
    (left : int list) -> (middle : int list) -> (right : int list) ->
    {u : unit | if 0Z <= index && index < length before
      && element before index = pivot
      && range before pivot true 0Z index
      && range before pivot false ((Bigint.add index 1Z)) (length before)
      && sorted left && sorted right
      && permutation (take index before) left
      && middle === sub before index ((Bigint.add index 1Z))
      && permutation (drop ((Bigint.add index 1Z)) before) right then
      sorted (append left (append middle right))
      && permutation before (append left (append middle right)) else true} =
    fun before pivot index left middle right ->
  let next = (Bigint.add index 1Z) in
  let old_left = take index before in
  let old_middle = sub before index next in
  let old_right = drop next before in
  let old_rest = append old_middle old_right in
  let new_rest = append middle right in
  let new_values = append left new_rest in
  let nil = [] in
  let low_side = true in
  let high_side = false in
  partition_bounds before pivot index;
  sub_one before index;
  decompose3 before index next;
  permutation_refl old_middle;
  permutation_append old_middle old_right middle right;
  permutation_append old_left old_rest left new_rest;
  permutation_def before new_values;
  all_permutation old_left left pivot low_side;
  all_permutation old_right right pivot high_side;
  append_def middle right;
  append_def nil right;
  sorted_glue left pivot right;
  let u = () in refine_ u

