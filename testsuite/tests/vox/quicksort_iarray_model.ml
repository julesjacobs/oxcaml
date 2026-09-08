open Vox_iarray
open Vox_iarray.Int

let (swap_partition @ total) : (values : int iarray) -> (pivot : int) ->
    (lower : int) -> (scan : int) ->
    {u : unit | if 0 <= lower && lower <= scan && scan < Iarray.length values
      && range values pivot true 0 lower
      && range values pivot false lower scan
      && element values scan <= pivot then
      range (swap values lower scan) pivot true 0 ((lower + 1))
      && range (swap values lower scan) pivot false ((lower + 1)) ((scan + 1))
      else true} = fun values pivot lower scan ->
  let zero = 0 in
  let low_side = true in
  let high_side = false in
  let next_lower = (lower + 1) in
  let next_scan = (scan + 1) in
  let x = element values lower in
  let y = element values scan in
  let intermediate = updated values lower y in
  let swapped = swap values lower scan in
  swap_def values lower scan;
  element_def values lower;
  element_def values scan;
  updated_length values lower y;
  updated_length intermediate scan x;
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

let (glue_partition @ total) : (before : int iarray) ->
    (after : int iarray) -> (pivot : int) -> (index : int) ->
    {u : unit | 0 <= index && index < Iarray.length before
      && Iarray.length after = Iarray.length before
      && element before index = pivot
      && range before pivot true 0 index
      && range before pivot false (index + 1) (Iarray.length before)
      && sorted (slice after 0 index)
      && sorted (slice after (index + 1) (Iarray.length after))
      && permutation (slice before 0 index) (slice after 0 index)
      && slice after index (index + 1) === slice before index (index + 1)
      && permutation (slice before (index + 1) (Iarray.length before))
        (slice after (index + 1) (Iarray.length after))} @ ghost ->
    {u : unit | sorted after && permutation before after} =
    fun before after pivot index premise ->
  let refine_ premise = premise in
  let zero = 0 in
  let next = index + 1 in
  let size = Iarray.length before in
  let old_left = slice before zero index in
  let old_right = slice before next size in
  let left = slice after zero index in
  let right = slice after next size in
  let low = true in
  let high = false in
  slice_length after zero index;
  slice_length after next size;
  range_slice before pivot low zero index;
  range_slice before pivot high next size;
  let premise = () in
  all_permutation old_left left pivot low
    (refine_ premise);
  all_permutation old_right right pivot high
    (refine_ premise);
  count_extensional before after (fun target ->
    count_decompose3 before index next target;
    count_decompose3 after index next target;
    permutation_count old_left left target;
    permutation_count old_right right target;
    let u = () in refine_ u);
  element_slice before index next zero;
  element_slice after index next zero;
  sorted_glue after pivot index;
  let u = () in refine_ u
