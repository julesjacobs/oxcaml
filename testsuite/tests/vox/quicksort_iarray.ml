open Borrow_iarray
module Spec = Vox_iarray.Int

external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"
let (half @ total) : (value : int) ->
    {result : int | if value > 0 then 0 <= result && result < value else true} =
    fun value ->
  let two = 2 in
  let result = divide value (refine_ two) in
  refine_ result

type runner =
  (spawn : bool) ->
  (left : int Slice.t) @ local unique -> (right : int Slice.t) @ local unique ->
  (lp : (int iarray @ total immutable -> bool @ ghost)) @ ghost ->
  (rp : (int iarray @ total immutable -> bool @ ghost)) @ ghost ->
  ((s : {s : int Slice.t | Slice.current s === Slice.current left
      && Slice.final s === Slice.final left}) @ local unique ->
    {u : unit | let refine_ s = s in lp (Slice.final s)}) @ portable once ->
  ((s : {s : int Slice.t | Slice.current s === Slice.current right
      && Slice.final s === Slice.final right}) @ local unique ->
    {u : unit | let refine_ s = s in rp (Slice.final s)}) @ portable once ->
  {u : unit | lp (Slice.final left) && rp (Slice.final right)}

let (sequential_runner @ portable total) : runner =
    fun _spawn left right lp rp lf rf ->
  let left_arg : {s : int Slice.t | Slice.current s === Slice.current left
    && Slice.final s === Slice.final left} = refine_ left in
  let right_arg : {s : int Slice.t | Slice.current s === Slice.current right
    && Slice.final s === Slice.final right} = refine_ right in
  let refine_ l = lf left_arg in
  let refine_ r = rf right_arg in
  let u = () in refine_ u

let rec (partition @ total) : (pivot : int) -> (size : int) ->
    (lower : int) -> (scan : int) ->
    (loan : {s : int Slice.t |
      0 < size && 0 <= lower && lower <= scan && scan < size
      && Iarray.length (Slice.current s) = size
      && Spec.element (Slice.current s) ((size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0 (lower)
      && Spec.range (Slice.current s) pivot false (lower) (scan)})
      @ local unique ->
    {r : (int, int Slice.t) step | let refine_ s = loan in
      0 <= r.value && r.value < size
      && Iarray.length (Slice.current r.state) = size
      && Spec.element (Slice.current r.state) (r.value) = pivot
      && Spec.range (Slice.current r.state) pivot true 0 (r.value)
      && Spec.range (Slice.current r.state) pivot false
        ((r.value + 1)) (size)
      && Spec.permutation (Slice.current s) (Slice.current r.state)
      && Slice.final r.state === Slice.final s} @ local unique =
    fun pivot size lower scan loan -> exclave_ (
  let refine_ s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  let last = (size - 1) in
  let zero = 0 in
  let low_side = true in
  let high_side = false in
  if scan < size - 1 then (
    let index : {i : int | 0 <= i
      && i < Iarray.length (Slice.current s)} =
      refine_ scan in
    let refine_ value = Slice.get (borrow_ s) index in
    ghost_ (Spec.element_def before scan);
    let next_scan = scan + 1 in
    let next_lower, s2 =
      if value < pivot || (value = pivot && scan land 1 = 0) then (
        let first : {i : int | 0 <= i
          && i < Iarray.length (Slice.current s)} =
          refine_ lower in
        let second : {i : int | 0 <= i
          && i < Iarray.length (Slice.current s)} =
          refine_ scan in
        let refine_ s2 = Slice.swap s first second in
        ghost_ (Quicksort_iarray_model.swap_partition before pivot lower scan);
        ghost_ (Spec.element_swap before lower scan last);
        ghost_ (Spec.permutation_swap before lower scan);
        lower + 1, s2)
      else (
        ghost_ (Spec.accepts_def value pivot high_side);
        ghost_ (Spec.range_grow before pivot high_side lower scan);
        ghost_ (Spec.permutation_refl before);
        lower, s) in
    let intermediate = ghost_ (Slice.current (borrow_ s2)) in
    let next : {s : int Slice.t |
      0 < size && 0 <= next_lower && next_lower <= next_scan && next_scan < size
      && Iarray.length (Slice.current s) = size
      && Spec.element (Slice.current s) ((size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0 (next_lower)
      && Spec.range (Slice.current s) pivot false
        (next_lower) (next_scan)} = refine_ s2 in
    let refine_ result = partition pivot size next_lower next_scan next in
    let {value; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    ghost_ (Spec.permutation_trans before intermediate after);
    let result = {value; state} in
    refine_ result)
  else (
    let first : {i : int | 0 <= i
      && i < Iarray.length (Slice.current s)} =
      refine_ lower in
    let second : {i : int | 0 <= i
      && i < Iarray.length (Slice.current s)} =
      refine_ scan in
    let refine_ state = Slice.swap s first second in
    let after = ghost_ (Slice.current (borrow_ state)) in
    let next_lower = lower + 1 in
    ghost_ (Quicksort_iarray_model.swap_partition before pivot lower scan);
    ghost_ (Spec.range_shrink after pivot low_side zero next_lower zero lower);
    ghost_ (Spec.element_swap before lower scan lower);
    ghost_ (Spec.permutation_swap before lower scan);
    let result = {value = lower; state} in
    refine_ result))
[@@decreases
  let size : int = size in
  let scan : int = scan in
  size - scan]

let rec (sort_sized @ portable total) : (run : runner) @ portable ->
    (domains : int) -> (cutoff : int) -> (size : int) ->
    (loan : {s : int Slice.t | 0 <= size
      && Iarray.length (Slice.current s) = size}) @ local unique ->
    {u : unit | let refine_ s = loan in
      Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun run domains cutoff size loan ->
  let refine_ s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  if size <= 1 then (
    ghost_ (Spec.sorted_short before);
    ghost_ (Spec.permutation_refl before);
    Slice.finish s;
    let u = () in refine_ u)
  else (
    let zero = 0 in
    let last = size - 1 in
    let refine_ middle = half size in
    let middle_index : {i : int | 0 <= i
      && i < Iarray.length (Slice.current s)} =
      refine_ middle in
    let last_index : {i : int | 0 <= i
      && i < Iarray.length (Slice.current s)} =
      refine_ last in
    let refine_ s = Slice.swap s middle_index last_index in
    let seeded = ghost_ (Slice.current (borrow_ s)) in
    ghost_ (Spec.permutation_swap before middle last);
    let low_side = true in
    let high_side = false in
    let index : {i : int | 0 <= i
      && i < Iarray.length (Slice.current s)} =
      refine_ last in
    let refine_ pivot = Slice.get (borrow_ s) index in
    let (pivot : int) = pivot in
    ghost_ (Spec.element_def seeded last);
    ghost_ (Spec.range_empty seeded pivot low_side zero zero);
    ghost_ (Spec.range_empty seeded pivot high_side zero zero);
    let initial : {s : int Slice.t |
      0 < size && 0 <= zero && zero <= zero && zero < size
      && Iarray.length (Slice.current s) = size
      && Spec.element (Slice.current s) ((size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0 (zero)
      && Spec.range (Slice.current s) pivot false (zero) (zero)} =
      refine_ s in
    let refine_ partitioned = partition pivot size zero zero initial in
    let {value = (boundary : int); state = s2} = partitioned in
    let divided = ghost_ (Slice.current (borrow_ s2)) in
    ghost_ (Spec.permutation_trans before seeded divided);
    let past = boundary + 1 in
    let right_size = size - past in
    let split_first : {i : int | 0 <= i
      && i <= Iarray.length (Slice.current s2)} =
      refine_ boundary in
    let split_past : {j : int | let refine_ i = split_first in i <= j
      && j <= Iarray.length (Slice.current s2)} =
      refine_ past in
    let post = ghost_ (fun (_ : unit @ immutable)
        (left : int iarray @ total immutable) (middle : int iarray @ total immutable)
        (right : int iarray @ total immutable) ->
      Spec.sorted left && Spec.sorted right
      && Spec.permutation (Vox_iarray.slice divided 0 boundary) left
      && middle === Vox_iarray.slice divided boundary (boundary + 1)
      && Spec.permutation (Vox_iarray.slice divided (boundary + 1) size) right) in
    let spawn = domains > 1 && boundary >= cutoff && right_size >= cutoff in
    let left_domains =
      if spawn then let refine_ n = half domains in n else domains in
    let right_domains = if spawn then domains - left_domains else domains in
    let refine_ result = Slice.split3 s2 split_first split_past post (fun l m r ->
      let refine_ left = l in
      let refine_ middle = m in
      let refine_ right = r in
      let left_before = ghost_ (Slice.current (borrow_ left)) in
      let right_before = ghost_ (Slice.current (borrow_ right)) in
      let left_post = ghost_ (fun (after : int iarray @ total immutable) ->
        Spec.sorted after && Spec.permutation left_before after) in
      let right_post = ghost_ (fun (after : int iarray @ total immutable) ->
        Spec.sorted after && Spec.permutation right_before after) in
      ghost_ (Vox_iarray.slice_length divided zero boundary);
      ghost_ (Vox_iarray.slice_length divided past size);
      let refine_ children = run spawn left right left_post right_post
        (fun child ->
          let refine_ child = child in
          let sized : {s : int Slice.t | 0 <= boundary
            && Iarray.length (Slice.current s) = boundary} = refine_ child in
          let refine_ u = sort_sized run left_domains cutoff boundary sized in
          refine_ u)
        (fun child ->
          let refine_ child = child in
          let sized : {s : int Slice.t | 0 <= right_size
            && Iarray.length (Slice.current s) = right_size} = refine_ child in
          let refine_ u = sort_sized run right_domains cutoff right_size sized in
          refine_ u) in
      Slice.finish middle;
      let u = () in
      refine_ u) in
    let {value = u; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    let premise = () in
    ghost_ (Quicksort_iarray_model.glue_partition
      divided after pivot boundary (refine_ premise));
    ghost_ (Spec.permutation_trans before divided after);
    Slice.finish state;
    refine_ u)
[@@decreases size]

let (sort_with_budget @ portable total) : (run : runner) @ portable ->
    (domains : int) -> (cutoff : int) -> (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun run domains cutoff s ->
  let refine_ size = Slice.length (borrow_ s) in
  let (size : int) = size in
  let sized : {s : int Slice.t | 0 <= size
    && Iarray.length (Slice.current s) = size} = refine_ s in
  let refine_ u = sort_sized run domains cutoff size sized in
  refine_ u

let (sort_array_with_budget @ portable total) : (run : runner) @ portable ->
    (domains : int) -> (cutoff : int) ->
    (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun run domains cutoff a ->
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let post = ghost_ (fun (_ : unit @ immutable) (after : int iarray @ total immutable) ->
        Spec.sorted after && Spec.permutation before after) in
  let refine_ result = Owned_array.with_mut a post (fun loan ->
    let refine_ s = loan in
    let refine_ u = sort_with_budget run domains cutoff s in
    refine_ u) in
  let {value = u; state} = result in
  refine_ state

let (sort @ portable total) : (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun s ->
  let refine_ u = sort_with_budget sequential_runner 1 512 s in
  refine_ u

let (sort_array @ portable total) : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique = fun a ->
  let refine_ result = sort_array_with_budget sequential_runner 1 512 a in
  refine_ result

let (parallel_sort @ portable) : ?max_domains:int -> ?cutoff:int -> (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) s ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let refine_ u = sort_with_budget Slice.parallel domains cutoff s in
  refine_ u

let (parallel_sort_array @ portable) : ?max_domains:int -> ?cutoff:int -> (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) a ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let refine_ result = sort_array_with_budget Slice.parallel domains cutoff a in
  refine_ result
