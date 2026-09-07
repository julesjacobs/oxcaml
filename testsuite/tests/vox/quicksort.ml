open Borrow
module Spec = Quicksort_model

let rec partition : (pivot : int) -> (size : int) -> (lower : int) -> (scan : int) ->
    (loan : {s : int Slice.t |
      0 < size && 0 <= lower && lower <= scan && scan < size
      && Model.length (Slice.current s) === Bigint.of_int size
      && Spec.element (Slice.current s) (Bigint.of_int (size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0Z (Bigint.of_int lower)
      && Spec.range (Slice.current s) pivot false (Bigint.of_int lower) (Bigint.of_int scan)})
      @ local unique ->
    {r : (int, int Slice.t) step | let refine_ s = loan in
      0 <= r.value && r.value < size
      && Model.length (Slice.current r.state) === Bigint.of_int size
      && Spec.element (Slice.current r.state) (Bigint.of_int r.value) = pivot
      && Spec.range (Slice.current r.state) pivot true 0Z (Bigint.of_int r.value)
      && Spec.range (Slice.current r.state) pivot false
        (Bigint.of_int (r.value + 1)) (Bigint.of_int size)
      && Spec.permutation (Slice.current s) (Slice.current r.state)
      && Slice.final r.state === Slice.final s} @ local unique =
    fun pivot size lower scan loan -> exclave_ (
  let refine_ s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  let blo = ghost_ (Bigint.of_int lower) in
  let bscan = ghost_ (Bigint.of_int scan) in
  let blast = ghost_ (Bigint.of_int (size - 1)) in
  let zero = ghost_ 0Z in
  let low_side = true in
  let high_side = false in
  if scan < size - 1 then (
    let index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      refine_ scan in
    let refine_ read = Slice.get s index in
    let {value; state = s1} = read in
    ghost_ (Spec.element_def before bscan);
    let next_scan = scan + 1 in
    let next_lower, s2 =
      if value < pivot || (value = pivot && scan land 1 = 0) then (
        let first : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s1)) < 0} =
          refine_ lower in
        let second : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s1)) < 0} =
          refine_ scan in
        let refine_ s2 = Slice.swap s1 first second in
        ghost_ (Spec.swap_partition before pivot blo bscan);
        ghost_ (Spec.element_swap before blo bscan blast);
        ghost_ (Spec.permutation_swap before blo bscan);
        lower + 1, s2)
      else (
        ghost_ (Spec.accepts_def value pivot high_side);
        ghost_ (Spec.range_grow before pivot high_side blo bscan);
        ghost_ (Spec.permutation_refl before);
        lower, s1) in
    let intermediate = ghost_ (Slice.current (borrow_ s2)) in
    let next : {s : int Slice.t |
      0 < size && 0 <= next_lower && next_lower <= next_scan && next_scan < size
      && Model.length (Slice.current s) === Bigint.of_int size
      && Spec.element (Slice.current s) (Bigint.of_int (size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0Z (Bigint.of_int next_lower)
      && Spec.range (Slice.current s) pivot false
        (Bigint.of_int next_lower) (Bigint.of_int next_scan)} = refine_ s2 in
    let refine_ result = partition pivot size next_lower next_scan next in
    let {value; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    ghost_ (Spec.permutation_trans before intermediate after);
    let result = {value; state} in
    refine_ result)
  else (
    let first : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      refine_ lower in
    let second : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      refine_ scan in
    let refine_ state = Slice.swap s first second in
    let after = ghost_ (Slice.current (borrow_ state)) in
    let next_lower = ghost_ (Bigint.add blo 1Z) in
    ghost_ (Spec.swap_partition before pivot blo bscan);
    ghost_ (Spec.range_shrink after pivot low_side zero next_lower zero blo);
    ghost_ (Spec.element_swap before blo bscan blo);
    ghost_ (Spec.permutation_swap before blo bscan);
    let result = {value = lower; state} in
    refine_ result))
[@@decreases
  let size : int = size in
  let scan : int = scan in
  size - scan]

let rec (sort_sized @ portable) : (domains : int) -> (cutoff : int) -> (size : int) ->
    (loan : {s : int Slice.t | 0 <= size
      && Model.length (Slice.current s) === Bigint.of_int size}) @ local unique ->
    {u : unit | let refine_ s = loan in
      Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun domains cutoff size loan ->
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
    let blast = ghost_ (Bigint.of_int last) in
    let middle = size / 2 in
    let bmiddle = ghost_ (Bigint.of_int middle) in
    let middle_index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      refine_ middle in
    let last_index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      refine_ last in
    let refine_ s = Slice.swap s middle_index last_index in
    let seeded = ghost_ (Slice.current (borrow_ s)) in
    ghost_ (Spec.permutation_swap before bmiddle blast);
    let bzero = ghost_ 0Z in
    let low_side = true in
    let high_side = false in
    let index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      refine_ last in
    let refine_ read = Slice.get s index in
    let {value = (pivot : int); state = s1} = read in
    ghost_ (Spec.element_def seeded blast);
    ghost_ (Spec.range_empty seeded pivot low_side bzero bzero);
    ghost_ (Spec.range_empty seeded pivot high_side bzero bzero);
    let initial : {s : int Slice.t |
      0 < size && 0 <= zero && zero <= zero && zero < size
      && Model.length (Slice.current s) === Bigint.of_int size
      && Spec.element (Slice.current s) (Bigint.of_int (size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0Z (Bigint.of_int zero)
      && Spec.range (Slice.current s) pivot false (Bigint.of_int zero) (Bigint.of_int zero)} =
      refine_ s1 in
    let refine_ partitioned = partition pivot size zero zero initial in
    let {value = (boundary : int); state = s2} = partitioned in
    let divided = ghost_ (Slice.current (borrow_ s2)) in
    ghost_ (Spec.permutation_trans before seeded divided);
    let past = boundary + 1 in
    let right_size = size - past in
    let bboundary = ghost_ (Bigint.of_int boundary) in
    let bpast = ghost_ (Bigint.of_int past) in
    let first : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s2)) <= 0} =
      refine_ boundary in
    let past : {j : int | let refine_ i = first in i <= j
      && Bigint.compare (Bigint.of_int j) (Model.length (Slice.current s2)) <= 0} =
      refine_ past in
    let[@def] (post @ total) (u : unit @ immutable) (left : int Model.t @ immutable)
        (middle : int Model.t @ immutable) (right : int Model.t @ immutable) =
      ghost_ (Spec.sorted (Model.append left (Model.append middle right))
        && Spec.permutation divided (Model.append left (Model.append middle right))) in
    let erased_post = ghost_ post in
    let spawn = domains > 1 && boundary >= cutoff && right_size >= cutoff in
    let left_domains = if spawn then domains / 2 else domains in
    let right_domains = if spawn then domains - left_domains else domains in
    let refine_ result = Slice.split3 s2 first past erased_post (fun l m r ->
      let refine_ left = l in
      let refine_ middle = m in
      let refine_ right = r in
      let left_before = ghost_ (Slice.current (borrow_ left)) in
      let right_before = ghost_ (Slice.current (borrow_ right)) in
      let left_end = ghost_ (Slice.final (borrow_ left)) in
      let middle_end = ghost_ (Slice.final (borrow_ middle)) in
      let right_end = ghost_ (Slice.final (borrow_ right)) in
      let[@def] (left_post @ total) (after : int Model.t @ immutable) =
        ghost_ (Spec.sorted after && Spec.permutation left_before after) in
      let[@def] (right_post @ total) (after : int Model.t @ immutable) =
        ghost_ (Spec.sorted after && Spec.permutation right_before after) in
      let left_post_value = ghost_ left_post in
      let right_post_value = ghost_ right_post in
      ghost_ (Model.cut divided bboundary);
      ghost_ (Model.cut divided bpast);
      let refine_ children = Slice.parallel spawn left right left_post_value right_post_value
        (fun child ->
          let refine_ child = child in
          let after = ghost_ (Slice.final (borrow_ child)) in
          let sized : {s : int Slice.t | 0 <= boundary
            && Model.length (Slice.current s) === Bigint.of_int boundary} = refine_ child in
          let refine_ u = sort_sized left_domains cutoff boundary sized in
          ghost_ (left_post_def after);
          refine_ u)
        (fun child ->
          let refine_ child = child in
          let after = ghost_ (Slice.final (borrow_ child)) in
          let sized : {s : int Slice.t | 0 <= right_size
            && Model.length (Slice.current s) === Bigint.of_int right_size} = refine_ child in
          let refine_ u = sort_sized right_domains cutoff right_size sized in
          ghost_ (right_post_def after);
          refine_ u) in
      ghost_ (left_post_def left_end);
      ghost_ (right_post_def right_end);
      Slice.finish middle;
      ghost_ (Spec.glue_partition divided pivot bboundary left_end middle_end right_end);
      let u = () in
      ghost_ (post_def u left_end middle_end right_end);
      refine_ u) in
    let {value = u; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    let left = ghost_ (Model.take bboundary after) in
    let middle = ghost_ (Model.sub after bboundary bpast) in
    let right = ghost_ (Model.drop bpast after) in
    ghost_ (post_def u left middle right);
    ghost_ (Spec.decompose3 after bboundary bpast);
    ghost_ (Spec.permutation_trans before divided after);
    Slice.finish state;
    refine_ u)

let (sort_with_budget @ portable) : (domains : int) -> (cutoff : int) -> (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun domains cutoff s ->
  let refine_ sized = Slice.length s in
  let {value = (size : int); state} = sized in
  let sized : {s : int Slice.t | 0 <= size
    && Model.length (Slice.current s) === Bigint.of_int size} = refine_ state in
  let refine_ u = sort_sized domains cutoff size sized in
  refine_ u

let (sort_array_with_budget @ portable) : (domains : int) -> (cutoff : int) ->
    (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun domains cutoff a ->
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let[@def] (post @ total) (u : unit @ immutable) (after : int Model.t @ immutable) =
    ghost_ (Spec.sorted after && Spec.permutation before after) in
  let erased_post = ghost_ post in
  let refine_ result = Owned_array.with_mut a erased_post (fun loan ->
    let refine_ s = loan in
    let eventual = ghost_ (Slice.final (borrow_ s)) in
    let refine_ u = sort_with_budget domains cutoff s in
    ghost_ (post_def u eventual);
    refine_ u) in
  let {value = u; state} = result in
  let after = ghost_ (Owned_array.contents (borrow_ state)) in
  ghost_ (post_def u after);
  refine_ state

let (sort @ portable) : (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun s ->
  let refine_ u = sort_with_budget 1 512 s in
  refine_ u

let (sort_array @ portable) : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique = fun a ->
  let refine_ result = sort_array_with_budget 1 512 a in
  refine_ result

let (parallel_sort @ portable) : ?max_domains:int -> ?cutoff:int -> (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) s ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let refine_ u = sort_with_budget domains cutoff s in
  refine_ u

let (parallel_sort_array @ portable) : ?max_domains:int -> ?cutoff:int -> (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) a ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let refine_ result = sort_array_with_budget domains cutoff a in
  refine_ result
