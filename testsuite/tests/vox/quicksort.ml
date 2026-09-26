open Borrow
module Spec = Vox_int_sequence

external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"
let (half @ total) : (value : int) ->
    {result : int | if value > 0 then 0 <= result && result < value else true} =
    fun value ->
  let two = 2 in
  let result = divide value (two) in
  result

type runner =
  (spawn : bool) ->
  (left : int Slice.t) @ local unique -> (right : int Slice.t) @ local unique ->
  (lp : (int Model.t @ immutable -> bool @ ghost)) @ ghost ->
  (rp : (int Model.t @ immutable -> bool @ ghost)) @ ghost ->
  ((s : {s : int Slice.t | Slice.current s === Slice.current left
      && Slice.final s === Slice.final left}) @ local unique ->
    {u : unit | let s = s in lp (Slice.final s)}) @ portable once ->
  ((s : {s : int Slice.t | Slice.current s === Slice.current right
      && Slice.final s === Slice.final right}) @ local unique ->
    {u : unit | let s = s in rp (Slice.final s)}) @ portable once ->
  {u : unit | lp (Slice.final left) && rp (Slice.final right)}

let (sequential_runner @ portable total) : runner =
    fun _spawn left right lp rp lf rf ->
  let left_arg : {s : int Slice.t | Slice.current s === Slice.current left
    && Slice.final s === Slice.final left} = left in
  let right_arg : {s : int Slice.t | Slice.current s === Slice.current right
    && Slice.final s === Slice.final right} = right in
  let _l = lf left_arg in
  let _r = rf right_arg in
  let u = () in u

let (goes_left @ total) (value : int) (pivot : int) (scan : int) :
    {left : bool | if left then value <= pivot else pivot <= value} =
  let left = value < pivot || (value = pivot && scan land 1 = 0) in
  left

let rec (partition @ total) : (pivot : int) -> (size : int) ->
    (lower : int) -> (scan : int) ->
    (loan : {s : int Slice.t |
      0 < size && 0 <= lower && lower <= scan && scan < size
      && Model.length (Slice.current s) === Bigint.of_int size
      && Spec.element (Slice.current s) (Bigint.of_int (size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0Z (Bigint.of_int lower)
      && Spec.range (Slice.current s) pivot false (Bigint.of_int lower) (Bigint.of_int scan)})
      @ local unique ->
    {r : (int, int Slice.t) step | let s = loan in
      0 <= r.value && r.value < size
      && Model.length (Slice.current r.state) === Bigint.of_int size
      && Spec.element (Slice.current r.state) (Bigint.of_int r.value) = pivot
      && Spec.range (Slice.current r.state) pivot true 0Z (Bigint.of_int r.value)
      && Spec.range (Slice.current r.state) pivot false
        (Bigint.of_int (r.value + 1)) (Bigint.of_int size)
      && Spec.permutation (Slice.current s) (Slice.current r.state)
      && Slice.final r.state === Slice.final s} @ local unique =
    fun pivot size lower scan loan -> exclave_ (
  let s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  let blo = ghost_ (Bigint.of_int lower) in
  let bscan = ghost_ (Bigint.of_int scan) in
  let blast = ghost_ (Bigint.of_int (size - 1)) in
  if scan < size - 1 then (
    let index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      scan in
    let value = Slice.get (borrow_ s) index in
    ghost_ (Spec.element_def before bscan);
    ghost_ (
      let u = () in
      (u : {u : unit | 0 <= lower && lower < size
        && Bigint.of_int lower < Model.length (Slice.current s)}));
    let next_scan = scan + 1 in
    let left = goes_left value pivot scan in
    let next_lower, s2 =
      if left then (
        let first : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
          lower in
        let second : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
          scan in
        let s2 = Slice.swap s first second in
        ghost_ (Quicksort_model.scan_left before pivot blo bscan blast);
        lower + 1, s2)
      else (
        ghost_ (Quicksort_model.scan_right before pivot blo bscan);
        lower, s) in
    let intermediate = ghost_ (Slice.current (borrow_ s2)) in
    let next : {s : int Slice.t |
      0 < size && 0 <= next_lower && next_lower <= next_scan && next_scan < size
      && Model.length (Slice.current s) === Bigint.of_int size
      && Spec.element (Slice.current s) (Bigint.of_int (size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0Z (Bigint.of_int next_lower)
      && Spec.range (Slice.current s) pivot false
        (Bigint.of_int next_lower) (Bigint.of_int next_scan)} = s2 in
    let result = partition pivot size next_lower next_scan next in
    let {value; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    ghost_ (Spec.permutation_trans before intermediate after);
    let result = {value; state} in
    result)
  else (
    let first : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      lower in
    let second : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      scan in
    let state = Slice.swap s first second in
    ghost_ (Quicksort_model.finish_partition before pivot blo bscan);
    let result = {value = lower; state} in
    result))
[@@decreases
  let size : int = size in
  let scan : int = scan in
  size - scan]

let rec (sort_sized @ portable total) : (run : runner) @ portable ->
    (domains : int) -> (cutoff : int) -> (size : int) ->
    (loan : {s : int Slice.t | 0 <= size
      && Model.length (Slice.current s) === Bigint.of_int size}) @ local unique ->
    {u : unit | let s = loan in
      Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun run domains cutoff size loan ->
  let s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  if size <= 1 then (
    ghost_ (Spec.sorted_short before);
    ghost_ (Spec.permutation_refl before);
    Slice.finish s;
    let u = () in u)
  else (
    let zero = 0 in
    let last = size - 1 in
    let blast = ghost_ (Bigint.of_int last) in
    let middle = half size in
    let bmiddle = ghost_ (Bigint.of_int middle) in
    let middle_index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      middle in
    let last_index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      last in
    let s = Slice.swap s middle_index last_index in
    let seeded = ghost_ (Slice.current (borrow_ s)) in
    let index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} =
      last in
    let pivot = Slice.get (borrow_ s) index in
    let (pivot : int) = pivot in
    ghost_ (Quicksort_model.initialize_partition before seeded pivot bmiddle blast);
    let initial : {s : int Slice.t |
      0 < size && 0 <= zero && zero <= zero && zero < size
      && Model.length (Slice.current s) === Bigint.of_int size
      && Spec.element (Slice.current s) (Bigint.of_int (size - 1)) = pivot
      && Spec.range (Slice.current s) pivot true 0Z (Bigint.of_int zero)
      && Spec.range (Slice.current s) pivot false (Bigint.of_int zero) (Bigint.of_int zero)} =
      s in
    let partitioned = partition pivot size zero zero initial in
    let {value = (boundary : int); state = s2} = partitioned in
    let divided = ghost_ (Slice.current (borrow_ s2)) in
    ghost_ (Spec.permutation_trans before seeded divided);
    let past = boundary + 1 in
    let right_size = size - past in
    let bboundary = ghost_ (Bigint.of_int boundary) in
    let bpast = ghost_ (Bigint.of_int past) in
    let first : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s2)) <= 0} =
      boundary in
    let past : {j : int | let i = first in i <= j
      && Bigint.compare (Bigint.of_int j) (Model.length (Slice.current s2)) <= 0} =
      past in
    let post = ghost_ (fun (_ : unit @ immutable) (left : int Model.t @ immutable)
        (middle : int Model.t @ immutable) (right : int Model.t @ immutable) ->
        Spec.sorted (Model.append left (Model.append middle right))
        && Spec.permutation divided (Model.append left (Model.append middle right))) in
    let spawn = domains > 1 && boundary >= cutoff && right_size >= cutoff in
    let left_domains =
      if spawn then let n = half domains in n else domains in
    let right_domains = if spawn then domains - left_domains else domains in
    let result = Slice.split3 s2 first past post (fun l m r ->
      let left = l in
      let middle = m in
      let right = r in
      let left_before = ghost_ (Slice.current (borrow_ left)) in
      let right_before = ghost_ (Slice.current (borrow_ right)) in
      let left_end = ghost_ (Slice.final (borrow_ left)) in
      let middle_end = ghost_ (Slice.final (borrow_ middle)) in
      let right_end = ghost_ (Slice.final (borrow_ right)) in
      let left_post = ghost_ (fun (after : int Model.t @ immutable) ->
        Spec.sorted after && Spec.permutation left_before after) in
      let right_post = ghost_ (fun (after : int Model.t @ immutable) ->
        Spec.sorted after && Spec.permutation right_before after) in
      ghost_ (Model.cut divided bboundary);
      ghost_ (Model.cut divided bpast);
      let _children = run spawn left right left_post right_post
        (fun child ->
          let child = child in
          let sized : {s : int Slice.t | 0 <= boundary
            && Model.length (Slice.current s) === Bigint.of_int boundary} = child in
          let u = sort_sized run left_domains cutoff boundary sized in
          u)
        (fun child ->
          let child = child in
          let sized : {s : int Slice.t | 0 <= right_size
            && Model.length (Slice.current s) === Bigint.of_int right_size} = child in
          let u = sort_sized run right_domains cutoff right_size sized in
          u) in
      Slice.finish middle;
      ghost_ (Quicksort_model.glue_partition divided pivot bboundary left_end middle_end right_end);
      let u = () in
      u) in
    let {value = u; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    ghost_ (Model.decompose3 after bboundary bpast);
    ghost_ (Spec.permutation_trans before divided after);
    Slice.finish state;
    u)
[@@decreases size]

let (sort_with_budget @ portable total) : (run : runner) @ portable ->
    (domains : int) -> (cutoff : int) -> (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun run domains cutoff s ->
  let size = Slice.length (borrow_ s) in
  let (size : int) = size in
  let sized : {s : int Slice.t | 0 <= size
    && Model.length (Slice.current s) === Bigint.of_int size} = s in
  let u = sort_sized run domains cutoff size sized in
  u

let (sort_array_with_budget @ portable total) : (run : runner) @ portable ->
    (domains : int) -> (cutoff : int) ->
    (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun run domains cutoff a ->
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let post = ghost_ (fun (_ : unit @ immutable) (after : int Model.t @ immutable) ->
        Spec.sorted after && Spec.permutation before after) in
  let result = Owned_array.with_mut a post (fun loan ->
    let s = loan in
    let u = sort_with_budget run domains cutoff s in
    u) in
  let {value = u; state} = result in
  state

let (sort @ portable total) : (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun s ->
  let u = sort_with_budget sequential_runner 1 512 s in
  u

let (sort_array @ portable total) : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique = fun a ->
  let result = sort_array_with_budget sequential_runner 1 512 a in
  result

let (parallel_sort @ portable) : ?max_domains:int -> ?cutoff:int -> (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) s ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let u = sort_with_budget Slice.parallel domains cutoff s in
  u

let (parallel_sort_array @ portable) : ?max_domains:int -> ?cutoff:int -> (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) a ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let result = sort_array_with_budget Slice.parallel domains cutoff a in
  result
