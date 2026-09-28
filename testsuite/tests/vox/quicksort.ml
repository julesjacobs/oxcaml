open Borrow
module Spec = Vox_int_sequence

external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"
  [@@warning "-trusted-external"]
let (half @ total) : (value : int) ->
    {result : int | if value > 0 then 0 <= result && result < value else true} =
    fun value ->
  let two = 2 in
  let result = divide value (two) in
  result

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
    let value = Slice.get (borrow_ s) scan in
    ghost_ (Spec.element_def before bscan);
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
    let result = partition pivot size next_lower next_scan s2 in
    let {value; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    ghost_ (Spec.permutation_trans before intermediate after);
    let result = {value; state} in
    result)
  else (
    let state = Slice.swap s lower scan in
    ghost_ (Quicksort_model.finish_partition before pivot blo bscan);
    let result = {value = lower; state} in
    result))
[@@decreases
  let size : int = size in
  let scan : int = scan in
  size - scan]

(* Swaps the middle element to the end and partitions around it. The pivot
   ends at [r.value]: everything before it is at most the pivot, everything
   after it at least the pivot. *)
let (partition_middle @ total) : (size : int) ->
    (loan : {s : int Slice.t | 1 < size
      && Model.length (Slice.current s) === Bigint.of_int size}) @ local unique ->
    {r : (int, int Slice.t) step | let s = loan in
      Quicksort_model.partitioned (Slice.current r.state) (Bigint.of_int r.value)
      && Model.length (Slice.current r.state) === Bigint.of_int size
      && Spec.permutation (Slice.current s) (Slice.current r.state)
      && Slice.final r.state === Slice.final s} @ local unique =
    fun size loan -> exclave_ (
  let s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  let zero = 0 in
  let last = size - 1 in
  let blast = ghost_ (Bigint.of_int last) in
  let middle = half size in
  let bmiddle = ghost_ (Bigint.of_int middle) in
  let s = Slice.swap s middle last in
  let seeded = ghost_ (Slice.current (borrow_ s)) in
  let pivot = Slice.get (borrow_ s) last in
  ghost_ (Quicksort_model.initialize_partition before seeded pivot bmiddle blast);
  let partitioned = partition pivot size zero zero s in
  let {value; state} = partitioned in
  let divided = ghost_ (Slice.current (borrow_ state)) in
  ghost_ (Spec.permutation_trans before seeded divided);
  ghost_ (Quicksort_model.partitioned_def divided (Bigint.of_int value));
  let result = {value; state} in
  result)

let rec sort_sized : (size : int) ->
    (loan : {s : int Slice.t | 0 <= size
      && Model.length (Slice.current s) === Bigint.of_int size}) @ local unique ->
    {u : unit | let s = loan in
      Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} =
    fun size loan ->
  let s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  if size <= 1 then (
    ghost_ (Spec.sorted_short before);
    ghost_ (Spec.permutation_refl before);
    Slice.finish s;
    ())
  else (
    let partitioned = partition_middle size s in
    let {value = (boundary : int); state = s2} = partitioned in
    let divided = ghost_ (Slice.current (borrow_ s2)) in
    let past = boundary + 1 in
    let right_size = size - past in
    let bboundary = ghost_ (Bigint.of_int boundary) in
    let bpast = ghost_ (Bigint.of_int past) in
    ghost_ (Quicksort_model.partitioned_def divided bboundary);
    let post = ghost_ (fun (_ : unit @ immutable) (left : int Model.t @ immutable)
        (middle : int Model.t @ immutable) (right : int Model.t @ immutable) ->
        Spec.sorted (Model.append left (Model.append middle right))
        && Spec.permutation divided (Model.append left (Model.append middle right))) in
    let result = Slice.split3 s2 boundary past post (fun left middle right ->
      let left_end = ghost_ (Slice.final (borrow_ left)) in
      let middle_end = ghost_ (Slice.final (borrow_ middle)) in
      let right_end = ghost_ (Slice.final (borrow_ right)) in
      ghost_ (Model.cut divided bboundary);
      ghost_ (Model.cut divided bpast);
      sort_sized boundary left;
      sort_sized right_size right;
      Slice.finish middle;
      ghost_ (Quicksort_model.glue_partition divided bboundary
        left_end middle_end right_end);
      ()) in
    let {value = u; state} = result in
    let after = ghost_ (Slice.current (borrow_ state)) in
    ghost_ (Model.decompose3 after bboundary bpast);
    ghost_ (Spec.permutation_trans before divided after);
    Slice.finish state;
    u)
[@@decreases size]

let sort : (s : int Slice.t) @ local unique ->
    {u : unit | Spec.sorted (Slice.final s)
      && Spec.permutation (Slice.current s) (Slice.final s)} = fun s ->
  let size = Slice.length (borrow_ s) in
  sort_sized size s

let sort_array : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun a ->
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let post = ghost_ (fun (_ : unit @ immutable) (after : int Model.t @ immutable) ->
        Spec.sorted after && Spec.permutation before after) in
  let result = Owned_array.with_mut a post (fun loan -> sort loan) in
  let {value = u; state} = result in
  state

(* The parallel sort partitions an owned array in place, splits it into owned
   pieces without copying, sorts the two sides with the ordinary polymorphic
   [Vox_parallel.fork_join], and appends the pieces again without copying. *)
let rec (sort_owned @ portable) : (domains : int) -> (cutoff : int) ->
    (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun domains cutoff a ->
  let size = Owned_array.length (borrow_ a) in
  if domains <= 1 || size <= 1 || half (size - 1) < cutoff then sort_array a
  else (
    let before = ghost_ (Owned_array.contents (borrow_ a)) in
    let post = ghost_ (fun (boundary : int @ immutable) (after : int Model.t @ immutable) ->
      Quicksort_model.partitioned after (Bigint.of_int boundary)
      && Model.length after === Bigint.of_int size
      && Spec.permutation before after) in
    let stepped = Owned_array.with_mut a post (fun loan ->
      let {value; state} = partition_middle size loan in
      Slice.finish state;
      value) in
    let {value = boundary; state = partitioned} = stepped in
    let divided = ghost_ (Owned_array.contents (borrow_ partitioned)) in
    let bboundary = ghost_ (Bigint.of_int boundary) in
    let bpast = ghost_ (Bigint.of_int (boundary + 1)) in
    ghost_ (Quicksort_model.partitioned_def divided bboundary);
    ghost_ (Model.cut divided bboundary);
    let left, rest = Owned_array.split_at partitioned boundary in
    let middle, right = Owned_array.split_at rest 1 in
    let left_before = ghost_ (Owned_array.contents (borrow_ left)) in
    let right_before = ghost_ (Owned_array.contents (borrow_ right)) in
    let spawn = boundary >= cutoff && size - boundary - 1 >= cutoff in
    let left_domains = if spawn then half domains else domains in
    let right_domains = if spawn then domains - left_domains else domains in
    let sort_left () : {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
        && Spec.permutation left_before (Owned_array.contents r)} =
      sort_owned left_domains cutoff left in
    let sort_right () : {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
        && Spec.permutation right_before (Owned_array.contents r)} =
      sort_owned right_domains cutoff right in
    let sorted_left, sorted_right =
      if spawn then Vox_parallel.fork_join sort_left sort_right
      else (
        let l = sort_left () in
        l, sort_right ()) in
    let left_after = ghost_ (Owned_array.contents (borrow_ sorted_left)) in
    let middle_after = ghost_ (Owned_array.contents (borrow_ middle)) in
    let right_after = ghost_ (Owned_array.contents (borrow_ sorted_right)) in
    ghost_ (Model.sub_def divided bboundary bpast);
    ghost_ (Model.drop_add divided bboundary 1Z);
    ghost_ (Quicksort_model.glue_partition divided bboundary
      left_after middle_after right_after);
    let rest = Owned_array.append middle sorted_right in
    let result = Owned_array.append sorted_left rest in
    ghost_ (Spec.permutation_trans before divided (Owned_array.contents (borrow_ result)));
    result)

let (parallel_sort_array @ portable) : ?max_domains:int -> ?cutoff:int ->
    (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)} @ unique =
    fun ?(max_domains = Domain.recommended_domain_count ()) ?(cutoff = 512) a ->
  let domains = max 1 (min max_domains (Domain.recommended_domain_count ())) in
  let cutoff = max 2 cutoff in
  let result = sort_owned domains cutoff a in
  result
