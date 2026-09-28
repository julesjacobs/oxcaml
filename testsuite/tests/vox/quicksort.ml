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

(* The same partition on an owned array. [Owned_array.get] and
   [Owned_array.swap] take the array and return it, so these functions are
   [total] and no loan is created. *)
let rec (partition_array @ total) : (pivot : int) -> (size : int) ->
    (lower : int) -> (scan : int) ->
    (owner : {a : int Owned_array.t |
      0 < size && 0 <= lower && lower <= scan && scan < size
      && Model.length (Owned_array.contents a) === Bigint.of_int size
      && Spec.element (Owned_array.contents a) (Bigint.of_int (size - 1))
        = pivot
      && Spec.range (Owned_array.contents a) pivot true 0Z (Bigint.of_int lower)
      && Spec.range (Owned_array.contents a) pivot false (Bigint.of_int lower)
        (Bigint.of_int scan)}) @ unique ->
    {r : (int, int Owned_array.t) step | let a = owner in
      0 <= r.value && r.value < size
      && Model.length (Owned_array.contents r.state) === Bigint.of_int size
      && Spec.element (Owned_array.contents r.state) (Bigint.of_int r.value)
        = pivot
      && Spec.range (Owned_array.contents r.state) pivot true 0Z
        (Bigint.of_int r.value)
      && Spec.range (Owned_array.contents r.state) pivot false
        (Bigint.of_int (r.value + 1)) (Bigint.of_int size)
      && Spec.permutation (Owned_array.contents a)
        (Owned_array.contents r.state)} @ unique =
    fun pivot size lower scan owner ->
  let a = owner in
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let blo = ghost_ (Bigint.of_int lower) in
  let bscan = ghost_ (Bigint.of_int scan) in
  let blast = ghost_ (Bigint.of_int (size - 1)) in
  if scan < size - 1 then (
    let value = Owned_array.get (borrow_ a) scan in
    ghost_ (Spec.element_def before bscan);
    let next_scan = scan + 1 in
    let left = goes_left value pivot scan in
    let next_lower, a2 =
      if left then (
        let a2 = Owned_array.swap a lower scan in
        ghost_ (Quicksort_model.scan_left before pivot blo bscan blast);
        lower + 1, a2)
      else (
        ghost_ (Quicksort_model.scan_right before pivot blo bscan);
        lower, a) in
    let intermediate = ghost_ (Owned_array.contents (borrow_ a2)) in
    let result = partition_array pivot size next_lower next_scan a2 in
    let {value; state} = result in
    let after = ghost_ (Owned_array.contents (borrow_ state)) in
    ghost_ (Spec.permutation_trans before intermediate after);
    let result = {value; state} in
    result)
  else (
    let state = Owned_array.swap a lower scan in
    ghost_ (Quicksort_model.finish_partition before pivot blo bscan);
    let result = {value = lower; state} in
    result)
[@@decreases
  let size : int = size in
  let scan : int = scan in
  size - scan]

let (partition_middle_array @ total) : (size : int) ->
    (owner : {a : int Owned_array.t | 1 < size
      && Model.length (Owned_array.contents a) === Bigint.of_int size})
      @ unique ->
    {r : (int, int Owned_array.t) step | let a = owner in
      Quicksort_model.partitioned (Owned_array.contents r.state)
        (Bigint.of_int r.value)
      && Model.length (Owned_array.contents r.state) === Bigint.of_int size
      && Spec.permutation (Owned_array.contents a)
        (Owned_array.contents r.state)} @ unique =
    fun size owner ->
  let a = owner in
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let zero = 0 in
  let last = size - 1 in
  let blast = ghost_ (Bigint.of_int last) in
  let middle = half size in
  let bmiddle = ghost_ (Bigint.of_int middle) in
  let a = Owned_array.swap a middle last in
  let seeded = ghost_ (Owned_array.contents (borrow_ a)) in
  let pivot = Owned_array.get (borrow_ a) last in
  ghost_ (Quicksort_model.initialize_partition before seeded pivot bmiddle
    blast);
  let partitioned = partition_array pivot size zero zero a in
  let {value; state} = partitioned in
  let divided = ghost_ (Owned_array.contents (borrow_ state)) in
  ghost_ (Spec.permutation_trans before seeded divided);
  ghost_ (Quicksort_model.partitioned_def divided (Bigint.of_int value));
  let result = {value; state} in
  result

(* Splits a partitioned array into the left side, the pivot and the right
   side, without copying. *)
let (split_partition @ total) : (boundary : int) ->
    (owner : {a : int Owned_array.t |
      Quicksort_model.partitioned (Owned_array.contents a)
        (Bigint.of_int boundary)}) @ unique ->
    {r : int Owned_array.t * int Owned_array.t * int Owned_array.t |
      let a = owner in let k = boundary in
      match r with left, middle, right ->
        Owned_array.contents left ===
          Model.take (Bigint.of_int k) (Owned_array.contents a)
        && Owned_array.contents middle ===
          Model.sub (Owned_array.contents a) (Bigint.of_int k)
            (Bigint.of_int (k + 1))
        && Owned_array.contents right ===
          Model.drop (Bigint.of_int (k + 1)) (Owned_array.contents a)
        && Model.length (Owned_array.contents left) === Bigint.of_int k
        && Model.length (Owned_array.contents right) ===
          Bigint.sub (Model.length (Owned_array.contents a))
            (Bigint.of_int (k + 1))} @ unique =
    fun boundary owner ->
  let a = owner in
  let divided = ghost_ (Owned_array.contents (borrow_ a)) in
  let bboundary = ghost_ (Bigint.of_int boundary) in
  let bpast = ghost_ (Bigint.of_int (boundary + 1)) in
  ghost_ (Quicksort_model.partitioned_def divided bboundary);
  ghost_ (Model.cut divided bboundary);
  ghost_ (Model.cut divided bpast);
  let left, rest = Owned_array.split_at a boundary in
  let middle, right = Owned_array.split_at rest 1 in
  ghost_ (Model.sub_def divided bboundary bpast);
  ghost_ (Model.drop_add divided bboundary 1Z);
  left, middle, right

(* Puts the three pieces back together; they are adjacent, so nothing is
   copied. *)
let (join_partition @ total) : (divided : int Model.t) @ ghost ->
    (boundary : {k : int | Quicksort_model.partitioned divided (Bigint.of_int k)
      && Bigint.compare (Model.length divided)
        (Bigint.of_int (max_length ())) <= 0}) ->
    (left : {a : int Owned_array.t | let k = boundary in
      Spec.sorted (Owned_array.contents a)
      && Spec.permutation (Model.take (Bigint.of_int k) divided)
        (Owned_array.contents a)
      && Model.length (Owned_array.contents a) === Bigint.of_int k}) @ unique ->
    (middle : {a : int Owned_array.t | let k = boundary in
      Owned_array.contents a ===
        Model.sub divided (Bigint.of_int k) (Bigint.of_int (k + 1))})
      @ unique ->
    (right : {a : int Owned_array.t | let k = boundary in
      Spec.sorted (Owned_array.contents a)
      && Spec.permutation (Model.drop (Bigint.of_int (k + 1)) divided)
        (Owned_array.contents a)
      && Model.length (Owned_array.contents a) ===
        Bigint.sub (Model.length divided) (Bigint.of_int (k + 1))}) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation divided (Owned_array.contents r)
      && Model.length (Owned_array.contents r) === Model.length divided}
      @ unique =
    fun divided boundary left middle right ->
  let bboundary = ghost_ (Bigint.of_int boundary) in
  let bpast = ghost_ (Bigint.of_int (boundary + 1)) in
  let left_after = ghost_ (Owned_array.contents (borrow_ left)) in
  let middle_after = ghost_ (Owned_array.contents (borrow_ middle)) in
  let right_after = ghost_ (Owned_array.contents (borrow_ right)) in
  ghost_ (Quicksort_model.partitioned_def divided bboundary);
  ghost_ (Model.sub_length divided bboundary bpast);
  ghost_ (Quicksort_model.glue_partition divided bboundary
    left_after middle_after right_after);
  let rest = Owned_array.append middle right in
  ghost_ (Model.append_length middle_after right_after);
  let result = Owned_array.append left rest in
  ghost_ (Model.append_length left_after
    (Model.append middle_after right_after));
  result

let rec (sort_array_sized @ total) : (size : int) ->
    (owner : {a : int Owned_array.t | 0 <= size && size <= max_length ()
      && Model.length (Owned_array.contents a) === Bigint.of_int size})
      @ unique ->
    {r : int Owned_array.t | let a = owner in
      Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)
      && Model.length (Owned_array.contents r) === Bigint.of_int size}
      @ unique =
    fun size owner ->
  let a = owner in
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  if size <= 1 then (
    ghost_ (Spec.sorted_short before);
    ghost_ (Spec.permutation_refl before);
    a)
  else (
    let partitioned = partition_middle_array size a in
    let {value = boundary; state = divided_array} = partitioned in
    let divided = ghost_ (Owned_array.contents (borrow_ divided_array)) in
    ghost_ (Quicksort_model.partitioned_def divided (Bigint.of_int boundary));
    let left, middle, right = split_partition boundary divided_array in
    let right_size = size - boundary - 1 in
    let sorted_left = sort_array_sized boundary left in
    let sorted_right = sort_array_sized right_size right in
    let result = join_partition divided boundary sorted_left middle
      sorted_right in
    ghost_ (Spec.permutation_trans before divided
      (Owned_array.contents (borrow_ result)));
    result)
[@@decreases size]

let (sort_array @ total) : (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)}
      @ unique =
    fun a ->
  let size = Owned_array.length (borrow_ a) in
  let result = sort_array_sized size a in
  result

(* The parallel sort partitions and splits the owned array in the same way
   and sorts the two sides with the ordinary polymorphic
   [Vox_parallel.fork_join]. *)
let rec (sort_owned @ portable) : (domains : int) -> (cutoff : int) ->
    (a : int Owned_array.t) @ unique ->
    {r : int Owned_array.t | Spec.sorted (Owned_array.contents r)
      && Spec.permutation (Owned_array.contents a) (Owned_array.contents r)
      && Model.length (Owned_array.contents r) ===
        Model.length (Owned_array.contents a)} @ unique =
    fun domains cutoff a ->
  let size = Owned_array.length (borrow_ a) in
  if domains <= 1 || size <= 1 || half (size - 1) < cutoff then
    sort_array_sized size a
  else (
    let before = ghost_ (Owned_array.contents (borrow_ a)) in
    let partitioned = partition_middle_array size a in
    let {value = boundary; state = divided_array} = partitioned in
    let divided = ghost_ (Owned_array.contents (borrow_ divided_array)) in
    ghost_ (Quicksort_model.partitioned_def divided (Bigint.of_int boundary));
    let left, middle, right = split_partition boundary divided_array in
    let left_before = ghost_ (Owned_array.contents (borrow_ left)) in
    let right_before = ghost_ (Owned_array.contents (borrow_ right)) in
    let spawn = boundary >= cutoff && size - boundary - 1 >= cutoff in
    let left_domains = if spawn then half domains else domains in
    let right_domains = if spawn then domains - left_domains else domains in
    let sort_left () : {r : int Owned_array.t |
        Spec.sorted (Owned_array.contents r)
        && Spec.permutation left_before (Owned_array.contents r)
        && Model.length (Owned_array.contents r) === Model.length left_before} =
      sort_owned left_domains cutoff left in
    let sort_right () : {r : int Owned_array.t |
        Spec.sorted (Owned_array.contents r)
        && Spec.permutation right_before (Owned_array.contents r)
        && Model.length (Owned_array.contents r) ===
          Model.length right_before} =
      sort_owned right_domains cutoff right in
    let sorted_left, sorted_right =
      if spawn then Vox_parallel.fork_join sort_left sort_right
      else (
        let l = sort_left () in
        l, sort_right ()) in
    let result = join_partition divided boundary sorted_left middle
      sorted_right in
    ghost_ (Spec.permutation_trans before divided
      (Owned_array.contents (borrow_ result)));
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
