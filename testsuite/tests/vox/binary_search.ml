(* TEST
 has-z3;
 {
   flags = "-extension refinement_types";
   { expect; }
   { expect.opt; }
 }{
   flags = "-extension refinement_types -principal";
   { expect; }
   { expect.opt; }
 }
*)

module Binary = struct
  external divide : int -> {d : int | d <> 0} -> int @@ total = "%divint"

  type splitter =
      (left : int) -> (right : int) ->
      {u : unit | -1 <= left && left < right && 0 < right - left} @ ghost ->
      {m : int option | match m with
        | None -> right = left + 1
        | Some m -> left < m && m < right}

  let (midpoint @ total) : splitter =
    fun left right premise ->
    let refine_ premise = premise in
    let distance = right - left in
    if distance > 1 then
      let two = 2 in
      let half = divide distance (refine_ two) in
      let result = Some (left + half) in
      refine_ result
    else
      let result = None in
      refine_ result

  let (forward @ total) : splitter = fun left right premise ->
    let refine_ premise = premise in
    let result = if right - left > 1 then Some (left + 1) else None in
    refine_ result

  let (backward @ total) : splitter = fun left right premise ->
    let refine_ premise = premise in
    let result = if right - left > 1 then Some (right - 1) else None in
    refine_ result

  let (search @ total) :
      (split : splitter) @ total -> (p : (int -> bool)) ->
      (lower : int) -> (upper : int) ->
      {u : unit | -1 <= lower && lower < upper && 0 < upper - lower
        && not (p lower) && p upper} @ ghost ->
      {result : int * int | match result with left, right ->
        lower <= left && right <= upper && right = left + 1
        && not (p left) && p right} =
    fun split p lower upper premise ->
    let refine_ premise = premise in
    let (evaluate @ total) :
        (index : {i : int | lower < i && i < upper}) ->
        {b : bool | let refine_ i = index in b = p i} =
      fun index ->
      let refine_ i = index in
      let b = p i in
      refine_ b
    in
    let rec (loop @ total) :
        (left : int) -> (right : int) ->
        {u : unit | -1 <= left && 0 < right - left
          && lower <= left && left < right && right <= upper
          && not (p left) && p right} @ ghost ->
        {result : int * int | match result with l, r ->
          left <= l && r <= right && r = l + 1
          && not (p l) && p r} =
      fun left right invariant ->
      let refine_ invariant = invariant in
      let u = () in
      let refine_ middle = split left right (refine_ u) in
      match middle with
      | None ->
        let result = left, right in
        refine_ result
      | Some middle ->
        let index : {i : int | lower < i && i < upper} =
          refine_ middle in
        let refine_ yes = evaluate index in
        if yes then
          let refine_ result = loop left middle (refine_ u) in
          refine_ result
        else
          let refine_ result = loop middle right (refine_ u) in
          refine_ result
    [@@decreases right - left]
    in
    let u = () in
    let refine_ result = loop lower upper (refine_ u) in
    refine_ result

end;;
[%%expect{|
module Binary :
  sig
    external divide : int -> {d : int | d <> 0} -> int = "%divint"
    type splitter =
        (left : int) ->
        (right : int) ->
        {u : unit
          | ((-1) <= left) && ((left < right) && (0 < (right - left)))} @ ghost ->
        {m' : int option
          | match m' with
            | None -> right = (left + 1)
            | Some m -> (left < m) && (m < right)}
    val midpoint : splitter
    val forward : splitter
    val backward : splitter
    val search :
      splitter @ total ->
      ((p : (int -> bool)) ->
       (lower : int) ->
       (upper : int) ->
       {u : unit
         | ((-1) <= lower) &&
             ((lower < upper) &&
                ((0 < (upper - lower)) && ((not (p lower)) && (p upper))))} @ ghost ->
       {result : int * int
         | match result with
           | (left, right) ->
               (lower <= left) &&
                 ((right <= upper) &&
                    ((right = (left + 1)) && ((not (p left)) && (p right))))}) @ total
      stateful
  end
|}]

module Arrays = struct
  let[@def] at (array : int iarray) (index : int) =
    if 0 <= index && index < Iarray.length array then
      let bounded : {i : int | 0 <= i && i < Iarray.length array} =
        refine_ index in
      Iarray.Refined.get array bounded
    else 0

  let[@def] above (array : int iarray) (target : int) (strict : bool)
      (index : int) =
    if index < 0 then false
    else if Iarray.length array <= index then true
    else if strict then at array index > target else at array index >= target

  let (bounds @ total) :
      (array : int iarray) -> (target : int) -> (strict : bool) ->
      {u : unit | 0 < Iarray.length array + 1} @ ghost ->
      {result : int * int | match result with left, right ->
        -1 <= left && right <= Iarray.length array && right = left + 1
        && not (above array target strict left)
        && above array target strict right} =
    fun array target strict size ->
    let refine_ size = size in
    let lower = -1 in
    let upper = Iarray.length array in
    let[@def] p (index : int) = above array target strict index in
    let premise = ghost_ (
      let refine_ lower_call = p_def lower in
      let refine_ upper_call = p_def upper in
      let refine_ lower_value = above_def array target strict lower in
      let refine_ upper_value = above_def array target strict upper in
      let u = () in
      (refine_ u : {u : unit | -1 <= lower && lower < upper
        && 0 < upper - lower && not (p lower) && p upper}))
    in
    let split = Binary.midpoint in
    let refine_ result = Binary.search split p lower upper premise in
    let (left : int), (right : int) = result in
    let refine_ proof = ghost_ (
      let refine_ lower_call = p_def left in
      let refine_ upper_call = p_def right in
      let u = () in
      (refine_ u : {u : unit |
        not (above array target strict left)
        && above array target strict right}))
    in
    refine_ result

  let[@def] rec sorted (array : int iarray) (start : int) (stop : int) =
    if 0 <= start && start < stop then
      let next = start + 1 in
      if next < stop then
        at array start <= at array next && sorted array next stop
      else true
    else true
  [@@decreases
    let start : int = start in
    let stop : int = stop in
    if 0 <= start && start < stop then stop - start else 0]

  let rec (ordered @ total) :
      (array : int iarray) -> (start : int) -> (i : int) -> (j : int) ->
      {u : unit | 0 <= start && start <= i && i <= j
        && j < Iarray.length array
        && sorted array start (Iarray.length array)} @ ghost ->
      {u : unit | at array i <= at array j} =
    fun array start i j premise ->
    let refine_ premise = premise in
    let u = () in
    if i = j then refine_ u
    else
      let stop = Iarray.length array in
      let next = start + 1 in
      let refine_ equation = sorted_def array start stop in
      if start < i then
        let refine_ induction = ordered array next i j (refine_ u) in
        refine_ u
      else
        let refine_ induction = ordered array next next j (refine_ u) in
        refine_ u
  [@@decreases j - start]

  let (partition @ total) :
      (array : int iarray) -> (target : int) -> (strict : bool) ->
      (left : int) -> (right : int) -> (index : int) ->
      {u : unit | 0 <= index && index < Iarray.length array
        && sorted array 0 (Iarray.length array)
        && -1 <= left && right <= Iarray.length array && right = left + 1
        && not (above array target strict left)
        && above array target strict right} @ ghost ->
      {u : unit |
        if index <= left then
          (if strict then at array index <= target
           else at array index < target)
        else
          (if strict then at array index > target
           else at array index >= target)} =
    fun array target strict left right index premise ->
    let refine_ premise = premise in
    let u = () in
    let zero = 0 in
    if index <= left then
      let refine_ equation = above_def array target strict left in
      let refine_ order = ordered array zero index left (refine_ u) in
      refine_ u
    else
      let refine_ equation = above_def array target strict right in
      let refine_ order = ordered array zero right index (refine_ u) in
      refine_ u


  let[@def] range_spec (array : int iarray) (target : int)
      (first : int) (past : int) =
    0 <= first && first <= past && past <= Iarray.length array
    && sorted array 0 (Iarray.length array)
    && not (above array target false (first - 1))
    && above array target false first
    && not (above array target true (past - 1))
    && above array target true past

  let (range_at @ total) :
      (array : int iarray) -> (target : int) ->
      (first : int) -> (past : int) -> (index : int) ->
      {u : unit | range_spec array target first past
        && 0 <= index && index < Iarray.length array} @ ghost ->
      {u : unit | if index < first then at array index < target
        else if index < past then at array index = target
        else at array index > target} =
    fun array target first past index premise ->
    let refine_ premise = premise in
    let refine_ spec = range_spec_def array target first past in
    let lower_left = first - 1 in
    let upper_left = past - 1 in
    let lower_strict = false in
    let upper_strict = true in
    let u = () in
    let refine_ lower = partition array target lower_strict
      lower_left first index (refine_ u) in
    let refine_ upper = partition array target upper_strict
      upper_left past index (refine_ u) in
    refine_ u

  let[@def] rec occurs (array : int iarray) (target : int)
      (start : int) (stop : int) =
    if 0 <= start && start < stop then
      at array start = target || occurs array target (start + 1) stop
    else false
  [@@decreases
    let start : int = start in
    let stop : int = stop in
    if 0 <= start && start < stop then stop - start else 0]

  let rec (occurs_range @ total) :
      (array : int iarray) -> (target : int) ->
      (first : int) -> (past : int) -> (start : int) -> (stop : int) ->
      {u : unit | range_spec array target first past
        && 0 <= start && start <= stop && stop <= Iarray.length array}
        @ ghost ->
      {u : unit | occurs array target start stop =
        (start < stop && first < past && start < past && first < stop)} =
    fun array target first past start stop premise ->
    let refine_ premise = premise in
    let refine_ spec = range_spec_def array target first past in
    let refine_ equation = occurs_def array target start stop in
    let u = () in
    if start = stop then refine_ u
    else
      let refine_ value = range_at array target first past start (refine_ u) in
      let next = start + 1 in
      let refine_ induction = occurs_range array target first past
        next stop (refine_ u) in
      refine_ u
  [@@decreases stop - start]

  let (equal_range @ total) :
      (array : int iarray) -> (target : int) ->
      {u : unit | 0 < Iarray.length array + 1
        && sorted array 0 (Iarray.length array)} @ ghost ->
      {result : int * int | match result with first, past ->
        range_spec array target first past
        && occurs array target 0 (Iarray.length array) = (first < past)} =
    fun array target premise ->
    let refine_ premise = premise in
    let u = () in
    let lower_strict = false in
    let upper_strict = true in
    let refine_ lower = bounds array target lower_strict (refine_ u) in
    let refine_ upper = bounds array target upper_strict (refine_ u) in
    let (lower_left : int), (first : int) = lower in
    let (upper_left : int), (past : int) = upper in
    let refine_ proof = ghost_ (
      let refine_ first_value = above_def array target lower_strict first in
      let refine_ past_value = above_def array target upper_strict past in
      let refine_ lower_value =
        above_def array target lower_strict lower_left in
      let refine_ upper_value =
        above_def array target upper_strict upper_left in
      let refine_ order =
        if past < first then
          let refine_ value = partition array target lower_strict
            lower_left first past (refine_ u) in
          (refine_ u : {u : unit | first <= past})
        else (refine_ u : {u : unit | first <= past})
      in
      let refine_ spec = range_spec_def array target first past in
      let zero = 0 in
      let stop = Iarray.length array in
      let refine_ membership = occurs_range array target first past
        zero stop (refine_ u) in
      (refine_ u : {u : unit | range_spec array target first past
        && occurs array target 0 (Iarray.length array) = (first < past)}))
    in
    let result = first, past in
    refine_ result


  let (find_first @ total) :
      (array : int iarray) -> (target : int) ->
      {u : unit | 0 < Iarray.length array + 1
        && sorted array 0 (Iarray.length array)} @ ghost ->
      {result : int option | match result with
        | None -> not (occurs array target 0 (Iarray.length array))
        | Some index -> 0 <= index && index < Iarray.length array
          && at array index = target && not (occurs array target 0 index)} =
    fun array target premise ->
    let refine_ range = equal_range array target premise in
    let (first : int), (past : int) = range in
    if first = past then
      let result = None in
      refine_ result
    else
      let refine_ proof = ghost_ (
        let refine_ spec = range_spec_def array target first past in
        let u = () in
        let refine_ value =
          range_at array target first past first (refine_ u) in
        let zero = 0 in
        let refine_ prefix = occurs_range array target first past
          zero first (refine_ u) in
        (refine_ u : {u : unit | 0 <= first && first < Iarray.length array
          && at array first = target && not (occurs array target 0 first)}))
      in
      let result = Some first in
      refine_ result

  let (find_last @ total) :
      (array : int iarray) -> (target : int) ->
      {u : unit | 0 < Iarray.length array + 1
        && sorted array 0 (Iarray.length array)} @ ghost ->
      {result : int option | match result with
        | None -> not (occurs array target 0 (Iarray.length array))
        | Some index -> 0 <= index && index < Iarray.length array
          && at array index = target
          && not (occurs array target (index + 1) (Iarray.length array))} =
    fun array target premise ->
    let refine_ range = equal_range array target premise in
    let (first : int), (past : int) = range in
    if first = past then
      let result = None in
      refine_ result
    else
      let index = past - 1 in
      let refine_ proof = ghost_ (
        let refine_ spec = range_spec_def array target first past in
        let u = () in
        let refine_ value =
          range_at array target first past index (refine_ u) in
        let stop = Iarray.length array in
        let refine_ suffix = occurs_range array target first past
          past stop (refine_ u) in
        (refine_ u : {u : unit | 0 <= index && index < Iarray.length array
          && at array index = target
          && not (occurs array target (index + 1) (Iarray.length array))}))
      in
      let result = Some index in
      refine_ result

  let (mem @ total) :
      (array : int iarray) -> (target : int) ->
      {u : unit | 0 < Iarray.length array + 1
        && sorted array 0 (Iarray.length array)} @ ghost ->
      {result : bool | result = occurs array target 0 (Iarray.length array)} =
    fun array target premise ->
    let refine_ range = equal_range array target premise in
    let (first : int), (past : int) = range in
    let result = first < past in
    refine_ result

end;;
[%%expect{|
module Arrays :
  sig
    val at : int iarray -> int -> int
    val at_def :
      (array : int iarray) ->
      (index : int) ->
      {u : unit
        | (at array index) ===
            (if (0 <= index) && (index < (Iarray.length array))
             then
               let (bounded : int) = (index : int) in
               Iarray.Refined.get array bounded
             else 0)}
    val above : int iarray -> int -> bool -> int -> bool
    val above_def :
      (array : int iarray) ->
      (target : int) ->
      (strict : bool) ->
      (index : int) ->
      {u : unit
        | (above array target strict index) ===
            (if index < 0
             then false
             else
               if (Iarray.length array) <= index
               then true
               else
                 if strict
                 then (at array index) > target
                 else (at array index) >= target)}
    val bounds :
      (array : int iarray) ->
      ((target : int) ->
       (strict : bool) ->
       {u : unit | 0 < ((Iarray.length array) + 1)} @ ghost ->
       {result : int * int
         | match result with
           | (left, right) ->
               ((-1) <= left) &&
                 ((right <= (Iarray.length array)) &&
                    ((right = (left + 1)) &&
                       ((not (above array target strict left)) &&
                          (above array target strict right))))}) @ total
      stateful
    val sorted : int iarray -> int -> int -> bool
    val sorted_def :
      (array : int iarray) ->
      (start : int) ->
      (stop : int) ->
      {u : unit
        | (sorted array start stop) ===
            (if (0 <= start) && (start < stop)
             then
               let next = start + 1 in
               (if next < stop
                then
                  ((at array start) <= (at array next)) &&
                    (sorted array next stop)
                else true)
             else true)}
    val ordered :
      (array : int iarray) ->
      ((start : int) ->
       (i : int) ->
       (j : int) ->
       {u : unit
         | (0 <= start) &&
             ((start <= i) &&
                ((i <= j) &&
                   ((j < (Iarray.length array)) &&
                      (sorted array start (Iarray.length array)))))} @ ghost ->
       {u : unit | (at array i) <= (at array j)}) @ total
      stateful
    val partition :
      (array : int iarray) ->
      ((target : int) ->
       (strict : bool) ->
       (left : int) ->
       (right : int) ->
       (index : int) ->
       {u : unit
         | (0 <= index) &&
             ((index < (Iarray.length array)) &&
                ((sorted array 0 (Iarray.length array)) &&
                   (((-1) <= left) &&
                      ((right <= (Iarray.length array)) &&
                         ((right = (left + 1)) &&
                            ((not (above array target strict left)) &&
                               (above array target strict right)))))))} @ ghost ->
       {u : unit
         | if index <= left
           then
             (if strict
              then (at array index) <= target
              else (at array index) < target)
           else
             if strict
             then (at array index) > target
             else (at array index) >= target}) @ total
      stateful
    val range_spec : int iarray -> int -> int -> int -> bool
    val range_spec_def :
      (array : int iarray) ->
      (target : int) ->
      (first : int) ->
      (past : int) ->
      {u : unit
        | (range_spec array target first past) ===
            ((0 <= first) &&
               ((first <= past) &&
                  ((past <= (Iarray.length array)) &&
                     ((sorted array 0 (Iarray.length array)) &&
                        ((not (above array target false (first - 1))) &&
                           ((above array target false first) &&
                              ((not (above array target true (past - 1))) &&
                                 (above array target true past))))))))}
    val range_at :
      (array : int iarray) ->
      ((target : int) ->
       (first : int) ->
       (past : int) ->
       (index : int) ->
       {u : unit
         | (range_spec array target first past) &&
             ((0 <= index) && (index < (Iarray.length array)))} @ ghost ->
       {u : unit
         | if index < first
           then (at array index) < target
           else
             if index < past
             then (at array index) = target
             else (at array index) > target}) @ total
      stateful
    val occurs : int iarray -> int -> int -> int -> bool
    val occurs_def :
      (array : int iarray) ->
      (target : int) ->
      (start : int) ->
      (stop : int) ->
      {u : unit
        | (occurs array target start stop) ===
            (if (0 <= start) && (start < stop)
             then
               ((at array start) = target) ||
                 (occurs array target (start + 1) stop)
             else false)}
    val occurs_range :
      (array : int iarray) ->
      ((target : int) ->
       (first : int) ->
       (past : int) ->
       (start : int) ->
       (stop : int) ->
       {u : unit
         | (range_spec array target first past) &&
             ((0 <= start) &&
                ((start <= stop) && (stop <= (Iarray.length array))))} @ ghost ->
       {u : unit
         | (occurs array target start stop) =
             ((start < stop) &&
                ((first < past) && ((start < past) && (first < stop))))}) @ total
      stateful
    val equal_range :
      (array : int iarray) ->
      ((target : int) ->
       {u : unit
         | (0 < ((Iarray.length array) + 1)) &&
             (sorted array 0 (Iarray.length array))} @ ghost ->
       {result : int * int
         | match result with
           | (first, past) ->
               (range_spec array target first past) &&
                 ((occurs array target 0 (Iarray.length array)) =
                    (first < past))}) @ total
      stateful
    val find_first :
      (array : int iarray) ->
      ((target : int) ->
       {u : unit
         | (0 < ((Iarray.length array) + 1)) &&
             (sorted array 0 (Iarray.length array))} @ ghost ->
       {result : int option
         | match result with
           | None -> not (occurs array target 0 (Iarray.length array))
           | Some index ->
               (0 <= index) &&
                 ((index < (Iarray.length array)) &&
                    (((at array index) = target) &&
                       (not (occurs array target 0 index))))}) @ total
      stateful
    val find_last :
      (array : int iarray) ->
      ((target : int) ->
       {u : unit
         | (0 < ((Iarray.length array) + 1)) &&
             (sorted array 0 (Iarray.length array))} @ ghost ->
       {result : int option
         | match result with
           | None -> not (occurs array target 0 (Iarray.length array))
           | Some index ->
               (0 <= index) &&
                 ((index < (Iarray.length array)) &&
                    (((at array index) = target) &&
                       (not
                          (occurs array target (index + 1)
                             (Iarray.length array)))))}) @ total
      stateful
    val mem :
      (array : int iarray) ->
      ((target : int) ->
       {u : unit
         | (0 < ((Iarray.length array) + 1)) &&
             (sorted array 0 (Iarray.length array))} @ ghost ->
       {result : bool
         | result = (occurs array target 0 (Iarray.length array))}) @ total
      stateful
  end
|}]

module Examples : sig end = struct
  let[@def] p (i : int) = i = 2 || i = 4 || i = 6

  let () =
    let lower = 0 in
    let upper = 6 in
    let premise = ghost_ (
      let refine_ a = p_def lower in
      let refine_ b = p_def upper in
      let u = () in
      (refine_ u : {u : unit | -1 <= lower && lower < upper
        && 0 < upper - lower && not (p lower) && p upper}))
    in
    let split = Binary.midpoint in
    let refine_ pair = Binary.search split p lower upper premise in
    let l, r = pair in
    Format.printf "binary transition: %d,%d@." l r;
    let split = Binary.forward in
    let refine_ pair = Binary.search split p lower upper premise in
    let l, r = pair in
    Format.printf "forward transition: %d,%d@." l r;
    let split = Binary.backward in
    let refine_ pair = Binary.search split p lower upper premise in
    let l, r = pair in
    Format.printf "backward transition: %d,%d@." l r

  let () =
    let array = [: 2; 3; 3; 3; 6; 8; 8; 9 :] in
    let u = () in
    let size : {u : unit | 0 < Iarray.length array + 1} = refine_ u in
    List.iter (fun (target : int) ->
      let strict = false in
      let refine_ lower = Arrays.bounds array target strict size in
      let strict = true in
      let refine_ upper = Arrays.bounds array target strict size in
      let q1, q2 = lower in
      let q3, q4 = upper in
      Format.printf "%d: %d,%d,%d,%d@." target q1 q2 q3 q4)
      [0; 3; 5; 8; 10]

  let () =
    let lower = 0 in
    let upper = 4_611_686_018_427_387_903 in
    let[@def] at_limit (index : int) =
      index = 4_611_686_018_427_387_903 in
    let premise = ghost_ (
      let refine_ a = at_limit_def lower in
      let refine_ b = at_limit_def upper in
      let u = () in
      (refine_ u : {u : unit | -1 <= lower && lower < upper
        && 0 < upper - lower && not (at_limit lower) && at_limit upper}))
    in
    let split = Binary.midpoint in
    let refine_ pair = Binary.search split at_limit lower upper premise in
    let left, right = pair in
    assert (left = 4_611_686_018_427_387_902 && right = upper);
    Format.printf "largest positive interval: adjacent endpoints@."

  let check_array (values : int list) (target : int) =
    let array : int iarray = Iarray.of_list values in
    let length = Iarray.length array in
    let zero = 0 in
    let u = () in
    let size : {u : unit | 0 < Iarray.length array + 1} = assume_ u in
    let sorted : {u : unit | Arrays.sorted array zero length} = assume_ u in
    let check (strict : bool) =
      let refine_ result = Arrays.bounds array target strict size in
      let (left : int), (right : int) = result in
      let refine_ sorted = sorted in
      let premise : {u : unit |
        Arrays.sorted array 0 (Iarray.length array)
        && -1 <= left && right <= Iarray.length array && right = left + 1
        && not (Arrays.above array target strict left)
        && Arrays.above array target strict right} = refine_ u in
      let rec linear index =
        if index = length then length
        else
          let value = Iarray.get array index in
          if (if strict then value > target else value >= target) then index
          else linear (index + 1)
      in
      let expected = linear 0 in
      assert (right = expected && left = expected - 1);
      List.iter (fun (index : int) ->
        let bounded : {i : int | 0 <= i && i < Iarray.length array} =
          assume_ index in
        let refine_ index = bounded in
        let index : int = index in
        let refine_ premise = premise in
        let refine_ proof = ghost_ (
          Arrays.partition array target strict left right index (refine_ u))
        in
        let value : int = Iarray.Refined.get array bounded in
        let refine_ equation = ghost_ (Arrays.at_def array index) in
        let guarantee : {u : unit |
          if index <= left then
            (if strict then value <= target else value < target)
          else
            (if strict then value > target else value >= target)} = refine_ u in
        let refine_ guarantee = guarantee in
        ())
        (List.init length Fun.id);
      expected
    in
    let expected_first = check false in
    let expected_past = check true in
    let premise = ghost_ (
      let refine_ size = size in
      let refine_ sorted = sorted in
      (refine_ u : {u : unit | 0 < Iarray.length array + 1
        && Arrays.sorted array 0 (Iarray.length array)}))
    in
    let refine_ range = Arrays.equal_range array target premise in
    let (first : int), (past : int) = range in
    assert (first = expected_first && past = expected_past);
    let expected_member = expected_first < expected_past in
    let refine_ member = Arrays.mem array target premise in
    assert (member = expected_member);
    let refine_ first_match = Arrays.find_first array target premise in
    let refine_ last_match = Arrays.find_last array target premise in
    assert (first_match =
      (if expected_member then Some expected_first else None));
    assert (last_match =
      (if expected_member then Some (expected_past - 1) else None));
    let certificate : {u : unit | Arrays.range_spec array target first past} =
      refine_ u in
    List.iter (fun (index : int) ->
      let bounded : {i : int | 0 <= i && i < Iarray.length array} =
        assume_ index in
      let refine_ index = bounded in
      let index : int = index in
      let refine_ certificate = certificate in
      let refine_ proof = ghost_ (
        Arrays.range_at array target first past index (refine_ u)) in
      let value : int = Iarray.Refined.get array bounded in
      let refine_ equation = ghost_ (Arrays.at_def array index) in
      let guarantee : {u : unit |
        if index < first then value < target
        else if index < past then value = target
        else value > target} = refine_ u in
      let refine_ guarantee = guarantee in
      ())
      (List.init length Fun.id)

  let () =
    let rec sorted_lists minimum length =
      if length = 0 then [[]]
      else
        List.concat_map (fun value ->
          List.map (fun tail -> value :: tail)
            (sorted_lists value (length - 1)))
          (List.init (4 - minimum) (fun offset -> minimum + offset))
    in
    let arrays = List.concat_map (sorted_lists 0) [0; 1; 2; 3; 4; 5] in
    List.iter (fun values ->
      List.iter (check_array values) [-1; 0; 1; 2; 3; 4]) arrays;
    Format.printf "checked ranges and matches on %d sorted arrays@."
      (List.length arrays);
    List.iter (check_array [min_int; min_int; max_int])
      [min_int; 0; max_int]

end;;
[%%expect{|
binary transition: 3,4
forward transition: 1,2
backward transition: 5,6
0: -1,0,-1,0
3: 0,1,3,4
5: 3,4,3,4
8: 4,5,6,7
10: 7,8,7,8
largest positive interval: adjacent endpoints
checked ranges and matches on 126 sorted arrays
module Examples : sig end
|}]

let invalid_midpoint : Binary.splitter = fun left right premise ->
  let refine_ premise = premise in
  let result = Some left in
  refine_ result;;
[%%expect{|
Line 4, characters 2-16:
4 |   refine_ result;;
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_stop : Binary.splitter = fun left right premise ->
  let refine_ premise = premise in
  let result = None in
  refine_ result;;
[%%expect{|
Line 4, characters 2-16:
4 |   refine_ result;;
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_divisor () =
  let zero = 0 in
  Binary.divide 10 (refine_ zero);;
[%%expect{|
Line 3, characters 19-33:
3 |   Binary.divide 10 (refine_ zero);;
                       ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_absence () =
  let array = [: 3 :] in
  let target = 3 in
  let start = 0 in
  let stop = 1 in
  let refine_ value = Arrays.at_def array start in
  let refine_ equation = Arrays.occurs_def array target start stop in
  let u = () in
  let proof : {u : unit | not (Arrays.occurs array target start stop)} =
    refine_ u in
  let refine_ proof = proof in
  ();;
[%%expect{|
Line 10, characters 4-13:
10 |     refine_ u in
         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_first_match () =
  let array = [: 3; 3 :] in
  let target = 3 in
  let start = 0 in
  let later = 1 in
  let refine_ value = Arrays.at_def array start in
  let refine_ value = Arrays.at_def array later in
  let refine_ equation = Arrays.occurs_def array target start later in
  let result : {index : int | Arrays.at array index = target
    && not (Arrays.occurs array target 0 index)} = refine_ later in
  let refine_ result = result in
  result;;
[%%expect{|
Line 10, characters 51-64:
10 |     && not (Arrays.occurs array target 0 index)} = refine_ later in
                                                        ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_last_match () =
  let array = [: 3; 3 :] in
  let target = 3 in
  let first = 0 in
  let later = 1 in
  let stop = 2 in
  let refine_ value = Arrays.at_def array first in
  let refine_ value = Arrays.at_def array later in
  let refine_ equation = Arrays.occurs_def array target later stop in
  let result : {index : int | Arrays.at array index = target
    && not (Arrays.occurs array target (index + 1) stop)} = refine_ first in
  let refine_ result = result in
  result;;
[%%expect{|
Line 11, characters 60-73:
11 |     && not (Arrays.occurs array target (index + 1) stop)} = refine_ first in
                                                                 ^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
