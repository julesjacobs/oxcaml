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
    premise;
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
    premise;
    let result = if right - left > 1 then Some (left + 1) else None in
    refine_ result

  let (backward @ total) : splitter = fun left right premise ->
    premise;
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
    premise;
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
    size;
    let lower = -1 in
    let upper = Iarray.length array in
    let[@def] p (index : int) = above array target strict index in
    let premise = ghost_ (
      p_def lower;
      p_def upper;
      above_def array target strict lower;
      above_def array target strict upper;
      let u = () in
      (refine_ u : {u : unit | -1 <= lower && lower < upper
        && 0 < upper - lower && not (p lower) && p upper}))
    in
    let split = Binary.midpoint in
    let refine_ result = Binary.search split p lower upper premise in
    let (left : int), (right : int) = result in
    ghost_ (
      p_def left;
      p_def right;
      let u = () in
      (refine_ u : {u : unit |
        not (above array target strict left)
        && above array target strict right}));
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
    premise;
    let u = () in
    if i = j then refine_ u
    else
      let stop = Iarray.length array in
      let next = start + 1 in
      sorted_def array start stop;
      if start < i then
        (ordered array next i j (refine_ u);
        refine_ u)
      else
        (ordered array next next j (refine_ u);
        refine_ u)
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
    premise;
    let u = () in
    let zero = 0 in
    if index <= left then
      (above_def array target strict left;
      ordered array zero index left (refine_ u);
      refine_ u)
    else
      (above_def array target strict right;
      ordered array zero right index (refine_ u);
      refine_ u)


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
    premise;
    range_spec_def array target first past;
    let lower_left = first - 1 in
    let upper_left = past - 1 in
    let lower_strict = false in
    let upper_strict = true in
    let u = () in
    partition array target lower_strict
      lower_left first index (refine_ u);
    partition array target upper_strict
      upper_left past index (refine_ u);
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
    premise;
    range_spec_def array target first past;
    occurs_def array target start stop;
    let u = () in
    if start = stop then refine_ u
    else
      (range_at array target first past start (refine_ u);
      let next = start + 1 in
      occurs_range array target first past
        next stop (refine_ u);
      refine_ u)
  [@@decreases stop - start]

  let (equal_range @ total) :
      (array : int iarray) -> (target : int) ->
      {u : unit | 0 < Iarray.length array + 1
        && sorted array 0 (Iarray.length array)} @ ghost ->
      {result : int * int | match result with first, past ->
        range_spec array target first past
        && occurs array target 0 (Iarray.length array) = (first < past)} =
    fun array target premise ->
    premise;
    let u = () in
    let lower_strict = false in
    let upper_strict = true in
    let refine_ lower = bounds array target lower_strict (refine_ u) in
    let refine_ upper = bounds array target upper_strict (refine_ u) in
    let (lower_left : int), (first : int) = lower in
    let (upper_left : int), (past : int) = upper in
    ghost_ (
      above_def array target lower_strict first;
      above_def array target upper_strict past;
      above_def array target lower_strict lower_left;
      above_def array target upper_strict upper_left;
      if past < first then
          (partition array target lower_strict
            lower_left first past (refine_ u);
          (refine_ u : {u : unit | first <= past}))
        else (refine_ u : {u : unit | first <= past});
      range_spec_def array target first past;
      let zero = 0 in
      let stop = Iarray.length array in
      occurs_range array target first past
        zero stop (refine_ u);
      (refine_ u : {u : unit | range_spec array target first past
        && occurs array target 0 (Iarray.length array) = (first < past)}));
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
      (ghost_ (
        range_spec_def array target first past;
        let u = () in
        range_at array target first past first (refine_ u);
        let zero = 0 in
        occurs_range array target first past
          zero first (refine_ u);
        (refine_ u : {u : unit | 0 <= first && first < Iarray.length array
          && at array first = target && not (occurs array target 0 first)}));
      let result = Some first in
      refine_ result)

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
      ghost_ (
        range_spec_def array target first past;
        let u = () in
        range_at array target first past index (refine_ u);
        let stop = Iarray.length array in
        occurs_range array target first past
          past stop (refine_ u);
        (refine_ u : {u : unit | 0 <= index && index < Iarray.length array
          && at array index = target
          && not (occurs array target (index + 1) (Iarray.length array))}));
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


  let[@def] edit_value (source : int iarray) (position : int)
      (value : int) (inserting : bool) (index : int) =
    if index < position then at source index
    else if inserting then
      if index = position then value else at source (index - 1)
    else at source (index + 1)

  let[@def] rec edited (source : int iarray) (result : int iarray)
      (position : int) (value : int) (inserting : bool)
      (start : int) (stop : int) =
    if 0 <= start && start < stop then
      at result start = edit_value source position value inserting start
      && edited source result position value inserting (start + 1) stop
    else true
  [@@decreases
    let start : int = start in
    let stop : int = stop in
    if 0 <= start && start < stop then stop - start else 0]

  let rec (edited_at @ total) :
      (source : int iarray) -> (result : int iarray) ->
      (position : int) -> (value : int) -> (inserting : bool) ->
      (start : int) -> (stop : int) -> (index : int) ->
      {u : unit | 0 <= start && start <= index && index < stop
        && edited source result position value inserting start stop}
        @ ghost ->
      {u : unit | at result index =
        edit_value source position value inserting index} =
    fun source result position value inserting start stop index premise ->
    premise;
    edited_def source result position value inserting start stop;
    let u = () in
    if start = index then refine_ u
    else
      let next = start + 1 in
      edited_at source result position value
        inserting next stop index (refine_ u);
      refine_ u
  [@@decreases index - start]

  let insert :
      (source : int iarray) -> (value : int) ->
      {u : unit | 0 < Iarray.length source + 1
        && sorted source 0 (Iarray.length source)} @ ghost ->
      {pair : int * int iarray | match pair with position, result ->
        0 <= position && position <= Iarray.length source
        && Iarray.length result = Iarray.length source + 1
        && sorted result 0 (Iarray.length result)
        && edited source result position value true 0 (Iarray.length result)} =
    fun source value premise ->
    premise;
    let u = () in
    let refine_ range = equal_range source value (refine_ u) in
    let (position : int), (past : int) = range in
    let prefix = Iarray.sub source ~pos:0 ~len:position in
    let suffix = Iarray.sub source ~pos:position
      ~len:(Iarray.length source - position) in
    let result = Iarray.append (Iarray.append prefix [: value :]) suffix in
    ghost_ (
      range_spec_def source value position past;
      let stop = Iarray.length result in
      let zero = 0 in
      let inserting = true in
      let rec (certify @ total) : (start : int) ->
          {u : unit | 0 <= start && start <= stop} @ ghost ->
          {u : unit | edited source result position value inserting start stop
            && sorted result start stop} =
        fun start bounds ->
        let refine_ bounds = bounds in
        edited_def source result position value inserting start stop;
        sorted_def result start stop;
        if start = stop then refine_ u
        else
          let next = start + 1 in
          certify next (refine_ u);
          at_def result start;
          edit_value_def source position value inserting start;
          let original = if start < position then start else start - 1 in
          at_def source original;
          if next < stop then (
              at_def result next;
              let original_next =
                if next < position then next else next - 1 in
              at_def source original_next;
              if start = position then
                (range_at source value position past original_next
                    (refine_ u);
                (refine_ u : {u : unit |
                  next >= stop || at result start <= at result next}))
              else if next = position then
                (range_at source value position past original (refine_ u);
                (refine_ u : {u : unit |
                  next >= stop || at result start <= at result next}))
              else
                (ordered source zero original original_next (refine_ u);
                (refine_ u : {u : unit |
                  next >= stop || at result start <= at result next})))
            else (refine_ u : {u : unit |
              next >= stop || at result start <= at result next});
          refine_ u
      [@@decreases stop - start]
      in
      certify zero (refine_ u);
      (refine_ u : {u : unit | sorted result 0 (Iarray.length result)
        && edited source result position value true 0 (Iarray.length result)}));
    let pair = position, result in
    refine_ pair


  let remove_at :
      (source : int iarray) -> (position : int) ->
      {u : unit | 0 <= position && position < Iarray.length source
        && sorted source 0 (Iarray.length source)} @ ghost ->
      {result : int iarray | Iarray.length result = Iarray.length source - 1
        && sorted result 0 (Iarray.length result)
        && edited source result position 0 false 0 (Iarray.length result)} =
    fun source position premise ->
    premise;
    let prefix = Iarray.sub source ~pos:0 ~len:position in
    let suffix = Iarray.sub source ~pos:(position + 1)
      ~len:(Iarray.length source - position - 1) in
    let result = Iarray.append prefix suffix in
    ghost_ (
      let stop = Iarray.length result in
      let zero = 0 in
      let value = 0 in
      let inserting = false in
      let u = () in
      let rec (certify @ total) : (start : int) ->
          {u : unit | 0 <= start && start <= stop} @ ghost ->
          {u : unit | edited source result position value inserting start stop
            && sorted result start stop} =
        fun start bounds ->
        let refine_ bounds = bounds in
        edited_def source result position value inserting start stop;
        sorted_def result start stop;
        if start = stop then refine_ u
        else
          let next = start + 1 in
          certify next (refine_ u);
          at_def result start;
          edit_value_def source position value inserting start;
          let original = if start < position then start else start + 1 in
          at_def source original;
          if next < stop then (
              at_def result next;
              let original_next =
                if next < position then next else next + 1 in
              at_def source original_next;
              ordered source zero original original_next (refine_ u);
              (refine_ u : {u : unit |
                next >= stop || at result start <= at result next}))
            else (refine_ u : {u : unit |
              next >= stop || at result start <= at result next});
          refine_ u
      [@@decreases stop - start]
      in
      certify zero (refine_ u);
      (refine_ u : {u : unit | sorted result 0 (Iarray.length result)
        && edited source result position 0 false 0 (Iarray.length result)}));
    refine_ result

  let remove_one :
      (source : int iarray) -> (value : int) ->
      {u : unit | 0 < Iarray.length source + 1
        && sorted source 0 (Iarray.length source)} @ ghost ->
      {result : (int * int iarray) option | match result with
        | None -> not (occurs source value 0 (Iarray.length source))
        | Some (position, array) ->
          0 <= position && position < Iarray.length source
          && at source position = value
          && not (occurs source value 0 position)
          && Iarray.length array = Iarray.length source - 1
          && sorted array 0 (Iarray.length array)
          && edited source array position 0 false 0 (Iarray.length array)} =
    fun source value premise ->
    premise;
    let u = () in
    let refine_ found = find_first source value (refine_ u) in
    match found with
    | None ->
      let result = None in
      refine_ result
    | Some position ->
      let position : int = position in
      let refine_ array = remove_at source position (refine_ u) in
      let result = Some (position, array) in
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
    val edit_value : int iarray -> int -> int -> bool -> int -> int
    val edit_value_def :
      (source : int iarray) ->
      (position : int) ->
      (value : int) ->
      (inserting : bool) ->
      (index : int) ->
      {u : unit
        | (edit_value source position value inserting index) ===
            (if index < position
             then at source index
             else
               if inserting
               then
                 (if index = position then value else at source (index - 1))
               else at source (index + 1))}
    val edited :
      int iarray -> int iarray -> int -> int -> bool -> int -> int -> bool
    val edited_def :
      (source : int iarray) ->
      (result : int iarray) ->
      (position : int) ->
      (value : int) ->
      (inserting : bool) ->
      (start : int) ->
      (stop : int) ->
      {u : unit
        | (edited source result position value inserting start stop) ===
            (if (0 <= start) && (start < stop)
             then
               ((at result start) =
                  (edit_value source position value inserting start))
                 &&
                 (edited source result position value inserting (start + 1)
                    stop)
             else true)}
    val edited_at :
      (source : int iarray) ->
      ((result : int iarray) ->
       (position : int) ->
       (value : int) ->
       (inserting : bool) ->
       (start : int) ->
       (stop : int) ->
       (index : int) ->
       {u : unit
         | (0 <= start) &&
             ((start <= index) &&
                ((index < stop) &&
                   (edited source result position value inserting start stop)))} @ ghost ->
       {u : unit
         | (at result index) =
             (edit_value source position value inserting index)}) @ total
      stateful
    val insert :
      (source : int iarray) ->
      (value : int) ->
      {u : unit
        | (0 < ((Iarray.length source) + 1)) &&
            (sorted source 0 (Iarray.length source))} @ ghost ->
      {pair : int * int iarray
        | match pair with
          | (position, result) ->
              (0 <= position) &&
                ((position <= (Iarray.length source)) &&
                   (((Iarray.length result) = ((Iarray.length source) + 1))
                      &&
                      ((sorted result 0 (Iarray.length result)) &&
                         (edited source result position value true 0
                            (Iarray.length result)))))}
    val remove_at :
      (source : int iarray) ->
      (position : int) ->
      {u : unit
        | (0 <= position) &&
            ((position < (Iarray.length source)) &&
               (sorted source 0 (Iarray.length source)))} @ ghost ->
      {result : int iarray
        | ((Iarray.length result) = ((Iarray.length source) - 1)) &&
            ((sorted result 0 (Iarray.length result)) &&
               (edited source result position 0 false 0
                  (Iarray.length result)))}
    val remove_one :
      (source : int iarray) ->
      (value : int) ->
      {u : unit
        | (0 < ((Iarray.length source) + 1)) &&
            (sorted source 0 (Iarray.length source))} @ ghost ->
      {result : (int * int iarray) option
        | match result with
          | None -> not (occurs source value 0 (Iarray.length source))
          | Some (position, array) ->
              (0 <= position) &&
                ((position < (Iarray.length source)) &&
                   (((at source position) = value) &&
                      ((not (occurs source value 0 position)) &&
                         (((Iarray.length array) =
                             ((Iarray.length source) - 1))
                            &&
                            ((sorted array 0 (Iarray.length array)) &&
                               (edited source array position 0 false 0
                                  (Iarray.length array)))))))}
  end
|}]

module Examples : sig end = struct
  let[@def] p (i : int) = i = 2 || i = 4 || i = 6

  let () =
    let lower = 0 in
    let upper = 6 in
    let premise = ghost_ (
      p_def lower;
      p_def upper;
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
      at_limit_def lower;
      at_limit_def upper;
      let u = () in
      (refine_ u : {u : unit | -1 <= lower && lower < upper
        && 0 < upper - lower && not (at_limit lower) && at_limit upper}))
    in
    let split = Binary.midpoint in
    let refine_ pair = Binary.search split at_limit lower upper premise in
    let left, right = pair in
    assert (left = 4_611_686_018_427_387_902 && right = upper);
    Format.printf "largest positive interval: adjacent endpoints@."

  let round_trip :
      (source : int iarray) -> (value : int) -> (index : int) ->
      {u : unit | 0 < Iarray.length source + 1
        && Arrays.sorted source 0 (Iarray.length source)
        && 0 <= index && index < Iarray.length source} @ ghost ->
      {result : int iarray | Iarray.length result = Iarray.length source
        && Arrays.sorted result 0 (Iarray.length result)
        && Arrays.at result index = Arrays.at source index} =
    fun source value index premise ->
    premise;
    let u = () in
    let refine_ pair = Arrays.insert source value (refine_ u) in
    let (position : int), (inserted : int iarray) = pair in
    let refine_ result = Arrays.remove_at inserted position (refine_ u) in
    ghost_ (
      let zero = 0 in
      let insertion = true in
      let removal = false in
      let stop = Iarray.length result in
      let inserted_stop = Iarray.length inserted in
      let original = if index < position then index else index + 1 in
      Arrays.edited_at inserted result position zero
        removal zero stop index (refine_ u);
      Arrays.edit_value_def inserted position zero removal index;
      Arrays.edited_at source inserted position value
        insertion zero inserted_stop original (refine_ u);
      Arrays.edit_value_def source position value insertion original;
      (refine_ u : {u : unit |
        Arrays.at result index = Arrays.at source index}));
    refine_ result

  let insert_and_search :
      (source : int iarray) -> (value : int) ->
      {u : unit | 0 < Iarray.length source + 2
        && Arrays.sorted source 0 (Iarray.length source)} @ ghost ->
      {result : bool | result} =
    fun source value premise ->
    premise;
    let u = () in
    let refine_ pair = Arrays.insert source value (refine_ u) in
    let (position : int), (array : int iarray) = pair in
    let refine_ range = Arrays.equal_range array value (refine_ u) in
    let (first : int), (past : int) = range in
    ghost_ (
      let zero = 0 in
      let inserting = true in
      let stop = Iarray.length array in
      Arrays.edited_at source array position value
        inserting zero stop position (refine_ u);
      Arrays.edit_value_def source position value inserting position;
      Arrays.range_at array value first past position (refine_ u);
      (refine_ u : {u : unit |
        Arrays.occurs array value 0 (Iarray.length array)}));
    let refine_ result = Arrays.mem array value (refine_ u) in
    refine_ result

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
      sorted;
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
        premise;
        ghost_ (
          Arrays.partition array target strict left right index (refine_ u));
        let value : int = Iarray.Refined.get array bounded in
        ghost_ (Arrays.at_def array index);
        let guarantee : {u : unit |
          if index <= left then
            (if strict then value <= target else value < target)
          else
            (if strict then value > target else value >= target)} = refine_ u in
        guarantee;
        ())
        (List.init length Fun.id);
      expected
    in
    let expected_first = check false in
    let expected_past = check true in
    let premise = ghost_ (
      size;
      sorted;
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
      certificate;
      ghost_ (
        Arrays.range_at array target first past index (refine_ u));
      let value : int = Iarray.Refined.get array bounded in
      ghost_ (Arrays.at_def array index);
      let guarantee : {u : unit |
        if index < first then value < target
        else if index < past then value = target
        else value > target} = refine_ u in
      guarantee;
      ())
      (List.init length Fun.id);
    let refine_ inserted_pair = Arrays.insert array target premise in
    let (position : int), (inserted : int iarray) = inserted_pair in
    let rec insert_list = function
      | [] -> [target]
      | head :: tail as all ->
        if target <= head then target :: all else head :: insert_list tail
    in
    assert (position = expected_first);
    assert (Iarray.to_list inserted = insert_list values);
    let restored_premise = ghost_ (
      (refine_ u : {u : unit | 0 <= position
        && position < Iarray.length inserted
        && Arrays.sorted inserted 0 (Iarray.length inserted)})) in
    let refine_ restored =
      Arrays.remove_at inserted position restored_premise in
    assert (Iarray.to_list restored = values);
    let refine_ removed = Arrays.remove_one array target premise in
    let rec remove_list = function
      | [] -> []
      | head :: tail ->
        if head = target then tail else head :: remove_list tail
    in
    (match removed with
     | None -> assert (not expected_member)
     | Some (index, result) ->
       assert (expected_member && index = expected_first);
       assert (Iarray.to_list result = remove_list values));
    let search_premise : {u : unit | 0 < Iarray.length array + 2
      && Arrays.sorted array 0 (Iarray.length array)} = assume_ u in
    let refine_ present = insert_and_search array target search_premise in
    assert present

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
    Format.printf "checked search, insertion, and removal on %d sorted arrays@."
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
checked search, insertion, and removal on 126 sorted arrays
module Examples : sig end
|}]

let invalid_midpoint : Binary.splitter = fun left right premise ->
  premise;
  let result = Some left in
  refine_ result;;
[%%expect{|
Line 4, characters 2-16:
4 |   refine_ result;;
      ^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let invalid_stop : Binary.splitter = fun left right premise ->
  premise;
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
  Arrays.at_def array start;
  Arrays.occurs_def array target start stop;
  let u = () in
  let proof : {u : unit | not (Arrays.occurs array target start stop)} =
    refine_ u in
  proof;
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
  Arrays.at_def array start;
  Arrays.at_def array later;
  Arrays.occurs_def array target start later;
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
  Arrays.at_def array first;
  Arrays.at_def array later;
  Arrays.occurs_def array target later stop;
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


let invalid_removal () =
  let source = [: :] in
  let position = 0 in
  let u = () in
  let refine_ result = Arrays.remove_at source position (refine_ u) in
  result;;
[%%expect{|
Line 5, characters 56-67:
5 |   let refine_ result = Arrays.remove_at source position (refine_ u) in
                                                            ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
