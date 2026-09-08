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

end

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

  let (at_model @ total) : (array : int iarray) -> (index : int) ->
      {u : unit | at array index = Vox_iarray.Int.element array index} =
      fun array index ->
    at_def array index;
    Vox_iarray.Int.element_def array index;
    if 0 <= index && index < Iarray.length array then
      let bounded : {i : int | 0 <= i && i < Iarray.length array} =
        refine_ index in
      Vox_iarray.at_get array bounded;
      let u = () in refine_ u
    else
      (Vox_iarray.at_outside array index;
      let u = () in refine_ u)

  let (ordered @ total) : (array : int iarray) -> (i : int) -> (j : int) ->
      {u : unit | 0 <= i && i <= j && j < Iarray.length array
        && Vox_iarray.Int.sorted array} @ ghost ->
      {u : unit | at array i <= at array j} = fun array i j premise ->
    premise;
    Vox_iarray.Int.ordered array i j;
    at_model array i;
    at_model array j;
    let u = () in refine_ u

  let (partition @ total) :
      (array : int iarray) -> (target : int) -> (strict : bool) ->
      (left : int) -> (right : int) -> (index : int) ->
      {u : unit | 0 <= index && index < Iarray.length array
        && Vox_iarray.Int.sorted array
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
    if index <= left then
      (above_def array target strict left;
      ordered array index left (refine_ u);
      refine_ u)
    else
      (above_def array target strict right;
      ordered array right index (refine_ u);
      refine_ u)


  let[@def] range_spec (array : int iarray) (target : int)
      (first : int) (past : int) =
    0 <= first && first <= past && past <= Iarray.length array
    && Vox_iarray.Int.sorted array
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
        && Vox_iarray.Int.sorted array} @ ghost ->
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
      (if past < first then
          (partition array target lower_strict
            lower_left first past (refine_ u);
          (refine_ u : {u : unit | first <= past}))
        else (refine_ u : {u : unit | first <= past}));
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
        && Vox_iarray.Int.sorted array} @ ghost ->
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
        && Vox_iarray.Int.sorted array} @ ghost ->
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
        && Vox_iarray.Int.sorted array} @ ghost ->
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
        && Vox_iarray.Int.sorted source} @ ghost ->
      {pair : int * int iarray | match pair with position, result ->
        0 <= position && position <= Iarray.length source
        && Iarray.length result = Iarray.length source + 1
        && Vox_iarray.Int.sorted result
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
          {u : unit | edited source result position value inserting start stop} =
        fun start bounds ->
        let refine_ bounds = bounds in
        edited_def source result position value inserting start stop;
        if start = stop then refine_ u
        else
          let next = start + 1 in
          certify next (refine_ u);
          at_def result start;
          edit_value_def source position value inserting start;
          let original = if start < position then start else start - 1 in
          at_def source original;

          refine_ u
      [@@decreases stop - start]
      in
      certify zero (refine_ u);
      Vox_iarray.Int.sorted_intro result (fun start ->
        if 0 <= start && start < stop - 1 then (
          let next = start + 1 in
          let original = if start < position then start else start - 1 in
          let original_next = if next < position then next else next - 1 in
          edited_at source result position value
            inserting zero stop start (refine_ u);
          edited_at source result position value
            inserting zero stop next (refine_ u);
          edit_value_def source position value inserting start;
          edit_value_def source position value inserting next;
          (if start = position then
              (range_at source value position past original_next (refine_ u);
              (refine_ u : {u : unit | at result start <= at result next}))
            else if next = position then
              (range_at source value position past original (refine_ u);
              (refine_ u : {u : unit | at result start <= at result next}))
            else
              (ordered source original original_next
                (refine_ u);
              (refine_ u : {u : unit | at result start <= at result next})));
          at_model result start;
          at_model result next;
          refine_ u)
        else refine_ u);
      (refine_ u : {u : unit | Vox_iarray.Int.sorted result
        && edited source result position value true 0 (Iarray.length result)}));
    let pair = position, result in
    refine_ pair


  let remove_at :
      (source : int iarray) -> (position : int) ->
      {u : unit | 0 <= position && position < Iarray.length source
        && Vox_iarray.Int.sorted source} @ ghost ->
      {result : int iarray | Iarray.length result = Iarray.length source - 1
        && Vox_iarray.Int.sorted result
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
          {u : unit | edited source result position value inserting start stop} =
        fun start bounds ->
        let refine_ bounds = bounds in
        edited_def source result position value inserting start stop;
        if start = stop then refine_ u
        else
          let next = start + 1 in
          certify next (refine_ u);
          at_def result start;
          edit_value_def source position value inserting start;
          let original = if start < position then start else start + 1 in
          at_def source original;

          refine_ u
      [@@decreases stop - start]
      in
      certify zero (refine_ u);
      Vox_iarray.Int.sorted_intro result (fun start ->
        if 0 <= start && start < stop - 1 then (
          let next = start + 1 in
          let original = if start < position then start else start + 1 in
          let original_next = if next < position then next else next + 1 in
          edited_at source result position value
            inserting zero stop start (refine_ u);
          edited_at source result position value
            inserting zero stop next (refine_ u);
          edit_value_def source position value inserting start;
          edit_value_def source position value inserting next;
          ordered source original original_next (refine_ u);
          at_model result start;
          at_model result next;
          refine_ u)
        else refine_ u);
      (refine_ u : {u : unit | Vox_iarray.Int.sorted result
        && edited source result position 0 false 0 (Iarray.length result)}));
    refine_ result

  let remove_one :
      (source : int iarray) -> (value : int) ->
      {u : unit | 0 < Iarray.length source + 1
        && Vox_iarray.Int.sorted source} @ ghost ->
      {result : (int * int iarray) option | match result with
        | None -> not (occurs source value 0 (Iarray.length source))
        | Some (position, array) ->
          0 <= position && position < Iarray.length source
          && at source position = value
          && not (occurs source value 0 position)
          && Iarray.length array = Iarray.length source - 1
          && Vox_iarray.Int.sorted array
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

end
