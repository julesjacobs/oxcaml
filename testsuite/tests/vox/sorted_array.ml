open Sorted_array_proofs

type t = {array : int iarray |
  0 < Iarray.length array + 1 && Vox_iarray.Int.sorted array}

let[@def] (contents @ total) (array : t) =
  let refine_ array = array in ghost_ (Vox_sequence.of_iarray array)

let[@def] (length @ total) (array : t) =
  let refine_ array = array in Iarray.length array

let[@def] (at @ total) (array : t) (index : int) =
  let refine_ array = array in Arrays.at array index

let[@def] (occurs @ total) (array : t) (value : int) =
  let refine_ array = array in
  Arrays.occurs array value 0 (Iarray.length array)

let[@def] (occurs_between @ total) (array : t) (value : int)
    (start : int) (stop : int) =
  let refine_ array = array in Arrays.occurs array value start stop

let[@def] (range_spec @ total) (array : t) (value : int)
    (first : int) (past : int) =
  let refine_ array = array in Arrays.range_spec array value first past

let[@def] (edited @ total) (source : t) (result : t)
    (position : int) (value : int) (inserting : bool) =
  let refine_ source = source in
  let refine_ result = result in
  Arrays.edited source result position value inserting 0 (Iarray.length result)

let (empty @ total) : {array : t | length array = 0} =
  let array = [: :] in
  ghost_ (Vox_iarray.Int.sorted_intro array
    (fun index -> let u = () in refine_ u));
  let wrapped : t = refine_ array in
  ghost_ (length_def wrapped);
  refine_ wrapped

let (mem @ total) : (array : t) -> (value : int) ->
    {result : bool | result = occurs array value} =
  fun array value ->
  let refine_ source = array in
  let u = () in
  let refine_ result = Arrays.mem source value (refine_ u) in
  ghost_ (occurs_def array value);
  refine_ result

let (equal_range @ total) : (array : t) -> (value : int) ->
    {result : int * int | match result with first, past ->
      0 <= first && first <= past && past <= length array
      && occurs array value = (first < past)
      && range_spec array value first past} =
  fun array value ->
  let refine_ source = array in
  let u = () in
  let refine_ result = Arrays.equal_range source value (refine_ u) in
  let (first : int), (past : int) = result in
  ghost_ (
    Arrays.range_spec_def source value first past;
    length_def array;
    occurs_def array value;
    range_spec_def array value first past;
    (refine_ u : {u : unit | 0 <= first && first <= past
      && past <= length array && occurs array value = (first < past)
      && range_spec array value first past}));
  refine_ result

let insert : (source : t) -> (value : int) ->
    {u : unit | 0 < length source + 2} @ ghost ->
    {pair : int * t | match pair with position, result ->
      0 <= position && position <= length source
      && length result = length source + 1
      && occurs result value && edited source result position value true} =
  fun source value capacity ->
  capacity;
  let refine_ raw_source = source in
  ghost_ (length_def source);
  let u = () in
  let refine_ pair = Arrays.insert raw_source value (refine_ u) in
  let (position : int), (raw_result : int iarray) = pair in
  let result : t = refine_ raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = true in
    let stop = Iarray.length raw_result in
    Arrays.edited_at raw_source raw_result position value
      inserting zero stop position (refine_ u);
    Arrays.edit_value_def raw_source position value inserting position;
    let refine_ range_spec = Arrays.equal_range raw_result value (refine_ u) in
    let (first : int), (past : int) = range_spec in
    Arrays.range_at raw_result value first past position (refine_ u);
    length_def result;
    occurs_def result value;
    edited_def source result position value inserting;
    (refine_ u : {u : unit | occurs result value
      && length result = length source + 1
      && edited source result position value true}));
  let pair = position, result in
  refine_ pair

let remove_at : (source : t) -> (position : int) ->
    {u : unit | 0 <= position && position < length source} @ ghost ->
    {result : t | length result = length source - 1
      && edited source result position 0 false} =
  fun source position bounds ->
  let refine_ bounds = bounds in
  let refine_ raw_source = source in
  ghost_ (length_def source);
  let u = () in
  let refine_ raw_result = Arrays.remove_at raw_source position (refine_ u) in
  let result : t = refine_ raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = false in
    length_def result;
    edited_def source result position zero inserting;
    (refine_ u : {u : unit | length result = length source - 1
      && edited source result position 0 false}));
  refine_ result

let (edited_at @ total) : (source : t) -> (result : t) ->
    (position : int) -> (value : int) -> (inserting : bool) -> (index : int) ->
    {u : unit | 0 <= index && index < length result
      && edited source result position value inserting} @ ghost ->
    {u : unit | at result index =
      (if index < position then at source index
       else if inserting then
         if index = position then value else at source (index - 1)
       else at source (index + 1))} =
  fun source result position value inserting index premise ->
  premise;
  let refine_ raw_source = source in
  let refine_ raw_result = result in
  length_def result;
  edited_def source result position value inserting;
  let zero = 0 in
  let stop = Iarray.length raw_result in
  let u = () in
  Arrays.edited_at raw_source raw_result position
    value inserting zero stop index (refine_ u);
  Arrays.edit_value_def raw_source position value inserting index;
  at_def result index;
  let original =
    if index < position then index
    else if inserting then index - 1 else index + 1 in
  at_def source original;
  refine_ u

let (ordered @ total) : (array : t) -> (left : int) -> (right : int) ->
    {u : unit | 0 <= left && left <= right && right < length array} @ ghost ->
    {u : unit | at array left <= at array right} =
  fun array left right bounds ->
  let refine_ bounds = bounds in
  let refine_ raw = array in
  length_def array;
  let u = () in
  Arrays.ordered raw left right (refine_ u);
  at_def array left;
  at_def array right;
  refine_ u

let (find_first @ total) : (array : t) -> (value : int) ->
    {result : int option | match result with
      | None -> not (occurs array value)
      | Some index -> 0 <= index && index < length array
        && at array index = value
        && not (occurs_between array value 0 index)} =
  fun array value ->
  let refine_ raw = array in
  let u = () in
  let refine_ result = Arrays.find_first raw value (refine_ u) in
  ghost_ (length_def array);
  ghost_ (occurs_def array value);
  match result with
  | None -> refine_ result
  | Some index ->
    let index : int = index in
    ghost_ (at_def array index);
    let zero = 0 in
    ghost_ (occurs_between_def array value zero index);
    refine_ result

let remove_one : (source : t) -> (value : int) ->
    {result : (int * t) option | match result with
      | None -> not (occurs source value)
      | Some (position, array) ->
        0 <= position && position < length source
        && at source position = value
        && not (occurs_between source value 0 position)
        && length array = length source - 1
        && edited source array position 0 false} =
  fun source value ->
  let refine_ found = find_first source value in
  match found with
  | None ->
    let result = None in
    refine_ result
  | Some position ->
    let position : int = position in
    let u = () in
    let refine_ array = remove_at source position (refine_ u) in
    let result = Some (position, array) in
    refine_ result

let (range_at @ total) : (array : t) -> (value : int) ->
    (first : int) -> (past : int) -> (index : int) ->
    {u : unit | range_spec array value first past
      && 0 <= index && index < length array} @ ghost ->
    {u : unit |
      (if index < first then at array index < value
       else if index < past then at array index = value
       else at array index > value)} =
  fun array value first past index premise ->
  premise;
  let refine_ raw = array in
  length_def array;
  range_spec_def array value first past;
  let u = () in
  Arrays.range_at raw value first past index (refine_ u);
  at_def array index;
  refine_ u

let (find_last @ total) : (array : t) -> (value : int) ->
    {result : int option | match result with
      | None -> not (occurs array value)
      | Some index -> 0 <= index && index < length array
        && at array index = value
        && not (occurs_between array value (index + 1) (length array))} =
  fun array value ->
  let refine_ raw = array in
  let u = () in
  let refine_ result = Arrays.find_last raw value (refine_ u) in
  ghost_ (length_def array);
  ghost_ (occurs_def array value);
  match result with
  | None -> refine_ result
  | Some index ->
    let index : int = index in
    ghost_ (at_def array index);
    let next = index + 1 in
    let stop = length array in
    ghost_ (occurs_between_def array value next stop);
    refine_ result

let (contents_length @ total) : (array : t) ->
    {u : unit | Vox_sequence.length (contents array) ===
      Bigint.of_int (length array)} = fun array ->
  let refine_ source = array in
  ghost_ (length_def array);
  ghost_ (contents_def array);
  ghost_ (Vox_sequence.of_iarray_length source);
  let u = () in refine_ u

let (contents_at @ total) : (array : t) ->
    (index : {i : int | 0 <= i && i < length array}) ->
    {u : unit | let refine_ i = index in
      Vox_sequence.at (contents array) (Bigint.of_int i) === Some (at array i)} =
    fun array index ->
  let refine_ source = array in
  let refine_ i = index in
  ghost_ (length_def array);
  let bounded : {j : int | 0 <= j && j < Iarray.length source} = refine_ i in
  let refine_ value = Vox_sequence.Iarray.get source bounded in
  ghost_ (contents_def array);
  ghost_ (at_def array i);
  ghost_ (Arrays.at_def source i);
  let u = () in refine_ u
