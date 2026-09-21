open Sorted_array_proofs

type t = {array : int iarray |
  0 < Iarray.length array + 1 && Vox_iarray.Int.sorted array}

let[@def] (contents @ total) (array : t) =
  ghost_ (Vox_sequence.of_iarray array)

let[@def] (length @ total) (array : t) =
  Iarray.length array

let[@def] (at @ total) (array : t) (index : int) =
  Arrays.at array index

let[@def] (occurs @ total) (array : t) (value : int) =
  Arrays.occurs array value 0 (Iarray.length array)

let[@def] (occurs_between @ total) (array : t) (value : int)
    (start : int) (stop : int) =
  Arrays.occurs array value start stop

let[@def] (range_spec @ total) (array : t) (value : int)
    (first : int) (past : int) =
  Arrays.range_spec array value first past

let[@def] (edited @ total) (source : t) (result : t)
    (position : int) (value : int) (inserting : bool) =
  Arrays.edited source result position value inserting 0 (Iarray.length result)

let (empty @ total) : {array : t | length array = 0} =
  let array = [: :] in
  ghost_ (Vox_iarray.Int.sorted_intro array
    (fun index -> let u = () in u));
  let wrapped : t = array in
  ghost_ (length_def wrapped);
  wrapped

let (mem @ total) : (array : t) -> (value : int) ->
    {result : bool | result = occurs array value} =
  fun array value ->
  let source = array in
  let u = () in
  let result = Arrays.mem source value (u) in
  ghost_ (occurs_def array value);
  result

let (equal_range @ total) : (array : t) -> (value : int) ->
    {result : int * int | match result with first, past ->
      0 <= first && first <= past && past <= length array
      && occurs array value = (first < past)
      && range_spec array value first past} =
  fun array value ->
  let source = array in
  let u = () in
  let result = Arrays.equal_range source value (u) in
  let (first : int), (past : int) = result in
  ghost_ (
    Arrays.range_spec_def source value first past;
    length_def array;
    occurs_def array value;
    range_spec_def array value first past;
    (u : {u : unit | 0 <= first && first <= past
      && past <= length array && occurs array value = (first < past)
      && range_spec array value first past}));
  result

let insert : (source : t) -> (value : int) ->
    {u : unit | 0 < length source + 2} @ ghost ->
    {pair : int * t | match pair with position, result ->
      0 <= position && position <= length source
      && length result = length source + 1
      && occurs result value && edited source result position value true} =
  fun source value capacity ->
  capacity;
  let raw_source = source in
  ghost_ (length_def source);
  let u = () in
  let pair = Arrays.insert raw_source value (u) in
  let (position : int), (raw_result : int iarray) = pair in
  let result : t = raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = true in
    let stop = Iarray.length raw_result in
    Arrays.edited_at raw_source raw_result position value
      inserting zero stop position (u);
    Arrays.edit_value_def raw_source position value inserting position;
    let range_spec = Arrays.equal_range raw_result value (u) in
    let (first : int), (past : int) = range_spec in
    Arrays.range_at raw_result value first past position (u);
    length_def result;
    occurs_def result value;
    edited_def source result position value inserting;
    (u : {u : unit | occurs result value
      && length result = length source + 1
      && edited source result position value true}));
  let pair : int * t = position, result in
  pair

let remove_at : (source : t) -> (position : int) ->
    {u : unit | 0 <= position && position < length source} @ ghost ->
    {result : t | length result = length source - 1
      && edited source result position 0 false} =
  fun source position bounds ->
  let raw_source = source in
  ghost_ (length_def source);
  let u = () in
  let raw_result = Arrays.remove_at raw_source position (u) in
  let result : t = raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = false in
    length_def result;
    edited_def source result position zero inserting;
    (u : {u : unit | length result = length source - 1
      && edited source result position 0 false}));
  result

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
  let raw_source = source in
  let raw_result = result in
  length_def result;
  edited_def source result position value inserting;
  let zero = 0 in
  let stop = Iarray.length raw_result in
  let u = () in
  Arrays.edited_at raw_source raw_result position
    value inserting zero stop index (u);
  Arrays.edit_value_def raw_source position value inserting index;
  at_def result index;
  let original =
    if index < position then index
    else if inserting then index - 1 else index + 1 in
  at_def source original;
  u

let (ordered @ total) : (array : t) -> (left : int) -> (right : int) ->
    {u : unit | 0 <= left && left <= right && right < length array} @ ghost ->
    {u : unit | at array left <= at array right} =
  fun array left right bounds ->
  let raw = array in
  length_def array;
  let u = () in
  Arrays.ordered raw left right (u);
  at_def array left;
  at_def array right;
  u

let (find_first @ total) : (array : t) -> (value : int) ->
    {result : int option | match result with
      | None -> not (occurs array value)
      | Some index -> 0 <= index && index < length array
        && at array index = value
        && not (occurs_between array value 0 index)} =
  fun array value ->
  let raw = array in
  let u = () in
  let result = Arrays.find_first raw value (u) in
  ghost_ (length_def array);
  ghost_ (occurs_def array value);
  match result with
  | None -> result
  | Some index ->
    let index : int = index in
    ghost_ (at_def array index);
    let zero = 0 in
    ghost_ (occurs_between_def array value zero index);
    result

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
  let found = find_first source value in
  match found with
  | None ->
    let result = None in
    result
  | Some position ->
    let position : int = position in
    let u = () in
    let array = remove_at source position (u) in
    let result : (int * t) option = Some (position, array) in
    result

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
  let raw = array in
  length_def array;
  range_spec_def array value first past;
  let u = () in
  Arrays.range_at raw value first past index (u);
  at_def array index;
  u

let (find_last @ total) : (array : t) -> (value : int) ->
    {result : int option | match result with
      | None -> not (occurs array value)
      | Some index -> 0 <= index && index < length array
        && at array index = value
        && not (occurs_between array value (index + 1) (length array))} =
  fun array value ->
  let raw = array in
  let u = () in
  let result = Arrays.find_last raw value (u) in
  ghost_ (length_def array);
  ghost_ (occurs_def array value);
  match result with
  | None -> result
  | Some index ->
    let index : int = index in
    ghost_ (at_def array index);
    let next = index + 1 in
    let stop = length array in
    ghost_ (occurs_between_def array value next stop);
    result

let (contents_length @ total) : (array : t) ->
    {u : unit | Vox_sequence.length (contents array) ===
      Bigint.of_int (length array)} = fun array ->
  let source = array in
  ghost_ (length_def array);
  ghost_ (contents_def array);
  ghost_ (Vox_sequence.of_iarray_length source);
  let u = () in u

let (contents_at @ total) : (array : t) ->
    (index : {i : int | 0 <= i && i < length array}) ->
    {u : unit | let i = index in
      Vox_sequence.at (contents array) (Bigint.of_int i) === Some (at array i)} =
    fun array index ->
  let source = array in
  let i = index in
  ghost_ (length_def array);
  let bounded : {j : int | 0 <= j && j < Iarray.length source} = i in
  let _value = Vox_sequence.Iarray.get source bounded in
  ghost_ (contents_def array);
  ghost_ (at_def array i);
  ghost_ (Arrays.at_def source i);
  let u = () in u
