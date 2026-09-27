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
    (fun index -> ()));
  let wrapped : t = array in
  ghost_ (length_def wrapped);
  wrapped

let (mem @ total) : (array : t) -> (value : int) ->
    {result : bool | result = occurs array value} =
  fun array value ->
  let result = Arrays.mem array value () in
  ghost_ (occurs_def array value);
  result

let (equal_range @ total) : (array : t) -> (value : int) ->
    {result : int * int | match result with first, past ->
      0 <= first && first <= past && past <= length array
      && occurs array value = (first < past)
      && range_spec array value first past} =
  fun array value ->
  let result = Arrays.equal_range array value () in
  let (first : int), (past : int) = result in
  ghost_ (
    Arrays.range_spec_def array value first past;
    length_def array;
    occurs_def array value;
    range_spec_def array value first past;
    (() : {u : unit | 0 <= first && first <= past
      && past <= length array && occurs array value = (first < past)
      && range_spec array value first past}));
  result

let insert : (source : t) -> (value : int) ->
    {pair : int * t | match pair with position, result ->
      0 <= position && position <= length source
      && length result = length source + 1
      && occurs result value && edited source result position value true} =
  fun source value ->
  (* [Iarray.append] raises [Invalid_argument] long before a length gets
     near [max_int]. The checker does not know the runtime's size limit, so
     this test establishes that the result's length plus one does not wrap,
     as the representation invariant requires. *)
  if Iarray.length source + 2 <= 0 then
    raise (Invalid_argument "Sorted_array.insert");
  ghost_ (length_def source);
  let pair = Arrays.insert source value () in
  let (position : int), (raw_result : int iarray) = pair in
  let result : t = raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = true in
    let stop = Iarray.length result in
    Arrays.edited_at source result position value
      inserting zero stop position ();
    Arrays.edit_value_def source position value inserting position;
    let range_spec = Arrays.equal_range result value () in
    let (first : int), (past : int) = range_spec in
    Arrays.range_at result value first past position ();
    length_def result;
    occurs_def result value;
    edited_def source result position value inserting;
    (() : {u : unit | occurs result value
      && length result = length source + 1
      && edited source result position value true}));
  let pair : int * t = position, result in
  pair

let remove_at : (source : t) -> (position : int) ->
    {u : unit | 0 <= position && position < length source} @ ghost ->
    {result : t | length result = length source - 1
      && edited source result position 0 false} =
  fun source position bounds ->
  ghost_ (length_def source);
  let raw_result = Arrays.remove_at source position () in
  let result : t = raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = false in
    length_def result;
    edited_def source result position zero inserting;
    (() : {u : unit | length result = length source - 1
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
  length_def result;
  edited_def source result position value inserting;
  let zero = 0 in
  let stop = Iarray.length result in
  Arrays.edited_at source result position
    value inserting zero stop index ();
  Arrays.edit_value_def source position value inserting index;
  at_def result index;
  let original =
    if index < position then index
    else if inserting then index - 1 else index + 1 in
  at_def source original;
  ()

let (ordered @ total) : (array : t) -> (left : int) -> (right : int) ->
    {u : unit | 0 <= left && left <= right && right < length array} @ ghost ->
    {u : unit | at array left <= at array right} =
  fun array left right bounds ->
  length_def array;
  Arrays.ordered array left right ();
  at_def array left;
  at_def array right;
  ()

let (find_first @ total) : (array : t) -> (value : int) ->
    {result : int option | match result with
      | None -> not (occurs array value)
      | Some index -> 0 <= index && index < length array
        && at array index = value
        && not (occurs_between array value 0 index)} =
  fun array value ->
  let result = Arrays.find_first array value () in
  ghost_ (length_def array);
  ghost_ (occurs_def array value);
  match result with
  | None -> result
  | Some index ->
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
    let array = remove_at source position () in
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
  length_def array;
  range_spec_def array value first past;
  Arrays.range_at array value first past index ();
  at_def array index;
  ()

let (find_last @ total) : (array : t) -> (value : int) ->
    {result : int option | match result with
      | None -> not (occurs array value)
      | Some index -> 0 <= index && index < length array
        && at array index = value
        && not (occurs_between array value (index + 1) (length array))} =
  fun array value ->
  let result = Arrays.find_last array value () in
  ghost_ (length_def array);
  ghost_ (occurs_def array value);
  match result with
  | None -> result
  | Some index ->
    ghost_ (at_def array index);
    let next = index + 1 in
    let stop = length array in
    ghost_ (occurs_between_def array value next stop);
    result

let (contents_length @ total) : (array : t) ->
    {u : unit | Vox_sequence.length (contents array) ===
      Bigint.of_int (length array)} = fun array ->
  ghost_ (length_def array);
  ghost_ (contents_def array);
  ghost_ (Vox_sequence.of_iarray_length array);
  ()

let (contents_at @ total) : (array : t) ->
    (index : {i : int | 0 <= i && i < length array}) ->
    {u : unit | let i = index in
      Vox_sequence.at (contents array) (Bigint.of_int i) === Some (at array i)} =
    fun array index ->
  let i = index in
  ghost_ (length_def array);
  let bounded : {j : int | 0 <= j && j < Iarray.length array} = i in
  let _value = Vox_sequence.Iarray.get array bounded in
  ghost_ (contents_def array);
  ghost_ (at_def array i);
  ghost_ (Arrays.at_def array i);
  ()

let[@def] (edit_suffix @ total) (source : t) (result : t)
    (position : int) (value : int) (inserting : bool) (index : int) =
  Arrays.edited source result position value inserting index (Iarray.length result)

let (length_bounds @ total) : (array : t) ->
  {u : unit | 0 <= length array && 0 < length array + 1} =
  fun array ->
  length_def array;
  ()

let (at_outside @ total) : (array : t) -> (index : int) ->
  {u : unit | if index < 0 || length array <= index then
    at array index = 0 else true} =
  fun array index ->
  length_def array;
  at_def array index;
  Arrays.at_def array index;
  ()

let (occurs_equation @ total) : (array : t) -> (value : int) ->
  {u : unit | occurs array value =
    occurs_between array value 0 (length array)} =
  fun array value ->
  let zero = 0 in
  let stop = length array in
  length_def array;
  occurs_def array value;
  occurs_between_def array value zero stop;
  ()

let (occurs_between_equation @ total) : (array : t) -> (value : int) ->
  (start : int) -> (stop : int) ->
  {u : unit | occurs_between array value start stop =
    (if 0 <= start && start < stop then
      at array start = value || occurs_between array value (start + 1) stop
     else false)} =
  fun array value start stop ->
  occurs_between_def array value start stop;
  Arrays.occurs_def array value start stop;
  at_def array start;
  let next = start + 1 in
  occurs_between_def array value next stop;
  ()

let (range_equation @ total) : (array : t) -> (value : int) ->
  (first : int) -> (past : int) ->
  {u : unit | range_spec array value first past =
    (0 <= first && first <= past && past <= length array
      && (first = 0 || at array (first - 1) < value)
      && (first = length array || value <= at array first)
      && (past = 0 || at array (past - 1) <= value)
      && (past = length array || value < at array past))} =
  fun array value first past ->
  length_def array;
  range_spec_def array value first past;
  Arrays.range_spec_def array value first past;
  let low = false in
  let high = true in
  let before_first = first - 1 in
  let before_past = past - 1 in
  Arrays.above_def array value low before_first;
  Arrays.above_def array value low first;
  Arrays.above_def array value high before_past;
  Arrays.above_def array value high past;
  at_def array before_first;
  at_def array first;
  at_def array before_past;
  at_def array past;
  ()

let (edit_suffix_equation @ total) : (source : t) -> (result : t) ->
  (position : int) -> (value : int) -> (inserting : bool) -> (index : int) ->
  {u : unit | edit_suffix source result position value inserting index =
    (if 0 <= index && index < length result then
      at result index =
        (if index < position then at source index
         else if inserting then
           if index = position then value else at source (index - 1)
         else at source (index + 1))
      && edit_suffix source result position value inserting (index + 1)
     else true)} =
  fun source result position value inserting index ->
  length_def result;
  edit_suffix_def source result position value inserting index;
  let stop = Iarray.length result in
  Arrays.edited_def source result position value inserting index stop;
  Arrays.edit_value_def source position value inserting index;
  at_def result index;
  at_def source index;
  let before = index - 1 in
  let after = index + 1 in
  at_def source before;
  at_def source after;
  edit_suffix_def source result position value inserting after;
  ()

let (edited_equation @ total) : (source : t) -> (result : t) ->
  (position : int) -> (value : int) -> (inserting : bool) ->
  {u : unit | edited source result position value inserting =
    edit_suffix source result position value inserting 0} =
  fun source result position value inserting ->
  let zero = 0 in
  edited_def source result position value inserting;
  edit_suffix_def source result position value inserting zero;
  ()
