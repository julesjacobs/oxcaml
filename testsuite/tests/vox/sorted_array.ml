open Sorted_array_proofs
module S = Vox_sequence
module M = Sorted_array_model

let rec (take_at @ total) (source : int list @ immutable)
    (count : Bigint.t) (index : Bigint.t) :
    {u : unit | if 0Z <= count && 0Z <= index then
      S.at (S.take count source) index ===
        (if index < count then S.at source index else None) else true}
    @ ghost = ghost_ (
  S.take_def count source;
  S.at_def source index;
  S.at_def (S.take count source) index;
  match source with
  | [] -> ()
  | _ :: rest ->
    if count <= 0Z then ()
    else if index <= 0Z then ()
    else take_at rest (Bigint.sub count 1Z) (Bigint.sub index 1Z))

let rec (drop_at @ total) (source : int list @ immutable)
    (count : Bigint.t) (index : Bigint.t) :
    {u : unit | if 0Z <= count && 0Z <= index then
      S.at (S.drop count source) index ===
        S.at source (Bigint.add count index) else true} @ ghost = ghost_ (
  S.drop_def count source;
  S.at_def (S.drop count source) index;
  S.at_def source (Bigint.add count index);
  match source with
  | [] -> ()
  | _ :: rest ->
    if count <= 0Z then ()
    else drop_at rest (Bigint.sub count 1Z) index)

let rec (append_at @ total) (left : int list @ immutable)
    (right : int list @ immutable) (index : Bigint.t) :
    {u : unit | if 0Z <= index then
      S.at (S.append left right) index ===
        (if index < S.length left then S.at left index
         else S.at right (Bigint.sub index (S.length left))) else true}
    @ ghost = ghost_ (
  S.append_def left right; S.length_def left;
  S.at_def left index; S.at_def (S.append left right) index;
  match left with
  | [] -> ()
  | _ :: rest ->
    if index <= 0Z then ()
    else append_at rest right (Bigint.sub index 1Z))

let (insert_length @ total) (source : int list @ immutable)
    (position : Bigint.t) (value : int) :
    {u : unit | if 0Z <= position && position <= S.length source then
      S.length (M.insert source position value) ===
        Bigint.add (S.length source) 1Z else true} @ ghost = ghost_ (
  M.insert_def source position value;
  S.cut source position;
  S.length_def (value :: S.drop position source);
  S.append_length (S.take position source) (value :: S.drop position source))

let (remove_length @ total) (source : int list @ immutable)
    (position : Bigint.t) :
    {u : unit | if 0Z <= position && position < S.length source then
      S.length (M.remove source position) ===
        Bigint.sub (S.length source) 1Z else true} @ ghost = ghost_ (
  M.remove_def source position;
  S.cut source position;
  S.cut source (Bigint.add position 1Z);
  S.append_length (S.take position source)
    (S.drop (Bigint.add position 1Z) source))

let (insert_at @ total) (source : int list @ immutable)
    (position : Bigint.t) (value : int) (index : Bigint.t) :
    {u : unit | if 0Z <= position && position <= S.length source
      && 0Z <= index then
      S.at (M.insert source position value) index ===
        (if index < position then S.at source index
         else if index = position then Some value
         else S.at source (Bigint.sub index 1Z)) else true}
    @ ghost = ghost_ (
  M.insert_def source position value; S.cut source position;
  append_at (S.take position source) (value :: S.drop position source) index;
  take_at source position index;
  let suffix_index = Bigint.sub index position in
  S.at_def (value :: S.drop position source) suffix_index;
  drop_at source position (Bigint.sub suffix_index 1Z))

let (remove_at_model @ total) (source : int list @ immutable)
    (position : Bigint.t) (index : Bigint.t) :
    {u : unit | if 0Z <= position && position < S.length source
      && 0Z <= index then
      S.at (M.remove source position) index ===
        (if index < position then S.at source index
         else S.at source (Bigint.add index 1Z)) else true}
    @ ghost = ghost_ (
  M.remove_def source position; S.cut source position;
  append_at (S.take position source)
    (S.drop (Bigint.add position 1Z) source) index;
  take_at source position index;
  drop_at source (Bigint.add position 1Z) (Bigint.sub index position))

let (edited_contents @ total) (source : int iarray @ immutable)
    (result : int iarray @ immutable) (position : int) (value : int)
    (inserting : bool) :
    {u : unit | if 0 <= position && 0 < Iarray.length source + 1
      && (if inserting then position <= Iarray.length source
            && Iarray.length result = Iarray.length source + 1
          else position < Iarray.length source
            && Iarray.length result = Iarray.length source - 1)
      && Arrays.edited source result position value inserting 0
           (Iarray.length result) then
      S.of_iarray result ===
        (if inserting then M.insert (S.of_iarray source)
          (Bigint.of_int position) value
         else M.remove (S.of_iarray source) (Bigint.of_int position))
      else true} @ ghost = ghost_ (
  let before = S.of_iarray source in
  let after = S.of_iarray result in
  let p = Bigint.of_int position in
  let expected = if inserting then M.insert before p value else M.remove before p in
  S.of_iarray_length source; S.of_iarray_length result;
  if inserting then insert_length before p value else remove_length before p;
  if not (0 <= position && 0 < Iarray.length source + 1
    && (if inserting then position <= Iarray.length source
          && Iarray.length result = Iarray.length source + 1
        else position < Iarray.length source
          && Iarray.length result = Iarray.length source - 1)
    && Arrays.edited source result position value inserting 0
         (Iarray.length result)) then ()
  else S.extensional after expected (fun index ->
    if index < 0Z then ()
    else if Bigint.of_int (Iarray.length result) <= index then (
      S.at_outside after index; S.at_outside expected index)
    else match Bigint.to_int_opt index with
    | None -> ()
    | Some i ->
      S.of_iarray_at result i;
      Arrays.edited_at source result position value inserting 0
        (Iarray.length result) i ();
      Arrays.edit_value_def source position value inserting i;
      Arrays.at_def result i;
      if inserting then insert_at before p value index
      else remove_at_model before p index;
      if inserting && i = position then ()
      else (
        let original = if i < position then i
          else if inserting then i - 1 else i + 1 in
        S.of_iarray_at source original;
        Arrays.at_def source original)))

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

let[@def transparent] (inserted @ total) (source : t) (result : t)
    (position : int) (value : int) = ghost_ (
  0 <= position && position <= length source
  && length result = length source + 1
  && contents result === M.insert (contents source) (Bigint.of_int position) value)

let[@def transparent] (removed @ total) (source : t) (result : t)
    (position : int) = ghost_ (
  0 <= position && position < length source
  && length result = length source - 1
  && contents result === M.remove (contents source) (Bigint.of_int position))

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
      inserted source result position value && occurs result value} =
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
    contents_def source; contents_def result;
    edited_contents source result position value true;
    (() : {u : unit | occurs result value
      && length result = length source + 1
      && inserted source result position value}));
  let pair : int * t = position, result in
  pair

let remove_at : (source : t) -> (position : int) ->
    {u : unit | 0 <= position && position < length source} @ ghost ->
    {result : t | removed source result position} =
  fun source position bounds ->
  ghost_ (length_def source);
  let raw_result = Arrays.remove_at source position () in
  let result : t = raw_result in
  ghost_ (
    let zero = 0 in
    let inserting = false in
    length_def result;
    edited_def source result position zero inserting;
    contents_def source; contents_def result;
    edited_contents source result position zero false;
    (() : {u : unit | length result = length source - 1
      && removed source result position}));
  result

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
        removed source array position && at source position = value
        && not (occurs_between source value 0 position)} =
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
    let next = ghost_ (index + 1) in
    let stop = ghost_ (length array) in
    ghost_ (occurs_between_def array value next stop);
    result

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


let (inserted_at @ total) : (source : t) -> (result : t) ->
  (position : int) -> (value : int) -> (index : int) ->
  {u : unit | inserted source result position value
    && 0 <= index && index < length result} @ ghost ->
  {u : unit | at result index =
    (if index < position then at source index
     else if index = position then value else at source (index - 1))} @ ghost =
  fun source result position value index premise -> ghost_ (
    inserted_def source result position value;
    contents_length source; contents_at result index;
    insert_at (contents source) (Bigint.of_int position) value
      (Bigint.of_int index);
    if index = position then () else
      let original = if index < position then index else index - 1 in
      contents_at source original)

let (removed_at @ total) : (source : t) -> (result : t) ->
  (position : int) -> (index : int) ->
  {u : unit | removed source result position
    && 0 <= index && index < length result} @ ghost ->
  {u : unit | at result index =
    (if index < position then at source index else at source (index + 1))} @ ghost =
  fun source result position index premise -> ghost_ (
    removed_def source result position;
    contents_length source; contents_at result index;
    remove_at_model (contents source) (Bigint.of_int position)
      (Bigint.of_int index);
    let original = if index < position then index else index + 1 in
    contents_at source original)
