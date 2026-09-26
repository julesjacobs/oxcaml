type t : immutable_data

val contents : t -> int Vox_sequence.t @ immutable total ghost @@ total

val length : t -> int @@ total
val at : t -> int -> int @@ total
val occurs : t -> int -> bool @@ total
val occurs_between : t -> int -> int -> int -> bool @@ total
val range_spec : t -> int -> int -> int -> bool @@ total
val edited : t -> t -> int -> int -> bool -> bool @@ total

val empty : {array : t | length array = 0} @@ total
val mem : (array : t) -> (value : int) ->
  {result : bool | result = occurs array value} @@ total
val equal_range : (array : t) -> (value : int) ->
  {result : int * int | match result with first, past ->
    0 <= first && first <= past && past <= length array
    && occurs array value = (first < past)
    && range_spec array value first past} @@ total

val insert : (source : t) -> (value : int) ->
  {u : unit | 0 < length source + 2} @ ghost ->
  {pair : int * t | match pair with position, result ->
    0 <= position && position <= length source
    && length result = length source + 1
    && occurs result value && edited source result position value true}
val remove_at : (source : t) -> (position : int) ->
  {u : unit | 0 <= position && position < length source} @ ghost ->
  {result : t | length result = length source - 1
    && edited source result position 0 false}

val edited_at : (source : t) -> (result : t) ->
  (position : int) -> (value : int) -> (inserting : bool) -> (index : int) ->
  {u : unit | 0 <= index && index < length result
    && edited source result position value inserting} @ ghost ->
  {u : unit | at result index =
    (if index < position then at source index
     else if inserting then
       if index = position then value else at source (index - 1)
     else at source (index + 1))} @@ total

val ordered : (array : t) -> (left : int) -> (right : int) ->
  {u : unit | 0 <= left && left <= right && right < length array} @ ghost ->
  {u : unit | at array left <= at array right} @@ total
val find_first : (array : t) -> (value : int) ->
  {result : int option | match result with
    | None -> not (occurs array value)
    | Some index -> 0 <= index && index < length array
      && at array index = value
      && not (occurs_between array value 0 index)} @@ total
val remove_one : (source : t) -> (value : int) ->
  {result : (int * t) option | match result with
    | None -> not (occurs source value)
    | Some (position, array) ->
      0 <= position && position < length source
      && at source position = value
      && not (occurs_between source value 0 position)
      && length array = length source - 1
      && edited source array position 0 false}

val range_at : (array : t) -> (value : int) ->
  (first : int) -> (past : int) -> (index : int) ->
  {u : unit | range_spec array value first past
    && 0 <= index && index < length array} @ ghost ->
  {u : unit |
    (if index < first then at array index < value
     else if index < past then at array index = value
     else at array index > value)} @@ total
val find_last : (array : t) -> (value : int) ->
  {result : int option | match result with
    | None -> not (occurs array value)
    | Some index -> 0 <= index && index < length array
      && at array index = value
      && not (occurs_between array value (index + 1) (length array))} @@ total

val contents_length : (array : t) ->
  {u : unit | Vox_sequence.length (contents array) ===
    Bigint.of_int (length array)} @@ total
val contents_at : (array : t) ->
  (index : {i : int | 0 <= i && i < length array}) ->
  {u : unit | let i = index in
    Vox_sequence.at (contents array) (Bigint.of_int i) === Some (at array i)}
  @@ total

val length_bounds : (array : t) ->
  {u : unit | 0 <= length array && 0 < length array + 1} @@ total

val at_outside : (array : t) -> (index : int) ->
  {u : unit | if index < 0 || length array <= index then
    at array index = 0 else true} @@ total

val occurs_equation : (array : t) -> (value : int) ->
  {u : unit | occurs array value =
    occurs_between array value 0 (length array)} @@ total

val occurs_between_equation : (array : t) -> (value : int) ->
  (start : int) -> (stop : int) ->
  {u : unit | occurs_between array value start stop =
    (if 0 <= start && start < stop then
      at array start = value || occurs_between array value (start + 1) stop
     else false)} @@ total

val range_equation : (array : t) -> (value : int) ->
  (first : int) -> (past : int) ->
  {u : unit | range_spec array value first past =
    (0 <= first && first <= past && past <= length array
      && (first = 0 || at array (first - 1) < value)
      && (first = length array || value <= at array first)
      && (past = 0 || at array (past - 1) <= value)
      && (past = length array || value < at array past))} @@ total

val edit_suffix : t -> t -> int -> int -> bool -> int -> bool @@ total
val edit_suffix_equation : (source : t) -> (result : t) ->
  (position : int) -> (value : int) -> (inserting : bool) -> (index : int) ->
  {u : unit | edit_suffix source result position value inserting index =
    (if 0 <= index && index < length result then
      at result index =
        (if index < position then at source index
         else if inserting then
           if index = position then value else at source (index - 1)
         else at source (index + 1))
      && edit_suffix source result position value inserting (index + 1)
     else true)} @@ total
val edited_equation : (source : t) -> (result : t) ->
  (position : int) -> (value : int) -> (inserting : bool) ->
  {u : unit | edited source result position value inserting =
    edit_suffix source result position value inserting 0} @@ total
