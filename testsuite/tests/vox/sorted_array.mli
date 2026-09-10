type t : immutable_data

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
