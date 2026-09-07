type ('a : immutable_data) t = 'a list

external length : ('a : immutable_data).
  'a t @ immutable -> Bigint.t @@ total = "caml_borrow_model_length"

val length_def : ('a : immutable_data).
  (values : 'a t) @ immutable ->
  {u : unit | length values ===
    (match values with [] -> 0Z | _ :: tail -> Bigint.add 1Z (length tail))}
  @@ total

val append : ('a : immutable_data).
  'a t @ immutable total -> 'a t @ immutable total ->
  'a t @ immutable total @@ total
val append_def : ('a : immutable_data).
  (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
  {u : unit | append left right ===
    (match left with [] -> right | head :: tail -> head :: append tail right)}
  @@ total

val at : ('a : immutable_data).
  'a t @ immutable total -> Bigint.t -> 'a option @ immutable total @@ total
val at_def : ('a : immutable_data).
  (values : 'a t) @ immutable -> (index : Bigint.t) ->
  {u : unit | at values index ===
    (match values with [] -> None | head :: tail ->
      if Bigint.equal index 0Z then Some head
      else at tail (Bigint.sub index 1Z))} @@ total

val set : ('a : immutable_data).
  'a t @ immutable total -> Bigint.t -> 'a @ immutable total ->
  'a t @ immutable total @@ total
val set_def : ('a : immutable_data).
  (values : 'a t) @ immutable -> (index : Bigint.t) ->
  (value : 'a) @ immutable ->
  {u : unit | set values index value ===
    (match values with [] -> [] | head :: tail ->
      if Bigint.equal index 0Z then value :: tail
      else head :: set tail (Bigint.sub index 1Z) value)} @@ total

val take : ('a : immutable_data).
  Bigint.t -> 'a t @ immutable total -> 'a t @ immutable total @@ total
val take_def : ('a : immutable_data).
  (count : Bigint.t) -> (values : 'a t) @ immutable ->
  {u : unit | take count values ===
    (match values with [] -> [] | head :: tail ->
      if Bigint.compare count 0Z <= 0 then []
      else head :: take (Bigint.sub count 1Z) tail)} @@ total

val drop : ('a : immutable_data).
  Bigint.t -> 'a t @ immutable total -> 'a t @ immutable total @@ total
val drop_def : ('a : immutable_data).
  (count : Bigint.t) -> (values : 'a t) @ immutable ->
  {u : unit | drop count values ===
    (match values with [] -> [] | _ :: tail ->
      if Bigint.compare count 0Z <= 0 then values
      else drop (Bigint.sub count 1Z) tail)} @@ total

val sub : ('a : immutable_data).
  'a t @ immutable total -> Bigint.t -> Bigint.t ->
  'a t @ immutable total @@ total
val sub_def : ('a : immutable_data).
  (values : 'a t) @ immutable -> (first : Bigint.t) -> (past : Bigint.t) ->
  {u : unit | sub values first past ===
    take (Bigint.sub past first) (drop first values)} @@ total

val of_iarray : ('a : immutable_data).
  'a iarray @ immutable total -> 'a t @ immutable total @@ total

val append_length : ('a : immutable_data).
  (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
  {u : unit | length (append left right) ===
    Bigint.add (length left) (length right)} @@ total
val append_split : ('a : immutable_data).
  (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
  {u : unit | take (length left) (append left right) === left
    && drop (length left) (append left right) === right} @@ total
val set_length : ('a : immutable_data).
  (values : 'a t) @ immutable -> (index : Bigint.t) ->
  (value : 'a) @ immutable ->
  {u : unit | length (set values index value) === length values} @@ total

val swap : ('a : immutable_data).
  'a t @ immutable total -> Bigint.t -> Bigint.t ->
  'a t @ immutable total @@ total
val swap_def : ('a : immutable_data).
  (values : 'a t) @ immutable -> (first : Bigint.t) -> (second : Bigint.t) ->
  {u : unit | swap values first second ===
    (match at values first, at values second with
    | Some x, Some y -> set (set values first y) second x
    | _ -> values)} @@ total

val cut : ('a : immutable_data).
  (values : 'a t) @ immutable -> (count : Bigint.t) ->
  {u : unit | if Bigint.compare count 0Z >= 0
    && Bigint.compare count (length values) <= 0 then
    length (take count values) === count
    && length (drop count values) === Bigint.sub (length values) count
    && append (take count values) (drop count values) === values
    else true} @@ total
val drop_add : ('a : immutable_data).
  (values : 'a t) @ immutable -> (first : Bigint.t) -> (second : Bigint.t) ->
  {u : unit | if Bigint.compare first 0Z >= 0
    && Bigint.compare second 0Z >= 0 then
    drop second (drop first values) === drop (Bigint.add first second) values
    else true} @@ total

val sub_length : ('a : immutable_data).
  (values : 'a t) @ immutable -> (first : Bigint.t) -> (past : Bigint.t) ->
  {u : unit | if Bigint.compare first 0Z >= 0 && Bigint.compare first past <= 0
    && Bigint.compare past (length values) <= 0 then
    length (sub values first past) === Bigint.sub past first else true} @@ total

external iarray_get : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {i : int | 0 <= i && i < Iarray.length values} ->
  'a @ immutable total @@ total = "%array_safe_get"
val of_iarray_length : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {u : unit | length (of_iarray values) === Bigint.of_int (Iarray.length values)} @@ total
val of_iarray_at : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  (index : {i : int | 0 <= i && i < Iarray.length values}) ->
  {u : unit | let refine_ i = index in
    at (of_iarray values) (Bigint.of_int i) === Some (iarray_get values index)} @@ total
