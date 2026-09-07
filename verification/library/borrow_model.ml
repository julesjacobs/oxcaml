open! Stdlib

type ('a : immutable_data) t = 'a list

external length : ('a : immutable_data).
  'a t @ immutable -> Bigint.t @@ total = "caml_borrow_model_length"

let (length_def @ total) (values : ('a : immutable_data) t @ immutable) :
    {u : unit | length values ===
      (match values with [] -> 0Z | _ :: tail -> Bigint.add 1Z (length tail))} =
  let u = () in refine_ u

let[@def] rec append (left : ('a : immutable_data) t @ immutable total)
    (right : 'a t @ immutable total) : 'a t @ immutable total =
  match left with [] -> right | head :: tail -> head :: append tail right

let[@def] rec at (values : ('a : immutable_data) t @ immutable total)
    (index : Bigint.t) : 'a option @ immutable total =
  match values with
  | [] -> None
  | head :: tail ->
    if Bigint.equal index 0Z then Some head
    else at tail (Bigint.sub index 1Z)

let[@def] rec set (values : ('a : immutable_data) t @ immutable total)
    (index : Bigint.t) (value : 'a @ immutable total) : 'a t @ immutable total =
  match values with
  | [] -> []
  | head :: tail ->
    if Bigint.equal index 0Z then value :: tail
    else head :: set tail (Bigint.sub index 1Z) value

let[@def] rec take (count : Bigint.t)
    (values : ('a : immutable_data) t @ immutable total) : 'a t @ immutable total =
  match values with
  | [] -> []
  | head :: tail ->
    if Bigint.compare count 0Z <= 0 then []
    else head :: take (Bigint.sub count 1Z) tail

let[@def] rec drop (count : Bigint.t)
    (values : ('a : immutable_data) t @ immutable total) : 'a t @ immutable total =
  match values with
  | [] -> []
  | _ :: tail ->
    if Bigint.compare count 0Z <= 0 then values
    else drop (Bigint.sub count 1Z) tail

let[@def] sub (values : ('a : immutable_data) t @ immutable total)
    (first : Bigint.t) (past : Bigint.t) : 'a t @ immutable total =
  take (Bigint.sub past first) (drop first values)

external iarray_get : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {i : int | 0 <= i && i < Iarray.length values} ->
  'a @ immutable total @@ total = "%array_safe_get"

let[@def] rec from_iarray
    (values : ('a : immutable_data) iarray @ immutable total)
    (count : int) (suffix : 'a t @ immutable total) : 'a t @ immutable total =
  if 0 < count && count <= Iarray.length values then
    let index = count - 1 in
    let bounded : {i : int | 0 <= i && i < Iarray.length values} =
      refine_ index in
    let head = iarray_get values bounded in
    from_iarray values index (head :: suffix)
  else suffix
[@@decreases let count : int = count in if 0 < count then count else 0]

let[@def] of_iarray (values : ('a : immutable_data) iarray @ immutable total)
    : 'a t @ immutable total =
  from_iarray values (Iarray.length values) []

let rec (append_length @ total) : ('a : immutable_data).
    (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
    {u : unit | length (append left right) ===
      Bigint.add (length left) (length right)} = fun left right ->
  let refine_ equation = append_def left right in
  let refine_ size = length_def left in
  let joined = append left right in
  let refine_ size = length_def joined in
  match left with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    let refine_ induction = append_length tail right in
    let u = () in refine_ u

let rec (append_split @ total) : ('a : immutable_data).
    (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
    {u : unit | take (length left) (append left right) === left
      && drop (length left) (append left right) === right} = fun left right ->
  let refine_ equation = append_def left right in
  let refine_ size = length_def left in
  let count = length left in
  let joined = append left right in
  let refine_ front = take_def count joined in
  let refine_ back = drop_def count joined in
  match left with
  | [] ->
    let zero = 0Z in
    let refine_ front = take_def zero right in
    let refine_ back = drop_def zero right in
    let u = () in refine_ u
  | _ :: tail ->
    let refine_ induction = append_split tail right in
    let u = () in refine_ u

let rec (set_length @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (index : Bigint.t) ->
    (value : 'a) @ immutable ->
    {u : unit | length (set values index value) === length values} =
    fun values index value ->
  let refine_ equation = set_def values index value in
  let refine_ size = length_def values in
  let updated = set values index value in
  let refine_ size = length_def updated in
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    let next = Bigint.sub index 1Z in
    let refine_ induction = set_length tail next value in
    let u = () in refine_ u

let[@def] swap (values : ('a : immutable_data) t @ immutable total)
    (first : Bigint.t) (second : Bigint.t) : 'a t @ immutable total =
  match at values first, at values second with
  | Some x, Some y -> set (set values first y) second x
  | _ -> values

let rec (cut @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (count : Bigint.t) ->
    {u : unit | if Bigint.compare count 0Z >= 0
      && Bigint.compare count (length values) <= 0 then
      length (take count values) === count
      && length (drop count values) === Bigint.sub (length values) count
      && append (take count values) (drop count values) === values
      else true} = fun values count ->
  let refine_ size = length_def values in
  let refine_ front = take_def count values in
  let refine_ back = drop_def count values in
  let prefix = take count values in
  let suffix = drop count values in
  let refine_ size = length_def prefix in
  let refine_ size = length_def suffix in
  let refine_ joined = append_def prefix suffix in
  if Bigint.compare count 0Z < 0
     || Bigint.compare count (length values) > 0 then
    let u = () in refine_ u
  else match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if Bigint.equal count 0Z then let u = () in refine_ u
    else
      let next = Bigint.sub count 1Z in
      let refine_ induction = cut tail next in
      let u = () in refine_ u

let rec (drop_add @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if Bigint.compare first 0Z >= 0
      && Bigint.compare second 0Z >= 0 then
      drop second (drop first values) === drop (Bigint.add first second) values
      else true} = fun values first second ->
  let sum = Bigint.add first second in
  let rest = drop first values in
  let refine_ outer = drop_def first values in
  let refine_ inner = drop_def second rest in
  let refine_ combined = drop_def sum values in
  if Bigint.compare first 0Z < 0 || Bigint.compare second 0Z < 0 then
    let u = () in refine_ u
  else match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if Bigint.equal first 0Z then let u = () in refine_ u
    else
      let next = Bigint.sub first 1Z in
      let refine_ induction = drop_add tail next second in
      let u = () in refine_ u

let (sub_length @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if Bigint.compare first 0Z >= 0 && Bigint.compare first past <= 0
      && Bigint.compare past (length values) <= 0 then
      length (sub values first past) === Bigint.sub past first else true} =
    fun values first past ->
  let width = Bigint.sub past first in
  let rest = drop first values in
  let refine_ front = cut values first in
  let refine_ back = cut rest width in
  let refine_ subset = sub_def values first past in
  let u = () in refine_ u

let rec (from_iarray_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (count : int) -> (suffix : 'a t) @ immutable ->
    {u : unit | if 0 <= count && count <= Iarray.length values then
      length (from_iarray values count suffix) ===
        Bigint.add (Bigint.of_int count) (length suffix) else true} = fun values count suffix ->
  let refine_ equation = from_iarray_def values count suffix in
  if 0 < count && count <= Iarray.length values then
    let index = count - 1 in
    let bounded : {i : int | 0 <= i && i < Iarray.length values} = refine_ index in
    let head = iarray_get values bounded in
    let suffix1 = head :: suffix in
    let refine_ size = length_def suffix1 in
    let refine_ induction = from_iarray_length values index suffix1 in
    let u = () in refine_ u
  else let u = () in refine_ u
[@@decreases let count : int = count in if count > 0 then count else 0]

let (of_iarray_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {u : unit | length (of_iarray values) === Bigint.of_int (Iarray.length values)} = fun values ->
  let count = Iarray.length values in
  let nil = [] in
  let refine_ equation = of_iarray_def values in
  let refine_ size = length_def nil in
  let refine_ proof = from_iarray_length values count nil in
  let u = () in refine_ u

let[@def] iarray_at (values : ('a : immutable_data) iarray @ immutable total)
    (index : int) : 'a option @ immutable total =
  if 0 <= index && index < Iarray.length values then
    let bounded : {i : int | 0 <= i && i < Iarray.length values} = refine_ index in
    Some (iarray_get values bounded)
  else None

let rec (from_iarray_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (count : int) -> (suffix : 'a t) @ immutable ->
    (query : int) ->
    {u : unit | if 0 <= count && count <= Iarray.length values then
      at (from_iarray values count suffix) (Bigint.of_int query) ===
        (if 0 <= query && query < count then
          iarray_at values query
        else at suffix (Bigint.sub (Bigint.of_int query) (Bigint.of_int count)))
      else true} = fun values count suffix query ->
  let refine_ equation = from_iarray_def values count suffix in
  let refine_ query_value = iarray_at_def values query in
  if 0 < count && count <= Iarray.length values then
    let index = count - 1 in
    let bounded : {i : int | 0 <= i && i < Iarray.length values} = refine_ index in
    let head = iarray_get values bounded in
    let suffix1 = head :: suffix in
    let shifted = Bigint.sub (Bigint.of_int query) (Bigint.of_int index) in
    let refine_ tail_at = at_def suffix1 shifted in
    let refine_ induction = from_iarray_at values index suffix1 query in
    let u = () in refine_ u
  else let u = () in refine_ u
[@@decreases let count : int = count in if count > 0 then count else 0]

let (of_iarray_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | let refine_ i = index in
      at (of_iarray values) (Bigint.of_int i) === Some (iarray_get values index)} =
    fun values index ->
  let refine_ query = index in
  let count = Iarray.length values in
  let nil = [] in
  let refine_ equation = of_iarray_def values in
  let refine_ proof = from_iarray_at values count nil query in
  let refine_ query_value = iarray_at_def values query in
  let u = () in refine_ u
