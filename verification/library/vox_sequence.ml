open! Stdlib

type ('a : immutable_data) t = 'a list

external length : ('a : immutable_data).
  'a t @ immutable -> Bigint.t @@ total = "caml_vox_sequence_length"

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
    (values : ('a : immutable_data) t @ immutable total) : 'a t @ immutable
      total =
  match values with
  | [] -> []
  | head :: tail ->
    if Bigint.compare count 0Z <= 0 then []
    else head :: take (Bigint.sub count 1Z) tail

let[@def] rec drop (count : Bigint.t)
    (values : ('a : immutable_data) t @ immutable total) : 'a t @ immutable
      total =
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
  append_def left right;
  length_def left;
  let joined = append left right in
  length_def joined;
  match left with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    append_length tail right;
    let u = () in refine_ u

let rec (append_split @ total) : ('a : immutable_data).
    (left : 'a t) @ immutable -> (right : 'a t) @ immutable ->
    {u : unit | take (length left) (append left right) === left
      && drop (length left) (append left right) === right} = fun left right ->
  append_def left right;
  length_def left;
  let count = length left in
  let joined = append left right in
  take_def count joined;
  drop_def count joined;
  match left with
  | [] ->
    let zero = 0Z in
    take_def zero right;
    drop_def zero right;
    let u = () in refine_ u
  | _ :: tail ->
    append_split tail right;
    let u = () in refine_ u

let rec (set_length @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (index : Bigint.t) ->
    (value : 'a) @ immutable ->
    {u : unit | length (set values index value) === length values} =
    fun values index value ->
  set_def values index value;
  length_def values;
  let updated = set values index value in
  length_def updated;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    let next = Bigint.sub index 1Z in
    set_length tail next value;
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
  length_def values;
  take_def count values;
  drop_def count values;
  let prefix = take count values in
  let suffix = drop count values in
  length_def prefix;
  length_def suffix;
  append_def prefix suffix;
  if Bigint.compare count 0Z < 0
     || Bigint.compare count (length values) > 0 then
    let u = () in refine_ u
  else match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if Bigint.equal count 0Z then let u = () in refine_ u
    else
      let next = Bigint.sub count 1Z in
      cut tail next;
      let u = () in refine_ u

let rec (drop_add @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) -> (second : Bigint.t) ->
    {u : unit | if Bigint.compare first 0Z >= 0
      && Bigint.compare second 0Z >= 0 then
      drop second (drop first values) === drop (Bigint.add first second) values
      else true} = fun values first second ->
  let sum = Bigint.add first second in
  let rest = drop first values in
  drop_def first values;
  drop_def second rest;
  drop_def sum values;
  if Bigint.compare first 0Z < 0 || Bigint.compare second 0Z < 0 then
    let u = () in refine_ u
  else match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    if Bigint.equal first 0Z then let u = () in refine_ u
    else
      let next = Bigint.sub first 1Z in
      drop_add tail next second;
      let u = () in refine_ u

let (sub_length @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if Bigint.compare first 0Z >= 0 && Bigint.compare first past <=
      0
      && Bigint.compare past (length values) <= 0 then
      length (sub values first past) === Bigint.sub past first else true} =
    fun values first past ->
  let width = Bigint.sub past first in
  let rest = drop first values in
  cut values first;
  cut rest width;
  sub_def values first past;
  let u = () in refine_ u

let rec (from_iarray_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (count : int) -> (suffix : 'a t) @
      immutable ->
    {u : unit | if 0 <= count && count <= Iarray.length values then
      length (from_iarray values count suffix) ===
        Bigint.add (Bigint.of_int count) (length suffix) else true} = fun values
          count suffix ->
  from_iarray_def values count suffix;
  if 0 < count && count <= Iarray.length values then
    let index = count - 1 in
    let bounded : {i : int | 0 <= i && i < Iarray.length values} = refine_ index
      in
    let head = iarray_get values bounded in
    let suffix1 = head :: suffix in
    length_def suffix1;
    from_iarray_length values index suffix1;
    let u = () in refine_ u
  else let u = () in refine_ u
[@@decreases let count : int = count in if count > 0 then count else 0]

let (of_iarray_length @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {u : unit | length (of_iarray values) === Bigint.of_int (Iarray.length
      values)} = fun values ->
  let count = Iarray.length values in
  let nil = [] in
  of_iarray_def values;
  length_def nil;
  from_iarray_length values count nil;
  let u = () in refine_ u

let[@def] iarray_at (values : ('a : immutable_data) iarray @ immutable total)
    (index : int) : 'a option @ immutable total =
  if 0 <= index && index < Iarray.length values then
    let bounded : {i : int | 0 <= i && i < Iarray.length values} = refine_ index
      in
    Some (iarray_get values bounded)
  else None

let rec (from_iarray_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (count : int) -> (suffix : 'a t) @
      immutable ->
    (query : int) ->
    {u : unit | if 0 <= count && count <= Iarray.length values then
      at (from_iarray values count suffix) (Bigint.of_int query) ===
        (if 0 <= query && query < count then
          iarray_at values query
        else at suffix (Bigint.sub (Bigint.of_int query) (Bigint.of_int count)))
      else true} = fun values count suffix query ->
  from_iarray_def values count suffix;
  iarray_at_def values query;
  if 0 < count && count <= Iarray.length values then
    let index = count - 1 in
    let bounded : {i : int | 0 <= i && i < Iarray.length values} = refine_ index
      in
    let head = iarray_get values bounded in
    let suffix1 = head :: suffix in
    let shifted = Bigint.sub (Bigint.of_int query) (Bigint.of_int index) in
    at_def suffix1 shifted;
    from_iarray_at values index suffix1 query;
    let u = () in refine_ u
  else let u = () in refine_ u
[@@decreases let count : int = count in if count > 0 then count else 0]

let (of_iarray_at @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | let refine_ i = index in
      at (of_iarray values) (Bigint.of_int i) === Some (iarray_get values
        index)} =
    fun values index ->
  let refine_ query = index in
  let count = Iarray.length values in
  let nil = [] in
  of_iarray_def values;
  from_iarray_at values count nil query;
  iarray_at_def values query;
  let u = () in refine_ u

module Iarray = struct
  let (length @ total) : ('a : immutable_data).
      (values : 'a iarray) @ immutable ->
      {n : int | n = Iarray.length values
        && Bigint.of_int n === length (of_iarray values)} = fun values ->
    let n = Iarray.length values in
    ghost_ (of_iarray_length values);
    refine_ n

  let (get @ total) : ('a : immutable_data).
      (values : 'a iarray) @ immutable ->
      (index : {i : int | 0 <= i && i < Iarray.length values}) ->
      {value : 'a | let refine_ i = index in
        at (of_iarray values) (Bigint.of_int i) === Some value
        && value === iarray_get values index}
      @ immutable total = fun values index ->
    let value = iarray_get values index in
    ghost_ (of_iarray_at values index);
    refine_ value
end

let rec (take_all @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable ->
    {u : unit | take (length values) values === values} = fun values ->
  let size = length values in
  length_def values;
  take_def size values;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    take_all tail;
    let u = () in refine_ u


let (sub_prefix @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (past : Bigint.t) ->
    {u : unit | sub values 0Z past === take past values} = fun values past ->
  let zero = 0Z in
  sub_def values zero past;
  drop_def zero values;
  let u = () in refine_ u


let (sub_suffix @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) ->
    {u : unit | if Bigint.compare 0Z first <= 0 && Bigint.compare first (length
      values) <= 0 then
      sub values first (length values) === drop first values else true} = fun
        values first ->
  let size = length values in
  let rest = drop first values in
  sub_def values first size;
  cut values first;
  take_all rest;
  let u = () in refine_ u


let (decompose3 @ total) : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if Bigint.compare 0Z first <= 0 && Bigint.compare first past <=
      0 && Bigint.compare past (length values) <= 0 then
      values === append (take first values) (append (sub values first past)
        (drop past values))
      else true} = fun values first past ->
  let rest = drop first values in
  let width = Bigint.sub past first in
  cut values first;
  cut rest width;
  sub_def values first past;
  drop_add values first width;
  let u = () in refine_ u
