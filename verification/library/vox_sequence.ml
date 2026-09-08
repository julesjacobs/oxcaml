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

let rec (append_nil @ total) : (xs : ('a : immutable_data) list) @ immutable ->
    {u : unit | append xs [] === xs} = fun xs ->
  let nil : 'a list = [] in
  append_def xs nil;
  let u = () in
  match xs with
  | [] -> refine_ u
  | _ :: tail ->
    append_nil tail;
    refine_ u

let rec (append_associative @ total) :
    (xs : ('a : immutable_data) list) @ immutable ->
    (ys : 'a list) @ immutable -> (zs : 'a list) @ immutable ->
    {u : unit | append (append xs ys) zs === append xs (append ys zs)} =
  fun xs ys zs ->
  let xy = append xs ys in
  let yz = append ys zs in
  append_def xs ys;
  append_def xy zs;
  append_def xs yz;
  let u = () in
  match xs with
  | [] -> refine_ u
  | _ :: tail ->
    append_associative tail ys zs;
    refine_ u

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

let (iarray_at_get @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | let refine_ i = index in
      iarray_at values i === Some (iarray_get values index)} = fun values index
        ->
  let refine_ i = index in
  iarray_at_def values i;
  let u = () in refine_ u

let (from_iarray_unfold @ total) : ('a : immutable_data).
    (values : 'a iarray) @ immutable -> (count : int) ->
    (suffix : 'a t) @ immutable ->
    {u : unit | from_iarray values count suffix ===
      (if 0 < count && count <= Iarray.length values then
        match iarray_at values (count - 1) with
        | None -> suffix
        | Some head -> from_iarray values (count - 1) (head :: suffix)
      else suffix)} = fun values count suffix ->
  let index = count - 1 in
  from_iarray_def values count suffix;
  iarray_at_def values index;
  let u = () in refine_ u

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

module type Predicate = sig
  type element : immutable_data
  val test : element @ immutable total -> bool @@ total
end

module For_all (P : Predicate) = struct
  let[@def] rec holds (values : P.element list @ immutable total) =
    match values with [] -> true | head :: tail -> P.test head && holds tail

  let rec (get @ total) : (values : P.element list) @ immutable ->
      (index : Bigint.t) ->
      {u : unit | if holds values then
        match at values index with None -> true | Some value -> P.test value
        else true} = fun values index ->
    holds_def values;
    at_def values index;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      let previous = Bigint.sub index 1Z in
      let refine_ induction = get tail previous in
      let u = () in refine_ u

  let rec (intro @ total) : (values : P.element list) @ immutable ->
      ((index : Bigint.t) -> {u : unit | if 0Z <= index then
        match at values index with None -> true | Some value -> P.test value
        else true}) @ total ->
      {u : unit | holds values} = fun values proof ->
    holds_def values;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      let zero = 0Z in
      let refine_ first = proof zero in
      at_def values zero;
      intro tail (fun index ->
        let next = Bigint.add index 1Z in
        let refine_ known = proof next in
        at_def values next;
        let u = () in refine_ u);
      let u = () in refine_ u

  let rec (append_holds @ total) : (left : P.element list) @ immutable ->
      (right : P.element list) @ immutable ->
      {u : unit | holds (append left right) = (holds left && holds right)} =
      fun left right ->
    let joined = append left right in
    append_def left right;
    holds_def left;
    holds_def joined;
    match left with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      append_holds tail right;
      let u = () in refine_ u

  let[@def] rec filter (values : P.element list @ immutable total) :
      P.element list @ immutable total =
    match values with
    | [] -> []
    | head :: tail ->
      if P.test head then head :: filter tail else filter tail

  let rec (filter_holds @ total) : (values : P.element list) @ immutable ->
      {u : unit | holds (filter values)} = fun values ->
    let result = filter values in
    filter_def values;
    holds_def result;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      filter_holds tail;
      let u = () in refine_ u
  let rec (set_holds @ total) : (values : P.element list) @ immutable ->
      (index : Bigint.t) -> (value : P.element) @ immutable ->
      {u : unit | if holds values && P.test value then
        holds (set values index value) else true} = fun values index value ->
    let result = set values index value in
    set_def values index value;
    holds_def values;
    holds_def result;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      let previous = Bigint.sub index 1Z in
      set_holds tail previous value;
      let u = () in refine_ u

  let (split_holds @ total) : (values : P.element list) @ immutable ->
      (index : Bigint.t) ->
      {u : unit | if 0Z <= index && index <= length values then
        holds values = (holds (take index values) && holds (drop index values))
        else true} = fun values index ->
    let left = take index values in
    let right = drop index values in
    cut values index;
    append_holds left right;
    let u = () in refine_ u

  let rec (filter_length @ total) : (values : P.element list) @ immutable ->
      {u : unit | 0Z <= length (filter values)
        && length (filter values) <= length values} = fun values ->
    let result = filter values in
    filter_def values;
    length_def values;
    length_def result;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      filter_length tail;
      let u = () in refine_ u

  let rec (filter_append @ total) : (left : P.element list) @ immutable ->
      (right : P.element list) @ immutable ->
      {u : unit | filter (append left right) ===
        append (filter left) (filter right)} = fun left right ->
    let joined = append left right in
    let first = filter left in
    let second = filter right in
    append_def left right;
    filter_def left;
    filter_def joined;
    append_def first second;
    match left with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      filter_append tail right;
      let u = () in refine_ u

  let rec (filter_identity @ total) : (values : P.element list) @ immutable ->
      {u : unit | if holds values then filter values === values else true} =
      fun values ->
    filter_def values;
    holds_def values;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      filter_identity tail;
      let u = () in refine_ u

  let (filter_idempotent @ total) : (values : P.element list) @ immutable ->
      {u : unit | filter (filter values) === filter values} = fun values ->
    let result = filter values in
    filter_holds values;
    filter_identity result;
    let u = () in refine_ u

end

module type Mapping = sig
  type input : immutable_data
  type output : immutable_data
  val apply : input @ immutable total -> output @ immutable total @@ total
end

module Map (F : Mapping) = struct
  let[@def] rec map (values : F.input list @ immutable total) :
      F.output list @ immutable total =
    match values with [] -> [] | head :: tail -> F.apply head :: map tail

  let rec (map_length @ total) : (values : F.input list) @ immutable ->
      {u : unit | length (map values) === length values} = fun values ->
    let result = map values in
    map_def values;
    length_def values;
    length_def result;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      map_length tail;
      let u = () in refine_ u

  let rec (map_at @ total) : (values : F.input list) @ immutable ->
      (index : Bigint.t) ->
      {u : unit | at (map values) index ===
        (match at values index with None -> None
          | Some value -> Some (F.apply value))} = fun values index ->
    let result = map values in
    map_def values;
    at_def values index;
    at_def result index;
    match values with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      let previous = Bigint.sub index 1Z in
      map_at tail previous;
      let u = () in refine_ u
  let rec (map_append @ total) : (left : F.input list) @ immutable ->
      (right : F.input list) @ immutable ->
      {u : unit | map (append left right) ===
        append (map left) (map right)} = fun left right ->
    let joined = append left right in
    let first = map left in
    let second = map right in
    append_def left right;
    map_def joined;
    map_def left;
    append_def first second;
    match left with
    | [] -> let u = () in refine_ u
    | _ :: tail ->
      map_append tail right;
      let u = () in refine_ u

end

module type Folding = sig
  type element : immutable_data
  type accumulator : immutable_data
  val step : element @ immutable total -> accumulator @ immutable total ->
    accumulator @ immutable total @@ total
end

module Fold (F : Folding) = struct
  let[@def] rec fold (values : F.element list @ immutable total)
      (initial : F.accumulator @ immutable total) :
      F.accumulator @ immutable total =
    match values with
    | [] -> initial
    | head :: tail -> F.step head (fold tail initial)

  let rec (fold_append @ total) : (left : F.element list) @ immutable ->
      (right : F.element list) @ immutable ->
      (initial : F.accumulator) @ immutable ->
      {u : unit | fold (append left right) initial ===
        fold left (fold right initial)} = fun left right initial ->
    let joined = append left right in
    let suffix = fold right initial in
    append_def left right;
    fold_def joined initial;
    fold_def left suffix;
    match left with
    | [] -> let u = () in refine_ u
    | head :: tail ->
      fold_append tail right initial;
      let u = () in refine_ u
end

let rec (extensional @ total) : ('a : immutable_data).
    (left : 'a list) @ immutable -> (right : 'a list) @ immutable ->
    ((index : Bigint.t) -> {u : unit | length left === length right
      && (if 0Z <= index then at left index === at right index else true)})
      @ total ->
    {u : unit | left === right} = fun left right proof ->
  let zero = 0Z in
  let refine_ known = proof zero in
  length_def left;
  length_def right;
  at_def left zero;
  at_def right zero;
  match left with
  | [] ->
    (match right with
     | [] -> let u = () in refine_ u
     | _ :: _ -> let u = () in refine_ u)
  | x :: xs ->
    match right with
    | [] -> let u = () in refine_ u
    | y :: ys ->
      extensional xs ys (fun index ->
        let next = Bigint.add index 1Z in
        let refine_ known = proof next in
        at_def left next;
        at_def right next;
        let u = () in refine_ u);
      let u = () in refine_ u

let rec (at_outside @ total) : ('a : immutable_data).
    (values : 'a list) @ immutable -> (index : Bigint.t) ->
    {u : unit | if index < 0Z || length values <= index then
      at values index === None else true} = fun values index ->
  length_def values;
  at_def values index;
  match values with
  | [] -> let u = () in refine_ u
  | head :: tail ->
    let previous = Bigint.sub index 1Z in
    length_def tail;
    at_outside tail previous;
    let u = () in refine_ u
