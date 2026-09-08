type ('a : immutable_data) t = 'a list

external length : ('a : immutable_data).
  'a t @ immutable -> Bigint.t @@ total = "caml_vox_sequence_length"

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

external iarray_get : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {i : int | 0 <= i && i < Iarray.length values} ->
  'a @ immutable total @@ total = "%array_safe_get"
val from_iarray : ('a : immutable_data).
  'a iarray @ immutable total -> int -> 'a t @ immutable total ->
  'a t @ immutable total @@ total
val iarray_at : ('a : immutable_data).
  'a iarray @ immutable total -> int -> 'a option @ immutable total @@ total
val iarray_at_get : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {u : unit | let refine_ i = index in
      iarray_at values i === Some (iarray_get values index)} @@ total
val from_iarray_unfold : ('a : immutable_data).
  (values : 'a iarray) @ immutable -> (count : int) ->
  (suffix : 'a t) @ immutable ->
  {u : unit | from_iarray values count suffix ===
    (if 0 < count && count <= Iarray.length values then
      match iarray_at values (count - 1) with
      | None -> suffix
      | Some head -> from_iarray values (count - 1) (head :: suffix)
    else suffix)} @@ total

val of_iarray : ('a : immutable_data).
  'a iarray @ immutable total -> 'a t @ immutable total @@ total

val of_iarray_def : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {u : unit | of_iarray values ===
    from_iarray values (Iarray.length values) []} @@ total

val append_nil : (xs : ('a : immutable_data) list) @ immutable ->
    {u : unit | append xs [] === xs} @@ total

val append_associative : (xs : ('a : immutable_data) list) @ immutable ->
    (ys : 'a list) @ immutable -> (zs : 'a list) @ immutable ->
    {u : unit | append (append xs ys) zs === append xs (append ys zs)} @@ total

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

val of_iarray_length : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  {u : unit | length (of_iarray values) === Bigint.of_int (Iarray.length
    values)} @@ total
val of_iarray_at : ('a : immutable_data).
  (values : 'a iarray) @ immutable ->
  (index : {i : int | 0 <= i && i < Iarray.length values}) ->
  {u : unit | let refine_ i = index in
    at (of_iarray values) (Bigint.of_int i) === Some (iarray_get values index)}
      @@ total

module Iarray : sig
  val length : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    {n : int | n = Iarray.length values
        && Bigint.of_int n === length (of_iarray values)} @@ total
  val get : ('a : immutable_data).
    (values : 'a iarray) @ immutable ->
    (index : {i : int | 0 <= i && i < Iarray.length values}) ->
    {value : 'a | let refine_ i = index in
      at (of_iarray values) (Bigint.of_int i) === Some value
        && value === iarray_get values index}
    @ immutable total @@ total
end

val take_all : ('a : immutable_data).
    (values : 'a t) @ immutable ->
    {u : unit | take (length values) values === values} @@ total

val sub_prefix : ('a : immutable_data).
    (values : 'a t) @ immutable -> (past : Bigint.t) ->
    {u : unit | sub values 0Z past === take past values} @@ total

val sub_suffix : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) ->
    {u : unit | if Bigint.compare 0Z first <= 0 && Bigint.compare first (length
      values) <= 0 then
      sub values first (length values) === drop first values else true} @@ total

val decompose3 : ('a : immutable_data).
    (values : 'a t) @ immutable -> (first : Bigint.t) -> (past : Bigint.t) ->
    {u : unit | if Bigint.compare 0Z first <= 0 && Bigint.compare first past <=
      0 && Bigint.compare past (length values) <= 0 then
      values === append (take first values) (append (sub values first past)
        (drop past values))
      else true} @@ total

module type Predicate = sig
  type element : immutable_data
  val test : element @ immutable total -> bool @@ total
end

module For_all (P : Predicate) : sig
  val holds : P.element list @ immutable total -> bool @@ total
  val holds_def : (values : P.element list) @ immutable ->
    {u : unit | holds values === (match values with [] -> true
      | head :: tail -> P.test head && holds tail)} @@ total
  val filter : P.element list @ immutable total ->
    P.element list @ immutable total @@ total
  val filter_def : (values : P.element list) @ immutable ->
    {u : unit | filter values === (match values with [] -> []
      | head :: tail -> if P.test head then head :: filter tail
        else filter tail)} @@ total
  val get : (values : P.element list) @ immutable ->
      (index : Bigint.t) ->
      {u : unit | if holds values then
        match at values index with None -> true | Some value -> P.test value
        else true} @@ total

  val intro : (values : P.element list) @ immutable ->
      ((index : Bigint.t) -> {u : unit | if 0Z <= index then
        match at values index with None -> true | Some value -> P.test value
        else true}) @ total ->
      {u : unit | holds values} @@ total

  val append_holds : (left : P.element list) @ immutable ->
      (right : P.element list) @ immutable ->
      {u : unit | holds (append left right) = (holds left && holds right)} @@
        total

  val filter_holds : (values : P.element list) @ immutable ->
      {u : unit | holds (filter values)} @@ total

  val set_holds : (values : P.element list) @ immutable ->
      (index : Bigint.t) -> (value : P.element) @ immutable ->
      {u : unit | if holds values && P.test value then
        holds (set values index value) else true} @@ total

  val split_holds : (values : P.element list) @ immutable ->
      (index : Bigint.t) ->
      {u : unit | if 0Z <= index && index <= length values then
        holds values = (holds (take index values) && holds (drop index values))
        else true} @@ total

  val filter_length : (values : P.element list) @ immutable ->
      {u : unit | 0Z <= length (filter values)
        && length (filter values) <= length values} @@ total

  val filter_append : (left : P.element list) @ immutable ->
      (right : P.element list) @ immutable ->
      {u : unit | filter (append left right) ===
        append (filter left) (filter right)} @@ total

  val filter_identity : (values : P.element list) @ immutable ->
      {u : unit | if holds values then filter values === values else true} @@
        total

  val filter_idempotent : (values : P.element list) @ immutable ->
      {u : unit | filter (filter values) === filter values} @@ total
end

module type Mapping = sig
  type input : immutable_data
  type output : immutable_data
  val apply : input @ immutable total -> output @ immutable total @@ total
end

module Map (F : Mapping) : sig
  val map : F.input list @ immutable total ->
    F.output list @ immutable total @@ total
  val map_def : (values : F.input list) @ immutable ->
    {u : unit | map values === (match values with [] -> []
      | head :: tail -> F.apply head :: map tail)} @@ total
  val map_length : (values : F.input list) @ immutable ->
      {u : unit | length (map values) === length values} @@ total

  val map_at : (values : F.input list) @ immutable ->
      (index : Bigint.t) ->
      {u : unit | at (map values) index ===
        (match at values index with None -> None
          | Some value -> Some (F.apply value))} @@ total
  val map_append : (left : F.input list) @ immutable ->
    (right : F.input list) @ immutable ->
    {u : unit | map (append left right) ===
      append (map left) (map right)} @@ total

end

module type Folding = sig
  type element : immutable_data
  type accumulator : immutable_data
  val step : element @ immutable total -> accumulator @ immutable total ->
    accumulator @ immutable total @@ total
end

module Fold (F : Folding) : sig
  val fold : F.element list @ immutable total ->
    F.accumulator @ immutable total -> F.accumulator @ immutable total @@ total
  val fold_def : (values : F.element list) @ immutable ->
    (initial : F.accumulator) @ immutable ->
    {u : unit | fold values initial === (match values with [] -> initial
      | head :: tail -> F.step head (fold tail initial))} @@ total
  val fold_append : (left : F.element list) @ immutable ->
      (right : F.element list) @ immutable ->
      (initial : F.accumulator) @ immutable ->
      {u : unit | fold (append left right) initial ===
        fold left (fold right initial)} @@ total
end

val extensional : ('a : immutable_data).
    (left : 'a list) @ immutable -> (right : 'a list) @ immutable ->
    ((index : Bigint.t) -> {u : unit | length left === length right
      && (if 0Z <= index then at left index === at right index else true)})
      @ total ->
    {u : unit | left === right} @@ total

val at_outside : ('a : immutable_data).
  (values : 'a list) @ immutable -> (index : Bigint.t) ->
  {u : unit | if index < 0Z || length values <= index then
    at values index === None else true} @@ total
