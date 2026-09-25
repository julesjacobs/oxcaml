type t = Nil | Cons of int * t [@@inductive]
val append : t -> t -> t @@ total
val append_def : (xs : t) -> (ys : t) ->
  {u : unit | append xs ys === (match xs with
    | Nil -> ys | Cons (head, tail) -> Cons (head, append tail ys))} @@ total
val length : t -> int @@ total
val length_def : (xs : t) ->
  {u : unit | length xs === (match xs with
    | Nil -> 0 | Cons (_, tail) -> 1 + length tail)} @@ total
val sum : t -> int @@ total
val sum_def : (xs : t) ->
  {u : unit | sum xs === (match xs with
    | Nil -> 0 | Cons (head, tail) -> head + sum tail)} @@ total
module Laws : sig
  val append_nil_left : (ys : t) ->
    {u : unit | append Nil ys === ys} @@ total
  val append_nil_right : (xs : t) ->
    {u : unit | append xs Nil === xs} @ immutable contended @@ total
  val append_associative : (xs : t) -> (ys : t) -> (zs : t) ->
    {u : unit | append (append xs ys) zs === append xs (append ys zs)}
    @ immutable contended @@ total
  val length_append : (xs : t) -> (ys : t) ->
    {u : unit | length (append xs ys) === length xs + length ys}
    @ immutable contended @@ total
  val sum_append : (xs : t) -> (ys : t) ->
    {u : unit | sum (append xs ys) === sum xs + sum ys}
    @ immutable contended @@ total
end
