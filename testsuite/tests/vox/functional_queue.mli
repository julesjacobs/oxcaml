type t : immutable_data

val contents : t -> int list @@ total

val append : int list -> int list -> int list @@ total
val append_def : (xs : int list) -> (ys : int list) ->
  {u : unit |
    append xs ys ===
      (match xs with [] -> ys | h :: t -> h :: append t ys)}
  @@ total

val empty : {q : t | contents q === []} @@ total
val enqueue : (q : t) -> (value : int) ->
  {r : t | contents r === append (contents q) [value]} @@ total
val dequeue : (q : {q : t | (contents q === []) === false}) ->
  {r : int * t |
    let refine_ original = q in
    match r with head, tail -> contents original === head :: contents tail}
  @@ total
