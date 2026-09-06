type ('a : immutable_data) t : immutable_data

val contents : ('a : immutable_data).
  'a t @ immutable total -> 'a list @ immutable total @@ total

val append : ('a : immutable_data).
  'a list @ immutable total -> 'a list @ immutable total ->
  'a list @ immutable total @@ total
val append_def : ('a : immutable_data).
  (xs : 'a list) @ immutable -> (ys : 'a list) @ immutable ->
  {u : unit |
    append xs ys ===
      (match xs with [] -> ys | h :: t -> h :: append t ys)}
  @@ total

val empty : ('a : immutable_data).
  {q : 'a t | contents q === []} @@ total immutable
val enqueue : ('a : immutable_data).
  (q : 'a t) @ immutable -> (value : 'a) @ immutable ->
  {r : 'a t | contents r === append (contents q) [value]}
  @ immutable total @@ total
val dequeue : ('a : immutable_data).
  (q : {q : 'a t | (contents q === []) === false}) @ immutable ->
  {r : 'a * 'a t |
    let refine_ original = q in
    match r with head, tail -> contents original === head :: contents tail}
  @ immutable total @@ total
