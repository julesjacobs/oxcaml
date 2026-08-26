external equal : int -> int -> bool @@ total = "%equal"

type zero = { v : int | v = 0 }
val absent : ?value:'a -> int -> bool @@ total
type optional = { v : int | absent v }
type suppressed_warning =
  { v : int | (match v with _ -> true | _ -> false) [@warning "-11"] }
val dependent_identity : (x : int) -> { z : int | equal z x } @@ total
val n : int
type dependent =
  { v : int | let refine_ z = dependent_identity n in equal z v }
type direct_dependent =
  { v : int | let refine_ z = dependent_identity v in equal z v }
type local_dependent =
  { v : int |
    let y = v in
    let r = dependent_identity y in
    let refine_ z = r in equal z v }
exception E of int
type exception_predicate =
  { v : int | match E v with E n -> equal n v | _ -> false }
type t = A of int | B
type record = { value : int; other : int }
type structured =
  { v : int |
    let record = { value = v; other = 0 } in
    let record = { record with other = v } in
    let identity x = x in
    match A (identity record.value) with
    | A n -> if equal n v then identity true else false
    | B -> false }

module type Input = sig
  val check : int -> bool @@ total
end

module F (X : Input) : sig
  type checked = { v : int | X.check v }
end
