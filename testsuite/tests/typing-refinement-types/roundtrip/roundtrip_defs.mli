val positive : int -> bool @@ total

type nat = { x : int | positive x }

val one : nat

type unpacking = { x : int | let refine_ one = one in true }

type nested = { x : nat list | true }

type total_function = { f : unit -> unit | true }

type local_polymorphism =
  { x : int | let _unused = fun _y -> true in true }

type point = { x : int; y : int }
type pair = Pair of int * int

type pattern_predicates =
  { r : int |
    match { x = r; y = r } with
    | { x; y = 0 } -> x = r
    | { x = 0; y } -> y = r
    | _ -> true }

type open_pattern_predicate =
  { r : int | match { x = r; y = r } with { x; _ } -> x = r }

type or_predicate =
  { r : int |
    match Pair (r, r) with
    | Pair (0, x) | Pair (x, 0) -> x = r
    | Pair (_, _) -> true }
