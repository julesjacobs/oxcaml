val positive : int -> bool @@ total

type nat = { x : int | positive x }

val one : nat

type unpacking = { x : int | let refine_ one = one in true }

type nested = { x : nat list | true }

type total_function = { f : unit -> unit | true }

type local_polymorphism =
  { x : int | let _unused = fun _y -> true in true }
