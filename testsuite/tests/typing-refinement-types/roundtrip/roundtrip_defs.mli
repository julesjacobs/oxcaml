val positive : int -> bool @@ total

type nat = { x : int | positive x }

type nested = { x : nat list | true }

type total_function = { f : unit -> unit | true }
