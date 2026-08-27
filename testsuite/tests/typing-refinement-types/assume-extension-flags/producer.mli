external equal : int -> int -> bool @@ total = "%equal"
type record = #{ value : int }
type checked =
  { v : int |
    let _small = 1.0s in
    let _array = [: v :] in
    let _record = #{ value = v } in
    equal v 0 }

type sequenced = { v : int | (v; true) [@warning "-10"] }

external ignore_labelled : (label:int -> int) -> unit @@ total = "%ignore"
type labels = { v : int | ignore_labelled (fun x -> x); true }
