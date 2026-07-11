(* A model is a term-keyed value assignment. Construction is added by the first real
   consumer (the M2 EUF adapter / M4 combinator), which also pins the [Uninterp] witness
   encoding (open q3); this module is not hash-frozen until then (ADR-0005 Tranche B). *)

type value =
  | Int of int
  | Bool of bool
  | Uninterp of int

type t = value Term.Map.t

let value m term = Term.Map.find_opt term m
