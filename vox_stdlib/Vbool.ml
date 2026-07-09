(* See Vbool.mli. The algebra is native (each body IS the boolish operator);
   bnot is written as a bool-scrutinee match to exercise Variant V's wart-(b)
   positive-arm fact (the [true] arm refines to [b = true]). to_int cases in the
   model on the Bool value; its body [if b then 1 else 0] matches vb_toint. *)
let bnot (b : bool) : bool{ _ = not b } =
  match b with
  | true -> false
  | false -> true

let band (a : bool) (b : bool) : bool{ _ = (a && b) } = a && b
let bor (a : bool) (b : bool) : bool{ _ = (a || b) } = a || b
let bxor (a : bool) (b : bool) : bool{ _ = (a <> b) } = a <> b
let bequal (a : bool) (b : bool) : bool{ _ = (a = b) } = a = b

let to_int (b : bool) : int{ _ = vb_toint b } = if b then 1 else 0
let of_int (n : int) : bool{ _ = (n <> 0) } = n <> 0
