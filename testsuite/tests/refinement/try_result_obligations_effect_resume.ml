open Effect
open Effect.Deep

type _ eff += Pick : int eff

let handled () : int{ _ > 0 } =
  try perform Pick with
  | effect Pick, continuation -> continue continuation 1
