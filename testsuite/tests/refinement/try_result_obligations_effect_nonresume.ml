open Effect

type _ eff += Pick : int eff

let handled () : int{ _ > 0 } =
  try perform Pick with
  | effect Pick, _continuation -> 1
