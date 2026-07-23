open Effect

type _ eff += A : int eff

let false_postcondition () : int{ _ > 0 } =
  try perform A with
  | effect A, _continuation -> 0
