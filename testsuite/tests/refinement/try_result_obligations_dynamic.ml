open Effect

type _ eff += Known : int eff

let dynamic operation : int{ _ > 0 } =
  try perform operation with
  | effect Known, _continuation -> 1
