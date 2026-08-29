let (predecessor @ total) = function
  | Definition.Z -> Definition.Z
  | Definition.S n -> n

let (call @ total) n = Definition.predecessor n
