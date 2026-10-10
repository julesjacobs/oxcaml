type number = Bigint.t
type positive = {n : number | n > 0Z}
let[@def] next (x : number) = Bigint.add x 1Z
let huge () : positive =
  let n = 123456789012345678901234567890Z in refine_ n
