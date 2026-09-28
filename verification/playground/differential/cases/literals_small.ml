(* Literals that fit in 32 bits: checked in the browser as natively. *)
let a = 1073741823
let b = 1073741824
let c = 2147483647
let d = -2147483648
let e = 0x7fff_ffff
let f = -0x8000_0000
let n = 2147483647n
let (under @ total) (x : {x : int | 0 <= x && x <= 1073741824}) :
    {r : int | r <= 2147483647} = x + x - 1
