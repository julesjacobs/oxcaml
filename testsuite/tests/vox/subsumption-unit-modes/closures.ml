let pure () = fun (x : int) -> x
let counter () = let c = ref 0 in fun (x : int) -> c := !c + x; !c
