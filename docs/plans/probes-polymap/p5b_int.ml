[%%vox.lean {lean|
@[grind, expose] def ile (x y : Int) : Prop := x <= y
|lean}]
let pick : (a : int) -> int{ ile a _ } = fun a -> a
