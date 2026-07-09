(* PROBE A: can an OCaml tuple (int * int) be given a via/model sort? *)
type pair = int * int [@@vox.sort lean "IPair"]

[%%vox.lean {lean|
public structure IPair where
  fst : Int
  snd : Int
|lean}]

let mk : (a : int) -> (b : int) -> pair{ 0 = 0 } =
  fun a b -> (a, b)
