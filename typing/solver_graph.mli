type atom =
  | Unary of int * bool array
  | Binary of int * int * bool array array

type t

exception Limit

type budget

val budget : unit -> budget

val create :
  domains:int array -> orders:bool array array array -> atom list -> t

val atoms : t -> atom list

val satisfies : t -> int array -> bool

val project : ?budget:budget -> t -> int list -> t option

val implies : ?budget:budget -> t -> t -> bool option
