type t

exception Limit

val create :
  domains:int array ->
  orders:bool array array array ->
  Solver_graph.atom list ->
  t option

val project : ?allow_self_edges:bool -> t -> int list -> t option

val implies : t -> t -> bool option

val satisfies : t -> int array -> bool
