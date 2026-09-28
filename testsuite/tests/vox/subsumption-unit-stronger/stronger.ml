(* Trusted declarations only: this file makes no refinement introduction, so
   verification runs only because of the interface's obligations. *)
external f : (x : int) -> {r : int | r = x} = "%identity"
external g : (x : {x : int | x >= 0}) -> {r : int | r = x} = "%identity"
external lem : (x : int) -> {u : unit | x + 0 = x && x * 1 = x} @@ total =
  "%ignore"
external h : (x : int) -> {r : int | r = x} = "%identity"
