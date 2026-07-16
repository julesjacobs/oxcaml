(* variant A: short leading comment (keeps declaration offsets low) *)
val s : string{ _ = "x" }
val i : int{ _ >= 0 }
val n : string{ (if _ = "a" then "b" else "c") = _ }
