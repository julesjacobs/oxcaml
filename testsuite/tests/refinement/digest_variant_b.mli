(* variant B: a deliberately longer, multi-line leading comment *)
(* whose only effect is to move the declarations below to different *)
(* line numbers and absolute character offsets than in variant A.   *)
val s : string{ _ = "x" }
val i : int{ _ >= 0 }
val n : string{ (if _ = "a" then "b" else "c") = _ }
