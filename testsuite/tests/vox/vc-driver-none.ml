(* TEST
 flags = "-vox-backend none";
 expect;
*)

(* The default driver policy: -vox-backend none short-circuits before the
   walk, so a unit full of refined claims — including one that a solver
   would refute — compiles silently, byte-identical to today
   (design-docs/vc-generation.md, "Where the pass sits").  Under this
   default, refined types are recorded, unverified claims. *)

let v : int{ _ > 0 } = 5;;
[%%expect{|
val v : int{ _ > 0 } = 5
|}]

let refuted_but_not_run : int{ _ > 0 } = 0;;
[%%expect{|
val refuted_but_not_run : int{ _ > 0 } = 0
|}]

let f1 : int{ _ > 0 } -> int = fun y -> y;;
[%%expect{|
val f1 : int{ _ > 0 } -> int = <fun>
|}]

let arrow_domain = f1 5;;
[%%expect{|
val arrow_domain : int = 5
|}]
