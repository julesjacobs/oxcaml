(* TEST
   flags = "-extension-universe alpha";
   expect;
*)

type update = { x : int; y : int }
[%%expect{|
type update = { x : int; y : int; }
|}]

let (overwrite @ total) (r : update @ unique) =
  overwrite_ r with { x = 1 }
[%%expect{|
Line 2, characters 2-29:
2 |   overwrite_ r with { x = 1 }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-2, characters 24-29
         which is expected to be "total".
|}]
