(* TEST
   flags = "-extension layouts_alpha -ikind";
   expect;
*)

type 'a myref : value with 'a = int ref
[%%expect {|
type 'a myref = int ref
|}]

