(* TEST
 flags = "-extension refinement_types";
 expect;
*)

type ordinary_comparison = {n : int | n >= 0};;
[%%expect{|
type ordinary_comparison = {n : int | n >= 0}
|}]

type boolean_comparison = {b : bool | b = true};;
[%%expect{|
type boolean_comparison = {b : bool | b = true}
|}]

type nested_comparison =
  {n : int option | match n with None -> true | Some n -> n >= 0};;
[%%expect{|
type nested_comparison =
    {n : int option | match n with | None -> true | Some n -> n >= 0}
|}]

type 'a constrained =
  {p : 'a * int | match p with a, b -> a + b >= 0};;
[%%expect{|
type 'a constrained = {p : 'a * int | match p with | (a, b) -> (a + b) >= 0}
  constraint 'a = int
|}]

type 'a inconsistent = {f : 'a -> int | f 0 = f true};;
[%%expect{|
Line 1, characters 48-52:
1 | type 'a inconsistent = {f : 'a -> int | f 0 = f true};;
                                                    ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]
