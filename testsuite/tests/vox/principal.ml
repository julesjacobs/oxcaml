(* TEST
 flags = "-extension refinement_types";
 expect;
*)

type ordinary_comparison = {n : int | n >= 0};;
[%%expect{|
type ordinary_comparison = {n : int | n >= 0}
|}, Principal{|
Line 1, characters 38-39:
1 | type ordinary_comparison = {n : int | n >= 0};;
                                          ^
Error: This value is "immutable" but is expected to be "read_write".
|}]
