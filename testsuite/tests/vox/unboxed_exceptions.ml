(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
*)

external[@layout_poly] raise_any : ('a : any).
  exn -> 'a @ portable unique = "%raise"
type result = #{left : int; right : int}
let failed () : result = raise_any (Failure "unboxed result")
let caught = try let _ = failed () in false with Failure _ -> true;;
[%%expect{|
external raise_any : ('a : any). exn -> 'a @ unique portable = "%raise"
  [@@layout_poly]
type result = #{ left : int; right : int; }
val failed : unit -> result = <fun>
val caught : bool = true
|}]

let (false_proof @ total) () : {u : unit | false} = raise_any Exit;;
[%%expect{|
Line 1, characters 52-61:
1 | let (false_proof @ total) () : {u : unit | false} = raise_any Exit;;
                                                        ^^^^^^^^^
Error: The value "raise_any" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 26-66
         which is expected to be "total".
|}]

let standard_failed () : result = raise (Failure "standard raise")
let standard_caught =
  try let _ = standard_failed () in false with Failure _ -> true;;
[%%expect{|
val standard_failed : unit -> result = <fun>
val standard_caught : bool = true
|}]

let notrace_failed () : result = raise_notrace Exit
let notrace_caught =
  try let _ = notrace_failed () in false with Exit -> true;;
[%%expect{|
val notrace_failed : unit -> result = <fun>
val notrace_caught : bool = true
|}]

let (standard_false_proof @ total) () : {u : unit | false} = raise Exit;;
[%%expect{|
Line 1, characters 61-66:
1 | let (standard_false_proof @ total) () : {u : unit | false} = raise Exit;;
                                                                 ^^^^^
Error: The value "raise" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 35-71
         which is expected to be "total".
|}]
