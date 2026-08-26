(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
*)

external ( = ) : int -> int -> bool @@ total = "%equal"
external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external ( && ) : bool -> bool -> bool @@ total = "%sequand"
external ( || ) : bool -> bool -> bool @@ total = "%sequor"
external not : bool -> bool @@ total = "%boolnot"
type zero = {n : int | n = 0};;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
external not : bool -> bool = "%boolnot"
type zero = {n : int | n = 0}
|}]

let predicate_cases x :
    {n : int | match n with
               | 0 when false -> false
               | y when y >= 0 -> true
               | _ -> true} =
  refine_ x;;
[%%expect{|
val predicate_cases :
  int @ total ->
  {n : int
    | match n with
      | 0 when false -> false
      | y when y >= 0 -> true
      | _ -> true} =
  <fun>
|}]

let guard_fact x : zero =
  match x with
  | _ when (let (_ : zero) = assume_ x in false) -> refine_ x
  | _ -> refine_ x;;
[%%expect{|
val guard_fact : int @ total -> zero = <fun>
|}]

let checked_or x : zero =
  let _ = false || (let (_ : zero) = assume_ x in true) in
  refine_ x;;
[%%expect{|
val checked_or : int @ total -> zero = <fun>
|}]

let unchecked_or x b : zero =
  let _ = b || (let (_ : zero) = assume_ x in true) in
  refine_ x;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ x;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let predicate_short_circuit x :
    {n : int | (n = 0 || not (n = 0)) && true} = refine_ x;;
[%%expect{|
val predicate_short_circuit :
  int @ total -> {n : int | ((n = 0) || (not (n = 0))) && true} = <fun>
|}]

let right_to_left x =
  let consume (_ : zero) () = () in
  consume (refine_ x) (let (_ : zero) = assume_ x in ());;
[%%expect{|
val right_to_left : int @ total -> unit = <fun>
|}]

let not_left_to_right x =
  let consume () (_ : zero) = () in
  consume (let (_ : zero) = assume_ x in ()) (refine_ x);;
[%%expect{|
Line 3, characters 45-56:
3 |   consume (let (_ : zero) = assume_ x in ()) (refine_ x);;
                                                 ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let callee_after_argument x =
  (let (_ : zero) = refine_ x in fun () -> ())
    (let (_ : zero) = assume_ x in ());;
[%%expect{|
val callee_after_argument : int @ total -> unit = <fun>
|}]

let not_callee_before_argument x =
  (let (_ : zero) = assume_ x in fun (_ : zero) -> ()) (refine_ x);;
[%%expect{|
Line 2, characters 55-66:
2 |   (let (_ : zero) = assume_ x in fun (_ : zero) -> ()) (refine_ x);;
                                                           ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
