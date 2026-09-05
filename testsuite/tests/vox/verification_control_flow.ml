(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
*)

external ( = ) : int -> int -> bool @@ total = "%equal"
external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external not : bool -> bool @@ total = "%boolnot"
type zero = {n : int | n = 0};;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
external ( >= ) : int -> int -> bool = "%greaterequal"
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

let quotient () : zero =
  let n = (-7 / 2) + 3 in
  refine_ n;;
[%%expect{|
val quotient : unit -> zero = <fun>
|}]

let remainder () : zero =
  let n = (-7 mod 2) + 1 in
  refine_ n;;
[%%expect{|
val remainder : unit -> zero = <fun>
|}]

let normal_return x : zero =
  if x = 0 then () else raise Exit;
  refine_ x;;
[%%expect{|
val normal_return : int @ total -> zero = <fun>
|}]

let caught_exception x : zero =
  (try if x = 0 then () else raise Exit with Exit -> ());
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

let joined_value b : zero =
  let x = if b then 0 else 0 in
  refine_ x;;
[%%expect{|
val joined_value : bool -> zero = <fun>
|}]

let joined_check x b : zero =
  (if b then let (_ : zero) = assume_ x in ()
   else let (_ : zero) = assume_ x in ());
  refine_ x;;
[%%expect{|
val joined_check : int @ total -> bool -> zero = <fun>
|}]

let later_assumption x =
  let (_ : zero) = refine_ x in
  let (_ : {n : int | false}) = assume_ x in ();;
[%%expect{|
Line 2, characters 19-28:
2 |   let (_ : zero) = refine_ x in
                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let failing_batch x =
  let z = 0 in
  let (_ : zero) = refine_ z in
  let (_ : zero) = refine_ x in ();;
[%%expect{|
Line 4, characters 19-28:
4 |   let (_ : zero) = refine_ x in ();;
                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let captured_fact x : zero =
  let (_ : zero) = assume_ x in
  let get () : zero = refine_ x in
  get ();;
[%%expect{|
val captured_fact : int @ total -> zero = <fun>
|}]

let branch_assertion x b =
  if b then (let (_ : zero) = refine_ x in raise Exit) else ();;
[%%expect{|
Line 2, characters 30-39:
2 |   if b then (let (_ : zero) = refine_ x in raise Exit) else ();;
                                  ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let joined_divisor b : zero =
  let d = if b then 1 else 2 in
  let x = 0 / d in refine_ x;;
[%%expect{|
val joined_divisor : bool -> zero = <fun>
|}]

let joined_modulus b : zero =
  let d = if b then 1 else 2 in
  let x = 0 mod d in refine_ x;;
[%%expect{|
val joined_modulus : bool -> zero = <fun>
|}]

let divisor_normal_return d : {n : int | not (n = 0)} =
  let _ = 0 / d in refine_ d;;
[%%expect{|
val divisor_normal_return : int @ total -> {n : int | not (n = 0)} = <fun>
|}]

let joined_assumption b x : zero =
  let y = if b then (let (_ : zero) = assume_ x in x) else 0 in
  refine_ y;;
[%%expect{|
val joined_assumption : bool -> int @ total -> zero = <fun>
|}]

let conditional_assumption b x : zero =
  let _ = if b then (let (_ : zero) = assume_ x in x) else 0 in
  refine_ x;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ x;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let joined_exit b : zero =
  let y = if b then 0 else raise Exit in
  refine_ y;;
[%%expect{|
val joined_exit : bool -> zero = <fun>
|}]

let conditional_divisor d b : {n : int | not (n = 0)} =
  let _ = if b then 1 / d else 0 in
  refine_ d;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ d;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let multiple_joins b c : zero =
  let x = if b then 0 else 1 in
  let (_ : {n : int | n >= 0}) = refine_ x in
  let y = if c then 0 else x - x in
  refine_ y;;
[%%expect{|
val multiple_joins : bool -> bool -> zero = <fun>
|}]
