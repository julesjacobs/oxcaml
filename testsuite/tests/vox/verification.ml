(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
 { expect.opt; }
*)

external ( = ) : int -> int -> bool @@ total = "%equal"
external ( >= ) : int -> int -> bool @@ total = "%greaterequal"
external ( > ) : int -> int -> bool @@ total = "%greaterthan";;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
external ( >= ) : int -> int -> bool = "%greaterequal"
external ( > ) : int -> int -> bool = "%greaterthan"
|}]

type zero = {n : int | n = 0}
type nonnegative = {n : int | n >= 0};;
[%%expect{|
type zero = {n : int | n = 0}
type nonnegative = {n : int | n >= 0}
|}]

let clamp x : nonnegative =
  if x >= 0 then refine_ x else let z = 0 in refine_ z;;
[%%expect{|
val clamp : int @ total -> nonnegative = <fun>
|}]

let checked_read read : zero =
  let x = read () in
  let (_ : zero) = assume_ x in
  refine_ x;;
[%%expect{|
val checked_read : (unit -> int @ total) -> zero = <fun>
|}]

let unproved read : zero =
  let x = read () in refine_ x;;
[%%expect{|
Line 2, characters 21-30:
2 |   let x = read () in refine_ x;;
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let overflow x : {n : int | n > x} =
  let y = x + 1 in refine_ y;;
[%%expect{|
Line 2, characters 19-28:
2 |   let y = x + 1 in refine_ y;;
                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let bad_branch b : nonnegative =
  let x = if b then 1 else -1 in refine_ x;;
[%%expect{|
Line 2, characters 33-42:
2 |   let x = if b then 1 else -1 in refine_ x;;
                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let eliminate (r : zero) : zero =
  let refine_ x = r in refine_ x;;
[%%expect{|
val eliminate : zero -> zero = <fun>
|}]

type tuple_fact =
  {u : unit | match (0, 0) with (left, right) -> left = right}

let omit_irrelevant_tuple_fact (fact : tuple_fact) : zero =
  let refine_ proof = fact in
  let zero = 0 in
  refine_ zero;;
[%%expect{|
type tuple_fact =
    {u : unit | match (0, 0) with | (left, right) -> left = right}
val omit_irrelevant_tuple_fact : tuple_fact -> zero = <fun>
|}]

let report_needed_tuple_fact (fact : tuple_fact) : zero =
  let refine_ proof = fact in
  let one = 1 in
  refine_ one;;
[%%expect{|
Line 4, characters 2-13:
4 |   refine_ one;;
      ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 22-26:
2 |   let refine_ proof = fact in
                          ^^^^
  This refinement premise was omitted because it could not be translated to SMT
Line 2, characters 20-26:
2 |   {u : unit | match (0, 0) with (left, right) -> left = right}
                        ^^^^^^
  Unsupported refinement predicate in VC generation
|}]

let hidden (r : zero) : zero =
  let x = 1 in refine_ x;;
[%%expect{|
Line 2, characters 15-24:
2 |   let x = 1 in refine_ x;;
                   ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let hidden_false (r : {n : int | false}) : zero =
  let x = 1 in refine_ x;;
[%%expect{|
Line 2, characters 15-24:
2 |   let x = 1 in refine_ x;;
                   ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let eliminated_false (r : {n : int | false}) : zero =
  let refine_ unused = r in
  let x = 1 in refine_ x;;
[%%expect{|
val eliminated_false : {n : int | false} -> zero = <fun>
|}]

type copy_zero = (r : zero) -> {y : int | let refine_ x = r in y = x}
let copy : copy_zero = fun r -> let refine_ x = r in refine_ x;;
[%%expect{|
type copy_zero = (r : zero) -> {y : int | let refine_ x = r in y = x}
val copy : copy_zero = <fun>
|}]

let repeated read : zero =
  let x = read () in
  let (_ : zero) = assume_ x in
  let y = read () in refine_ y;;
[%%expect{|
Line 4, characters 21-30:
4 |   let y = read () in refine_ y;;
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let caught x : zero =
  let _ = try let (_ : zero) = assume_ x in () with _ -> () in
  refine_ x;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ x;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let guarded x : nonnegative =
  match x with
  | n when n >= 0 -> refine_ n
  | _ -> let z = 0 in refine_ z;;
[%%expect{|
val guarded : int @ total -> nonnegative = <fun>
|}]

let nested x = ignore (let f () : zero = refine_ x in f);;
[%%expect{|
Line 1, characters 41-50:
1 | let nested x = ignore (let f () : zero = refine_ x in f);;
                                             ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let nonvariable : zero = refine_ 0;;
[%%expect{|
Line 1, characters 33-34:
1 | let nonvariable : zero = refine_ 0;;
                                     ^
Error: "refine_" requires a plain local variable
|}]

let rec diverge () : zero = diverge ();;
[%%expect{|
val diverge : unit -> zero = <fun>
|}]

let different_lines () =
  let x = __LINE__ in
  let y = __LINE__ in
  let (_ : {n : int | n = x}) = refine_ y in
  ();;
[%%expect{|
Line 4, characters 32-41:
4 |   let (_ : {n : int | n = x}) = refine_ y in
                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let unchecked_short_circuit x b : zero =
  let _ = b && (let (_ : zero) = assume_ x in true) in
  refine_ x;;
[%%expect{|
Line 3, characters 2-11:
3 |   refine_ x;;
      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let checked_short_circuit x : zero =
  let _ = true && (let (_ : zero) = assume_ x in true) in
  refine_ x;;
[%%expect{|
val checked_short_circuit : int @ total -> zero = <fun>
|}]

let rec circular_proof : {n : int | false} =
  let refine_ unused = circular_proof in
  let z = 0 in refine_ z;;
[%%expect{|
Lines 2-3, characters 2-24:
2 | ..let refine_ unused = circular_proof in
3 |   let z = 0 in refine_ z..
Error: Refinement verification does not support recursive value initialization
|}]

class circular_class =
  let rec bad : {n : int | false} =
    let refine_ unused = bad in
    let z = 0 in refine_ z
  in object end;;
[%%expect{|
Line 2, characters 10-13:
2 |   let rec bad : {n : int | false} =
              ^^^
Warning 26 [unused-var]: unused variable "bad".

Lines 3-4, characters 4-26:
3 | ....let refine_ unused = bad in
4 |     let z = 0 in refine_ z
Error: Refinement verification does not support recursive value initialization
|}]

external lt : int -> int -> bool @@ total = "%ltint"
external le : int -> int -> bool @@ total = "%leint"
external gt : int -> int -> bool @@ total = "%gtint"
external ge : int -> int -> bool @@ total = "%geint";;
[%%expect{|
external lt : int -> int -> bool = "%ltint"
external le : int -> int -> bool = "%leint"
external gt : int -> int -> bool = "%gtint"
external ge : int -> int -> bool = "%geint"
|}]

let less x : {n : int | lt n 0} =
  if lt x 0 then refine_ x else let n = -1 in refine_ n
let less_equal x : {n : int | le n 0} =
  if le x 0 then refine_ x else let n = 0 in refine_ n
let greater x : {n : int | gt n 0} =
  if gt x 0 then refine_ x else let n = 1 in refine_ n
let greater_equal x : {n : int | ge n 0} =
  if ge x 0 then refine_ x else let n = 0 in refine_ n;;
[%%expect{|
val less : int @ total -> {n : int | lt n 0} = <fun>
val less_equal : int @ total -> {n : int | le n 0} = <fun>
val greater : int @ total -> {n : int | gt n 0} = <fun>
val greater_equal : int @ total -> {n : int | ge n 0} = <fun>
|}]

external to_int : bool -> int @@ total = "%identity";;
[%%expect{|
external to_int : bool -> int = "%identity"
|}]

let converted_sort b =
  match to_int b with
  | 0 -> let x = 0 in let (_ : {n : int | true}) = refine_ x in ()
  | _ -> ();;
[%%expect{|
val converted_sort : bool -> unit = <fun>
|}]

type number = int
type flag = bool;;
[%%expect{|
type number = int
type flag = bool
|}]

let alias_step (x : number) : {n : number | n = x + 1} =
  let n = x + 1 in refine_ n
let aliased_add (x : int) : {n : int | n = x + 1} =
  let add = ( + ) in
  let add_again = add in
  let n = add_again x 1 in
  let (_ : {n : int | n = add_again x 1}) = refine_ n in
  refine_ n
let alias_flag (b : flag) : {r : flag | r} =
  let r = if b then true else not b in refine_ r;;
[%%expect{|
val alias_step : (x : number) -> {n : number | n = (x + 1)} = <fun>
val aliased_add : (x : int) -> {n : int | n = (x + 1)} = <fun>
val alias_flag : flag -> {r : flag | r} = <fun>
|}]

let physical_int (x : number) : {b : bool | b} =
  let y = x + 0 in
  let b = x == y in refine_ b
let physical_bool (x : flag) : {b : bool | b} =
  let y = not (not x) in
  let b = x == y in refine_ b
let physical_refined (x : zero) : {b : bool | b} =
  let b = x == x in refine_ b;;
[%%expect{|
val physical_int : number -> {b : bool | b} = <fun>
val physical_bool : flag -> {b : bool | b} = <fun>
val physical_refined : zero -> {b : bool | b} = <fun>
|}]

external physical_equal : 'a @ immutable -> 'a @ immutable -> bool @@ total =
  "%eq";;
[%%expect{|
external physical_equal : 'a @ immutable -> 'a @ immutable -> bool = "%eq"
|}]

let physical_predicate (x : number) : {n : number | physical_equal n x} =
  let n = x + 0 in refine_ n;;
[%%expect{|
val physical_predicate : (x : number) -> {n : number | physical_equal n x} =
  <fun>
|}]

let shadowed_add () : {n : int | n = 2} =
  let ( + ) x y = x - y in
  let add = ( + ) in
  let n = add 1 1 in refine_ n;;
[%%expect{|
Line 4, characters 21-30:
4 |   let n = add 1 1 in refine_ n;;
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Abstract : sig type t end = struct type t = int end;;
[%%expect{|
module Abstract : sig type t end
|}]

let abstract_physical (x : Abstract.t) : {b : bool | b} =
  let b = physical_equal x x in refine_ b;;
[%%expect{|
Line 2, characters 32-41:
2 |   let b = physical_equal x x in refine_ b;;
                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
