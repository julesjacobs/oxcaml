(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

type interval = { lower : int; upper : {u : int | lower <= u} };;
[%%expect{|
type interval = { lower : int @@ total; upper : {u : int | lower <= u}; }
|}]

let bad = { lower = 5; upper = 2 };;
[%%expect{|
Line 1, characters 31-32:
1 | let bad = { lower = 5; upper = 2 };;
                                   ^
Error: Refinement could not be proved (counterexample)
Line 1, characters 50-60:
1 | type interval = { lower : int; upper : {u : int | lower <= u} };;
                                                      ^^^^^^^^^^
  The refinement is stated here.
|}]

let bad (r : interval) = { r with lower = 100 };;
[%%expect{|
Line 1, characters 25-47:
1 | let bad (r : interval) = { r with lower = 100 };;
                             ^^^^^^^^^^^^^^^^^^^^^^
Error: Updating this record also requires replacing the dependent field upper
|}]

let bad (r : interval) = { r with lower = 100; upper = r.upper };;
[%%expect{|
Line 1, characters 25-64:
1 | let bad (r : interval) = { r with lower = 100; upper = r.upper };;
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 23 [useless-record-with]: all the fields are explicitly listed in this record:
  the "with" clause is useless.

Line 1, characters 55-62:
1 | let bad (r : interval) = { r with lower = 100; upper = r.upper };;
                                                           ^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 50-60:
1 | type interval = { lower : int; upper : {u : int | lower <= u} };;
                                                      ^^^^^^^^^^
  The refinement is stated here.
|}]

let bad (r : interval) (s : interval) : {u : int | s.lower <= u} = r.upper;;
[%%expect{|
Line 1, characters 67-74:
1 | let bad (r : interval) (s : interval) : {u : int | s.lower <= u} = r.upper;;
                                                                       ^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 51-63:
1 | let bad (r : interval) (s : interval) : {u : int | s.lower <= u} = r.upper;;
                                                       ^^^^^^^^^^^^
  The refinement is stated here.
|}]

type minimum = {
  value : int;
  optimality : (other : {n : int | 0 <= n}) ->
    {u : unit | value <= other} @@ ghost total;
};;
[%%expect{|
type minimum = {
  value : int @@ total;
  optimality : (other : {n : int | 0 <= n}) -> {u : unit | value <= other} @@
    ghost total;
}
|}]

let bad = { value = 1;
  optimality = ghost_ (fun _other -> let u = () in refine_ u) };;
[%%expect{|
Line 2, characters 51-60:
2 |   optimality = ghost_ (fun _other -> let u = () in refine_ u) };;
                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample: _other = 0)
Line 4, characters 16-30:
4 |     {u : unit | value <= other} @@ ghost total;
                    ^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

type mutable_dependency = {
  mutable lower : int;
  upper : {u : int | lower <= u};
};;
[%%expect{|
Line 2, characters 2-22:
2 |   mutable lower : int;
      ^^^^^^^^^^^^^^^^^^^^
Error: Dependent record fields must be immutable
|}]

type forward_dependency = {
  upper : {u : int | lower <= u};
  lower : int;
};;
[%%expect{|
Line 2, characters 21-26:
2 |   upper : {u : int | lower <= u};
                         ^^^^^
Error: Unbound value "lower"
Hint:   Did you mean "lor"?
|}]

module Bad : sig
  type t = { lower : int; upper : {u : int | lower < u} }
end = struct
  type t = { lower : int; upper : {u : int | lower <= u} }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = { lower : int; upper : {u : int | lower <= u} }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = { lower : int @@ total; upper : {u : int | lower <= u}; }
         end
       is not included in
         sig
           type t = { lower : int @@ total; upper : {u : int | lower < u}; }
         end
       Type declarations do not match:
         type t = { lower : int @@ total; upper : {u : int | lower <= u}; }
       is not included in
         type t = { lower : int @@ total; upper : {u : int | lower < u}; }
       Fields do not match:
         "upper : {u : int | lower <= u};"
       is not the same as:
         "upper : {u : int | lower < u};"
       The type "{u : int | lower <= u}" is not equal to the type
         "{u : int | lower < u}"
|}]

let bad_pattern (r : interval) (s : interval) : {u : int | s.lower <= u} =
  let {upper; _} = r in upper;;
[%%expect{|
Line 2, characters 24-29:
2 |   let {upper; _} = r in upper;;
                            ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 59-71:
1 | let bad_pattern (r : interval) (s : interval) : {u : int | s.lower <= u} =
                                                               ^^^^^^^^^^^^
  The refinement is stated here.
|}]

type function_dependency = {
  f : int -> int;
  proof : {u : unit | f 0 = 0} @@ ghost;
};;
[%%expect{|
type function_dependency = {
  f : int -> int @@ total;
  proof : {u : unit | (f 0) = 0} @@ ghost;
}
|}]

let partial_f (_ : int) = failwith "partial";;
[%%expect{|
val partial_f : int -> 'a = <fun>
|}]

let bad_function = { f = partial_f; proof = ghost_ () };;
[%%expect{|
Line 1, characters 25-34:
1 | let bad_function = { f = partial_f; proof = ghost_ () };;
                             ^^^^^^^^^
Error: This value is "partial"
       but is expected to be "total"
         because it is the field "f" (with some modality) of the record at line 1, characters 19-55.
|}]

let good = { value = 0;
  optimality = ghost_ (fun _other -> let u = () in refine_ u) };;
[%%expect{|
val good : minimum = {value = 0; optimality = <ghost>}
|}]

let mismatched_proof = { value = 1; optimality = good.optimality };;
[%%expect{|
Line 1, characters 49-64:
1 | let mismatched_proof = { value = 1; optimality = good.optimality };;
                                                     ^^^^^^^^^^^^^^^
Error: The field access "good.optimality" has type
         "(other : {n : int | 0 <= n}) -> {u : unit | good.value <= other}"
       but an expression was expected of type
         "(other : {n : int | 0 <= n}) -> {u : unit | ( *value* ) <= other}"
       Type "{u : unit | good.value <= other}" is not compatible with type
         "{u : unit | ( *value* ) <= other}"
|}]

type partial_dependency = {
  callback : int -> int @@ partial;
  proof : {u : unit | callback 0 = 0} @@ ghost;
};;
[%%expect{|
Line 2, characters 27-34:
2 |   callback : int -> int @@ partial;
                               ^^^^^^^
Warning 220 [redundant-modality]: This modality is redundant.

Line 2, characters 27-34:
2 |   callback : int -> int @@ partial;
                               ^^^^^^^
Error: Fields used by later refinements must be total, stateless, and portable
|}]

let escape () =
  let r = good in
  let {optimality; _} = r in
  optimality;;
[%%expect{|
Line 4, characters 2-12:
4 |   optimality;;
      ^^^^^^^^^^
Error: the refinement type of this expression escapes the scope of binding "_*record0*"
Hint: bind "_*record0*" outside this expression.
|}]

module Escaping_pattern = struct
  let {upper; _} = {lower = 1; upper = 2}
end;;
[%%expect{|
Line 2, characters 7-12:
2 |   let {upper; _} = {lower = 1; upper = 2}
           ^^^^^
Error: This inferred type depends on an unnamed record. Bind the record to a name and project its fields.
|}]

type linked = { first : int; second : {v : int | v = first} };;
[%%expect{|
type linked = { first : int @@ total; second : {v : int | v = first}; }
|}]

let bad_constants (left : linked) (right : linked) : {b : bool | b} =
  match left, right with
  | {second = 0; _}, {second = 1; _} -> false
  | _ -> true;;
[%%expect{|
Line 3, characters 40-45:
3 |   | {second = 0; _}, {second = 1; _} -> false
                                            ^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 65-66:
1 | let bad_constants (left : linked) (right : linked) : {b : bool | b} =
                                                                     ^
  The refinement is stated here.
|}]
