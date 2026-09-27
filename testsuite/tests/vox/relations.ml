(* TEST
 flags = "-extension refinement_types";
 has-z3;
 { expect; }
*)

(* [@@relation] declares an inductive relation by its rules. It expands to a
   derivation datatype and ordinary checked definitions. *)

external ( = ) : int -> int -> bool @@ total = "%equal"
external ( < ) : int -> int -> bool @@ total = "%lessthan"
external ( <= ) : int -> int -> bool @@ total = "%lessequal";;
[%%expect{|
external ( = ) : int -> int -> bool = "%equal"
external ( < ) : int -> int -> bool = "%lessthan"
external ( <= ) : int -> int -> bool = "%lessequal"
|}]

(* The reflexive-transitive closure of a step relation. *)
module Star = struct
  let[@def transparent] step (x : int) (y : int) = x < y

  type star = Refl of int | Step of int * int * int * star
  [@@relation function
    | Refl x -> (x, x)
    | Step (x, y, z, rest) when step x y && star rest y z -> (x, z)]
end
open Star;;
[%%expect{|
module Star :
  sig
    val step : int -> int -> bool
    val step_def :
      (x : int) -> (y : int) -> {u : unit | (step x y) === (x < y)}
    type star = Refl of int | Step of int * int * int * star
    [@@inductive]
    val star_concl : star @ immutable -> int * int
    val star_concl_def :
      (d : star) @ immutable ->
      {u : unit
        | (star_concl d) ===
            (match d with
             | Refl x' -> (x', x')
             | Step (x, y, z, rest) -> (x, z))}
    val star_valid : star @ total immutable -> bool @ ghost
    val star_valid_def :
      (d : star) @ immutable ->
      {u : unit
        | (star_valid d) ===
            (ghost_
               (match d with
                | Refl x' -> true
                | Step (x, y, z, rest) ->
                    (step x y) &&
                      ((star_valid rest) && ((star_concl rest) === (y, z)))))}
    val star :
      star @ total immutable ->
      int @ total immutable -> int @ total immutable -> bool @ ghost
    val star_def :
      (d : star) @ immutable ->
      (a1 : int) @ immutable ->
      (a2 : int) @ immutable ->
      {u : unit
        | (star d a1 a2) ===
            (ghost_ ((star_valid d) && ((star_concl d) === (a1, a2))))}
    val star_inversion :
      (d : star) @ immutable ->
      (a1 : int) @ immutable ->
      (a2 : int) @ immutable ->
      {u : unit
        | (star d a1 a2) ===
            (match d with
             | Refl x' -> (a1, a2) === (x', x')
             | Step (x, y, z, rest) ->
                 ((a1, a2) === (x, z)) && ((step x y) && (star rest y z)))} @ ghost
  end
|}]

(* Rule induction is structural recursion over the derivation; inversion
   gives the premises of the rule that applies. *)
let rec (monotone @ total) :
    (d : star) -> (x : int) -> (z : int) ->
    {u : unit | if star d x z then x <= z else true} @ ghost =
  fun d x z -> ghost_ (
    star_inversion d x z;
    match d with
    | Refl _ -> ()
    | Step (_, y, _, rest) -> monotone rest y z);;
[%%expect{|
val monotone :
  (d : Star.star) ->
  ((x : int) ->
   (z : int) -> {u : unit | if Star.star d x z then x <= z else true} @ ghost) @ total
  stateful = <fun>
|}]

(* The same equation introduces a derivation. *)
let rec (trans @ total) :
    (d1 : star) -> (d2 : star) -> (x : int) -> (y : int) -> (z : int) ->
    {d : star | if star d1 x y && star d2 y z then star d x z else true}
    @ ghost =
  fun d1 d2 x y z -> ghost_ (
    star_inversion d1 x y;
    match d1 with
    | Refl _ -> d2
    | Step (_, w, _, rest) ->
      let d = Step (x, w, z, trans rest d2 w y z) in
      star_inversion d x z;
      d);;
[%%expect{|
val trans :
  (d1 : Star.star) ->
  ((d2 : Star.star) ->
   (x : int) ->
   (y : int) ->
   (z : int) ->
   {d : Star.star
     | if (Star.star d1 x y) && (Star.star d2 y z)
       then Star.star d x z
       else true} @ ghost) @ total
  stateful = <fun>
|}]

let (no_step_back @ total) (d : star) (x : int) :
    {u : unit | not (star d (x + 1) x) || x + 1 < x} @ ghost =
  ghost_ (monotone d (x + 1) x);;
[%%expect{|
val no_step_back :
  (d : Star.star) ->
  (x : int) ->
  {u : unit | (not (Star.star d (x + 1) x)) || ((x + 1) < x)} @ ghost = <fun>
|}]

(* A false claim is still rejected. *)
let rec (wrong @ total) :
    (d : star) -> (x : int) -> (z : int) ->
    {u : unit | if star d x z then x < z else true} @ ghost =
  fun d x z -> ghost_ (
    star_inversion d x z;
    match d with
    | Refl _ -> ()
    | Step (_, y, _, rest) -> wrong rest y z);;
[%%expect{|
Line 7, characters 16-18:
7 |     | Refl _ -> ()
                    ^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 16-50:
3 |     {u : unit | if star d x z then x < z else true} @ ghost =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* A relation with one argument, and with type parameters. *)
module Lists = struct
  type ordered = O_nil | O_one of int | O_cons of int * int * int list * ordered
  [@@relation function
    | O_nil -> []
    | O_one x -> [x]
    | O_cons (x, y, rest, p) when x <= y && ordered p (y :: rest) ->
      x :: y :: rest]

  type ('a : immutable_data) mem =
    | Here of 'a * 'a list
    | There of 'a * 'a * 'a list * 'a mem
  [@@relation function
    | Here (x, xs) -> (x, x :: xs)
    | There (x, y, xs, p) when mem p x xs -> (x, y :: xs)]
end
open Lists;;
[%%expect{|
module Lists :
  sig
    type ordered =
        O_nil
      | O_one of int
      | O_cons of int * int * int list * ordered
    [@@inductive]
    val ordered_concl : ordered @ immutable -> int list
    val ordered_concl_def :
      (d : ordered) @ immutable ->
      {u : unit
        | (ordered_concl d) ===
            (match d with
             | O_nil -> []
             | O_one x' -> [x']
             | O_cons (x, y, rest, p) -> x :: y :: rest)}
    val ordered_valid : ordered @ total immutable -> bool @ ghost
    val ordered_valid_def :
      (d : ordered) @ immutable ->
      {u : unit
        | (ordered_valid d) ===
            (ghost_
               (match d with
                | O_nil -> true
                | O_one x' -> true
                | O_cons (x, y, rest, p) ->
                    (x <= y) &&
                      ((ordered_valid p) &&
                         ((ordered_concl p) === (y :: rest)))))}
    val ordered :
      ordered @ total immutable -> int list @ total immutable -> bool @ ghost
    val ordered_def :
      (d : ordered) @ immutable ->
      (a1 : int list) @ immutable ->
      {u : unit
        | (ordered d a1) ===
            (ghost_ ((ordered_valid d) && ((ordered_concl d) === a1)))}
    val ordered_inversion :
      (d : ordered) @ immutable ->
      (a1 : int list) @ immutable ->
      {u : unit
        | (ordered d a1) ===
            (match d with
             | O_nil -> a1 === []
             | O_one x' -> a1 === [x']
             | O_cons (x, y, rest, p) ->
                 (a1 === (x :: y :: rest)) &&
                   ((x <= y) && (ordered p (y :: rest))))} @ ghost
    type ('a : immutable_data) mem =
        Here of 'a * 'a list
      | There of 'a * 'a * 'a list * 'a mem
    [@@inductive]
    val mem_concl :
      ('a : immutable_data). 'a mem @ immutable -> 'a * 'a list @ immutable
    val mem_concl_def :
      ('a : immutable_data).
        (d : 'a mem) @ immutable ->
        {u : unit
          | (mem_concl d) ===
              (match d with
               | Here (x', xs') -> (x', (x' :: xs'))
               | There (x, y, xs, p) -> (x, (y :: xs)))}
    val mem_valid :
      ('a : immutable_data). 'a mem @ total immutable -> bool @ ghost
    val mem_valid_def :
      ('a : immutable_data).
        (d : 'a mem) @ immutable ->
        {u : unit
          | (mem_valid d) ===
              (ghost_
                 (match d with
                  | Here (x', xs') -> true
                  | There (x, y, xs, p) ->
                      (mem_valid p) && ((mem_concl p) === (x, xs))))}
    val mem :
      ('a : immutable_data).
        'a mem @ total immutable ->
        'a @ total immutable -> 'a list @ total immutable -> bool @ ghost
    val mem_def :
      ('a : immutable_data).
        (d : 'a mem) @ immutable ->
        (a1 : 'a) @ immutable ->
        (a2 : 'a list) @ immutable ->
        {u : unit
          | (mem d a1 a2) ===
              (ghost_ ((mem_valid d) && ((mem_concl d) === (a1, a2))))}
    val mem_inversion :
      ('a : immutable_data).
        (d : 'a mem) @ immutable ->
        (a1 : 'a) @ immutable ->
        (a2 : 'a list) @ immutable ->
        {u : unit
          | (mem d a1 a2) ===
              (match d with
               | Here (x', xs') -> (a1, a2) === (x', (x' :: xs'))
               | There (x, y, xs, p) ->
                   ((a1, a2) === (x, (y :: xs))) && (mem p x xs))} @ ghost
  end
|}]

let (head_le @ total) d x y rest :
    {u : unit | if ordered d (x :: y :: rest) then x <= y else true} @ ghost =
  ghost_ (ordered_inversion d (x :: y :: rest));;
[%%expect{|
val head_le :
  (d : Lists.ordered) ->
  (x : int) ->
  (y : int) ->
  (rest : int list) ->
  {u : unit | if Lists.ordered d (x :: y :: rest) then x <= y else true} @ ghost =
  <fun>
|}]

let (not_in_empty @ total) (d : int mem) (x : int) :
    {u : unit | not (mem d x [])} @ ghost =
  ghost_ (mem_inversion d x []);;
[%%expect{|
val not_in_empty :
  (d : int Lists.mem) ->
  (x : int) -> {u : unit | not (Lists.mem d x [])} @ ghost = <fun>
|}]

(* Names that a rule binds are avoided. *)
module Primed = struct
  type r = R of int * int [@@relation function R (d, a1) -> (d, a1)]
end;;
[%%expect{|
module Primed :
  sig
    type r = R of int * int
    [@@inductive]
    val r_concl : r @ immutable -> int * int
    val r_concl_def :
      (d' : r) @ immutable ->
      {u : unit | (r_concl d') === (match d' with | R (d, a1) -> (d, a1))}
    val r_valid : r @ total immutable -> bool @ ghost
    val r_valid_def :
      (d' : r) @ immutable ->
      {u : unit
        | (r_valid d') === (ghost_ (match d' with | R (d, a1) -> true))}
    val r :
      r @ total immutable ->
      int @ total immutable -> int @ total immutable -> bool @ ghost
    val r_def :
      (d' : r) @ immutable ->
      (a1' : int) @ immutable ->
      (a2 : int) @ immutable ->
      {u : unit
        | (r d' a1' a2) ===
            (ghost_ ((r_valid d') && ((r_concl d') === (a1', a2))))}
    val r_inversion :
      (d' : r) @ immutable ->
      (a1' : int) @ immutable ->
      (a2 : int) @ immutable ->
      {u : unit
        | (r d' a1' a2) ===
            (match d' with | R (d, a1) -> (a1', a2) === (d, a1))} @ ghost
  end
|}]

(* Malformed declarations. *)
type missing = A | B of int [@@relation function A -> 0];;
[%%expect{|
Line 1, characters 28-56:
1 | type missing = A | B of int [@@relation function A -> 0];;
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The constructor B has no rule
|}]

type twice = C of int [@@relation function C x -> x | C y -> y];;
[%%expect{|
Line 1, characters 22-63:
1 | type twice = C of int [@@relation function C x -> x | C y -> y];;
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The constructor C has several rules
|}]

type nested = N of int option [@@relation function N (Some x) -> x];;
[%%expect{|
Line 1, characters 51-61:
1 | type nested = N of int option [@@relation function N (Some x) -> x];;
                                                       ^^^^^^^^^^
Error: A rule matches one constructor of nested and binds each of its fields to a variable or _
|}]

type not_a_function = E [@@relation 0];;
[%%expect{|
Line 1, characters 24-38:
1 | type not_a_function = E [@@relation 0];;
                            ^^^^^^^^^^^^^^
Error: The relation attribute requires a function with one case per rule
|}]

type arity = F of int * arity
[@@relation function F (x, p) when arity p -> x];;
[%%expect{|
Line 2, characters 35-42:
2 | [@@relation function F (x, p) when arity p -> x];;
                                       ^^^^^^^
Error: The relation arity takes a derivation and one argument
|}]

type record = { field : int } [@@relation function x -> x];;
[%%expect{|
Line 1, characters 0-58:
1 | type record = { field : int } [@@relation function x -> x];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A relation must be a variant type
|}]

(* A premise must be about a sub-derivation. *)
module Bad = struct
  type bad = G of int | H of int
  [@@relation function G x -> x | H x when bad (G x) x -> x]
end;;
[%%expect{|
Line 3, characters 43-54:
3 |   [@@relation function G x -> x | H x when bad (G x) x -> x]
                                               ^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]

module type S = sig
  type s = S of int [@@relation function S x -> x]
end;;
[%%expect{|
Line 2, characters 20-50:
2 |   type s = S of int [@@relation function S x -> x]
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: A relation must be declared in a structure; a signature lists its generated values
|}]
