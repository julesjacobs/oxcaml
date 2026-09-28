(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A recursive function whose later parameter or result annotation mentions an
   earlier parameter. The function's type is known before its body is
   checked, so the earlier parameter is a dependent binder, while a later
   parameter that nothing mentions is not. The result annotation is given in
   terms of the binders and the expected result in terms of the parameters;
   both are compared with the parameters mapped to the binders. *)

let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
  if r >= n then r else go n (r + 1);;
[%%expect{|
val go : (n : int) -> {r : int | r <= n} -> {s : int | s <= n} = <fun>
|}]

let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
  if r >= n then r else go (n + 0) (r + 1);;
[%%expect{|
val go : (n : int) -> {r : int | r <= n} -> {s : int | s <= n} = <fun>
|}]

(* The recursive call substitutes its own arguments into the result. *)
let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
  if n <= r then r else go (n - 1) r;;
[%%expect{|
val go : (n : int) -> {r : int | r <= n} -> {s : int | s <= n} = <fun>
|}]

let rec go (n : int) (xs : int list) : {s : int | s <= n} =
  match xs with [] -> n | _ :: rest -> go n rest;;
[%%expect{|
val go : (n : int) -> int list -> {s : int | s <= n} = <fun>
|}]

let rec go (n : int) (a : int) (r : {r : int | r <= n})
    : {s : int | s <= n} =
  if r >= n then r else go n a (r + 1);;
[%%expect{|
val go : (n : int) -> int -> {r : int | r <= n} -> {s : int | s <= n} = <fun>
|}]

let rec go (n : int) (r : {r : int | r <= n}) (q : int)
    : {s : int | s <= n + q} =
  if r >= n then n + q else go n (r + 1) q;;
[%%expect{|
val go :
  (n : int) -> {r : int | r <= n} -> (q : int) -> {s : int | s <= (n + q)} =
  <fun>
|}]

let rec (go @ total) (n : int) (r : {r : int | 0 <= r && r <= n})
    : {s : int | s <= n} =
  if r >= n then r else go n (r + 1)
[@@decreases n - r];;
[%%expect{|
val go : (n : int) -> {r : int | (0 <= r) && (r <= n)} -> {s : int | s <= n} =
  <fun>
|}]

module Local = struct
  let f (m : int) : {s : int | s <= m} =
    let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
      if r >= n then r else go n (r + 1)
    in
    go m m
end;;
[%%expect{|
module Local : sig val f : (m : int) -> {s : int | s <= m} end
|}]

module Mutual = struct
  let rec even (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
    if r >= n then r else odd n (r + 1)
  and odd (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
    if r >= n then r else even n (r + 1)
end;;
[%%expect{|
module Mutual :
  sig
    val even : (n : int) -> {r : int | r <= n} -> {s : int | s <= n}
    val odd : (n : int) -> {r : int | r <= n} -> {s : int | s <= n}
  end
|}]

module Partial = struct
  let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
    if r >= n then r else let g = go n in g (r + 1)
end;;
[%%expect{|
module Partial :
  sig val go : (n : int) -> {r : int | r <= n} -> {s : int | s <= n} end
|}]

module type Bounded = sig
  val go : (n : int) -> int list -> {s : int | s <= n}
end;;
[%%expect{|
module type Bounded =
  sig val go : (n : int) -> int list -> {s : int | s <= n} end
|}]

module Functor (X : sig end) : Bounded = struct
  let rec go (n : int) (xs : int list) : {s : int | s <= n} =
    match xs with [] -> n | _ :: rest -> go n rest
end;;
[%%expect{|
module Functor : functor (X : sig end) -> Bounded
|}]

type chain = Stop | Next of chain [@@inductive];;
[%%expect{|
type chain = Stop | Next of chain [@@inductive]
|}]

module Total_structural = struct
  let rec (len @ total) (base : int) (xs : chain) : {s : int | s >= base} =
    match xs with
    | Stop -> base
    | Next rest -> let refine_ k = len base rest in refine_ k
end;;
[%%expect{|
module Total_structural :
  sig val len : (base : int) -> chain -> {s : int | s >= base} end
|}]

(* The first parameter mentioned only by the result, including a
   function-typed one. *)

let rec go (n : int) (i : int) : {x : int | x >= n} =
  if i >= n then i else go n (i + 1);;
[%%expect{|
val go : (n : int) -> int -> {x : int | x >= n} = <fun>
|}]

let rec search (p : int -> bool) (i : int) : {x : int | p x} =
  if p i then i else search p (i + 1);;
[%%expect{|
val search : (p : (int -> bool)) -> int -> {x : int | p x} = <fun>
|}]

(* The same comparison when the dependent type comes from an annotation,
   including across a locally abstract type. *)

let go : (n : int) -> int -> {s : int | s <= n} =
  fun n (x : int) : {s : int | s <= n} -> n;;
[%%expect{|
val go : (n : int) -> int -> {s : int | s <= n} = <fun>
|}]

let go : (n : int) -> 'a -> {s : int | s <= n} =
  fun n (type a) (x : a) : {s : int | s <= n} -> n;;
[%%expect{|
val go : (n : int) -> 'a -> {s : int | s <= n} = <fun>
|}]

let go : (n : int) -> 'a -> {s : int | s < n} =
  fun n (type a) (x : a) : {s : int | s < n} -> n;;
[%%expect{|
Line 2, characters 48-49:
2 |   fun n (type a) (x : a) : {s : int | s < n} -> n;;
                                                    ^
Error: Refinement could not be proved (counterexample: n = 0)
Line 2, characters 38-43:
2 |   fun n (type a) (x : a) : {s : int | s < n} -> n;;
                                          ^^^^^
  The refinement is stated here.
|}]

let go : (n : int) -> 'a -> {s : int | s <= n} =
  fun n (type a) (x : a) : {s : int | s <= n + 1} -> n;;
[%%expect{|
Line 2, characters 8-54:
2 |   fun n (type a) (x : a) : {s : int | s <= n + 1} -> n;;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a -> {s : int | s <= (n + 1)}"
       but an expression was expected of type "'a -> {s : int | s <= n}"
       Type "{s : int | s <= (n + 1)}" is not compatible with type
         "{s : int | s <= n}"
|}]

(* The refinements of the recursive call are still checked. *)

let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
  if r >= n then r else go n (r + 2);;
[%%expect{|
Line 2, characters 29-36:
2 |   if r >= n then r else go n (r + 2);;
                                 ^^^^^^^
Error: Refinement could not be proved (counterexample: r = -1, n = 0)
Line 1, characters 37-43:
1 | let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s <= n} =
                                         ^^^^^^
  The refinement is stated here.
|}]

let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s < n} =
  if r >= n then r else go n (r + 1);;
[%%expect{|
Line 2, characters 17-18:
2 |   if r >= n then r else go n (r + 1);;
                     ^
Error: Refinement could not be proved (counterexample: r = 0, n = 0)
Line 1, characters 59-64:
1 | let rec go (n : int) (r : {r : int | r <= n}) : {s : int | s < n} =
                                                               ^^^^^
  The refinement is stated here.
|}]

let rec go (n : int) (xs : int list) : {s : int | s <= n} =
  match xs with [] -> n | _ :: rest -> go (n + 1) rest;;
[%%expect{|
Line 2, characters 39-54:
2 |   match xs with [] -> n | _ :: rest -> go (n + 1) rest;;
                                           ^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 50-56:
1 | let rec go (n : int) (xs : int list) : {s : int | s <= n} =
                                                      ^^^^^^
  The refinement is stated here.
|}]

let rec go (n : int) (m : int) (r : {r : int | r <= n})
    : {s : int | s <= n} =
  if r >= n then r else go m m (r + 1);;
[%%expect{|
Line 3, characters 31-38:
3 |   if r >= n then r else go m m (r + 1);;
                                   ^^^^^^^
Error: Refinement could not be proved (counterexample: r = 0, n = 1, m = 0)
Line 1, characters 47-53:
1 | let rec go (n : int) (m : int) (r : {r : int | r <= n})
                                                   ^^^^^^
  The refinement is stated here.
|}]

let rec search (p : int -> bool) (i : int) : {x : int | p x && x = i} =
  if p i then i else search p (i + 1);;
[%%expect{|
Line 2, characters 21-37:
2 |   if p i then i else search p (i + 1);;
                         ^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: i = 0)
Line 1, characters 63-68:
1 | let rec search (p : int -> bool) (i : int) : {x : int | p x && x = i} =
                                                                   ^^^^^
  The refinement is stated here.
|}]

let rec search (p : (int -> bool) @ total) (q : (int -> bool) @ total)
    (i : int) : {x : int | p x} =
  if p i then i else search q p (i + 1);;
[%%expect{|
Line 3, characters 21-39:
3 |   if p i then i else search q p (i + 1);;
                         ^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample: i = 0)
Line 2, characters 27-30:
2 |     (i : int) : {x : int | p x} =
                               ^^^
  The refinement is stated here.
|}]
