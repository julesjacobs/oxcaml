(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* A closure's totality lock constrains only the values it captures. A local
   recursive function is defined inside the enclosing closure, so a call to
   it crosses no lock. Recursion that is not shown to terminate is therefore
   a partial construct, like a loop: it makes every enclosing closure
   partial. Without this, [bad] was accepted as total, and
   [ghost_ (bad 0)] let a program print "verified to be 2: 1". *)

let (bad @ total) (x : int) : {u : unit | false} =
  let rec go (k : int) : {u : unit | false} = go k in
  go x
[%%expect{|
Line 2, characters 2-50:
2 |   let rec go (k : int) : {u : unit | false} = go k in
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 18-6
         which is expected to be "total".
Line 2, characters 13-50:
2 |   let rec go (k : int) : {u : unit | false} = go k in
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* An unannotated function that loops through a local [let rec] is inferred
   partial, not total. *)
let loops (x : int) = let rec go (k : int) = go k in go x
let (use_loops @ total) () = loops 0
[%%expect{|
val loops : int -> 'a = <fun>
Line 2, characters 29-34:
2 | let (use_loops @ total) () = loops 0
                                 ^^^^^
Error: The value "loops" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 24-36
         which is expected to be "total".
|}]

(* An unused looping local function is rejected too, as [raise] is. *)
let (unused @ total) (x : int) = let rec go (k : int) : int = go k in x
[%%expect{|
Line 1, characters 33-66:
1 | let (unused @ total) (x : int) = let rec go (k : int) : int = go k in x
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 21-71
         which is expected to be "total".
Line 1, characters 44-66:
1 | let (unused @ total) (x : int) = let rec go (k : int) : int = go k in x
                                                ^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* Mutual recursion is never total. *)
let (mutual @ total) (x : int) : {u : unit | false} =
  let rec f (k : int) : {u : unit | false} = g k
  and g (k : int) : {u : unit | false} = f k in
  f x
[%%expect{|
Line 2, characters 2-48:
2 |   let rec f (k : int) : {u : unit | false} = g k
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-4, characters 21-5
         which is expected to be "total".
This recursive function is partial:
mutually recursive functions are never total.
|}]

(* A recursive value. *)
type r = {run : unit -> int}
let (knot @ total) (x : int) : int =
  let rec r = {run = fun () -> r.run ()} in
  r.run () + x
[%%expect{|
type r = { run : unit -> int; }
Line 3, characters 2-40:
3 |   let rec r = {run = fun () -> r.run ()} in
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-4, characters 19-14
         which is expected to be "total".
This recursive function is partial: recursive values are never total.
|}]

(* A local recursive function annotated partial. *)
let (annotated @ total) (x : int) : int =
  let rec (go @ partial) (k : int) : int = go k in
  go x
[%%expect{|
Line 2, characters 2-47:
2 |   let rec (go @ partial) (k : int) : int = go k in
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 24-6
         which is expected to be "total".
Line 2, characters 11-47:
2 |   let rec (go @ partial) (k : int) : int = go k in
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* Partial application, and passing the function to a total higher-order
   function, do not hide it. *)
let (apply @ total) (f : int -> int) (x : int) : int = f x
let (partially_applied @ total) (x : int) : int =
  let rec go (j : int) (k : int) : int = go j k in
  let h = go 1 in
  h x
[%%expect{|
val apply : (int -> int) -> int -> int = <fun>
Line 3, characters 2-47:
3 |   let rec go (j : int) (k : int) : int = go j k in
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-5, characters 32-5
         which is expected to be "total".
Line 3, characters 13-47:
3 |   let rec go (j : int) (k : int) : int = go j k in
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

let (passed @ total) (x : int) : int =
  let rec go (k : int) : int = go k in
  apply go x
[%%expect{|
Line 2, characters 2-35:
2 |   let rec go (k : int) : int = go k in
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 21-12
         which is expected to be "total".
Line 2, characters 13-35:
2 |   let rec go (k : int) : int = go k in
                 ^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* A call hidden in a [let*] body is not a checked call, so the recursion is
   partial. *)
let (( let* ) @ total) (x : int) (f : int -> int) = f x
let (letop @ total) (x : int) : int =
  let rec go (k : int) : int = let* y = k in go y in
  go x
[%%expect{|
val ( let* ) : int -> (int -> int) -> int = <fun>
Line 3, characters 2-49:
3 |   let rec go (k : int) : int = let* y = k in go y in
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-4, characters 20-6
         which is expected to be "total".
Line 3, characters 13-49:
3 |   let rec go (k : int) : int = let* y = k in go y in
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* Nested closures: every enclosing lock becomes partial. *)
let (nested @ total) (x : int) : int =
  let inner () = let rec go (k : int) : int = go k in go x in
  inner ()
[%%expect{|
Line 2, characters 17-50:
2 |   let inner () = let rec go (k : int) : int = go k in go x in
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 21-10
         which is expected to be "total".
Line 2, characters 28-50:
2 |   let inner () = let rec go (k : int) : int = go k in go x in
                                ^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* Ghost code must be total, whatever the enclosing function. *)
let in_ghost (x : int) : {u : unit | false} =
  ghost_ (let rec go (k : int) : {u : unit | false} = go k in go x)
[%%expect{|
Line 2, characters 10-58:
2 |   ghost_ (let rec go (k : int) : {u : unit | false} = go k in go x)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 2-67).
Line 2, characters 21-58:
2 |   ghost_ (let rec go (k : int) : {u : unit | false} = go k in go x)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* A lemma. *)
let (lemma @ total) (x : int) : {u : unit | false} @ ghost =
  ghost_ (let rec go (k : int) : {u : unit | false} = go k in go x)
[%%expect{|
Line 2, characters 10-58:
2 |   ghost_ (let rec go (k : int) : {u : unit | false} = go k in go x)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-2, characters 20-67
         which is expected to be "total".
Line 2, characters 21-58:
2 |   ghost_ (let rec go (k : int) : {u : unit | false} = go k in go x)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* A decreases measure is typed under a total lock. A looping measure with
   the result type [{r : int | false}] made any recursion "decrease". *)
let rec (spin @ total) (n : int) : {u : unit | false} = spin n
[@@decreases (let rec go (k : int) : {r : int | false} = go k in go n)]
[%%expect{|
Line 2, characters 14-61:
2 | [@@decreases (let rec go (k : int) : {r : int | false} = go k in go n)]
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The function is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 23-62
         which is expected to be "total".
Line 2, characters 25-61:
2 | [@@decreases (let rec go (k : int) : {r : int | false} = go k in go n)]
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This recursive function is partial:
no parameter has a checked inductive datatype.
|}]

(* Accepted: structural recursion, a decreases measure, and a function that
   does not call itself. *)
let (structural @ total) (xs : int list) : int =
  let rec length (ys : int list) : int =
    match ys with [] -> 0 | _ :: rest -> 1 + length rest
  in
  length xs

let (measured @ total) (n : int) : int =
  let rec count (k : int) : int = if k <= 0 then 0 else 1 + count (k - 1)
  [@@decreases k] in
  count n

let (not_recursive @ total) (n : int) : int =
  let rec id (k : int) : int = k in
  id n

let (ghost_structural @ total) (xs : int list) : {u : unit | true} @ ghost =
  ghost_ (
    let rec walk (ys : int list) : {u : unit | true} =
      match ys with [] -> () | _ :: rest -> walk rest
    in
    walk xs)
[%%expect{|
val structural : int list -> int = <fun>
val measured : int -> int = <fun>
val not_recursive : int -> int = <fun>
val ghost_structural : int list -> {u : unit | true} @ ghost = <fun>
|}]

(* A partial local recursive function in a function that need not be total
   is fine; the enclosing function is partial. *)
let search (x : int) : int =
  let rec go (k : int) : int = if k * k >= x then k else go (k + 1) in
  go 0
[%%expect{|
val search : int -> int = <fun>
|}]
