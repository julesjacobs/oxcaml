(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Structural = struct
  type chain = Stop | Next of chain [@@inductive]

  let rec (proof @ total) : (xs : chain) @ immutable -> (q : int) ->
      {u : unit | true} @ ghost = fun xs q -> ghost_ (
    match xs with
    | Stop -> let u = () in refine_ u
    | Next rest ->
      let callback : (x : int) -> {u : unit | true} @ total =
        fun x -> proof rest x in
      callback q)

  let rec (length @ total) xs =
    match xs with
    | Stop -> 0
    | Next rest ->
      let callback = fun () -> length rest in
      callback () + callback () + 1

  let rec (returned @ total) xs =
    let callback : (int -> int) @ total = fun offset ->
      match xs with
      | Stop -> offset
      | Next rest -> (returned rest) offset + 1 in
    callback
end;;
[%%expect{|
module Structural :
  sig
    type chain = Stop | Next of chain
    [@@inductive]
    val proof : chain @ immutable -> int -> {u : unit | true} @ ghost
    val length : chain -> int
    val returned : chain @ total -> int -> int
  end
|}]

let result =
  let xs = Structural.Next (Structural.Next Structural.Stop) in
  Structural.length xs, (Structural.returned xs) 10;;
[%%expect{|
val result : int * int = (3, 12)
|}]

module Measured = struct
  let rec (countdown @ total) n =
    if n > 0 then
      let later : (unit -> int) @ total = fun () -> countdown (n - 1) in
      later ()
    else 0
  [@@decreases n]

  let rec (returned @ total) (n : int) =
    let later : (unit -> int) @ total = fun () ->
      if n > 0 then (returned (n - 1)) () else 0 in
    later
  [@@decreases n]

  let rec (bounded @ total) (n : int) =
    if n > 0 then
      let later : ({m : int | 0 <= m && m < n} -> int) @ total =
        fun smaller ->
          let refine_ m = smaller in
          bounded m in
      let m = n - 1 in later (refine_ m)
    else 0
  [@@decreases n]
end;;
[%%expect{|
module Measured :
  sig
    val countdown : int @ total -> int
    val returned : int -> unit -> int
    val bounded : int -> int
  end
|}]

let result = Measured.countdown 20, (Measured.returned 20) (), Measured.bounded 20;;
[%%expect{|
val result : int * int * int = (0, 0, 0)
|}]

module Same_structural = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (loop @ total) (xs : chain) =
    let later : (unit -> int) @ total = fun () -> loop xs in
    later ()
end;;
[%%expect{|
Line 4, characters 50-57:
4 |     let later : (unit -> int) @ total = fun () -> loop xs in
                                                      ^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]

module Fresh_parameter = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (loop @ total) xs =
    match xs with
    | Stop -> 0
    | Next rest ->
      let later : (chain @ immutable -> int) @ total = fun rest -> loop rest in
      later rest
end;;
[%%expect{|
Line 7, characters 67-76:
7 |       let later : (chain @ immutable -> int) @ total = fun rest -> loop rest in
                                                                       ^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]

module Same_measure = struct
  let rec (loop @ total) n =
    let later : (unit -> int) @ total = fun () -> loop n in
    later ()
  [@@decreases n]
end;;
[%%expect{|
Line 3, characters 50-56:
3 |     let later : (unit -> int) @ total = fun () -> loop n in
                                                      ^^^^^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 15-16:
5 |   [@@decreases n]
                   ^
  Required by this decreases attribute
|}]

module Arbitrary_measure = struct
  let rec (loop @ total) n =
    let later : (int -> int) @ total = fun m -> loop m in
    later n
  [@@decreases n]
end;;
[%%expect{|
Line 3, characters 48-54:
3 |     let later : (int -> int) @ total = fun m -> loop m in
                                                    ^^^^^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 15-16:
5 |   [@@decreases n]
                   ^
  Required by this decreases attribute
|}]

module Escaped = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (loop @ total) xs =
    match xs with
    | Stop -> 0
    | Next rest ->
      let later = fun () -> let alias = loop in alias rest in
      later ()
end;;
[%%expect{|
Line 7, characters 40-44:
7 |       let later = fun () -> let alias = loop in alias rest in
                                            ^^^^
Error: This recursive function cannot be total: the recursive function must be called directly.
|}]

module Partial_application = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (loop @ total) xs q =
    match xs with
    | Stop -> q
    | Next rest ->
      let later = fun () -> let apply = loop rest in apply q in
      later ()
end;;
[%%expect{|
Line 7, characters 40-49:
7 |       let later = fun () -> let apply = loop rest in apply q in
                                            ^^^^^^^^^
Error: This recursive function cannot be total: recursive calls must supply every value parameter.
|}]

module Lazy_body = struct
  type chain = Stop | Next of chain [@@inductive]
  let rec (loop @ total) xs =
    match xs with
    | Stop -> 0
    | Next rest ->
      let later = lazy (loop rest) in Lazy.force later
end;;
[%%expect{|
Line 7, characters 18-34:
7 |       let later = lazy (loop rest) in Lazy.force later
                      ^^^^^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 3-7, characters 25-54
         which is expected to be "total".
|}]
