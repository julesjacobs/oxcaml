(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

module Within : sig
  val call : (unit -> 'a) @ local once total -> 'a @@ total
end = struct
  let (call @ total) (body : (unit -> 'a) @ local once total) = body ()
end;;
[%%expect{|
module Within :
  sig val call : (unit -> 'a) @ local once total -> 'a @@ total end
|}]

module Positive : sig
  val countdown : int -> int @@ total
  val list_length : 'a list @ total immutable -> int @@ total
end = struct
  let rec (countdown @ total) n =
    if n > 0 then Within.call (fun () -> countdown (n - 1)) else 0
  [@@decreases n]

  let rec (list_length @ total) values =
    match values with
    | [] -> 0
    | _ :: tail -> Within.call (fun () -> 1 + list_length tail)
end;;
[%%expect{|
module Positive :
  sig
    val countdown : int -> int @@ total
    val list_length : 'a list @ total immutable -> int @@ total
  end
|}]

let result = Positive.countdown 100, Positive.list_length [1; 2; 3];;
[%%expect{|
val result : int * int = (0, 3)
|}]

module Unchanged = struct
  let rec (loop @ total) n = Within.call (fun () -> loop n)
  [@@decreases n]
end;;
[%%expect{|
Line 2, characters 52-58:
2 |   let rec (loop @ total) n = Within.call (fun () -> loop n)
                                                        ^^^^^^
Error: Refinement could not be proved (counterexample)
Line 3, characters 15-16:
3 |   [@@decreases n]
                   ^
  Required by this decreases attribute
|}]

module Delayed = struct
  let rec (loop @ total) n =
    let later = fun () -> loop (n - 1) in
    later ()
  [@@decreases n]
end;;
[%%expect{|
Line 3, characters 26-30:
3 |     let later = fun () -> loop (n - 1) in
                              ^^^^
Error: the recursive function occurs in a delayed body
|}]

module Blocking = struct
  let partial body = while true do body () done
  let rec (loop @ total) n =
    if n > 0 then partial (fun () -> loop (n - 1)) else ()
  [@@decreases n]
end;;
[%%expect{|
Line 4, characters 18-25:
4 |     if n > 0 then partial (fun () -> loop (n - 1)) else ()
                      ^^^^^^^
Error: The value "partial" is "partial"
         because it closes over the loop at line 2, characters 21-47
         which is "partial".
       However, the value "partial" highlighted is expected to be "total"
         because it is used inside the function at lines 3-4, characters 25-58
         which is expected to be "total".
|}]

module Fix : sig
  val fix :
    ((n : int) ->
      (({m : int | m < n} -> 'a) @ total -> 'a) @ total) @ total ->
    (int -> 'a) @ total stateful @@ total
end = struct
  let rec (fix @ total) :
      (step : ((n : int) ->
        (({m : int | m < n} -> 'a) @ total -> 'a) @ total)) @ total ->
      (n : int) -> 'a = fun step n ->
    (step n) (fun smaller ->
      let refine_ m = smaller in
      fix step m)
  [@@decreases n]
end;;
[%%expect{|
module Fix :
  sig
    val fix :
      ((n : int) -> (({m : int | m < n} -> 'a) @ total -> 'a) @ total) @ total ->
      (int -> 'a) @ total stateful @@ total
  end
|}]

let result = (Fix.fix (fun n recurse ->
  if n > 0 then let next = n - 1 in recurse (refine_ next) else 0)) 100;;
[%%expect{|
val result : int = 0
|}]
