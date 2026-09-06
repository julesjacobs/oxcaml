(* TEST
 flags = "-extension comprehensions";
 expect;
*)

let id x = x
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

let g (x : int @ ghost) = ()
let () = g 5
[%%expect{|
val g : int @ ghost -> unit = <fun>
|}]

let bad () =
  let x = ghost_ 5 in
  x + 1
[%%expect{|
Line 3, characters 2-3:
3 |   x + 1
      ^
Error: This value is "ghost" but is expected to be "real".
|}]

let ret x = ghost_ (x + 1)
[%%expect{|
val ret : int @ total -> int @ ghost = <fun>
|}]

let bad () =
  let b = ghost_ true in
  if b then 1 else 2
[%%expect{|
Line 3, characters 5-6:
3 |   if b then 1 else 2
         ^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad () =
  let b = ghost_ true in
  match b with true -> 1 | false -> 2
[%%expect{|
Line 3, characters 15-19:
3 |   match b with true -> 1 | false -> 2
                   ^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

let ok () =
  let b = ghost_ true in
  ghost_ (if b then 1 else 2)
[%%expect{|
val ok : unit -> int @ ghost = <fun>
|}]

let ok () =
  let b = ghost_ true in
  ghost_ (match b with true -> 1 | false -> 2)
[%%expect{|
val ok : unit -> int @ ghost = <fun>
|}]

let ok () =
  let p = ghost_ (1, 2) in
  let _q = p in
  ()
[%%expect{|
val ok : unit -> unit = <fun>
|}]

let bad () =
  let p = ghost_ (1, 2) in
  let (a, b) = p in
  a + b
[%%expect{|
Line 3, characters 15-16:
3 |   let (a, b) = p in
                   ^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad () =
  let f = ghost_ (fun x -> x + 1) in
  f 3
[%%expect{|
Line 3, characters 2-3:
3 |   f 3
      ^
Error: This value is "ghost" but is expected to be "real".
|}]

let ok () =
  let f = ghost_ (fun x -> x + 1) in
  ghost_ (f 3)
[%%expect{|
val ok : unit -> int @ ghost = <fun>
|}]

let takes_ghost_param (x : int @ ghost) (y : int) = y
let apply_it (f : (int @ ghost -> int -> int)) x y = f x y
[%%expect{|
val takes_ghost_param : int @ ghost -> int -> int = <fun>
val apply_it : (int @ ghost -> int -> int) -> int -> int -> int = <fun>
|}]

let call () =
  let x = ghost_ 5 in
  takes_ghost_param x 1 + takes_ghost_param 42 2
[%%expect{|
val call : unit -> int = <fun>
|}]

let f (u : unit) (z : int @ ghost) = ()
let mk () =
  let x = ghost_ 42 in
  let clo = fun y -> f y x in
  clo ()
[%%expect{|
val f : unit -> int @ ghost -> unit = <fun>
val mk : unit -> unit = <fun>
|}]

let bad () =
  let x = ghost_ 42 in
  fun y -> x + y
[%%expect{|
Line 3, characters 11-12:
3 |   fun y -> x + y
               ^
Error: This value is "ghost" but is expected to be "real".
|}]

let ok () =
  let g = ghost_ (fun (y : int) -> y + 1) in
  let _k = ghost_ (fun y -> g y) in
  ()
[%%expect{|
val ok : unit -> unit = <fun>
|}]

let ok (r : int) =
  let _k = ghost_ (fun y -> r + y) in
  ()
[%%expect{|
val ok : int -> unit = <fun>
|}]

let use () =
  let x = ghost_ 5 in
  let h = takes_ghost_param x in
  h 3
[%%expect{|
val use : unit -> int = <fun>
|}]

let quiet = 5
let quiet_use (h : int -> int) = h quiet
[%%expect{|
val quiet : int = 5
val quiet_use : (int -> int) -> int = <fun>
|}]

module M = struct
  let x = ghost_ 5
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x = ghost_ 5
          ^
Error: The expression is "ghost"
       but is expected to be "real"
         because it is the value "x" in the structure at line 2, characters 2-18
         which is expected to be "real"
         because modules always need to be allocated on the heap.
|}]

let bad (x : int @ ghost) = x * 2
[%%expect{|
Line 1, characters 28-29:
1 | let bad (x : int @ ghost) = x * 2
                                ^
Error: This value is "ghost" but is expected to be "real".
|}]

let ok (x : int @ ghost) : int = x
[%%expect{|
val ok : int @ ghost -> int @ ghost = <fun>
|}]

type t : value mod ghost
[%%expect{|
Line 1, characters 19-24:
1 | type t : value mod ghost
                       ^^^^^
Error: Unrecognized modifier ghost.
|}]

module Ok : sig
  val f : int -> int @ ghost
end = struct
  let f x = x + 1
  let _force = f 0 + 1 
end
[%%expect{|
module Ok : sig val f : int -> int @ ghost end
|}]

module Bad : sig
  val f : int -> int
end = struct
  let f x = ghost_ (x + 1)
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = ghost_ (x + 1)
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : int @ total -> int @ ghost end
       is not included in
         sig val f : int -> int end
       Values do not match:
         val f : int @ total -> int @ ghost
       is not included in
         val f : int -> int
       The type "int @ total -> int @ ghost" is not compatible with the type
         "int -> int"
|}]

let ok (f : int -> int) = (f :> (int -> int @ ghost))
[%%expect{|
val ok : (int -> int) -> int -> int @ ghost = <fun>
|}]

let bad (f : (int -> int @ ghost)) = (f :> int -> int)
[%%expect{|
Line 1, characters 37-54:
1 | let bad (f : (int -> int @ ghost)) = (f :> int -> int)
                                         ^^^^^^^^^^^^^^^^^
Error: Type "int -> int @ ghost" is not a subtype of "int -> int"
|}]

let bad () = let b = ghost_ true in while b do () done
[%%expect{|
Line 1, characters 42-43:
1 | let bad () = let b = ghost_ true in while b do () done
                                              ^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad () = let lo = ghost_ 0 in for _i = lo to 1 do () done
[%%expect{|
Line 1, characters 43-45:
1 | let bad () = let lo = ghost_ 0 in for _i = lo to 1 do () done
                                               ^^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad () = let b = ghost_ true in assert b
[%%expect{|
Line 1, characters 43-44:
1 | let bad () = let b = ghost_ true in assert b
                                               ^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad x = let b = ghost_ true in match x with _ when b -> 0 | _ -> 1
[%%expect{|
Line 1, characters 55-56:
1 | let bad x = let b = ghost_ true in match x with _ when b -> 0 | _ -> 1
                                                           ^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad () = let hi = ghost_ 3 in [| x for x = 0 to hi |]
[%%expect{|
Line 1, characters 52-54:
1 | let bad () = let hi = ghost_ 3 in [| x for x = 0 to hi |]
                                                        ^^
Error: This value is "ghost" but is expected to be "real".
|}]

type m = { mutable v : int }
let bad () = let r = ghost_ { v = 1 } in r.v
[%%expect{|
type m = { mutable v : int; }
Line 2, characters 28-37:
2 | let bad () = let r = ghost_ { v = 1 } in r.v
                                ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 21-37).
|}]

let bad () = let r = ghost_ { v = 1 } in r.v <- 2
[%%expect{|
Line 1, characters 28-37:
1 | let bad () = let r = ghost_ { v = 1 } in r.v <- 2
                                ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 21-37).
|}]

let rejected_effect () = (ghost_ (print_string "gone")); 1
[%%expect{|
Line 1, characters 34-46:
1 | let rejected_effect () = (ghost_ (print_string "gone")); 1
                                      ^^^^^^^^^^^^
Error: The value "print_string" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 25-55).
|}]

module Returns = struct
  let (ret @ total) (_x : int) : bool @ ghost = ghost_ true
  let a () = ghost_ (if ret 1 then 1 else 2)
  let b () = ghost_ (if (ghost_ true) then 1 else 2)
end
[%%expect{|
module Returns :
  sig
    val ret : int -> bool @ ghost
    val a : unit -> int @ ghost
    val b : unit -> int @ ghost
  end
|}]

let use (p : (int * int) @ ghost) = 0
let ok (y : int @ ghost) = use (y, 1)
let bad (y : int @ ghost) = fst (y, 1)
[%%expect{|
val use : int * int @ ghost -> int = <fun>
val ok : int @ ghost -> int = <fun>
Line 3, characters 33-34:
3 | let bad (y : int @ ghost) = fst (y, 1)
                                     ^
Error: This value is "ghost"
       but is expected to be "real"
         because it is an element of the tuple at line 3, characters 32-38
         which is expected to be "real".
|}]

module Ok_nested : sig
  val g : (int @ ghost -> unit) -> unit
end = struct
  let g (f : int -> unit) = f 1
end
[%%expect{|
module Ok_nested : sig val g : (int @ ghost -> unit) -> unit end
|}]

module Bad_nested_rev : sig
  val g : (int -> unit) -> unit
end = struct
  let g (f : int @ ghost -> unit) = ()
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let g (f : int @ ghost -> unit) = ()
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : (int @ ghost -> unit) -> unit end
       is not included in
         sig val g : (int -> unit) -> unit end
       Values do not match:
         val g : (int @ ghost -> unit) -> unit
       is not included in
         val g : (int -> unit) -> unit
       The type "(int @ ghost -> unit) -> unit" is not compatible with the type
         "(int -> unit) -> unit"
       Type "int @ ghost -> unit" is not compatible with type "int -> unit"
|}]

type r = { x : int @@ ghost; y : int }
[%%expect{|
type r = { x : int @@ ghost; y : int; }
|}]

let f () : float# = ghost_ #1.0
[%%expect{|
val f : unit -> float# @ ghost = <fun>
|}]
