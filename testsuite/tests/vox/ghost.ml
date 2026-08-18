(* TEST
 expect;
*)

(* The ghostliness axis: Real < Ghost, legacy Real. Ghostliness is an
   information-flow property of the mode system, with no effect on the ABI:
   a ghost value may only be used where a ghost value is expected, and
   [ghost_ e] deletes the evaluation of [e], producing a placeholder that
   the discipline guarantees is never read. *)

(* Defaults: unannotated code is real and prints as today. *)
let id x = x
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

(* A real value is accepted where ghost is expected. *)
let g (x : int @ ghost) = ()
let () = g 5
[%%expect{|
val g : int @ ghost -> unit = <fun>
|}]

(* A ghost value is rejected where real is expected. *)
let bad () =
  let x = ghost_ 5 in
  x + 1
[%%expect{|
Line 3, characters 2-3:
3 |   x + 1
      ^
Error: This value is "ghost" but is expected to be "real".
|}]

(* A ghost value returned from a real function. *)
let ret x = ghost_ (x + 1)
[%%expect{|
val ret : int -> int @ ghost = <fun>
|}]

(* Ghost values are rejected as if conditions and match scrutinees in
   real code... *)
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

(* ...and accepted inside ghost_. *)
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

(* Binding patterns do not read: variables, wildcards and aliases are fine,
   destructuring is not. *)
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

(* Application: a ghost function cannot be called... *)
let bad () =
  let f = ghost_ (fun x -> x + 1) in
  f 3
[%%expect{|
Line 3, characters 2-3:
3 |   f 3
      ^
Error: This value is "ghost" but is expected to be "real".
|}]

(* ...but ghost_ (f x) is fine for the same f. *)
let ok () =
  let f = ghost_ (fun x -> x + 1) in
  ghost_ (f 3)
[%%expect{|
val ok : unit -> int @ ghost = <fun>
|}]

(* A ghost parameter and a ghost function are independent. *)
let takes_ghost_param (x : int @ ghost) (y : int) = y
let apply_it (f : (int @ ghost -> int -> int)) x y = f x y
[%%expect{|
val takes_ghost_param : int @ ghost -> int -> int = <fun>
val apply_it : (int @ ghost -> int -> int) -> int -> int -> int = <fun>
|}]

(* Arguments are not ghost silently: a real argument is usable at an
   ghost parameter (ordinary submoding; it is passed like any argument, the
   callee just cannot read it), and a ghost argument passes a
   placeholder. *)
let call () =
  let x = ghost_ 5 in
  takes_ghost_param x 1 + takes_ghost_param 42 2
[%%expect{|
val call : unit -> int = <fun>
|}]

(* Closures: a real closure may capture a ghost value and stays
   callable. *)
let f (u : unit) (z : int @ ghost) = ()
let mk () =
  let x = ghost_ 42 in
  let clo = fun y -> f y x in
  clo ()
[%%expect{|
val f : unit -> int @ ghost -> unit = <fun>
val mk : unit -> unit = <fun>
|}]

(* The same closure is rejected when it uses the capture at a real
   position. *)
let bad () =
  let x = ghost_ 42 in
  fun y -> x + y
[%%expect{|
Line 3, characters 11-12:
3 |   fun y -> x + y
               ^
Error: This value is "ghost" but is expected to be "real".
|}]

(* The body rule: ghost_ over a lambda makes the body a ghost context. *)
let ok () =
  let g = ghost_ (fun (y : int) -> y + 1) in
  let _k = ghost_ (fun y -> g y) in
  ()
[%%expect{|
val ok : unit -> unit = <fun>
|}]

(* A ghost closure may capture a real value. *)
let ok (r : int) =
  let _k = ghost_ (fun y -> r + y) in
  ()
[%%expect{|
val ok : int -> unit = <fun>
|}]

(* Partial application across a ghost parameter does not erase the
   result. *)
let use () =
  let x = ghost_ 5 in
  let h = takes_ghost_param x in
  h 3
[%%expect{|
val use : unit -> int = <fun>
|}]

(* Inference direction: modes zap to legacy, so an unannotated binding cannot
   drift to Ghost and vanish. *)
let quiet = 5
let quiet_use (h : int -> int) = h quiet
[%%expect{|
val quiet : int = 5
val quiet_use : (int -> int) -> int = <fun>
|}]

(* A structure cannot hold a ghost binding: nothing would exist to put in
   the module block. *)
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

(* Ghostliness does not cross: even immediates stay ghost. If int crossed
   ghostliness this would be accepted. *)
let bad (x : int @ ghost) = x * 2
[%%expect{|
Line 1, characters 28-29:
1 | let bad (x : int @ ghost) = x * 2
                                ^
Error: This value is "ghost" but is expected to be "real".
|}]

(* A ghost result may be returned from a real function: return position
   is covariant. *)
let ok (x : int @ ghost) : int = x
[%%expect{|
val ok : int @ ghost -> int @ ghost = <fun>
|}]

(* mod ghost is not a thing: types cannot cross ghostliness. *)
type t : value mod ghost
[%%expect{|
Line 1, characters 19-24:
1 | type t : value mod ghost
                       ^^^^^
Error: Unrecognized modifier ghost.
|}]

(* Sealing, return position: a real-returning implementation is accepted
   against a ghost-returning signature, the reverse is rejected. *)
module Ok : sig
  val f : int -> int @ ghost
end = struct
  let f x = x + 1
  let _force = f 0 + 1 (* really real: the result is used *)
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
         sig val f : int -> int @ ghost end
       is not included in
         sig val f : int -> int end
       Values do not match:
         val f : int -> int @ ghost
       is not included in
         val f : int -> int
       The type "int -> int @ ghost" is not compatible with the type "int -> int"
|}]

(* Sealing, argument position: ordinary contravariance. Ghostliness has no ABI
   effect, so a ghost-parameter implementation may hide behind a
   real-parameter signature: callers pass real values, and the
   implementation is free to ignore them. *)
module Ok_contra : sig
  val f : int -> unit
end = struct
  let f (x : int @ ghost) = ()
end
[%%expect{|
module Ok_contra : sig val f : int -> unit end
|}]

(* The reverse is rejected: the signature promises callers may pass ghost
   values, but the implementation reads its argument. *)
module Bad_rev : sig
  val f : int @ ghost -> unit
end = struct
  let r = ref 0
  let f (x : int) = r := x
end
[%%expect{|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let r = ref 0
5 |   let f (x : int) = r := x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val r : int ref val f : int -> unit end
       is not included in
         sig val f : int @ ghost -> unit end
       Values do not match:
         val f : int -> unit
       is not included in
         val f : int @ ghost -> unit
       The type "int -> unit" is not compatible with the type
         "int @ ghost -> unit"
|}]

(* The same directions through an explicit coercion. *)
let ok (f : (int @ ghost -> unit)) = (f :> int -> unit)
[%%expect{|
val ok : (int @ ghost -> unit) -> int -> unit = <fun>
|}]

let bad (f : int -> unit) = (f :> (int @ ghost -> unit))
[%%expect{|
Line 1, characters 28-56:
1 | let bad (f : int -> unit) = (f :> (int @ ghost -> unit))
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int -> unit" is not a subtype of "int @ ghost -> unit"
|}]

(* Return position through coercion: real-returning to ghost-returning
   is accepted, the reverse rejected. *)
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

(* All read positions require real: while and for (found in review), and
   the ones below pin assert, guards and comprehensions against reverting
   mode_max's ghostliness component. *)
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
Line 1, characters 34-57:
1 | let bad () = let hi = ghost_ 3 in [| x for x = 0 to hi |]
                                      ^^^^^^^^^^^^^^^^^^^^^^^
Error: The extension "comprehensions" is disabled and cannot be used
|}]

(* Reading or writing a field is a runtime access of the record. *)
type m = { mutable v : int }
let bad () = let r = ghost_ { v = 1 } in r.v
[%%expect{|
type m = { mutable v : int; }
Line 2, characters 41-42:
2 | let bad () = let r = ghost_ { v = 1 } in r.v
                                             ^
Error: This value is "ghost" but is expected to be "real".
|}]

let bad () = let r = ghost_ { v = 1 } in r.v <- 2
[%%expect{|
Line 1, characters 41-42:
1 | let bad () = let r = ghost_ { v = 1 } in r.v <- 2
                                             ^
Error: This value is "ghost" but is expected to be "real".
|}]

(* Statement position discards the value, so ghost is fine there. *)
let ok () = (ghost_ (print_string "gone")); 1
[%%expect{|
val ok : unit -> int = <fun>
|}]

(* The ghost context is compositional: any ghost expression may be read
   inside ghost_, not just variables. *)
let ret (_x : int) : bool @ ghost = ghost_ true
let a () = ghost_ (if ret 1 then 1 else 2)
let b () = ghost_ (if (ghost_ true) then 1 else 2)
[%%expect{|
val ret : int -> bool @ ghost = <fun>
val a : unit -> int @ ghost = <fun>
val b : unit -> int @ ghost = <fun>
|}]

(* Structural ghostliness: a tuple with a ghost component is itself ghost, so
   it is usable at a ghost position and rejected at a real one. *)
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

(* Contravariance composes through arrows nested in argument position: a
   callback that tolerates ghost arguments may be used where one taking
   real arguments is expected. *)
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

(* Externals may declare ghost parameters: the argument is passed
   physically like any other, and the mode only constrains OCaml-side
   uses. *)
external sink : int @ ghost -> unit = "sink"
[%%expect{|
external sink : int @ ghost -> unit = "sink"
|}]

(* The @@ ghost field modality; ghost_fields.ml is the real coverage. *)
type r = { x : int @@ ghost; y : int }
[%%expect{|
type r = { x : int @@ ghost; y : int; }
|}]
