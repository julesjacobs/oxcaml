(* TEST
 include stdlib_stable;
 flags = "-extension layouts_beta";
 expect;
*)

type positive = { field : int{ _ > 0 } }
[%%expect {|
type positive = { field : int{ _ > 0 }; }
|}]

let direct_layout f = Stdlib_stable.Int8_u.to_int (f ())
[%%expect {|
val direct_layout : (unit -> int8#) -> int = <fun>
|}]

let ordinary_layout f =
  let result = f () in
  Stdlib_stable.Int8_u.to_int result
[%%expect {|
val ordinary_layout : (unit -> int8#) -> int = <fun>
|}]

let rec bottom () : 'a = bottom ()
let require_positive (_ : int{ _ > 0 }) = ()
let outer_bottom (_ : unit) : 'a = bottom ()
[%%expect {|
val bottom : unit -> 'a = <fun>
val require_positive : int{ _ > 0 } -> unit = <fun>
val outer_bottom : unit -> 'a = <fun>
|}]

(* Typing the inner polymorphic application temporarily replaces the
   contextual-imposition head.  Restoring the outer head retains both
   applications' bottom-result contracts. *)
let nested_contextual_bottom () =
  { field =
      outer_bottom (require_positive ((bottom () : int{ _ > 0 }))) }
[%%expect {|
val nested_contextual_bottom : unit -> positive = <fun>
|}]

let identity x = x
[%%expect {|
val identity : 'a -> 'a = <fun>
|}]

let nested_contextual_bad () =
  { field =
      outer_bottom (require_positive ((identity 0 : int{ _ > 0 }))) }
[%%expect {|
Line 3, characters 37-66:
3 |       outer_bottom (require_positive ((identity 0 : int{ _ > 0 }))) }
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement verification failed (not-proved)
|}]

(* Inferring the first parameter's refinement reconstructs its arrow.  The
   second parameter and result must retain their shared unboxed layout. *)
let refine_first x (type a : bits8) (y : a) =
  ignore { field = x };
  y
[%%expect {|
val refine_first : ('a : bits8). int{ _ > 0 } -> 'a -> 'a = <fun>
|}]

let consume_refined_first (x : int{ _ > 0 }) f =
  let result = refine_first x (f ()) in
  Stdlib_stable.Int8_u.to_int result
[%%expect {|
val consume_refined_first : int{ _ > 0 } -> (unit -> int8#) -> int = <fun>
|}]
