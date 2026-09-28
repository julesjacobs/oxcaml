(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* The mode side condition MC (DESIGN.md 2.1, 3.3.1): a value that enters a
   refined type must be total, stateless and portable at its position,
   because refined types cross those axes (typing/ikind.ml:30-52).  Dropping
   a refinement never needs it.  All three axes are checked ("total" implies
   the other two only as an annotation default, typing/typemode.ml:250-264).
   The failing axis named in each message is a prediction.  Each
   block is the output after stages 1-4; the comment says what
   trunk does today (always a plain type mismatch, since trunk cannot add a
   refinement at inclusion at all). *)

(* [stage 3] Root position: a partial closure exported at a refined type.
   The obligation "true" is trivial; the mode check rejects it.
   Currently: rejected, 'The type "int -> int" is not compatible with the type
   "{f : int -> int | true}"'. *)
module Partial_root : sig val h : {f : int -> int | true} end = struct
  let h (x : int) = if x > 0 then x else failwith "negative"
end;;
[%%expect{|
Lines 1-3, characters 64-3:
1 | ................................................................struct
2 |   let h (x : int) = if x > 0 then x else failwith "negative"
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val h : int -> int end @ partial
       is not included in
         sig val h : {f : int -> int | true} end @ partial
       Values do not match:
         val h : int -> int (* in a structure at partial *)
       is not included in
         val h : {f : int -> int | true} (* in a structure at partial *)
       The first is "partial"
         because it closes over the value "failwith" at line 2, characters 41-49
         which is "partial".
       However, the second is "total".
|}]

(* [stage 3] Root position: a nonportable closure.
   Currently: rejected, 'The type "int -> int" is not compatible with the type
   "{f : int -> int | true}"'. *)
module Nonportable_root : sig val h : {f : int -> int | true} end = struct
  let (h @ nonportable) = fun (x : int) -> x
end;;
[%%expect{|
Lines 1-3, characters 68-3:
1 | ....................................................................struct
2 |   let (h @ nonportable) = fun (x : int) -> x
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val h : int -> int end @ nonportable
       is not included in
         sig val h : {f : int -> int | true} end @ nonportable
       Values do not match:
         val h : int -> int (* in a structure at nonportable *)
       is not included in
         val h : {f : int -> int | true} (* in a structure at nonportable *)
       The first is "nonportable"
       but the second is "portable".
|}]

(* [stage 3] Result position: the returned closure is stateful and partial,
   and MC is checked against the arrow's return mode (r1 in DESIGN.md 2.2).
   Without MC this would export a stateful closure that clients (and the
   verifier) treat as a total function.
   Currently: rejected, 'Type "int -> int" is not compatible with type
   "{f : int -> int | true}"'. *)
module Stateful_result : sig val mk : unit -> {f : int -> int | true} end = struct
  let mk () = let r = ref 0 in fun (x : int) -> r := !r + x; !r
end;;
[%%expect{|
Lines 1-3, characters 76-3:
1 | ............................................................................struct
2 |   let mk () = let r = ref 0 in fun (x : int) -> r := !r + x; !r
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val mk : unit -> int -> int end
       is not included in
         sig val mk : unit -> {f : int -> int | true} end
       Values do not match:
         val mk : unit -> int -> int
       is not included in
         val mk : unit -> {f : int -> int | true}
       The type "unit -> int -> int" is not compatible with the type
         "unit -> {f : int -> int | true}"
       Type "int -> int" is not compatible with type "{f : int -> int | true}"
       The refined type "{f : int -> int | true}" requires values that are
       "total", "stateless" and "portable", but at this position they may be
       "partial".
|}]

(* [stage 3] Parameter position, contravariant add: the implementation
   assumes its callback is refined, but callers may pass any closure (the
   declared parameter mode a2 is legacy).
   Currently: rejected, 'Type "{f : int -> int | true}" is not compatible with
   type "int -> int"'. *)
module Refined_callback : sig val apply : (int -> int) -> int end = struct
  let apply (f : {f : int -> int | true}) = f 0
end;;
[%%expect{|
Lines 1-3, characters 68-3:
1 | ....................................................................struct
2 |   let apply (f : {f : int -> int | true}) = f 0
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val apply : {f : int -> int | true} -> int end
       is not included in
         sig val apply : (int -> int) -> int end
       Values do not match:
         val apply : {f : int -> int | true} -> int
       is not included in
         val apply : (int -> int) -> int
       The type "{f : int -> int | true} -> int"
       is not compatible with the type "(int -> int) -> int"
       Type "{f : int -> int | true}" is not compatible with type "int -> int"
       The refined type "{f : int -> int | true}" requires values that are
       "total", "stateless" and "portable", but at this position they may be
       "partial".
|}]

(* [stage 3] Under a covariant constructor the position mode is inherited
   from the value (DESIGN.md 2): the list is partial, so its elements are.
   Currently: rejected, 'Type "int -> int" is not compatible with type
   "{f : int -> int | true}"'. *)
module Closure_list : sig val fs : {f : int -> int | true} list end = struct
  let r = ref 0
  let fs = [ (fun (x : int) -> r := x; x) ]
end;;
[%%expect{|
Lines 1-4, characters 70-3:
1 | ......................................................................struct
2 |   let r = ref 0
3 |   let fs = [ (fun (x : int) -> r := x; x) ]
4 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val r : int ref val fs : (int -> int) list end
       is not included in
         sig val fs : {f : int -> int | true} list end
       Values do not match:
         val fs : (int -> int) list
       is not included in
         val fs : {f : int -> int | true} list
       The type "(int -> int) list" is not compatible with the type
         "{f : int -> int | true} list"
       Type "int -> int" is not compatible with type "{f : int -> int | true}"
       The refined type "{f : int -> int | true}" requires values that are
       "total", "stateless" and "portable", but at this position they may be
       "partial".
|}]

(* [stage 3] Accepted: the closure is total (hence stateless and portable).
   Currently: rejected, 'The type "int -> int" is not compatible with the type
   "{f : int -> int | true}"'. *)
module Total_root : sig val h : {f : int -> int | true} end = struct
  let (h @ total) (x : int) = x
end;;
[%%expect{|
module Total_root : sig val h : {f : int -> int | true} end
|}]

(* [stage 3] Accepted: the returned closure's mode is still a variable at
   inclusion, and MC constrains it to total, as moregen_alloc_mode does for
   declared return modes (compare "unit -> (int -> int) @ portable", accepted
   today for the same implementation).
   Currently: rejected, 'Type "int -> int" is not compatible with type
   "{f : int -> int | true}"'. *)
module Pure_result : sig val mk : unit -> {f : int -> int | true} end = struct
  let mk () = fun (x : int) -> x
end;;
[%%expect{|
module Pure_result : sig val mk : unit -> {f : int -> int | true} end
|}]

(* [stage 3] Accepted: both sides refined (rule (Both)); the value was
   already checked total when it entered the implementation's refinement, so
   no mode check is made, only the (trivial) implication.
   Currently: rejected, 'Type "{f : int -> int | true}" is not compatible with
   type "{g : int -> int | true && true}"'. *)
module Both_refined : sig val mk : unit -> {g : int -> int | true && true} end =
struct
  let mk () : {f : int -> int | true} = fun x -> x
end;;
[%%expect{|
module Both_refined :
  sig val mk : unit -> {g : int -> int | true && true} end
|}]

(* [stage 3] Accepted: the declared parameter mode is total, so every
   argument may enter the implementation's refinement.
   Currently: rejected, 'Type "{f : int -> int | true}" is not compatible with
   type "int -> int"'. *)
module Total_callback : sig val apply : (int -> int) @ total -> int end = struct
  let apply (f : {f : int -> int | true}) = f 0
end;;
[%%expect{|
module Total_callback : sig val apply : (int -> int) @ total -> int end
|}]
