(* TEST
 expect;
*)

(* Arrow-mode subsumption on the erasure axis.

   Erasure is an information-flow property with no ABI effect, so arrows
   follow the ordinary mode rules everywhere: contravariant in argument
   position, covariant in return position. An erased-parameter function
   promises to read less of its argument, so it may flow anywhere a
   retained-parameter function is expected; the reverse direction would let
   erased values reach a function that reads its argument, and is rejected.

   The typechecker relates arrow modes on four distinct paths — unification,
   the [type_argument] loosening, [moregen] (signature sealing) and
   [subtype]/[build_subtype] (explicit coercions) — and they must agree, so
   each is pinned here in both directions. *)

let f : int @ erased -> int -> int = fun _ y -> y

[%%expect {|
val f : int @ erased -> int -> int = <fun>
|}]

(* Annotation on a let-bound value (the [type_argument] loosening). *)

let a : int -> int -> int = f

[%%expect {|
val a : int -> int -> int = <fun>
|}]

(* The same with a lambda-bound function, so no instantiation is involved. *)

let b (p : int @ erased -> int -> int) : int -> int -> int = p

[%%expect {|
val b : (int @ erased -> int -> int) -> int -> int -> int = <fun>
|}]

(* The unsafe direction is rejected. *)

let b_rev (p : int -> int -> int) : int @ erased -> int -> int = p

[%%expect {|
Line 1, characters 65-66:
1 | let b_rev (p : int -> int -> int) : int @ erased -> int -> int = p
                                                                     ^
Error: The value "p" has type "int -> int -> int"
       but an expression was expected of type "int @ erased -> int -> int"
|}]

(* Explicit coercions, both directions. *)

let c (p : int @ erased -> int -> int) = (p :> int -> int -> int)

[%%expect {|
val c : (int @ erased -> int -> int) -> int -> int -> int = <fun>
|}]

let c_rev (p : int -> int -> int) = (p :> int @ erased -> int -> int)

[%%expect {|
Line 1, characters 36-69:
1 | let c_rev (p : int -> int -> int) = (p :> int @ erased -> int -> int)
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int -> int -> int" is not a subtype of "int @ erased -> int -> int"
|}]

(* A coercion whose target is not closed goes through [build_subtype] rather
   than [subtype]; it must agree. Object coercion is the idiomatic
   trigger (open row -> not closed). *)

let g = (f :> int -> int -> _)

[%%expect {|
val g : int -> int -> int = <fun>
|}]

let obj (o : < m : string @ erased -> int; .. >) = (o :> < m : string -> int >)

[%%expect {|
val obj : < m : string @ erased -> int; .. > -> < m : string -> int > = <fun>
|}]

(* A generic higher-order function accepts an erased-parameter callback:
   there is no ABI to disagree about, and [app] passing a retained value to
   a callback that will not read it is ordinary submoding. *)

module App : sig
  val app : ('a -> int) -> 'a -> int
end = struct
  let app g x = g x
end

let d (x : int @ erased) = 42
let e = App.app d

[%%expect {|
module App : sig val app : ('a -> int) -> 'a -> int end
val d : int @ erased -> int = <fun>
val e : int -> int = <fun>
|}]

(* Signature sealing ([moregen]), both directions. (The safe direction
   nested under another arrow, where the ambient variance has flipped, is
   pinned in erasure.ml.) *)

module Seal : sig
  val f : int -> unit
end = struct
  let f (x : int @ erased) = ()
end

[%%expect {|
module Seal : sig val f : int -> unit end
|}]

module Seal_rev : sig
  val f : int @ erased -> unit
end = struct
  let r = ref 0
  let f (x : int) = r := x
end

[%%expect {|
Lines 3-6, characters 6-3:
3 | ......struct
4 |   let r = ref 0
5 |   let f (x : int) = r := x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val r : int ref val f : int -> unit end
       is not included in
         sig val f : int @ erased -> unit end
       Values do not match:
         val f : int -> unit
       is not included in
         val f : int @ erased -> unit
       The type "int -> unit" is not compatible with the type
         "int @ erased -> unit"
|}]

(* Within one structure, a call site may raise an unannotated parameter's
   erasure before the binding's modes are zapped: [h]'s parameter is
   inferred erased from the use below. This is sound — the body never read
   the argument, or the inference would have failed — and harmless, since
   erasure carries no ABI. At toplevel the binding is zapped to legacy
   (retained) before the next item, so the same use is rejected there. *)

module M = struct
  let h x = 42
  let () = ignore (h (erased_ 5))
end

[%%expect {|
module M : sig val h : 'a @ erased -> int end
|}]

(* Optional parameters may be erased like any other: they are physically
   passed as options regardless, and the mode only constrains uses. *)

let opt ?(a : int @ erased = 0) () = 1
let opt2 ?a:(_ : int option @ erased) () = 1

[%%expect {|
val opt : ?a:int @ erased -> unit -> int = <fun>
val opt2 : ?a:int @ erased -> unit -> int = <fun>
|}]

(* Externals may declare erased parameters anywhere in their type; the
   argument is passed physically like any other. *)

external ext : unit -> (int @ erased -> int) = "caml_id"

[%%expect {|
external ext : unit -> int @ erased -> int = "caml_id"
|}]

(* A module is a runtime block whose fields are retained, so an erased value
   still cannot be stored in one, including a local module reached through
   [let open struct ... end]. *)

let f () = let open struct let x = erased_ 5 let y = 42 end in ignore x; y

[%%expect {|
Line 1, characters 31-32:
1 | let f () = let open struct let x = erased_ 5 let y = 42 end in ignore x; y
                                   ^
Error: The expression is "erased"
       but is expected to be "retained"
         because it is the value "x" in the structure at line 1, characters 27-55
         which is expected to be "retained"
         because modules always need to be allocated on the heap.
|}]
