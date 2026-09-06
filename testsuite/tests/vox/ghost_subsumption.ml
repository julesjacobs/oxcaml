(* TEST
 expect;
*)

let f : int @ ghost -> int -> int = fun _ y -> y

[%%expect {|
val f : int @ ghost -> int -> int = <fun>
|}]

let a : int -> int -> int = f

[%%expect {|
val a : int -> int -> int = <fun>
|}]

let b (p : int @ ghost -> int -> int) : int -> int -> int = p

[%%expect {|
val b : (int @ ghost -> int -> int) -> int -> int -> int = <fun>
|}]

let b_rev (p : int -> int -> int) : int @ ghost -> int -> int = p

[%%expect {|
Line 1, characters 64-65:
1 | let b_rev (p : int -> int -> int) : int @ ghost -> int -> int = p
                                                                    ^
Error: The value "p" has type "int -> int -> int"
       but an expression was expected of type "int @ ghost -> int -> int"
|}]

let c (p : int @ ghost -> int -> int) = (p :> int -> int -> int)

[%%expect {|
val c : (int @ ghost -> int -> int) -> int -> int -> int = <fun>
|}]

let c_rev (p : int -> int -> int) = (p :> int @ ghost -> int -> int)

[%%expect {|
Line 1, characters 36-68:
1 | let c_rev (p : int -> int -> int) = (p :> int @ ghost -> int -> int)
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "int -> int -> int" is not a subtype of "int @ ghost -> int -> int"
|}]

let g = (f :> int -> int -> _)

[%%expect {|
val g : int -> int -> int = <fun>
|}]

let obj (o : < m : string @ ghost -> int; .. >) = (o :> < m : string -> int >)

[%%expect {|
val obj : < m : string @ ghost -> int; .. > -> < m : string -> int > = <fun>
|}]

let obj_rev (o : < m : string -> int; .. >) = (o :> < m : string @ ghost -> int >)

[%%expect {|
Line 1, characters 47-48:
1 | let obj_rev (o : < m : string -> int; .. >) = (o :> < m : string @ ghost -> int >)
                                                   ^
Error: This expression cannot be coerced to type
         ""< m : string @ ghost -> int >"";
       it has type "< m : string -> int; .. >" but is here used with type
         "< m : string @ ghost -> int; .. >"
       The method "m" has type "string -> int", but the expected method type was
       "string @ ghost -> int"
|}]

module App : sig
  val app : ('a -> int) -> 'a -> int
end = struct
  let app g x = g x
end

let d (x : int @ ghost) = 42
let e = App.app d

[%%expect {|
module App : sig val app : ('a -> int) -> 'a -> int end
val d : int @ ghost -> int = <fun>
val e : int -> int = <fun>
|}]

module Seal : sig
  val f : int -> unit
end = struct
  let f (x : int @ ghost) = ()
end

[%%expect {|
module Seal : sig val f : int -> unit end
|}]

module Seal_rev : sig
  val f : int @ ghost -> unit
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
         sig val f : int @ ghost -> unit end
       Values do not match:
         val f : int -> unit
       is not included in
         val f : int @ ghost -> unit
       The type "int -> unit" is not compatible with the type
         "int @ ghost -> unit"
|}]

module M = struct
  let h x = 42
  let () = ignore (h (ghost_ 5))
end

[%%expect {|
module M : sig val h : 'a @ ghost -> int end
|}]

let opt ?(a : int = 0) () = 1
let opt2 ?a:(_ : int option @ ghost) () = 1

[%%expect {|
val opt : ?a:int -> unit -> int = <fun>
val opt2 : ?a:int @ ghost -> unit -> int = <fun>
|}]

external ext : unit -> (int @ ghost -> int) = "caml_id"

[%%expect {|
external ext : unit -> int @ ghost -> int = "caml_id"
|}]

let f () = let open struct let x = ghost_ 5 let y = 42 end in ignore x; y

[%%expect {|
Line 1, characters 31-32:
1 | let f () = let open struct let x = ghost_ 5 let y = 42 end in ignore x; y
                                   ^
Error: The expression is "ghost"
       but is expected to be "real"
         because it is the value "x" in the structure at line 1, characters 27-54
         which is expected to be "real"
         because modules always need to be allocated on the heap.
|}]

let rejected_default ?(a : int @ ghost = 0) () = 1;;
[%%expect{|
Line 1, characters 23-38:
1 | let rejected_default ?(a : int @ ghost = 0) () = 1;;
                           ^^^^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

let defaulted ?(a = 0) () = 1;;
[%%expect{|
val defaulted : ?a:int -> unit -> int = <fun>
|}]

let rejected_option () = defaulted ?a:(ghost_ None) ();;
[%%expect{|
Line 1, characters 38-51:
1 | let rejected_option () = defaulted ?a:(ghost_ None) ();;
                                          ^^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]
