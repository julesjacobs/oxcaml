(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* [@@vox.sort]: an abstract type declares that its LOGICAL
   REPRESENTATIVE is its value at a base sort -- values are modelled
   as opaque Ints (or Bools) instead of at VoxU, so refinements can
   use them directly as the values they stand for (ghost types:
   prophecies, refs denoting their contents).  TRUSTED: the declaring
   library asserts every fact it issues is true of that
   interpretation. *)

(* An int-sorted ghost: the value participates in arithmetic
   refinements directly. *)
module G : sig
  type g [@@vox.sort int]

  val mk : (v : int) -> g{ _ = v } @ unique
end = struct
  type g = { mutable c : int } [@@vox.sort int]

  let mk : (v : int) -> g{ _ = v } @ unique =
    fun v -> assume_unchecked_ (Obj.magic_unique { c = v })
end
[%%expect{|
module G : sig type g val mk : (v : int) -> g{ _ = v } @ unique end
|}]

let use : (n : int) -> int{ _ = n + 1 } =
  fun n ->
  let x = G.mk n in
  let _u : unit{ x + 1 = n + 1 } = () in
  n + 1
[%%expect{|
Line 3, characters 6-7:
3 |   let x = G.mk n in
          ^
Warning 26 [unused-var]: unused variable "x".

val use : (n : int) -> int{ _ = n + 1 } = <fun>
|}]

(* The sort is logic-level only: the program cannot treat g as int. *)
let bad (x : G.g) = x + 1
[%%expect{|
Line 1, characters 20-21:
1 | let bad (x : G.g) = x + 1
                        ^
Error: The value "x" has type "G.g" but an expression was expected of type "int"
|}]

(* bool sort. *)
module B : sig
  type b [@@vox.sort bool]

  val mk : (v : bool) -> b{ _ = v } @ unique
end = struct
  type b = { mutable c : bool } [@@vox.sort bool]

  let mk : (v : bool) -> b{ _ = v } @ unique =
    fun v -> assume_unchecked_ (Obj.magic_unique { c = v })
end
[%%expect{|
module B : sig type b val mk : (v : bool) -> b{ _ = v } @ unique end
|}]

let useb : (v : bool) -> bool{ _ = v } =
  fun v ->
  let x = B.mk v in
  let _u : unit{ x = v } = () in
  v
[%%expect{|
Line 3, characters 6-7:
3 |   let x = B.mk v in
          ^
Warning 26 [unused-var]: unused variable "x".

val useb : (v : bool) -> bool{ _ = v } = <fun>
|}]

(* A malformed sort name is rejected EAGERLY, even though no value of
   the type ever reaches a verification condition. *)
type t1 [@@vox.sort float]
[%%expect{|
Line 1, characters 8-26:
1 | type t1 [@@vox.sort float]
            ^^^^^^^^^^^^^^^^^^
Error: vox: unknown vox.sort "float" (expected "int", "bool", "opaque", or lean "Name")
|}]

type t2 [@@vox.sort]
[%%expect{|
Line 1, characters 8-20:
1 | type t2 [@@vox.sort]
            ^^^^^^^^^^^^
Error: vox: vox.sort takes a single sort name, e.g. [@@vox.sort int]
|}]

(* The attribute must agree between a signature and its
   implementation: sorts are computed per-compilation from the
   visible declaration, so a mismatch would let clients reason at one
   sort against an implementation verified at another.
   (The .mli/.ml pair check is exercised in vox_sort_intf.ml; module
   ascription inside one unit uses the STRUCT's declaration for the
   implementation's own VCs and the ascribed signature only shapes
   what clients of the module see, so both carry the attribute
   above.) *)

(* The attribute on a pure ALIAS would be silently ignored (sorting
   expands aliases first); it is rejected instead. *)
type t3 = bool [@@vox.sort int]
[%%expect{|
Line 1, characters 15-31:
1 | type t3 = bool [@@vox.sort int]
                   ^^^^^^^^^^^^^^^^
Error: vox: vox.sort on a type alias has no effect (an alias expands to its definition before sorting); put the attribute on the definition
|}]

(* Types declared inside local modules classify correctly (the
   verifier consults each expression's own env, not the enclosing
   structure's). *)
let localmod : (n : int) -> int{ _ = n + 1 } =
  fun n ->
  let module M = struct
    type h = { mutable c : int } [@@vox.sort int]

    let mk : (v : int) -> h{ _ = v } @ unique =
      fun v -> assume_unchecked_ (Obj.magic_unique { c = v })
  end
  in
  let x = M.mk n in
  let _u : unit{ x + 1 = n + 1 } = () in
  n + 1
[%%expect{|
Line 10, characters 6-7:
10 |   let x = M.mk n in
           ^
Warning 26 [unused-var]: unused variable "x".

val localmod : (n : int) -> int{ _ = n + 1 } = <fun>
|}]
