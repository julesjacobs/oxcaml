(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: the [refines] component of a kind.  [type t : value refines
   int] declares the type's logical modeling in its KIND, so it
   survives abstraction: clients of an abstract [t] reason about its
   values as solver Ints.  The component is declaration metadata --
   the kind algebra never combines it -- checked once, at signature
   inclusion, against the implementation's declared or structural
   modeling. *)

module M : sig
  type t : value refines int

  val zero : t{ _ = 0 }
  val next : (x : t) -> t{ _ = x + 1 }
end = struct
  type t = int

  let zero : t{ _ = 0 } = 0
  let next : (x : t) -> t{ _ = x + 1 } = fun x -> x + 1
end
[%%expect{|
Line 9, characters 26-27: vox VC:
  goal: 0 = 0
  hypotheses: <none>
Line 10, characters 50-55: vox VC:
  goal: x + 1 = x + 1
  hypotheses: <none>
module M :
  sig type t val zero : t{ _ = 0 } val next : (x : t) -> t{ _ = x + 1 } end
|}]

(* The client computes at the declared modeling: [M.t] values are
   Ints, so arithmetic refinements about them elaborate, the imported
   facts flow, and the goal is arithmetic. *)
let one () =
  let z = M.zero in
  let o = M.next z in
  let refine_ ok = (o : M.t{ _ = 1 }) in
  ok
[%%expect{|
Line 4, characters 20-21: vox VC:
  goal: o = 1
  hypotheses:
  o = z + 1
  z = M.zero
  M.zero = 0
val one : unit -> M.t = <fun>
|}]

(* Inclusion is checked: an interface may not claim a modeling the
   implementation does not carry. *)
module Bad : sig
  type t : value refines bool
end = struct
  type t = int
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int end
       is not included in
         sig type t end
       Type declarations do not match: type t = int is not included in type t
       The second declares refines bool where the first declares refines int.
|}]

(* A parameterized head: the declared modeling applies to every
   instance. *)
module P : sig
  type 'a t : value refines int

  val v : int t{ _ > 0 }
end = struct
  type 'a t = int

  let v : int t{ _ > 0 } = 1
end
[%%expect{|
Line 8, characters 27-28: vox VC:
  goal: 1 > 0
  hypotheses: <none>
module P : sig type 'a t val v : int t{ _ > 0 } end
|}]

let use_p () =
  let x = P.v in
  let refine_ ok = (x : int P.t{ _ > 0 }) in
  ok
[%%expect{|
Line 3, characters 20-21: vox VC:
  goal: x > 0
  hypotheses:
  x = P.v
  P.v > 0
val use_p : unit -> int P.t = <fun>
|}]

(* vox: the GENERAL form -- [refines] takes a parenthesized CORE TYPE,
   elaborated into a refinement SORT.  A type modeled as a TUPLE lets
   clients project it with [fst]/[snd]; [type t = int * int] satisfies
   [refines (int * int)] structurally, so the interface need not repeat
   the shape in the implementation. *)
module Pair : sig
  type t : value refines (int * int)

  val mk : (a : int) -> (b : int) -> t{ fst _ = a }
end = struct
  type t = int * int

  let mk : (a : int) -> (b : int) -> t{ fst _ = a } = fun a b -> (a, b)
end
[%%expect{|
Line 8, characters 65-71: vox VC:
  goal: fst (a, b) = a
  hypotheses: <none>
module Pair : sig type t val mk : (a : int) -> int -> t{ fst _ = a } end
|}]

let use_pair () =
  let p = Pair.mk 3 4 in
  let refine_ ok = (p : Pair.t{ fst _ = 3 }) in
  ok
[%%expect{|
Line 3, characters 20-21: vox VC:
  goal: fst p = 3
  hypotheses:
  fst p = 3
val use_pair : unit -> Pair.t = <fun>
|}]

(* An abstract type modeled as a local simple VARIANT: its constructors
   are usable in predicates about the abstract type, even though the
   value itself is opaque (there is no [match] on an abstract type).
   The predicate [_ = ICons (1, INil)] elaborates because the declared
   modeling gives the abstract type the [ilist] datatype sort. *)
type ilist = INil | ICons of int * ilist
type opaque_list : value refines (ilist)
[%%expect{|
type ilist = INil | ICons of int * ilist
type opaque_list
|}]

let use_ilist (x : opaque_list{ _ = ICons (1, INil) }) =
  let refine_ ok = (x : opaque_list{ _ = ICons (1, INil) }) in
  ok
[%%expect{|
Line 2, characters 20-21: vox VC:
  goal: x = ICons (1, INil)
  hypotheses:
  x = ICons (1, INil)
val use_ilist : opaque_list{ _ = ICons (1, INil) } -> opaque_list = <fun>
|}]

(* A PARAMETERIZED modeling: [refines ('a mylist)] carries a type
   parameter, instantiated positionally at each use.  At [int q] the
   element sort is [int], so [Cons (7, Nil)] elaborates over ints. *)
type 'a mylist = Nil | Cons of 'a * 'a mylist
type 'a q : value refines ('a mylist)
[%%expect{|
type 'a mylist = Nil | Cons of 'a * 'a mylist
type 'a q
|}]

let use_q (s : (int q){ _ = Cons (7, Nil) }) =
  let refine_ ok = (s : (int q){ _ = Cons (7, Nil) }) in
  ok
[%%expect{|
Line 2, characters 20-21: vox VC:
  goal: s = Cons (7, Nil)
  hypotheses:
  s = Cons (7, Nil)
val use_q : int q{ _ = Cons (7, Nil) } -> int q = <fun>
|}]

(* Inclusion checks the whole sort structurally, and the printer renders
   it readably: [int] is not the tuple the interface claims. *)
module Bad_tuple : sig
  type t : value refines (int * int)
end = struct
  type t = int
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int end
       is not included in
         sig type t end
       Type declarations do not match: type t = int is not included in type t
       The second declares refines (int * int) where the first declares refines int.
|}]

(* A type that cannot model any sort is a hard error at the annotation. *)
type bad : value refines (float)
[%%expect{|
Line 1, characters 26-31:
1 | type bad : value refines (float)
                              ^^^^^
Error: vox: this type cannot model a refinement sort
|}]

(* An HONEST datatype implementation: [type s = ilist] satisfies
   [refines (ilist)] structurally, like [type t = int] satisfies
   [refines int] -- no trusted assertion needed. *)
module Honest : sig
  type s : value refines (ilist)
end = struct
  type s = ilist
end
[%%expect{|
module Honest : sig type s end
|}]

(* A MONOMORPHIC alias of a supported head models like its target. *)
type index = int
type pos : value refines (index)
[%%expect{|
type index = int
type pos
|}]

(* vox: a modeling may carry an INVARIANT -- [refines (int{ _ >= 0 })]
   declares that the type is modeled at [int] but every value satisfies
   [_ >= 0].  The invariant survives abstraction as a FREE FACT: each
   binder of the abstract type contributes it, so clients reason with
   it even though the definition is hidden. *)
module M : sig
  type nat : value refines (int{ _ >= 0 })

  val get : unit -> nat
end = struct
  type nat = int{ _ >= 0 }

  let get () : nat = refine_ 0
end
[%%expect{|
Line 8, characters 29-30: vox VC:
  goal: 0 >= 0
  hypotheses: <none>
module M : sig type nat val get : unit -> nat end
|}]

(* The invariant flows to the client as a hypothesis about the binder,
   with no refinement written at the use site. *)
let use_nat () =
  let n = M.get () in
  let refine_ ok = (n : M.nat{ _ + 1 >= 1 }) in
  ok
[%%expect{|
Line 3, characters 20-21: vox VC:
  goal: n + 1 >= 1
  hypotheses:
  n >= 0
val use_nat : unit -> M.nat = <fun>
|}]

(* An HONEST refined manifest satisfies the invariant interface: [type
   nat = int{ _ >= 0 }] structurally carries the same fact, no trusted
   assertion needed. *)
module Honest_inv : sig
  type nat : value refines (int{ _ >= 0 })
end = struct
  type nat = int{ _ >= 0 }
end
[%%expect{|
module Honest_inv : sig type nat end
|}]

(* Inclusion compares the invariant structurally; a different predicate
   is rejected, and the printer renders both readably. *)
module Bad_inv : sig
  type t : value refines (int{ _ >= 0 })
end = struct
  type t = int{ _ > 0 }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = int{ _ > 0 }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int{ _ > 0 } end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = int{ _ > 0 }
       is not included in
         type t
       The second declares refines int{ _ >= 0 } where the first declares refines int{ _ > 0 }.
|}]

(* An invariant may mention only the value: a program variable in the
   predicate is a hard error at the annotation. *)
let bound = 5
type bad_inv : value refines (int{ _ >= bound })
[%%expect{|
val bound : int = 5
Line 2, characters 30-47:
2 | type bad_inv : value refines (int{ _ >= bound })
                                  ^^^^^^^^^^^^^^^^^
Error: vox: an invariant may mention only the value (and module-level values, constructors, and spec functions)
|}]

(* Invariants compose only at the head of the written type: one in an
   argument position is out of v2 scope. *)
type 'a box = Box of 'a
type bad_arg : value refines ((int{ _ >= 0 }) box)
[%%expect{|
type 'a box = Box of 'a
Line 2, characters 30-49:
2 | type bad_arg : value refines ((int{ _ >= 0 }) box)
                                  ^^^^^^^^^^^^^^^^^^^
Error: vox: invariants compose at the head only
|}]

(* A module-level value of an invariant type carries the invariant BY
   PATH, like a written refinement would (review finding). *)
module ByPath : sig
  type nat2 : value refines (int{ _ >= 0 })

  val zero : nat2
end = struct
  type nat2 = int{ _ >= 0 }

  let zero : nat2 = 0
end
[%%expect{|
Line 8, characters 20-21: vox VC:
  goal: 0 >= 0
  hypotheses:
  bound = 5
module ByPath : sig type nat2 val zero : nat2 end
|}]

let use_by_path () =
  let refine_ ok = (refine_ true : bool{ ByPath.zero >= 0 }) in
  ok
[%%expect{|
Line 2, characters 28-32: vox VC:
  goal: ByPath.zero >= 0
  hypotheses:
  bound = 5
  ByPath.zero >= 0
val use_by_path : unit -> bool = <fun>
|}]
