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
  goal: (x + 1) = (x + 1)
  hypotheses: <none>
module M :
  sig type t val zero : t{ _ = 0 } val next : (x : t) -> t{ _ = (x + 1) } end
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
  o = (z + 1)
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
