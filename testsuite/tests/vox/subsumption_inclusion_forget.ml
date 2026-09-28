(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Forgetful subsumption at signature inclusion (DESIGN.md 2.1 (Drop), 3.1).
   Every case here only forgets a guarantee (a result refinement) or asks more
   of the caller (a parameter refinement the implementation does not need).
   No solver query is made for the inclusion itself.  Each block's expected
   output is the output after stages 1-4; the comment says what the
   installed trunk compiler (475c25b3dd) does today. *)

(* [stage 1] Dropping a dependent result refinement: the binder occurs only on
   the implementation side.
   Currently: rejected, 'The type "(x : int) -> {r : int | r >= x}" is not
   compatible with the type "int -> int"' (no further explanation; the
   one-sided binder fails in align_arrow_codomains). *)
module Drop_dependent : sig val f : int -> int end = struct
  let (f @ total) (x : int) : {r : int | r >= x} = x
end;;
[%%expect{|
module Drop_dependent : sig val f : int -> int end
|}]

(* [stage 1] Dropping a non-dependent result refinement.
   Currently: rejected, 'Type "{r : int | r > 0}" is not compatible with type
   "int"'. *)
module Drop_result : sig val f : int -> int end = struct
  let f (x : int) : {r : int | r > 0} = if x > 0 then x else 1
end;;
[%%expect{|
module Drop_result : sig val f : int -> int end
|}]

(* [stage 1] A refined constant exported at its payload type.
   Currently: rejected, 'The type "{m : int list | m === []}" is not compatible
   with the type "int list"'. *)
module Empty : sig val empty : int list end = struct
  let empty : {m : int list | m === []} = []
end;;
[%%expect{|
module Empty : sig val empty : int list end
|}]

(* [stage 1] The interface refines a parameter that the implementation does
   not need refined (the caller promises more).
   Currently: rejected, 'Type "int" is not compatible with type
   "{x : int | x > 0}"'. *)
module Refine_parameter : sig val g : {x : int | x > 0} -> int end = struct
  let g (x : int) = x
end;;
[%%expect{|
module Refine_parameter : sig val g : {x : int | x > 0} -> int end
|}]

(* [stage 1] A polymorphic implementation instantiated at a refined type
   ('a := {x | x > 0}); the result position then drops the refinement.
   Currently: rejected, 'The type "{x : int | x > 0} -> {x : int | x > 0}" is
   not compatible with the type "{x : int | x > 0} -> int"'. *)
module Polymorphic : sig val g : {x : int | x > 0} -> int end = struct
  let g x = x
end;;
[%%expect{|
module Polymorphic : sig val g : {x : int | x > 0} -> int end
|}]

(* [stage 1] A lemma exported at unit: its statement is forgotten.  The
   lemma's arrow is dependent (the statement mentions x), so this also needs
   the one-sided binder rule.
   Currently: rejected, 'The type "(x : int) -> {u : unit | (x + 0) = x}" is
   not compatible with the type "int -> unit"'. *)
module Lemma_unit : sig val lem : int -> unit end = struct
  let (lem @ total) (x : int) : {u : unit | x + 0 = x} = ()
end;;
[%%expect{|
module Lemma_unit : sig val lem : int -> unit end
|}]

(* [stage 1] Refined elements under a covariant constructor.
   Currently: rejected, 'Type "{x : int | x > 0}" is not compatible with type
   "int"'. *)
module Positive_list : sig val l : int list end = struct
  let l : {x : int | x > 0} list = [1; 2]
end;;
[%%expect{|
module Positive_list : sig val l : int list end
|}]

type ('a : immutable_data) box = { v : 'a };;
[%%expect{|
type ('a : immutable_data) box = { v : 'a; }
|}]

(* [stage 1] A record type with a covariant (immutable) parameter.
   Currently: rejected, 'Type "{x : int | x > 0}" is not compatible with type
   "int"'. *)
module Box : sig val b : int box end = struct
  let b : {x : int | x > 0} box = { v = 1 }
end;;
[%%expect{|
module Box : sig val b : int box end
|}]

(* [stage 1] Labelled and optional arguments: labels must match, the result
   refinement is dropped.
   Currently: both rejected, 'Type "{r : int | r >= 0}" is not compatible with
   type "int"'. *)
module Labelled : sig val f : lo:int -> int -> int end = struct
  let (f @ total) ~(lo : int) (x : int) : {r : int | r >= 0} =
    if x > lo then 0 else 1
end;;
[%%expect{|
module Labelled : sig val f : lo:int -> int -> int end
|}]

module Optional : sig val f : ?lo:int -> int -> int end = struct
  let (f @ total) ?(lo = 0) (x : int) : {r : int | r >= 0} =
    if x > lo then 0 else 1
end;;
[%%expect{|
module Optional : sig val f : ?lo:int -> int -> int end
|}]

(* [stage 1] Optional argument whose payload is refined in the interface only
   ('a option is covariant; the caller promises more).
   Currently: rejected, 'Type "int" is not compatible with type
   "{x : int | x > 0}"'. *)
module Optional_refined : sig val f : ?lo:{x : int | x > 0} -> int -> int end =
struct
  let f ?(lo = 1) (x : int) = x + lo
end;;
[%%expect{|
module Optional_refined : sig val f : ?lo:{x : int | x > 0} -> int -> int end
|}]

(* [stage 1] Functor application: the argument's value has a stronger type
   than the parameter's.
   Currently: rejected at the application, 'The type
   "(y : int) -> {s : int | s >= y}" is not compatible with the type
   "int -> int"'. *)
module type P = sig val f : int -> int end
module G (X : P) = struct let g = X.f 3 end
module Applied = G (struct
  let (f @ total) (y : int) : {s : int | s >= y} = y
end);;
[%%expect{|
module type P = sig val f : int -> int end
module G : functor (X : P) -> sig val g : int end
module Applied : sig val g : int end
|}]

(* [stage 1] The same with a module path as the argument.
   Currently: rejected in the same way. *)
module Strong = struct
  let (f @ total) (y : int) : {s : int | s >= y} = y
end
module Applied_path = G (Strong);;
[%%expect{|
module Strong : sig val f : (y : int) -> {s : int | s >= y} end
module Applied_path : sig val g : int end
|}]

(* [stage 1] A local module constraint inside a function.
   Currently: rejected, 'Signature mismatch ... The type
   "(y : int) -> {s : int | s >= y}" is not compatible with the type
   "int -> int"'. *)
let local () =
  let module M : sig val f : int -> int end = struct
    let (f @ total) (y : int) : {s : int | s >= y} = y
  end in
  M.f 3;;
[%%expect{|
val local : unit -> int = <fun>
|}]

(* [stage 1] A first-class module.  Packs are discharged in the fallback
   (empty) state (DESIGN.md 3.3.5), which does not matter here: no proof.
   Currently: rejected, 'Signature mismatch ... is not included in P'. *)
module Pack = struct
  let m =
    (module struct let (f @ total) (y : int) : {s : int | s >= y} = y end : P)
end;;
[%%expect{|
module Pack : sig val m : (module P) end
|}]

(* [stage 1] A functor type in a signature.  The parameter position is
   negative: the declared functor requires X.f : int -> {r | r > 0}, the
   implementation only uses X.f : int -> int, so the refinement is dropped
   (value flows from the declared parameter into the implementation's).
   Currently: rejected, 'Type "{r : int | r > 0}" is not compatible with type
   "int"'. *)
module Higher : sig
  module F (X : sig val f : int -> {r : int | r > 0} end) : sig val y : int end
end = struct
  module F (X : sig val f : int -> int end) = struct let y = X.f 0 end
end;;
[%%expect{|
module Higher :
  sig
    module F :
      functor (X : sig val f : int -> {r : int | r > 0} end) ->
        sig val y : int end
  end
|}]

(* [stage 1] Dropping a refinement from a closure never needs a mode check:
   the value leaves a type that crosses portability and totality.
   Currently: rejected, 'The type "{f : int -> int | true}" is not compatible
   with the type "int -> int"'. *)
module Drop_closure : sig val h : int -> int end = struct
  let (k @ total) (x : int) = x
  let h : {f : int -> int | true} = k
end;;
[%%expect{|
module Drop_closure : sig val h : int -> int end
|}]
