(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Semantic inclusion (DESIGN.md 2.1 (Add)/(Both), 2.2-2.4, 3.3): the
   refinements differ in a way that needs a proof.  Typing compares the
   skeletons and records an obligation; the verifier discharges it in the
   state after the constrained structure.  All cases here are valid and are
   accepted.  Each block's expected output is the output after
   stages 1-4; the comment says what the trunk compiler does today. *)

(* [stage 3] A genuinely stronger postcondition: s > y implies r >= x.
   Currently: rejected, 'Type "{s : int | s > x}" is not compatible with type
   "{r : int | r >= x}"'. *)
module Stronger : sig val f : (x : int) -> {r : int | r >= x} end = struct
  let f (y : int) : {s : int | s > y} =
    if y < max_int then y + 1 else failwith "top"
end;;
[%%expect{|
module Stronger : sig val f : (x : int) -> {r : int | r >= x} end
|}]

(* [stage 3] Alpha-different but logically equal predicates (y <= s).
   Currently: rejected, 'Type "{s : int | x <= s}" is not compatible with type
   "{r : int | r >= x}"'. *)
module Reordered : sig val f : (x : int) -> {r : int | r >= x} end = struct
  let f (y : int) : {s : int | y <= s} = y
end;;
[%%expect{|
module Reordered : sig val f : (x : int) -> {r : int | r >= x} end
|}]

(* [stage 3] A weaker precondition: the interface promises x > 0, the
   implementation needs only x >= 0.
   Currently: rejected, 'Type "{x : int | x > 0}" is not compatible with type
   "{x : int | x >= 0}"'. *)
module Weaker_pre : sig val g : {x : int | x > 0} -> int end = struct
  let g (x : {x : int | x >= 0}) = x
end;;
[%%expect{|
module Weaker_pre : sig val g : {x : int | x > 0} -> int end
|}]

(* [stage 3] A dependent precondition on an earlier binder: from b > a (the
   caller's promise) prove the implementation's b >= a, with a bound to the
   same value on both sides.
   Currently: rejected, 'Type "{b : int | b >= a}" is not compatible with type
   "{b : int | b > a}"'. *)
module Earlier_binder : sig
  val between : (a : int) -> {b : int | b > a} -> int
end = struct
  (* The full annotation avoids the let-parameter sugar bug of item 39
     (signatures.md), which rejects [let between (a : int) (b : {b : int |
     b >= a}) = ...] with a scope-escape error. *)
  let between : (a : int) -> {b : int | b >= a} -> int = fun a b -> b - a
end;;
[%%expect{|
module Earlier_binder :
  sig val between : (a : int) -> {b : int | b > a} -> int end
|}]

(* [stage 3] Both arrows are dependent, with different binder names (y and
   x); the implementation's conjunction implies the interface's predicate.
   Both binders are bound to the same fresh argument (DESIGN.md 2.2).
   Currently: rejected, 'Type "{s : int | (s >= 0) && (s >= x)}" is not
   compatible with type "{r : int | r >= x}"'. *)
module Binder_renamed : sig val f : (x : int) -> {r : int | r >= x} end = struct
  let f (y : int) : {s : int | s >= 0 && s >= y} = if y >= 0 then y else 0
end;;
[%%expect{|
module Binder_renamed : sig val f : (x : int) -> {r : int | r >= x} end
|}]

(* [stage 3] Only the interface's arrow has a binder; the implementation's
   non-dependent result (r > 100) implies the dependent claim r > x for the
   x the interface's precondition allows (x < 100).
   Currently: rejected; the one-sided binder fails in align_arrow_codomains,
   'The type "{x : int | x < 100} -> {r : int | r > 100}" is not compatible
   with the type "(x : {x : int | x < 100}) -> {r : int | r > x}"'. *)
module One_sided : sig
  val f : (x : {x : int | x < 100}) -> {r : int | r > x}
end = struct
  let f (_ : {x : int | x < 100}) : {r : int | r > 100} = 101
end;;
[%%expect{|
module One_sided :
  sig val f : (x : {x : int | x < 100}) -> {r : int | r > x} end
|}]

(* [stage 3] A callback contract (the case e16 of subsumption.md that refine_
   cannot adapt): the interface's callback returns r > 0, the implementation
   needs only r >= 0.  The walker introduces a fresh function at the
   interface's callback type and checks it against the implementation's.
   Currently: rejected, 'Type "{r : int | r >= 0}" is not compatible with type
   "{r : int | r > 0}"'. *)
module Callback : sig
  val apply : (int -> {r : int | r > 0}) -> {n : int | n >= 0}
end = struct
  let apply (k : int -> {r : int | r >= 0}) : {n : int | n >= 0} = k 0
end;;
[%%expect{|
module Callback :
  sig val apply : (int -> {r : int | r > 0}) -> {n : int | n >= 0} end
|}]

(* [stage 3] A stronger lemma exported at a weaker statement; the result is
   ghost.  No ghost call is made: the obligation is a pure implication.
   Currently: rejected, 'Type "{u : unit | ((x + 0) = x) && ((x * 1) = x)}" is
   not compatible with type "{u : unit | (x + 0) = x}"'. *)
module Lemma : sig
  val lem : (x : int) -> {u : unit | x + 0 = x} @ ghost
end = struct
  let (lem @ total) (x : int) : {u : unit | x + 0 = x && x * 1 = x} @ ghost =
    ghost_ (refine_ ())
end;;
[%%expect{|
module Lemma :
  sig val lem : (x : int) -> {u : unit | (x + 0) = x} @ ghost end
|}]

(* [stage 3] Refined elements under covariant constructors: list, option,
   tuple and an immutable record.  Each element is a fresh value; no run-time
   coercion.
   Currently: all rejected, 'Type "{x : int | x > 0}" is not compatible with
   type "{x : int | x >= 0}"'. *)
type ('a : immutable_data) box = { v : 'a }
module Data : sig
  val l : {x : int | x >= 0} list
  val o : {x : int | x >= 0} option
  val p : {x : int | x >= 0} * {y : int | y > 0}
  val b : {x : int | x >= 0} box
end = struct
  let l : {x : int | x > 0} list = [1; 2]
  let o : {x : int | x > 0} option = Some 3
  let p : {x : int | x > 0} * {y : int | y > 1} = (1, 2)
  let b : {x : int | x > 0} box = { v = 1 }
end;;
[%%expect{|
type ('a : immutable_data) box = { v : 'a; }
module Data :
  sig
    val l : {x : int | x >= 0} list
    val o : {x : int | x >= 0} option
    val p : {x : int | x >= 0} * {y : int | y > 0}
    val b : {x : int | x >= 0} box
  end
|}]

(* [stage 3] A fact from the implementation's structure: n = 5 is known in
   the state after the structure, so r = x + n implies r = x + 5.
   Currently: rejected, 'Type "{r : int | r = (x + n)}" is not compatible with
   type "{r : int | r = (x + 5)}"'. *)
module Known_constant : sig
  val f : (x : int) -> {r : int | r = x + 5}
end = struct
  let n = 5
  let f (x : int) : {r : int | r = x + n} = x + n
end;;
[%%expect{|
module Known_constant : sig val f : (x : int) -> {r : int | r = (x + 5)} end
|}]

(* [stage 3] A predicate naming another declaration of the signature.  After
   the inclusion substitution, "limit" in the interface refers to the
   implementation's limit (DESIGN.md 3.3.2), so r < limit implies
   r <= limit.  Without the substitution the two occurrences would be
   unrelated symbols and the proof would fail.
   Currently: rejected, 'Type "{r : int | r < limit}" is not compatible with
   type "{r : int | r <= limit}"'. *)
module Limited : sig
  val limit : int
  val clamp : int -> {r : int | r <= limit}
end = struct
  let limit = 10
  let clamp (x : int) : {r : int | r < limit} =
    if x >= limit then limit - 1 else x
end;;
[%%expect{|
module Limited :
  sig val limit : int val clamp : int -> {r : int | r <= limit} end
|}]

(* [stage 3] Functor application with a stronger argument (experiment e3 of
   subsumption.md).  The obligation is discharged in the state after the
   argument's structure.
   Currently: rejected at the application, 'Type "{s : int | s > x}" is not
   compatible with type "{r : int | r >= x}"'. *)
module type S = sig val f : (x : int) -> {r : int | r >= x} end
module F (X : S) = struct let g = X.f 3 end
module Applied = F (struct
  let f (y : int) : {s : int | s > y} =
    if y < max_int then y + 1 else failwith "top"
end);;
[%%expect{|
module type S = sig val f : (x : int) -> {r : int | r >= x} end
module F : functor (X : S) -> sig val g : int end
module Applied : sig val g : int end
|}]

(* [stage 3] A local module constraint under a path condition: inside the
   branch, the fact k > 0 is available to the obligation r > k => r > 0.
   Currently: rejected, 'Signature mismatch ... Type "{r : int | r > k}" is not
   compatible with type "{r : int | r > 0}"'. *)
let positive_above (k : int) =
  if k > 0 then begin
    let module M : sig val above : int -> {r : int | r > 0} end = struct
      let above (_ : int) : {r : int | r > k} =
        if k < max_int then k + 1 else failwith "top"
    end in
    M.above 0
  end else 1;;
[%%expect{|
val positive_above : int -> int = <fun>
|}]

(* [stage 3] A constraint inside a functor body: the obligation mentions the
   functor parameter's value X.bound, discharged with the facts of X's type.
   Currently: rejected, 'Type "{r : int | r = X.bound}" is not compatible with
   type "{r : int | r >= 0}"'. *)
module Body (X : sig val bound : {b : int | b >= 0} end) = struct
  module M : sig val get : unit -> {r : int | r >= 0} end = struct
    let get () : {r : int | r = X.bound} = X.bound
  end
end;;
[%%expect{|
module Body :
  functor (X : sig val bound : {b : int | b >= 0} end) ->
    sig module M : sig val get : unit -> {r : int | r >= 0} end end
|}]

(* [stage 3] A polymorphic implementation whose type variable is instantiated
   by moregen, then a semantic comparison at the instance.
   Currently: rejected, 'Type "{x : int | x > 0}" is not compatible with type
   "{x : int | x >= 0}"'. *)
module Poly_instance : sig
  val first : {x : int | x > 0} list -> {x : int | x >= 0} option
end = struct
  let first = function [] -> None | x :: _ -> Some x
end;;
[%%expect{|
module Poly_instance :
  sig val first : {x : int | x > 0} list -> {x : int | x >= 0} option end
|}]
