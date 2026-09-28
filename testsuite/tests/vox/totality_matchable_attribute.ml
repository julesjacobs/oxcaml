(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* The [@@total_matchable] guarantee closes the abstract-consumer route: an
   abstract type whose implementation hides a negative recursion (a function
   that consumes the type and reaches back) may NOT be matched in total code,
   even through a positive field, unless its declaration carries the checked
   guarantee. The guarantee is verified where the type is defined, with its
   representation visible; a negatively recursive type cannot carry it, but a
   positively recursive one (a tree, a list, a functional map) can. Signature
   inclusion lets an interface promise it only when the implementation has it,
   by attribute or by computation. See negative_totality.ml and
   totality_hidden_types.ml for the underlying totality checks. *)

(* The abstract-consumer knot: [u = t -> unit] is hidden behind an abstract
   [type u], and a total [app] applies it. [u] occurs only positively in
   [Roll of u], but total code still may not match [t]. *)
module Abstract_consumer = struct
  module M : sig
    type u
    type t = Roll of u
    val app : u -> t -> int @@ total
  end = struct
    type u = t -> int
    and t = Roll of u
    let app f x = f x
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
Line 11, characters 45-53:
11 |   let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
                                                  ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 11, characters 20-58
         which is expected to be "total".
|}]

(* An immediate abstract type cannot be a function or the boxed matched type,
   so it needs no attribute. *)
module Abstract_consumer_immediate = struct
  module M : sig
    type u : immediate
    type t = Roll of u
    val get : u -> int @@ total
  end = struct
    type u = int
    type t = Roll of u
    let get u = u
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
module Abstract_consumer_immediate :
  sig
    module M :
      sig
        type u : immediate
        type t = Roll of u
        val get : u -> int @@ total
      end
    val use : M.t -> int
  end
|}]

(* A genuinely opaque data type may carry the guarantee, and then total code
   may match a container that holds it. *)
module Attributed_opaque = struct
  module M : sig
    type u [@@total_matchable]
    type t = Roll of u
    val v : u
  end = struct
    type u = int
    type t = Roll of u
    let v = 0
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
module Attributed_opaque :
  sig
    module M : sig type u type t = Roll of u val v : u end
    val use : M.t -> int
  end
|}]

(* A boxed [immutable_data] data type also may carry it: [immutable_data] alone
   does not make a hidden type safe (it can hold a function), but the checked
   guarantee, verified against the implementation, does. *)
module Attributed_immutable_data = struct
  module M : sig
    type u : immutable_data [@@total_matchable]
    type t = Roll of u
  end = struct
    type u = int list
    type t = Roll of u
  end
  let (use @ total) (x : M.t) = match x with M.Roll _ -> 0
end;;
[%%expect{|
module Attributed_immutable_data :
  sig
    module M : sig type u : immutable_data type t = Roll of u end
    val use : M.t -> int
  end
|}]

(* Positive recursion is fine for the guarantee: a recursive tree is knot-free.
   Total code may match a container holding the abstract tree. *)
module Attributed_recursive = struct
  module M : sig
    type tree [@@total_matchable]
    type t = Wrap of tree
    val leaf : tree
  end = struct
    type tree = Leaf | Node of tree * int * tree
    type t = Wrap of tree
    let leaf = Leaf
  end
  let (use @ total) (x : M.t) = match x with M.Wrap _ -> 0
end;;
[%%expect{|
module Attributed_recursive :
  sig
    module M : sig type tree type t = Wrap of tree val leaf : tree end
    val use : M.t -> int
  end
|}]

(* Inclusion refuses the guarantee when the implementation is negatively
   recursive (would forge it). *)
module False_attribute = struct
  module M : sig
    type u [@@total_matchable]
    type t = Roll of u
  end = struct
    type u = t -> int
    and t = Roll of u
  end
end;;
[%%expect{|
Lines 5-8, characters 8-5:
5 | ........struct
6 |     type u = t -> int
7 |     and t = Roll of u
8 |   end
Error: Signature mismatch:
       Modules do not match:
         sig type u = t -> int and t = Roll of u end
       is not included in
         sig type u type t = Roll of u end
       Type declarations do not match:
         type u = t -> int
       is not included in
         type u
       Their total-matchability guarantees differ;
       an interface may promise
       "[@@total_matchable]" only if the implementation's declaration does.
|}]

(* The definition site itself rejects a [@@total_matchable] that its own
   representation refutes (a negatively recursive type). *)
module Self_negative = struct
  type t = Roll of (t -> int) [@@total_matchable]
end;;
[%%expect{|
Line 2, characters 2-49:
2 |   type t = Roll of (t -> int) [@@total_matchable]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid "[@@total_matchable]" declaration: total code cannot pattern-match this type, because eliminating it could reach the type itself.
|}]

(* A [with type] constraint may not forge the guarantee either. *)
module type Hidden = sig
  type u [@@total_matchable]
  type t = Roll of u
end
module Constraint_forge = struct
  type bad = Loop of (bad -> int)
  module type S = Hidden with type u = bad -> int
end;;
[%%expect{|
module type Hidden = sig type u type t = Roll of u end
Line 7, characters 30-49:
7 |   module type S = Hidden with type u = bad -> int
                                  ^^^^^^^^^^^^^^^^^^^
Error: This constraint requires a type with a checked total-matchability guarantee.
|}]

(* Round-trip through a compiled interface: a client matches a container that
   holds another module's attributed abstract type. *)
module Producer : sig
  type u [@@total_matchable]
  type t = Hold of u
  val make : unit -> t
end = struct
  type u = int
  type t = Hold of u
  let make () = Hold 0
end

module Client = struct
  let (use @ total) (x : Producer.t) = match x with Producer.Hold _ -> 0
end;;
[%%expect{|
module Producer : sig type u type t = Hold of u val make : unit -> t end
module Client : sig val use : Producer.t -> int end
|}]
