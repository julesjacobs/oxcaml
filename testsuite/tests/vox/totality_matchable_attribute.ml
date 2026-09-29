(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

(* Logicality closes the abstract-consumer route. Total code may look inside a
   value only if its type is logical (its values form a set), and a type is
   logical only if its components are. An abstract type is logical only if its
   kind says so ([value mod logical], [logical_data], ...), and
   inclusion checks that claim against the implementation like any other kind
   bound. An abstract type whose implementation hides a negative recursion (a
   function that consumes the type and reaches back) cannot make that claim,
   so a container holding it may not be matched in total code. A recursive
   type is logical only if it is [@@inductive]. See negative_totality.ml and
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
Error: The match on a value whose type is not logical (M.u is abstract and its kind does not say mod logical) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 11, characters 20-58
         which is expected to be "total".
|}]

(* An immediate abstract type cannot be a function or the boxed matched type;
   [immediate] includes [logical]. *)
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

(* A genuinely opaque data type may be declared logical, and then total code
   may match a container that holds it. *)
module Attributed_opaque = struct
  module M : sig
    type u : value mod logical
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
    module M :
      sig type u : value mod logical type t = Roll of u val v : u end
    val use : M.t -> int
  end
|}]

(* [immutable_data] alone says nothing about logicality: a non-inductive
   recursive type is immutable data. With [mod logical] the claim is checked
   against the implementation. *)
module Attributed_immutable_data = struct
  module M : sig
    type u : logical_data
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
    module M : sig type u : logical_data type t = Roll of u end
    val use : M.t -> int
  end
|}]

(* A recursive tree is logical only if it is [@@inductive], which makes its
   values finite. Without the attribute (possibly cyclic values) the claim is
   refused; see Attributed_inductive below. *)
module Attributed_recursive = struct
  module M : sig
    type tree : value mod logical
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
Lines 6-10, characters 8-5:
 6 | ........struct
 7 |     type tree = Leaf | Node of tree * int * tree
 8 |     type t = Wrap of tree
 9 |     let leaf = Leaf
10 |   end
Error: Signature mismatch:
       Modules do not match:
         sig
           type tree = Leaf | Node of tree * int * tree
           type t = Wrap of tree
           val leaf : tree
         end
       is not included in
         sig
           type tree : value mod logical
           type t = Wrap of tree
           val leaf : tree
         end
       Type declarations do not match:
         type tree = Leaf | Node of tree * int * tree
       is not included in
         type tree : value mod logical
       The kind of the first is immutable_data
         because of the definition of tree at line 7, characters 4-48.
       But the kind of the first must be a subkind of value mod logical
         because of the definition of tree at line 3, characters 4-33.
       The first is not logical: tree is recursive but not [@@inductive].
|}]

module Attributed_inductive = struct
  module M : sig
    type tree : value mod logical
    type t = Wrap of tree
    val leaf : tree
  end = struct
    type tree = Leaf | Node of tree * int * tree [@@inductive]
    type t = Wrap of tree
    let leaf = Leaf
  end
  let (use @ total) (x : M.t) = match x with M.Wrap _ -> 0
end;;
[%%expect{|
module Attributed_inductive :
  sig
    module M :
      sig
        type tree : value mod logical
        type t = Wrap of tree
        val leaf : tree
      end
    val use : M.t -> int
  end
|}]

(* Inclusion refuses the claim when the implementation is negatively recursive
   (would forge it). *)
module False_attribute = struct
  module M : sig
    type u : value mod logical
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
         sig type u : value mod logical type t = Roll of u end
       Type declarations do not match:
         type u = t -> int
       is not included in
         type u : value mod logical
       The kind of the first is value non_float mod aliased immutable
         because it's a function type.
       But the kind of the first must be a subkind of value mod logical
         because of the definition of u at line 3, characters 4-30.
       The first is not logical: t is recursive but not [@@inductive].
|}]

(* The definition site itself rejects a [mod logical] annotation that its own
   representation refutes (a negatively recursive type). *)
module Self_negative = struct
  type t : value mod logical = Roll of (t -> int)
end;;
[%%expect{|
Line 2, characters 2-49:
2 |   type t : value mod logical = Roll of (t -> int)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value non_float mod immutable
         because it's a boxed variant type.
       But the kind of type "t" must be a subkind of value mod logical
         because of the annotation on the declaration of the type t.
       It is not logical: t is recursive but not [@@inductive].
|}]

(* A [with type] constraint may not forge the claim either. *)
module type Hidden = sig
  type u : value mod logical
  type t = Roll of u
end
module Constraint_forge = struct
  type bad = Loop of (bad -> int)
  module type S = Hidden with type u = bad -> int
end;;
[%%expect{|
module type Hidden = sig type u : value mod logical type t = Roll of u end
Line 7, characters 18-49:
7 |   module type S = Hidden with type u = bad -> int
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "u"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type u = bad -> int
       is not included in
         type u : value mod logical
       The kind of the first is value non_float mod aliased immutable
         because it's a function type.
       But the kind of the first must be a subkind of value mod logical
         because of the definition of u at line 2, characters 2-28.
       The first is not logical: bad is recursive but not [@@inductive].
|}]

(* Round-trip through a compiled interface: a client matches a container that
   holds another module's abstract logical type. *)
module Producer : sig
  type u : value mod logical
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
module Producer :
  sig type u : value mod logical type t = Hold of u val make : unit -> t end
module Client : sig val use : Producer.t -> int end
|}]
