(* TEST
   {
     flags = "-w -a -error-style short -extension refinement_types -extension module_strengthening";
     expect;
   }{
     flags = "-w -a -error-style short -extension refinement_types -extension module_strengthening -principal";
     expect;
   }
*)

module _ = struct
  type 'a ordinary = { value : 'a }

  module rec Domain_only : sig
    type t
    val function_ : unit -> (t -> int @ total)
    val tuple : (t -> unit) * (unit -> int @ total)
    val variant : [ `Domain of (t -> unit) * (unit -> int @ total) ]
    val record : t ordinary
    val object_ :
      < consume : t -> unit; produce : unit -> int @ total >
    class class_ : object
      method consume : t -> unit
      method produce : unit -> int @ total
    end
    exception Extension of (t -> unit) * (unit -> int @ total)
  end = struct
    type t = unit
    let (produce @ total) () = 0
    let function_ () = fun _ -> 0
    let tuple = (fun _ -> ()), produce
    let variant = `Domain ((fun _ -> ()), produce)
    let record = { value = () }
    let object_ = object
      method consume _ = ()
      method produce : unit -> int @ total = produce
    end
    class class_ = object
      method consume (_ : t) = ()
      method produce : unit -> int @ total = produce
    end
    exception Extension of (t -> unit) * (unit -> int @ total)
  end
end
[%%expect{|
|}]

module _ = struct
  type _ exposed = Exposed : 'a @@ total -> 'a list exposed

  module rec Gadt : sig
    type t
    val get : unit -> t list exposed
  end = struct
    type t = unit
    let get () = Exposed ()
  end
end
[%%expect{|
Line 6, characters 4-36:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module _ = struct
  type (_, _) eq = Refl : ('a, 'a) eq
  type 'b hidden = Hidden : ('a, 'b) eq * 'a @@ total -> 'b hidden

  module rec Relational_gadt : sig
    type t
    val get : unit -> t hidden
  end = struct
    type t = unit
    let get () = Hidden (Refl, ())
  end
end
[%%expect{|
Line 7, characters 4-30:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module _ = struct
  type 'a exposed = { exposed : 'a @@ total }

  module rec Private_alias : sig
    type t
    type wrapped = private t exposed
    val get : unit -> wrapped
  end = struct
    type t = unit
    type wrapped = t exposed
    let get () = { exposed = () }
  end
end
[%%expect{|
Line 7, characters 4-29:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module _ = struct
  module Hidden : sig
    type 'a t
    val make : 'a @ total -> 'a t
    val get : 'a t -> 'a @ total
  end = struct
    type 'a t = Hidden of 'a @@ total
    let make x = Hidden x
    let get (Hidden x) = x
  end

  module rec Opaque : sig
    type t
    val get : unit -> t Hidden.t
  end = struct
    type t = unit
    let get () = Hidden.make ()
  end
end
[%%expect{|
Line 14, characters 4-32:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module _ = struct
  module Hidden (Argument : sig type t end) : sig
    type t
    val get : t -> Argument.t @ total
  end = struct
    type t = unit
    external get : t -> Argument.t @ total = "%identity"
  end

  module rec Applicative : sig
    type t
    val get : unit -> Hidden(Applicative).t
  end = struct
    type t = unit
    let get () = failwith "unreachable"
  end
end
[%%expect{|
Line 12, characters 4-43:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module _ = struct
  module rec Sibling : sig
    type t
  end = struct
    type t = unit
  end
  and Dependent_parameter : functor
    (Argument : sig type t = Sibling.t end) ->
    sig val get : unit -> Argument.t end =
  functor (Argument : sig type t = Sibling.t end) -> struct
    let get () = failwith "unreachable"
  end
end
[%%expect{|
Lines 8-9, characters 4-40:
Error: The signature item "Argument" depends on the current recursive module group in a form that is not allowed in a recursive module signature.
|}]

type (_, _) structural_eq = Refl : ('a, 'a) structural_eq
type 'a exposed = { exposed : 'a @@ total }

module rec Structural_tuple : sig
  type t
  val get :
    unit -> ((t, int) structural_eq * (unit -> int @ total))
end = struct
  type t = int
  let get () = Refl, (fun () -> 0)
end
[%%expect{|
type (_, _) structural_eq = Refl : ('a, 'a) structural_eq
type 'a exposed = { exposed : 'a @@ total; }
Lines 6-7, characters 2-60:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Structural_variant : sig
  type t
  val get :
    unit ->
      [ `Hidden of (t, int) structural_eq * (unit -> int @ total) ]
end = struct
  type t = int
  let get () = `Hidden (Refl, fun () -> 0)
end
[%%expect{|
Lines 3-5, characters 2-67:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Structural_object : sig
  type t
  val get :
    unit ->
      < eq : (t, int) structural_eq; value : unit -> int @ total >
end = struct
  type t = int
  let get () = object
    method eq = Refl
    method value () = 0
  end
end
[%%expect{|
Lines 3-5, characters 2-66:
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Structural_class : sig
  type t
  class getter : object
    method eq : (t, int) structural_eq
    method value : int exposed
  end
end = struct
  type t = int
  class getter = object
    method eq : (t, int) structural_eq = Refl
    method value = { exposed = 0 }
  end
end
[%%expect{|
Lines 3-6, characters 2-5:
Error: The signature item "getter" depends on the current recursive module group in a form that is not allowed in a recursive module signature.
|}]

module rec Class_variable : sig
  type t
  class holder : object
    val payload : t exposed
  end
end = struct
  type t = unit
  class holder = object
    val payload = { exposed = () }
  end
end
[%%expect{|
Lines 3-5, characters 2-5:
Error: The signature item "holder" depends on the current recursive module group in a form that is not allowed in a recursive module signature.
|}]

module rec Class_variable_sibling : sig
  type t
  class holder : object
    val eq : (t, int) structural_eq
    method value : int exposed
  end
end = struct
  type t = int
  class holder = object
    val eq : (int, int) structural_eq = Refl
    method value = { exposed = 0 }
  end
end
[%%expect{|
Lines 3-6, characters 2-5:
Error: The signature item "holder" depends on the current recursive module group in a form that is not allowed in a recursive module signature.
|}]

module rec Structural_extension : sig
  type t
  exception Hidden of (t, int) structural_eq * int @@ total
end = struct
  type t = int
  exception Hidden of (t, int) structural_eq * int @@ total
end
[%%expect{|
Line 3, characters 2-59:
Error: The signature item "Hidden" depends on the current recursive module group in a form that is not allowed in a recursive module signature.
|}]

module rec Extension_domain : sig
  type t
  exception Domain of (t -> int) @@ total
end = struct
  type t = unit
  exception Domain of (t -> int) @@ total
end
[%%expect{|
module rec Extension_domain :
  sig type t exception Domain of (t -> int) @@ total end
|}]

module type Strengthened_signature = sig val value : int end

module rec Strengthened_base : Strengthened_signature = struct
  let value = 0
end
and Strengthened_copy : Strengthened_signature with Strengthened_base =
  Strengthened_base
[%%expect{|
module type Strengthened_signature = sig val value : int end
Line 6, characters 52-69:
Error: This module type is recursive.
       This use of the recursive module "Strengthened_base"
       within the definition of the module "Strengthened_copy"
       makes the module type of "Strengthened_copy" depend on
       the module type of "Strengthened_base".
       Such recursive definitions of module types are not allowed.
|}]
