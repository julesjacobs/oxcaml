(* TEST
   flags = "-w -a -extension refinement_types";
   expect;
*)

type existing_inductive = Existing [@@inductive]
module rec Existing_reference : sig
  val values : existing_inductive list
end = struct
  let values = []
end
[%%expect{|
type existing_inductive = Existing [@@inductive]
module rec Existing_reference : sig val values : existing_inductive list end
|}]

module rec Introduced_guarantee : sig
  type t = T [@@inductive]
  val values : t list
end = struct
  type t = T
  let values = []
end
[%%expect{|
Lines 1-4, characters 34-3:
1 | ..................................sig
2 |   type t = T [@@inductive]
3 |   val values : t list
4 | end.........
Error: Type "t" has an [@@inductive] guarantee, which is not allowed in a recursive module signature.
|}]

module rec Aliased_guarantee : sig
  type t = existing_inductive
end = struct
  type t = existing_inductive
end
[%%expect{|
module rec Aliased_guarantee : sig type t = existing_inductive end
|}]

type 'a existing_container = Empty | Item of 'a * 'a existing_container
[@@inductive]

module rec Current_payload : sig
  type t
  val values : t existing_container
end = struct
  type t = unit
  let values = Empty
end
[%%expect{|
type 'a existing_container = Empty | Item of 'a * 'a existing_container
[@@inductive]
module rec Current_payload : sig type t val values : t existing_container end
|}]

module rec Partial_values : sig
  type t
  val values : t list
  val functions : (t -> int) list
end = struct
  type t = unit
  let values = []
  let functions = []
end
[%%expect{|
module rec Partial_values :
  sig type t val values : t list val functions : (t -> int) list end
|}]

module rec Total_identity : sig
  type t
  val identity : t -> t @@ total
end = struct
  type t = unit
  let (identity @ total) x = x
end
[%%expect{|
Line 3, characters 2-32:
3 |   val identity : t -> t @@ total
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "identity" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Transparent_alias : sig
  type t = int
  val identity : t -> t @@ total
end = struct
  type t = int
  let (identity @ total) x = x
end
[%%expect{|
module rec Transparent_alias :
  sig type t = int val identity : t -> t @@ total end
|}]

module rec Nested_abstract : sig
  module Inner : sig type t end
  val identity : Inner.t -> Inner.t @@ total
end = struct
  module Inner = struct type t = int end
  let (identity @ total) x = x
end
[%%expect{|
Line 3, characters 2-44:
3 |   val identity : Inner.t -> Inner.t @@ total
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "identity" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Fresh_nominal : sig
  type t = T
  val identity : t -> t @@ total
end = struct
  type t = T
  let (identity @ total) x = x
end
[%%expect{|
Line 3, characters 2-32:
3 |   val identity : t -> t @@ total
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "identity" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Returned_total : sig
  type t
  val get_pack : unit -> ((t -> int) -> t) @ total
  val get_apply : unit -> (t -> t -> int) @ total
end = struct
  type t = unit
  let get_pack () = fun _ -> ()
  let get_apply () = fun _ _ -> 0
end
[%%expect{|
Line 3, characters 2-50:
3 |   val get_pack : unit -> ((t -> int) -> t) @ total
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "get_pack" exposes a total value whose type depends on the current recursive module group.
|}]

type 'a exposed_record = { exposed_record : 'a @@ total }
type 'a exposed_variant = Exposed_variant of 'a @@ total
type 'a exposed_alias = 'a exposed_record
type 'a ordinary_record = { ordinary_record : 'a }
type 'a total_function_record = { run : ('a -> int) @@ total }

module rec Instantiated_fields : sig
  type t
  val get : unit -> int exposed_record * t exposed_record
end = struct
  type t = unit
  let get () = { exposed_record = 0 }, { exposed_record = () }
end
[%%expect{|
type 'a exposed_record = { exposed_record : 'a @@ total; }
type 'a exposed_variant = Exposed_variant of 'a @@ total
type 'a exposed_alias = 'a exposed_record
type 'a ordinary_record = { ordinary_record : 'a; }
type 'a total_function_record = { run : 'a -> int @@ total; }
Line 9, characters 2-57:
9 |   val get : unit -> int exposed_record * t exposed_record
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Nested_field : sig
  type t
  val get : unit -> t total_function_record ordinary_record
end = struct
  type t = unit
  let get () = { ordinary_record = { run = fun _ -> 0 } }
end
[%%expect{|
Line 3, characters 2-59:
3 |   val get : unit -> t total_function_record ordinary_record
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Ordinary_crossing : sig
  type t
  val get : unit -> t ordinary_record
end = struct
  type t = unit
  let get () = { ordinary_record = () }
end
[%%expect{|
module rec Ordinary_crossing :
  sig type t val get : unit -> t ordinary_record end
|}]

module rec Aliased_field : sig
  type t
  val get : unit -> t exposed_alias
end = struct
  type t = unit
  let get () = { exposed_record = () }
end
[%%expect{|
Line 3, characters 2-35:
3 |   val get : unit -> t exposed_alias
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Instantiated_variant : sig
  type t
  val get : unit -> t exposed_variant
end = struct
  type t = unit
  let get () = Exposed_variant ()
end
[%%expect{|
Line 3, characters 2-37:
3 |   val get : unit -> t exposed_variant
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

type ('a, 'b) rotating =
  | Stop of 'a @@ total
  | Next of ('b, 'a) rotating

module rec Rotating_fields : sig
  type t
  val get : unit -> (int, t) rotating
end = struct
  type t = unit
  let get () = Next (Stop ())
end
[%%expect{|
type ('a, 'b) rotating = Stop of 'a @@ total | Next of ('b, 'a) rotating
Line 7, characters 2-37:
7 |   val get : unit -> (int, t) rotating
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "get" exposes a total value whose type depends on the current recursive module group.
|}]

module rec Sibling : sig
  type t
end = struct
  type t = unit
end
and Functor_alias : functor
  (Argument : sig type t = Sibling.t end) ->
  sig val identity : Argument.t -> Argument.t @@ total end =
functor (Argument : sig type t = Sibling.t end) -> struct
  let (identity @ total) x = x
end
[%%expect{|
Lines 7-8, characters 2-58:
7 | ..(Argument : sig type t = Sibling.t end) ->
8 |   sig val identity : Argument.t -> Argument.t @@ total end..
Error: The signature item "Argument" depends on the current recursive module group in a form that is not allowed in a recursive module signature.
|}]

module rec Independent_left : sig
  type t
end = struct
  type t = unit
end
and Independent_right : sig
  module Nested : sig val identity : int -> int @@ total end
  module Make : functor (_ : sig type t end) ->
    sig val identity : int -> int @@ total end
end = struct
  module Nested = struct let (identity @ total) x = x end
  module Make (_ : sig type t end) = Nested
end
[%%expect{|
module rec Independent_left : sig type t end
and Independent_right :
  sig
    module Nested : sig val identity : int -> int @@ total end
    module Make :
      sig type t end -> sig val identity : int -> int @@ total end
  end
|}]
