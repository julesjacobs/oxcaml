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
Lines 1-4, characters 28-3:
1 | ............................sig
2 |   type t
3 |   val identity : t -> t @@ total
4 | end.........
Error: The type of total value "identity" depends on the current recursive module group.
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
Lines 1-4, characters 29-3:
1 | .............................sig
2 |   module Inner : sig type t end
3 |   val identity : Inner.t -> Inner.t @@ total
4 | end.........
Error: The type of total value "identity" depends on the current recursive module group.
|}]

module rec Fresh_nominal : sig
  type t = T
  val identity : t -> t @@ total
end = struct
  type t = T
  let (identity @ total) x = x
end
[%%expect{|
Lines 1-4, characters 27-3:
1 | ...........................sig
2 |   type t = T
3 |   val identity : t -> t @@ total
4 | end.........
Error: The type of total value "identity" depends on the current recursive module group.
|}]
