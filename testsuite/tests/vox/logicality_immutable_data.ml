(* TEST
 expect;
*)

(* [immutable_data] does not imply [mod logical]. A non-inductive recursive
   type is immutable data, so it implements [type t : immutable_data] as in
   upstream OxCaml. Its values may be cyclic, so it is not logical: it does
   not implement [type t : logical_data] (the abbreviation for
   [immutable_data mod logical]), and total code may not match it. *)

module Data : sig
  type t : immutable_data
end = struct
  type t = Nil | Cons of int * t
end;;
[%%expect{|
module Data : sig type t : immutable_data end
|}]

module Logical : sig
  type t : logical_data
end = struct
  type t = Nil | Cons of int * t
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = Nil | Cons of int * t
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Nil | Cons of int * t end
       is not included in
         sig type t : logical_data end
       Type declarations do not match:
         type t = Nil | Cons of int * t
       is not included in
         type t : logical_data
       The kind of the first is immutable_data
         because of the definition of t at line 4, characters 2-32.
       But the kind of the first must be a subkind of logical_data
         because of the definition of t at line 2, characters 2-23.
       The first is not logical: t is recursive but not [@@inductive].
|}]

type t = Nil | Cons of int * t
let (head @ total) (x : t) : int = match x with Nil -> 0 | Cons (n, _) -> n;;
[%%expect{|
type t = Nil | Cons of int * t
Line 2, characters 59-70:
2 | let (head @ total) (x : t) : int = match x with Nil -> 0 | Cons (n, _) -> n;;
                                                               ^^^^^^^^^^^
Error: The match on a value whose type is not logical (t is recursive but not [@@inductive]) is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 19-75
         which is expected to be "total".
|}]

(* With [@@inductive] the same list is logical. *)
module Inductive : sig
  type t : logical_data
end = struct
  type t = Nil | Cons of int * t [@@inductive]
end;;
[%%expect{|
module Inductive : sig type t : logical_data end
|}]

(* [mod logical] composes with with-bounds: ['a box] is logical when ['a]
   is, and [int -> int] (a function between logical types) is logical as a
   field. *)
module Box : sig
  type 'a box : logical_data with 'a
end = struct
  type 'a box = Box of 'a
end
type wrapped = Wrapped of int Box.box
let (unwrap @ total) (Wrapped b : wrapped) = b
type functions = { f : int -> int }
let (call @ total) (r : functions) = r.f;;
[%%expect{|
module Box : sig type 'a box : logical_data with 'a end
type wrapped = Wrapped of int Box.box
val unwrap : wrapped -> int Box.box = <fun>
type functions = { f : int -> int; }
val call : functions -> int -> int = <fun>
|}]

(* [immediate] and [float] are logical; so is [value mod everything]. *)
type i : value mod logical = int
type f : value mod logical = float;;
[%%expect{|
type i = int
type f = float
|}]

(* [logical_data] is the abbreviation for [immutable_data mod logical]: each
   implements the other. *)
module Spelled_out : sig
  type a : logical_data
  type b : immutable_data mod logical
end = struct
  type a : immutable_data mod logical
  type b : logical_data
end;;
[%%expect{|
module Spelled_out : sig type a : logical_data type b : logical_data end
|}]

(* [logical_data] is included in [immutable_data], not the reverse. *)
module Narrower : sig type t : immutable_data end = struct
  type t : logical_data
end;;
[%%expect{|
module Narrower : sig type t : immutable_data end
|}]

module Wider : sig type t : logical_data end = struct
  type t : immutable_data
end;;
[%%expect{|
Lines 1-3, characters 47-3:
1 | ...............................................struct
2 |   type t : immutable_data
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t : immutable_data end
       is not included in
         sig type t : logical_data end
       Type declarations do not match:
         type t : immutable_data
       is not included in
         type t : logical_data
       The kind of the first is immutable_data
         because of the definition of t at line 2, characters 2-25.
       But the kind of the first must be a subkind of logical_data
         because of the definition of t at line 1, characters 19-40.
       The first is not logical:
       t is abstract and its kind does not say mod logical.
|}]

(* [logical_data] combines with modifiers and with-bounds. *)
module type Composed = sig
  type a : logical_data mod global
  type 'x b : logical_data with 'x
  type 'x c : logical_data mod global with 'x
end;;
[%%expect{|
module type Composed =
  sig
    type a : logical_data mod global unforkable yielding
    type 'x b : logical_data with 'x
    type 'x c : logical_data mod global unforkable yielding with 'x
  end
|}]

