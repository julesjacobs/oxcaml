(* TEST
   flags = "-w -a -extension refinement_types";
   expect;
*)

type nat = Z | S of nat [@@inductive]
[%%expect{|
type nat = Z | S of nat [@@inductive]
|}]
module Test_depth = struct
  let rec (depth @ total) n =
    match n with Z -> 0 | S smaller -> 1 + depth smaller
end
[%%expect{|
module Test_depth : sig val depth : nat -> int end
|}]

let answer = Test_depth.depth (S (S Z))
[%%expect{|
val answer : int = 2
|}]
module Test_inferred = struct
  let rec inferred n = match n with Z -> 0 | S x -> inferred x
  let (use_inferred @ total) n = inferred n
end
[%%expect{|
module Test_inferred :
  sig val inferred : nat -> int val use_inferred : nat -> int end
|}]
module Test_depth_acc = struct
  let rec (depth_acc @ total) acc n =
    match n with Z -> acc | S smaller -> depth_acc (acc + 1) smaller
end
[%%expect{|
module Test_depth_acc : sig val depth_acc : int -> nat -> int end
|}]
module Test_alias = struct
  let rec (alias @ total) n =
    match n with Z -> 0 | S smaller -> let next = smaller in alias next
end
[%%expect{|
module Test_alias : sig val alias : nat -> int end
|}]
module Test_same = struct
  let rec (same @ total) n = match n with Z -> 0 | S _ -> same n
end
[%%expect{|
Line 2, characters 58-64:
2 |   let rec (same @ total) n = match n with Z -> 0 | S _ -> same n
                                                              ^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
module Test_rebuild = struct
  let rec (rebuild @ total) n =
    match n with Z -> 0 | S smaller -> rebuild (S smaller)
end
[%%expect{|
Line 3, characters 39-58:
3 |     match n with Z -> 0 | S smaller -> rebuild (S smaller)
                                           ^^^^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
module Test_bad_or = struct
  let rec (bad_or @ total) n =
    match n with S x | (Z as x) -> bad_or x
end
[%%expect{|
Line 3, characters 35-43:
3 |     match n with S x | (Z as x) -> bad_or x
                                       ^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
module Test_good_or = struct
  let rec (good_or @ total) n =
    match n with Z -> 0 | S (S x) | S x -> good_or x
end
[%%expect{|
module Test_good_or : sig val good_or : nat -> int end
|}]
module Test_escape = struct
  let rec (escape @ total) n =
    let again = escape in
    match n with Z -> 0 | S smaller -> again smaller
end
[%%expect{|
Line 3, characters 16-22:
3 |     let again = escape in
                    ^^^^^^
Error: This recursive function cannot be total: the recursive function must be called directly.
|}]
module Test_delayed = struct
  let rec (delayed @ total) n =
    match n with
    | Z -> 0
    | S smaller -> let again () = delayed smaller in again ()
end
[%%expect{|
Line 5, characters 34-41:
5 |     | S smaller -> let again () = delayed smaller in again ()
                                      ^^^^^^^
Error: This recursive function cannot be total: the recursive function occurs in a delayed body.
|}]
module Test_noisy = struct
  let rec (noisy @ total) n =
    match n with Z -> 0 | S smaller -> print_endline "step"; noisy smaller
end
[%%expect{|
Line 3, characters 39-52:
3 |     match n with Z -> 0 | S smaller -> print_endline "step"; noisy smaller
                                           ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-3, characters 26-74
         which is expected to be "total".
|}]
module Test_partial = struct
  let rec partial n = match n with Z -> 0 | S _ -> partial n
  let (bad_partial @ total) n = partial n
end
[%%expect{|
Line 3, characters 32-39:
3 |   let (bad_partial @ total) n = partial n
                                    ^^^^^^^
Error: The value "partial" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 3, characters 28-41
         which is expected to be "total".
|}]
module Test_numeric = struct
  let rec (numeric @ total) n = if n = 0 then 0 else numeric (n - 1)
end
[%%expect{|
Line 2, characters 11-68:
2 |   let rec (numeric @ total) n = if n = 0 then 0 else numeric (n - 1)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: no parameter has a checked inductive datatype.
|}]

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree [@@inductive]
[%%expect{|
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree [@@inductive]
|}]
module Test_size = struct
  let rec (size @ total) tree =
    match tree with Leaf _ -> 1 | Node (left, right) -> 1 + size left + size right
end
[%%expect{|
module Test_size : sig val size : 'a tree -> int end
|}]
module Test_branches = struct
  type branch = End | Fork of (int * (branch * branch)) [@@inductive]
  let rec (branches @ total) tree =
    match tree with
    | End -> 0
    | Fork (_, (left, right)) -> 1 + branches left + branches right
end
[%%expect{|
module Test_branches :
  sig
    type branch = End | Fork of (int * (branch * branch))
    [@@inductive]
    val branches : branch -> int
  end
|}]

let rec omega = S omega
[%%expect{|
Line 1, characters 16-23:
1 | let rec omega = S omega
                    ^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let child = S Z
let shared = Node (Leaf child, Leaf child)
[%%expect{|
val child : nat = S Z
val shared : nat tree = Node (Leaf (S Z), Leaf (S Z))
|}]

type t = End | T of (int * t) [@@inductive]
let rec pair = (0, T pair)
[%%expect{|
type t = End | T of (int * t) [@@inductive]
Line 2, characters 15-26:
2 | let rec pair = (0, T pair)
                   ^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x = let pair = (0, x) in T pair
[%%expect{|
Line 1, characters 12-39:
1 | let rec x = let pair = (0, x) in T pair
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

let rec x = let rec pair = (0, x) in T pair
[%%expect{|
Line 1, characters 12-43:
1 | let rec x = let rec pair = (0, x) in T pair
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]
module Test_cycle = struct
  type ordinary = O | C of ordinary
  let rec cycle = C cycle
  let rec (ordinary_depth @ total) n =
    match n with O -> 0 | C x -> ordinary_depth x
end
[%%expect{|
Line 5, characters 26-29:
5 |     match n with O -> 0 | C x -> ordinary_depth x
                              ^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 4-5, characters 35-49
         which is expected to be "total".
|}]

type bad_arrow = A of (unit -> bad_arrow) [@@inductive]
[%%expect{|
Line 1, characters 0-55:
1 | type bad_arrow = A of (unit -> bad_arrow) [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

type bad_negative = N of ((bad_negative -> int) -> int) [@@inductive]
[%%expect{|
Line 1, characters 0-69:
1 | type bad_negative = N of ((bad_negative -> int) -> int) [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

type bad_ref = R of bad_ref ref [@@inductive]
[%%expect{|
Line 1, characters 0-45:
1 | type bad_ref = R of bad_ref ref [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

type bad_lazy = L of bad_lazy lazy_t [@@inductive]
[%%expect{|
Line 1, characters 0-50:
1 | type bad_lazy = L of bad_lazy lazy_t [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

type bad_nested = B of bad_nested list [@@inductive]
[%%expect{|
Line 1, characters 0-52:
1 | type bad_nested = B of bad_nested list [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

type bad_inline = I of { child : bad_inline } [@@inductive]
[%%expect{|
Line 1, characters 0-59:
1 | type bad_inline = I of { child : bad_inline } [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: inline-record constructors are not supported.
|}]

type bad_mutable = M of { mutable child : bad_mutable } [@@inductive]
[%%expect{|
Line 1, characters 0-69:
1 | type bad_mutable = M of { mutable child : bad_mutable } [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: inline-record constructors are not supported.
|}]

type payload = P of (int -> int) | Next of payload [@@inductive]
[%%expect{|
type payload = P of (int -> int) | Next of payload [@@inductive]
|}]

type 'a bad_gadt = G : int bad_gadt [@@inductive]
[%%expect{|
Line 1, characters 0-49:
1 | type 'a bad_gadt = G : int bad_gadt [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: GADT constructors are not supported.
|}]

type 'a bad_regular = BR of ('a * 'a) bad_regular [@@inductive]
[%%expect{|
Line 1, characters 0-63:
1 | type 'a bad_regular = BR of ('a * 'a) bad_regular [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursion must use unchanged type parameters.
|}]

type first = First of second [@@inductive]
and second = Second of first
[%%expect{|
Line 1, characters 0-42:
1 | type first = First of second [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: mutually declared types are not supported.
|}]

type bad_abstract [@@inductive]
[%%expect{|
Line 1, characters 0-31:
1 | type bad_abstract [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: a closed variant declaration is required.
|}]

type bad_open = .. [@@inductive]
[%%expect{|
Line 1, characters 0-32:
1 | type bad_open = .. [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: a closed variant declaration is required.
|}]
module Test_alias_depth = struct
  type 'a identity = 'a
  type alias_nat = AZ | AS of alias_nat identity [@@inductive]
  let rec (alias_depth @ total) n =
    match n with AZ -> 0 | AS x -> alias_depth x
end
[%%expect{|
module Test_alias_depth :
  sig
    type 'a identity = 'a
    type alias_nat = AZ | AS of alias_nat identity
    [@@inductive]
    val alias_depth : alias_nat identity -> int
  end
|}]

module Ordinary = struct type t = Z | S of t end
module Inductive = struct type t = Z | S of t [@@inductive] end
[%%expect{|
module Ordinary : sig type t = Z | S of t end
module Inductive : sig type t = Z | S of t [@@inductive] end
|}]

module Forge : sig type t = Z | S of t [@@inductive] end = Ordinary
[%%expect{|
Line 1, characters 59-67:
1 | module Forge : sig type t = Z | S of t [@@inductive] end = Ordinary
                                                               ^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = Ordinary.t = Z | S of t end
       is not included in
         sig type t = Z | S of t [@@inductive] end
       Type declarations do not match:
         type t = Ordinary.t = Z | S of t
       is not included in
         type t = Z | S of t
       [@@inductive]
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

module Drop : sig type t = Z | S of t end = Inductive
[%%expect{|
Line 1, characters 44-53:
1 | module Drop : sig type t = Z | S of t end = Inductive
                                                ^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = Inductive.t = Z | S of t [@@inductive] end
       is not included in
         sig type t = Z | S of t end
       Type declarations do not match:
         type t = Inductive.t = Z | S of t
       [@@inductive]
       is not included in
         type t = Z | S of t
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

module Hide : sig type t end = Inductive
[%%expect{|
module Hide : sig type t end
|}]

type forged = Ordinary.t = Z | S of forged [@@inductive]
[%%expect{|
Line 1, characters 0-56:
1 | type forged = Ordinary.t = Z | S of forged [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "Ordinary.t"
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

type dropped = Inductive.t = Z | S of dropped
[%%expect{|
Line 1, characters 0-45:
1 | type dropped = Inductive.t = Z | S of dropped
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "Inductive.t"
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

type reexport = Inductive.t = Z | S of reexport [@@inductive]
[%%expect{|
type reexport = Inductive.t = Z | S of reexport [@@inductive]
|}]

module type I = sig type t = Z | S of t [@@inductive] end
module type O = sig type t = Z | S of t end
module type Bad_with = I with type t = Ordinary.t
[%%expect{|
module type I = sig type t = Z | S of t [@@inductive] end
module type O = sig type t = Z | S of t end
Line 3, characters 30-49:
3 | module type Bad_with = I with type t = Ordinary.t
                                  ^^^^^^^^^^^^^^^^^^^
Error: This constraint requires a type with a checked inductive guarantee.
|}]

module type Bad_drop = O with type t = Inductive.t
[%%expect{|
Line 1, characters 30-50:
1 | module type Bad_drop = O with type t = Inductive.t
                                  ^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "Inductive.t"
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

module type Bad_subst = I with type t := Ordinary.t
[%%expect{|
Line 1, characters 31-51:
1 | module type Bad_subst = I with type t := Ordinary.t
                                   ^^^^^^^^^^^^^^^^^^^^
Error: This constraint requires a type with a checked inductive guarantee.
|}]

module type Bad_subst_drop = O with type t := Inductive.t
[%%expect{|
Line 1, characters 36-57:
1 | module type Bad_subst_drop = O with type t := Inductive.t
                                        ^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         "Inductive.t"
       Their inductive guarantees differ;
       the guarantee can only be hidden
       behind an abstract type.
|}]

module type Good_with = I with type t = Inductive.t
module type Good_subst = I with type t := Inductive.t
[%%expect{|
module type Good_with =
  sig type t = Inductive.t = Z | S of t [@@inductive] end
module type Good_subst = sig end
|}]

module V = struct
  module List = struct
    type 'a t = Nil | Cons of 'a * 'a t [@@inductive]
    let rec (length @ total) xs =
      match xs with Nil -> 0 | Cons (_, tail) -> 1 + length tail
    let rec (map @ total) f xs =
      match xs with Nil -> Nil | Cons (x, tail) -> Cons (f x, map f tail)
  end
end
let n = V.List.length (V.List.Cons (42, V.List.Nil))
let mapped = V.List.map (fun x -> 1 / x) (V.List.Cons (42, V.List.Nil))
[%%expect{|
module V :
  sig
    module List :
      sig
        type 'a t = Nil | Cons of 'a * 'a t
        [@@inductive]
        val length : 'a t -> int
        val map : ('a -> 'b) -> 'a t -> 'b t
      end
  end
val n : int = 1
val mapped : int V.List.t = V.List.Cons (0, V.List.Nil)
|}]
module Test_shadow = struct
  let rec (shadow @ total) n =
    match n with Z -> 0 | S x -> let shadow (_ : Inductive.t) = 1 in shadow x
end
[%%expect{|
module Test_shadow : sig val shadow : reexport -> int end
|}]
module Test_partial_apply = struct
  let rec (partial_apply @ total) acc n =
    match n with
    | Z -> acc
    | S smaller -> let f = partial_apply acc in f smaller
end
[%%expect{|
Line 5, characters 27-44:
5 |     | S smaller -> let f = partial_apply acc in f smaller
                               ^^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: recursive calls must supply every value parameter.
|}]
module Test_as_root = struct
  let rec (as_root @ total) n =
    match n with Z -> 0 | (S _ as whole) -> as_root whole
end
[%%expect{|
Line 3, characters 44-57:
3 |     match n with Z -> 0 | (S _ as whole) -> as_root whole
                                                ^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
module Test_as_child = struct
  let rec (as_child @ total) n =
    match n with Z -> 0 | S ((Z | S _) as child) -> as_child child
end
[%%expect{|
module Test_as_child : sig val as_child : reexport -> int end
|}]
module Test_branch_merge = struct
  let rec (branch_merge @ total) n =
    match n with
    | Z -> 0
    | S child -> let x = if true then child else child in branch_merge x
end
[%%expect{|
Line 5, characters 58-72:
5 |     | S child -> let x = if true then child else child in branch_merge x
                                                              ^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive argument is not a known proper descendant.
|}]
module Test_stored = struct
  let rec (stored @ total) n =
    match n with Z -> 0 | S child -> let f, x = stored, child in f x
end
[%%expect{|
Line 3, characters 48-54:
3 |     match n with Z -> 0 | S child -> let f, x = stored, child in f x
                                                    ^^^^^^
Error: This recursive function cannot be total: the recursive function must be called directly.
|}]
module Test_lazy_call = struct
  let rec (lazy_call @ total) n =
    match n with Z -> 0 | S child -> let _ = lazy (lazy_call child) in 0
end
[%%expect{|
Line 3, characters 51-60:
3 |     match n with Z -> 0 | S child -> let _ = lazy (lazy_call child) in 0
                                                       ^^^^^^^^^
Error: This recursive function cannot be total: the recursive function occurs in a delayed body.
|}]
module Test_type_call = struct
  let rec (type_call @ total) (n @ immutable) =
    match n with
    | Z -> true
    | S child ->
        let value = true in
        let x : {x : bool | type_call child} = refine_ value in
        let refine_ x = x in x
end
[%%expect{|
Line 7, characters 12-13:
7 |         let x : {x : bool | type_call child} = refine_ value in
                ^
Error: This recursive function cannot be total: the recursive function occurs in a type predicate.
|}]
module Test_mutable_read = struct
  let rec (mutable_read @ total) r n =
    match n with Z -> !r | S child -> mutable_read r child
end
[%%expect{|
Line 3, characters 22-23:
3 |     match n with Z -> !r | S child -> mutable_read r child
                          ^
Error: The value "(!)" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-3, characters 33-58
         which is expected to be "total".
|}]
module Test_allocating = struct
  let rec (allocating @ total) n =
    match n with Z -> ref 0 | S child -> allocating child
end
[%%expect{|
module Test_allocating : sig val allocating : reexport -> int ref end
|}]
module Test_labelled = struct
  let rec (labelled @ total) ~n =
    match n with Z -> 0 | S child -> labelled ~n:child
end
[%%expect{|
Line 2, characters 29-31:
2 |   let rec (labelled @ total) ~n =
                                 ^^
Error: This recursive function cannot be total: structural recursion requires simple unlabelled parameters.
|}]
module Test_pattern_param = struct
  let rec (pattern_param @ total) (S child) = pattern_param child
end
[%%expect{|
Line 2, characters 34-43:
2 |   let rec (pattern_param @ total) (S child) = pattern_param child
                                      ^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 34-65
         which is expected to be "total".
|}]
module Test_mutual = struct
  let rec (mutual @ total) n = match n with Z -> 0 | S x -> other x
  and other n = match n with Z -> 0 | S x -> mutual x
end
[%%expect{|
Line 2, characters 60-65:
2 |   let rec (mutual @ total) n = match n with Z -> 0 | S x -> other x
                                                                ^^^^^
Error: The value "other" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 27-67
         which is expected to be "total".
|}]

module F (X : sig type t = Z | S of t [@@inductive] end) = struct
  let rec (depth @ total) n = match n with X.Z -> 0 | X.S x -> depth x
end
module Applied = F (Inductive)
module Invalid = F (Ordinary)
[%%expect{|
module F :
  functor (X : sig type t = Z | S of t [@@inductive] end) ->
    sig val depth : X.t -> int end
module Applied : sig val depth : Inductive.t -> int end
Line 5, characters 17-29:
5 | module Invalid = F (Ordinary)
                     ^^^^^^^^^^^^
Error: Modules do not match: sig type t = Ordinary.t = Z | S of t end
     is not included in sig type t = Z | S of t [@@inductive] end
     Type declarations do not match:
       type t = Ordinary.t = Z | S of t
     is not included in
       type t = Z | S of t
     [@@inductive]
     Their inductive guarantees differ;
     the guarantee can only be hidden
     behind an abstract type.
|}]
module Test_named_depth = struct
  module Alias = Inductive
  type named = Alias.t
  let rec (named_depth @ total) (n : named) =
    match n with Alias.Z -> 0 | Alias.S x -> named_depth x
end
[%%expect{|
module Test_named_depth :
  sig
    module Alias = Inductive
    type named = Alias.t
    val named_depth : named -> int
  end
|}]

type holder = Holder of (unit -> holder) [@@inductive]
[%%expect{|
Line 1, characters 0-54:
1 | type holder = Holder of (unit -> holder) [@@inductive]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]

let rec callback () = P (fun _ -> match callback () with _ -> 0)
[%%expect{|
val callback : unit -> payload = <fun>
|}]

let rec payload_cycle = P (fun _ -> match payload_cycle with _ -> 0)
[%%expect{|
Line 1, characters 24-68:
1 | let rec payload_cycle = P (fun _ -> match payload_cycle with _ -> 0)
                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

module Test_recursive_letop = struct
  let rec (( let* ) @ total) (n : nat) (f : nat -> int) =
    let* x = n in f x
end
[%%expect{|
Line 3, characters 4-21:
3 |     let* x = n in f x
        ^^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive function cannot be used as a binding operator.
|}, Principal{|
Line 3, characters 4-8:
3 |     let* x = n in f x
        ^^^^
Error: This value is "immutable"
         because it is used inside the function at lines 2-3, characters 29-21
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "read_write".
|}]

module Test_recursive_andop = struct
  let (let*) x f = f x
  let rec (( and* ) @ total) (n : nat) m =
    let* x = n and* y = m in (x, y)
end
[%%expect{|
Line 4, characters 9-25:
4 |     let* x = n and* y = m in (x, y)
             ^^^^^^^^^^^^^^^^
Error: This recursive function cannot be total: the recursive function cannot be used as a binding operator.
|}, Principal{|
Line 4, characters 15-19:
4 |     let* x = n and* y = m in (x, y)
                   ^^^^
Error: This value is "immutable"
         because it is used inside the function at lines 3-4, characters 29-35
         which is expected to be "stateless".
       However, the highlighted expression is expected to be "read_write".
|}]

type 'a shared_identity = 'a
type bad_shared = B of bad_shared shared_identity *
  (bad_shared shared_identity -> int) [@@inductive]
[%%expect{|
type 'a shared_identity = 'a
Lines 2-3, characters 0-51:
2 | type bad_shared = B of bad_shared shared_identity *
3 |   (bad_shared shared_identity -> int) [@@inductive]
Error: Invalid inductive declaration: recursive occurrences must be direct fields or tuple components.
|}]
