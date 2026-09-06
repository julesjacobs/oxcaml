(* TEST
 has-z3;
 flags = "-extension refinement_types";
 expect;
*)

external gt : int -> int -> bool @@ total = "%greaterthan"
external ge : int -> int -> bool @@ total = "%greaterequal"
external deref_total : int ref -> int @@ total = "%field0";;
[%%expect{|
external gt : int -> int -> bool = "%greaterthan"
external ge : int -> int -> bool = "%greaterequal"
external deref_total : int ref -> int = "%field0"
|}]

type positive = { x : int | gt x 0 };;
[%%expect{|
type positive = {x : int | gt x 0}
|}]

type positive_again = { y : int | gt y 0 };;
[%%expect{|
type positive_again = {y : int | gt y 0}
|}]

let same : positive list = ([] : positive_again list);;
[%%expect{|
val same : positive list = []
|}]

type local_binders =
  { x : int | let y = x in (fun z -> gt z y) x }
type local_binders_again =
  { a : int | let b = a in (fun c -> gt c b) a }
let local_binders_same : local_binders list =
  ([] : local_binders_again list);;
[%%expect{|
type local_binders = {x : int | let y = x in (fun z -> gt z y) x}
type local_binders_again = {a : int | let b = a in (fun c -> gt c b) a}
val local_binders_same : local_binders list = []
|}]

type match_binder =
  { x : int option | match x with Some y -> gt y 0 | None -> true }
type match_binder_again =
  { a : int option | match a with Some b -> gt b 0 | None -> true }
let match_binder_same : match_binder list =
  ([] : match_binder_again list);;
[%%expect{|
type match_binder =
    {x : int option | match x with | Some y -> gt y 0 | None -> true}
type match_binder_again =
    {a : int option | match a with | Some b -> gt b 0 | None -> true}
val match_binder_same : match_binder list = []
|}]

type pair = Pair of int * int
type ordered_pair =
  { x : pair | match x with Pair (left, right) -> ge right left };;
[%%expect{|
type pair = Pair of int * int
type ordered_pair =
    {x : pair | match x with | Pair (left, right) -> ge right left}
|}]

type nested_functions = { x : int | (fun left right -> ge left right) x 0 };;
[%%expect{|
type nested_functions =
    {x : int | (fun left -> fun right -> ge left right) x 0}
|}]

external[@layout_poly] any_array_length :
  ('a : any mod separable).
  'a array @ immutable contended -> int @@ total = "%array_length"

let runtime_array_length x = any_array_length x

external[@layout_poly] refined_array_length :
  ('a : any mod separable).
  { a : 'a array | ge (any_array_length a) 0 } -> int @@ total
  = "%array_length";;
[%%expect{|
external any_array_length : ('a : any separable). 'a array @ immutable -> int
  = "%array_length" [@@layout_poly]
val runtime_array_length : ('a : value_maybe_null). 'a array -> int = <fun>
external refined_array_length :
  ('a : any separable). {a : 'a array | ge (any_array_length a) 0} -> int
  = "%array_length" [@@layout_poly]
|}]

type _ witness = Int : int witness
type 'a gadt_predicate =
  { x : 'a witness | match x with Int -> true };;
[%%expect{|
type _ witness = Int : int witness
Line 3, characters 34-37:
3 |   { x : 'a witness | match x with Int -> true };;
                                      ^^^
Error: A GADT constructor pattern is not yet supported in a refinement predicate
|}]

type local_scoping = { x : int | let y = x in gt y x }
type local_scoping_different = { x : int | let y = x in gt x y }
let local_scoping_not_same : local_scoping list =
  ([] : local_scoping_different list);;
[%%expect{|
type local_scoping = {x : int | let y = x in gt y x}
type local_scoping_different = {x : int | let y = x in gt x y}
Line 4, characters 2-37:
4 |   ([] : local_scoping_different list);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "local_scoping_different list"
       but an expression was expected of type "local_scoping list"
       Type "local_scoping_different" = "{x : int | let y = x in gt x y}"
       is not compatible with type
         "local_scoping" = "{x : int | let y = x in gt y x}"
|}]

type nonnegative = { x : int | ge x 0 };;
[%%expect{|
type nonnegative = {x : int | ge x 0}
|}]

let different : positive list = ([] : nonnegative list);;
[%%expect{|
Line 1, characters 32-55:
1 | let different : positive list = ([] : nonnegative list);;
                                    ^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "nonnegative list"
       but an expression was expected of type "positive list"
       Type "nonnegative" = "{x : int | ge x 0}" is not compatible with type
         "positive" = "{x : int | gt x 0}"
|}]

let one : positive = let raw = 1 in refine_ raw;;
[%%expect{|
val one : positive = 1
|}]

type impossible = { x : int | false }
let unchecked : impossible = let raw = 0 in refine_ raw;;
[%%expect{|
type impossible = {x : int | false}
Line 2, characters 44-55:
2 | let unchecked : impossible = let raw = 0 in refine_ raw;;
                                                ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

let no_implicit_unwrap : int = one;;
[%%expect{|
Line 1, characters 31-34:
1 | let no_implicit_unwrap : int = one;;
                                   ^^^
Error: The value "one" has type "positive" = "{x : int | gt x 0}"
       but an expression was expected of type "int"
|}]

let unwrapped = let refine_ x = one in x + 1;;
[%%expect{|
val unwrapped : int = 2
|}]

type refined_function_modes = { f : unit -> unit | true };;
[%%expect{|
type refined_function_modes = {f : unit -> unit | true}
|}]

let escape_unwrapped_local (x : refined_function_modes @ local)
    : (unit -> unit) @ global =
  let refine_ f = x in
  f;;
[%%expect{|
Line 4, characters 2-3:
4 |   f;;
      ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let call_unwrapped_once_twice (x : refined_function_modes @ once) =
  let refine_ f = x in
  f ();
  f ();;
[%%expect{|
Line 4, characters 2-3:
4 |   f ();;
      ^
Error: This value is used here,
       but it is defined as once and has already been used at:
Line 3, characters 2-3:
3 |   f ();
      ^

|}]

type greater_than_one =
  { y : int | let refine_ x = one in gt y x };;
[%%expect{|
type greater_than_one = {y : int | let refine_ x = one in gt y x}
|}]

type greater_than_one_again =
  { result : int | let refine_ lower = one in gt result lower }
let greater_than_one_same : greater_than_one list =
  ([] : greater_than_one_again list);;
[%%expect{|
type greater_than_one_again =
    {result : int | let refine_ lower = one in gt result lower}
val greater_than_one_same : greater_than_one list = []
|}]

type ordinary_unused_unpack = { y : int | let x = one in ignore x; true }
type refined_unused_unpack =
  { y : int | let refine_ x = one in ignore x; true }
let binding_kinds_differ : ordinary_unused_unpack list =
  ([] : refined_unused_unpack list);;
[%%expect{|
type ordinary_unused_unpack = {y : int | let x = one in ignore x; true}
type refined_unused_unpack =
    {y : int | let refine_ x = one in ignore x; true}
Line 5, characters 2-35:
5 |   ([] : refined_unused_unpack list);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "refined_unused_unpack list"
       but an expression was expected of type "ordinary_unused_unpack list"
       Type
         "refined_unused_unpack" =
           "{y : int | let refine_ x = one in ignore x; true}"
       is not compatible with type
         "ordinary_unused_unpack" = "{y : int | let x = one in ignore x; true}"
|}]

let inferred = refine_ 1;;
[%%expect{|
Line 1, characters 15-24:
1 | let inferred = refine_ 1;;
                   ^^^^^^^^^
Error: "refine_" requires a known refinement type from its context
|}]

let not_refined = let refine_ x = 1 in x;;
[%%expect{|
Line 1, characters 34-35:
1 | let not_refined = let refine_ x = 1 in x;;
                                      ^
Error: the right-hand side of "let refine_" must have a known refinement type
|}]

let partial () = print_endline "partial"; 1
type from_partial = { x : int | gt (partial ()) x };;
[%%expect{|
val partial : unit -> int = <fun>
Line 2, characters 36-43:
2 | type from_partial = { x : int | gt (partial ()) x };;
                                        ^^^^^^^
Error: The value "partial" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 32-49).
|}]

type not_bool = { x : int | x + 1 };;
[%%expect{|
Line 1, characters 28-33:
1 | type not_bool = { x : int | x + 1 };;
                                ^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

type constrained_predicate = { x : int | (gt x 0 : bool) };;
[%%expect{|
type constrained_predicate = {x : int | (gt x 0 : bool)}
|}]

type nested_refine =
  { x : int |
    let raw = false in
    let refine_ proof = (refine_ raw : {b : bool | b}) in
    proof };;
[%%expect{|
Line 4, characters 25-36:
4 |     let refine_ proof = (refine_ raw : {b : bool | b}) in
                             ^^^^^^^^^^^
Error: This expression annotation is not yet supported in a refinement predicate
|}]

type box = { value : int }
type positive_via_record =
  { x : int | let box = { value = x } in gt box.value 0 };;
[%%expect{|
type box = { value : int; }
type positive_via_record =
    {x : int | let box = { value = x } in gt box.value 0}
|}]

type allocates_mutable = { x : int | let _cell = ref 0 in true };;
[%%expect{|
Line 1, characters 49-52:
1 | type allocates_mutable = { x : int | let _cell = ref 0 in true };;
                                                     ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 1, characters 37-62).
|}]

type reads_ref = { r : int ref | ge (deref_total r) 0 };;
[%%expect{|
Line 1, characters 49-50:
1 | type reads_ref = { r : int ref | ge (deref_total r) 0 };;
                                                     ^
Error: This value is "immutable" but is expected to be "read_write".
|}]

type total_function = { f : unit -> unit | true };;
[%%expect{|
type total_function = {f : unit -> unit | true}
|}]

let wrapped_function : total_function = let raw = fun () -> () in refine_ raw;;
[%%expect{|
val wrapped_function : total_function = <fun>
|}]

let (stateless_wrapped @ stateless) = wrapped_function
let (portable_wrapped @ portable) = wrapped_function;;
[%%expect{|
val stateless_wrapped : total_function = <fun>
val portable_wrapped : total_function = <fun>
|}]

let partial_function : total_function =
  refine_ (fun () -> print_endline "partial");;
[%%expect{|
Line 2, characters 21-34:
2 |   refine_ (fun () -> print_endline "partial");;
                         ^^^^^^^^^^^^^
Error: The value "print_endline" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 10-45
         which is expected to be "total".
|}]

let (run_wrapped @ total) () =
  let refine_ f = wrapped_function in
  f ();;
[%%expect{|
val run_wrapped : unit -> unit = <fun>
|}]

type refined_ref = { r : int ref | true };;
[%%expect{|
type refined_ref = {r : int ref | true}
|}]

let (allocate_ref @ total) () = ref 0;;
[%%expect{|
Line 1, characters 32-35:
1 | let (allocate_ref @ total) () = ref 0;;
                                    ^^^
Error: The value "ref" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 1, characters 27-37
         which is expected to be "total".
|}]

let raw_ref = ref 0;;
[%%expect{|
val raw_ref : int ref = {contents = 0}
|}]

let wrapped_ref : refined_ref = refine_ raw_ref;;
[%%expect{|
val wrapped_ref : refined_ref = {contents = 0}
|}]

type refined_atomic = { a : int Atomic.t | true };;
[%%expect{|
type refined_atomic = {a : int Atomic.t | true}
|}]

let raw_atomic = Atomic.make 0;;
[%%expect{|
val raw_atomic : int Atomic.t = {Atomic.contents = 0}
|}]

let wrapped_atomic : refined_atomic = refine_ raw_atomic;;
[%%expect{|
val wrapped_atomic : refined_atomic = {Atomic.contents = 0}
|}]

type refined_list = { xs : int list | true };;
[%%expect{|
type refined_list = {xs : int list | true}
|}]

let escape_local (raw @ local) : refined_list @ global = refine_ raw;;
[%%expect{|
Line 1, characters 65-68:
1 | let escape_local (raw @ local) : refined_list @ global = refine_ raw;;
                                                                     ^^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

module C1 = struct type t = A end
module C2 = struct type t = A end
[@@@warning "-18"]
open C1
open C2
type resolved_ambiguous = { x : C1.t | match x with A -> true }
type resolved_explicit = { x : C1.t | match x with C1.A -> true }
let resolved_same : resolved_ambiguous list =
  ([] : resolved_explicit list);;
[%%expect{|
module C1 : sig type t = A end
module C2 : sig type t = A end
type resolved_ambiguous = {x : C1.t | match x with | C1.A -> true}
type resolved_explicit = {x : C1.t | match x with | C1.A -> true}
val resolved_same : resolved_ambiguous list = []
|}]

module C1_alias = C1
type resolved_constructor_alias =
  { x : C1.t | match x with C1_alias.A -> true }
let resolved_constructor_alias_same : resolved_explicit list =
  ([] : resolved_constructor_alias list);;
[%%expect{|
module C1_alias = C1
type resolved_constructor_alias =
    {x : C1.t | match x with | C1_alias.A -> true}
val resolved_constructor_alias_same : resolved_explicit list = []
|}]

module R1 = struct type t = { field : int } end
module R2 = struct type t = { field : int } end
open R1
open R2
type resolved_field_ambiguous = { r : R1.t | gt r.field 0 }
type resolved_field_explicit = { r : R1.t | gt r.R1.field 0 }
let resolved_field_same : resolved_field_ambiguous list =
  ([] : resolved_field_explicit list);;
[%%expect{|
module R1 : sig type t = { field : int; } end
module R2 : sig type t = { field : int; } end
type resolved_field_ambiguous = {r : R1.t | gt r.R1.field 0}
type resolved_field_explicit = {r : R1.t | gt r.R1.field 0}
val resolved_field_same : resolved_field_ambiguous list = []
|}]

module R1_alias = R1
type resolved_field_alias = { r : R1.t | gt r.R1_alias.field 0 }
let resolved_field_alias_same : resolved_field_explicit list =
  ([] : resolved_field_alias list);;
[%%expect{|
module R1_alias = R1
type resolved_field_alias = {r : R1.t | gt r.R1_alias.field 0}
val resolved_field_alias_same : resolved_field_explicit list = []
|}]

external as_r1 : R1.t -> R1.t @@ total = "%identity" [@@warning "-61"]
type resolved_record_ambiguous =
  { n : int | gt (as_r1 { field = n }).field 0 }
type resolved_record_explicit =
  { n : int | gt (as_r1 { R1.field = n }).R1.field 0 }
let resolved_record_same : resolved_record_ambiguous list =
  ([] : resolved_record_explicit list);;
[%%expect{|
external as_r1 : R1.t -> R1.t = "%identity"
type resolved_record_ambiguous =
    {n : int | gt (as_r1 { R1.field = n }).R1.field 0}
type resolved_record_explicit =
    {n : int | gt (as_r1 { R1.field = n }).R1.field 0}
val resolved_record_same : resolved_record_ambiguous list = []
|}]

type resolved_record_r1 =
  { n : int | gt ({ R1.field = n }).field 0 }
type resolved_record_r2 =
  { n : int | gt ({ R2.field = n }).field 0 }
let resolved_record_different : resolved_record_r1 list =
  ([] : resolved_record_r2 list);;
[%%expect{|
type resolved_record_r1 = {n : int | gt { R1.field = n }.R1.field 0}
type resolved_record_r2 = {n : int | gt { R2.field = n }.R2.field 0}
Line 6, characters 2-32:
6 |   ([] : resolved_record_r2 list);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "resolved_record_r2 list"
       but an expression was expected of type "resolved_record_r1 list"
       Type "resolved_record_r2" = "{n : int | gt { R2.field = n }.R2.field 0}"
       is not compatible with type
         "resolved_record_r1" = "{n : int | gt { R1.field = n }.R1.field 0}"
|}]

type qualified_free_value =
  { max_int : int | gt Stdlib.max_int max_int };;
[%%expect{|
type qualified_free_value = {max_int : int | gt Stdlib.max_int max_int}
|}]

module type Predicate = sig
  val holds : int -> bool @@ total
end

module Make (P : Predicate) = struct
  type t = { x : int | P.holds x }
end

module Nonzero = struct
  let (holds @ total) x = gt x 0
end

module Applied = Make (Nonzero)
type applied = Applied.t;;
[%%expect{|
module type Predicate = sig val holds : int -> bool @@ total end
module Make :
  functor (P : Predicate) -> sig type t = {x : int | P.holds x} end
module Nonzero : sig val holds : int -> bool end
module Applied : sig type t = {x : int | Nonzero.holds x} end
type applied = Applied.t
|}]

module Stable = struct
  let (holds @ total) (_ : int) = true
end

module Stable_alias = Stable
type stable_path = { x : int | Stable.holds x }
type stable_alias_path = { x : int | Stable_alias.holds x }
let stable_alias_same : stable_path list = ([] : stable_alias_path list);;
[%%expect{|
module Stable : sig val holds : int -> bool end
module Stable_alias = Stable
type stable_path = {x : int | Stable.holds x}
type stable_alias_path = {x : int | Stable_alias.holds x}
val stable_alias_same : stable_path list = []
|}]

module Make_alias (X : sig
  module P : sig
    val holds : int -> bool @@ total
  end
end) = struct
  type t = { x : int | X.P.holds x }
end

module Applied_alias = Make_alias (struct module P = Stable end)
type applied_alias = Applied_alias.t;;
[%%expect{|
module Make_alias :
  functor (X : sig module P : sig val holds : int -> bool @@ total end end)
    -> sig type t = {x : int | X.P.holds x} end
module Applied_alias : sig type t = {x : int | Stable.holds x} end
type applied_alias = Applied_alias.t
|}]

module type Result_signature = sig
  val holds : int -> bool @@ total
  type t = { x : int | holds x }
  module Refined : sig
    val accept : { x : int | holds x } -> unit @@ total
  end
end

module Make_result (P : Predicate) : Result_signature = struct
  let (holds @ total) = P.holds
  type t = { x : int | holds x }
  module Refined = struct
    external accept : { x : int | holds x } -> unit @@ total = "%ignore"
  end
end

module Outer_modtype : sig
  module type R = Result_signature
  module N : R
end = struct
  module type R = sig
    val holds : int -> bool @@ total
    type t = { x : int | holds x }
    module Refined : sig
      val accept : { x : int | holds x } -> unit @@ total
    end
  end
  module N = Make_result (Nonzero)
end;;
[%%expect{|
module type Result_signature =
  sig
    val holds : int -> bool @@ total
    type t = {x : int | holds x}
    module Refined :
      sig val accept : {x : int | holds x} -> unit @@ total end
  end
module Make_result : functor (P : Predicate) -> Result_signature
module Outer_modtype : sig module type R = Result_signature module N : R end
|}]

module Result = Make_result (Nonzero)
module Result_alias = Result;;
[%%expect{|
module Result :
  sig
    val holds : int -> bool @@ total
    type t = {x : int | holds x}
    module Refined :
      sig val accept : {x : int | holds x} -> unit @@ total end
  end
module Result_alias = Result
|}]

module Shadowed : sig
  val holds : int -> bool @@ total
  type hidden = { x : int | holds x }
  val holds : int -> bool @@ total
  type exported = { x : int | holds x }
end = struct
  let (holds @ total) (_ : int) = true
  type hidden = { x : int | holds x }
  let (holds @ total) (_ : int) = false
  type exported = { x : int | holds x }
end;;
[%%expect{|
module Shadowed :
  sig
    type hidden : immediate
    val holds : int -> bool @@ total
    type exported = {x : int | holds x}
  end
|}]

let hidden_is_not_exported : Shadowed.hidden list =
  ([] : Shadowed.exported list);;
[%%expect{|
Line 2, characters 2-31:
2 |   ([] : Shadowed.exported list);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "Shadowed.exported list"
       but an expression was expected of type "Shadowed.hidden list"
       Type "Shadowed.exported" = "{x : int | Shadowed.holds x}"
       is not compatible with type "Shadowed.hidden"
|}]

module Missing_nested_constraint : sig
  module N : sig val y : string end
end = struct
  module N = struct let x = 0 end
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module N = struct let x = 0 end
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module N : sig val x : int end end
       is not included in
         sig module N : sig val y : string end end
       In module "N":
       Modules do not match:
         sig val x : int end
       is not included in
         sig val y : string end
       In module "N":
       The value "y" is required but not provided
|}]

let accept_result (x : Result.t) = Result.Refined.accept x
let accept_alias (x : Result_alias.t) = Result.Refined.accept x
let accept_applicative (x : Make_result(Nonzero).t) =
  Result.Refined.accept x;;
[%%expect{|
val accept_result : Result.t -> unit = <fun>
val accept_alias : Result_alias.t -> unit = <fun>
Line 4, characters 24-25:
4 |   Result.Refined.accept x;;
                            ^
Error: The value "x" has type
         "Make_result(Nonzero).t" = "{x : int | Make_result(Nonzero).holds x}"
       but an expression was expected of type "{x : int | Result.holds x}"
|}]

module Empty = struct end
let choose_positive = ref true
module Nontrivial : Predicate = struct
  let (holds @ total) x = gt x 1
end

module Unstable (_ : sig end) : Result_signature = struct
  let (holds @ total) =
    if !choose_positive then Nonzero.holds else Nontrivial.holds
  type t = { x : int | holds x }
  module Refined = struct
    external accept : { x : int | holds x } -> unit @@ total = "%ignore"
  end
end

module First = Unstable (Empty)
let () = choose_positive := false
module Second = Unstable (Empty);;
[%%expect{|
module Empty : sig end
val choose_positive : bool ref = {contents = true}
module Nontrivial : Predicate
module Unstable : sig end -> Result_signature
module First :
  sig
    val holds : int -> bool @@ total
    type t = {x : int | holds x}
    module Refined :
      sig val accept : {x : int | holds x} -> unit @@ total end
  end
module Second :
  sig
    val holds : int -> bool @@ total
    type t = {x : int | holds x}
    module Refined :
      sig val accept : {x : int | holds x} -> unit @@ total end
  end
|}]

let first_is_not_second (x : First.t) : Second.t = x;;
[%%expect{|
Line 1, characters 51-52:
1 | let first_is_not_second (x : First.t) : Second.t = x;;
                                                       ^
Error: The value "x" has type "First.t" = "{x : int | First.holds x}"
       but an expression was expected of type
         "Second.t" = "{x : int | Second.holds x}"
|}]

let direct_is_not_first (x : Unstable(Empty).t) : First.t = x;;
[%%expect{|
Line 1, characters 60-61:
1 | let direct_is_not_first (x : Unstable(Empty).t) : First.t = x;;
                                                                ^
Error: The value "x" has type
         "Unstable(Empty).t" = "{x : int | Unstable(Empty).holds x}"
       but an expression was expected of type
         "First.t" = "{x : int | First.holds x}"
|}]
