(* TEST
   readonly_files = "\
     specification_only_provider.mli \
     specification_only_provider.ml \
     specification_only_external.mli \
     specification_only_external.ml \
     specification_only_paths_positive.ml \
   ";
   setup-ocamlc.byte-build-env;
   module = "specification_only_provider.mli";
   ocamlc.byte;
   module = "specification_only_provider.ml";
   ocamlc.byte;
   module = "specification_only_external.mli";
   ocamlc.byte;
   module = "specification_only_external.ml";
   ocamlc.byte;
   module = "specification_only_paths_positive.ml";
   ocamlc.byte;
   flags += "-I ocamlc.byte \
     ocamlc.byte/specification_only_provider.cmo \
     ocamlc.byte/specification_only_external.cmo";
   expect;
*)

(* A specification-only declaration remains available in refinements after
   crossing a CMI boundary. *)
let refinement_use (x : int)
    : unit{
        Specification_only_provider.project x
        = Specification_only_provider.project x
      }
  = ()

[%%expect{|
val refinement_use :
  (x : int) ->
  unit{
   Specification_only_provider.project x = Specification_only_provider.project x
   } =
  <fun>
|}]

(* It is not available as an executable function. *)
let _ = Specification_only_provider.project 0

[%%expect{|
Line 1, characters 8-43:
1 | let _ = Specification_only_provider.project 0
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The specification-only value "Specification_only_provider.project" cannot be used in executable code.
|}]

(* Nesting the use inside another expression does not hide it. *)
let _ = Some (Specification_only_provider.project 0)

[%%expect{|
Line 1, characters 14-49:
1 | let _ = Some (Specification_only_provider.project 0)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The specification-only value "Specification_only_provider.project" cannot be used in executable code.
|}]

(* A value alias cannot turn a specification-only declaration executable. *)
let alias = Specification_only_provider.project

[%%expect{|
Line 1, characters 12-47:
1 | let alias = Specification_only_provider.project
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The specification-only value "Specification_only_provider.project" cannot be used in executable code.
|}]

(* Qualified module aliases retain the declaration metadata. *)
module Alias = Specification_only_provider
let _ = Alias.project 0

[%%expect{|
module Alias = Specification_only_provider
Line 2, characters 8-21:
2 | let _ = Alias.project 0
            ^^^^^^^^^^^^^
Error: The specification-only value "Alias.project" cannot be used in executable code.
|}]

module Outer = struct
  module Inner = Specification_only_provider
end

let _ = Outer.Inner.project 0

[%%expect{|
module Outer : sig module Inner = Specification_only_provider end
Line 5, characters 8-27:
5 | let _ = Outer.Inner.project 0
            ^^^^^^^^^^^^^^^^^^^
Error: The specification-only value "Outer.Inner.project" cannot be used in executable code.
|}]

(* [include] and [module type of] preserve the restriction.  Local modules
   keep the successful declarations out of the toplevel output. *)
let include_runtime () =
  let module Included = struct
    include Specification_only_provider
  end in
  Included.project 0

[%%expect{|
Line 5, characters 2-18:
5 |   Included.project 0
      ^^^^^^^^^^^^^^^^
Error: The specification-only value "Included.project" cannot be used in executable code.
|}]

let module_type_runtime () =
  let module Via : module type of Specification_only_provider =
    Specification_only_provider
  in
  Via.project 0

[%%expect{|
Line 5, characters 2-13:
5 |   Via.project 0
      ^^^^^^^^^^^
Error: The specification-only value "Via.project" cannot be used in executable code.
|}]

(* Functor parameters and results cannot launder the restriction. *)
let functor_runtime () =
  let module F = struct
    module Make (X : module type of Specification_only_provider) = struct
      module Inner = X
    end
  end in
  let module Through = F.Make (Specification_only_provider) in
  Through.Inner.project 0

[%%expect{|
Line 8, characters 2-23:
8 |   Through.Inner.project 0
      ^^^^^^^^^^^^^^^^^^^^^
Error: The specification-only value "Through.Inner.project" cannot be used in executable code.
|}]

(* Opening the provider changes only the printed name, not the restriction. *)
let local_open_runtime () =
  let open Specification_only_provider in
  project 0

[%%expect{|
Line 3, characters 2-9:
3 |   project 0
      ^^^^^^^
Error: The specification-only value "project" cannot be used in executable code.
|}]

(* Binding operators resolve values without constructing [Pexp_ident] nodes.
   They must preserve the same executable-use restriction across a CMI. *)
let binding_operator_runtime () =
  let open Specification_only_provider.Let_only in
  let* x = 1 in
  x

[%%expect{|
Line 3, characters 2-6:
3 |   let* x = 1 in
      ^^^^
Error: The specification-only value "( let*
       )" cannot be used in executable code.
|}]

(* A qualified local open follows the same binding-operator lookup path. *)
let qualified_binding_operator_runtime () =
  Specification_only_provider.Let_only.(
    let* x = 1 in
    x)

[%%expect{|
Line 3, characters 4-8:
3 |     let* x = 1 in
        ^^^^
Error: The specification-only value "( let*
       )" cannot be used in executable code.
|}]

(* [and*] has its own lookup after the ordinary [let*] operator. *)
let parallel_binding_operator_runtime () =
  let open Specification_only_provider.And_only in
  let* x = 1
  and* y = 2 in
  x + y

[%%expect{|
Line 4, characters 2-6:
4 |   and* y = 2 in
      ^^^^
Error: The specification-only value "( and*
       )" cannot be used in executable code.
|}]

(* Ordinary binding operators remain executable. *)
let ordinary_binding_operators () =
  let open Specification_only_provider.Ordinary in
  let* x = 1
  and* y = 2 in
  x + y

[%%expect{|
val ordinary_binding_operators : unit -> int = <fun>
|}]

(* Ascription cannot erase the restriction before making an alias. *)
module Laundered : sig
  val project : int -> int @@ total logical
end = Specification_only_provider

[%%expect{|
Line 3, characters 6-33:
3 | end = Specification_only_provider
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig
           val project : int -> int @@ total logical
           module Let_only = Specification_only_provider.Let_only
           module And_only = Specification_only_provider.And_only
           module Ordinary = Specification_only_provider.Ordinary
         end
       is not included in
         sig val project : int -> int @@ total logical end
       Values do not match:
         val project : int -> int @@ total logical
       is not included in
         val project : int -> int @@ total logical
       The first is specification-only, but the second permits executable use.
       File "specification_only_provider.mli", line 1, characters 0-59:
         Actual declaration
|}]

(* A specification-only external cannot be weakened to a plain [val].  That
   module coercion would otherwise materialize an executable primitive
   wrapper even though clients cannot call it. *)
module External_weakened : sig
  val project : int -> int @@ total logical [@@vox.spec_only]
end = struct
  external project : int -> int @@ total logical
    = "vox_missing_weakened_specification_only_primitive"
    [@@vox.spec_only]
end

[%%expect{|
Lines 3-7, characters 6-3:
3 | ......struct
4 |   external project : int -> int @@ total logical
5 |     = "vox_missing_weakened_specification_only_primitive"
6 |     [@@vox.spec_only]
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           external project : int -> int
             = "vox_missing_weakened_specification_only_primitive"
         end
       is not included in
         sig val project : int -> int @@ total logical end
       Values do not match:
         external project : int -> int
           = "vox_missing_weakened_specification_only_primitive"
       is not included in
         val project : int -> int @@ total logical
       A specification-only external must remain external in the matching interface.
|}]

(* Erasing a specification-only primitive dependency does not bypass the
   ordinary builtin-primitive declaration checks. *)
module Bad_primitive_arity = struct
  external wrong : int -> int -> int = "%identity" [@@vox.spec_only]
end

[%%expect{|
Line 2, characters 2-68:
2 |   external wrong : int -> int -> int = "%identity" [@@vox.spec_only]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Wrong arity for builtin primitive "%identity"
|}]

(* The marker is a flag, not a payload-bearing attribute. *)
module type Bad_payload = sig
  val x : int [@@vox.spec_only "payload"]
end

[%%expect{|
Line 2, characters 17-30:
2 |   val x : int [@@vox.spec_only "payload"]
                     ^^^^^^^^^^^^^
Error: Attribute "vox.spec_only" does not accept a payload
|}]

(* The restriction is attached only to declarations that opt into it. *)
let nonnegative : int{ _ >= 0 } = 0
let runtime_refined = nonnegative + 1

let logical_identity @ logical = fun x -> x
let runtime_logical = logical_identity 42

let runtime_total @ total = fun x -> x + 1
let runtime_physical = runtime_total 42

[%%expect{|
val nonnegative : int{ _ >= 0 } = 0
val runtime_refined : int = 1
val logical_identity : 'a -> 'a @@ logical = <fun>
val runtime_logical : int = 42
val runtime_total : int -> int = <fun>
val runtime_physical : int = 43
|}]
