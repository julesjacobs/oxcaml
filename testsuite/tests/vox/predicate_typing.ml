(* TEST
 flags = "-drefinements -extension let_mutable -extension layout_poly_alpha";
 expect;
*)

(* Vox predicate typing (design-docs/predicate-typing.md): every refinement
   predicate is checked, at the point the type is formed, to be a bool by
   Typecore reentry — the hole [_] bound at the payload type, each
   dependent-arrow binder bound at its completed declared type — and the
   checked result is stored as a typed mirror.

   RED pins today's behaviour: predicates are resolved but not typed, so
   ill-typed predicates are accepted.  GREEN flips them to located errors;
   the expectation diff is the demonstration. *)

(* --- Rejections and acceptance, located ------------------------------ *)

type a = int{ 42 };;
[%%expect{|
Line 1, characters 14-16:
1 | type a = int{ 42 };;
                  ^^
Error: The constant "42" has type "int" but an expression was expected of type
         "bool"
|}]

type b = int{ _ + "x" };;
[%%expect{|
Line 1, characters 18-21:
1 | type b = int{ _ + "x" };;
                      ^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

(* The hole is a string, so the application itself is well-typed; its
   [int] result is what must be rejected against the expected [bool]. *)
let total_length @ total = fun (_ : string) -> 0;;
[%%expect{|
val total_length : string -> int = <fun>
|}]

type c = string{ total_length _ };;
[%%expect{|
Line 1, characters 17-31:
1 | type c = string{ total_length _ };;
                     ^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "bool"
|}]

type d = int{ _ > 0 };;
[%%expect{|
type d = int{ _ > 0 }
|}]

(* Rollback must retain the concrete payload in the eventual diagnostic. *)
type rollback_diagnostic = (int * int){ _ > "s" };;
[%%expect{|
Line 1, characters 44-47:
1 | type rollback_diagnostic = (int * int){ _ > "s" };;
                                                ^^^
Error: This constant has type "string" but an expression was expected of type
         "int * int"
|}]

(* --- Holes ------------------------------------------------------------ *)

(* multiple occurrences, each at the payload *)
type h1 = int{ _ > 0 && _ < 10 };;
[%%expect{|
type h1 = int{ (_ > 0) && (_ < 10) }
|}]

(* nested refinement: each hole means the innermost enclosing refinement's
   value — the inner hole is a string, the outer an int *)
type h2 = int{
  let _s = ("s" : string{ total_length _ > 0 }) in
  _ > 0
};;
[%%expect{|
type h2 = int{ let _s = ("s" : string{ (total_length _) > 0 }) in _ > 0 }
|}]

(* the inner hole is *not* the outer int: [_ > 0] at payload string must
   reject *)
type h3 = int{ let _s = ("s" : string{ _ > 0 }) in _ > 0 };;
[%%expect{|
Line 1, characters 43-44:
1 | type h3 = int{ let _s = ("s" : string{ _ > 0 }) in _ > 0 };;
                                               ^
Error: The constant "0" has type "int" but an expression was expected of type
         "string"
|}]

(* holes under the predicate's own binders *)
type h4 = int{ let y = _ in y > 0 };;
[%%expect{|
type h4 = int{ let y = _ in y > 0 }
|}]

type h5 = int{ match _ with 0 -> true | n -> n > 0 };;
[%%expect{|
type h5 = int{ match _ with | 0 -> true | n -> n > 0 }
|}]

type h6 = int{ (fun b -> b && _ > 0) true };;
[%%expect{|
type h6 = int{ (fun b -> b && (_ > 0)) true }
|}]

(* an ill-typed hole use under a binder *)
type h7 = int{ let y = _ in y && true };;
[%%expect{|
Line 1, characters 28-29:
1 | type h7 = int{ let y = _ in y && true };;
                                ^
Error: The value "y" has type "int" but an expression was expected of type "bool"
|}]

(* --- Binders ----------------------------------------------------------- *)

(* bare binder at payload *)
type b1 = n:int{ n > 0 } -> unit;;
[%%expect{|
type b1 = n:int{ n > 0 } -> unit
|}]

(* labelled binder at payload *)
type b2 = ~x:int{ x > 0 } -> unit;;
[%%expect{|
type b2 = ~x:int{ x > 0 } -> unit
|}]

(* an ill-typed use of the binder *)
let accepts_string @ total = fun (_ : string) -> true;;
[%%expect{|
val accepts_string : string -> bool = <fun>
|}]

type b3 = n:int{ accepts_string n } -> unit;;
[%%expect{|
Line 1, characters 32-33:
1 | type b3 = n:int{ accepts_string n } -> unit;;
                                    ^
Error: The value "n" has type "int" but an expression was expected of type "string"
|}]

(* refined binder head-strip: [x] is used at the payload of its declared
   refined type *)
type b4 = x:int{ x > 0 } -> int{ _ > x };;
[%%expect{|
type b4 = x:int{ x > 0 } -> int{ _ > x }
|}]

(* binder-in-own-domain: [x] is the whole tuple, not an int — flips to a
   type error when predicates are typed *)
type b5 = x:(int{ x > 0 } * int) -> unit;;
[%%expect{|
Line 1, characters 22-23:
1 | type b5 = x:(int{ x > 0 } * int) -> unit;;
                          ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ _ } * int"
|}]

(* the same shape used correctly: the domain completes before the
   predicate is typed, so [fst x] projects the tuple *)
type b6 = x:(int{ fst x > 0 } * int) -> unit;;
[%%expect{|
type b6 = x:int{ (fst x) > 0 } * int -> unit
|}]

(* labelled spelling of the own-domain pair *)
type b7 = ~x:(int{ x > 0 } * int) -> unit;;
[%%expect{|
Line 1, characters 23-24:
1 | type b7 = ~x:(int{ x > 0 } * int) -> unit;;
                           ^
Error: The constant "0" has type "int" but an expression was expected of type
         "int{ _ } * int"
|}]

type b8 = ~x:(int{ fst x > 0 } * int) -> unit;;
[%%expect{|
type b8 = ~x:int{ (fst x) > 0 } * int -> unit
|}]

(* Stored annotations must not create metadata cycles when more than one
   predicate destructures the same completed binder domain, or when a
   codomain predicate follows an own-domain predicate. *)
type b8_multi = x:(int{ fst x > 0 } * int{ snd x > 0 }) -> unit;;
[%%expect{|
type b8_multi = x:int{ (fst x) > 0 } * int{ (snd x) > 0 } -> unit
|}]

type b8_codomain = x:(int{ fst x > 0 } * int) -> bool{ snd x = 0 };;
[%%expect{|
type b8_codomain = x:int{ (fst x) > 0 } * int -> bool{ (snd x) = 0 }
|}]

(* A hole at a dependent-arrow payload must remain equal to its own copied
   type; binder freshening may not desynchronize the nested predicate. *)
type dependent_hole =
  (x:int -> int{ _ >= x }){
    let _f = if true then _ else _ in
    true
  };;
[%%expect{|
type dependent_hole =
    (x:int -> int{ _ >= x }){ let _f = if true then _ else _ in true }
|}]

module type Dependent_hole_signature = sig
  type t = (x:int -> int{ _ >= x }){
    let _f = if true then _ else _ in
    true
  }
end;;
[%%expect{|
module type Dependent_hole_signature =
  sig
    type t =
        (x:int -> int{ _ >= x }){ let _f = if true then _ else _ in true }
  end
|}]

module Dependent_hole_source = struct
  type t = (x:int -> int{ _ >= x }){
    let _f = if true then _ else _ in
    true
  }
end;;
[%%expect{|
module Dependent_hole_source :
  sig
    type t =
        (x:int -> int{ _ >= x }){ let _f = if true then _ else _ in true }
  end
|}]

module Dependent_hole_copy (X : Dependent_hole_signature) = X;;
[%%expect{|
module Dependent_hole_copy :
  functor (X : Dependent_hole_signature) ->
    sig
      type t =
          (x:int -> int{ _ >= x }){ let _f = if true then _ else _ in true }
    end
|}]

module Dependent_hole_result : Dependent_hole_signature =
  Dependent_hole_copy (Dependent_hole_source);;
[%%expect{|
module Dependent_hole_result : Dependent_hole_signature
|}]

(* Derived node annotations are metadata and must not add variance
   occurrences to the written declaration. *)
type -'a contravariant = x:'a -> int{
  let _y = if true then x else x in
  true
};;
[%%expect{|
type 'a contravariant = x:'a -> int{ let _y = if true then x else x in true }
|}]

(* Predicate typing must not rewrite the written abbreviation spelling in a
   refined binder payload. *)
type refined_string_binder =
  s:string{ total_length s > 0 } -> unit;;
[%%expect{|
type refined_string_binder = s:string{ (total_length s) > 0 } -> unit
|}]

(* Frame views copy object and unboxed-tuple payload spines too: typing the
   predicate must not relink the written [string] spelling to this alias. *)
type string_alias = string;;
type object_control = < value : string >;;
type unboxed_control = #(int * string);;
[%%expect{|
type string_alias = string
type object_control = < value : string >
type unboxed_control = #(int * string)
|}]

let check_object @ total = fun (_ : < value : string_alias >) -> true;;
type object_payload =
  < value : string >{ check_object (_ : < value : string_alias >) };;
[%%expect{|
val check_object : < value : string_alias > -> bool = <fun>
type object_payload =
    < value : string >{ check_object (_ : < value: string_alias   > ) }
|}]

let check_unboxed @ total = fun (_ : #(int * string_alias)) -> true;;
type unboxed_payload =
  #(int * string){ check_unboxed (_ : #(int * string_alias)) };;
[%%expect{|
val check_unboxed : #(int * string_alias) -> bool = <fun>
type unboxed_payload =
    #(int * string){ check_unboxed (_ : #(int * string_alias)) }
|}]

(* The structural copy also covers the polymorphic-variant special case and a
   generic first-class-package spine. *)
type variant_alias = [ `A of string ];;
type variant_control = [ `A of string ];;
[%%expect{|
type variant_alias = [ `A of string ]
type variant_control = [ `A of string ]
|}]

let check_variant @ total = fun (_ : variant_alias) -> true;;
type variant_payload =
  [ `A of string ]{ check_variant (_ : variant_alias) };;
[%%expect{|
val check_variant : variant_alias -> bool = <fun>
type variant_payload = [ `A of string ]{ check_variant (_ : variant_alias) }
|}]

module type Package_source = sig type t end;;
module type Package_alias = Package_source;;
type package_control = (module Package_source);;
[%%expect{|
module type Package_source = sig type t end
module type Package_alias = Package_source
type package_control = (module Package_source)
|}]

let check_package @ total = fun (_ : (module Package_alias)) -> true;;
type package_payload =
  (module Package_source){ check_package (_ : (module Package_alias)) };;
[%%expect{|
val check_package : (module Package_alias) -> bool = <fun>
type package_payload =
    (module Package_source){ check_package (_ : (module Package_alias)) }
|}]

(* predicate binders shadow arrow binders *)
type b9 = n:int -> bool{ (let n = true in n) && n > 0 };;
[%%expect{|
type b9 = n:int -> bool{ (let n = true in n) && (n > 0) }
|}]

(* Binder order-sensitivity: the early annotation constrains [g] before use
   and accepts; the late annotation follows a use that fixes [g] to a bare
   result and must reject the refined result type cleanly. *)
type b10 = bool{
  let f = fun g ->
    let h = (g : unit -> int{ true }) in
    h () + 1 > 0
  in
  true
};;
[%%expect{|
Line 2, characters 6-7:
2 |   let f = fun g ->
          ^
Warning 26 [unused-var]: unused variable "f".

type b10 =
    bool{ let f g = let h = (g : unit -> int{ true }) in ((h ()) + 1) > 0 in
          true }
|}]

type b11 = bool{
  let f = fun g ->
    let y = g () in
    let z = y + 1 in
    let h = (g : unit -> int{ true }) in
    z > 0
  in
  true
};;
[%%expect{|
Line 5, characters 13-14:
5 |     let h = (g : unit -> int{ true }) in
                 ^
Error: The value "g" has type "unit -> int"
       but an expression was expected of type "unit -> int{ true }"
       Type "int" is not compatible with type "int{ true }"
|}]

(* --- Disambiguation ---------------------------------------------------- *)

(* Two records sharing a label name: the parsetree-era resolver froze the
   first candidate; Typecore selects by the expected (payload) type. *)
type r1 = { f : int };;
type r2 = { f : bool };;
[%%expect{|
type r1 = { f : int; }
type r2 = { f : bool; }
|}]

(* [_ : r1], so [_.f] must be [r1]'s int-valued field, not [r2]'s *)
type dr = r1{ _.f > 0 };;
[%%expect{|
type dr = r1{ _.f > 0 }
|}]

(* and [r2]'s field is a bool *)
type dr2 = r2{ _.f };;
[%%expect{|
type dr2 = r2{ _.f }
|}]

(* Queued predicates must observe only authoritative Typecore identities in
   other refinements of the same completed domain, in either source order. *)
module Queue_int = struct type t = { shared : int } end;;
module Queue_string = struct type t = { shared : string } end;;
open Queue_int;;
open Queue_string;;
[%%expect{|
module Queue_int : sig type t = { shared : int; } end
module Queue_string : sig type t = { shared : string; } end
|}]

type queue_wanted = Queue_int.t{ _.shared > 0 };;
let accepts_queue_wanted @ total = fun (_ : queue_wanted list) -> true;;
[%%expect{|
type queue_wanted = Queue_int.t{ _.Queue_int.shared > 0 }
val accepts_queue_wanted : queue_wanted list -> bool = <fun>
|}]

type queue_observes_later =
  x:(unit{ accepts_queue_wanted (snd x) }
     * Queue_int.t{ _.shared > 0 } list) -> unit;;
[%%expect{|
type queue_observes_later =
    x:unit{ accepts_queue_wanted (snd x) } *
      Queue_int.t{ _.Queue_int.shared > 0 } list ->
    unit
|}]

type queue_observes_earlier =
  x:(Queue_int.t{ _.shared > 0 } list
     * unit{ accepts_queue_wanted (fst x) }) -> unit;;
[%%expect{|
type queue_observes_earlier =
    x:Queue_int.t{ _.Queue_int.shared > 0 } list *
      unit{ accepts_queue_wanted (fst x) } ->
    unit
|}]

(* the selection is observable: [r1]'s [f] is an int, so using it as a
   bool is an error naming int — a first-candidate resolver would have
   picked [r2]'s bool-valued [f] and accepted *)
type drx = r1{ _.f && true };;
[%%expect{|
Line 1, characters 15-18:
1 | type drx = r1{ _.f && true };;
                   ^^^
Error: The field access "_.f" has type "int"
       but an expression was expected of type "bool"
|}]

(* Two variants sharing a constructor name, selected by expected type *)
type v1 = A of int;;
type v2 = A of bool;;
[%%expect{|
type v1 = A of int
type v2 = A of bool
|}]

type dv = v1{ let _v = if true then _ else A 1 in true };;
[%%expect{|
Line 1, characters 43-44:
1 | type dv = v1{ let _v = if true then _ else A 1 in true };;
                                               ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

type dv = v1{ let _v = if true then _ else A 1 in true }
|}]

type dv2 = v1{ match _ with A n -> n > 0 };;
[%%expect{|
type dv2 = v1{ match _ with | A n -> n > 0 }
|}]

(* the pattern selection is observable too: [v1]'s [A] carries an int *)
type dvx = v1{ match _ with A n -> n && true };;
[%%expect{|
Line 1, characters 35-36:
1 | type dvx = v1{ match _ with A n -> n && true };;
                                       ^
Error: The value "n" has type "int" but an expression was expected of type "bool"
|}]

(* Typecore changes the source shape of a wildcard constructor argument when
   the selected constructor's arity is not one.  RED's correspondence walk
   rejects with a located error; GREEN rejects in the predicate judgment before
   mirror construction. *)
type predicate_pair = Predicate_pair of int * int;;
type multi_arity_constructor_wildcard = bool{
  match Predicate_pair (1, 2) with
  | Predicate_pair _ -> true
};;
[%%expect{|
type predicate_pair = Predicate_pair of int * int
Line 4, characters 4-20:
4 |   | Predicate_pair _ -> true
        ^^^^^^^^^^^^^^^^
Error: An elaborated constructor pattern is not supported in a refinement predicate.
|}]

type predicate_nullary = Predicate_nullary;;
type nullary_constructor_wildcard = bool{
  match Predicate_nullary with
  | Predicate_nullary _ -> true
};;
[%%expect{|
type predicate_nullary = Predicate_nullary
Line 4, characters 22-23:
4 |   | Predicate_nullary _ -> true
                          ^
Warning 28 [wildcard-arg-to-constant-constr]: wildcard pattern given as argument to a constant constructor

Line 4, characters 4-23:
4 |   | Predicate_nullary _ -> true
        ^^^^^^^^^^^^^^^^^^^
Error: An elaborated constructor pattern is not supported in a refinement predicate.
|}]

(* --- Application -------------------------------------------------------- *)

module App : sig
  val labelled : x:'a @ total -> y:'b @ total -> 'b @ total @@ total
  val optional : ?o:int @ total -> unit @ total -> bool @ total @@ total
  val optional_labelled :
    ?o:'a @ total -> y:'b @ total -> 'b @ total @@ total
  val positional :
    p:[%call_pos] @ total -> unit @ total -> bool @ total @@ total
  val positional_labelled :
    p:[%call_pos] @ total -> y:bool @ total -> bool @ total @@ total
  val id : 'a @ total -> 'a @ total @@ total
end = struct
  let labelled @ total = fun ~x:_ ~y -> y
  let optional @ total = fun ?(o = 0) () -> true
  let optional_labelled @ total = fun ?o:_ ~y -> y
  let positional @ total = fun ~(p : [%call_pos]) () -> true
  let positional_labelled @ total = fun ~(p : [%call_pos]) ~y -> y
  let id @ total = fun x -> x
end;;
[%%expect{|
module App :
  sig
    val labelled : x:'a @ total -> y:'b @ total -> 'b @ total @@ total
    val optional : ?o:int @ total -> unit @ total -> bool @ total @@ total
    val optional_labelled : ?o:'a @ total -> y:'b @ total -> 'b @ total @@
      total
    val positional : p:[%call_pos] -> unit @ total -> bool @ total @@ total
    val positional_labelled : p:[%call_pos] -> y:bool @ total -> bool @ total
      @@ total
    val id : 'a @ total -> 'a @ total @@ total
  end
|}]

(* labelled application with commuting *)
type ap1 = int{ App.labelled ~y:true ~x:_ };;
[%%expect{|
type ap1 = int{ App.labelled ~y:true ~x:_ }
|}]

(* a commuted partial application omits the earlier label ([Omitted] in
   the typedtree): rejected — the typedtree reordered beyond the source *)
type ap2 = int{ (App.labelled ~y:true) ~x:_ };;
[%%expect{|
Line 1, characters 16-38:
1 | type ap2 = int{ (App.labelled ~y:true) ~x:_ };;
                    ^^^^^^^^^^^^^^^^^^^^^^
Error: A partial application that omits a labelled argument is not supported in a refinement predicate.
|}]

(* an in-order partial application omits nothing and is accepted
   (fixture added at GREEN: the in-order spelling was pinned by the
   neighbouring RED fixtures) *)
type ap2b = bool{ (App.labelled ~x:1) ~y:true };;
[%%expect{|
type ap2b = bool{ (App.labelled ~x:1) ~y:true }
|}]

(* Optional/Position arrows in an applied callee's type: rejected in all
   four spellings *)
type ap3 = int{ App.optional ~o:_ () };;
[%%expect{|
Line 1, characters 16-36:
1 | type ap3 = int{ App.optional ~o:_ () };;
                    ^^^^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

type ap4 = int{ App.optional ?o:(Some _) () };;
[%%expect{|
Line 1, characters 16-43:
1 | type ap4 = int{ App.optional ?o:(Some _) () };;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

type ap5 = int{ App.optional () };;
[%%expect{|
Line 1, characters 16-31:
1 | type ap5 = int{ App.optional () };;
                    ^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

type ap6 = int{ App.positional () };;
[%%expect{|
Line 1, characters 16-33:
1 | type ap6 = int{ App.positional () };;
                    ^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

(* An optional parameter can itself be retained as [Omitted] by a partial
   application, separately from the defaulted [None] synthesized by [ap5]. *)
type ap6b = bool{
  let f = App.optional_labelled ~y:true in
  f ?o:None
};;
[%%expect{|
Line 2, characters 10-39:
2 |   let f = App.optional_labelled ~y:true in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

(* A position parameter can be retained as [Omitted] by a partial
   application.  This is distinct from the synthesized call position in
   [ap6]. *)
type ap6c = bool{
  let _f = App.positional_labelled ~y:true in
  true
};;
[%%expect{|
Line 2, characters 11-42:
2 |   let _f = App.positional_labelled ~y:true in
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

(* The clean spelling: the omission is well-typed on its own, so the located
   rejection is the mirror's. *)
type ap7b = bool{ let g = App.labelled ~y:true in g ~x:1 };;
[%%expect{|
Line 1, characters 26-46:
1 | type ap7b = bool{ let g = App.labelled ~y:true in g ~x:1 };;
                              ^^^^^^^^^^^^^^^^^^^^
Error: A partial application that omits a labelled argument is not supported in a refinement predicate.
|}]

(* %apply / %revapply rewrites have no faithful preimage *)
type ap8 = int{ (fun n -> true) @@ _ };;
[%%expect{|
Line 1, characters 16-36:
1 | type ap8 = int{ (fun n -> true) @@ _ };;
                    ^^^^^^^^^^^^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

type ap9 = int{ _ |> fun n -> true };;
[%%expect{|
type ap9 = int{ _ |> (fun n -> true) }
|}]

(* An inferred RHS is genuinely rewritten as [%revapply], unlike the lambda
   control above, and therefore has no faithful mirror preimage. *)
type ap9b = bool{ true |> App.id };;
[%%expect{|
Line 1, characters 18-32:
1 | type ap9b = bool{ true |> App.id };;
                      ^^^^^^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

(* RED pins both source-shaped operators at a non-crossing operand type. *)
let (accepts_logical_ref @ total)
    : int ref @ logical -> bool @ total
  =
  fun _ -> true;;
[%%expect{|
val accepts_logical_ref : int ref @ logical -> bool @ total = <fun>
|}]

type ap9c = (int ref){ accepts_logical_ref @@ _ };;
[%%expect{|
Line 1, characters 23-47:
1 | type ap9c = (int ref){ accepts_logical_ref @@ _ };;
                           ^^^^^^^^^^^^^^^^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

type ap9d = (int ref){ _ |> accepts_logical_ref };;
[%%expect{|
Line 1, characters 23-47:
1 | type ap9d = (int ref){ _ |> accepts_logical_ref };;
                           ^^^^^^^^^^^^^^^^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

type apply_partial_callback = (bool -> bool){ _ @@ true };;
[%%expect{|
Line 1, characters 46-55:
1 | type apply_partial_callback = (bool -> bool){ _ @@ true };;
                                                  ^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

type revapply_partial_callback = (bool -> bool){ true |> _ };;
[%%expect{|
Line 1, characters 49-58:
1 | type revapply_partial_callback = (bool -> bool){ true |> _ };;
                                                     ^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

(* A dependent binder declared Total must keep that mode through predicate
   reentry.  RED rejects the operator spellings only because their typedtree
   rewrites are not represented yet. *)
type direct_total_binder =
  f:(int -> bool) @ total -> bool{ f 0 };;
[%%expect{|
type direct_total_binder = f:(int -> bool) @ total -> bool{ f 0 }
|}]

type apply_total_binder =
  f:(int -> bool) @ total -> bool{ f @@ 0 };;
[%%expect{|
Line 2, characters 35-41:
2 |   f:(int -> bool) @ total -> bool{ f @@ 0 };;
                                       ^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

type revapply_total_binder =
  f:(int -> bool) @ total -> bool{ 0 |> f };;
[%%expect{|
Line 2, characters 35-41:
2 |   f:(int -> bool) @ total -> bool{ 0 |> f };;
                                       ^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

(* Keep the direct-call control and the two operator cases in separate phrases:
   RED's first translation failure must not hide the other operator. *)
module type Direct_partial_result = sig
  val returns_partial :
    unit @ total -> (unit -> bool) @ partial @@ total

  type direct = bool{ let _f = returns_partial () in true }
end;;
[%%expect{|
module type Direct_partial_result =
  sig
    val returns_partial : unit @ total -> unit -> bool @@ total
    type direct = bool{ let _f = returns_partial () in true }
  end
|}]

module type Apply_partial_result = sig
  val returns_partial :
    unit @ total -> (unit -> bool) @ partial @@ total

  type apply = bool{ let _f = returns_partial @@ () in true }
end;;
[%%expect{|
Line 5, characters 30-51:
5 |   type apply = bool{ let _f = returns_partial @@ () in true }
                                  ^^^^^^^^^^^^^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

module type Revapply_partial_result = sig
  val returns_partial :
    unit @ total -> (unit -> bool) @ partial @@ total

  type revapply = bool{ let _f = () |> returns_partial in true }
end;;
[%%expect{|
Line 5, characters 33-54:
5 |   type revapply = bool{ let _f = () |> returns_partial in true }
                                     ^^^^^^^^^^^^^^^^^^^^^
Error: An application rewritten by the typechecker is not supported in a refinement predicate.
|}]

(* Noncanonical externals that reuse the primitive names stay on the generic
   application path.  Pin this on both sides of the RED/GREEN boundary. *)
external malformed_predicate_apply :
  int @ total -> int @ total -> bool @ total = "%apply";;
external malformed_predicate_revapply :
  int @ total -> int @ total -> bool @ total = "%revapply";;
[%%expect{|
external malformed_predicate_apply :
  int @ total -> int @ total -> bool @ total = "%apply"
external malformed_predicate_revapply :
  int @ total -> int @ total -> bool @ total = "%revapply"
|}]

type ap9e = bool{ malformed_predicate_apply 1 2 };;
type ap9f = bool{ malformed_predicate_revapply 1 2 };;
[%%expect{|
type ap9e = bool{ malformed_predicate_apply 1 2 }
type ap9f = bool{ malformed_predicate_revapply 1 2 }
|}]

(* a format-string rewrite has no faithful preimage *)
let accepts_format @ total =
  fun (_ : (int -> string, unit, string) format) -> true;;
[%%expect{|
val accepts_format : (int -> string, unit, string) format -> bool = <fun>
|}]

type ap10 = int{ accepts_format "%d" };;
[%%expect{|
Line 1, characters 32-36:
1 | type ap10 = int{ accepts_format "%d" };;
                                    ^^^^
Error: A format string is not supported in a refinement predicate.
|}]

(* Optional/position function coercions synthesize an eta wrapper even though
   the source is only an identifier.  RED pins the typedtree-only rewrite's
   current rejection. *)
let accepts_total_unit_function @ total =
  fun (_ : (unit @ total -> bool @ total) @ total) -> true;;
[%%expect{|
val accepts_total_unit_function :
  (unit @ total -> bool @ total) @ total -> bool = <fun>
|}]

type ap11 = bool{ accepts_total_unit_function App.optional };;
[%%expect{|
Line 1, characters 46-58:
1 | type ap11 = bool{ accepts_total_unit_function App.optional };;
                                                  ^^^^^^^^^^^^
Error: An elaborated identifier is not supported in a refinement predicate.
|}]

type ap12 = bool{ accepts_total_unit_function App.positional };;
[%%expect{|
Line 1, characters 46-60:
1 | type ap12 = bool{ accepts_total_unit_function App.positional };;
                                                  ^^^^^^^^^^^^^^
Error: An elaborated identifier is not supported in a refinement predicate.
|}]

(* The two synthesized [%call_pos] locations differ between these declarations.
   RED pins the current translation-time rejection before GREEN makes location
   metadata irrelevant to predicate identity. *)
module type Positional_predicate = sig
  type t = bool{ App.positional () }
end;;
[%%expect{|
Line 2, characters 17-34:
2 |   type t = bool{ App.positional () }
                     ^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

module Positional_predicate_impl : Positional_predicate = struct
  type t = bool{ App.positional () }
end;;
[%%expect{|
Line 2, characters 17-34:
2 |   type t = bool{ App.positional () }
                     ^^^^^^^^^^^^^^^^^
Error: An application of a function with optional or position parameters is not supported in a refinement predicate.
|}]

(* An ordinary layout-polymorphic value (not the wrapper-free primitive path)
   introduces [Texp_apply_layout], which RED cannot mirror. *)
module Layout_predicate
    (M : sig
       val id :
         layout_ x. ('a : x). 'a @ total -> 'a @ total @@ total
     end @ static) =
struct
  type ap13 = bool{ M.id true }
end;;
[%%expect{|
Line 7, characters 20-24:
7 |   type ap13 = bool{ M.id true }
                        ^^^^
Error: A layout-polymorphic use is not supported in a refinement predicate.
|}]

(* --- Polymorphic let inside a predicate --------------------------------- *)

type pl = int{ let id = fun x -> x in id 0 = 0 && id true };;
[%%expect{|
type pl = int{ let id x = x in ((id 0) = 0) && (id true) }
|}]

(* A simple binding annotation has a faithful [Rexp_constraint] preimage and
   must not become an unsupported-form flip. *)
type pla = int{ let y : int = _ in y > 0 };;
[%%expect{|
type pla = int{ let y = (_ : int) in y > 0 }
|}]

(* An explicit polymorphic binding annotation has no faithful expression
   constraint node.  RED's correspondence walk rejects it; GREEN rejects it
   earlier in the syntactic judgment, and neither path may be fatal. *)
type polymorphic_binding_annotation = bool{
  let id : 'a. 'a -> 'a = fun x -> x in
  id true
};;
[%%expect{|
Lines 2-3, characters 2-9:
2 | ..let id : 'a. 'a -> 'a = fun x -> x in
3 |   id true
Error: An elaborated binding annotation is not supported in a refinement predicate.
|}]

(* --- Occurrence strips inside predicates --------------------------------- *)

module Occ = struct
  let (g @ total) : unit -> int{ _ > 0 } = fun () -> 1
  let l : int{ _ > 0 } list = [1]
end;;
[%%expect{|
Line 2, characters 53-54: refinement obligation: int{ _ > 0 }
Line 3, characters 31-32: refinement obligation: int{ _ > 0 }
module Occ : sig val g : unit -> int{ _ > 0 } val l : int{ _ > 0 } list end
|}]

(* application-result head strip: [Occ.g ()] is an int inside a predicate *)
type oc1 = bool{ Occ.g () + 1 > 0 };;
[%%expect{|
type oc1 = bool{ ((Occ.g ()) + 1) > 0 }
|}]

(* nested heads stay intact: int{p} list is not int list *)
type oc2 = bool{
  let _xs : int{ _ > 0 } list = Occ.l in
  true
};;
[%%expect{|
type oc2 = bool{ let _xs = (Occ.l : int{ _ > 0 } list) in true }
|}]

let accepts_plain_int_list @ total = fun (_ : int list) -> true;;
[%%expect{|
val accepts_plain_int_list : int list -> bool = <fun>
|}]

(* This call is rejected only if the nested element refinement remains
   distinct; polymorphic equality in [oc2] alone cannot discriminate that. *)
type oc2b = bool{ accepts_plain_int_list Occ.l };;
[%%expect{|
Line 1, characters 41-46:
1 | type oc2b = bool{ accepts_plain_int_list Occ.l };;
                                             ^^^^^
Error: The value "Occ.l" has type "int{ _ > 0 } list"
       but an expression was expected of type "int list"
       Type "int{ _ > 0 }" is not compatible with type "int"
|}]

(* element projection strips the element's head *)
let (head_refined @ total)
    : int{ _ > 0 } list -> int{ _ > 0 }
  =
  fun (xs : int{ _ > 0 } list) ->
    match xs with
    | [] -> 0
    | x :: _ -> x;;
[%%expect{|
Lines 5-7, characters 4-17: refinement obligation: int{ _ > 0 }
val head_refined : int{ _ > 0 } list -> int{ _ > 0 } = <fun>
|}]

type oc3 = bool{ head_refined Occ.l > 0 };;
[%%expect{|
type oc3 = bool{ (head_refined Occ.l) > 0 }
|}]

(* a well-typed call to a partial function is accepted by this piece
   (modes are a later piece) *)
type oc4 = bool{ List.hd [] };;
[%%expect{|
type oc4 = bool{ List.hd [] }
|}]

(* a qualified mutable access is likewise accepted by this piece *)
module Mut = struct let cell = { contents = 0 } end;;
type oc5 = bool{ Mut.cell.contents = 0 };;
[%%expect{|
module Mut : sig val cell : int ref end
type oc5 = bool{ Mut.cell.Stdlib.contents = 0 }
|}]

(* --- Round 4 mode-discipline RED2 baseline ------------------------------- *)

(* A function-valued spec entity defaults to legacy Partial.  RED admits
   direct calls through the hole, aliases and indirect callback parameters;
   GREEN requires the callee itself to be established Total. *)
type direct_partial_hole = (int -> bool){ _ 0 };;
[%%expect{|
type direct_partial_hole = (int -> bool){ _ 0 }
|}]

type aliased_partial_hole = (int -> bool){ let f = _ in f 0 };;
[%%expect{|
type aliased_partial_hole = (int -> bool){ let f = _ in f 0 }
|}]

type indirect_partial_hole = (int -> bool){ (fun f -> f 0) _ };;
[%%expect{|
type indirect_partial_hole = (int -> bool){ (fun f -> f 0) _ }
|}]

(* RED treats a Total primitive as covering an overapplied stage.  GREEN checks
   the Partial function returned by that primitive before calling it. *)
type overapplied_total_primitive = (int -> bool){ Fun.id _ 0 };;
[%%expect{|
type overapplied_total_primitive = (int -> bool){ Fun.id _ 0 }
|}]

(* The same default and an explicit Partial mode apply to dependent binders. *)
type direct_default_binder =
  f:(int -> bool) -> bool{ f 0 };;
[%%expect{|
type direct_default_binder = f:(int -> bool) -> bool{ f 0 }
|}]

type direct_partial_binder =
  f:(int -> bool) @ partial -> bool{ f 0 };;
[%%expect{|
type direct_partial_binder = f:(int -> bool) -> bool{ f 0 }
|}]

(* An explicitly Total payload context is the positive hole control. *)
type total_hole_context = ((int -> bool){ _ 0 }) @ total -> unit;;
[%%expect{|
type total_hole_context = (int -> bool){ _ 0 } @ total -> unit
|}]

(* Pin the soundness consequence: a genuinely diverging Partial function
   cannot inhabit a refinement whose predicate calls the hole. *)
let rec diverging_function (_ : int) : bool = diverging_function 0;;
let diverging_refined_value : (int -> bool){ _ 0 } = diverging_function;;
[%%expect{|
val diverging_function : int -> bool = <fun>
Line 2, characters 4-27: refined environment entry: diverging_refined_value :
  (int -> bool){ _ 0 }
Line 2, characters 53-71: refinement obligation: (int -> bool){ _ 0 }
val diverging_refined_value : (int -> bool){ _ 0 } = <fun>
|}]

(* Every consumed stage of a curried call must be Total.  Merely producing an
   unused Partial result remains accepted in [Direct_partial_result] above. *)
module type Calls_partial_result = sig
  val returns_partial :
    unit @ total -> (unit -> bool) @ partial @@ total
  type called = bool{ returns_partial () () }
end;;
[%%expect{|
module type Calls_partial_result =
  sig
    val returns_partial : unit @ total -> unit -> bool @@ total
    type called = bool{ returns_partial () () }
  end
|}]

module type Calls_total_curried = sig
  val total_curried :
    unit @ total -> (unit @ total -> bool @ total) @ total @@ total
  type called = bool{ total_curried () () }
end;;
[%%expect{|
Line 4, characters 22-38:
4 |   type called = bool{ total_curried () () }
                          ^^^^^^^^^^^^^^^^
Error: This application is complete, but surplus arguments were provided afterwards.
       When passing or calling partial values, extra arguments are passed in a separate application.
Hint: Try wrapping the marked application in parentheses.
|}]

(* Predicate-admitted primitives retain Total intermediate stages when
   aliased locally. *)
type predicate_comparison_alias = int{
  let greater = (>) in
  greater _ 0
};;
[%%expect{|
type predicate_comparison_alias = int{ let greater = (>) in greater _ 0 }
|}]

(* Integer comparison is already admitted by the predicate language; the
   round-4 judgment keeps this case accepted while making admission depend on
   the operand type. *)
type mode_int_comparison = int{ _ > 0 };;
[%%expect{|
type mode_int_comparison = int{ _ > 0 }
|}]

(* Comparison admission is scoped to the predicate judgment.  The same
   primitive remains partial in an ordinary total closure. *)
let global_int_comparison_stays_partial @ total =
  fun (x : int) -> x > 0;;
[%%expect{|
Line 2, characters 21-22:
2 |   fun (x : int) -> x > 0;;
                         ^
Error: The value "(>)" is "partial"
       but is expected to be "total"
         because it is used inside the function at line 2, characters 2-24
         which is expected to be "total".
|}]

(* A non-immediate comparison is currently accepted because predicate reentry
   runs at the legacy mode.  GREEN2 rejects it. *)
type mode_string_comparison = string{ _ = "x" };;
[%%expect{|
type mode_string_comparison = string{ _ = "x" }
|}]

(* Division and modulo remain partial even though integer comparisons become
   predicate-scoped total operations. *)
type mode_division = int{ 10 / _ > 0 };;
[%%expect{|
type mode_division = int{ (10 / _) > 0 }
|}]

type mode_modulo = int{ 10 mod _ > 0 };;
[%%expect{|
type mode_modulo = int{ (10 mod _) > 0 }
|}]

(* The helper is total, so GREEN2 can isolate the logicality of the hole: an
   [int ref] does not cross logicality and cannot flow to a physical argument. *)
let accepts_physical_ref @ total =
  fun (_ : int ref @ physical) -> true;;
[%%expect{|
val accepts_physical_ref : int ref -> bool = <fun>
|}]

type mode_logical_hole = (int ref){ accepts_physical_ref _ };;
[%%expect{|
type mode_logical_hole = int ref{ accepts_physical_ref _ }
|}]

(* Dependent binders are spec entities too, and GREEN2 gives them the same
   logical mode as the hole. *)
type mode_logical_binder =
  x:(int ref){ accepts_physical_ref x } -> unit;;
[%%expect{|
type mode_logical_binder = x:int ref{ accepts_physical_ref x } -> unit
|}]

(* Hereditary totality reaches a function literal even when it is merely
   bound and its partial body is never called. *)
type mode_nested_fun = bool{
  let unused = fun ignored -> List.hd [] in
  true
};;
[%%expect{|
Line 2, characters 6-12:
2 |   let unused = fun ignored -> List.hd [] in
          ^^^^^^
Warning 26 [unused-var]: unused variable "unused".

type mode_nested_fun = bool{ let unused ignored = List.hd [] in true }
|}]

(* --- Refined interior constraint: payload-checked, no obligation -------- *)

(* The refined constraint checks [5] against the payload; the obligation
   Typecore records inside the predicate is discarded with the frame, so
   the probe prints no "refinement obligation" line for it. *)
type ric = int{ (5 : int{ _ > 0 }) = _ };;
[%%expect{|
type ric = int{ (5 : int{ _ > 0 }) = _ }
|}]

(* an interior constraint failing against its payload *)
type ric2 = int{ ("s" : int{ _ > 0 }) = _ };;
[%%expect{|
Line 1, characters 18-21:
1 | type ric2 = int{ ("s" : int{ _ > 0 }) = _ };;
                      ^^^
Error: This constant has type "string" but an expression was expected of type
         "int"
|}]

(* --- Ambient type variables ---------------------------------------------- *)

(* ['a{ _ = 0 }] pins ['a = int] by ordinary inference *)
let pin : 'a -> 'a{ _ = 0 } = fun x -> x;;
[%%expect{|
Line 1, characters 39-40: refinement obligation: int{ _ = 0 }
val pin : int -> int{ _ = 0 } = <fun>
|}]

(* a predicate may not introduce a new named type variable — declaration
   context (Closed policy)... *)
type nv = int{ (1 : 'newvar) = 1 };;
[%%expect{|
Line 1, characters 15-32:
1 | type nv = int{ (1 : 'newvar) = 1 };;
                   ^^^^^^^^^^^^^^^^^
Error: The type variable "'newvar" is not bound by the enclosing declaration:
       a refinement predicate cannot introduce named type variables.
|}]

(* ...and expression-annotation context (Open policy) *)
let nv2 (x : int{ (1 : 'newvar2) = 1 }) = x;;
[%%expect{|
Line 1, characters 18-36:
1 | let nv2 (x : int{ (1 : 'newvar2) = 1 }) = x;;
                      ^^^^^^^^^^^^^^^^^^
Error: The type variable "'newvar2" is not bound by the enclosing declaration:
       a refinement predicate cannot introduce named type variables.
|}]

(* the enclosing declaration's named variables remain visible, and an
   interior constraint type must not clear the enclosing bookkeeping:
   both occurrences of ['a] below are the same variable *)
let tv : 'a -> int{ (0 : 'a) = 0 } -> 'a = fun x _ -> x;;
[%%expect{|
val tv : int -> int{ (0 : int) = 0 } -> int = <fun>
|}]

(* --- Recursive groups ------------------------------------------------------ *)

(* a predicate mentioning a constructor of its own group cannot resolve it *)
type t = RC of int
and u = int{ (RC _ : t) = RC 0 };;
[%%expect{|
Line 2, characters 14-16:
2 | and u = int{ (RC _ : t) = RC 0 };;
                  ^^
Error: Unbound constructor "RC"
|}]

(* likewise a field of its own group *)
type r = { g : int }
and w = r{ _.g > 0 };;
[%%expect{|
Line 2, characters 13-14:
2 | and w = r{ _.g > 0 };;
                 ^
Error: Unbound record field "g"
|}]

(* the same restriction in a signature *)
module type SRec = sig
  type t = RD of int
  and u = int{ (RD _ : t) = RD 0 }
end;;
[%%expect{|
Line 3, characters 16-18:
3 |   and u = int{ (RD _ : t) = RD 0 }
                    ^^
Error: Unbound constructor "RD"
|}]

module type SRecField = sig
  type r = { g : int }
  and w = r{ _.g > 0 }
end;;
[%%expect{|
Line 3, characters 15-16:
3 |   and w = r{ _.g > 0 }
                   ^
Error: Unbound record field "g"
|}]

(* non-group mentions are fine *)
type earlier = B of int;;
type later = int{
  let _v = if true then (B _ : earlier) else B 0 in
  true
};;
[%%expect{|
type earlier = B of int
type later = int{ let _v = if true then (B _ : earlier) else B 0 in true }
|}]

(* --- GADT / existential constructor patterns ------------------------------- *)

type _ g = I : int g;;
[%%expect{|
type _ g = I : int g
|}]

type gd = int{ match (I : int g) with I -> _ > 0 };;
[%%expect{|
Line 1, characters 38-39:
1 | type gd = int{ match (I : int g) with I -> _ > 0 };;
                                          ^
Error: A GADT or existential-introducing constructor pattern is not supported in a refinement predicate.
|}]

type ex = E : 'a -> ex;;
[%%expect{|
type ex = E : 'a -> ex
|}]

type exd = int{ match E 1 with E _ -> true };;
[%%expect{|
Line 1, characters 31-34:
1 | type exd = int{ match E 1 with E _ -> true };;
                                   ^^^
Error: A GADT or existential-introducing constructor pattern is not supported in a refinement predicate.
|}]

type ex_pair = Ex_pair : 'a * int -> ex_pair;;
[%%expect{|
type ex_pair = Ex_pair : 'a * int -> ex_pair
|}]

(* Split these phrases so RED records both safe value-binder shapes rather than
   stopping at the first broad existential-pattern rejection. *)
type ex_safe = bool{
  match Ex_pair ((), 1) with
  | Ex_pair (_, n) -> n = n
};;
[%%expect{|
Line 3, characters 4-18:
3 |   | Ex_pair (_, n) -> n = n
        ^^^^^^^^^^^^^^
Error: A GADT or existential-introducing constructor pattern is not supported in a refinement predicate.
|}]

type ex_unused = bool{ match E 1 with E _x -> true };;
[%%expect{|
Line 1, characters 38-42:
1 | type ex_unused = bool{ match E 1 with E _x -> true };;
                                          ^^^^
Error: A GADT or existential-introducing constructor pattern is not supported in a refinement predicate.
|}]

(* RED rejects all existential constructor patterns.  GREEN narrows this
   rejection to a value binder whose arm-local type could reach persisted
   annotations. *)
type ex_bound = bool{
  match E 1 with
  | E x -> (fun _ignored -> true) x
};;
[%%expect{|
Line 3, characters 4-7:
3 |   | E x -> (fun _ignored -> true) x
        ^^^
Error: A GADT or existential-introducing constructor pattern is not supported in a refinement predicate.
|}]

(* --- Frame hygiene ------------------------------------------------------ *)

(* a failing predicate inside a signature does not corrupt subsequent
   typing *)
module type Hyg = sig
  val bad : int{ 42 }
  val good : int -> int
end;;
[%%expect{|
Line 2, characters 17-19:
2 |   val bad : int{ 42 }
                     ^^
Error: The constant "42" has type "int" but an expression was expected of type
         "bool"
|}]

let after_hyg = 1 + 1;;
[%%expect{|
val after_hyg : int = 2
|}]

(* RED2 pins the former mode-confinement boundary: the predicate commits the
   arrow shape of [f], but its totality constraint is currently rolled back.
   GREEN2 removes that confinement, so [f] is inferred [@ total]. *)
let predicate_mode_commit f =
  let (_ : bool{ f 0 }) = true in
  f;;
[%%expect{|
Line 2, characters 26-30: refinement obligation: bool{ f 0 }
val predicate_mode_commit : (int -> bool) -> int -> bool = <fun>
|}]

(* mode boundary: a partial call inside a predicate must not make the
   enclosing closure partial — the frame presents no ambient locks *)
let mode_wall @ total = fun (b : bool{ List.hd [ true ] }) -> b;;
[%%expect{|
val mode_wall : bool{ List.hd [true] } -> bool = <fun>
|}]

(* This piece is type-only: reading a ghost value is admitted and leaves the
   later mode/ghost policy undecided. *)
let uses_ghost_in_predicate (x : int @ ghost) =
  let (_ : int{ x = 0 }) = 0 in
  ();;
[%%expect{|
Line 2, characters 27-28: refinement obligation: int{ x = 0 }
val uses_ghost_in_predicate : int @ ghost -> unit = <fun>
|}]

(* ...nor does a function literal inside a predicate register against the
   enclosing closure's totality *)
let mode_wall2 @ total = fun (b : bool{ (fun z -> z) true }) -> b;;
[%%expect{|
val mode_wall2 : bool{ (fun z -> z) true } -> bool = <fun>
|}]

(* a mutable-variable read inside a predicate is an ordinary occurrence,
   and does not count as a capture by an enclosing closure *)
let mode_wall3 () =
  let mutable mw = 1 in
  ignore (fun (b : bool{ mw > 0 }) -> b);
  mw <- 2;;
[%%expect{|
val mode_wall3 : unit -> unit = <fun>
|}]

(* Instance-variable reads have distinct typedtree forms.  RED pins both the
   immutable and mutable cases before the mirror represents the former and
   mode checking diagnoses the latter. *)
class immutable_instance_predicate = object
  val x = 1
  method check =
    ignore (fun (_ : bool{ x = x }) -> ());
    true
end;;
[%%expect{|
Line 4, characters 27-28:
4 |     ignore (fun (_ : bool{ x = x }) -> ());
                               ^
Error: An elaborated identifier is not supported in a refinement predicate.
|}]

class mutable_instance_predicate = object
  val mutable x = 1
  method check =
    ignore (fun (_ : bool{ x = x }) -> ());
    true
end;;
[%%expect{|
Line 4, characters 27-28:
4 |     ignore (fun (_ : bool{ x = x }) -> ());
                               ^
Error: An elaborated identifier is not supported in a refinement predicate.
|}]

(* --- Predicate-local binder in a nested refinement, freshened ------------- *)

(* [k] is a predicate-local binder mentioned by a nested refinement's
   interior type; functor application copies the signature with fresh
   binder stamps, and the copy must remain alpha-equal to the original. *)
module type SK = sig
  val v : int{ let k = 1 in (k : int{ _ = k }) = 1 }
end;;
[%%expect{|
module type SK = sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
|}]

module FK (X : SK) = X;;
[%%expect{|
module FK :
  functor (X : SK) ->
    sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
|}]

module MK = struct
  let v : int{ let k = 1 in (k : int{ _ = k }) = 1 } = 1
end;;
[%%expect{|
Line 2, characters 6-7: refined environment entry: v :
  int{ let k = 1 in (k : int{ _ = k }) = 1 }
Line 2, characters 55-56: refinement obligation:
  int{ let k = 1 in (k : int{ _ = k }) = 1 }
module MK : sig val v : int{ let k = 1 in (k : int{ _ = k }) = 1 } end
|}]

module GK : SK = FK (MK);;
[%%expect{|
module GK : SK
|}]

(* --- Signature sequencing --------------------------------------------------- *)

(* a signature predicate sees earlier items only *)
module type Seq = sig
  val bound : int
  val v : int{ _ > bound }
end;;
[%%expect{|
module type Seq = sig val bound : int val v : int{ _ > bound } end
|}]

module type SeqBad = sig
  val v : int{ _ > bound }
  val bound : int
end;;
[%%expect{|
Line 2, characters 19-24:
2 |   val v : int{ _ > bound }
                       ^^^^^
Error: Unbound value "bound"
|}]
