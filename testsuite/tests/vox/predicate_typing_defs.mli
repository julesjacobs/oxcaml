type int_record = { selected : int }
type string_record = { selected : string }

type selected_field = int_record{ _.selected > 0 }

type int_variant = Selected of int
type string_variant = Selected of string

type selected_constructor =
  int_variant{ let _v = if true then _ else Selected 0 in true }

type selected_application = int{ (fun n -> n + 1) _ > 0 }

(* Round-4 application completion and typedtree-rewrite forms.  Keep each
   manifest separate so the raw-CMI test can inspect one discriminating shape
   at a time. *)
val mirror_labelled :
  x:int @ total -> (y:bool @ total -> bool @ total) @ total @@ total
val mirror_optional :
  ?o:int @ total -> (unit @ total -> bool @ total) @ total @@ total
val mirror_optional_labelled :
  ?o:int @ total -> (y:bool @ total -> bool @ total) @ total @@ total
val mirror_positional :
  p:[%call_pos] @ total ->
  (y:bool @ total ->
   (unit @ total -> bool @ total) @ total) @ total @@ total
val mirror_positional_eta :
  p:[%call_pos] @ total ->
  (unit @ total -> bool @ total) @ total @@ total
(* [%apply]/[%revapply] preserve the operand mode relationally. *)
val mirror_id : bool @ total -> bool @ total @@ total
val mirror_accepts_format :
  (int -> string, unit, string) format @ total -> bool @ total @@ total
val mirror_accepts_unlabelled :
  (unit @ total -> bool @ total) @ total -> bool @ total @@ total

type completion_source =
  bool{ mirror_labelled ~y:true ~x:0 }

type completion_wrapper =
  bool{ mirror_optional ~o:0 () }

type completion_default =
  bool{ mirror_optional () }

type completion_eta_default =
  bool{ mirror_accepts_unlabelled mirror_optional }

type completion_eta_call_pos =
  bool{ mirror_accepts_unlabelled mirror_positional_eta }

type completion_omitted_optional =
  bool{ let _f = mirror_optional_labelled ~y:true in true }

type completion_call_pos =
  bool{ mirror_positional ~y:true () }

type completion_omitted_position =
  bool{ let _f = mirror_positional ~y:true in true }

type completion_omitted_required =
  bool{ let _f = mirror_labelled ~y:true in true }

type primitive_apply = bool{ mirror_id @@ true }
type primitive_revapply = bool{ true |> mirror_id }

type format_mirror = bool{ mirror_accepts_format "%d" }

type _ mirror_gadt = Mirror_int : int mirror_gadt
type gadt_mirror =
  int{ match (Mirror_int : int mirror_gadt) with Mirror_int -> _ > 0 }

type mirror_exists = Mirror_pack : 'a -> mirror_exists
type existential_mirror =
  bool{
    match Mirror_pack 0 with
    | Mirror_pack _ -> true
  }

type mirror_exists_pair = Mirror_pair : 'a * int -> mirror_exists_pair
type existential_bound_mirror =
  bool{
    match Mirror_pair ((), 0) with
    | Mirror_pair (_, n) -> n = n
  }

type dependent_hole =
  (x:int -> int{ _ >= x }){
    let _f = if true then _ else _ in
    true
  }

type generic_own_domain =
  x:(int{ fst x > 0 } * int) -> unit

module Binder (X : sig val zero : int end) : sig
  type t =
    int{
      let id = fun x -> (x : int{ x = X.zero }) in
      id 0 = 0
    }

  type stored =
    bool{
      let _f = fun x -> ([] : int{ _ = x } list) in
      true
    }
end

module Binder_source : sig
  val zero : int
end

module Binder_result : module type of Binder (Binder_source)

module Field_copy (X : sig type t = { picked : int } end) : sig
  type t = X.t{ _.X.picked > 0 }
end

module Field_source : sig
  type t = { picked : int }
end

module Field_result : module type of Field_copy (Field_source)
