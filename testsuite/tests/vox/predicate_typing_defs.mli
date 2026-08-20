type int_record = { selected : int }
type string_record = { selected : string }

type selected_field = int_record{ _.selected > 0 }

type int_variant = Selected of int
type string_variant = Selected of string

type selected_constructor = int_variant{ _ = Selected 0 }

type selected_application = int{ (fun n -> n + 1) _ > 0 }

type dependent_hole = (x:int -> int{ _ >= x }){ _ = _ }

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
