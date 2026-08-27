(* TEST
 modules = "producer.ml";
 flags = "-extension refinement_types -noassert -warn-error +11";
 { bytecode; }
 { native; }
*)

let ensure condition = if not condition then failwith "imported assume_"

module Alias = Producer
module Producer = struct
  type t = A of bool | B
  type record = { other : string; value : bool }
  let equal _ _ = false
end

let () =
  let x = 0 in
  let checked : Alias.zero = assume_ x in
  let refine_ result = checked in
  ensure (result = 0);
  let checked : Alias.optional = assume_ x in
  ignore checked;
  let checked : Alias.suppressed_warning = assume_ x in
  ignore checked;
  let checked : Alias.structured = assume_ x in
  ignore checked;
  let checked : Alias.variant_predicate = assume_ x in
  ignore checked;
  let checked : Alias.dependent = assume_ x in
  ignore checked;
  let checked : Alias.direct_dependent = assume_ x in
  ignore checked;
  let checked : Alias.local_dependent = assume_ x in
  ignore checked

let () =
  let x = 1 in
  match let checked : Alias.dependent = assume_ x in ignore checked with
  | () -> failwith "imported dependent argument"
  | exception Assert_failure _ -> ()

module Input = struct let check x = x = 7 end
module Instantiated = Alias.F (Input)

let () =
  let x = 7 in
  let checked : Instantiated.checked = assume_ x in
  ignore checked;
  let x = 8 in
  let line = __LINE__ + 1 in
  match let checked : Instantiated.checked = assume_ x in ignore checked with
  | () -> failwith "imported functor check omitted"
  | exception Assert_failure (file, actual_line, _) ->
      ensure (file = __FILE__ && actual_line = line)
