(* TEST
 modules = "assume_stubs.c";
 flags = "-extension refinement_types";
 has-z3;
 { bytecode; }
 { native; }
 { flags += " -noassert"; bytecode; }
 { flags += " -noassert"; native; }
*)

let ensure condition = if not condition then failwith "assume_ runtime"

external counted : int -> bool @@ total = "caml_assume_counted"
external predicate_calls : unit -> int = "caml_assume_predicate_calls"

type counted_zero = { v : int | counted v }

let () =
  let x = 0 in
  let checked : counted_zero = assume_ x in
  ignore checked;
  ensure (predicate_calls () = 1);
  let x = 1 in
  match let checked : counted_zero = assume_ x in ignore checked with
  | () -> failwith "missing counted failure"
  | exception Assert_failure _ -> ensure (predicate_calls () = 2)

type zero = { v : int | v = 0 }

let () =
  let calls = ref 0 in
  let x = incr calls; 0 in
  let checked : zero = assume_ x in
  let refine_ result = checked in
  ensure (result = 0 && !calls = 1)

let () =
  let x = 1 in
  let line = __LINE__ + 3 in
  match
    let checked : zero =
      assume_ x
    in
    ignore checked
  with
  | () -> failwith "missing mandatory check"
  | exception Assert_failure (file, actual_line, column) ->
      ensure (file = __FILE__ && actual_line = line && column = 6)

let () =
  let r = ref 0 in
  let checked : { v : int ref | true } = assume_ r in
  let refine_ result = checked in
  ensure (r == result)

external equal : int -> int -> bool @@ total = "%equal"
type shadowed_value = { v : int | equal v 0 }
let equal _ _ = false

let () =
  let x = 0 in
  let checked : shadowed_value = assume_ x in
  ignore checked

module Names = struct
  type t = A of int | B
  type record = { value : int; other : int }
  external equal : int -> int -> bool @@ total = "%equal"
end

type structured =
  { v : int |
    let record = { Names.value = v; other = 2 } in
    let changed = { record with Names.other = 3 } in
    let pair = v, changed.Names.value in
    let array = [|v; changed.Names.other|] in
    let immutable_array = [: v; changed.Names.other :] in
    let identity x = x in
    let f x y = Names.equal x y in
    let choose = Names.A (identity changed.Names.value) in
    ignore array;
    ignore immutable_array;
    match choose with
    | Names.A n when f n v ->
        (match pair with (a, b) as p -> ignore p; Names.equal a b)
    | Names.A _ -> false
    | Names.B -> true }

let () =
  let module Names = struct
    type t = A of bool | B
    type record = { other : string; value : bool }
    let equal _ _ = false
  end in
  ignore Names.B;
  let x = 4 in
  let checked : structured = assume_ x in
  ignore checked

let dependent :
    (x : int) -> (y : int) -> { z : unit | x + y = y + x } =
  fun x y -> let u = () in assume_ u

let () =
  let x = 2 in
  let y = 3 in
  let refine_ proof = dependent x y in
  proof

let above : (r : zero) -> { y : int | let refine_ x = r in y > x } =
  fun r -> let y = 1 in assume_ y

let () =
  let x = 0 in
  let r : zero = assume_ x in
  let refine_ y = above r in
  ensure (y = 1)

type polymorphic_local =
  { v : int |
    let identity x = x in
    if identity true then identity v = v else false }

let () =
  let x = 42 in
  let checked : polymorphic_local = assume_ x in
  ignore checked

let dependent_identity : (x : int) -> { z : int | z = x } =
  fun x -> refine_ x

type nested_dependent =
  { x : int |
    let y = x in
    let refine_ z = dependent_identity y in
    z = x }

let () =
  let x = 42 in
  let checked : nested_dependent = assume_ x in
  ignore checked

let n = 0
type shadowed_dependent =
  { x : int | let refine_ z = dependent_identity n in z = x }
let check_shadowed n x : shadowed_dependent =
  ignore n;
  assume_ x

let () =
  ignore (check_shadowed 1 0);
  match check_shadowed 0 1 with
  | _ -> failwith "shadowed dependent argument"
  | exception Assert_failure _ -> ()

let optional ?(value = 0) x = value = x
let absent ?value (x : int) =
  match value with None -> x = 0 | Some _ -> false
type omitted_optional = { x : int | optional x }
type omitted_polymorphic_optional = { x : int | absent x }

let () =
  let x = 0 in
  let checked : omitted_optional = assume_ x in
  ignore checked;
  let checked : omitted_polymorphic_optional = assume_ x in
  ignore checked;
  let x = 1 in
  match
    let checked : omitted_polymorphic_optional = assume_ x in ignore checked
  with
  | () -> failwith "omitted optional argument"
  | exception Assert_failure _ -> ()

let () =
  let f x = x + 1 in
  let checked : { g : int -> int | g 0 = 1 } = assume_ f in
  let refine_ g = checked in
  ensure (f == g)

type unboxed_record = #{ value : int; other : int }

type unboxed_predicate =
  { x : int |
    let r = #{ value = x; other = 0 } in
    let _updated = #{ r with other = x } in
    match x with 0 -> true | _ -> true }

let () =
  let x = 42 in
  let checked : unboxed_predicate = assume_ x in
  ignore checked

type unboxed_variant = Unboxed of int [@@unboxed]
type unboxed_constructor =
  { x : int | match Unboxed x with Unboxed y -> y = x }
type float_record = { floating : float }
external float_equal : float -> float -> bool @@ total = "%equal"
type float_field =
  { x : int |
    let r = { floating = 2.5 } in
    ignore "literal";
    if x = 0 then ();
    if x = 0 then float_equal r.floating 2.5
    else float_equal r.floating 2.5 }

let () =
  let x = 42 in
  let checked : unboxed_constructor = assume_ x in
  ignore checked;
  let checked : float_field = assume_ x in
  ignore checked

[@@@warnerror "+11"]
type suppressed_warning =
  { v : int | (match v with _ -> true | _ -> false) [@warning "-11"] }

let () =
  let x = 0 in
  let checked : suppressed_warning = assume_ x in
  ignore checked

let labelled ~a ~b x = a = b + x
type commuted_partial =
  { v : int | let g = labelled ~b:0 in g ~a:v v }

let () =
  let x = 42 in
  let checked : commuted_partial = assume_ x in
  ignore checked

type shadowed_bool = false | true
[@@@warning "@42"]

let () =
  let x = 1 in
  match let checked : zero = assume_ x in ignore checked with
  | () -> failwith "shadowed false"
  | exception Assert_failure _ -> ()
