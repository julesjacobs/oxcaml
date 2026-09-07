(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "borrow_model.mli borrow_model.ml borrow.mli borrow.ml";
 readonly_files = "borrow_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   flags += " -principal";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)

open Borrow;;
[%%expect{|
|}]

module Stale_handle = struct
  let rejected (s : int Slice.t @ local unique) =
    let refine_ first = Slice.length s in
    let refine_ second = Slice.length s in
    ()
end;;
[%%expect{|
Line 4, characters 38-39:
4 |     let refine_ second = Slice.length s in
                                          ^
Error: This value is used here, but it has already been used as unique at:
Line 3, characters 37-38:
3 |     let refine_ first = Slice.length s in
                                         ^

|}]

module Parent_during_borrow = struct
  let rejected (a : int Owned_array.t @ unique) =
    let[@def] (post @ total) (u : unit @ immutable) (after : int list @ immutable) = ghost_ true in
    let erased = ghost_ post in
    let refine_ result = Owned_array.with_mut a erased (fun loan ->
      let refine_ s = loan in
      let refine_ array = Owned_array.into_iarray a in
      let refine_ u = Slice.finish s in
      refine_ u) in
    ()
end;;
[%%expect{|
Line 7, characters 50-51:
7 |       let refine_ array = Owned_array.into_iarray a in
                                                      ^
Error: This value is used here, but it is also being used as unique at:
Line 5, characters 46-47:
5 |     let refine_ result = Owned_array.with_mut a erased (fun loan ->
                                                  ^

|}]

module Escaping_loan = struct
  let rejected (s : int Slice.t @ local unique) : int Slice.t @ global unique = s
end;;
[%%expect{|
Line 2, characters 80-81:
2 |   let rejected (s : int Slice.t @ local unique) : int Slice.t @ global unique = s
                                                                                    ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

module Unproved_postcondition = struct
  let rejected (a : int Owned_array.t @ unique) =
    let[@def] (post @ total) (u : unit @ immutable) (after : int list @ immutable) = ghost_ false in
    let erased = ghost_ post in
    let refine_ result = Owned_array.with_mut a erased (fun loan ->
      let refine_ s = loan in
      let refine_ u = Slice.finish s in
      refine_ u) in
    result
end;;
[%%expect{|
Line 8, characters 6-15:
8 |       refine_ u) in
          ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Out_of_bounds = struct
  let rejected (s : int Slice.t @ local unique) =
    let refine_ sized = Slice.length s in
    let {value = (size : int); state} = sized in
    let index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current state)) < 0} =
      refine_ size in
    let refine_ result = Slice.get state index in
    ()
end;;
[%%expect{|
Line 7, characters 6-18:
7 |       refine_ size in
          ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Real_final = struct
  let rejected (s : int Slice.t @ local unique) : int list @ real =
    Slice.final (borrow_ s)
end;;
[%%expect{|
Line 3, characters 4-27:
3 |     Slice.final (borrow_ s)
        ^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

module Runtime_final_check = struct
  let rejected (s : int Slice.t @ local unique) =
    let u = () in
    let checked : {u : unit | Slice.final s === []} = assume_ u in
    checked
end;;
[%%expect{|
Line 4, characters 30-43:
4 |     let checked : {u : unit | Slice.final s === []} = assume_ u in
                                  ^^^^^^^^^^^^^
Error: This value is "ghost" but is expected to be "real".
|}]

module Stale_model = struct
  let rejected : (s : int Slice.t) @ local unique ->
      (index : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0}) ->
      (value : int) -> {u : unit | Slice.final s === Slice.current s} = fun s index value ->
    let refine_ changed = Slice.set s index value in
    let refine_ closed = Slice.finish changed in
    let u = () in refine_ u
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Shared_element = struct
  type items : immutable_data = int list
  let require_unique (values : items @ unique) = ()
  let rejected : (s : items Slice.t) @ local unique ->
      (index : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0}) -> unit =
      fun s index ->
    let refine_ result = Slice.get s index in
    let {value; state} = result in
    require_unique value
end;;
[%%expect{|
Line 10, characters 19-24:
10 |     require_unique value
                        ^^^^^
Error: This value is "aliased"
         because it is the field "value" (with some modality) of the record at line 9, characters 8-22.
       However, the highlighted expression is expected to be "unique".
|}]

module Raw_is_sealed = struct
  let rejected = Borrow.Raw.open_
end;;
[%%expect{|
Line 2, characters 17-27:
2 |   let rejected = Borrow.Raw.open_
                     ^^^^^^^^^^
Error: Unbound module "Borrow.Raw"
|}]

module Local_observation = struct
  let check (values : int list @ local immutable) =
    let equal = ghost_ (values === values) in
    let u = () in
    let refine_ proof = (refine_ u : {u : unit | equal}) in
    ()
end;;
[%%expect{|
module Local_observation :
  sig val check : int list @ local immutable -> unit end
|}]

module Runtime_local_escape = struct
  let rejected (values : int list @ local) : int list @ global = values
end;;
[%%expect{|
Line 2, characters 65-71:
2 |   let rejected (values : int list @ local) : int list @ global = values
                                                                     ^^^^^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

module Partial_ghost = struct
  let partial () = print_endline "effect"; 1
  let rejected () = ghost_ (partial ())
end;;
[%%expect{|
Line 3, characters 28-35:
3 |   let rejected () = ghost_ (partial ())
                                ^^^^^^^
Error: The value "partial" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 3, characters 20-39).
|}]

module Effectful_descent = struct
  let rec scan (n : int) =
    if n > 0 then (print_endline "scan"; scan (n - 1))
  [@@decreases let n : int = n in n]
end;;
[%%expect{|
module Effectful_descent : sig val scan : int -> unit end
|}]

module Unchanged_measure = struct
  let rec rejected (n : int) =
    print_endline "scan";
    rejected n
  [@@decreases let n : int = n in n]
end;;
[%%expect{|
Line 4, characters 4-14:
4 |     rejected n
        ^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 15-35:
5 |   [@@decreases let n : int = n in n]
                   ^^^^^^^^^^^^^^^^^^^^
  Required by this decreases attribute
|}]

module Split_consistency = struct
  let rejected (s : int Slice.t @ local unique) =
    let[@def] (post @ total) (u : unit @ immutable)
        (left : int Model.t @ immutable) (right : int Model.t @ immutable) = ghost_ true in
    let erased = ghost_ post in
    let refine_ size = Slice.length s in
    let {value = n; state} = size in
    let half = n / 2 in
    let cut : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current state)) <= 0} =
      refine_ half in
    let refine_ result = Slice.split_at state cut erased (fun l r ->
      let refine_ left = l in
      let refine_ right = r in
      let lf = ghost_ (Slice.final (borrow_ left)) in
      let rf = ghost_ (Slice.final (borrow_ right)) in
      let refine_ lclosed = Slice.finish left in
      let refine_ rclosed = Slice.finish right in
      let u = () in
      let refine_ equation = ghost_ (post_def u lf rf) in
      refine_ u) in
    let {state; _} = result in
    let refine_ closed = Slice.finish state in
    if n = 2 then
      let u = () in
      let refine_ impossible = (refine_ u : {u : unit | false}) in ()
end;;
[%%expect{|
Line 26, characters 32-41:
26 |       let refine_ impossible = (refine_ u : {u : unit | false}) in ()
                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Frame_child_extent = struct
  type frame : value mod total contended
  external split : (s : int Slice.t) @ local unique -> int ->
    (frame * int Slice.t * int Slice.t) @ unique = "caml_borrow_split"
  external left_final : frame @ local immutable -> int Model.t @ immutable total ghost
    @@ total = "caml_borrow_frame_left"

  let rejected (s : int Slice.t @ local unique) =
    let refine_ sized = Slice.length s in
    let {value = n; state} = sized in
    if n = 2 then (
      let frame, left, right = split state 1 in
      let _lf = ghost_ (Slice.final (borrow_ left)) in
      let _ff = ghost_ (left_final (borrow_ frame)) in
      let refine_ lclosed = Slice.finish left in
      let refine_ rclosed = Slice.finish right in
      let u = () in
      let refine_ impossible = (refine_ u : {u : unit | false}) in ())
end;;
[%%expect{|
Line 18, characters 32-41:
18 |       let refine_ impossible = (refine_ u : {u : unit | false}) in ())
                                     ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Mutable_ghost_observation = struct
  let rejected (values : int array) = ghost_ values.(0)
end;;
[%%expect{|
Line 2, characters 45-55:
2 |   let rejected (values : int array) = ghost_ values.(0)
                                                 ^^^^^^^^^^
Error: The value "Array.get" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 38-55).
|}]
