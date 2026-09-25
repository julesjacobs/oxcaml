(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml";
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
    let first = Slice.finish s in
    let second = Slice.length (borrow_ s) in
    ()
end;;
[%%expect{|
Line 4, characters 30-41:
4 |     let second = Slice.length (borrow_ s) in
                                  ^^^^^^^^^^^
Error: This value is borrowed here,
       but it has already been used as unique at:
Line 3, characters 29-30:
3 |     let first = Slice.finish s in
                                 ^

|}]

module Parent_during_borrow = struct
  let rejected (a : int Owned_array.t @ unique) =
    let[@def] (post @ total) (u : unit @ immutable) (after : int list @ immutable) = ghost_ true in
    let erased = ghost_ post in
    let result = Owned_array.with_mut a erased (fun loan ->
      let s = loan in
      let array = Owned_array.into_iarray a in
      let u = Slice.finish s in
      u) in
    ()
end;;
[%%expect{|
Line 7, characters 42-43:
7 |       let array = Owned_array.into_iarray a in
                                              ^
Error: This value is used here, but it is also being used as unique at:
Line 5, characters 38-39:
5 |     let result = Owned_array.with_mut a erased (fun loan ->
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
    let result = Owned_array.with_mut a erased (fun loan ->
      let s = loan in
      let u = Slice.finish s in
      u) in
    result
end;;
[%%expect{|
Line 8, characters 6-7:
8 |       u) in
          ^
Error: Refinement could not be proved (counterexample)
|}]

module Out_of_bounds = struct
  let rejected (s : int Slice.t @ local unique) =
    let size = Slice.length (borrow_ s) in
    let state = s in
    let index : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current state)) < 0} =
      size in
    let _ = Slice.get (borrow_ state) index in
    ()
end;;
[%%expect{|
Line 7, characters 6-10:
7 |       size in
          ^^^^
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
    let changed = Slice.set s index value in
    let _ = Slice.finish changed in
    let u = () in u
end;;
[%%expect{|
Line 8, characters 18-19:
8 |     let u = () in u
                      ^
Error: Refinement could not be proved (counterexample)
|}]

module Shared_element = struct
  type items : immutable_data = int list
  let require_unique (values : items @ unique) = ()
  let rejected : (s : items Slice.t) @ local unique ->
      (index : {i : int | 0 <= i
        && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0}) -> unit =
      fun s index ->
    let value = Slice.get (borrow_ s) index in
    require_unique value
end;;
[%%expect{|
Line 9, characters 19-24:
9 |     require_unique value
                       ^^^^^
Error: This value is "aliased" but is expected to be "unique".
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
    let _ = (u : {u : unit | equal}) in
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
    let n = Slice.length (borrow_ s) in
    let state = s in
    let half = n / 2 in
    let cut : {i : int | 0 <= i
      && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current state)) <= 0} =
      half in
    let result = Slice.split_at state cut erased (fun l r ->
      let left = l in
      let right = r in
      let lf = ghost_ (Slice.final (borrow_ left)) in
      let rf = ghost_ (Slice.final (borrow_ right)) in
      let _ = Slice.finish left in
      let _ = Slice.finish right in
      let u = () in
      let _ = ghost_ (post_def u lf rf) in
      u) in
    let {state; _} = result in
    let _ = Slice.finish state in
    if n = 2 then
      let u = () in
      let _ = (u : {u : unit | false}) in ()
end;;
[%%expect{|
Line 26, characters 15-16:
26 |       let _ = (u : {u : unit | false}) in ()
                    ^
Error: Refinement could not be proved (counterexample)
|}]

module Frame_child_extent = struct
  type frame : value mod total contended
  external split : (s : int Slice.t) @ local unique -> int ->
    (frame * int Slice.t * int Slice.t) @ unique = "caml_borrow_split"
  external left_final : frame @ local immutable -> int Model.t @ immutable total ghost
    @@ total = "caml_borrow_frame_left"

  let rejected (s : int Slice.t @ local unique) =
    let n = Slice.length (borrow_ s) in
    let state = s in
    if n = 2 then (
      let frame, left, right = split state 1 in
      let _lf = ghost_ (Slice.final (borrow_ left)) in
      let _ff = ghost_ (left_final (borrow_ frame)) in
      let _ = Slice.finish left in
      let _ = Slice.finish right in
      let u = () in
      let _ = (u : {u : unit | false}) in ())
end;;
[%%expect{|
Line 18, characters 15-16:
18 |       let _ = (u : {u : unit | false}) in ())
                    ^
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

module Overlapping_access = struct
  let rejected (s : int Slice.t @ local unique) =
    let view = borrow_ s in
    let closed = Slice.finish s in
    let size = Slice.length view in
    ()
end;;
[%%expect{|
Line 4, characters 30-31:
4 |     let closed = Slice.finish s in
                                  ^
Error: This value is used as "unique" here, but it is being borrowed.
Line 3, characters 15-24:
3 |     let view = borrow_ s in
                   ^^^^^^^^^
  The value is being borrowed
Lines 3-6, characters 4-6:
3 | ....let view = borrow_ s in
4 |     let closed = Slice.finish s in
5 |     let size = Slice.length view in
6 |     ()
  during this borrow context
|}]
