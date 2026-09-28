(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml vox_parallel.mli vox_parallel.ml vox_int_sequence.mli vox_int_sequence.ml quicksort_model.ml quicksort.mli quicksort.ml";
 readonly_files = "borrow_partial.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

(* A total function is a function of its arguments. Creating a loan chooses
   its final contents in advance, and [Slice.finish] assumes that they equal
   its current contents; neither is a function of the arguments. So the
   operations that lend a slice ([Slice.split_at], [Slice.split3],
   [Slice.with_range], [Owned_array.with_mut]) and [Slice.finish] are not
   total, and erased code, lemmas and total functions cannot use them.
   Operations that take a slice or an owned array and return it
   ([Slice.set], [Owned_array.set], [Owned_array.split_at], ...) stay total.
   Verdicts only; each rejected program has an accepted control. *)

#load "vox_sequence.cmo";;
#load "borrow.cmo";;
#load "vox_parallel.cmo";;
#load "vox_int_sequence.cmo";;
#load "quicksort_model.cmo";;
#load "quicksort.cmo";;

open Borrow
module Spec = Vox_int_sequence;;
[%%expect{|
module Spec = Vox_int_sequence
|}]

(* Erased code. [ghost_] also makes [s] aliased, but the totality check
   rejects the call first. *)
let erased_finish (s : int Slice.t @ local unique) =
  ghost_ (Slice.finish s);;
[%%expect{|
Line 2, characters 10-22:
2 |   ghost_ (Slice.finish s);;
              ^^^^^^^^^^^^
Error: The value "Slice.finish" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 2-25).
|}]

(* The loan is created inside the erased code, so uniqueness is not what
   rejects it. *)
let erased_loan (values : int iarray) =
  ghost_ (
    let a = Owned_array.of_iarray values in
    let post = fun (_ : unit) (_ : int Model.t @ immutable) -> true in
    let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
    let {state; _} = result in
    Owned_array.into_iarray state);;
[%%expect{|
Line 5, characters 17-37:
5 |     let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
                     ^^^^^^^^^^^^^^^^^^^^
Error: The value "Owned_array.with_mut" is "partial"
       but is expected to be "total"
         because it is used in an expression (at lines 2-7, characters 2-34).
|}]

(* Control: writing an owned array in erased code. *)
let erased_write (values : int iarray) =
  ghost_ (
    let a = Owned_array.of_iarray values in
    let a =
      if Owned_array.length (borrow_ a) > 0 then Owned_array.set a 0 1
      else a in
    Owned_array.into_iarray a);;
[%%expect{|
val erased_write : int iarray -> int iarray @ ghost = <fun>
|}]

(* A total function cannot create a loan. *)
let (lend @ total) (a : int Owned_array.t @ unique) =
  let post = ghost_ (fun (_ : unit) (_ : int Model.t @ immutable) -> true) in
  let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
  let {state; _} = result in
  state;;
[%%expect{|
Line 3, characters 15-35:
3 |   let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
                   ^^^^^^^^^^^^^^^^^^^^
Error: The value "Owned_array.with_mut" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-5, characters 19-7
         which is expected to be "total".
|}]

(* Control: the same function, not declared total. *)
let lend (a : int Owned_array.t @ unique) =
  let post = ghost_ (fun (_ : unit) (_ : int Model.t @ immutable) -> true) in
  let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
  let {state; _} = result in
  state;;
[%%expect{|
val lend : int Borrow.Owned_array.t @ unique -> int Borrow.Owned_array.t =
  <fun>
|}]

(* A lemma is a total function. Proved by calling [finish], this one would
   state that every slice's final contents are its current contents. *)
let (resolve_lemma @ total) : (s : int Slice.t) @ local unique ->
    {u : unit | Slice.final s === Slice.current s} = fun s ->
  Slice.finish s;;
[%%expect{|
Line 3, characters 2-14:
3 |   Slice.finish s;;
      ^^^^^^^^^^^^
Error: The value "Slice.finish" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 2-3, characters 53-16
         which is expected to be "total".
|}]

(* Control: the same function, not declared total. *)
let resolve : (s : int Slice.t) @ local unique ->
    {u : unit | Slice.final s === Slice.current s} = fun s ->
  Slice.finish s;;
[%%expect{|
val resolve :
  (s : int Borrow.Slice.t) @ local unique ->
  {u : unit | (Borrow.Slice.final s) === (Borrow.Slice.current s)} = <fun>
|}]

(* Control: writing a slice keeps its final contents and stays total. *)
let (write_zero @ total) (s : int Slice.t @ local unique)
    : int Slice.t @ local unique =
  exclave_ (if Slice.length (borrow_ s) > 0 then Slice.set s 0 0 else s);;
[%%expect{|
val write_zero :
  int Borrow.Slice.t @ local unique -> int Borrow.Slice.t @ local unique =
  <fun>
|}]

(* The total sort on owned arrays proves sorted and a permutation. *)
let (sorted_copy @ total) : (values : int iarray) ->
    {r : int iarray | Spec.sorted (Model.of_iarray r)
      && Spec.permutation (Model.of_iarray values) (Model.of_iarray r)} =
    fun values ->
  let a = Owned_array.of_iarray values in
  let sorted = Quicksort.sort_array a in
  Owned_array.into_iarray sorted;;
[%%expect{|
val sorted_copy :
  (values : int iarray) ->
  {r : int iarray
    | (Spec.sorted (Borrow.Model.of_iarray r)) &&
        (Spec.permutation (Borrow.Model.of_iarray values)
           (Borrow.Model.of_iarray r))} =
  <fun>
|}]

let () =
  assert (Iarray.to_list (sorted_copy [: 3; 1; 2; 1 :]) = [1; 1; 2; 3]);;
[%%expect{|
|}]

(* Control: the sort does not keep the order. *)
let (unsorted_copy @ total) : (values : int iarray) ->
    {r : int iarray | r === values} = fun values ->
  let a = Owned_array.of_iarray values in
  let sorted = Quicksort.sort_array a in
  Owned_array.into_iarray sorted;;
[%%expect{|
Line 5, characters 2-32:
5 |   Owned_array.into_iarray sorted;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 22-34:
2 |     {r : int iarray | r === values} = fun values ->
                          ^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Erased code can run the total sort, but not the borrowing one. *)
let erased_sort (values : int iarray) =
  ghost_ (Owned_array.into_iarray
    (Quicksort.sort_array (Owned_array.of_iarray values)));;
[%%expect{|
val erased_sort : int iarray -> int iarray @ ghost = <fun>
|}]

let erased_borrowing_sort (values : int iarray) =
  ghost_ (
    let a = Owned_array.of_iarray values in
    let post = fun (_ : unit) (_ : int Model.t @ immutable) -> true in
    let result = Owned_array.with_mut a post (fun s -> Quicksort.sort s) in
    let {state; _} = result in
    Owned_array.into_iarray state);;
[%%expect{|
Line 5, characters 17-37:
5 |     let result = Owned_array.with_mut a post (fun s -> Quicksort.sort s) in
                     ^^^^^^^^^^^^^^^^^^^^
Error: The value "Owned_array.with_mut" is "partial"
       but is expected to be "total"
         because it is used in an expression (at lines 2-7, characters 2-34).
|}]

(* Calls of a total function with equal arguments give equal results. A
   function that lends a slice cannot be declared total (as [lend] above
   shows), so two of its calls are not known to agree. *)
let first_after_lend (values : {v : int iarray | Iarray.length v > 0}) : int =
  let a = Owned_array.of_iarray values in
  let post = ghost_ (fun (_ : unit) (_ : int Model.t @ immutable) -> true) in
  let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
  let {state; _} = result in
  let n = Owned_array.length (borrow_ state) in
  if n > 0 then Owned_array.get (borrow_ state) 0 else 0;;
[%%expect{|
val first_after_lend : {v : int iarray | (Iarray.length v) > 0} -> int =
  <fun>
|}]

let lend_twice (values : {v : int iarray | Iarray.length v > 0})
    : {b : bool | b} =
  first_after_lend values = first_after_lend values;;
[%%expect{|
Line 3, characters 2-51:
3 |   first_after_lend values = first_after_lend values;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 18-19:
2 |     : {b : bool | b} =
                      ^
  The refinement is stated here.
|}]

(* Control: the same read without a loan is total, and its calls agree. *)
let (first_owned @ total) (values : {v : int iarray | Iarray.length v > 0})
    : int =
  let a = Owned_array.of_iarray values in
  let n = Owned_array.length (borrow_ a) in
  if n > 0 then Owned_array.get (borrow_ a) 0 else 0;;
[%%expect{|
val first_owned : {v : int iarray | (Iarray.length v) > 0} -> int = <fun>
|}]

let owned_twice (values : {v : int iarray | Iarray.length v > 0})
    : {b : bool | b} =
  first_owned values = first_owned values;;
[%%expect{|
val owned_twice : {v : int iarray | (Iarray.length v) > 0} -> {b : bool | b} =
  <fun>
|}]
