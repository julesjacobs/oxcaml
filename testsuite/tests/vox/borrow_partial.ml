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
   ([Slice.set], [Owned_array.split_at], ...) stay total.
   Verdicts only; each rejected program has an accepted control. *)

#load "vox_sequence.cmo";;
#load "borrow.cmo";;
#load "vox_parallel.cmo";;
#load "vox_int_sequence.cmo";;
#load "quicksort_model.cmo";;
#load "quicksort.cmo";;

open Borrow;;
[%%expect{|
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

(* Control: splitting an owned array in erased code. *)
let erased_split (values : int iarray) =
  ghost_ (
    let a = Owned_array.of_iarray values in
    let _, right = Owned_array.split_at a 0 in
    Owned_array.into_iarray right);;
[%%expect{|
val erased_split : int iarray -> int iarray @ ghost = <fun>
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

(* The same holds for code built on loans: erased code cannot run the
   borrowing sort. *)
let erased_sort (s : int Slice.t @ local unique) =
  ghost_ (Quicksort.sort s);;
[%%expect{|
Line 2, characters 10-24:
2 |   ghost_ (Quicksort.sort s);;
              ^^^^^^^^^^^^^^
Error: The value "Quicksort.sort" is "partial"
       but is expected to be "total"
         because it is used in an expression (at line 2, characters 2-27).
|}]

(* Calls of a total function with equal arguments give equal results. A
   function that lends a slice cannot be declared total (as [lend] above
   shows), so two of its calls are not known to agree. *)
let length_after_lend (values : int iarray) : int =
  let a = Owned_array.of_iarray values in
  let post = ghost_ (fun (_ : unit) (_ : int Model.t @ immutable) -> true) in
  let result = Owned_array.with_mut a post (fun s -> Slice.finish s) in
  let {state; _} = result in
  Owned_array.length (borrow_ state);;
[%%expect{|
val length_after_lend : int iarray -> int = <fun>
|}]

let lend_twice (values : int iarray) : {b : bool | b} =
  length_after_lend values = length_after_lend values;;
[%%expect{|
Line 2, characters 2-53:
2 |   length_after_lend values = length_after_lend values;;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
Line 1, characters 51-52:
1 | let lend_twice (values : int iarray) : {b : bool | b} =
                                                       ^
  The refinement is stated here.
|}]

(* Control: the same read without a loan is total, and its calls agree. *)
let (length_owned @ total) (values : int iarray) : int =
  let a = Owned_array.of_iarray values in
  Owned_array.length (borrow_ a);;
[%%expect{|
val length_owned : int iarray -> int = <fun>
|}]

let owned_twice (values : int iarray) : {b : bool | b} =
  length_owned values = length_owned values;;
[%%expect{|
val owned_twice : int iarray -> {b : bool | b} = <fun>
|}]
