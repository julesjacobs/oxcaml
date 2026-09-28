(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_int_sequence.mli borrow.mli quicksort.mli";
 setup-ocamlc.opt-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.opt;
 module = "vox_int_sequence.mli";
 ocamlc.opt;
 module = "borrow.mli";
 ocamlc.opt;
 module = "quicksort.mli";
 ocamlc.opt;
 expect;
*)

#directory "ocamlc.opt";;

let (blocking_sort @ total) (values : int Borrow.Owned_array.t @ unique) =
  let _result = Quicksort.parallel_sort_array ~max_domains:1 values in
  ();;
[%%expect{|
Line 2, characters 16-45:
2 |   let _result = Quicksort.parallel_sort_array ~max_domains:1 values in
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "Quicksort.parallel_sort_array" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 28-4
         which is expected to be "total".
|}]

(* The borrowing [sort] is not total, so a total function cannot call it.
   [sort_array] is total. Only the interfaces are loaded here, so the
   accepted control is checked inside [module type of], which does not run
   it. *)
let (sort_slice @ total) (s : int Borrow.Slice.t @ local unique) =
  Quicksort.sort s;;
[%%expect{|
Line 2, characters 2-16:
2 |   Quicksort.sort s;;
      ^^^^^^^^^^^^^^
Error: The value "Quicksort.sort" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-2, characters 25-18
         which is expected to be "total".
|}]

module type Control = module type of struct
  let (sort_owned @ total) (values : int Borrow.Owned_array.t @ unique) =
    Quicksort.sort_array values
end;;
[%%expect{|
module type Control =
  sig
    val sort_owned :
      int Borrow.Owned_array.t @ unique -> int Borrow.Owned_array.t @@ total
  end
|}]

let ghost_write : (s : int Borrow.Slice.t) @ local unique ->
    (index : {i : int | 0 <= i && Bigint.compare (Bigint.of_int i)
      (Vox_sequence.length (Borrow.Slice.current s)) < 0}) -> unit =
    fun s index ->
  let value = 1 in
  let _erased = ghost_ (Borrow.Slice.set s index value) in
  ();;
[%%expect{|
Line 6, characters 41-42:
6 |   let _erased = ghost_ (Borrow.Slice.set s index value) in
                                             ^
Error: This value is "aliased"
         because it is used in an expression (at line 6, characters 16-55).
       However, the highlighted expression is expected to be "unique".
|}]

(* A sort that leaves its slice unchanged keeps the elements (the
   permutation half of [sort]'s contract is proved) but cannot claim the
   sorted half. *)
let unchanged : (s : int Borrow.Slice.t) @ local unique ->
    {u : unit | Quicksort.Spec.sorted (Borrow.Slice.final s)} =
    fun s ->
  let before = ghost_ (Borrow.Slice.current (borrow_ s)) in
  let after = ghost_ (Borrow.Slice.final (borrow_ s)) in
  Borrow.Slice.finish s;
  ghost_ (Quicksort.Spec.permutation_refl before);
  ghost_ (() : {u : unit | Quicksort.Spec.permutation before after});
  ();;
[%%expect{|
Line 9, characters 2-4:
9 |   ();;
      ^^
Error: Refinement could not be proved (counterexample)
Line 2, characters 16-60:
2 |     {u : unit | Quicksort.Spec.sorted (Borrow.Slice.final s)} =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

(* Overwriting the first element before sorting keeps the result sorted (the
   annotation on [sorted] is proved) but loses the permutation. *)
let overwrite_then_sort : (s : int Borrow.Slice.t) @ local unique ->
    {u : unit | Quicksort.Spec.sorted (Borrow.Slice.final s)
      && Quicksort.Spec.permutation (Borrow.Slice.current s)
        (Borrow.Slice.final s)} =
    fun s ->
  let after = ghost_ (Borrow.Slice.final (borrow_ s)) in
  if Borrow.Slice.length (borrow_ s) > 0 then begin
    let first : {i : int | 0 <= i && Bigint.compare (Bigint.of_int i)
      (Vox_sequence.length (Borrow.Slice.current s)) < 0} = 0 in
    let written = Borrow.Slice.set s first 0 in
    let sorted = Quicksort.sort written in
    ghost_ (() : {u : unit | Quicksort.Spec.sorted after});
    sorted
  end else Quicksort.sort s;;
[%%expect{|
Line 13, characters 4-10:
13 |     sorted
         ^^^^^^
Error: Refinement could not be proved (counterexample)
Lines 3-4, characters 9-30:
3 | .........Quicksort.Spec.permutation (Borrow.Slice.current s)
4 |         (Borrow.Slice.final s)...
  The refinement is stated here.
|}]
