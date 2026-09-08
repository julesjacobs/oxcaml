(* TEST
 has-z3;
 source_directories = "${test_source_directory}/../../../verification/library";
 readonly_files = "vox_sequence.mli vox_int_sequence.mli borrow.mli quicksort.mli";
 setup-ocamlc.byte-build-env;
 flags = "-extension refinement_types -principal";
 module = "vox_sequence.mli";
 ocamlc.byte;
 module = "vox_int_sequence.mli";
 ocamlc.byte;
 module = "borrow.mli";
 ocamlc.byte;
 module = "quicksort.mli";
 ocamlc.byte;
 expect;
*)

#directory "ocamlc.byte";;

let (blocking_sort @ total) (values : int Borrow.Owned_array.t @ unique) =
  let refine_ result = Quicksort.parallel_sort_array ~max_domains:1 values in
  ();;
[%%expect{|
Line 2, characters 23-52:
2 |   let refine_ result = Quicksort.parallel_sort_array ~max_domains:1 values in
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The value "Quicksort.parallel_sort_array" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 1-3, characters 28-4
         which is expected to be "total".
|}]

module Blocking_callback = struct
  let rec forever () = forever ()
  let (invalid @ total) (values : int Borrow.Owned_array.t @ unique) =
    let post = ghost_ (fun (_ : unit) (_ : int Vox_sequence.t @ immutable) -> true) in
    let refine_ result = Borrow.Owned_array.with_mut values post (fun loan ->
      let refine_ s = loan in
      forever ();
      let refine_ closed = Borrow.Slice.finish s in
      let u = () in refine_ u) in
    ()
end;;
[%%expect{|
Line 7, characters 6-13:
7 |       forever ();
          ^^^^^^^
Error: The value "forever" is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 3-10, characters 24-6
         which is expected to be "total".
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
