(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "borrow_model.mli borrow_model.ml borrow.mli borrow.ml borrow_demo.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Borrow

let swap_ends : ('a : immutable_data). (a : 'a Owned_array.t) @ unique ->
    {r : 'a Owned_array.t |
      Owned_array.contents r ===
        (if Bigint.compare (Model.length (Owned_array.contents a)) 0Z > 0 then
          Model.swap (Owned_array.contents a) 0Z
            (Bigint.sub (Model.length (Owned_array.contents a)) 1Z)
        else Owned_array.contents a)} @ unique = fun a ->
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let post = ghost_ (fun (_ : unit @ immutable)
      (after : 'a Model.t @ immutable) -> after ===
      (if Bigint.compare (Model.length before) 0Z > 0 then
        Model.swap before 0Z (Bigint.sub (Model.length before) 1Z)
       else before)) in
  let refine_ result = Owned_array.with_mut a post (fun loan ->
    let refine_ s = loan in
    let refine_ sized = Slice.length s in
    let {value = n; state = s1} = sized in
    let s2 =
      if n > 0 then
        let zero = 0 in
        let last = n - 1 in
        let first : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s1)) < 0} =
          refine_ zero in
        let second : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s1)) < 0} =
          refine_ last in
        let refine_ swapped = Slice.swap s1 first second in
        swapped
      else s1 in
    Slice.finish s2;
    let u = () in
    refine_ u) in
  let {state; _} = result in
  refine_ state

let check (values : int list) (expected : int list) =
  let source = Iarray.of_list values in
  let refine_ a = Owned_array.of_iarray source in
  let refine_ a = swap_ends a in
  let refine_ result = Owned_array.into_iarray a in
  assert (Iarray.to_list result = expected);
  assert (Iarray.to_list source = values)

let () =
  check [] [];
  check [1] [1];
  check [1; 2; 3; 4] [4; 2; 3; 1];
  print_endline "verified end swap: empty, singleton, and shared source"


type items : immutable_data = int list

let () =
  let shared : items = [1; 2] in
  let input : items iarray = [: shared; [3] :] in
  let refine_ a = Owned_array.of_iarray input in
  let refine_ a = swap_ends a in
  let refine_ output = Owned_array.into_iarray a in
  assert (Iarray.to_list output = [[3]; [1; 2]]);
  assert (Iarray.to_list input = [[1; 2]; [3]]);
  assert (shared = [1; 2])
