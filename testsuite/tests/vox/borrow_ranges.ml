(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml borrow.mli borrow.ml borrow_ranges.ml";
 { bytecode; }
 { native; }
 { flags += " -principal"; bytecode; }
 { flags += " -principal"; native; }
*)

open Borrow

type snapshot : immutable_data = int iarray

let (replace_two @ total) : (values : int list) ->
    {u : unit | if Model.length values === 2Z then
      Model.set (Model.set values 0Z 99) 1Z 88 === [99; 88] else true} = fun values ->
  let zero = 0Z in
  let one = 1Z in
  let first = 99 in
  let second = 88 in
  let intermediate = Model.set values zero first in
  Model.length_def values;
  Model.set_def values zero first;
  Model.set_def intermediate one second;
  match values with
  | [] -> let u = () in refine_ u
  | _ :: tail ->
    Model.length_def tail;
    Model.set_def tail zero second;
    match tail with
    | [] -> let u = () in refine_ u
    | _ :: rest ->
      Model.length_def rest;
      let u = () in refine_ u

let write_pair : (loan : {s : int Slice.t | Model.length (Slice.current s) === 2Z})
    @ local unique ->
    {u : unit | let refine_ s = loan in Slice.final s === [99; 88]} = fun loan ->
  let refine_ s = loan in
  let before = ghost_ (Slice.current (borrow_ s)) in
  let zero = 0 in
  let one = 1 in
  let first = 99 in
  let second = 88 in
  let index : {i : int | 0 <= i
    && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s)) < 0} = refine_ zero in
  let refine_ s1 = Slice.set s index first in
  let bzero = ghost_ 0Z in
  ghost_ (Model.set_length before bzero first);
  let index : {i : int | 0 <= i
    && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s1)) < 0} = refine_ one in
  let refine_ s2 = Slice.set s1 index second in
  ghost_ (replace_two before);
  Slice.finish s2;
  let u = () in refine_ u

let[@def] rewritten (before : int list) =
  if Bigint.compare (Model.length before) 4Z >= 0 then
    Model.set (Model.append (Model.take 1Z before)
      (Model.append [99; 88] (Model.drop 3Z before))) 0Z 7
  else before

let edit : (a : int Owned_array.t) @ unique ->
    {r : (snapshot, int Owned_array.t) step |
      Model.of_iarray r.value === Owned_array.contents a
      && Owned_array.contents r.state === rewritten (Owned_array.contents a)} @ unique = fun a ->
  let before = ghost_ (Owned_array.contents (borrow_ a)) in
  let post = ghost_ (fun (copy : snapshot @ immutable total)
      (after : int Model.t @ immutable) ->
        Model.of_iarray copy === before && after === rewritten before) in
  let refine_ result = Owned_array.with_mut a post (fun loan ->
    let refine_ s = loan in
    let refine_ snapshot = Slice.snapshot s in
    let {value = (copy : snapshot); state = s1} = snapshot in
    let refine_ sized = Slice.length s1 in
    let {value = n; state = s2} = sized in
    let state =
      if n >= 4 then (
        let first = 1 in
        let past = 3 in
        let bfirst = ghost_ 1Z in
        let bpast = ghost_ 3Z in
        ghost_ (Model.sub_length before bfirst bpast);
        let first : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current s2)) <= 0} = refine_ first in
        let past : {j : int | let refine_ i = first in i <= j
          && Bigint.compare (Bigint.of_int j) (Model.length (Slice.current s2)) <= 0} = refine_ past in
        let pair_post = ghost_ (fun (_ : unit @ immutable) (after : int Model.t @ immutable) ->
        after === [99; 88]) in
        let refine_ range = Slice.with_range s2 first past pair_post (fun middle ->
          let refine_ middle = middle in
          let sized : {s : int Slice.t | Model.length (Slice.current s) === 2Z} = refine_ middle in
          let refine_ u = write_pair sized in
          refine_ u) in
        let {value = u; state} = range in


        let zero = 0 in
        let index : {i : int | 0 <= i
          && Bigint.compare (Bigint.of_int i) (Model.length (Slice.current state)) < 0} = refine_ zero in
        let value = 7 in
        let refine_ state = Slice.set state index value in
        state)
      else s2 in
    Slice.finish state;
    ghost_ (rewritten_def before);
    refine_ copy) in
  let {value = copy; state} = result in

  let result = {value = copy; state} in
  refine_ result

let check (values : int list) (expected : int list) =
  let input = Iarray.of_list values in
  let refine_ a = Owned_array.of_iarray input in
  let refine_ result = edit a in
  let {value = snapshot; state} = result in
  let refine_ output = Owned_array.into_iarray state in
  assert (Iarray.to_list snapshot = values);
  assert (Iarray.to_list output = expected);
  assert (Iarray.to_list input = values)

let () =
  check [] [];
  check [1; 2; 3] [1; 2; 3];
  check [0; 1; 2; 3] [7; 99; 88; 3];
  check [0; 1; 2; 3; 4] [7; 99; 88; 3; 4];
  print_endline "verified range updates: preserved frame, later parent write, stable snapshot"
