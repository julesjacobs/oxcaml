(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_diff_spec.ml vox_diff.mli vox_diff.ml vox_compact_diff_spec.ml vox_compact_diff.mli vox_compact_diff.ml";
 { bytecode; }
 { native; }
*)

open Vox_compact_diff_spec
open Vox_compact_diff

let (equal @ total) (x : int) (y : int) :
    {same : bool | same = (x === y)} = x = y

let verified_client : type (a : logical_data).
    a equality @ total -> (old : a list) -> (fresh : a list) ->
    (other : a diff) @ ghost ->
    {edits : a diff | relates edits old fresh
      && (if relates other old fresh then cost edits <= cost other else true)} =
  fun equal old fresh other ->
  let result = diff equal old fresh in
  ghost_ (result.optimality other);
  ghost_ (
    relates_def result.edits old fresh;
    let forward = apply equal old result.edits in
    let inverse = invert result.edits in
    invert_correct result.edits old fresh;
    relates_def inverse fresh old;
    let backward = apply equal fresh inverse in
    (() : {u : unit | forward === Some fresh && backward === Some old}));
  result.edits

let distance old fresh =
  let a = Array.of_list old and b = Array.of_list fresh in
  let n = Array.length a and m = Array.length b in
  let row = Array.init (m + 1) Fun.id in
  for i = 1 to n do
    let diagonal = ref row.(0) in
    row.(0) <- i;
    for j = 1 to m do
      let above = row.(j) in
      row.(j) <- if a.(i - 1) = b.(j - 1) then !diagonal
        else 1 + min above row.(j - 1);
      diagonal := above
    done
  done;
  row.(m)


let runtime_cost edits =
  List.fold_left (fun n -> function Keep _ -> n | Delete _ | Insert _ -> n + 1)
    0 edits

let rec normalized = function
  | Keep n :: rest ->
    n > 0 && (match rest with Keep _ :: _ -> false | _ -> normalized rest)
  | _ :: rest -> normalized rest
  | [] -> true

let check old fresh =
  let result = diff equal old fresh in
  let block = Obj.repr result in
  (match Sys.backend_type with
   | Native -> assert (Obj.size block = 1)
   | Bytecode ->
     assert (Obj.size block = 4);
     List.iter (fun i ->
       let field = Obj.field block i in
       assert (Obj.is_int field || Obj.size field = 0)) [0; 1; 3]
   | Other _ -> assert false);
  let edits = result.edits in
  assert (normalized edits);
  assert (apply equal old edits = Some fresh);
  assert (apply equal fresh (invert edits) = Some old);
  assert (invert (invert edits) = edits);
  assert (runtime_cost edits = distance old fresh);
  assert (runtime_cost (invert edits) = runtime_cost edits);
  assert (verified_client equal old fresh (ghost_ edits) = edits);
  edits

let rec words n =
  if n = 0 then [[]] else
  let shorter = words (n - 1) in
  [] :: List.concat_map (fun w -> [0 :: w; 1 :: w]) shorter

let () =
  let inputs = words 5 in
  List.iter (fun old -> List.iter (fun fresh -> ignore (check old fresh)) inputs)
    inputs;
  let state = Random.State.make [|9025|] in
  for _ = 1 to 200 do
    let word () = List.init (Random.State.int state 40)
        (fun _ -> Random.State.int state 5) in
    ignore (check (word ()) (word ()))
  done;
  ignore (check [min_int; max_int; 0] [0; min_int; max_int]);
  assert (check [1; 2; 3] [1; 4; 3] =
    [Keep 1; Delete 2; Insert 4; Keep 1]);
  assert (apply equal [] [] = Some []);
  assert (apply equal [1] [] = None);
  assert (apply equal [] [Keep 0] = Some []);
  assert (apply equal [7; 8] [Keep 0; Keep 2; Keep 0] = Some [7; 8]);
  assert (apply equal [7; 8] [Keep 1] = None);
  List.iter (fun n -> assert (apply equal [1] [Keep n] = None))
    [-1; min_int; 2; max_int];
  assert (apply equal [] [Delete 1] = None);
  assert (apply equal [2] [Delete 1] = None);
  assert (apply equal [] [Insert 1] = Some [1]);
  assert (apply equal [2] [Delete 2; Insert 3] = Some [3]);
  assert (apply equal [9; 10] [Keep 2] = Some [9; 10]);
  assert (apply equal [1; 1] [Delete 1; Keep 1; Insert 1] = Some [1; 1]);
  let prefix = List.init 10000 Fun.id in
  assert ((diff equal (prefix @ [-1]) (prefix @ [-2])).edits =
    [Keep 10000; Delete (-1); Insert (-2)]);
  let at_limit = List.init 1000000 (fun _ -> 0) in
  let rejected old fresh =
    match diff equal old fresh with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  assert (rejected (1 :: at_limit) []);
  assert (rejected [] (1 :: at_limit));
  assert ((diff equal at_limit at_limit).edits = [Keep 1000000]);
  assert (apply equal at_limit [Keep 1000000] = Some at_limit)

type token = Name of int | Separator [@@inductive]

let (equal_token @ total) (a : token) (b : token) :
    {same : bool | same = (a === b)} =
  match a, b with
  | Name x, Name y -> x = y
  | Separator, Separator -> true
  | _ -> false

let () =
  let old = [Name 1; Separator; Name 2] in
  let fresh = [Name 1; Name 3; Separator; Name 2] in
  let edits = (diff equal_token old fresh).edits in
  assert (edits = [Keep 1; Insert (Name 3); Keep 2]);
  assert (apply equal_token old edits = Some fresh);
  assert (apply equal_token fresh (invert edits) = Some old);
  assert (verified_client equal_token old fresh (ghost_ edits) = edits)
