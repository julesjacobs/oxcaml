(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 prebuilt_modules = "vox_sequence.mli vox_sequence.ml vox_diff_spec.ml vox_diff.mli vox_diff.ml diff_public_client.ml";
 { bytecode; }
*)

open Vox_diff_spec
open Vox_diff

let (equal @ total) (x : int) (y : int) :
    {same : bool | same = (x === y)} = x = y

let bytes s = List.init (String.length s) (fun i -> Char.code s.[i])

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

let check old fresh =
  let result = diff equal old fresh in
  let block = Obj.repr result in
  (match Sys.backend_type with
   | Native -> assert (Obj.size block = 1)
   | Bytecode -> assert (Obj.size block = 2)
   | Other _ -> assert false);
  let script = result.edits in
  assert (source script = old);
  assert (target script = fresh);
  assert (apply equal old script = Some fresh);
  assert (apply equal fresh (invert script) = Some old);
  assert (cost script = Bigint.of_int (distance old fresh));
  assert (invert (invert script) = script);
  assert (Diff_public_client.verified_client ~equal old fresh (ghost_ script) = script);
  script

let rec words n =
  if n = 0 then [[]] else
  let shorter = words (n - 1) in
  [] :: List.concat_map (fun w -> [0 :: w; 1 :: w]) shorter

let () =
  List.iter (fun (a, b) -> ignore (check (bytes a) (bytes b)))
    ["", ""; "", "abc"; "abc", ""; "a", "a"; "a", "b";
     "ABCABBA", "CBABAC"; "aaaa", "aa"; "abab", "baba";
     "abc", "xyz"; "prefix-prefix-a", "prefix-prefix-b"];
  assert (check (bytes "a") (bytes "b") = [Delete 97; Insert 98]);
  let all_bytes = List.init 256 Fun.id in
  ignore (check all_bytes (List.tl all_bytes @ [0]));
  ignore (check [min_int; max_int; 0] [0; min_int; max_int]);
  let inputs = words 5 in
  List.iter (fun a -> List.iter (fun b -> ignore (check a b)) inputs) inputs;
  let state = Random.State.make [|9025|] in
  for _ = 1 to 200 do
    let word () = List.init (Random.State.int state 40)
        (fun _ -> Random.State.int state 5) in
    ignore (check (word ()) (word ()))
  done;
  let prefix = List.init 10000 (fun i -> i mod 256) in
  let script = check (prefix @ [7]) (prefix @ [8]) in
  assert (List.length script = 10002);
  assert (apply equal [1] [] = None);
  assert (apply equal [] [Keep 1] = None);
  assert (apply equal [2] [Keep 1] = None);
  assert (apply equal [2] [Delete 1] = None);
  assert (apply equal [] [Insert 1] = Some [1]);
  let at_limit = List.init 1000000 (fun _ -> 0) in
  let rejected old fresh =
    match diff equal old fresh with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  assert (rejected (1 :: at_limit) []);
  assert (rejected [] (1 :: at_limit));
  let script = (diff equal at_limit at_limit).edits in
  assert (List.length script = 1000000);
  assert (List.for_all (function Keep 0 -> true | _ -> false) script)

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
  assert (source edits = old && target edits = fresh);
  assert (cost edits = 1Z);
  assert (apply equal_token old edits = Some fresh);
  assert (apply equal_token fresh (invert edits) = Some old);
  assert (Diff_public_client.verified_client
    ~equal:equal_token old fresh (ghost_ edits) = edits)
