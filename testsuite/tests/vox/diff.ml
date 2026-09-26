(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_diff_spec.mli vox_diff_spec.ml vox_diff.mli vox_diff.ml diff_public_client.ml diff.ml";
 { bytecode; }
*)

open Vox_diff_spec
open Vox_diff

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
  match diff old fresh with
  | Error _ -> failwith "diff failed"
  | Ok script ->
    assert (source script = old);
    assert (target script = fresh);
    assert (apply old script = Some fresh);
    assert (apply fresh (invert script) = Some old);
    assert (cost script = Bigint.of_int (distance old fresh));
    assert (invert (invert script) = script);
    assert (Diff_public_client.verified_client old fresh script = Ok script);
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
  assert (apply [1] [] = None);
  assert (apply [] [Keep 1] = None);
  assert (apply [2] [Keep 1] = None);
  assert (apply [2] [Delete 1] = None);
  assert (apply [] [Insert 1] = Some [1]);
  let at_limit = List.init 1000000 (fun _ -> 0) in
  assert (diff (1 :: at_limit) [] = Error Input_too_large);
  assert (diff [] (1 :: at_limit) = Error Input_too_large);
  match diff at_limit at_limit with
  | Error _ -> assert false
  | Ok script ->
    assert (List.length script = 1000000);
    assert (List.for_all (function Keep 0 -> true | _ -> false) script)
