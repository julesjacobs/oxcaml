open Vox_diff_spec

let (equal @ total) (x : int) (y : int) :
    {same : bool | same = (x === y)} = x = y

let bytes s = List.init (String.length s) (fun i -> Char.code s.[i])

let text values = String.of_seq (List.to_seq (List.map Char.chr values))

let run old fresh =
  let script = (Vox_diff.diff equal (bytes old) (bytes fresh)).edits in
    List.iter
      (function
        | Keep x -> Printf.printf "  %C\n" (Char.chr x)
        | Delete x -> Printf.printf "- %C\n" (Char.chr x)
        | Insert x -> Printf.printf "+ %C\n" (Char.chr x))
      script;
    let edits =
      List.fold_left (fun n -> function Keep _ -> n | _ -> n + 1) 0 script
    in
    Printf.printf "Minimum insertions + deletions: %d\n" edits;
    match Vox_diff.apply equal (bytes old) script with
    | None -> assert false
    | Some patched -> Printf.printf "Patched: %S\n" (text patched)

let () =
  match Array.to_list Sys.argv with
  | [_] -> run "ABCABBA" "CBABAC"
  | [_; old; fresh] -> run old fresh
  | _ ->
    prerr_endline "Usage: diff-demo [OLD NEW]";
    exit 2
