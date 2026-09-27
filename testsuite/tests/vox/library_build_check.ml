(* Checks that library_build.ml compiles exactly the modules of
   verification/library/build.sh, in its order, followed by Vox_traversal.
   Argument: the test source directory. *)

let read file = In_channel.with_open_bin file In_channel.input_all

let words text =
  String.map (function '\n' | '\t' | '\\' -> ' ' | c -> c) text
  |> String.split_on_char ' '
  |> List.filter (( <> ) "")

let after text marker =
  let n = String.length marker in
  let rec find i =
    if i + n > String.length text then failwith ("missing " ^ marker)
    else if String.sub text i n = marker then i + n
    else find (i + 1)
  in
  let start = find 0 in
  String.sub text start (String.length text - start)

let () =
  let dir = Sys.argv.(1) in
  let build =
    read (Filename.concat dir "../../../verification/library/build.sh")
  in
  let listed = after build "modules=(" in
  let expected =
    words (String.sub listed 0 (String.index listed ')')) @ [ "vox_traversal" ]
  in
  (* The modules of the bytecode block's all_modules assignments before
     the output check, in order. *)
  let test = read (Filename.concat dir "library_build.ml") in
  let block = after test "setup-ocamlc.opt-build-env;" in
  let block =
    let stop = after block "check-ocamlc.opt-output" in
    String.sub block 0 (String.length block - String.length stop)
  in
  let compiled =
    String.split_on_char '\n' block
    |> List.concat_map (fun line ->
           match String.trim line with
           | line
             when String.length line > 11
                  && String.sub line 0 11 = "all_modules" ->
             let start = String.index line '"' + 1 in
             words (String.sub line start (String.rindex line '"' - start))
           | _ -> [])
    |> List.map Filename.remove_extension
    |> List.fold_left
         (fun acc m -> match acc with x :: _ when x = m -> acc | _ -> m :: acc)
         []
    |> List.rev
  in
  if compiled = expected then
    Printf.printf "ok: the %d modules of build.sh, and vox_traversal\n"
      (List.length expected - 1)
  else begin
    print_endline
      "FAILED: library_build.ml and build.sh list different modules";
    exit 1
  end
