let parse in_ =
  let rec one quote acc =
    match In_channel.input_char in_ with
    | None -> String.length acc
    | Some '\n' -> String.length acc
    | Some '"' when quote -> one false acc
    | Some '"' -> one true acc
    | Some c -> one quote (acc ^ String.make 1 c)
  in
  one false ""

let with_input_file contents f =
  let filename = Filename.temp_file "llvm-input-channel" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      let oc = open_out_bin filename in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc contents);
      let ic = open_in_bin filename in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic))

let () =
  let contents = String.make 5200 'a' ^ "\n" in
  with_input_file contents (fun ic -> Printf.printf "%d\n" (parse ic))
