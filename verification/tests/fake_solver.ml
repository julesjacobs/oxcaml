let () =
  if List.tl (Array.to_list Sys.argv) <> ["-in"; "-smt2"] then exit 2;
  let pid = open_out (Sys.getenv "VOX_FAKE_PID") in
  Printf.fprintf pid "%d\n" (Unix.getpid ());
  close_out pid;
  let mode = Sys.getenv "VOX_FAKE_MODE" in
  prerr_endline "fake solver started";
  let hang () =
    while true do
      Unix.sleepf 60.
    done
  in
  if mode = "startup-hang" then hang ();
  if mode = "early-exit" then exit 0;
  if mode = "stderr-flood"
  then (
    output_string stderr (String.make 100000 'e');
    flush stderr);
  let rec commands () =
    match read_line () with
    | "(check-sat)" ->
      (match mode with
      | "hang" -> hang ()
      | "crash" ->
        prerr_endline "solver crashed";
        exit 17
      | "signal" -> Unix.kill (Unix.getpid ()) Sys.sigkill
      | "interrupt" ->
        Unix.kill (Unix.getppid ()) Sys.sigint;
        hang ()
      | "bad-status" -> print_endline "probably"
      | "stdout-flood" -> print_endline (String.make (5 * 1024 * 1024) 'x')
      | "unknown" | "solver-timeout" -> print_endline "unknown"
      | "sat" | "model-error" | "bad-model" | "decimal-model" | "unparsed-model"
      | "wrong-model-shape" | "deep-model" | "flat-model" ->
        print_endline "sat"
      | "unsat-hang" ->
        print_endline "unsat";
        hang ()
      | "unsat-junk" -> print_endline "unsat\nsat"
      | "duplicate-status" -> print_endline "unsat\nunsat"
      | _ -> print_endline "unsat");
      commands ()
    | "(get-info :reason-unknown)" ->
      print_endline
        (if mode = "solver-timeout"
         then "(:reason-unknown \"timeout\")"
         else "(:reason-unknown \"incomplete\")");
      commands ()
    | "(exit)" -> ()
    | line when String.starts_with ~prefix:"(get-value" line ->
      (match mode with
      | "sat" -> print_endline ("((v0 #b" ^ String.make 63 '1' ^ "))")
      | "decimal-model" -> print_endline "((v0 (_ bv9223372036854775807 63)))"
      | "model-error" -> print_endline "(error \"model unavailable\")"
      | "unparsed-model" -> print_endline "((v0 unexpected))"
      | "bad-model" -> print_endline "((v0"
      | "wrong-model-shape" -> print_endline "(unsat)"
      | "deep-model" -> print_endline (String.make 300 '(' ^ String.make 300 ')')
      | "flat-model" ->
        print_endline
          ("(" ^ String.concat " " (List.init 100000 (fun _ -> "x")) ^ ")")
      | _ -> exit 3);
      commands ()
    | _ -> commands ()
    | exception End_of_file -> ()
  in
  commands ()
