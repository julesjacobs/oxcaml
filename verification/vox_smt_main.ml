let () =
  if Sys.getenv_opt "OCAMLRUNPARAM" = None
     && Sys.getenv_opt "CAMLRUNPARAM" = None
  then Gc.set { (Gc.get ()) with space_overhead = 200 };
  if Array.exists (fun arg -> arg = "-version" || arg = "--version") Sys.argv
  then print_endline "Vox SMT 0.1"
  else begin
    let input = ref stdin in
    Array.iteri (fun i arg ->
      if i > 0 && String.length arg > 0 && arg.[0] <> '-'
      then input := open_in_bin arg) Sys.argv;
    let frontend =
      Vox_smt_frontend.create ~require_enabled_retrievals:true () in
    let regular = ref (stdout, false) in
    let diagnostic = ref (stderr, false) in
    let close_channel channel =
      let output, owned = !channel in
      if owned then close_out_noerr output
    in
    let reset_channels () =
      close_channel regular;
      close_channel diagnostic;
      regular := (stdout, false);
      diagnostic := (stderr, false)
    in
    let execute command =
      match command with
      | Vox_smt_sexp.List
          [Vox_smt_sexp.Atom "set-option";
           Vox_smt_sexp.Atom
             ((":diagnostic-output-channel" | ":regular-output-channel")
              as option);
           Vox_smt_sexp.String name] ->
        let output, owned = match name with
          | "stdout" -> stdout, false
          | "stderr" -> stderr, false
          | _ ->
            open_out_gen [Open_wronly; Open_creat; Open_append; Open_binary]
              0o666 name, true
        in
        (match Vox_smt_frontend.execute frontend command with
        | response ->
          let channel = if option = ":regular-output-channel"
            then regular else diagnostic in
          close_channel channel;
          channel := (output, owned);
          response
        | exception exn ->
          if owned then close_out_noerr output;
          raise exn)
      | Vox_smt_sexp.List [Vox_smt_sexp.Atom "reset"] ->
        let response = Vox_smt_frontend.execute frontend command in
        reset_channels ();
        response
      | _ -> Vox_smt_frontend.execute frontend command
    in
    let rec loop () =
      match Vox_smt_sexp.read !input with
      | None -> ()
      | Some command ->
        (try Option.iter (fun response ->
           output_string (fst !regular) response;
           output_char (fst !regular) '\n') (execute command)
        with
        | Vox_smt_frontend.Error message
        | Invalid_argument message
        | Sys_error message ->
          Printf.fprintf (fst !regular) "(error %s)\n"
            (Vox_smt_sexp.to_string (Vox_smt_sexp.String message)));
        flush (fst !regular);
        (match command with
        | Vox_smt_sexp.List [Vox_smt_sexp.Atom "exit"] -> ()
        | _ -> loop ())
    in
    Fun.protect ~finally:reset_channels (fun () ->
      try loop () with Vox_smt_sexp.Error message ->
        Printf.fprintf (fst !regular) "(error %s)\n"
          (Vox_smt_sexp.to_string (Vox_smt_sexp.String message)))
  end
