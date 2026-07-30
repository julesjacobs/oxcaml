(**************************************************************************)
(* *)
(* OCaml *)
(* *)
(* Xavier Leroy, projet Cristal, INRIA Rocquencourt *)
(* *)
(* Copyright 1996 Institut National de Recherche en Informatique et *)
(* en Automatique. *)
(* *)
(* All rights reserved. This file is distributed under the terms of *)
(* the GNU Lesser General Public License version 2.1, with the *)
(* special exception on linking described in the file LICENSE. *)
(* *)
(**************************************************************************)

open Clflags

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

module Options = Oxcaml_args.Make_optcomp_options (Oxcaml_args.Default.Optmain)

let main unix argv ppf ~flambda2 =
  native_code := true;
  Compmisc.set_backtrace_defaults ();
  Compmisc.set_gc_pacing_defaults ~argv;
  let columns =
    match Sys.getenv "COLUMNS" with
    | exception Not_found -> None
    | columns ->
      (try Some (int_of_string columns) with
       | _ -> None)
  in
  (match columns with
   | None -> ()
   | Some columns ->
     (* Avoid getting too close to the edge just in case we've mismeasured the boxes for
        some reason. *)
     let columns = columns - 5 in
     let set_geometry ppf =
       Format.pp_set_margin ppf columns;
       (* Make sure the max indent is at least 3/4 of the total width. Without this,
          output can be unreadable no matter how wide your screen is. Note that
          [Format.pp_set_margin] already messes with the max indent sometimes, so we want
          to check [Format.pp_get_max_indent] rather than make assumptions. *)
       let desired_max_indent = columns * 3 / 4 in
       if Format.pp_get_max_indent ppf () < desired_max_indent
       then Format.pp_set_max_indent ppf desired_max_indent
     in
     set_geometry Format.std_formatter;
     set_geometry Format.err_formatter);
  match
    Compenv.warnings_for_discarded_params := true;
    Compenv.set_extra_params (Some Oxcaml_args.Extra_params.read_param);
    Compenv.readenv ppf Before_args;
    Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
    Clflags.add_arguments
      __LOC__
      [ ( "-depend"
        , Arg.Unit Makedepend.main_from_option
        , "<options> Compute dependencies (use 'ocamlopt -depend -help' for details)" )
      ];
    Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
    Compenv.parse_arguments (ref argv) Compenv.anonymous "ocamlopt";
    Compmisc.read_clflags_from_env ();
    (* Env-var equivalents for the campaign flags; CLI flag wins over env. *)
    Oxcaml_flags.resolve_from_env ();
    (* Set platform-appropriate DWARF fission default when oxcaml-dwarf is enabled *)
    if Config.oxcaml_dwarf
       && !Clflags.dwarf_fission = Clflags.Fission_none
       && Target_system.is_macos ()
    then Clflags.dwarf_fission := Clflags.Fission_dsymutil;
    (* Set up DWARF compression for C compiler invocations *)
    if !Clflags.debug && !Clflags.native_code
    then Clflags.dwarf_c_toolchain_flag := Dwarf_flags.get_dwarf_c_toolchain_flag ();
    if !Oxcaml_flags.gc_timings then Gc_timings.start_collection ();
    if !Clflags.plugin then Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    if !Clflags.requires_metaprogramming
       && not Language_extension.(is_enabled Runtime_metaprogramming)
    then
      Compenv.fatal
        "The -requires-metaprogramming flag is only supported with the runtime \
         metaprogramming extension";
    if !Clflags.uses_metaprogramming
       && not Language_extension.(is_enabled Runtime_metaprogramming)
    then
      Compenv.fatal
        "The -uses-metaprogramming flag is only supported with the runtime \
         metaprogramming extension";
    let (module Compiler : Optcompile.S) = Optcompile.native unix ~flambda2 in
    (* The artifact producers that read only a key/plan take no input files; skip
       deferred-action processing so they do not fatal on "No input files". *)
    if !Oxcaml_flags.compile_startup_stable = None
       && !Oxcaml_flags.plan_emit_startup_stable = None
       && !Oxcaml_flags.plan_emit_startup_volatile = None
    then (
      try
        Compenv.process_deferred_actions
          ( ppf
          , Compiler.implementation
          , Compiler.interface
          , Compiler.ext_flambda_obj
          , Compiler.ext_flambda_lib )
      with
      | Arg.Bad msg ->
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2);
    Compenv.readenv ppf Before_link;
    if !Oxcaml_flags.emit_cmxa_summary
    then (
      (* Standalone mode: project one input .cmxa to its lean summary. Opens the input by
         the path given on the command line, exactly as the linkbench emitter does, so the
         two producers' bytes match. *)
      let target = Compenv.extract_output !output_name in
      (match Compenv.get_objfiles ~with_ocamlparam:false with
       | [ src ] when Filename.check_suffix src Compiler.ext_flambda_lib ->
         Cmxa_summary.emit ~src ~dst:target
       | _ ->
         Compenv.fatal
           "-emit-cmxa-summary requires exactly one .cmxa input and -o <output>");
      Warnings.check_fatal ())
    else if !Oxcaml_flags.plan_emit_startup_stable <> None
    then (
      (* Emit the stable startup object from a plan-stable file; no scan. *)
      Compmisc.init_path ();
      let base = Option.get !Oxcaml_flags.plan_emit_startup_stable in
      let target = Compenv.extract_output !output_name in
      let stable = Link_plan.read_stable (base ^ ".plan-stable") in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmlink.compile_startup_stable_from_plan
          unix
          ~stable
          ~output_name:target
          ~ppf_dump);
      Warnings.check_fatal ())
    else if !Oxcaml_flags.plan_emit_startup_volatile <> None
    then (
      (* Emit the volatile startup object from a plan-volatile file; no scan. *)
      Compmisc.init_path ();
      let base = Option.get !Oxcaml_flags.plan_emit_startup_volatile in
      let target = Compenv.extract_output !output_name in
      let volatile = Link_plan.read_volatile (base ^ ".plan-volatile") in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmlink.compile_startup_volatile_from_plan
          unix
          ~volatile
          ~output_name:target
          ~ppf_dump);
      Warnings.check_fatal ())
    else (
      (if List.length
            (List.filter
               (fun x -> !x)
               [ make_package
               ; make_archive
               ; shared
               ; instantiate
               ; Compenv.stop_early
               ; output_c_object
               ])
          > 1
       then
         let module P = Clflags.Compiler_pass in
         match !stop_after with
         | None ->
           Compenv.fatal
             "Please specify at most one of -pack, -a, -shared, -c, -output-obj, \
              -instantiate"
         | Some
             (( P.Parsing
              | P.Typing
              | P.Lambda
              | P.Middle_end
              | P.Linearization
              | P.Simplify_cfg
              | P.Emit
              | P.Selection
              | P.Register_allocation
              | P.Llvmize ) as p) ->
           assert (P.is_compilation_pass p);
           Printf.ksprintf
             Compenv.fatal
             "Options -i and -stop-after (%s) are  incompatible with -pack, -a, -shared, \
              -output-obj"
             (String.concat
                "|"
                (P.available_pass_names ~filter:(fun _ -> true) ~native:true)));
      if !Oxcaml_flags.compile_startup_stable <> None
      then (
        Compmisc.init_path ();
        let keyfile = Option.get !Oxcaml_flags.compile_startup_stable in
        let target = Compenv.extract_output !output_name in
        Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Compiler.compile_startup_stable ~ppf_dump ~keyfile target);
        Warnings.check_fatal ())
      else if !make_archive
      then (
        Compmisc.init_path ();
        let target = Compenv.extract_output !output_name in
        Compiler.create_archive (Compenv.get_objfiles ~with_ocamlparam:false) target;
        Warnings.check_fatal ())
      else if !make_package
      then (
        Compmisc.init_path ();
        let target = Compenv.extract_output !output_name in
        Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Compiler.package_files
            ~ppf_dump
            (Compmisc.initial_env ())
            (Compenv.get_objfiles ~with_ocamlparam:false)
            target);
        Warnings.check_fatal ())
      else if !instantiate
      then (
        Compmisc.init_path ();
        (* Requiring [-o] isn't really necessary, but we don't intend for humans to be
           invoking [-instantiate] by hand, and computing the correct value here would be
           awkward *)
        let target = Compenv.extract_output !output_name in
        let src, args =
          match Compenv.get_objfiles ~with_ocamlparam:false with
          | [] | [ _ ] ->
            Printf.ksprintf
              Compenv.fatal
              "Must specify at least two %s files with -instantiate"
              Compiler.ext_flambda_obj
          | src :: args -> src, args
        in
        Compiler.instantiate ~src ~args target;
        Warnings.check_fatal ())
      else if !shared
      then (
        Compmisc.init_path ();
        let target = Compenv.extract_output !output_name in
        Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Compiler.link_shared
            ~ppf_dump
            (Linkenv.create ())
            (Compenv.get_objfiles ~with_ocamlparam:false)
            target);
        Warnings.check_fatal ())
      else if (not !Compenv.stop_early)
              && (!objfiles <> [] || Option.is_some !Oxcaml_flags.use_link_plan)
      then (
        let target =
          if !output_c_object
          then (
            let s = Compenv.extract_output !output_name in
            if Filename.check_suffix s Config.ext_obj
               || Filename.check_suffix s Config.ext_dll
            then s
            else
              Compenv.fatal
                (Printf.sprintf
                   "The extension of the output file must be %s or %s"
                   Config.ext_obj
                   Config.ext_dll))
          else Compenv.default_output !output_name
        in
        Compmisc.init_path ();
        Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          (* Latency-shape variant: generate the volatile startup object in-process from
             the plan-volatile and feed it to the consume link as if it were prebuilt,
             instead of a separate [startup_rule `Volatile] dune action. Removes that
             action's process spawn from the incremental relink critical path; same
             codegen, one process. *)
          if !Oxcaml_flags.emit_startup_volatile_inline
          then (
            match !Oxcaml_flags.use_link_plan, !Oxcaml_flags.use_startup_volatile with
            | Some base, None ->
              let tmp = Filename.temp_file "camlstartupv_inline" Config.ext_obj in
              let volatile = Link_plan.read_volatile (base ^ ".plan-volatile") in
              Asmlink.compile_startup_volatile_from_plan
                unix
                ~volatile
                ~output_name:tmp
                ~ppf_dump;
              Oxcaml_flags.use_startup_volatile := Some tmp
            | _ ->
              Compenv.fatal
                "-emit-startup-volatile-inline requires -use-link-plan and is mutually \
                 exclusive with -use-startup-volatile");
          let objs = Compenv.get_objfiles ~with_ocamlparam:true in
          Compiler.link ~ppf_dump objs target);
        Warnings.check_fatal ()))
  with
  | exception Compenv.Exit_with_status n -> n
  | exception x ->
    Location.report_exception ppf x;
    2
  | () ->
    let output_profile_csv ppf_file =
      Profile.output_to_csv
        ppf_file
        !Clflags.profile_columns
        ~timings_precision:!Clflags.timings_precision
    in
    let output_profile_standard ppf =
      if !Oxcaml_flags.gc_timings
      then (
        let minor = Gc_timings.gc_minor_ns () in
        let major = Gc_timings.gc_major_ns () in
        let stats = Gc.quick_stat () in
        let secs x = x *. 1e-9 in
        let precision = !Clflags.timings_precision in
        let w2b n = n * (Sys.word_size / 8) in
        let fw2b x = w2b (Float.to_int x) in
        Format.fprintf ppf "%0.*fs gc\n" precision (secs (minor +. major));
        Format.fprintf ppf "  %0.*fs minor\n" precision (secs minor);
        Format.fprintf ppf "  %0.*fs major\n" precision (secs major);
        Format.fprintf ppf "- heap\n";
        (* Having minor + major + promoted = total alloc make more sense for hierarchical
           stats. *)
        Format.fprintf
          ppf
          "  %ib alloc\n"
          (fw2b stats.minor_words + (fw2b stats.major_words - fw2b stats.promoted_words));
        Format.fprintf
          ppf
          "    %ib minor\n"
          (fw2b stats.minor_words - fw2b stats.promoted_words);
        Format.fprintf
          ppf
          "    %ib major\n"
          (fw2b stats.major_words - fw2b stats.promoted_words);
        Format.fprintf ppf "    %ib promoted\n" (fw2b stats.promoted_words);
        Format.fprintf ppf "  %ib top\n" (w2b stats.top_heap_words);
        Format.fprintf
          ppf
          "  %i collections\n"
          (stats.minor_collections + stats.major_collections);
        Format.fprintf ppf "    %i minor\n" stats.minor_collections;
        Format.fprintf ppf "    %i major\n" stats.major_collections);
      Profile.print
        ppf
        !Clflags.profile_columns
        ~timings_precision:!Clflags.timings_precision
    in
    if !Clflags.dump_into_csv
    then (
      let file_prefix =
        Compmisc.get_profile_file_prefix ~expected_suffix:".csv" ~default_name:"profile"
      in
      Compmisc.with_ppf_file ~file_prefix ~file_extension:".csv" output_profile_csv)
    else if !Oxcaml_flags.gc_timings || !Clflags.profile_columns <> []
    then (
      let file_prefix =
        Compmisc.get_profile_file_prefix ~expected_suffix:".dump" ~default_name:"profile"
      in
      Compmisc.with_ppf_dump ~stdout:() ~file_prefix output_profile_standard);
    0
;;
