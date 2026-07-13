(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let auto_include find_in_dir fn =
  if !Clflags.no_auto_include_otherlibs || !Clflags.no_std_include then
    raise Not_found
  else
    let alert = Location.auto_include_alert in
    Load_path.auto_include_otherlibs alert find_in_dir fn

(* Initialize the search path.
   [dir] (default: the current directory)
   is always searched first  unless -nocwd is specified,
   then the directories specified with the -I option (in command line order),
   then the standard library directory (unless the -nostdlib option is given),
   then the directories specified with the -H option (in command line order).
 *)

let init_path ?(auto_include=auto_include) ?(dir="") () =
  let visible =
    if !Clflags.use_threads then
      { Clflags.path = "+threads"; cmx_guaranteed = true }
      :: !Clflags.include_dirs
    else
      !Clflags.include_dirs
  in
  let visible =
    List.concat
      [!Compenv.last_include_dirs;
       visible;
       List.map
         (fun path : Clflags.visible_include ->
            { path; cmx_guaranteed = false })
         (* Config.flexdll_dirs is either [] or ["+flexdll"]: don't include a
            reference to the Standard Library when -nostdlib was specified. *)
         (if !Clflags.no_std_include then [] else Config.flexdll_dirs);
       !Compenv.first_include_dirs]
  in
  let visible =
    List.map (fun (e : Clflags.visible_include) : Clflags.visible_include ->
      { path = Misc.expand_directory Config.standard_library e.path;
        cmx_guaranteed = e.cmx_guaranteed })
      visible
  in
  let visible =
    (if !Clflags.no_cwd then []
     else [{ Clflags.path = dir; cmx_guaranteed = false }])
    @ List.rev_append visible
        (List.map
           (fun path -> { Clflags.path; cmx_guaranteed = true })
           (Clflags.std_include_dir ()))
  in
  let hidden =
    List.rev_map (Misc.expand_directory Config.standard_library)
      !Clflags.hidden_include_dirs
  in
  Load_path.init ~auto_include ~visible ~hidden;
  Env.reset_cache ~preserve_persistent_env:false

(* Read any [-parameters] flags. Important to do this before [initial_env ()]
   because someone might [-open] a parameterised module *)
let init_parameters () =
  let param_names = !Clflags.parameters in
  List.iter
    (fun param_name ->
        let param = Global_module.Parameter_name.of_string param_name in
        Env.register_parameter param
    )
    param_names

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#8227) *)

let initial_env () =
  Ident.reinit();
  Types.Uid.reinit();
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~initially_opened_module
    ~open_implicit_args:(List.rev !Clflags.open_args)

let set_from_env flag Clflags.{ parse; usage; env_var } =
  try
    match parse (Sys.getenv env_var) with
    | None ->
        Location.prerr_warning Location.none
          (Warnings.Bad_env_variable (env_var, usage))
    | Some x -> match !flag with
      | None -> flag := Some x
      | Some _ -> ()
  with
    Not_found -> ()

let read_clflags_from_env () =
  set_from_env Clflags.color Clflags.color_reader;
  let no_color () = (* See https://no-color.org/ *)
    match Sys.getenv_opt "NO_COLOR" with
    | None | Some "" -> false
    | _ -> true
  in
  if Option.is_none !Clflags.color && no_color () then
    Clflags.color := Some Misc.Color.Never;
  set_from_env Clflags.error_style Clflags.error_style_reader;
  ()

(* The compiler is allocation-heavy and short-lived, so it benefits from a
   laxer GC pace than the runtime default (space_overhead = 80,
   [Percent_free_def] in runtime/caml/config.h): raising it
   trades peak heap for fewer major collections. On large compilation units
   this is about -8 to -10% wall-clock time at the cost of roughly +45% peak
   heap. We only bump the default: if OCAMLRUNPARAM/CAMLRUNPARAM explicitly
   sets space_overhead ("o=..."), the user/build-farm asked for a specific
   value (possibly the default 120) and we leave it untouched. *)
(* Size-adaptive GC pace. Small/medium units get a lax pace (fewer major
   collections; the -8..-10% single-unit win). Large units (big generated or
   heavy modules) keep a tighter pace so we do not enlarge their already-big
   heaps: on the fleet a global lax pace regressed heavy compiles (>=5s) ~10%
   via aggregate memory pressure / paging, even while the tree aggregate
   improved. The size threshold is a best-effort proxy (heavy files are
   predominantly large sources) and wants a fleet run to calibrate. *)
let compiler_space_overhead_small = 800
let compiler_space_overhead_large = 400
let large_unit_source_bytes = 300 * 1024

(* Mirror the runtime's OCAMLRUNPARAM parser (runtime/startup_aux.c): the
   active variable is OCAMLRUNPARAM if it is set at all, otherwise
   CAMLRUNPARAM; within it, fields are separated by commas and each field's
   first character is its single-letter key. Lowercase 'o' is space_overhead
   (uppercase 'O' is a different knob, max_overhead, which we ignore here). *)
let space_overhead_set_in_env () =
  let param =
    match Sys.getenv_opt "OCAMLRUNPARAM" with
    | Some _ as p -> p
    | None -> Sys.getenv_opt "CAMLRUNPARAM"
  in
  match param with
  | None -> false
  | Some s ->
    String.split_on_char ',' s
    |> List.exists (fun field -> String.length field > 0 && field.[0] = 'o')

(* Best-effort size of the compilation unit's source. This runs before Arg
   parsing, so it walks the raw argv (the argv the driver was invoked with,
   which for embedded uses of [Maindriver.main]/[Optmaindriver.main] may
   differ from [Sys.argv]): .ml/.mli arguments count directly; the operands
   of -impl/-intf are sources regardless of suffix; -args/-args0 response
   files are expanded (newline- resp. NUL-separated, trailing CR trimmed, as
   in [Arg.read_arg]/[read_arg0]) and scanned with the same option-aware
   walk, to a bounded nesting depth; and the operands of common value-taking
   flags are skipped so an output name like [-o gen.ml] is not
   misclassified. Exotic forms (a custom -intf-suffix, .mlt) may still be
   missed: this is a pacing heuristic, and an explicit o= in OCAMLRUNPARAM
   stays authoritative. Uses in_channel_length to avoid a Unix
   dependency. *)
let source_file_size_bytes path =
  match open_in_bin path with
  | exception _ -> 0
  | ic ->
    let n = try in_channel_length ic with _ -> 0 in
    close_in_noerr ic;
    n

let response_file_args path ~null_sep =
  match open_in_bin path with
  | exception _ -> []
  | ic ->
    let n = try in_channel_length ic with _ -> 0 in
    if n <= 0 || n > 16 * 1024 * 1024
    then (close_in_noerr ic; [])
    else begin
      match really_input_string ic n with
      | exception _ -> close_in_noerr ic; []
      | s ->
        close_in_noerr ic;
        String.split_on_char (if null_sep then '\000' else '\n') s
        |> List.map (fun a ->
             (* [Arg.read_arg] strips one trailing CR per line, so
                CRLF-formatted -args files parse the same way here. *)
             let n = String.length a in
             if (not null_sep) && n > 0 && a.[n - 1] = '\r'
             then String.sub a 0 (n - 1)
             else a)
        |> List.filter (fun a -> String.length a > 0)
    end

let largest_input_source_bytes argv =
  let best = ref 0 in
  let consider path = best := Stdlib.max !best (source_file_size_bytes path) in
  let is_source a =
    Filename.check_suffix a ".ml" || Filename.check_suffix a ".mli"
  in
  (* Only flags whose operand is a non-source value; keep in sync with
     main_args.ml (e.g. -dump-into-file is Arg.Unit and must NOT be here,
     or it would swallow a following source file). *)
  let takes_value = function
    | "-o" | "-I" | "-H" | "-open" | "-pp" | "-ppx" | "-intf-suffix"
    | "-cmi-file" | "-runtime-variant" | "-use-runtime" ->
      true
    | _ -> false
  in
  (* [depth] bounds response-file nesting (and cuts self-referencing
     -args files short); the real expansion in [Arg] is recursive too. *)
  let rec scan depth = function
    | [] -> ()
    | ("-impl" | "-intf") :: path :: rest ->
      consider path;
      scan depth rest
    | (("-args" | "-args0") as flag) :: path :: rest ->
      (if depth > 0 then
         scan (depth - 1)
           (response_file_args path ~null_sep:(String.equal flag "-args0")));
      scan depth rest
    | flag :: _ :: rest when takes_value flag -> scan depth rest
    | a :: rest ->
      if is_source a then consider a;
      scan depth rest
  in
  (match Array.to_list argv with [] -> () | _ :: args -> scan 2 args);
  !best

let chosen_space_overhead argv =
  if largest_input_source_bytes argv > large_unit_source_bytes
  then compiler_space_overhead_large
  else compiler_space_overhead_small

let set_gc_pacing_defaults ~argv =
  if not (space_overhead_set_in_env ()) then
    Gc.set { (Gc.get ()) with Gc.space_overhead = chosen_space_overhead argv }

let directory_exists dir =
  Sys.file_exists dir && Sys.is_directory dir

let rec make_directory dir =
  if directory_exists dir then () else
    begin
      make_directory (Filename.dirname dir);
      (try
        Sys.mkdir dir 0o777
      with (Sys_error _) as se ->
        if not (directory_exists dir) then raise se)
    end

let with_ppf_file ~file_prefix ~file_extension f =
  let with_ch ch =
    let ppf = Format.formatter_of_out_channel ch in
    ppf,
    (fun () ->
       Format.pp_print_flush ppf ();
       close_out ch)
  in
  let ppf_dump, finally =
    match !Clflags.dump_dir with
    | None -> with_ch (open_out (file_prefix ^ file_extension))
    | Some d ->
      let () = make_directory Filename.(dirname @@ concat d @@ file_prefix) in
      let _, ch =
        Filename.open_temp_file ~temp_dir:d (file_prefix ^ ".") file_extension
      in
      with_ch ch
  in
  Misc.try_finally (fun () -> f ppf_dump) ~always:finally

let with_ppf_dump ?stdout ~file_prefix f =
  match !Clflags.dump_into_file with
  | false ->
    let formatter =
      if Option.is_some stdout then Format.std_formatter else Format.err_formatter in
    Misc.try_finally (fun () -> f formatter)
  | true -> with_ppf_file ~file_prefix ~file_extension:".dump" f

let get_profile_file_prefix ~expected_suffix ~default_name =
  match !Clflags.profile_output_name with
  | None -> default_name
  | Some filename ->
    if Filename.check_suffix filename expected_suffix then
      Filename.chop_suffix filename expected_suffix
    else
      Compenv.fatal
        (Printf.sprintf
          "Profile output filename must have %s extension, got: %s"
          expected_suffix filename)
