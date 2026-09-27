(* The Vox checker for the browser: the compiler front end (parsing, typing
   with modes, refinements and ghost checks) and the Vox verifier, compiled to
   JavaScript with js_of_ocaml.

   [voxCheck(name, source, lambda)] does what

     ocamlc -extension refinement_types -color never -c NAME

   does up to and including Lambda generation, on SOURCE saved as NAME in a
   working directory of js_of_ocaml's in-memory file system. It returns the
   status (0: accepted, 2: rejected, 3: not checked, see below) and the
   compiler's messages exactly as ocamlc prints them on standard error. With
   [lambda] true it also returns the program after erasure, as
   [ocamlc -dlambda] prints it. Z3 is reached through [globalThis.voxZ3Eval]
   (see vox_smt_solver.ml).

   Under js_of_ocaml, OCaml's [int] and [nativeint] have 32 bits on the host.
   The verifier models the target's 63-bit integers with [Int64] throughout,
   but the typer converts integer literals with the host's [int_of_string].
   A literal whose host value differs from its value on a 64-bit target is
   therefore not checked (status 3) rather than given a verdict native Vox
   would not give. *)

open Js_of_ocaml

module Options = Main_args.Make_bytecomp_options (Main_args.Default.Main)

(* The interfaces shipped with the page: the standard library and the
   verified library. *)
let arguments =
  [| "ocamlc"; "-extension"; "refinement_types"; "-color"; "never"; "-c";
     "-nostdlib"; "-I"; "/static/lib/ocaml"; "-I"; "/static/lib/vox" |]

let work_directory = "/static/work"

let stderr_buffer = Buffer.create 4096

let initialized = ref false

let initialize () =
  if not !initialized
  then begin
    initialized := true;
    Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);
    Sys_js.set_channel_flusher stdout (Buffer.add_string stderr_buffer);
    Vox_verify.install ();
    Clflags.add_arguments __LOC__ Options.list;
    Compenv.parse_arguments (ref arguments)
      (fun name -> failwith ("unexpected argument " ^ name))
      "ocamlc";
    if not (Sys.file_exists work_directory) then Sys.mkdir work_directory 0o755;
    Sys.chdir work_directory
  end

(* A limitation of this build, reported instead of a verdict. *)
exception Unsupported of Location.t * string

let () =
  Location.register_error_of_exn (function
    | Unsupported (loc, message) ->
      Some
        (Location.errorf ~loc
           "%s@.This is a limitation of the browser build, not a Vox \
            verdict. The native compiler checks this program." message)
    | _ -> None)

(* The value of an [int] literal on a 64-bit target, where [int] has 63 bits,
   following [parse_sign_and_base] and [parse_intnat] in runtime/ints.c:
   decimal literals must lie in [[-2^62, 2^62)], literals with a base prefix
   are read as unsigned 63-bit numbers. [None] when the target rejects it. *)
let target_int literal =
  let negative = String.length literal > 0 && literal.[0] = '-' in
  let digits =
    if String.length literal > 0 && (literal.[0] = '-' || literal.[0] = '+')
    then String.sub literal 1 (String.length literal - 1)
    else literal
  in
  let prefixed =
    String.length digits > 1
    && digits.[0] = '0'
    && String.contains "xXoObBuU" digits.[1]
  in
  match Int64.of_string_opt digits with
  | None -> None
  | Some magnitude when prefixed ->
    (* [Int64.of_string] reads prefixed literals as unsigned 64-bit numbers;
       the target reads them as unsigned 63-bit numbers. *)
    if Int64.compare magnitude 0L < 0
    then None
    else
      let value = Int64.shift_right (Int64.shift_left magnitude 1) 1 in
      Some (if negative then Int64.neg value else value)
  | Some magnitude ->
    let value = if negative then Int64.neg magnitude else magnitude in
    if
      Int64.compare value (-0x4000_0000_0000_0000L) >= 0
      && Int64.compare value 0x3fff_ffff_ffff_ffffL <= 0
    then Some value
    else None

let check_literal ~loc = function
  | Parsetree.Pconst_integer (literal, (None | Some 'm'))
  | Pconst_unboxed_integer (literal, 'm') -> (
    let host = Option.map Int64.of_int (int_of_string_opt literal) in
    match target_int literal with
    | Some value when host <> Some value ->
      raise
        (Unsupported
           ( loc,
             Printf.sprintf
               "The integer literal %s does not fit in the 32-bit int of \
                this JavaScript build."
               literal ))
    | _ -> ())
  | Pconst_integer (literal, Some 'n') | Pconst_unboxed_integer (literal, 'n')
    -> (
    (* A 64-bit target's nativeint has the semantics of [Int64]. *)
    let host = Option.map Int64.of_nativeint (Nativeint.of_string_opt literal) in
    match Int64.of_string_opt literal with
    | Some value when host <> Some value ->
      raise
        (Unsupported
           ( loc,
             Printf.sprintf
               "The nativeint literal %sn does not fit in the 32-bit \
                nativeint of this JavaScript build."
               literal ))
    | _ -> ())
  | _ -> ()

let check_literals structure =
  let open Ast_iterator in
  let constant (c : Parsetree.constant) =
    check_literal ~loc:c.pconst_loc c.pconst_desc
  in
  let iterator =
    { default_iterator with
      expr =
        (fun self e ->
          (match e.pexp_desc with Pexp_constant c -> constant c | _ -> ());
          default_iterator.expr self e);
      pat =
        (fun self p ->
          (match p.ppat_desc with
          | Ppat_constant c -> constant c
          | Ppat_interval (a, b) ->
            constant a;
            constant b
          | _ -> ());
          default_iterator.pat self p)
    }
  in
  iterator.structure iterator structure

(* The program after erasure, as [ocamlc -dlambda] prints it. *)
let lambda_of info (typed : Typedtree.implementation) =
  let loc =
    Location.in_file
      (Unit_info.original_source_file info.Compile_common.target)
  in
  let program =
    Translmod.transl_implementation ~loc info.module_name
      (typed.structure, typed.coercion, None)
  in
  Builtin_attributes.warn_unused ();
  let _static_data, lambda =
    Slambda.eval ~cu_static_data:(fun _ -> None) Fun.id program.Lambda.code
  in
  let lambda = Simplif.simplify_lambda_for_bytecode lambda in
  Format.asprintf "%a@." Printlambda.lambda lambda

let compile ~source_file =
  let output_prefix = Filename.remove_extension source_file in
  let unit_info =
    Compile_common.unit_info_from_cu_or_output_prefix ~source_file Impl
      ~output_prefix ~compilation_unit:Inferred_from_output_prefix
  in
  let lambda = ref None in
  Compile_common.with_info ~backend:Byte ~tool_name:"ocamlc" ~dump_ext:"cmo"
    unit_info
  @@ fun info ->
  Compile_common.implementation ~hook_parse_tree:check_literals
    ~hook_typed_tree:(fun _ -> ())
    info
    ~backend:(fun info typed -> lambda := Some (lambda_of info typed));
  !lambda

let reset () =
  Buffer.clear stderr_buffer;
  (* Interfaces read from disk are kept: they do not change between checks. *)
  Env.reset_cache ~preserve_persistent_env:true;
  Warnings.reset_fatal ();
  Location.reset ()

let check name source want_lambda =
  initialize ();
  reset ();
  let source_file = Filename.basename name in
  Out_channel.with_open_bin source_file (fun channel ->
      output_string channel source);
  let status, lambda =
    match compile ~source_file with
    | lambda -> 0, lambda
    | exception (Unsupported _ as exn) ->
      Location.report_exception Format.err_formatter exn;
      3, None
    | exception Stack_overflow ->
      Format.eprintf
        "The checker ran out of JavaScript stack. This is a limitation of \
         the browser build, not a Vox verdict.@.";
      3, None
    | exception exn ->
      Location.report_exception Format.err_formatter exn;
      2, None
  in
  Format.pp_print_flush Format.err_formatter ();
  Format.pp_print_flush Format.std_formatter ();
  flush stderr;
  flush stdout;
  let output = Buffer.contents stderr_buffer in
  Buffer.clear stderr_buffer;
  let lambda = if want_lambda then lambda else None in
  Js.Unsafe.obj
    [| "status", Js.Unsafe.inject status;
       "output", Js.Unsafe.inject (Js.string output);
       ( "lambda",
         match lambda with
         | Some text -> Js.Unsafe.inject (Js.string text)
         | None -> Js.Unsafe.inject Js.null ) |]

let () =
  Js.export "voxCheck"
    (Js.wrap_callback (fun name source want_lambda ->
         check (Js.to_string name) (Js.to_string source)
           (Js.to_bool want_lambda)))
