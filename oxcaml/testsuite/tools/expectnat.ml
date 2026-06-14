(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  James Rayman, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* See expectcommon.mli for usage *)

module Toplevel = struct
  let override_sys_argv = Opttoploop.override_sys_argv
  let initialize_toplevel_env = Opttoploop.initialize_toplevel_env
  let load_file = Opttopdirs.load_file
  let execute_phrase = Opttoploop.execute_phrase
end

let read_anonymous_arg =
  Expectcommon.read_anonymous_arg ~object_extensions:[".cmxs"; ".cmx"; ".cmxa"]

module Options = Oxcaml_args.Make_opttop_options (struct
  include Oxcaml_args.Default.Opttopmain
  let _stdin () = (* disabled *) ()
  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0
  let anonymous s = read_anonymous_arg s
end);;

external llvm_personality_anchor : unit -> unit
  = "caml_expectnat_llvm_personality_anchor"

let () =
  llvm_personality_anchor ();
  Expectcommon.register_assembly_callback :=
    Some Emit.register_expect_asm_callback;
  Expectcommon.register_llvm_ir_callback :=
    Some Llvmize.register_expect_llvm_ir_callback;
  Expectcommon.register_llvm_asm_callback :=
    Some Llvmize.register_expect_llvm_asm_callback;
  Expectcommon.run
    ~read_anonymous_arg
    ~extra_args:Options.list
    ~extra_init:(fun () ->
      Clflags.Opt_flag_handler.set Oxcaml_flags.opt_flag_handler;
      Compenv.set_extra_params (Some Oxcaml_args.Extra_params.read_param);
      Compenv.readenv Format.err_formatter Before_args;
      Clflags.native_code := true)
    ~extra_after_args:(fun () ->
      Compenv.readenv Format.err_formatter Before_link)
    (module Toplevel)
