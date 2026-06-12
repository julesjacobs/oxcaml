(******************************************************************************
 *                                  OxCaml                                    *
 *                       Yusuf Onur Üşümez, Jane Street                       *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

module String = Misc.Stdlib.String

module List = struct
  include List
  include Misc.Stdlib.List
end

module CL = Cfg_with_layout
module DLL = Oxcaml_utils.Doubly_linked_list
module LL = Llvm_ir
module V = LL.Value
module T = LL.Type
module E = LL.Function.Emitter
module I = LL.Instruction
module F = LL.Format

type error = Asm_generation of (string * int)

exception Error of error

let fail name = Misc.fatal_errorf "Llvmize.%s" name

let fail_msg ?name fmt =
  let name = match name with Some name -> "." ^ name | None -> "" in
  Format.kasprintf (fun msg -> Misc.fatal_errorf "Llvmize%s: %s" name msg) fmt

let fail_if_not ?msg name cond =
  if not cond
  then match msg with None -> fail name | Some msg -> fail_msg ~name "%s" msg

let rec llvm_intrinsic_type_suffix (typ : T.t) =
  match typ with
  | Int { width_in_bits } -> F.sprintf "i%d" width_in_bits
  | Float -> "f32"
  | Double -> "f64"
  | Vector { num_of_elems; elem_type } ->
    F.sprintf "v%d%s" num_of_elems (llvm_intrinsic_type_suffix elem_type)
  | Ptr _ | Struct _ | Array _ | Label | Token | Metadata ->
    fail_msg ~name:"llvm_intrinsic_type_suffix" "unexpected type %a" T.pp_t typ

let not_implemented_aux pp_instr ?msg i =
  fail_msg "unimplemented instruction: %a %a" pp_instr i
    (Format.pp_print_option
       ~none:(fun ppf () -> Format.fprintf ppf "(no msg)")
       (fun ppf msg -> Format.fprintf ppf "(%s)" msg))
    msg

let not_implemented_basic = not_implemented_aux Cfg.print_basic

let not_implemented_terminator = not_implemented_aux Cfg.print_terminator

type c_call_wrapper =
  { args : LL.Type.t list;
    res : LL.Type.t list;
    c_fun_name : string
  }

type trap_block_info =
  { trap_block : LL.Value.t;
    stacksave_ptr : LL.Value.t option;
    exn_bucket : LL.Value.t;
    exn_entry : LL.Value.t;
    recover_rbp_asm_ident : LL.Ident.t;
    recover_rbp_var_ident : LL.Ident.t
  }

type active_trap =
  { pushtrap_id : InstructionId.t;
    trap_handler : Label.t
  }

type slow_path_root_slot =
  { reg : Reg.t;
    root_slot : LL.Value.t
  }

(* CR yusumez: Refactor into its own sub-module *)
type fun_info =
  { emitter : LL.Function.Emitter.t;
        (* Emitter responsible for producing LLVM IR of a function *)
    reg2alloca : LL.Value.t Reg.Tbl.t;
        (* Map [Reg.t]'s from OCaml to alloca'd identifiers in LLVM IR *)
    const_ints : nativeint Reg.Tbl.t;
        (* Best-effort constants for registers defined by [Const_int]. This is
           used to avoid reloading raw header words from temporary allocas. *)
    mutable liveness : Cfg_liveness.Liveness.domain InstructionId.Tbl.t option;
        (* Per-instruction liveness for exposing live OCaml roots across
           safepoints. *)
    mutable fun_args : Reg.t array;
        (* Original CFG function arguments. Self-tailcalls branch back to the
           function entry in LLVM, so their new arguments must be copied into
           these slots before the branch. *)
    mutable preserved_reg_slots : Reg.Set.t;
        (* Registers whose slots must remain addressable, for example because a
           GC safepoint reports the slot through a ["gc-live"] bundle. *)
    mutable addr_regs_known_base : Reg.Set.t;
        (* Address-typed temporaries that are immediately reinterpreted as [Val].
           This is an explicit frontend assertion that the address is an OCaml
           object pointer, not an interior pointer. *)
    mutable static_addr_regs : Reg.Set.t;
        (* Integer/address temporaries derived from static symbols. These are
           address values, but not moving GC roots. *)
    mutable slow_path_root_slots : LL.Value.t list;
        (* Static entry-block spill slots used only by eligible allocation and
           poll slow paths. *)
    mutable active_traps : active_trap option InstructionId.Tbl.t;
        (* Top active trap region at each instruction. [Cfg.basic_block.exn] is
           only populated for blocks whose terminator can raise, but LLVM also
           needs this for basic-instruction safepoints such as allocation and
           polling. *)
    mutable active_trap_depths : int InstructionId.Tbl.t;
        (* Number of active trap regions at each instruction. *)
    trap_handler_exn_buckets : LL.Value.t Label.Tbl.t;
        (* Per-handler entry-block slots used on AArch64 when multiple static
           pushtrap regions can branch to the same handler block. *)
    aarch64_trap_blocks : trap_block_info InstructionId.Tbl.t;
        (* AArch64 trap recovery entries keyed by the static [Pushtrap]
           instruction id. The handler label alone is not a unique trap-region
           identity. *)
    trap_blocks : trap_block_info Label.Tbl.t;
        (* Identifiers created during a [Pushtrap] instruction needed for
           [Poptrap] and trap handler entry on non-AArch64 targets. *)
    mutable current_dbg_metadata : string option;
    subprogram_dbg_metadata_id : int option
  }

type t =
  { llvmir_filename : string;
    oc : Out_channel.t;
    ppf : Format.formatter;
    ppf_dump : Format.formatter;
    mutable sourcefile : string option; (* gets set in [begin_assembly] *)
    mutable asm_filename : string option; (* gets set in [open_out] *)
    mutable current_fun_info : fun_info option;
        (* Maintains the state of the current function (reset for every
           function) *)
    mutable function_defs : LL.Function.t list;
    mutable data_defs : LL.Data.t list;
    mutable defined_symbols : String.Set.t; (* Global symbols defined so far *)
    mutable referenced_symbols : String.Set.t;
        (* Global symbols referenced so far *)
    mutable function_decls : LL.Fundecl.t String.Map.t;
        (* Declarations for non-intrinsic functions that are referenced in LLVM
           IR-level constructs rather than ordinary calls. *)
    mutable function_arg_types : T.t list String.Map.t;
        (* Direct-call argument signatures for functions defined in the current
           compilation unit, excluding the threaded runtime registers. *)
    mutable called_intrinsics : LL.Fundecl.t String.Map.t;
        (* Names + signatures (args, ret) of LLVM intrinsics called so far. Note
           that external functions are treated as symbols and are not declared
           to avoid type mismatches, but intrinsics must be declared with their
           signatures. *)
    mutable c_call_wrappers : c_call_wrapper String.Map.t;
        (* Wrappers for noalloc C calls. This is currently needed since
           manipulating the stack inline is broken. *)
    mutable all_trap_blocks : trap_block_info list;
    mutable module_asm : string list;
    mutable next_debug_metadata_id : int;
    mutable debug_compile_unit_id : int option;
    mutable debug_file_id : int option;
    mutable debug_subroutine_type_id : int option;
    mutable debug_metadata_defs_rev : string list;
    mutable debug_module_flag_ids_rev : int list
  }

(* current_fun_info interface *)

let create_fun_info ?subprogram_dbg_metadata_id emitter =
  { emitter;
    reg2alloca = Reg.Tbl.create 0;
    const_ints = Reg.Tbl.create 0;
    liveness = None;
    fun_args = [||];
    preserved_reg_slots = Reg.Set.empty;
    addr_regs_known_base = Reg.Set.empty;
    static_addr_regs = Reg.Set.empty;
    slow_path_root_slots = [];
    active_traps = InstructionId.Tbl.create 0;
    active_trap_depths = InstructionId.Tbl.create 0;
    trap_handler_exn_buckets = Label.Tbl.create 0;
    aarch64_trap_blocks = InstructionId.Tbl.create 0;
    trap_blocks = Label.Tbl.create 0;
    current_dbg_metadata = None;
    subprogram_dbg_metadata_id
  }

let get_fun_info t =
  match t.current_fun_info with
  | Some fun_info -> fun_info
  | None -> fail_msg ~name:"get_fun_info" "not_available"

let reset_fun_info ?subprogram_dbg_metadata_id t emitter =
  t.current_fun_info
    <- Some (create_fun_info ?subprogram_dbg_metadata_id emitter)

let get_alloca_for_reg t reg =
  let fun_info = get_fun_info t in
  match Reg.Tbl.find_opt fun_info.reg2alloca reg with
  | Some v -> v
  | None -> fail_msg ~name:"get_alloca_for_reg" "reg not found"

let get_alloca_for_reg_opt t reg =
  let fun_info = get_fun_info t in
  Reg.Tbl.find_opt fun_info.reg2alloca reg

let set_alloca_for_reg t reg alloca =
  let fun_info = get_fun_info t in
  Reg.Tbl.add fun_info.reg2alloca reg alloca

let clear_const_int_for_reg t reg =
  let fun_info = get_fun_info t in
  Reg.Tbl.remove fun_info.const_ints reg

let set_const_int_for_reg t reg n =
  let fun_info = get_fun_info t in
  Reg.Tbl.replace fun_info.const_ints reg n

let get_const_int_for_reg t reg =
  let fun_info = get_fun_info t in
  Reg.Tbl.find_opt fun_info.const_ints reg

let preserve_reg_slot t (reg : Reg.t) =
  let fun_info = get_fun_info t in
  let typ = T.of_reg reg in
  Reg.Set.mem reg fun_info.preserved_reg_slots
  ||
  match reg.Reg.loc with
  | Reg _ ->
    Reg.Set.exists
      (fun preserved ->
        Reg.same_loc preserved reg && T.equal (T.of_reg preserved) typ)
      fun_info.preserved_reg_slots
  | Unknown | Stack _ -> false

(* t interface *)

let current_compilation_unit = ref None

let expect_llvm_ir_callbacks = ref []

let expect_llvm_asm_callbacks = ref []

let register_expect_llvm_ir_callback f =
  expect_llvm_ir_callbacks := f :: !expect_llvm_ir_callbacks

let register_expect_llvm_asm_callback f =
  expect_llvm_asm_callbacks := f :: !expect_llvm_asm_callbacks

let read_file_for_expect filename =
  let ic = In_channel.open_text filename in
  Fun.protect
    ~finally:(fun () -> In_channel.close ic)
    (fun () -> In_channel.input_all ic)

let normalize_llvm_expect_line line =
  if String.starts_with ~prefix:"source_filename = " line
  then "source_filename = \"<source>\""
  else if String.starts_with ~prefix:"\t.build_version macos, " line
  then "\t.build_version macos, <version>"
  else line

let normalize_toplevel_names s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len
    then Buffer.contents buf
    else if
      i + 7 <= len
      && String.sub s i 7 = "camlTOP"
      && i + 7 < len
      && Char.code s.[i + 7] >= Char.code '0'
      && Char.code s.[i + 7] <= Char.code '9'
    then (
      Buffer.add_string buf "camlTOP";
      let j = ref (i + 7) in
      while
        !j < len
        && Char.code s.[!j] >= Char.code '0'
        && Char.code s.[!j] <= Char.code '9'
      do
        incr j
      done;
      loop !j)
    else (
      Buffer.add_char buf s.[i];
      loop (i + 1))
  in
  loop 0

let base_normalize_llvm_output s =
  s |> Misc.normalise_eol |> Misc.delete_eol_spaces |> String.split_on_char '\n'
  |> List.map normalize_llvm_expect_line
  |> List.map normalize_toplevel_names

let line_contains_substring line substring =
  let line_len = String.length line in
  let substring_len = String.length substring in
  let rec loop i =
    if i + substring_len > line_len
    then false
    else String.sub line i substring_len = substring || loop (i + 1)
  in
  loop 0

let extract_llvm_code_functions lines =
  let rec collect_function acc = function
    | [] -> List.rev acc, []
    | line :: rest ->
      let acc = line :: acc in
      if String.trim line = "}"
      then List.rev acc, rest
      else collect_function acc rest
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | line :: rest ->
      if
        String.starts_with ~prefix:"define " line
        && line_contains_substring line "_code\""
      then
        let function_lines, rest = collect_function [] (line :: rest) in
        loop (List.rev function_lines @ acc) rest
      else loop acc rest
  in
  loop [] lines

let asm_code_label line =
  match String.index_opt line ':' with
  | None -> None
  | Some colon ->
    let label = String.sub line 0 colon |> String.trim in
    if String.ends_with ~suffix:"_code" label then Some (label ^ ":") else None

let extract_llvm_asm_functions lines =
  let rec collect_function acc = function
    | [] -> List.rev acc, []
    | line :: rest ->
      if line_contains_substring line "; -- End function"
      then List.rev acc, rest
      else collect_function (line :: acc) rest
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | line :: rest -> (
      match asm_code_label line with
      | Some label ->
        let function_lines, rest = collect_function [label] rest in
        loop (List.rev function_lines @ acc) rest
      | None -> loop acc rest)
  in
  loop [] lines

let normalize_llvm_ir_output s =
  s |> base_normalize_llvm_output |> extract_llvm_code_functions
  |> String.concat "\n"

let normalize_llvm_asm_output s =
  s |> base_normalize_llvm_output |> extract_llvm_asm_functions
  |> String.concat "\n"

let invoke_and_clear_callbacks callbacks output =
  let callbacks_to_invoke = List.rev !callbacks in
  callbacks := [];
  List.iter (fun f -> f output) callbacks_to_invoke

let invoke_expect_llvm_ir_callbacks ~llvmir_filename =
  if not (List.is_empty !expect_llvm_ir_callbacks)
  then
    invoke_and_clear_callbacks expect_llvm_ir_callbacks
      (normalize_llvm_ir_output (read_file_for_expect llvmir_filename))

let invoke_expect_llvm_asm_callbacks ~asm_filename =
  if not (List.is_empty !expect_llvm_asm_callbacks)
  then
    invoke_and_clear_callbacks expect_llvm_asm_callbacks
      (normalize_llvm_asm_output (read_file_for_expect asm_filename))

let get_current_compilation_unit msg =
  match !current_compilation_unit with
  | Some t -> t
  | None -> fail_msg "current compilation unit not set (%s)" msg

let create ~llvmir_filename ~ppf_dump =
  let oc = Out_channel.open_text llvmir_filename in
  let ppf = Format.formatter_of_out_channel oc in
  { llvmir_filename;
    asm_filename = None;
    sourcefile = None;
    oc;
    ppf;
    ppf_dump;
    current_fun_info = None;
    function_defs = [];
    data_defs = [];
    defined_symbols = String.Set.empty;
    referenced_symbols = String.Set.empty;
    function_decls = String.Map.empty;
    function_arg_types = String.Map.empty;
    called_intrinsics = String.Map.empty;
    c_call_wrappers = String.Map.empty;
    all_trap_blocks = [];
    module_asm = [];
    next_debug_metadata_id = 100000;
    debug_compile_unit_id = None;
    debug_file_id = None;
    debug_subroutine_type_id = None;
    debug_metadata_defs_rev = [];
    debug_module_flag_ids_rev = []
  }

let llvm_metadata_string s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b {|\22|}
      | '\\' -> Buffer.add_string b {|\5C|}
      | c when Char.code c < 32 || Char.code c >= 127 ->
        Buffer.add_string b (Printf.sprintf {|\%02X|} (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let fresh_debug_metadata_id t =
  let id = t.next_debug_metadata_id in
  t.next_debug_metadata_id <- id + 1;
  id

let add_debug_metadata_def t id def =
  t.debug_metadata_defs_rev
    <- Printf.sprintf "!%d = %s" id def :: t.debug_metadata_defs_rev

let add_debug_module_flag t name value =
  let id = fresh_debug_metadata_id t in
  add_debug_metadata_def t id
    (Printf.sprintf {|!{i32 2, !"%s", i32 %d}|} name value);
  t.debug_module_flag_ids_rev <- id :: t.debug_module_flag_ids_rev

let ensure_debug_compile_unit t =
  if not !Clflags.debug
  then None
  else
    match t.sourcefile with
    | None -> None
    | Some sourcefile -> (
      match t.debug_compile_unit_id with
      | Some id -> Some id
      | None ->
        let file_id = fresh_debug_metadata_id t in
        let subroutine_type_id = fresh_debug_metadata_id t in
        let compile_unit_id = fresh_debug_metadata_id t in
        let filename = Filename.basename sourcefile in
        let directory = Filename.dirname sourcefile in
        add_debug_metadata_def t file_id
          (Printf.sprintf {|!DIFile(filename: "%s", directory: "%s")|}
             (llvm_metadata_string filename)
             (llvm_metadata_string directory));
        add_debug_metadata_def t subroutine_type_id
          "!DISubroutineType(types: !{})";
        add_debug_metadata_def t compile_unit_id
          (Printf.sprintf
             {|distinct !DICompileUnit(language: DW_LANG_OCaml, file: !%d, producer: "oxcaml-llvm", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)|}
             file_id);
        add_debug_module_flag t "Dwarf Version" 4;
        add_debug_module_flag t "Debug Info Version" 3;
        t.debug_file_id <- Some file_id;
        t.debug_subroutine_type_id <- Some subroutine_type_id;
        t.debug_compile_unit_id <- Some compile_unit_id;
        Some compile_unit_id)

let first_debug_item dbg =
  Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg)
  |> List.rev
  |> List.find_opt (fun item -> item.Debuginfo.dinfo_line > 0)

let create_debug_subprogram t ~fun_name dbg =
  match ensure_debug_compile_unit t, first_debug_item dbg with
  | Some compile_unit_id, Some item ->
    let file_id = Option.get t.debug_file_id in
    let subroutine_type_id = Option.get t.debug_subroutine_type_id in
    let id = fresh_debug_metadata_id t in
    let name =
      Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
        item.dinfo_scopes
    in
    let name = if String.equal name "<unknown>" then fun_name else name in
    add_debug_metadata_def t id
      (Printf.sprintf
         {|distinct !DISubprogram(name: "%s", linkageName: "%s", scope: !%d, file: !%d, line: %d, type: !%d, scopeLine: %d, spFlags: DISPFlagDefinition, unit: !%d)|}
         (llvm_metadata_string name)
         (llvm_metadata_string fun_name)
         file_id file_id item.dinfo_line subroutine_type_id item.dinfo_line
         compile_unit_id);
    Some id
  | None, _ | _, None -> None

let create_debug_location t subprogram_dbg_metadata_id dbg =
  match subprogram_dbg_metadata_id, first_debug_item dbg with
  | Some subprogram_id, Some item ->
    let id = fresh_debug_metadata_id t in
    let column = Int.max 0 item.dinfo_char_start in
    add_debug_metadata_def t id
      (Printf.sprintf {|!DILocation(line: %d, column: %d, scope: !%d)|}
         item.dinfo_line column subprogram_id);
    Some (Printf.sprintf "!dbg !%d" id)
  | None, _ | _, None -> None

let add_called_intrinsic t name ~args ~res =
  fail_if_not ~msg:"expected intrinsic" "add_called_intrinsic"
    (String.begins_with name ~prefix:"llvm");
  let fundecl = LL.Fundecl.create name args res in
  (match String.Map.find_opt name t.called_intrinsics with
  | None -> ()
  | Some fundecl' ->
    fail_if_not ~msg:"incompatible signatures" "add_called_intrinsic"
      (LL.Fundecl.equal fundecl fundecl'));
  t.called_intrinsics <- String.Map.add name fundecl t.called_intrinsics

let add_function_decl t fundecl =
  let name = fundecl.LL.Fundecl.name in
  (match String.Map.find_opt name t.function_decls with
  | None -> ()
  | Some fundecl' ->
    fail_if_not ~msg:"incompatible signatures" "add_function_decl"
      (LL.Fundecl.equal fundecl fundecl'));
  t.function_decls <- String.Map.add name fundecl t.function_decls

let sanitize_symbol_component s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      let keep =
        match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false
      in
      Buffer.add_char buf (if keep then c else '_'))
    s;
  Buffer.contents buf

let add_c_call_wrapper t c_fun_name ~args ~res =
  let signature =
    List.map T.to_string (args @ res)
    |> List.map sanitize_symbol_component
    |> String.concat "."
  in
  let wrapper_name =
    F.sprintf "c_call_wrapper.%s.%d.%s.%d" c_fun_name (List.length args)
      signature (List.length res)
  in
  (match String.Map.find_opt wrapper_name t.c_call_wrappers with
  | None -> ()
  | Some { c_fun_name = c_fun_name'; args = args'; res = res' } ->
    let all_equal =
      String.equal c_fun_name c_fun_name'
      && List.equal T.equal args args'
      && List.equal T.equal res res'
    in
    fail_if_not ~msg:"incompatible signatures" "add_c_call_wrapper" all_equal);
  t.c_call_wrappers
    <- String.Map.add wrapper_name { c_fun_name; args; res } t.c_call_wrappers;
  wrapper_name

let add_defined_symbol t sym_name =
  t.defined_symbols <- String.Set.add sym_name t.defined_symbols

let add_referenced_symbol t sym_name =
  t.referenced_symbols <- String.Set.add sym_name t.referenced_symbols

(* Note that these need to be reversed while emitting *)

let add_function_def t fundef = t.function_defs <- fundef :: t.function_defs

let add_data_def t data_def = t.data_defs <- data_def :: t.data_defs

let add_module_asm t asm_lines = t.module_asm <- asm_lines @ t.module_asm

let register_function_signature t (fd : Cmm.fundecl) =
  let arg_types =
    List.concat_map
      (fun (_var, ty) -> Array.to_list ty)
      fd.fun_args
    |> List.map T.of_machtype_component
  in
  t.function_arg_types
    <- String.Map.add fd.fun_name.sym_name arg_types t.function_arg_types

let register_function_signatures phrases =
  let t = get_current_compilation_unit "register_function_signatures" in
  List.iter
    (function
      | Cmm.Cfunction fd -> register_function_signature t fd
      | Cmm.Cdata _ -> ())
    phrases

let complete_func_def t =
  add_function_def t (E.get_fun (get_fun_info t).emitter);
  t.all_trap_blocks
    <- (Label.Tbl.to_list (get_fun_info t).trap_blocks |> List.map snd)
       @ t.all_trap_blocks;
  t.current_fun_info <- None

let gc_name = "oxcaml" (* The name of the [GCStrategy] we use in LLVM *)

(* Runtime registers. These are registers that get threaded through function
   arguments and returns and are pinned to particular physical registers via the
   calling convention.

   The threading exists to teach LLVM that ordinary OxCaml calls receive and
   return the current domain state pointer and allocation pointer. It is not
   meant to add real data movement. On AArch64, the OxCaml calling convention
   assigns the first two integer argument and return slots to x28 and x27, which
   are the runtime registers expected by the native runtime. When both caller
   and callee use this calling convention, register allocation should therefore
   make the SSA values and the physical runtime registers coincide at the call
   boundary.

   Note that we treat these similarly to [Reg.t]s, where we alloca them in the
   entry block and access them through load/stores to be simplified by mem2reg
   in LLVM. We assume they are [i64]s while threading through functions. *)

let domainstate_ptr = V.of_ident ~typ:T.ptr (LL.Ident.local "ds")

let allocation_ptr = V.of_ident ~typ:T.ptr (LL.Ident.local "alloc")

let runtime_regs = [domainstate_ptr; allocation_ptr]

let domainstate_idx = 0

let _allocation_idx = 1

let runtime_reg_idents = List.map V.get_ident_exn runtime_regs

(* Registers living in the domain state are already stored in the appropriate
   address as arguments or when returned, so we don't touch them while doing a
   call / return *)
(* CR yusumez: We don't expect to get arguments from the stack, but this might
   happen if we have too many arguments. There might be a way to limit this in
   the frontend. *)
let reg_list_for_call regs =
  Array.to_list regs |> List.filter (fun reg -> not (Reg.is_domainstate reg))

let loc_results_call regs = Proc.loc_results_call (Reg.typv regs) |> fst

let loc_results_return regs = Proc.loc_results_return (Reg.typv regs)

let make_ret_type ret_types =
  let runtime_reg_types = List.map (fun _ -> T.i64) runtime_regs in
  T.(Struct [Struct runtime_reg_types; Struct ret_types])

(* Filters out return types to be passed via domain state since they aren't
   dealt with in LLVM's calling conventions *)
let filter_ds_and_make_ret_type ret_machtype =
  let cc_regs = Proc.loc_results_return ret_machtype in
  let actual_ret_types = reg_list_for_call cc_regs |> List.map T.of_reg in
  make_ret_type actual_ret_types

let make_arg_types arg_types =
  List.map (fun _ -> T.i64) runtime_regs @ arg_types

let join_instruction_metadata a b =
  match a, b with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> Some (a ^ ", " ^ b)

let emit_ins ?comment ?res_ident ?metadata t op =
  let fun_info = get_fun_info t in
  let dbg_metadata =
    join_instruction_metadata fun_info.current_dbg_metadata metadata
  in
  E.ins ?comment ?dbg_metadata ?res_ident
    fun_info.emitter op

let emit_ins_no_res ?comment t op =
  let fun_info = get_fun_info t in
  E.ins_no_res ?comment ?dbg_metadata:fun_info.current_dbg_metadata
    fun_info.emitter op

let emit_label t label_value =
  let fun_info = get_fun_info t in
  E.label_def fun_info.emitter (V.get_ident_exn label_value)

let emit_comment t fmt =
  let fun_info = get_fun_info t in
  F.kasprintf (fun s -> E.comment fun_info.emitter s) fmt

(* Common helpers *)

let cast t arg to_ =
  let from = V.get_type arg in
  if T.equal from to_
  then arg
  else if T.is_int from && T.is_ptr to_
  then emit_ins t (I.convert Inttoptr ~arg ~to_)
  else if T.is_ptr from && T.is_int to_
  then emit_ins t (I.convert Ptrtoint ~arg ~to_)
  else if T.is_ptr from && T.is_ptr to_
  then emit_ins t (I.convert Addrspacecast ~arg ~to_)
  else fail_msg ~name:"cast" "unexpected types: %a, %a" T.pp_t from T.pp_t to_

let cast_to_ptr t arg =
  if T.is_ptr (V.get_type arg) then arg else cast t arg T.ptr

let do_offset ?(int_type = T.i64) t arg res_type offset =
  if offset = 0
  then cast t arg res_type
  else if T.is_ptr (V.get_type arg) && T.is_ptr res_type
  then
    let base_ptr = cast t arg res_type in
    emit_ins t
      (I.getelementptr ~base_type:T.i8 ~base_ptr
         ~indices:[V.of_int offset])
  else
    let base = cast t arg int_type in
    let offset_val =
      emit_ins t (I.binary Add ~arg1:base ~arg2:(V.of_int offset))
    in
    cast t offset_val res_type

let load_reg_to_temp ?typ t reg =
  let typ = Option.value typ ~default:(T.of_reg reg) in
  match get_const_int_for_reg t reg with
  | Some n -> cast t (V.of_nativeint n) typ
  | None ->
    let ptr = get_alloca_for_reg t reg in
    let value = emit_ins t (I.load ~ptr ~typ:(T.of_reg reg)) in
    cast t value typ

let load_reg_as_expected_arg t (reg : Reg.t) expected_type =
  load_reg_to_temp ~typ:expected_type t reg

let load_reg_as_tagged_int t (reg : Reg.t) = load_reg_to_temp ~typ:T.i64 t reg

let store_into_reg ?(force_volatile = false) t reg to_store =
  if force_volatile
  then fail_msg ~name:"store_into_reg" "volatile register slots are unsupported";
  clear_const_int_for_reg t reg;
  let ptr = get_alloca_for_reg t reg in
  let to_store = cast t to_store (T.of_reg reg) in
  emit_ins_no_res t (I.store ~ptr ~to_store)

let live_gc_root_regs_across t (i : 'a Cfg.instruction) =
  match (get_fun_info t).liveness with
  | None -> Reg.Set.empty
  | Some liveness -> (
    match InstructionId.Tbl.find_opt liveness i.id with
    | None -> Reg.Set.empty
    | Some { Cfg_liveness.before = _; across } ->
      (* This path deliberately handles only single-word OCaml [Val] roots.
         [Cmm.is_val] excludes [Valx2], so the new slow-path root slots match
         the old alloca-root behavior rather than adding partial [Valx2]
         support. *)
      Reg.Set.filter
        (fun reg ->
          Cmm.is_val reg.typ && Option.is_some (get_alloca_for_reg_opt t reg))
        across)

let reject_addr_regs_across t (i : 'a Cfg.instruction) msg =
  match (get_fun_info t).liveness with
  | None -> ()
  | Some liveness -> (
    match InstructionId.Tbl.find_opt liveness i.id with
    | None -> ()
    | Some { Cfg_liveness.before = _; across } ->
      let addr_regs = Reg.Set.filter (fun reg -> Cmm.is_addr reg.typ) across in
      if not (Reg.Set.is_empty addr_regs)
      then
        fail_msg ~name:"reject_addr_regs_across" "%s: %a" msg Printreg.regset
          addr_regs)

let collect_addr_regs_known_base (cfg : Cfg.t) =
  let collect_once regs =
    Cfg.fold_body_instructions cfg ~init:regs ~f:(fun regs i ->
      match[@ocaml.warning "-fragile-match"] i.desc, i.arg, i.res with
      | Op Move, [| src |], [| dst |]
        when Cmm.is_addr src.typ && Cmm.is_val dst.typ ->
        Reg.Set.add dst (Reg.Set.add src regs)
      | Op Move, [| src |], [| dst |] when Reg.Set.mem src regs ->
        Reg.Set.add dst regs
      | Op (Alloc _), _, [| dst |] when Cmm.is_val dst.typ ->
        Reg.Set.add dst regs
      | _ -> regs)
  in
  let rec fixpoint regs =
    let regs' = collect_once regs in
    if Reg.Set.equal regs regs' then regs else fixpoint regs'
  in
  fixpoint Reg.Set.empty

let collect_static_addr_regs (cfg : Cfg.t) =
  let static regs reg = Reg.Set.mem reg regs in
  let collect_once regs =
    Cfg.fold_body_instructions cfg ~init:regs ~f:(fun regs i ->
      match[@ocaml.warning "-fragile-match"] i.desc, i.arg, i.res with
      | Op (Const_symbol _), _, [| dst |] -> Reg.Set.add dst regs
      | Op (Load { memory_chunk = Word_int; _ }), [| src |], [| dst |]
        when static regs src ->
        Reg.Set.add dst regs
      | Op (Move | Opaque), [| src |], [| dst |] when static regs src ->
        Reg.Set.add dst regs
      | Op (Intop_imm (Iadd, _)), [| src |], [| dst |] when static regs src ->
        Reg.Set.add dst regs
      | Op (Intop Iadd), [| left; right |], [| dst |]
        when static regs left || static regs right ->
        Reg.Set.add dst regs
      | Op (Intop Isub), [| left; _right |], [| dst |] when static regs left ->
        Reg.Set.add dst regs
      | _ -> regs)
  in
  let rec fixpoint regs =
    let regs' = collect_once regs in
    if Reg.Set.equal regs regs' then regs else fixpoint regs'
  in
  fixpoint Reg.Set.empty

let slow_path_root_regs_across t i =
  live_gc_root_regs_across t i
  |> Reg.Set.filter (fun reg -> Option.is_none (get_const_int_for_reg t reg))

let basic_has_gc_safepoint (i : Cfg.basic Cfg.instruction) =
  match i.desc with
  | Op (Alloc { mode = Heap; _ } | Poll) -> true
  | Op (Alloc { mode = Local; _ }) -> false
  | Reloadretaddr | Prologue | Epilogue | Pushtrap _ | Poptrap _ | Stack_check _
  | Op
      ( Move | Spill | Reload | Opaque | Pause | Begin_region | End_region
      | Dls_get | Tls_get | Domain_index | Const_int _ | Const_float32 _
      | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
      | Const_vec512 _ | Stackoffset _ | Load _
      | Store (_, _, _)
      | Intop _ | Int128op _
      | Intop_imm (_, _)
      | Intop_atomic _
      | Floatop (_, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Specific _ | Name_for_debugger _ ) ->
    false

let eligible_basic_safepoint_for_slow_root_slots ?(has_unwind = false)
    active_traps (i : Cfg.basic Cfg.instruction) =
  basic_has_gc_safepoint i && (not has_unwind)
  &&
  match InstructionId.Tbl.find_opt active_traps i.id with
  | Some None -> true
  | None | Some (Some _) -> false

let load_live_gc_roots_across t i =
  live_gc_root_regs_across t i
  |> Reg.Set.elements
  |> List.map (fun reg -> reg, V.poison T.val_ptr)

let slow_path_root_slots_for_basic_safepoint ?unwind_label t i =
  if
    eligible_basic_safepoint_for_slow_root_slots
      ~has_unwind:(Option.is_some unwind_label)
      (get_fun_info t).active_traps i
  then
    let rec pair slots regs =
      match slots, regs with
      | _, [] -> []
      | root_slot :: slots, reg :: regs -> { reg; root_slot } :: pair slots regs
      | [], _ :: _ ->
        fail_msg ~name:"slow_path_root_slots_for_basic_safepoint"
          "not enough slow path root slots"
    in
    slow_path_root_regs_across t i
    |> Reg.Set.elements
    |> pair (get_fun_info t).slow_path_root_slots
    |> fun roots -> Some roots
  else None

let frontend_gc_roots_enabled () =
  !Clflags.llvm_frontend_gc_roots

let frontend_alloca_roots_enabled = frontend_gc_roots_enabled

let slow_path_root_slots_enabled = frontend_gc_roots_enabled

let roots_for_basic_safepoint ?unwind_label t i msg =
  reject_addr_regs_across t i msg;
  if slow_path_root_slots_enabled ()
  then
    slow_path_root_slots_for_basic_safepoint ?unwind_label t i |> function
    | Some roots -> roots, []
    | None -> [], load_live_gc_roots_across t i
  else [], load_live_gc_roots_across t i

let refresh_live_gc_roots _t _roots = ()

let live_gc_root_alloca_bundles t roots =
  let roots =
    List.filter_map
      (fun (reg, _root) ->
        if preserve_reg_slot t reg
        then Some (reg, get_alloca_for_reg t reg)
        else None)
      roots
  in
  if
    not (frontend_alloca_roots_enabled ())
    && not (List.is_empty roots)
  then (
    let regs =
      List.fold_left
        (fun regs (reg, _root) -> Reg.Set.add reg regs)
        Reg.Set.empty roots
    in
    fail_msg ~name:"live_gc_root_alloca_bundles"
      "frontend register slots reached gc-live in no-frontend-root mode: %a"
      Printreg.regset regs);
  let roots = List.map snd roots in
  if List.is_empty roots then [] else ["gc-live", roots]

let live_gc_root_slot_bundles root_slots =
  let roots = List.map (fun { root_slot; reg = _ } -> root_slot) root_slots in
  if List.is_empty roots then [] else ["gc-live", roots]

let store_slow_path_roots t root_slots =
  List.iter
    (fun { reg; root_slot } ->
      let value = cast t (load_reg_to_temp t reg) T.val_ptr in
      emit_ins_no_res t (I.store ~ptr:root_slot ~to_store:value))
    root_slots

let refresh_slow_path_roots t root_slots =
  List.iter
    (fun { reg; root_slot } ->
      let value = emit_ins t (I.load ~ptr:root_slot ~typ:T.val_ptr) in
      store_into_reg t reg value)
    root_slots

let oxcaml_debug_deopt_marker = 0x6f786364

let oxcaml_debug_deopt_version = 1

let oxcaml_alloc_deopt_marker = 0x6f786361

let oxcaml_alloc_deopt_version = 1

let string_deopt_args s =
  let chunk_size = 3 in
  let len = String.length s in
  let rec loop pos acc =
    if pos >= len
    then List.rev acc
    else
      let chunk_len = Int.min chunk_size (len - pos) in
      let chunk = ref 0 in
      for i = 0 to chunk_len - 1 do
        chunk := !chunk lor (Char.code s.[pos + i] lsl (8 * i))
      done;
      loop (pos + chunk_len) (V.of_int !chunk :: acc)
  in
  V.of_int len :: loop 0 []

let debug_item_deopt_args d =
  let open Debuginfo in
  let defname =
    Scoped_location.string_of_scopes ~include_zero_alloc:false d.dinfo_scopes
  in
  let char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
  let char_end_offset = d.dinfo_end_bol - d.dinfo_start_bol in
  [ V.of_int d.dinfo_line;
    V.of_int (d.dinfo_end_line - d.dinfo_line);
    V.of_int d.dinfo_char_start;
    V.of_int char_end;
    V.of_int char_end_offset;
    V.of_int d.dinfo_char_end ]
  @ string_deopt_args d.dinfo_file
  @ string_deopt_args defname

let debug_deopt_args ~primitive_call ~raise_call dbg =
  let items = Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg) |> List.rev in
  match items with
  | [] -> []
  | { Debuginfo.dinfo_line = line; _ } :: _ when line > 0 ->
    [ V.of_int oxcaml_debug_deopt_marker;
      V.of_int oxcaml_debug_deopt_version;
      V.of_int (if raise_call then 2 else if primitive_call then 1 else 0);
      V.of_int (List.length items) ]
    @ (items |> List.map debug_item_deopt_args |> List.concat)
  | _ -> []

let alloc_deopt_args alloc_info =
  let alloc_item_deopt_args Cmm.{ alloc_words; alloc_dbg; _ } =
    let items =
      Debuginfo.Dbg.to_list (Debuginfo.get_dbg alloc_dbg) |> List.rev
    in
    V.of_int alloc_words
    :: V.of_int (List.length items)
    :: (items |> List.map debug_item_deopt_args |> List.concat)
  in
  match alloc_info with
  | None -> []
  | Some alloc_info ->
    [ V.of_int oxcaml_alloc_deopt_marker;
      V.of_int oxcaml_alloc_deopt_version;
      V.of_int (List.length alloc_info) ]
    @ (alloc_info |> List.map alloc_item_deopt_args |> List.concat)

let deopt_bundle ?alloc_info ~primitive_call ~raise_call dbg =
  match
    debug_deopt_args ~primitive_call ~raise_call dbg
    @ alloc_deopt_args alloc_info
  with
  | [] -> []
  | args -> ["deopt", args]

let call_operand_bundles ?alloc_info ?(slow_path_roots = []) t ~primitive_call
    ~raise_call dbg live_roots =
  let frontend_roots =
    if frontend_alloca_roots_enabled () then live_roots else []
  in
  deopt_bundle ?alloc_info ~primitive_call ~raise_call dbg
  @ live_gc_root_alloca_bundles t frontend_roots
  @ live_gc_root_slot_bundles slow_path_roots

let load_domainstate_addr ?ds_loc ?(offset = 0) t ds_field =
  let typ = T.ptr in
  let ds =
    match ds_loc with
    | None -> emit_ins t (I.load ~ptr:domainstate_ptr ~typ:T.i64)
    | Some ds_loc -> ds_loc
  in
  let offset = offset + (Domainstate.idx_of_field ds_field * Arch.size_addr) in
  do_offset t ds typ offset

let domainstate_addr_for_location t (loc : Reg.t) =
  match loc.loc with
  | Stack (Domainstate idx) ->
    load_domainstate_addr ~offset:idx t Domain_extra_params
  | Unknown | Reg _ | Stack (Local _ | Incoming _ | Outgoing _) ->
    fail_msg ~name:"domainstate_addr_for_location"
      "expected domainstate location"

let load_address t addr_mode base typ =
  let offset = Arch.addressing_displacement_for_llvmize addr_mode in
  do_offset t base typ offset

let load_address_from_reg t addr_mode (reg : Reg.t) =
  if Cmm.is_val reg.typ || Cmm.is_addr reg.typ
  then
    let base = load_reg_to_temp ~typ:T.val_ptr t reg in
    load_address t addr_mode base T.val_ptr
  else if Cmm.is_int reg.typ
  then
    let base = load_reg_to_temp ~typ:T.i64 t reg |> cast_to_ptr t in
    load_address t addr_mode base T.ptr
  else
    fail_msg ~name:"load_address_from_reg" "unsupported address register type"

let assemble_struct t root_type vals_to_insert =
  let insert cur_struct (indices, to_insert) =
    emit_ins t (I.insertvalue ~aggregate:cur_struct ~indices ~to_insert)
  in
  let init = V.poison root_type in
  List.fold_left insert init vals_to_insert

let extract_struct t root_struct indices_to_extract =
  List.map
    (fun indices -> emit_ins t (I.extractvalue ~aggregate:root_struct ~indices))
    indices_to_extract

(* Helpers for calls *)

let runtime_register_metadata name = F.sprintf {|!{!"%s\00"}|} name

let read_fixed_runtime_register t name =
  let intrinsic_name = "llvm.read_register.i64" in
  let args = [V.imm T.metadata (runtime_register_metadata name)] in
  add_called_intrinsic t intrinsic_name ~args:[T.metadata] ~res:(Some T.i64);
  emit_ins t
    (I.call ~func:(LL.Ident.global intrinsic_name) ~args ~res_type:(Some T.i64)
       ~attrs:[] ~operand_bundles:[] ~cc:Default ~musttail:false)

let prepare_call_args ?(use_physical_runtime_regs = false) t args =
  let runtime_args =
    if use_physical_runtime_regs
    then
      match Target_system.architecture () with
      | Target_system.AArch64 ->
        [read_fixed_runtime_register t "x28"; read_fixed_runtime_register t "x27"]
      | Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
        List.map (fun ptr -> emit_ins t (I.load ~ptr ~typ:T.i64)) runtime_regs
    else List.map (fun ptr -> emit_ins t (I.load ~ptr ~typ:T.i64)) runtime_regs
  in
  runtime_args @ args

let prepare_call_args_from_regs ?use_physical_runtime_regs ?expected_arg_types t
    regs =
  let args =
    match expected_arg_types with
    | None -> List.map (load_reg_to_temp t) regs
    | Some expected_arg_types ->
      (try
         List.map2
           (fun reg expected_type -> load_reg_as_expected_arg t reg expected_type)
           regs expected_arg_types
       with Invalid_argument _ ->
         fail_msg ~name:"prepare_call_args_from_regs"
           "direct-call argument count mismatch")
  in
  prepare_call_args ?use_physical_runtime_regs t
    args

let extract_call_res t call_res_struct num_res_values =
  (* Runtime regs *)
  let runtime_reg_indices = List.mapi (fun i _ -> [0; i]) runtime_regs in
  let extracted_values = extract_struct t call_res_struct runtime_reg_indices in
  List.iter2
    (fun ptr to_store -> emit_ins_no_res t (I.store ~ptr ~to_store))
    runtime_regs extracted_values;
  (* Actual return values *)
  let res_value_indices = List.init num_res_values (fun i -> [1; i]) in
  extract_struct t call_res_struct res_value_indices

let extract_call_res_into_original_regs t call_res_struct original_res_regs =
  let result_locations = loc_results_call original_res_regs in
  let returned_count =
    Array.fold_left
      (fun count loc -> if Reg.is_domainstate loc then count else count + 1)
      0 result_locations
  in
  let returned_values = extract_call_res t call_res_struct returned_count in
  let returned_values = ref returned_values in
  Array.iter2
    (fun original_reg loc ->
      if Reg.is_domainstate loc
      then
        let ptr = domainstate_addr_for_location t loc in
        let value = emit_ins t (I.load ~ptr ~typ:(T.of_reg original_reg)) in
        store_into_reg t original_reg value
      else
        match !returned_values with
        | value :: rest ->
          returned_values := rest;
          store_into_reg t original_reg value
        | [] ->
          fail_msg ~name:"extract_call_res_into_original_regs"
            "not enough returned values")
    original_res_regs result_locations

let assemble_return t res_type values =
  let runtime_values =
    List.mapi
      (fun i ptr -> [0; i], emit_ins t (I.load ~ptr ~typ:T.i64))
      runtime_regs
  in
  let actual_values = List.mapi (fun i v -> [1; i], v) values in
  assemble_struct t res_type (runtime_values @ actual_values)

(* Prepare and extract arguments following the OCaml calling convention in LLVM,
   handling the threading of runtime registers.

   For OxCaml calls, the generated IR signature explicitly passes [ds] and
   [alloc] before the ordinary OCaml arguments, and explicitly returns updated
   [ds] and [alloc] before the ordinary OCaml results. This is an SSA-level
   model of the runtime-register dependency. The AArch64 OxCaml calling
   convention maps those leading [i64] arguments/results to x28/x27, so the
   intended final code has no copies just to pass the domain state pointer or
   allocation pointer in and out of an ordinary OxCaml call. *)
let call_simple ?(attrs = []) ?(dbg = Debuginfo.none) ?(raise_call = false)
    ?(primitive_call = false) ?alloc_info ?(live_roots = [])
    ?(slow_path_roots = []) ?unwind_label ~cc t name args res_types =
  let fun_info = get_fun_info t in
  let prev_dbg_metadata = fun_info.current_dbg_metadata in
  let call_dbg_metadata =
    match prev_dbg_metadata with
    | Some _ -> prev_dbg_metadata
    | None -> create_debug_location t fun_info.subprogram_dbg_metadata_id dbg
  in
  fun_info.current_dbg_metadata <- call_dbg_metadata;
  Fun.protect
    ~finally:(fun () -> fun_info.current_dbg_metadata <- prev_dbg_metadata)
    (fun () ->
      let args = prepare_call_args t args in
      let res_type = Some (make_ret_type res_types) in
      let func = LL.Ident.global name in
      let operand_bundles =
        call_operand_bundles ?alloc_info ~slow_path_roots t ~primitive_call
          ~raise_call dbg live_roots
      in
      let res =
        match unwind_label with
        | None ->
          emit_ins t
            (I.call ~func ~args ~res_type ~attrs ~operand_bundles ~cc
               ~musttail:false)
        | Some unwind_label ->
          let normal_label = V.of_label (Cmm.new_label ()) in
          let res =
            emit_ins t
              (I.invoke ~func ~args ~res_type ~attrs ~operand_bundles ~cc
                 ~normal:normal_label ~unwind:unwind_label)
          in
          emit_label t normal_label;
          res
      in
      refresh_live_gc_roots t live_roots;
      extract_call_res t res (List.length res_types))

module Safepoint = struct
  type t =
    | Call of
        { stack_offset : int;
          active_trap_bytes : int
        }
    | Poll of
        { stack_offset : int;
          active_trap_bytes : int
        }
    | Allocation of
        { alloc_words : int;
          stack_offset : int;
          active_trap_bytes : int
        }

  (* We use statepoint IDs to pass information to the frametable printer in LLVM.
     The current encoding is:

     * [active_trap_bytes]: On AArch64, this is encoded as a trap depth in bits
     1..3. AArch64 stack offsets are 16-byte aligned, so these bits are
     otherwise unused. This tells LLVM to adjust live stack root offsets for
     native AArch64 trap frames. Those frames dynamically move SP, but they are
     not LLVM frame objects.

     * [alloc_words]: This lives in the most significant 16 bits. This is used
     for calls to the GC generated by [Alloc] instructions which need to know
     how many words have been allocated. See how [Emitaux.emit_frames] handles
     [Dbg_alloc] for details. We collect multiple allocations into one number
     because the current encoding does not have room to list them separately.

     * [stack_offset]: This lives in the least significant 16 bits. This tells
     LLVM to adjust the frame size for dynamic stack pointer changes that are
     already tracked in CFG stack offsets. On AArch64, native trap frame bytes
     are carried in [active_trap_bytes] instead.

     * The least significant bit is set if this call is to [caml_call_gc]. We
     can do this because [stack_offset] must be even. *)
  let validate_stack_offset stack_offset =
    let align_mask =
      match Target_system.architecture () with
      | Target_system.AArch64 -> 15
      | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
        1
    in
    fail_if_not ~msg:"invalid stack offset" "Safepoint.encode_statepoint_id"
      (0 <= stack_offset && stack_offset < 65_536
      && stack_offset land align_mask = 0)

  let validate_active_trap_bytes active_trap_bytes =
    fail_if_not ~msg:"invalid active trap bytes" "Safepoint.encode_statepoint_id"
      (0 <= active_trap_bytes && active_trap_bytes <= 7 * 16
      && active_trap_bytes land 15 = 0)

  let active_trap_bytes_bits active_trap_bytes =
    validate_active_trap_bytes active_trap_bytes;
    (active_trap_bytes / 16) lsl 1

  let encode_statepoint_id = function
    | Call { stack_offset; active_trap_bytes } ->
      validate_stack_offset stack_offset;
      active_trap_bytes_bits active_trap_bytes lor stack_offset
    | Poll { stack_offset; active_trap_bytes } ->
      validate_stack_offset stack_offset;
      active_trap_bytes_bits active_trap_bytes lor stack_offset lor 1
    | Allocation { alloc_words; stack_offset; active_trap_bytes } ->
      fail_if_not ~msg:"invalid alloc size" "Safepoint.encode_statepoint_id"
        (0 <= alloc_words && alloc_words < 65_536);
      validate_stack_offset stack_offset;
      active_trap_bytes_bits active_trap_bytes
      lor (alloc_words lsl 16) lor stack_offset lor 1

  let attr t = LL.Fn_attr.Statepoint_id (encode_statepoint_id t)

  let alloc_words_of_dbg_alloc =
    List.fold_left (fun acc Cmm.{ alloc_words; _ } -> acc + alloc_words) 0
end

let active_trap_bytes t (i : 'a Cfg.instruction) =
  match Target_system.architecture () with
  | Target_system.AArch64 ->
    let active_trap_depth =
      InstructionId.Tbl.find_opt (get_fun_info t).active_trap_depths i.id
      |> Option.value ~default:0
    in
    16 * active_trap_depth
  | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
  | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
    0

let statepoint_stack_offset t (i : 'a Cfg.instruction) =
  match Target_system.architecture () with
  | Target_system.AArch64 ->
    let stack_offset = i.stack_offset - active_trap_bytes t i in
    fail_if_not ~msg:"negative adjusted stack offset" "statepoint_stack_offset"
      (stack_offset >= 0);
    stack_offset
  | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
  | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
    i.stack_offset

let gc_attr ?alloc_info ?safepoint ~can_call_gc t (i : 'a Cfg.instruction) =
  if can_call_gc
  then
    let stack_offset = statepoint_stack_offset t i in
    let active_trap_bytes = active_trap_bytes t i in
    let safepoint =
      match safepoint, alloc_info with
      | Some safepoint, None -> safepoint
      | Some _, Some _ ->
        fail_msg ~name:"gc_attr" "got both [safepoint] and [alloc_info]"
      | None, Some alloc_info ->
        let alloc_words = Safepoint.alloc_words_of_dbg_alloc alloc_info in
        Safepoint.Allocation { alloc_words; stack_offset; active_trap_bytes }
      | None, None -> Safepoint.Call { stack_offset; active_trap_bytes }
    in
    [Safepoint.attr safepoint]
  else [LL.Fn_attr.Gc_leaf_function]

(* Helpers for LLVM intrinsics *)

let call_llvm_intrinsic_aux ~emit_ins t name args res_type =
  let arg_types = List.map V.get_type args in
  let intrinsic_name = "llvm." ^ name in
  let func = LL.Ident.global intrinsic_name in
  add_called_intrinsic t intrinsic_name ~args:arg_types ~res:res_type;
  emit_ins t
    (I.call ~func ~args ~res_type ~attrs:[] ~operand_bundles:[] ~cc:Default
       ~musttail:false)

let call_llvm_intrinsic t name args res_type =
  call_llvm_intrinsic_aux
    ~emit_ins:(fun t -> emit_ins t)
    t name args (Some res_type)

let call_llvm_intrinsic_no_res t name args =
  call_llvm_intrinsic_aux
    ~emit_ins:(fun t -> emit_ins_no_res t)
    t name args None

let stack_pointer_register_name () =
  match Target_system.architecture () with
  | Target_system.AArch64 -> "sp"
  | Target_system.X86_64 -> "rsp"
  | Target_system.IA32 | Target_system.ARM | Target_system.POWER
  | Target_system.Z | Target_system.Riscv ->
    fail_msg ~name:"stack_pointer_register_name"
      "unsupported architecture for LLVM backend"

let stack_pointer_register_metadata () =
  F.sprintf {|!{!"%s\00"}|} (stack_pointer_register_name ())

let read_stack_pointer t =
  (* [read_register] (rather than inline asm) lets the backend fold the read
     into the limit comparison ([cmp sp, xN]) and removes the inline asm
     scheduling barrier. Ordering with respect to SP updates is preserved:
     the surrounding stack-check sequences read SP in the block that uses it,
     with no SP-modifying instructions in between. *)
  call_llvm_intrinsic t "read_register.i64"
    [V.imm T.metadata (stack_pointer_register_metadata ())]
    T.i64

let write_stack_pointer t v =
  let v = cast t v T.i64 in
  call_llvm_intrinsic_no_res t "write_register.i64"
    [V.imm T.metadata (stack_pointer_register_metadata ()); v]

let read_allocation_pointer_register t =
  let asm, constraints =
    match Target_system.architecture () with
    | Target_system.X86_64 -> "movq %r15, $0", "=r"
    | Target_system.AArch64 -> "mov $0, x27", "=r"
    | Target_system.IA32 | Target_system.ARM | Target_system.POWER
    | Target_system.Z | Target_system.Riscv ->
      fail_msg ~name:"read_allocation_pointer_register"
        "unsupported architecture for LLVM backend"
  in
  emit_ins t
    (I.inline_asm ~asm ~constraints ~args:[] ~res_type:(Some T.i64)
       ~sideeffect:true)

let write_trap_pointer_register t trap_ptr =
  match Target_system.architecture () with
  | Target_system.AArch64 ->
    let trap_ptr = cast t trap_ptr T.i64 in
    emit_ins_no_res t
      (I.inline_asm ~asm:"mov x26, $0" ~constraints:"r" ~args:[trap_ptr]
         ~res_type:T.Or_void.void ~sideeffect:true)
  | Target_system.X86_64 -> ()
  | Target_system.IA32 | Target_system.ARM | Target_system.POWER
  | Target_system.Z | Target_system.Riscv ->
    fail_msg ~name:"write_trap_pointer_register"
      "unsupported architecture for LLVM backend"

(* Other miscellaneous stuff... *)

let reject_addr_regs (regs : Reg.t array) msg =
  if Array.exists (fun (reg : Reg.t) -> Cmm.is_addr reg.typ) regs
  then fail_msg ~name:"reject_addr_regs" "%s" msg

let br_label t label = emit_ins_no_res t (I.br (V.of_label label))

let llvm_eh_personality = LL.Ident.global "caml_llvm_eh_personality"

let llvm_landingpad_type = T.(Struct [ptr; i32])

(* Terminator instructions *)

let int_comp t cond (i : _ Cfg.instruction) ~imm =
  let cond = I.icmp_cond_of_ocaml cond in
  let compare_as_val =
    Array.exists (fun (arg : Reg.t) -> Cmm.is_val arg.typ || Cmm.is_addr arg.typ)
      i.arg
  in
  let typ = if compare_as_val then T.val_ptr else T.i64 in
  let load_arg reg = load_reg_to_temp ~typ t reg in
  match imm with
  | None ->
    let arg1 = load_arg i.arg.(0) in
    let arg2 = load_arg i.arg.(1) in
    emit_ins t (I.icmp cond ~arg1 ~arg2)
  | Some n ->
    let arg1 = load_arg i.arg.(0) in
    let arg2 = cast t (V.of_int n) typ in
    emit_ins t (I.icmp cond ~arg1 ~arg2)

let float_comp t cond (i : _ Cfg.instruction) typ =
  let cond = I.fcmp_cond_of_ocaml cond in
  let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
  let arg2 = load_reg_to_temp ~typ t i.arg.(1) in
  emit_ins t (I.fcmp cond ~arg1 ~arg2)

let odd_test t (i : _ Cfg.instruction) =
  let arg = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
  emit_ins t (I.convert Trunc ~arg ~to_:T.i1)

let test t (op : Operation.test) (i : _ Cfg.instruction) =
  match op with
  | Itruetest -> int_comp t Cne i ~imm:(Some 0)
  | Ifalsetest -> int_comp t Ceq i ~imm:(Some 0)
  | Iinttest int_comp_op -> int_comp t int_comp_op i ~imm:None
  | Iinttest_imm (int_comp_op, imm) -> int_comp t int_comp_op i ~imm:(Some imm)
  | Ifloattest (width, float_comp_op) ->
    let typ = T.of_float_width width in
    float_comp t float_comp_op i typ
  | Ioddtest -> odd_test t i
  | Ieventest ->
    let is_odd = odd_test t i in
    emit_ins t (I.binary Xor ~arg1:is_odd ~arg2:(V.of_int ~typ:T.i1 1))

let call ?(tail = false) ?unwind_label t (i : Cfg.terminator Cfg.instruction)
    (op : Cfg.func_call_operation) =
  if not tail then reject_addr_regs_across t i "call";
  let args_begin, args_end =
    (* [Indirect] has the function in i.arg.(0) *)
    match op with
    | Direct _ -> 0, Array.length i.arg
    | Indirect _ -> 1, Array.length i.arg - 1
  in
  let arg_regs = Array.sub i.arg args_begin args_end |> reg_list_for_call in
  let expected_arg_types =
    match op with
    | Direct { sym_name; sym_global = _ } ->
      (match String.Map.find_opt sym_name t.function_arg_types with
      | Some expected_arg_types
        when List.length expected_arg_types = List.length arg_regs ->
        Some expected_arg_types
      | Some _ | None -> None)
    | Indirect _ -> None
  in
  let use_physical_runtime_regs =
    (* Design 1 keeps the domain-state and allocation pointers as SSA values
       and relies on the OxCaml calling convention to place them in x28/x27 at
       call boundaries. The leading SSA arguments/results are the contract that
       lets LLVM see the dependency; the target calling convention is what makes
       it disappear in register allocation.

       Reading physical x28/x27 here is only valid when those registers are
       globally reserved, so keep ordinary calls on the SSA path. *)
    false
  in
  let args =
    prepare_call_args_from_regs ~use_physical_runtime_regs ?expected_arg_types t
      arg_regs
  in
  let res_locs = loc_results_call i.res in
  let res_regs = reg_list_for_call res_locs in
  let res_type =
    (* This will always be [Some] *)
    if tail
    then E.get_res_type (get_fun_info t).emitter
    else Some (make_ret_type (List.map T.of_reg res_regs))
  in
  let func =
    match op with
    | Direct { sym_name; sym_global = _ } ->
      add_referenced_symbol t sym_name;
      LL.Ident.global sym_name
    | Indirect _ -> load_reg_to_temp ~typ:T.ptr t i.arg.(0) |> V.get_ident_exn
  in
  let attrs = gc_attr ~can_call_gc:true t i in
  let live_roots = if tail then [] else load_live_gc_roots_across t i in
  let operand_bundles =
    if tail
    then []
    else
      call_operand_bundles t ~primitive_call:false ~raise_call:false i.dbg
        live_roots
  in
  let res =
    match unwind_label with
    | Some unwind_label when not tail ->
      let normal_label = V.of_label (Cmm.new_label ()) in
      let res =
        emit_ins t
          (I.invoke ~func ~args ~res_type ~attrs ~operand_bundles ~cc:Oxcaml
             ~normal:normal_label ~unwind:unwind_label)
      in
      emit_label t normal_label;
      res
    | Some _ | None ->
      emit_ins t
        (I.call ~func ~args ~res_type ~attrs ~operand_bundles ~cc:Oxcaml
           ~musttail:tail)
  in
  if tail
  then emit_ins_no_res t (I.ret res)
  else (
    refresh_live_gc_roots t live_roots;
    extract_call_res_into_original_regs t res i.res)

let emit_unwind_landingpad t exn_entry =
  ignore (emit_ins t (I.landingpad ~typ:llvm_landingpad_type ~cleanup:true));
  emit_ins_no_res t (I.br exn_entry)

let aarch64_exn_entry_for_active_trap t (i : 'a Cfg.instruction) =
  let fun_info = get_fun_info t in
  match InstructionId.Tbl.find_opt fun_info.active_traps i.id with
  | Some (Some { pushtrap_id; _ }) -> (
    match InstructionId.Tbl.find_opt fun_info.aarch64_trap_blocks pushtrap_id with
    | Some { exn_entry; _ } -> Some exn_entry
    | None -> None)
  | Some None | None -> None

let exn_entry_for_block t (block : Cfg.basic_block) =
  match Target_system.architecture (), block.exn with
  | Target_system.AArch64, _ -> aarch64_exn_entry_for_active_trap t block.terminator
  | ( ( Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
      _ )
    ->
    None

let exn_entry_for_instruction t (i : 'a Cfg.instruction) =
  match Target_system.architecture () with
  | Target_system.AArch64 -> aarch64_exn_entry_for_active_trap t i
  | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
  | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
    None

let emit_unwind_landingpad_after t unwind_label exn_entry =
  match unwind_label, exn_entry with
  | Some unwind_label, Some exn_entry ->
    emit_label t unwind_label;
    emit_unwind_landingpad t exn_entry
  | Some _, None | None, Some _ | None, None -> ()

let unwind_label_and_landingpad_for_exn_entry exn_entry =
  match Target_system.architecture (), exn_entry with
  | Target_system.AArch64, Some exn_entry ->
    (* AArch64 OxCaml trap recovery is modeled directly as an invoke edge to a
       token landingpad recovery entry. Do not insert an intermediate ordinary
       EH landingpad wrapper. *)
    Some exn_entry, None
  | Target_system.AArch64, None -> None, None
  | ( ( Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
      Some exn_entry ) ->
    Some (V.of_label (Cmm.new_label ())), Some exn_entry
  | ( ( Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
      None ) ->
    None, None

let trap_block_type () =
  match Target_system.architecture () with
  | Target_system.X86_64 -> T.(Struct [i64; i64; i64; i64])
  | Target_system.AArch64 -> T.(Struct [i64; i64; i64; i64; i64])
  | Target_system.IA32 | Target_system.ARM | Target_system.POWER
  | Target_system.Z | Target_system.Riscv ->
    fail_msg ~name:"trap_block_type" "unsupported architecture for LLVM backend"

let tailcall_self t (i : Cfg.terminator Cfg.instruction) destination =
  let fun_args = (get_fun_info t).fun_args in
  if Array.length i.arg <> Array.length fun_args
  then
    fail_msg ~name:"tailcall_self"
      "argument count mismatch: terminator has %d args, function has %d args"
      (Array.length i.arg) (Array.length fun_args);
  Array.iter2
    (fun src dst ->
      let value = load_reg_as_expected_arg t src (T.of_reg dst) in
      store_into_reg t dst value)
    i.arg fun_args;
  br_label t destination

let return t (i : Cfg.terminator Cfg.instruction) =
  let result_locations = loc_results_return i.arg in
  let res_type = E.get_res_type (get_fun_info t).emitter |> Option.get in
  (* Note: type information is not propagated to the backend for functions that
     never return, like [raise], so there might be type mismatches. If that is
     the case, this point is unreachable. *)
  let res_type_elems =
    Option.bind (T.extract_struct res_type [1]) T.get_struct_elements
    |> Option.get
  in
  let returned_count =
    Array.fold_left
      (fun count loc -> if Reg.is_domainstate loc then count else count + 1)
      0 result_locations
  in
  if List.length res_type_elems <> returned_count
  then emit_ins_no_res t I.unreachable
  else
    let res_type_elems = ref res_type_elems in
    let res_values = ref [] in
    Array.iter2
      (fun original_reg loc ->
        if Reg.is_domainstate loc
        then
          let value =
            load_reg_to_temp ~typ:(T.of_reg original_reg) t original_reg
          in
          let ptr = domainstate_addr_for_location t loc in
          emit_ins_no_res t (I.store ~ptr ~to_store:value)
        else
          match !res_type_elems with
          | typ :: rest ->
            res_type_elems := rest;
            res_values := load_reg_to_temp ~typ t original_reg :: !res_values
          | [] -> fail_msg ~name:"return" "not enough return types")
      i.arg result_locations;
    let res_values = List.rev !res_values in
    let res = assemble_return t res_type res_values in
    emit_ins_no_res t (I.ret res)

let extcall_arg_type (ty_arg : Cmm.exttype) (_arg_reg : Reg.t) =
  match ty_arg with
  | XInt -> T.i64
  | XValue | XAddr -> T.val_ptr
  | XInt64 | XInt32 | XInt16 | XInt8 -> T.i64
  | XFloat -> T.double
  | XFloat32 -> T.float
  | XVec128 -> T.vec128
  | XVec256 -> T.vec256
  | XVec512 -> T.vec512

let extcall_arg_types ty_args arg_regs =
  let ty_args =
    match ty_args with
    | [] -> List.map (fun _ -> Cmm.XInt) arg_regs
    | _ -> ty_args
  in
  if List.compare_lengths ty_args arg_regs <> 0
  then fail_msg ~name:"extcall" "external call argument arity mismatch";
  List.map2 extcall_arg_type ty_args arg_regs

let supports_inline_string_compare () =
  match Target_system.architecture (), Arch.size_addr with
  | Target_system.AArch64, 8 -> true
  | ( ( Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
      _ )
  | Target_system.AArch64, _ ->
    false

let header_wosize_shift = 10

let header_wosize_mask =
  let header_bits = Arch.size_addr * 8 in
  let header_tag_bits = 8 in
  let header_color_bits = 2 in
  let header_wosize_bits =
    header_bits - header_tag_bits - header_color_bits
    - Config.reserved_header_bits
  in
  Nativeint.(
    shift_left (sub (shift_left 1n header_wosize_bits) 1n)
      header_wosize_shift)

let emit_ocaml_string_length t s =
  let s = cast t s T.val_ptr in
  let header_ptr = do_offset t s T.val_ptr (-Arch.size_addr) in
  let header =
    emit_ins t (I.load_atomic ~ordering:Monotonic ~ptr:header_ptr ~typ:T.i64)
  in
  let wosize_masked =
    emit_ins t
      (I.binary And ~arg1:header ~arg2:(V.of_nativeint header_wosize_mask))
  in
  let wosize =
    emit_ins t
      (I.binary Lshr ~arg1:wosize_masked
         ~arg2:(V.of_int header_wosize_shift))
  in
  let bosize =
    emit_ins t (I.binary Shl ~arg1:wosize ~arg2:(V.of_int 3))
  in
  let offset_index =
    emit_ins t (I.binary Sub ~arg1:bosize ~arg2:(V.of_int 1))
  in
  let offset_ptr =
    emit_ins t
      (I.getelementptr ~base_type:T.i8 ~base_ptr:s ~indices:[offset_index])
  in
  let offset_byte =
    emit_ins t (I.load_with_align ~align:1 ~ptr:offset_ptr ~typ:T.i8)
  in
  let offset = emit_ins t (I.convert Zext ~arg:offset_byte ~to_:T.i64) in
  emit_ins t (I.binary Sub ~arg1:offset_index ~arg2:offset)

let emit_min_u64 t x y =
  let x_lt_y = emit_ins t (I.icmp Iult ~arg1:x ~arg2:y) in
  emit_ins t (I.select ~cond:x_lt_y ~ifso:x ~ifnot:y)

let emit_string_compare_word t s1 s2 ~offset ~count =
  let load_word s =
    let ptr = do_offset t (cast t s T.val_ptr) T.val_ptr offset in
    let word = emit_ins t (I.load_with_align ~align:8 ~ptr ~typ:T.i64) in
    call_llvm_intrinsic t "bswap.i64" [word] T.i64
  in
  let shift_bytes =
    emit_ins t (I.binary Sub ~arg1:(V.of_int 8) ~arg2:count)
  in
  let shift_bits =
    emit_ins t (I.binary Shl ~arg1:shift_bytes ~arg2:(V.of_int 3))
  in
  let mask =
    emit_ins t (I.binary Shl ~arg1:(V.of_int (-1)) ~arg2:shift_bits)
  in
  let word1 = load_word s1 in
  let word2 = load_word s2 in
  let word1_masked = emit_ins t (I.binary And ~arg1:word1 ~arg2:mask) in
  let word2_masked = emit_ins t (I.binary And ~arg1:word2 ~arg2:mask) in
  let neq = emit_ins t (I.icmp Ine ~arg1:word1_masked ~arg2:word2_masked) in
  let lt = emit_ins t (I.icmp Iult ~arg1:word1_masked ~arg2:word2_masked) in
  neq, lt

let emit_string_compare_result_value t ~lt =
  emit_ins t (I.select ~cond:lt ~ifso:(V.of_int (-1)) ~ifnot:(V.of_int 3))

let emit_string_compare_result t res_reg ~lt ~done_label =
  let result = emit_string_compare_result_value t ~lt in
  store_into_reg t res_reg result;
  emit_ins_no_res t (I.br done_label)

let emit_string_compare_length_tiebreak_value t ~len1 ~len2 =
  let len1_lt_len2 = emit_ins t (I.icmp Iult ~arg1:len1 ~arg2:len2) in
  let len1_gt_len2 = emit_ins t (I.icmp Iugt ~arg1:len1 ~arg2:len2) in
  let gt_or_eq =
    emit_ins t
      (I.select ~cond:len1_gt_len2 ~ifso:(V.of_int 3) ~ifnot:(V.of_int 1))
  in
  emit_ins t
    (I.select ~cond:len1_lt_len2 ~ifso:(V.of_int (-1)) ~ifnot:gt_or_eq)

let emit_string_compare_length_tiebreak t res_reg ~len1 ~len2 ~done_label =
  let result = emit_string_compare_length_tiebreak_value t ~len1 ~len2 in
  store_into_reg t res_reg result;
  emit_ins_no_res t (I.br done_label)

let emit_memcmp_direct_call t s1 s2 ~offset ~count =
  add_referenced_symbol t "memcmp";
  let s1_arg = do_offset t (cast t s1 T.ptr) T.ptr offset in
  let s2_arg = do_offset t (cast t s2 T.ptr) T.ptr offset in
  let args = prepare_call_args t [s1_arg; s2_arg; count] in
  let direct_res_types = List.map (fun _ -> T.i64) runtime_regs @ [T.i32] in
  let c_res =
    emit_ins t
      (I.call ~func:(LL.Ident.global "memcmp") ~args
         ~res_type:(Some (T.Struct direct_res_types))
         ~attrs:[LL.Fn_attr.Gc_leaf_function] ~operand_bundles:[]
         ~cc:Oxcaml_c_direct_call ~musttail:false)
  in
  let runtime_values =
    extract_struct t c_res (List.init (List.length runtime_regs) (fun i -> [i]))
  in
  List.iter2
    (fun ptr to_store -> emit_ins_no_res t (I.store ~ptr ~to_store))
    runtime_regs runtime_values;
  match extract_struct t c_res [[List.length runtime_regs]] with
  | [memcmp_result] -> memcmp_result
  | _ -> Misc.fatal_error "unexpected memcmp direct call result shape"

let emit_string_compare_memcmp_suffix t res_reg s1 s2 ~offset ~count ~len1 ~len2
    ~done_label =
  let memcmp_result = emit_memcmp_direct_call t s1 s2 ~offset ~count in
  let memcmp_lt =
    emit_ins t
      (I.icmp Islt ~arg1:memcmp_result ~arg2:(V.of_int ~typ:T.i32 0))
  in
  let memcmp_neq =
    emit_ins t
      (I.icmp Ine ~arg1:memcmp_result ~arg2:(V.of_int ~typ:T.i32 0))
  in
  let neq_result = emit_string_compare_result_value t ~lt:memcmp_lt in
  let eq_result = emit_string_compare_length_tiebreak_value t ~len1 ~len2 in
  let result =
    emit_ins t (I.select ~cond:memcmp_neq ~ifso:neq_result ~ifnot:eq_result)
  in
  store_into_reg t res_reg result;
  emit_ins_no_res t (I.br done_label)

let emit_string_equal_memcmp_suffix t res_reg s1 s2 ~offset ~count ~done_label =
  let memcmp_result = emit_memcmp_direct_call t s1 s2 ~offset ~count in
  let memcmp_eq =
    emit_ins t
      (I.icmp Ieq ~arg1:memcmp_result ~arg2:(V.of_int ~typ:T.i32 0))
  in
  let result =
    emit_ins t (I.select ~cond:memcmp_eq ~ifso:(V.of_int 3) ~ifnot:(V.of_int 1))
  in
  store_into_reg t res_reg result;
  emit_ins_no_res t (I.br done_label)

let string_compare_symbol = function
  | "caml_string_compare" | "caml_bytes_compare" -> true
  | _ -> false

let string_equal_symbol = function
  | "caml_string_equal" | "caml_bytes_equal" -> true
  | _ -> false

let supports_inline_obj_tag () =
  match Target_system.architecture (), Arch.size_addr with
  | Target_system.AArch64, 8 -> not Config.tsan
  | ( ( Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
      _ )
  | Target_system.AArch64, _ ->
    false

let maybe_emit_specialized_obj_tag_extcall t ~func_symbol ~alloc ~stack_ofs
    arg_regs res_regs =
  match arg_regs, res_regs with
  | [arg], [res_reg]
    when (not alloc) && stack_ofs = 0
         && String.equal func_symbol "caml_obj_tag"
         && supports_inline_obj_tag ()
         && (T.equal (T.of_reg res_reg) T.i64
            || T.equal (T.of_reg res_reg) T.val_ptr) ->
    add_referenced_symbol t func_symbol;
    let done_label = V.of_label (Cmm.new_label ()) in
    let null_label = V.of_label (Cmm.new_label ()) in
    let not_null_label = V.of_label (Cmm.new_label ()) in
    let int_label = V.of_label (Cmm.new_label ()) in
    let not_int_label = V.of_label (Cmm.new_label ()) in
    let unaligned_label = V.of_label (Cmm.new_label ()) in
    let block_label = V.of_label (Cmm.new_label ()) in
    let arg_raw = cast t (load_reg_to_temp t arg) T.i64 in
    let is_null = emit_ins t (I.icmp Ieq ~arg1:arg_raw ~arg2:(V.of_int 0)) in
    emit_ins_no_res t
      (I.br_cond ~cond:is_null ~ifso:null_label ~ifnot:not_null_label);
    emit_label t null_label;
    store_into_reg t res_reg (V.of_int 2021);
    emit_ins_no_res t (I.br done_label);
    emit_label t not_null_label;
    let low_bit = emit_ins t (I.binary And ~arg1:arg_raw ~arg2:(V.of_int 1)) in
    let is_int = emit_ins t (I.icmp Ine ~arg1:low_bit ~arg2:(V.of_int 0)) in
    emit_ins_no_res t
      (I.br_cond ~cond:is_int ~ifso:int_label ~ifnot:not_int_label);
    emit_label t int_label;
    store_into_reg t res_reg (V.of_int 2001);
    emit_ins_no_res t (I.br done_label);
    emit_label t not_int_label;
    let alignment_bits =
      emit_ins t
        (I.binary And ~arg1:arg_raw ~arg2:(V.of_int (Arch.size_addr - 1)))
    in
    let is_unaligned =
      emit_ins t (I.icmp Ine ~arg1:alignment_bits ~arg2:(V.of_int 0))
    in
    emit_ins_no_res t
      (I.br_cond ~cond:is_unaligned ~ifso:unaligned_label ~ifnot:block_label);
    emit_label t unaligned_label;
    store_into_reg t res_reg (V.of_int 2005);
    emit_ins_no_res t (I.br done_label);
    emit_label t block_label;
    let block = cast t arg_raw T.ptr in
    let header_ptr = do_offset t block T.ptr (-Arch.size_addr) in
    let header =
      emit_ins t (I.load_atomic ~ordering:Acquire ~ptr:header_ptr ~typ:T.i64)
    in
    let tag = emit_ins t (I.binary And ~arg1:header ~arg2:(V.of_int 255)) in
    let shifted = emit_ins t (I.binary Shl ~arg1:tag ~arg2:(V.of_int 1)) in
    let result = emit_ins t (I.binary Or ~arg1:shifted ~arg2:(V.of_int 1)) in
    store_into_reg t res_reg result;
    emit_ins_no_res t (I.br done_label);
    emit_label t done_label;
    true
  | _ -> false

let maybe_emit_specialized_string_compare_extcall t ~func_symbol ~alloc
    ~stack_ofs arg_regs res_regs _emit_fallback =
  match arg_regs, res_regs with
  | [arg1; arg2], [res_reg]
    when (not alloc)
         && stack_ofs = 0
         && string_compare_symbol func_symbol
         && supports_inline_string_compare ()
         && (T.equal (T.of_reg res_reg) T.i64
            || T.equal (T.of_reg res_reg) T.val_ptr) ->
    add_referenced_symbol t func_symbol;
    let done_label = V.of_label (Cmm.new_label ()) in
    let pointer_equal_label = V.of_label (Cmm.new_label ()) in
    let not_pointer_equal_label = V.of_label (Cmm.new_label ()) in
    let long_fallback_label = V.of_label (Cmm.new_label ()) in
    let long_word1_mismatch_label = V.of_label (Cmm.new_label ()) in
    let long_word1_equal_label = V.of_label (Cmm.new_label ()) in
    let short_compare_label = V.of_label (Cmm.new_label ()) in
    let length_tiebreak_label = V.of_label (Cmm.new_label ()) in
    let word1_label = V.of_label (Cmm.new_label ()) in
    let word1_mismatch_label = V.of_label (Cmm.new_label ()) in
    let word1_equal_label = V.of_label (Cmm.new_label ()) in
    let word2_label = V.of_label (Cmm.new_label ()) in
    let word2_mismatch_label = V.of_label (Cmm.new_label ()) in
    let s1 = load_reg_to_temp ~typ:T.val_ptr t arg1 in
    let s2 = load_reg_to_temp ~typ:T.val_ptr t arg2 in
    let s1_int = cast t s1 T.i64 in
    let s2_int = cast t s2 T.i64 in
    let pointer_equal = emit_ins t (I.icmp Ieq ~arg1:s1_int ~arg2:s2_int) in
    emit_ins_no_res t
      (I.br_cond ~cond:pointer_equal ~ifso:pointer_equal_label
         ~ifnot:not_pointer_equal_label);
    emit_label t pointer_equal_label;
    store_into_reg t res_reg (V.of_int 1);
    emit_ins_no_res t (I.br done_label);
    emit_label t not_pointer_equal_label;
    let len1 = emit_ocaml_string_length t s1 in
    let len2 = emit_ocaml_string_length t s2 in
    let min_len = emit_min_u64 t len1 len2 in
    let min_len_gt_16 =
      emit_ins t (I.icmp Iugt ~arg1:min_len ~arg2:(V.of_int 16))
    in
    emit_ins_no_res t
      (I.br_cond ~cond:min_len_gt_16 ~ifso:long_fallback_label
         ~ifnot:short_compare_label);
    emit_label t long_fallback_label;
    let word1_neq, word1_lt =
      emit_string_compare_word t s1 s2 ~offset:0 ~count:(V.of_int 8)
    in
    emit_ins_no_res t
      (I.br_cond ~cond:word1_neq ~ifso:long_word1_mismatch_label
         ~ifnot:long_word1_equal_label);
    emit_label t long_word1_mismatch_label;
    emit_string_compare_result t res_reg ~lt:word1_lt ~done_label;
    emit_label t long_word1_equal_label;
    let suffix_len =
      emit_ins t (I.binary Sub ~arg1:min_len ~arg2:(V.of_int 8))
    in
    emit_string_compare_memcmp_suffix t res_reg s1 s2 ~offset:8
      ~count:suffix_len ~len1 ~len2 ~done_label;
    emit_label t short_compare_label;
    let min_len_is_zero =
      emit_ins t (I.icmp Ieq ~arg1:min_len ~arg2:(V.of_int 0))
    in
    emit_ins_no_res t
      (I.br_cond ~cond:min_len_is_zero ~ifso:length_tiebreak_label
         ~ifnot:word1_label);
    emit_label t word1_label;
    let min_len_gt_8 =
      emit_ins t (I.icmp Iugt ~arg1:min_len ~arg2:(V.of_int 8))
    in
    let word1_count =
      emit_ins t
        (I.select ~cond:min_len_gt_8 ~ifso:(V.of_int 8) ~ifnot:min_len)
    in
    let word1_neq, word1_lt =
      emit_string_compare_word t s1 s2 ~offset:0 ~count:word1_count
    in
    emit_ins_no_res t
      (I.br_cond ~cond:word1_neq ~ifso:word1_mismatch_label
         ~ifnot:word1_equal_label);
    emit_label t word1_mismatch_label;
    emit_string_compare_result t res_reg ~lt:word1_lt ~done_label;
    emit_label t word1_equal_label;
    emit_ins_no_res t
      (I.br_cond ~cond:min_len_gt_8 ~ifso:word2_label
         ~ifnot:length_tiebreak_label);
    emit_label t word2_label;
    let word2_count =
      emit_ins t (I.binary Sub ~arg1:min_len ~arg2:(V.of_int 8))
    in
    let word2_neq, word2_lt =
      emit_string_compare_word t s1 s2 ~offset:8 ~count:word2_count
    in
    emit_ins_no_res t
      (I.br_cond ~cond:word2_neq ~ifso:word2_mismatch_label
         ~ifnot:length_tiebreak_label);
    emit_label t word2_mismatch_label;
    emit_string_compare_result t res_reg ~lt:word2_lt ~done_label;
    emit_label t length_tiebreak_label;
    emit_string_compare_length_tiebreak t res_reg ~len1 ~len2 ~done_label;
    emit_label t done_label;
    true
  | _ -> false

let maybe_emit_specialized_string_equal_extcall t ~func_symbol ~alloc ~stack_ofs
    arg_regs res_regs _emit_fallback =
  match arg_regs, res_regs with
  | [arg1; arg2], [res_reg]
    when (not alloc)
         && stack_ofs = 0
         && string_equal_symbol func_symbol
         && supports_inline_string_compare ()
         && (T.equal (T.of_reg res_reg) T.i64
            || T.equal (T.of_reg res_reg) T.val_ptr) ->
    add_referenced_symbol t func_symbol;
    let done_label = V.of_label (Cmm.new_label ()) in
    let true_label = V.of_label (Cmm.new_label ()) in
    let not_pointer_equal_label = V.of_label (Cmm.new_label ()) in
    let false_label = V.of_label (Cmm.new_label ()) in
    let length_equal_label = V.of_label (Cmm.new_label ()) in
    let memcmp_compare_label = V.of_label (Cmm.new_label ()) in
    let memcmp_word1_equal_label = V.of_label (Cmm.new_label ()) in
    let short_compare_label = V.of_label (Cmm.new_label ()) in
    let word1_label = V.of_label (Cmm.new_label ()) in
    let word1_equal_label = V.of_label (Cmm.new_label ()) in
    let word2_label = V.of_label (Cmm.new_label ()) in
    let word2_equal_label = V.of_label (Cmm.new_label ()) in
    let word3_label = V.of_label (Cmm.new_label ()) in
    let s1 = load_reg_to_temp ~typ:T.val_ptr t arg1 in
    let s2 = load_reg_to_temp ~typ:T.val_ptr t arg2 in
    let s1_int = cast t s1 T.i64 in
    let s2_int = cast t s2 T.i64 in
    let pointer_equal = emit_ins t (I.icmp Ieq ~arg1:s1_int ~arg2:s2_int) in
    emit_ins_no_res t
      (I.br_cond ~cond:pointer_equal ~ifso:true_label
         ~ifnot:not_pointer_equal_label);
    emit_label t not_pointer_equal_label;
    let len1 = emit_ocaml_string_length t s1 in
    let len2 = emit_ocaml_string_length t s2 in
    let lengths_equal = emit_ins t (I.icmp Ieq ~arg1:len1 ~arg2:len2) in
    emit_ins_no_res t
      (I.br_cond ~cond:lengths_equal ~ifso:length_equal_label
         ~ifnot:false_label);
    emit_label t length_equal_label;
    let len_is_zero = emit_ins t (I.icmp Ieq ~arg1:len1 ~arg2:(V.of_int 0)) in
    emit_ins_no_res t
      (I.br_cond ~cond:len_is_zero ~ifso:true_label
         ~ifnot:short_compare_label);
    emit_label t short_compare_label;
    let len_gt_24 = emit_ins t (I.icmp Iugt ~arg1:len1 ~arg2:(V.of_int 24)) in
    emit_ins_no_res t
      (I.br_cond ~cond:len_gt_24 ~ifso:memcmp_compare_label
         ~ifnot:word1_label);
    emit_label t memcmp_compare_label;
    let long_word1_neq, _long_word1_lt =
      emit_string_compare_word t s1 s2 ~offset:0 ~count:(V.of_int 8)
    in
    emit_ins_no_res t
      (I.br_cond ~cond:long_word1_neq ~ifso:false_label
         ~ifnot:memcmp_word1_equal_label);
    emit_label t memcmp_word1_equal_label;
    let suffix_len =
      emit_ins t (I.binary Sub ~arg1:len1 ~arg2:(V.of_int 8))
    in
    emit_string_equal_memcmp_suffix t res_reg s1 s2 ~offset:8 ~count:suffix_len
      ~done_label;
    emit_label t word1_label;
    let len_gt_8 = emit_ins t (I.icmp Iugt ~arg1:len1 ~arg2:(V.of_int 8)) in
    let word1_count =
      emit_ins t
        (I.select ~cond:len_gt_8 ~ifso:(V.of_int 8) ~ifnot:len1)
    in
    let word1_neq, _word1_lt =
      emit_string_compare_word t s1 s2 ~offset:0 ~count:word1_count
    in
    emit_ins_no_res t
      (I.br_cond ~cond:word1_neq ~ifso:false_label
         ~ifnot:word1_equal_label);
    emit_label t word1_equal_label;
    emit_ins_no_res t
      (I.br_cond ~cond:len_gt_8 ~ifso:word2_label ~ifnot:true_label);
    emit_label t word2_label;
    let len_gt_16 = emit_ins t (I.icmp Iugt ~arg1:len1 ~arg2:(V.of_int 16)) in
    let word2_tail_count =
      emit_ins t (I.binary Sub ~arg1:len1 ~arg2:(V.of_int 8))
    in
    let word2_count =
      emit_ins t
        (I.select ~cond:len_gt_16 ~ifso:(V.of_int 8)
           ~ifnot:word2_tail_count)
    in
    let word2_neq, _word2_lt =
      emit_string_compare_word t s1 s2 ~offset:8 ~count:word2_count
    in
    emit_ins_no_res t
      (I.br_cond ~cond:word2_neq ~ifso:false_label
         ~ifnot:word2_equal_label);
    emit_label t word2_equal_label;
    emit_ins_no_res t
      (I.br_cond ~cond:len_gt_16 ~ifso:word3_label ~ifnot:true_label);
    emit_label t word3_label;
    let word3_count =
      emit_ins t (I.binary Sub ~arg1:len1 ~arg2:(V.of_int 16))
    in
    let word3_neq, _word3_lt =
      emit_string_compare_word t s1 s2 ~offset:16 ~count:word3_count
    in
    emit_ins_no_res t
      (I.br_cond ~cond:word3_neq ~ifso:false_label ~ifnot:true_label);
    emit_label t false_label;
    store_into_reg t res_reg (V.of_int 1);
    emit_ins_no_res t (I.br done_label);
    emit_label t true_label;
    store_into_reg t res_reg (V.of_int 3);
    emit_ins_no_res t (I.br done_label);
    emit_label t done_label;
    true
  | _ -> false

let supports_inline_caml_modify () =
  match Target_system.architecture (), Arch.size_addr with
  | Target_system.AArch64, 8 ->
    (* Keep the runtime helper as the source of truth until the inline barrier
       is revalidated against minor-heap remembered-set tests. *)
    false
  | ( ( Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
      _ )
  | Target_system.AArch64, _ ->
    false

let load_runtime_i64_global t name =
  add_referenced_symbol t name;
  emit_ins t (I.load ~ptr:(V.of_symbol name) ~typ:T.i64)

let load_runtime_i32_global t name =
  add_referenced_symbol t name;
  emit_ins t (I.load ~ptr:(V.of_symbol name) ~typ:T.i32)

let emit_i1_not t v =
  emit_ins t (I.icmp Ieq ~arg1:v ~arg2:(V.of_int ~typ:T.i1 0))

let emit_i1_and t a b = emit_ins t (I.binary And ~arg1:a ~arg2:b)

let emit_i1_or t a b = emit_ins t (I.binary Or ~arg1:a ~arg2:b)

let emit_is_block_raw t raw =
  let low_bit = emit_ins t (I.binary And ~arg1:raw ~arg2:(V.of_int 1)) in
  let low_bit_zero = emit_ins t (I.icmp Ieq ~arg1:low_bit ~arg2:(V.of_int 0)) in
  let not_null = emit_ins t (I.icmp Ine ~arg1:raw ~arg2:(V.of_int 0)) in
  emit_i1_and t low_bit_zero not_null

let emit_is_young_raw t raw =
  let young_start = load_runtime_i64_global t "caml_minor_heaps_start" in
  let young_end = load_runtime_i64_global t "caml_minor_heaps_end" in
  let above_start = emit_ins t (I.icmp Iugt ~arg1:raw ~arg2:young_start) in
  let below_end = emit_ins t (I.icmp Iult ~arg1:raw ~arg2:young_end) in
  emit_i1_and t above_start below_end

let emit_is_block_and_young_raw t raw =
  let is_block = emit_is_block_raw t raw in
  let is_young = emit_is_young_raw t raw in
  emit_i1_and t is_block is_young

let maybe_emit_specialized_caml_modify_extcall t (i : Cfg.terminator Cfg.instruction)
    ~func_symbol ~alloc ~stack_ofs arg_regs res_regs emit_fallback =
  match arg_regs, res_regs with
  | [fp_reg; new_val_reg], []
    when (not alloc)
         && stack_ofs = 0
         && String.equal func_symbol "caml_modify"
         && supports_inline_caml_modify () ->
    let done_label = V.of_label (Cmm.new_label ()) in
    let fallback_label = V.of_label (Cmm.new_label ()) in
    let inline_label = V.of_label (Cmm.new_label ()) in
    let slow_barrier_label = V.of_label (Cmm.new_label ()) in
    let store_label = V.of_label (Cmm.new_label ()) in
    let profile_enabled =
      load_runtime_i32_global t "caml_llvm_helper_profile_enabled"
    in
    let profile_enabled =
      emit_ins t (I.icmp Ine ~arg1:profile_enabled ~arg2:(V.of_int ~typ:T.i32 0))
    in
    let profile_enabled_expect =
      call_llvm_intrinsic t "expect.i1"
        [profile_enabled; V.of_int ~typ:T.i1 0] T.i1
    in
    emit_ins_no_res t
      (I.br_cond ~cond:profile_enabled_expect ~ifso:fallback_label
         ~ifnot:inline_label);
    emit_label t fallback_label;
    emit_fallback ();
    emit_ins_no_res t (I.br done_label);
    emit_label t inline_label;
    let fp = load_reg_to_temp ~typ:T.val_ptr t fp_reg in
    let fp_raw = cast t fp T.i64 in
    let new_val = load_reg_to_temp ~typ:T.val_ptr t new_val_reg in
    let new_val_raw = cast t new_val T.i64 in
    let old_val_raw = emit_ins t (I.load ~ptr:fp ~typ:T.i64) in
    let old_val = cast t old_val_raw T.val_ptr in
    let dest_is_young = emit_is_young_raw t fp_raw in
    let dest_is_old = emit_i1_not t dest_is_young in
    let old_is_block = emit_is_block_raw t old_val_raw in
    let old_is_young = emit_is_block_and_young_raw t old_val_raw in
    let old_is_not_young = emit_i1_not t old_is_young in
    let old_is_old_block = emit_i1_and t old_is_block old_is_not_young in
    let new_is_young = emit_is_block_and_young_raw t new_val_raw in
    let gc_phase = load_runtime_i32_global t "caml_gc_phase" in
    let marking_started =
      emit_ins t (I.icmp Ine ~arg1:gc_phase ~arg2:(V.of_int ~typ:T.i32 0))
    in
    let old_needs_darken = emit_i1_and t old_is_old_block marking_started in
    let slow_reason = emit_i1_or t new_is_young old_needs_darken in
    let dest_slow_reason = emit_i1_and t dest_is_old slow_reason in
    let need_slow = emit_i1_and t dest_slow_reason old_is_not_young in
    let need_slow_expect =
      call_llvm_intrinsic t "expect.i1" [need_slow; V.of_int ~typ:T.i1 0] T.i1
    in
    emit_ins_no_res t
      (I.br_cond ~cond:need_slow_expect ~ifso:slow_barrier_label
         ~ifnot:store_label);
    emit_label t slow_barrier_label;
    let wrapper_symbol =
      add_c_call_wrapper t "caml_modify_slow_barrier"
        ~args:[T.val_ptr; T.val_ptr; T.val_ptr] ~res:[]
    in
    add_referenced_symbol t "caml_modify_slow_barrier";
    call_simple
      ~attrs:(gc_attr ~can_call_gc:false t i @ [LL.Fn_attr.Cold])
      ~dbg:i.dbg ~primitive_call:true ~live_roots:[]
      ~cc:Oxcaml t wrapper_symbol [fp; old_val; new_val] []
    |> ignore;
    emit_ins_no_res t (I.br store_label);
    emit_label t store_label;
    emit_ins_no_res t (I.fence Acquire);
    emit_ins_no_res t
      (I.store_atomic ~ordering:Release ~ptr:fp ~to_store:new_val_raw);
    emit_ins_no_res t (I.br done_label);
    emit_label t done_label;
    true
  | _ -> false

let extcall ?unwind_label t (i : Cfg.terminator Cfg.instruction) ~func_symbol
    ~alloc ~ty_args ~stack_ofs ~stack_align =
  if alloc || stack_ofs > 0 then reject_addr_regs_across t i "extcall";
  let func_ptr =
    emit_ins t (I.convert Ptrtoint ~arg:(V.of_symbol func_symbol) ~to_:T.i64)
  in
  let make_ocaml_c_call ~cc caml_c_call_symbol args res_types =
    add_referenced_symbol t caml_c_call_symbol;
    add_referenced_symbol t func_symbol;
    call_simple
      ~attrs:(gc_attr ~can_call_gc:true t i)
      ~dbg:i.dbg ~primitive_call:true
      ~live_roots:(load_live_gc_roots_across t i)
      ?unwind_label ~cc t caml_c_call_symbol args res_types
  in
  let call_func arg_regs arg_types res_types =
    if stack_ofs > 0
    then (
      (* We handle stack arguments manually as opposed to making LLVM's C
         calling conventions handle it. The reason is twofold:

         * We want to be able to get pointers delimiting the arguments to pass
         to [caml_c_call_stack_args] so that it properly transfer them. Doing
         this via reading the stack pointer is not reliable.

         * We want LLVM to know the frame size statically for frametable
         emission. We make sure that any additional tampering with the stack is
         done by us, not by LLVM, since we can keep track of it ourselves and
         give that information to the frametable printer. *)
      let opaque_stack_ofs =
        emit_ins t
          (I.inline_asm ~asm:"" ~constraints:"=r,0"
             ~args:[V.of_int stack_ofs]
             ~res_type:(Some T.i64) ~sideeffect:true)
      in
      (* Save stack + make space for stack args *)
      let stacksave_ptr = call_llvm_intrinsic t "stacksave" [] T.ptr in
      let stack_args_alloca =
        emit_ins t (I.alloca ~count:opaque_stack_ofs T.i8)
      in
      (* Determine which ones to pass directly and which ones via stack *)
      let stack_args, direct_args =
        List.partition
          (fun ((reg : Reg.t), _) ->
            match reg.loc with
            | Stack (Outgoing _) -> true
            | Stack (Local _ | Incoming _ | Domainstate _) | Unknown | Reg _ ->
              false)
          (List.combine arg_regs arg_types)
      in
      (* Fill up the slots *)
      List.iter2
        (fun (reg : Reg.t) typ ->
          match reg.loc with
          | Stack (Outgoing n) ->
            let temp = load_reg_to_temp ~typ t reg in
            let slot =
              emit_ins t
                (I.getelementptr ~base_type:T.i8 ~base_ptr:stack_args_alloca
                   ~indices:[V.of_int n])
            in
            emit_ins_no_res t (I.store ~ptr:slot ~to_store:temp)
          | Stack (Local _ | Incoming _ | Domainstate _) | Unknown | Reg _ ->
            assert false)
        (List.map fst stack_args) (List.map snd stack_args);
      (* Prepare direct args + special values for [caml_c_call_stack_args] *)
      let stack_args_begin =
        emit_ins t (I.convert Ptrtoint ~arg:stack_args_alloca ~to_:T.i64)
      in
      let stack_args_end =
        emit_ins t
          (I.binary Add ~arg1:stack_args_begin ~arg2:(V.of_int stack_ofs))
      in
      let args =
        [func_ptr; stack_args_begin; stack_args_end]
        @ List.map (fun (reg, typ) -> load_reg_to_temp ~typ t reg) direct_args
      in
      let caml_c_call_stack_args =
        "caml_c_call_stack_args"
        ^
        match (stack_align : Cmm.stack_align) with
        | Align_16 -> ""
        | Align_32 -> "_avx"
        | Align_64 -> "_avx512"
      in
      let res_vals =
        make_ocaml_c_call ~cc:Oxcaml_c_call_stack_args caml_c_call_stack_args
          args res_types
      in
      call_llvm_intrinsic_no_res t "stackrestore" [stacksave_ptr];
      res_vals)
    else if alloc
    then
      let args =
        [func_ptr]
        @ List.map2
            (fun reg typ -> load_reg_to_temp ~typ t reg)
            arg_regs arg_types
      in
      make_ocaml_c_call ~cc:Oxcaml_c_call "caml_c_call" args res_types
    else
      let args =
        List.map2
          (fun reg typ -> load_reg_to_temp ~typ t reg)
          arg_regs arg_types
      in
      if Target_system.architecture () = Target_system.AArch64
      then (
        add_referenced_symbol t func_symbol;
        let args = prepare_call_args t args in
        let direct_res_types = List.map (fun _ -> T.i64) runtime_regs @ res_types in
        let c_res =
          (* Noalloc C calls follow the native backend model: they are ordinary
             C ABI calls that cannot allocate or raise, so any active trap does
             not create an exceptional edge from this call.

             The leading runtime-register operands/results are not C-level
             arguments/results. The [oxcaml_c_directcc] convention assigns them
             to x28/x27 before the ordinary C arguments and returns them from
             x28/x27 before the ordinary C result registers. AAPCS preserves
             x28/x27, so this models the domain-state/allocation-pointer
             dependency that the stack-switch pseudo needs while still
             generating a plain direct C call. *)
          ignore unwind_label;
          emit_ins t
            (I.call ~func:(LL.Ident.global func_symbol) ~args
               ~res_type:(Some (T.Struct direct_res_types))
               ~attrs:(gc_attr ~can_call_gc:false t i)
               ~operand_bundles:[] ~cc:Oxcaml_c_direct_call ~musttail:false)
        in
        let runtime_values =
          extract_struct t c_res
            (List.init (List.length runtime_regs) (fun i -> [i]))
        in
        List.iter2
          (fun ptr to_store -> emit_ins_no_res t (I.store ~ptr ~to_store))
          runtime_regs runtime_values;
        extract_struct t c_res
          (List.init (List.length res_types) (fun i ->
             [List.length runtime_regs + i])))
      else
        (* Wrap C calls to avoid reloading from the stack after overwriting the
           stack pointer. *)
        let wrapper_symbol =
          add_c_call_wrapper t func_symbol ~args:(List.map V.get_type args)
            ~res:res_types
        in
        add_referenced_symbol t func_symbol;
        call_simple
          ~attrs:(gc_attr ~can_call_gc:true t i)
          ~dbg:i.dbg ~primitive_call:true
          ~live_roots:(load_live_gc_roots_across t i)
          ?unwind_label ~cc:Oxcaml t wrapper_symbol args res_types
  in
  let arg_regs = reg_list_for_call i.arg in
  let arg_types = extcall_arg_types ty_args arg_regs in
  let res_regs = reg_list_for_call i.res in
  let res_types = List.map T.of_reg res_regs in
  let emit_fallback () =
    let res_values = call_func arg_regs arg_types res_types in
    List.iter2 (store_into_reg t) res_regs res_values
  in
  if
    not
      (maybe_emit_specialized_obj_tag_extcall t ~func_symbol ~alloc ~stack_ofs
         arg_regs res_regs)
  then
    if
      not
        (maybe_emit_specialized_string_compare_extcall t ~func_symbol ~alloc
           ~stack_ofs arg_regs res_regs emit_fallback)
    then
      if
        not
          (maybe_emit_specialized_caml_modify_extcall t i ~func_symbol ~alloc
             ~stack_ofs arg_regs res_regs emit_fallback)
      then
        if
          not
            (maybe_emit_specialized_string_equal_extcall t ~func_symbol ~alloc
               ~stack_ofs arg_regs res_regs emit_fallback)
        then emit_fallback ()

let raise_ t ~(exn_handler : Label.t option)
    (i : Cfg.terminator Cfg.instruction) (raise_kind : Lambda.raise_kind) =
  let unwind_label_for_active_handler () =
    match Target_system.architecture () with
    | Target_system.AArch64 -> aarch64_exn_entry_for_active_trap t i
    | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
    | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
      None
  in
  let call_raise raise_fn_name =
    let exn_bucket = load_reg_to_temp t i.arg.(0) in
    (match Target_system.architecture () with
    | Target_system.AArch64 ->
      add_referenced_symbol t raise_fn_name;
      call_simple
        ?unwind_label:(unwind_label_for_active_handler ())
        ~attrs:(gc_attr ~can_call_gc:true t i)
        ~dbg:i.dbg ~raise_call:true
        ~live_roots:(load_live_gc_roots_across t i)
        ~cc:Oxcaml t raise_fn_name [exn_bucket] []
      |> ignore;
      emit_ins_no_res t I.unreachable
    | Target_system.X86_64 ->
      add_referenced_symbol t raise_fn_name;
      call_simple
        ~attrs:(gc_attr ~can_call_gc:true t i)
        ~dbg:i.dbg ~raise_call:true
        ~live_roots:(load_live_gc_roots_across t i)
        ~cc:Oxcaml t raise_fn_name [exn_bucket] []
      |> ignore;
      (match exn_handler with
      | Some lbl_handler -> (
        match Label.Tbl.find_opt (get_fun_info t).trap_blocks lbl_handler with
        | Some { exn_entry; _ } -> emit_ins_no_res t (I.br exn_entry)
        | None -> emit_ins_no_res t I.unreachable)
      | None -> emit_ins_no_res t I.unreachable)
    | Target_system.IA32 | Target_system.ARM | Target_system.POWER
    | Target_system.Z | Target_system.Riscv ->
      not_implemented_terminator ~msg:"raise" i)
  in
  match raise_kind with
  | Raise_notrace -> (
    (* Get exn bucket *)
    let exn_bucket = load_reg_to_temp t i.arg.(0) in
    let exn_bucket_raw = cast t exn_bucket T.i64 in
    (match Target_system.architecture () with
    | Target_system.AArch64 ->
      (* The destination handler is runtime-entered and reads the domain
         state and allocation cursor from x28/x27; pass the current values
         so the backend can pin them into those registers. *)
      let ds = emit_ins t (I.load ~ptr:domainstate_ptr ~typ:T.i64) in
      let alloc = emit_ins t (I.load ~ptr:allocation_ptr ~typ:T.i64) in
      (match unwind_label_for_active_handler () with
      | Some unwind_label ->
        let intrinsic_name = "llvm.aarch64.oxcaml.raise.notrace.edge" in
        let fun_ident = E.get_fun_ident (get_fun_info t).emitter in
        let recovery_target =
          V.blockaddress ~func:fun_ident ~block:(V.get_ident_exn unwind_label)
        in
        add_called_intrinsic t intrinsic_name
          ~args:[T.i64; T.i64; T.i64; T.ptr]
          ~res:None;
        let normal_label = V.of_label (Cmm.new_label ()) in
        emit_ins_no_res t
          (I.invoke ~func:(LL.Ident.global intrinsic_name)
             ~args:[exn_bucket_raw; ds; alloc; recovery_target]
             ~res_type:T.Or_void.void ~attrs:[] ~operand_bundles:[]
             ~cc:Default ~normal:normal_label ~unwind:unwind_label);
        emit_label t normal_label
      | None ->
        if Config.tsan
        then (
          add_referenced_symbol t "caml_raise_notrace";
          call_simple ~attrs:[Gc_leaf_function] ~cc:Oxcaml t
            "caml_raise_notrace" [exn_bucket_raw] []
          |> ignore)
        else
          call_llvm_intrinsic_no_res t "aarch64.oxcaml.raise.notrace"
            [exn_bucket_raw; ds; alloc])
    | Target_system.X86_64 ->
      (* Get sp for trap block *)
      let exn_sp_ptr = load_domainstate_addr t Domain_exn_handler in
      let trap_block = emit_ins t (I.load ~ptr:exn_sp_ptr ~typ:T.ptr) in
      (* Get contents of the trap block *)
      let prev_exn_sp = emit_ins t (I.load ~ptr:trap_block ~typ:T.i64) in
      let handler_addr =
        let ptr = do_offset t trap_block T.ptr 8 in
        emit_ins t (I.load ~ptr ~typ:T.ptr)
      in
      (* Pop trap block from linked list in the domain *)
      emit_ins_no_res t (I.store ~ptr:exn_sp_ptr ~to_store:prev_exn_sp);
      let trap_block_int = cast t trap_block T.i64 in
      let new_sp =
        emit_ins t (I.binary Add ~arg1:trap_block_int ~arg2:(V.of_int 16))
      in
      write_stack_pointer t new_sp;
      emit_ins_no_res t
        (I.inline_asm ~asm:"movq $0, %rax; jmpq *$1" ~constraints:"r,r,~{rax}"
           ~args:[exn_bucket_raw; handler_addr] ~res_type:T.Or_void.void
           ~sideeffect:true)
    | Target_system.IA32 | Target_system.ARM | Target_system.POWER
    | Target_system.Z | Target_system.Riscv ->
      not_implemented_terminator ~msg:"raise notrace" i);
    match Target_system.architecture () with
    | Target_system.AArch64 -> emit_ins_no_res t I.unreachable
    | Target_system.X86_64 -> (
      match exn_handler with
      | Some lbl_handler -> (
        match Label.Tbl.find_opt (get_fun_info t).trap_blocks lbl_handler with
        | Some { exn_entry; _ } -> emit_ins_no_res t (I.br exn_entry)
        | None -> emit_ins_no_res t I.unreachable)
      | None -> emit_ins_no_res t I.unreachable)
    | Target_system.IA32 | Target_system.ARM | Target_system.POWER
    | Target_system.Z | Target_system.Riscv ->
      emit_ins_no_res t I.unreachable)
  | Raise_regular ->
    let backtrace_pos = load_domainstate_addr t Domain_backtrace_pos in
    emit_ins_no_res t (I.store ~ptr:backtrace_pos ~to_store:(V.of_int 0));
    call_raise "caml_raise_exn"
  | Raise_reraise -> call_raise "caml_reraise_exn"

let emit_terminator t (block : Cfg.basic_block)
    (i : Cfg.terminator Cfg.instruction) =
  emit_comment t "%a" F.pp_dbg_instr_terminator i;
  let fun_info = get_fun_info t in
  let prev_dbg_metadata = fun_info.current_dbg_metadata in
  fun_info.current_dbg_metadata
    <- create_debug_location t fun_info.subprogram_dbg_metadata_id i.dbg;
  Fun.protect
    ~finally:(fun () -> fun_info.current_dbg_metadata <- prev_dbg_metadata)
    (fun () ->
      match i.desc with
      | Never -> fail "terminator.Never"
      | Always lbl -> br_label t lbl
      | Parity_test { ifso; ifnot } ->
        (* ifso -> even / ifnot -> odd, so labels are flipped *)
        let cond = odd_test t i in
        emit_ins_no_res t
          (I.br_cond ~cond ~ifso:(V.of_label ifnot) ~ifnot:(V.of_label ifso))
      | Truth_test { ifso; ifnot } ->
        let cond = test t Itruetest i in
        emit_ins_no_res t
          (I.br_cond ~cond ~ifso:(V.of_label ifso) ~ifnot:(V.of_label ifnot))
      | Return -> return t i
      | Int_test { lt; eq; gt; is_signed; imm } ->
        let open struct
          type comp =
            | Lt
            | Gt
        end in
        let make_comp (comp : comp) : Cmm.integer_comparison =
          match is_signed, comp with
          | Signed, Lt -> Clt
          | Signed, Gt -> Cgt
          | Unsigned, Lt -> Cult
          | Unsigned, Gt -> Cugt
        in
        let lt = V.of_label lt in
        let eq = V.of_label eq in
        let gt = V.of_label gt in
        let ge = Cmm.new_label () |> V.of_label in
        let is_lt = int_comp t (make_comp Lt) i ~imm in
        emit_ins_no_res t (I.br_cond ~cond:is_lt ~ifso:lt ~ifnot:ge);
        emit_label t ge;
        let is_gt = int_comp t (make_comp Gt) i ~imm in
        emit_ins_no_res t (I.br_cond ~cond:is_gt ~ifso:gt ~ifnot:eq)
      | Float_test { width; lt; eq; gt; uo } ->
        let typ = T.of_float_width width in
        let lt = V.of_label lt in
        let eq = V.of_label eq in
        let gt = V.of_label gt in
        let uo = V.of_label uo in
        let ge = V.of_label (Cmm.new_label ()) in
        let eq_or_uo = V.of_label (Cmm.new_label ()) in
        let is_lt = float_comp t Cmm.CFlt i typ in
        emit_ins_no_res t (I.br_cond ~cond:is_lt ~ifso:lt ~ifnot:ge);
        emit_label t ge;
        let is_gt = float_comp t Cmm.CFgt i typ in
        emit_ins_no_res t (I.br_cond ~cond:is_gt ~ifso:gt ~ifnot:eq_or_uo);
        emit_label t eq_or_uo;
        let is_eq = float_comp t Cmm.CFeq i typ in
        emit_ins_no_res t (I.br_cond ~cond:is_eq ~ifso:eq ~ifnot:uo)
      | Switch labels ->
        let discr = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
        let default = V.of_label (Cmm.new_label ()) in
        let branches =
          List.mapi
            (fun i label : I.switch_branch ->
              { index = V.of_int i; label = V.of_label label })
            (Array.to_list labels)
        in
        emit_ins_no_res t (I.switch ~discr ~default ~branches);
        (* note: [Switch] does not take a default label, as switches are assumed
           to be exhaustive. Since it is mandatory in LLVM, we will use a label
           with an [unreachable] instruction for every instance of this
           instruction. *)
        emit_label t default;
        emit_ins_no_res t I.unreachable
      | Raise raise_kind -> raise_ t ~exn_handler:block.exn i raise_kind
      | Call { op; label_after } ->
        reject_addr_regs i.arg "call";
        let exn_entry = exn_entry_for_block t block in
        let unwind_label, exn_entry =
          unwind_label_and_landingpad_for_exn_entry exn_entry
        in
        call ?unwind_label t i op;
        br_label t label_after;
        emit_unwind_landingpad_after t unwind_label exn_entry
      | Tailcall_self { destination } -> tailcall_self t i destination
      | Tailcall_func op ->
        reject_addr_regs i.arg "tailcall func";
        call ~tail:true t i op
      | Call_no_return
          { func_symbol; alloc; ty_args; stack_ofs; stack_align; _ } ->
        let exn_entry = exn_entry_for_block t block in
        let unwind_label, exn_entry =
          unwind_label_and_landingpad_for_exn_entry exn_entry
        in
        extcall ?unwind_label t i ~func_symbol ~alloc ~ty_args ~stack_ofs
          ~stack_align;
        emit_ins_no_res t I.unreachable;
        emit_unwind_landingpad_after t unwind_label exn_entry
      | Prim { op; label_after } -> (
        match op with
        | Probe _ ->
          reject_addr_regs i.arg "prim";
          not_implemented_terminator ~msg:"probe" i
        | External { func_symbol; alloc; ty_args; stack_ofs; stack_align; _ } ->
          let exn_entry = exn_entry_for_block t block in
          let unwind_label, exn_entry =
            unwind_label_and_landingpad_for_exn_entry exn_entry
          in
          extcall ?unwind_label t i ~func_symbol ~alloc ~ty_args ~stack_ofs
            ~stack_align;
          br_label t label_after;
          emit_unwind_landingpad_after t unwind_label exn_entry)
      | Invalid { message = _; stack_ofs; stack_align; label_after = _ } ->
        reject_addr_regs i.arg "prim";
        extcall t i ~func_symbol:Cmm.caml_flambda2_invalid ~alloc:false
          ~ty_args:[XValue] ~stack_ofs ~stack_align;
        emit_ins_no_res t I.unreachable)

(* Basic instructions *)

let int_op t (i : Cfg.basic Cfg.instruction) (op : Operation.integer_operation)
    ~imm =
  let do_binary ?(tagged_args = false) op =
    let typ = T.i64 in
    let load_arg reg =
      if tagged_args then load_reg_as_tagged_int t reg
      else load_reg_to_temp ~typ t reg
    in
    reject_addr_regs i.res "int_op";
    match imm with
    | None ->
      let arg1 = load_arg i.arg.(0) in
      let arg2 = load_arg i.arg.(1) in
      emit_ins t (I.binary op ~arg1 ~arg2)
    | Some n ->
      let arg1 = load_arg i.arg.(0) in
      let arg2 = V.of_int ~typ n in
      emit_ins t (I.binary op ~arg1 ~arg2)
  in
  let do_unary_intrinsic_extra_args op_name extra_args =
    let typ = T.i64 in
    let arg = load_reg_to_temp ~typ t i.arg.(0) in
    call_llvm_intrinsic t
      (op_name ^ "." ^ llvm_intrinsic_type_suffix typ)
      ([arg] @ extra_args) typ
  in
  let do_unary_intrinsic op_name = do_unary_intrinsic_extra_args op_name [] in
  let do_gep ~negate_arg =
    let base_arg, offset_arg =
      if Cmm.is_val i.arg.(0).typ || Cmm.is_addr i.arg.(0).typ
      then i.arg.(0), (if Array.length i.arg > 1 then Some i.arg.(1) else None)
      else if (not negate_arg)
              && Array.length i.arg > 1
              && (Cmm.is_val i.arg.(1).typ || Cmm.is_addr i.arg.(1).typ)
      then i.arg.(1), Some i.arg.(0)
      else i.arg.(0), (if Array.length i.arg > 1 then Some i.arg.(1) else None)
    in
    let base_ptr =
      if Cmm.is_val base_arg.typ || Cmm.is_addr base_arg.typ
      then load_reg_to_temp ~typ:T.val_ptr t base_arg
      else if Reg.Set.mem base_arg (get_fun_info t).static_addr_regs
      then load_reg_to_temp ~typ:T.val_ptr t base_arg
      else
        fail_msg ~name:"int_op"
          "Addr arithmetic requires a Val, Addr, or static-symbol base \
           register; raw Int address arithmetic must use Int/Caddi; args=%a \
           res=%a"
          Printreg.regset (Reg.Set.of_list (Array.to_list i.arg))
          Printreg.regset (Reg.Set.of_list (Array.to_list i.res))
    in
    let offset =
      match imm with
      | None ->
        let offset_arg =
          match offset_arg with
          | Some offset_arg -> offset_arg
          | None ->
            fail_msg ~name:"int_op" "Addr arithmetic missing offset register"
        in
        let temp = load_reg_to_temp ~typ:T.i64 t offset_arg in
        if negate_arg
        then emit_ins t (I.binary Sub ~arg1:(V.of_int 0) ~arg2:temp)
        else temp
      | Some n -> V.of_int ~typ:T.i64 (if negate_arg then -n else n)
    in
    let metadata =
      if
        Cmm.is_val i.res.(0).typ
        || Reg.Set.mem i.res.(0) (get_fun_info t).addr_regs_known_base
      then Some "!is_base_value !{}"
      else None
    in
    emit_ins ?metadata t
      (I.getelementptr ~base_type:T.i8 ~base_ptr ~indices:[offset])
  in
  let do_imulh ~signed =
    (* Assuming operands are i64 *)
    let arg1 = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
    let arg2 =
      match imm with
      | None -> load_reg_to_temp ~typ:T.i64 t i.arg.(1)
      | Some n -> V.of_int n
    in
    (* Extend args to i128 *)
    let extend_value arg =
      let ext_op = if signed then I.Sext else I.Zext in
      emit_ins t (I.convert ext_op ~arg ~to_:T.i128)
    in
    let arg1 = extend_value arg1 in
    let arg2 = extend_value arg2 in
    (* Multiply as i128 *)
    let res_ext = emit_ins t (I.binary Mul ~arg1 ~arg2) in
    (* Shift the bits we care about *)
    let shifted =
      emit_ins t (I.binary Lshr ~arg1:res_ext ~arg2:(V.of_int ~typ:T.i128 64))
    in
    emit_ins t (I.convert Trunc ~arg:shifted ~to_:T.i64)
  in
  let res =
    match op with
    | Iadd ->
      if Cmm.is_val i.res.(0).typ || Cmm.is_addr i.res.(0).typ
      then do_gep ~negate_arg:false
      else do_binary ~tagged_args:true Add
    | Isub ->
      if Cmm.is_addr i.res.(0).typ
      then do_gep ~negate_arg:true
      else do_binary ~tagged_args:true Sub
    | Imul -> do_binary ~tagged_args:true Mul
    | Imulh { signed } -> do_imulh ~signed
    | Idiv -> do_binary ~tagged_args:true Sdiv
    | Imod -> do_binary ~tagged_args:true Srem
    | Iand -> do_binary And
    | Ior -> do_binary Or
    | Ixor -> do_binary Xor
    | Ilsl -> do_binary ~tagged_args:true Shl
    | Ilsr -> do_binary ~tagged_args:true Lshr
    | Iasr -> do_binary ~tagged_args:true Ashr
    | Icomp comp ->
      let bool_res = int_comp t comp i ~imm in
      (* convert i1 -> i64 *)
      emit_ins t (I.convert Zext ~arg:bool_res ~to_:T.i64)
    (* ctlz and cttz have a second optional argument that indicates whether 0 is
       poison or not. We pass false to match OCaml's behaviour. *)
    | Iclz -> do_unary_intrinsic_extra_args "ctlz" [V.of_int ~typ:T.i1 0]
    | Ictz -> do_unary_intrinsic_extra_args "cttz" [V.of_int ~typ:T.i1 0]
    | Ipopcnt -> do_unary_intrinsic "ctpop"
  in
  store_into_reg t i.res.(0) res

let float_op t (i : Cfg.basic Cfg.instruction) (width : Cmm.float_width)
    (op : Operation.float_operation) =
  let typ = T.of_float_width width in
  let do_binary op =
    let arg1 = load_reg_to_temp ~typ t i.arg.(0) in
    let arg2 = load_reg_to_temp ~typ t i.arg.(1) in
    emit_ins t (I.binary op ~arg1 ~arg2)
  in
  let do_unary_intrinsic op_name =
    let arg = load_reg_to_temp ~typ t i.arg.(0) in
    call_llvm_intrinsic t
      (op_name ^ "." ^ llvm_intrinsic_type_suffix typ)
      [arg] typ
  in
  let res =
    match op with
    | Iaddf -> do_binary Fadd
    | Isubf -> do_binary Fsub
    | Imulf -> do_binary Fmul
    | Idivf -> do_binary Fdiv
    | Inegf ->
      let arg = load_reg_to_temp ~typ t i.arg.(0) in
      emit_ins t (I.unary Fneg ~arg)
    | Iabsf -> do_unary_intrinsic "fabs"
    | Icompf comp ->
      let bool_res = float_comp t comp i typ in
      (* convert i1 -> i64 *)
      emit_ins t (I.convert Zext ~arg:bool_res ~to_:T.i64)
  in
  store_into_reg t i.res.(0) res

let int128_op _t (i : Cfg.basic Cfg.instruction)
    (op : Operation.int128_operation) =
  (* CR-soon mslater for gyorsh: implement these in llvm intrinsics *)
  match op with
  | Iadd128 -> not_implemented_basic ~msg:"Iadd128" i
  | Isub128 -> not_implemented_basic ~msg:"Isub128" i
  | Imul64 { signed = _ } -> not_implemented_basic ~msg:"Imul64" i

(* CR yusumez: add a generic Cfg instruction for bswap *)
let bswap t (i : Cfg.basic Cfg.instruction) (bitwidth : Arch.bswap_bitwidth) =
  let typ =
    match bitwidth with
    | Sixteen -> T.i16
    | Thirtytwo -> T.i32
    | Sixtyfour -> T.i64
  in
  let do_trunc arg =
    if T.equal typ (V.get_type arg)
    then arg
    else emit_ins t (I.convert Trunc ~arg ~to_:typ)
  in
  let do_zext arg =
    if T.equal typ T.i64
    then arg
    else emit_ins t (I.convert Zext ~arg ~to_:T.i64)
  in
  let arg = load_reg_to_temp t i.arg.(0) in
  let trunced = do_trunc arg in
  let bswapped =
    call_llvm_intrinsic t
      ("bswap." ^ llvm_intrinsic_type_suffix typ)
      [trunced] typ
  in
  let zexted = do_zext bswapped in
  store_into_reg t i.res.(0) zexted

(* CR yusumez: Make [Illvm_intrinsic] contain the LLVM intrinsic name and
   necessary types (as passed to [do_intrinsic_call]). This means
   [Cfg_selection] will be responsible for all the arch-specific handling, while
   this function can stay generic *)
let intrinsic t (i : Cfg.basic Cfg.instruction) intrinsic_name =
  let do_conv arg (to_ : T.t) =
    let from : T.t = V.get_type arg in
    (* CR yusumez: I really don't like the -fragile-match... *)
    match[@warning "-fragile-match"] from, to_ with
    | _ when T.equal from to_ -> arg
    | Double, Vector { num_of_elems = _; elem_type = Double } ->
      emit_ins t
        (I.insertelement ~vector:(V.poison to_) ~index:(V.of_int 0)
           ~to_insert:arg)
    | Vector { num_of_elems = _; elem_type = Double }, Double ->
      emit_ins t (I.extractelement ~vector:arg ~index:(V.of_int 0))
    | Int { width_in_bits = 64 }, Int { width_in_bits = 32 } ->
      emit_ins t (I.convert Trunc ~arg ~to_)
    | _ ->
      fail_msg ~name:"intrinsic" "unexpected reg types in do_conv: %a -> %a"
        T.pp_t from T.pp_t to_
  in
  let do_intrinsic_call name arg_types res_type =
    (* Sometimes, we get unit arguments for intrinsics with no arguments. We use
       [map2_prefix] to ignore them. *)
    let args, _ =
      List.map2_prefix
        (fun arg_type reg ->
          let temp = load_reg_to_temp t reg in
          do_conv temp arg_type)
        arg_types (Array.to_list i.arg)
    in
    let res = call_llvm_intrinsic t name args res_type in
    let conved_res = do_conv res (T.of_reg i.res.(0)) in
    store_into_reg t i.res.(0) conved_res
  in
  (* Intrinsics must not allocate on the OCaml heap. See
     [Arch.operation_allocates]. *)
  match intrinsic_name with
  | "caml_sse2_float64_min" ->
    do_intrinsic_call "x86.sse2.min.sd" [T.doublex2; T.doublex2] T.doublex2
  | "caml_sse2_float64_max" ->
    do_intrinsic_call "x86.sse2.max.sd" [T.doublex2; T.doublex2] T.doublex2
  | "caml_rdtsc_unboxed" -> do_intrinsic_call "readcyclecounter" [] T.i64
  | "caml_rdpmc_unboxed" -> do_intrinsic_call "x86.rdpmc" [T.i32] T.i64
  | _ -> not_implemented_basic ~msg:"specific intrinsic" i

let specific t (i : Cfg.basic Cfg.instruction) (op : Arch.specific_operation) =
  let int_arg n = load_reg_to_temp ~typ:T.i64 t i.arg.(n) in
  let store_int_res value = store_into_reg t i.res.(0) value in
  let float_arg typ n = load_reg_to_temp ~typ t i.arg.(n) in
  let round_intrinsic_name mode =
    match mode with
    | Simd.Rounding_mode.Current -> "nearbyint"
    | Simd.Rounding_mode.Neg_inf -> "floor"
    | Simd.Rounding_mode.Pos_inf -> "ceil"
    | Simd.Rounding_mode.Zero -> "trunc"
    | Simd.Rounding_mode.Nearest -> "roundeven"
  in
  let float_round typ mode =
    let name =
      round_intrinsic_name mode ^ "." ^ llvm_intrinsic_type_suffix typ
    in
    call_llvm_intrinsic t name [float_arg typ 0] typ
  in
  let float_minmax intrinsic typ =
    call_llvm_intrinsic t
      (intrinsic ^ "." ^ llvm_intrinsic_type_suffix typ)
      [float_arg typ 0; float_arg typ 1]
      typ
  in
  let float_minmax_match_sse cond typ =
    let arg1 = float_arg typ 0 in
    let arg2 = float_arg typ 1 in
    let cmp = emit_ins t (I.fcmp cond ~arg1 ~arg2) in
    emit_ins t (I.select ~cond:cmp ~ifso:arg1 ~ifnot:arg2)
  in
  let float_round_to_i64 typ =
    let rounded =
      call_llvm_intrinsic t
        ("roundeven." ^ llvm_intrinsic_type_suffix typ)
        [float_arg typ 0]
        typ
    in
    emit_ins t (I.convert Fptosi ~arg:rounded ~to_:T.i64)
  in
  let float_muladd kind typ =
    let product =
      emit_ins t
        (I.binary_contract Fmul ~arg1:(float_arg typ 1) ~arg2:(float_arg typ 2))
    in
    let arg0 = float_arg typ 0 in
    match kind with
    | `Muladd -> emit_ins t (I.binary_contract Fadd ~arg1:arg0 ~arg2:product)
    | `Mulsub -> emit_ins t (I.binary_contract Fsub ~arg1:arg0 ~arg2:product)
    | `Negmuladd ->
      let neg_arg0 = emit_ins t (I.unary Fneg ~arg:arg0) in
      emit_ins t (I.binary_contract Fsub ~arg1:neg_arg0 ~arg2:product)
    | `Negmulsub -> emit_ins t (I.binary_contract Fsub ~arg1:product ~arg2:arg0)
  in
  let int_vec_type ~width_in_bits =
    T.Vector
      { num_of_elems = 128 / width_in_bits;
        elem_type = T.Int { width_in_bits }
      }
  in
  let float_vec_type ~width =
    let elem_type, num_of_elems =
      match width with Cmm.Float32 -> T.float, 4 | Cmm.Float64 -> T.double, 2
    in
    T.Vector { num_of_elems; elem_type }
  in
  let cast_if_needed value typ =
    if T.equal (V.get_type value) typ
    then value
    else emit_ins t (I.convert Bitcast ~arg:value ~to_:typ)
  in
  let int_vector_constant width_in_bits n =
    let typ = int_vec_type ~width_in_bits in
    let elem_type = T.Int { width_in_bits } in
    let elem = Format.asprintf "%a %d" T.pp_t elem_type n in
    V.imm typ
      (Format.asprintf "<%a>"
         (Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
            Format.pp_print_string)
         (List.init (128 / width_in_bits) (fun _ -> elem)))
  in
  let int_vector_constant_like typ n =
    match typ with
    | T.Vector { num_of_elems; elem_type = T.Int _ as elem_type } ->
      let elem = Format.asprintf "%a %d" T.pp_t elem_type n in
      V.imm typ
        (Format.asprintf "<%a>"
           (Format.pp_print_list
              ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
              Format.pp_print_string)
           (List.init num_of_elems (fun _ -> elem)))
    | T.Int _ -> V.of_int ~typ n
    | T.Ptr _ | T.Float | T.Double | T.Struct _ | T.Array _ | T.Vector _
    | T.Label | T.Token | T.Metadata ->
      fail_msg ~name:"int_vector_constant_like" "expected integer vector"
  in
  let simd_int_unary width_in_bits op =
    let typ = int_vec_type ~width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let res =
      match op with
      | `Neg ->
        emit_ins t (I.binary Sub ~arg1:(V.zeroinitializer typ) ~arg2:arg)
      | `Not ->
        let all_ones = int_vector_constant width_in_bits (-1) in
        emit_ins t (I.binary Xor ~arg1:arg ~arg2:all_ones)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_unary_intrinsic typ intrinsic =
    let name = intrinsic ^ "." ^ llvm_intrinsic_type_suffix typ in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let res = call_llvm_intrinsic t name [arg] typ in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_binary width_in_bits op =
    let typ = int_vec_type ~width_in_bits in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let res = emit_ins t (I.binary op ~arg1 ~arg2) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_binary_intrinsic typ intrinsic =
    let name = intrinsic ^ "." ^ llvm_intrinsic_type_suffix typ in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let res = call_llvm_intrinsic t name [arg1; arg2] typ in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_minmax width_in_bits cond =
    let typ = int_vec_type ~width_in_bits in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let choose_arg1 = emit_ins t (I.icmp cond ~arg1 ~arg2) in
    let res = emit_ins t (I.select ~cond:choose_arg1 ~ifso:arg1 ~ifnot:arg2) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_shift_imm width_in_bits op n =
    let typ = int_vec_type ~width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let shift = int_vector_constant width_in_bits n in
    let res = emit_ins t (I.binary op ~arg1:arg ~arg2:shift) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_variable_shift width_in_bits intrinsic =
    let typ = int_vec_type ~width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let shift = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let res =
      call_llvm_intrinsic t
        (intrinsic ^ "." ^ llvm_intrinsic_type_suffix typ)
        [arg; shift] typ
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_dup_lane width_in_bits lane =
    let typ = int_vec_type ~width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let elem =
      emit_ins t (I.extractelement ~vector:arg ~index:(V.of_int lane))
    in
    let lanes = 128 / width_in_bits in
    let res =
      List.init lanes Fun.id
      |> List.fold_left
           (fun vector lane ->
             emit_ins t
               (I.insertelement ~vector ~index:(V.of_int lane) ~to_insert:elem))
           (V.poison typ)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_get_lane width_in_bits lane =
    let typ = int_vec_type ~width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let elem =
      emit_ins t (I.extractelement ~vector:arg ~index:(V.of_int lane))
    in
    let res =
      if width_in_bits = 64
      then elem
      else emit_ins t (I.convert Sext ~arg:elem ~to_:T.i64)
    in
    store_into_reg t i.res.(0) res
  in
  let simd_int_set_lane width_in_bits lane =
    let typ = int_vec_type ~width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let elem_typ = T.Int { width_in_bits } in
    let elem =
      if width_in_bits = 64
      then load_reg_to_temp ~typ:T.i64 t i.arg.(1)
      else
        emit_ins t
          (I.convert Trunc
             ~arg:(load_reg_to_temp ~typ:T.i64 t i.arg.(1))
             ~to_:elem_typ)
    in
    let res =
      emit_ins t
        (I.insertelement ~vector:arg ~index:(V.of_int lane) ~to_insert:elem)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_widen_low src_width_in_bits convert_op =
    let dst_width_in_bits = 2 * src_width_in_bits in
    let src_typ = int_vec_type ~width_in_bits:src_width_in_bits in
    let dst_typ = int_vec_type ~width_in_bits:dst_width_in_bits in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) src_typ in
    let dst_elem_typ = T.Int { width_in_bits = dst_width_in_bits } in
    let lanes = 128 / dst_width_in_bits in
    let res =
      List.init lanes Fun.id
      |> List.fold_left
           (fun vector lane ->
             let elem =
               emit_ins t (I.extractelement ~vector:arg ~index:(V.of_int lane))
             in
             let widened =
               emit_ins t (I.convert convert_op ~arg:elem ~to_:dst_elem_typ)
             in
             emit_ins t
               (I.insertelement ~vector ~index:(V.of_int lane)
                  ~to_insert:widened))
           (V.poison dst_typ)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_narrow src_width_in_bits ~high =
    let dst_width_in_bits = src_width_in_bits / 2 in
    let src_typ = int_vec_type ~width_in_bits:src_width_in_bits in
    let dst_typ = int_vec_type ~width_in_bits:dst_width_in_bits in
    let src_arg_index = if high then 1 else 0 in
    let src =
      cast_if_needed (load_reg_to_temp t i.arg.(src_arg_index)) src_typ
    in
    let dst_elem_typ = T.Int { width_in_bits = dst_width_in_bits } in
    let lanes = 128 / src_width_in_bits in
    let base =
      if high
      then cast_if_needed (load_reg_to_temp t i.arg.(0)) dst_typ
      else V.zeroinitializer dst_typ
    in
    let dst_lane_offset = if high then lanes else 0 in
    let res =
      List.init lanes Fun.id
      |> List.fold_left
           (fun vector lane ->
             let elem =
               emit_ins t (I.extractelement ~vector:src ~index:(V.of_int lane))
             in
             let narrowed =
               emit_ins t (I.convert Trunc ~arg:elem ~to_:dst_elem_typ)
             in
             emit_ins t
               (I.insertelement ~vector
                  ~index:(V.of_int (lane + dst_lane_offset))
                  ~to_insert:narrowed))
           base
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_saturating_narrow src_width_in_bits ~unsigned ~high =
    let dst_width_in_bits = src_width_in_bits / 2 in
    let src_typ = int_vec_type ~width_in_bits:src_width_in_bits in
    let dst_typ = int_vec_type ~width_in_bits:dst_width_in_bits in
    let src_arg_index = if high then 1 else 0 in
    let src =
      cast_if_needed (load_reg_to_temp t i.arg.(src_arg_index)) src_typ
    in
    let min_value, max_value, cmp_lt, cmp_gt =
      if unsigned
      then 0, (1 lsl dst_width_in_bits) - 1, I.Iult, I.Iugt
      else
        ( ~-(1 lsl (dst_width_in_bits - 1)),
          (1 lsl (dst_width_in_bits - 1)) - 1,
          I.Islt,
          I.Isgt )
    in
    let min_vector = int_vector_constant_like src_typ min_value in
    let max_vector = int_vector_constant_like src_typ max_value in
    let below_min = emit_ins t (I.icmp cmp_lt ~arg1:src ~arg2:min_vector) in
    let above_max = emit_ins t (I.icmp cmp_gt ~arg1:src ~arg2:max_vector) in
    let clamped_min =
      emit_ins t (I.select ~cond:below_min ~ifso:min_vector ~ifnot:src)
    in
    let clamped =
      emit_ins t (I.select ~cond:above_max ~ifso:max_vector ~ifnot:clamped_min)
    in
    let dst_elem_typ = T.Int { width_in_bits = dst_width_in_bits } in
    let lanes = 128 / src_width_in_bits in
    let base =
      if high
      then cast_if_needed (load_reg_to_temp t i.arg.(0)) dst_typ
      else V.zeroinitializer dst_typ
    in
    let dst_lane_offset = if high then lanes else 0 in
    let res =
      List.init lanes Fun.id
      |> List.fold_left
           (fun vector lane ->
             let elem =
               emit_ins t
                 (I.extractelement ~vector:clamped ~index:(V.of_int lane))
             in
             let narrowed =
               emit_ins t (I.convert Trunc ~arg:elem ~to_:dst_elem_typ)
             in
             emit_ins t
               (I.insertelement ~vector
                  ~index:(V.of_int (lane + dst_lane_offset))
                  ~to_insert:narrowed))
           base
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_int_widening_mul src_width_in_bits convert_op ~high =
    let dst_width_in_bits = 2 * src_width_in_bits in
    let src_typ = int_vec_type ~width_in_bits:src_width_in_bits in
    let dst_typ = int_vec_type ~width_in_bits:dst_width_in_bits in
    let dst_elem_typ = T.Int { width_in_bits = dst_width_in_bits } in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) src_typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) src_typ in
    let lanes = 128 / dst_width_in_bits in
    let src_lane_offset = if high then lanes else 0 in
    let widen_arg arg =
      List.init lanes Fun.id
      |> List.fold_left
           (fun vector lane ->
             let elem =
               emit_ins t
                 (I.extractelement ~vector:arg
                    ~index:(V.of_int (lane + src_lane_offset)))
             in
             let widened =
               emit_ins t (I.convert convert_op ~arg:elem ~to_:dst_elem_typ)
             in
             emit_ins t
               (I.insertelement ~vector ~index:(V.of_int lane)
                  ~to_insert:widened))
           (V.poison dst_typ)
    in
    let arg1 = widen_arg arg1 in
    let arg2 = widen_arg arg2 in
    let res = emit_ins t (I.binary Mul ~arg1 ~arg2) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_convert from_typ to_typ convert_op =
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) from_typ in
    let res = emit_ins t (I.convert convert_op ~arg ~to_:to_typ) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_fp_to_signed_int from_typ to_typ intrinsic =
    let name =
      Format.sprintf "aarch64.neon.%s.%s.%s" intrinsic
        (llvm_intrinsic_type_suffix to_typ)
        (llvm_intrinsic_type_suffix from_typ)
    in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) from_typ in
    let res = call_llvm_intrinsic t name [arg] to_typ in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_float_cmp width cond =
    let typ = float_vec_type ~width in
    let mask_width_in_bits =
      match width with Cmm.Float32 -> 32 | Cmm.Float64 -> 64
    in
    let cond =
      match cond with
      | Simd.Float_cond.EQ -> I.Foeq
      | Simd.Float_cond.GE -> I.Foge
      | Simd.Float_cond.GT -> I.Fogt
      | Simd.Float_cond.LE | Simd.Float_cond.LT | Simd.Float_cond.NE
      | Simd.Float_cond.CC | Simd.Float_cond.CS | Simd.Float_cond.LS
      | Simd.Float_cond.HI ->
        fail_msg ~name:"simd_float_cmp" "unexpected float comparison"
    in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let cmp = emit_ins t (I.fcmp cond ~arg1 ~arg2) in
    let res =
      emit_ins t
        (I.convert Sext ~arg:cmp
           ~to_:(int_vec_type ~width_in_bits:mask_width_in_bits))
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_float_minmax width intrinsic =
    let typ = float_vec_type ~width in
    let name = intrinsic ^ "." ^ llvm_intrinsic_type_suffix typ in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let res = call_llvm_intrinsic t name [arg1; arg2] typ in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_float_binary width op =
    let typ = float_vec_type ~width in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let res = emit_ins t (I.binary op ~arg1 ~arg2) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_float_unary_intrinsic width intrinsic =
    let typ = float_vec_type ~width in
    let name = intrinsic ^ "." ^ llvm_intrinsic_type_suffix typ in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let res = call_llvm_intrinsic t name [arg] typ in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_cvt_f64_f32 () =
    let src_typ = float_vec_type ~width:Cmm.Float32 in
    let dst_typ = float_vec_type ~width:Cmm.Float64 in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) src_typ in
    let low =
      List.init 2 Fun.id
      |> List.fold_left
           (fun vector lane ->
             let elem =
               emit_ins t (I.extractelement ~vector:arg ~index:(V.of_int lane))
             in
             emit_ins t
               (I.insertelement ~vector ~index:(V.of_int lane) ~to_insert:elem))
           (V.poison (T.Vector { num_of_elems = 2; elem_type = T.float }))
    in
    let res = emit_ins t (I.convert Fpext ~arg:low ~to_:dst_typ) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_cvt_f32_f64 () =
    let src_typ = float_vec_type ~width:Cmm.Float64 in
    let dst_typ = float_vec_type ~width:Cmm.Float32 in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) src_typ in
    let low =
      emit_ins t
        (I.convert Fptrunc ~arg
           ~to_:(T.Vector { num_of_elems = 2; elem_type = T.float }))
    in
    let res =
      List.init 2 Fun.id
      |> List.fold_left
           (fun vector lane ->
             let elem =
               emit_ins t (I.extractelement ~vector:low ~index:(V.of_int lane))
             in
             emit_ins t
               (I.insertelement ~vector ~index:(V.of_int lane) ~to_insert:elem))
           (V.zeroinitializer dst_typ)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_copyq_laneq_s64 ~src_lane ~dst_lane =
    let typ = int_vec_type ~width_in_bits:64 in
    let dst = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let src = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let elem =
      emit_ins t (I.extractelement ~vector:src ~index:(V.of_int src_lane))
    in
    let res =
      emit_ins t
        (I.insertelement ~vector:dst ~index:(V.of_int dst_lane) ~to_insert:elem)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_extq_u8 n =
    let typ = int_vec_type ~width_in_bits:8 in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
    let res =
      List.init 16 Fun.id
      |> List.fold_left
           (fun vector lane ->
             let src_lane = lane + n in
             let src, src_lane =
               if src_lane < 16 then arg1, src_lane else arg2, src_lane - 16
             in
             let elem =
               emit_ins t
                 (I.extractelement ~vector:src ~index:(V.of_int src_lane))
             in
             emit_ins t
               (I.insertelement ~vector ~index:(V.of_int lane) ~to_insert:elem))
           (V.poison typ)
    in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_zip typ ~high =
    match[@warning "-fragile-match"] typ with
    | T.Vector { num_of_elems; _ } ->
      let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
      let arg2 = cast_if_needed (load_reg_to_temp t i.arg.(1)) typ in
      let start = if high then num_of_elems / 2 else 0 in
      let lanes = num_of_elems / 2 in
      let res =
        List.init lanes Fun.id
        |> List.fold_left
             (fun vector lane ->
               let src_lane = start + lane in
               let elem1 =
                 emit_ins t
                   (I.extractelement ~vector:arg1 ~index:(V.of_int src_lane))
               in
               let elem2 =
                 emit_ins t
                   (I.extractelement ~vector:arg2 ~index:(V.of_int src_lane))
               in
               let vector =
                 emit_ins t
                   (I.insertelement ~vector
                      ~index:(V.of_int (2 * lane))
                      ~to_insert:elem1)
               in
               emit_ins t
                 (I.insertelement ~vector
                    ~index:(V.of_int ((2 * lane) + 1))
                    ~to_insert:elem2))
             (V.poison typ)
      in
      cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
    | _ -> Misc.fatal_error "expected vector type"
  in
  let simd_int_cmp width_in_bits cond ~zero =
    let typ = int_vec_type ~width_in_bits in
    let cond =
      match cond with
      | Simd.Cond.EQ -> I.Ieq
      | Simd.Cond.GE -> I.Isge
      | Simd.Cond.GT -> I.Isgt
      | Simd.Cond.LE -> I.Isle
      | Simd.Cond.LT -> I.Islt
    in
    let arg1 = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let arg2 =
      if zero
      then V.zeroinitializer typ
      else cast_if_needed (load_reg_to_temp t i.arg.(1)) typ
    in
    let cmp = emit_ins t (I.icmp cond ~arg1 ~arg2) in
    let res = emit_ins t (I.convert Sext ~arg:cmp ~to_:typ) in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  let simd_float_round width mode =
    let typ = float_vec_type ~width in
    let name =
      round_intrinsic_name mode ^ "." ^ llvm_intrinsic_type_suffix typ
    in
    let arg = cast_if_needed (load_reg_to_temp t i.arg.(0)) typ in
    let res = call_llvm_intrinsic t name [arg] typ in
    cast_if_needed res (T.of_reg i.res.(0)) |> store_into_reg t i.res.(0)
  in
  match[@warning "-fragile-match"] op with
  | Ibswap { bitwidth } -> bswap t i bitwidth
  | Illvm_intrinsic intrinsic_name -> intrinsic t i intrinsic_name
  | Ishiftarith (shift_op, shift) ->
    let shifted =
      emit_ins t
        (I.binary
           (if shift >= 0 then Shl else Ashr)
           ~arg1:(int_arg 1)
           ~arg2:(V.of_int ~typ:T.i64 (abs shift)))
    in
    let op : I.binary_op =
      match shift_op with Ishiftadd -> Add | Ishiftsub -> Sub
    in
    emit_ins t (I.binary op ~arg1:(int_arg 0) ~arg2:shifted) |> store_int_res
  | Imuladd | Imulsub ->
    let product =
      emit_ins t (I.binary Mul ~arg1:(int_arg 0) ~arg2:(int_arg 1))
    in
    let value =
      match op with
      | Imuladd -> emit_ins t (I.binary Add ~arg1:product ~arg2:(int_arg 2))
      | Imulsub -> emit_ins t (I.binary Sub ~arg1:(int_arg 2) ~arg2:product)
      | _ -> assert false
    in
    store_int_res value
  | Isignext size ->
    let narrowed_type = T.Int { width_in_bits = size } in
    let narrowed =
      emit_ins t (I.convert Trunc ~arg:(int_arg 0) ~to_:narrowed_type)
    in
    emit_ins t (I.convert Sext ~arg:narrowed ~to_:T.i64) |> store_int_res
  | Imove32 ->
    let narrowed = emit_ins t (I.convert Trunc ~arg:(int_arg 0) ~to_:T.i32) in
    emit_ins t (I.convert Zext ~arg:narrowed ~to_:T.i64) |> store_int_res
  | Inegmulf ->
    let typ = T.of_reg i.res.(0) in
    let product =
      emit_ins t (I.binary Fmul ~arg1:(float_arg typ 0) ~arg2:(float_arg typ 1))
    in
    emit_ins t (I.unary Fneg ~arg:product) |> store_into_reg t i.res.(0)
  | Imuladdf ->
    let typ = T.of_reg i.res.(0) in
    float_muladd `Muladd typ |> store_into_reg t i.res.(0)
  | Imulsubf ->
    let typ = T.of_reg i.res.(0) in
    float_muladd `Mulsub typ |> store_into_reg t i.res.(0)
  | Inegmuladdf ->
    let typ = T.of_reg i.res.(0) in
    float_muladd `Negmuladd typ |> store_into_reg t i.res.(0)
  | Inegmulsubf ->
    let typ = T.of_reg i.res.(0) in
    float_muladd `Negmulsub typ |> store_into_reg t i.res.(0)
  | Isqrtf ->
    let typ = T.of_reg i.res.(0) in
    call_llvm_intrinsic t
      ("sqrt." ^ llvm_intrinsic_type_suffix typ)
      [float_arg typ 0]
      typ
    |> store_into_reg t i.res.(0)
  | Isimd (Simd.Round_f32 mode) ->
    float_round T.float mode |> store_into_reg t i.res.(0)
  | Isimd (Simd.Round_f64 mode) ->
    float_round T.double mode |> store_into_reg t i.res.(0)
  | Isimd Simd.Min_scalar_f32 ->
    float_minmax_match_sse Folt T.float |> store_into_reg t i.res.(0)
  | Isimd Simd.Max_scalar_f32 ->
    float_minmax_match_sse Fogt T.float |> store_into_reg t i.res.(0)
  | Isimd Simd.Min_scalar_f64 ->
    float_minmax_match_sse Folt T.double |> store_into_reg t i.res.(0)
  | Isimd Simd.Max_scalar_f64 ->
    float_minmax_match_sse Fogt T.double |> store_into_reg t i.res.(0)
  | Isimd Simd.Fmin_f32 ->
    float_minmax "minimum" T.float |> store_into_reg t i.res.(0)
  | Isimd Simd.Fmax_f32 ->
    float_minmax "maximum" T.float |> store_into_reg t i.res.(0)
  | Isimd Simd.Fmin_f64 ->
    float_minmax "minimum" T.double |> store_into_reg t i.res.(0)
  | Isimd Simd.Fmax_f64 ->
    float_minmax "maximum" T.double |> store_into_reg t i.res.(0)
  | Isimd Simd.Round_f32_s64 ->
    float_round_to_i64 T.float |> store_into_reg t i.res.(0)
  | Isimd Simd.Round_f64_s64 ->
    float_round_to_i64 T.double |> store_into_reg t i.res.(0)
  | Isimd Simd.Addq_s64 -> simd_int_binary 64 Add
  | Isimd Simd.Addq_s32 -> simd_int_binary 32 Add
  | Isimd Simd.Addq_s16 -> simd_int_binary 16 Add
  | Isimd Simd.Addq_s8 -> simd_int_binary 8 Add
  | Isimd Simd.Subq_s64 -> simd_int_binary 64 Sub
  | Isimd Simd.Subq_s32 -> simd_int_binary 32 Sub
  | Isimd Simd.Subq_s16 -> simd_int_binary 16 Sub
  | Isimd Simd.Subq_s8 -> simd_int_binary 8 Sub
  | Isimd Simd.Mulq_s32 -> simd_int_binary 32 Mul
  | Isimd Simd.Mulq_s16 -> simd_int_binary 16 Mul
  | Isimd Simd.Minq_s32 -> simd_int_minmax 32 I.Islt
  | Isimd Simd.Minq_s16 -> simd_int_minmax 16 I.Islt
  | Isimd Simd.Minq_s8 -> simd_int_minmax 8 I.Islt
  | Isimd Simd.Maxq_s32 -> simd_int_minmax 32 I.Isgt
  | Isimd Simd.Maxq_s16 -> simd_int_minmax 16 I.Isgt
  | Isimd Simd.Maxq_s8 -> simd_int_minmax 8 I.Isgt
  | Isimd Simd.Minq_u32 -> simd_int_minmax 32 I.Iult
  | Isimd Simd.Minq_u16 -> simd_int_minmax 16 I.Iult
  | Isimd Simd.Minq_u8 -> simd_int_minmax 8 I.Iult
  | Isimd Simd.Maxq_u32 -> simd_int_minmax 32 I.Iugt
  | Isimd Simd.Maxq_u16 -> simd_int_minmax 16 I.Iugt
  | Isimd Simd.Maxq_u8 -> simd_int_minmax 8 I.Iugt
  | Isimd Simd.Andq_s64 -> simd_int_binary 64 And
  | Isimd Simd.Andq_s32 -> simd_int_binary 32 And
  | Isimd Simd.Andq_s16 -> simd_int_binary 16 And
  | Isimd Simd.Andq_s8 -> simd_int_binary 8 And
  | Isimd Simd.Orrq_s64 -> simd_int_binary 64 Or
  | Isimd Simd.Orrq_s32 -> simd_int_binary 32 Or
  | Isimd Simd.Orrq_s16 -> simd_int_binary 16 Or
  | Isimd Simd.Orrq_s8 -> simd_int_binary 8 Or
  | Isimd Simd.Eorq_s64 -> simd_int_binary 64 Xor
  | Isimd Simd.Eorq_s32 -> simd_int_binary 32 Xor
  | Isimd Simd.Eorq_s16 -> simd_int_binary 16 Xor
  | Isimd Simd.Eorq_s8 -> simd_int_binary 8 Xor
  | Isimd Simd.Negq_s64 -> simd_int_unary 64 `Neg
  | Isimd Simd.Negq_s32 -> simd_int_unary 32 `Neg
  | Isimd Simd.Negq_s16 -> simd_int_unary 16 `Neg
  | Isimd Simd.Negq_s8 -> simd_int_unary 8 `Neg
  | Isimd Simd.Mvnq_s64 -> simd_int_unary 64 `Not
  | Isimd Simd.Mvnq_s32 -> simd_int_unary 32 `Not
  | Isimd Simd.Mvnq_s16 -> simd_int_unary 16 `Not
  | Isimd Simd.Mvnq_s8 -> simd_int_unary 8 `Not
  | Isimd (Simd.Shlq_n_u64 n) -> simd_int_shift_imm 64 Shl n
  | Isimd (Simd.Shlq_n_u32 n) -> simd_int_shift_imm 32 Shl n
  | Isimd (Simd.Shlq_n_u16 n) -> simd_int_shift_imm 16 Shl n
  | Isimd (Simd.Shlq_n_u8 n) -> simd_int_shift_imm 8 Shl n
  | Isimd (Simd.Shrq_n_u64 n) -> simd_int_shift_imm 64 Lshr n
  | Isimd (Simd.Shrq_n_u32 n) -> simd_int_shift_imm 32 Lshr n
  | Isimd (Simd.Shrq_n_u16 n) -> simd_int_shift_imm 16 Lshr n
  | Isimd (Simd.Shrq_n_u8 n) -> simd_int_shift_imm 8 Lshr n
  | Isimd (Simd.Shrq_n_s64 n) -> simd_int_shift_imm 64 Ashr n
  | Isimd (Simd.Shrq_n_s32 n) -> simd_int_shift_imm 32 Ashr n
  | Isimd (Simd.Shrq_n_s16 n) -> simd_int_shift_imm 16 Ashr n
  | Isimd (Simd.Shrq_n_s8 n) -> simd_int_shift_imm 8 Ashr n
  | Isimd Simd.Shlq_u64 -> simd_int_variable_shift 64 "aarch64.neon.ushl"
  | Isimd Simd.Shlq_u32 -> simd_int_variable_shift 32 "aarch64.neon.ushl"
  | Isimd Simd.Shlq_u16 -> simd_int_variable_shift 16 "aarch64.neon.ushl"
  | Isimd Simd.Shlq_u8 -> simd_int_variable_shift 8 "aarch64.neon.ushl"
  | Isimd Simd.Shlq_s64 -> simd_int_variable_shift 64 "aarch64.neon.sshl"
  | Isimd Simd.Shlq_s32 -> simd_int_variable_shift 32 "aarch64.neon.sshl"
  | Isimd Simd.Shlq_s16 -> simd_int_variable_shift 16 "aarch64.neon.sshl"
  | Isimd Simd.Shlq_s8 -> simd_int_variable_shift 8 "aarch64.neon.sshl"
  | Isimd (Simd.Dupq_lane_s64 { lane }) -> simd_int_dup_lane 64 lane
  | Isimd (Simd.Dupq_lane_s32 { lane }) -> simd_int_dup_lane 32 lane
  | Isimd (Simd.Dupq_lane_s16 { lane }) -> simd_int_dup_lane 16 lane
  | Isimd (Simd.Dupq_lane_s8 { lane }) -> simd_int_dup_lane 8 lane
  | Isimd (Simd.Getq_lane_s64 { lane }) -> simd_int_get_lane 64 lane
  | Isimd (Simd.Getq_lane_s32 { lane }) -> simd_int_get_lane 32 lane
  | Isimd (Simd.Getq_lane_s16 { lane }) -> simd_int_get_lane 16 lane
  | Isimd (Simd.Getq_lane_s8 { lane }) -> simd_int_get_lane 8 lane
  | Isimd (Simd.Setq_lane_s64 { lane }) -> simd_int_set_lane 64 lane
  | Isimd (Simd.Setq_lane_s32 { lane }) -> simd_int_set_lane 32 lane
  | Isimd (Simd.Setq_lane_s16 { lane }) -> simd_int_set_lane 16 lane
  | Isimd (Simd.Setq_lane_s8 { lane }) -> simd_int_set_lane 8 lane
  | Isimd Simd.Movl_s32 -> simd_int_widen_low 32 Sext
  | Isimd Simd.Movl_u32 -> simd_int_widen_low 32 Zext
  | Isimd Simd.Movl_s16 -> simd_int_widen_low 16 Sext
  | Isimd Simd.Movl_u16 -> simd_int_widen_low 16 Zext
  | Isimd Simd.Movl_s8 -> simd_int_widen_low 8 Sext
  | Isimd Simd.Movl_u8 -> simd_int_widen_low 8 Zext
  | Isimd Simd.Cvtq_f64_s64 ->
    simd_convert
      (int_vec_type ~width_in_bits:64)
      (float_vec_type ~width:Cmm.Float64)
      Sitofp
  | Isimd Simd.Cvtq_f32_s32 ->
    simd_convert
      (int_vec_type ~width_in_bits:32)
      (float_vec_type ~width:Cmm.Float32)
      Sitofp
  | Isimd Simd.Cvtq_s64_f64 ->
    simd_fp_to_signed_int
      (float_vec_type ~width:Cmm.Float64)
      (int_vec_type ~width_in_bits:64)
      "fcvtzs"
  | Isimd Simd.Cvtnq_s64_f64 ->
    simd_fp_to_signed_int
      (float_vec_type ~width:Cmm.Float64)
      (int_vec_type ~width_in_bits:64)
      "fcvtns"
  | Isimd Simd.Cvtq_s32_f32 ->
    simd_fp_to_signed_int
      (float_vec_type ~width:Cmm.Float32)
      (int_vec_type ~width_in_bits:32)
      "fcvtzs"
  | Isimd Simd.Cvtnq_s32_f32 ->
    simd_fp_to_signed_int
      (float_vec_type ~width:Cmm.Float32)
      (int_vec_type ~width_in_bits:32)
      "fcvtns"
  | Isimd Simd.Cvt_f64_f32 -> simd_cvt_f64_f32 ()
  | Isimd Simd.Cvt_f32_f64 -> simd_cvt_f32_f64 ()
  | Isimd Simd.Movn_s64 -> simd_int_narrow 64 ~high:false
  | Isimd Simd.Movn_s32 -> simd_int_narrow 32 ~high:false
  | Isimd Simd.Movn_s16 -> simd_int_narrow 16 ~high:false
  | Isimd Simd.Movn_high_s64 -> simd_int_narrow 64 ~high:true
  | Isimd Simd.Movn_high_s32 -> simd_int_narrow 32 ~high:true
  | Isimd Simd.Movn_high_s16 -> simd_int_narrow 16 ~high:true
  | Isimd Simd.Qmovn_s64 ->
    simd_int_saturating_narrow 64 ~unsigned:false ~high:false
  | Isimd Simd.Qmovn_s32 ->
    simd_int_saturating_narrow 32 ~unsigned:false ~high:false
  | Isimd Simd.Qmovn_s16 ->
    simd_int_saturating_narrow 16 ~unsigned:false ~high:false
  | Isimd Simd.Qmovn_u32 ->
    simd_int_saturating_narrow 32 ~unsigned:true ~high:false
  | Isimd Simd.Qmovn_u16 ->
    simd_int_saturating_narrow 16 ~unsigned:true ~high:false
  | Isimd Simd.Qmovn_high_s64 ->
    simd_int_saturating_narrow 64 ~unsigned:false ~high:true
  | Isimd Simd.Qmovn_high_s32 ->
    simd_int_saturating_narrow 32 ~unsigned:false ~high:true
  | Isimd Simd.Qmovn_high_s16 ->
    simd_int_saturating_narrow 16 ~unsigned:false ~high:true
  | Isimd Simd.Qmovn_high_u32 ->
    simd_int_saturating_narrow 32 ~unsigned:true ~high:true
  | Isimd Simd.Qmovn_high_u16 ->
    simd_int_saturating_narrow 16 ~unsigned:true ~high:true
  | Isimd Simd.Absq_s8 ->
    simd_unary_intrinsic (int_vec_type ~width_in_bits:8) "aarch64.neon.abs"
  | Isimd Simd.Absq_s16 ->
    simd_unary_intrinsic (int_vec_type ~width_in_bits:16) "aarch64.neon.abs"
  | Isimd Simd.Absq_s32 ->
    simd_unary_intrinsic (int_vec_type ~width_in_bits:32) "aarch64.neon.abs"
  | Isimd Simd.Absq_s64 ->
    simd_unary_intrinsic (int_vec_type ~width_in_bits:64) "aarch64.neon.abs"
  | Isimd Simd.Mullq_s16 -> simd_int_widening_mul 16 Sext ~high:false
  | Isimd Simd.Mullq_high_s16 -> simd_int_widening_mul 16 Sext ~high:true
  | Isimd Simd.Mullq_u16 -> simd_int_widening_mul 16 Zext ~high:false
  | Isimd Simd.Mullq_high_u16 -> simd_int_widening_mul 16 Zext ~high:true
  | Isimd Simd.Qaddq_s8 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:8) "aarch64.neon.sqadd"
  | Isimd Simd.Qaddq_s16 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:16) "aarch64.neon.sqadd"
  | Isimd Simd.Qaddq_u8 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:8) "aarch64.neon.uqadd"
  | Isimd Simd.Qaddq_u16 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:16) "aarch64.neon.uqadd"
  | Isimd Simd.Qsubq_s8 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:8) "aarch64.neon.sqsub"
  | Isimd Simd.Qsubq_s16 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:16) "aarch64.neon.sqsub"
  | Isimd Simd.Qsubq_u8 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:8) "aarch64.neon.uqsub"
  | Isimd Simd.Qsubq_u16 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:16) "aarch64.neon.uqsub"
  | Isimd Simd.Paddq_s8 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:8) "aarch64.neon.addp"
  | Isimd Simd.Paddq_s16 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:16) "aarch64.neon.addp"
  | Isimd Simd.Paddq_s32 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:32) "aarch64.neon.addp"
  | Isimd Simd.Paddq_s64 ->
    simd_binary_intrinsic (int_vec_type ~width_in_bits:64) "aarch64.neon.addp"
  | Isimd Simd.Zip1_f32 ->
    simd_zip (T.Vector { num_of_elems = 2; elem_type = T.float }) ~high:false
  | Isimd Simd.Zip1q_s8 -> simd_zip (int_vec_type ~width_in_bits:8) ~high:false
  | Isimd Simd.Zip2q_s8 -> simd_zip (int_vec_type ~width_in_bits:8) ~high:true
  | Isimd Simd.Zip1q_s16 ->
    simd_zip (int_vec_type ~width_in_bits:16) ~high:false
  | Isimd Simd.Zip2q_s16 -> simd_zip (int_vec_type ~width_in_bits:16) ~high:true
  | Isimd Simd.Zip1q_f32 ->
    simd_zip (float_vec_type ~width:Cmm.Float32) ~high:false
  | Isimd Simd.Zip2q_f32 ->
    simd_zip (float_vec_type ~width:Cmm.Float32) ~high:true
  | Isimd Simd.Zip1q_f64 ->
    simd_zip (float_vec_type ~width:Cmm.Float64) ~high:false
  | Isimd Simd.Zip2q_f64 ->
    simd_zip (float_vec_type ~width:Cmm.Float64) ~high:true
  | Isimd (Simd.Extq_u8 n) -> simd_extq_u8 n
  | Isimd (Simd.Copyq_laneq_s64 { src_lane; dst_lane }) ->
    simd_copyq_laneq_s64 ~src_lane ~dst_lane
  | Isimd Simd.Addq_f32 -> simd_float_binary Cmm.Float32 Fadd
  | Isimd Simd.Subq_f32 -> simd_float_binary Cmm.Float32 Fsub
  | Isimd Simd.Mulq_f32 -> simd_float_binary Cmm.Float32 Fmul
  | Isimd Simd.Divq_f32 -> simd_float_binary Cmm.Float32 Fdiv
  | Isimd Simd.Addq_f64 -> simd_float_binary Cmm.Float64 Fadd
  | Isimd Simd.Subq_f64 -> simd_float_binary Cmm.Float64 Fsub
  | Isimd Simd.Mulq_f64 -> simd_float_binary Cmm.Float64 Fmul
  | Isimd Simd.Divq_f64 -> simd_float_binary Cmm.Float64 Fdiv
  | Isimd Simd.Paddq_f32 ->
    simd_binary_intrinsic
      (float_vec_type ~width:Cmm.Float32)
      "aarch64.neon.faddp"
  | Isimd Simd.Paddq_f64 ->
    simd_binary_intrinsic
      (float_vec_type ~width:Cmm.Float64)
      "aarch64.neon.faddp"
  | Isimd (Simd.Cmp_f32 cond) -> simd_float_cmp Cmm.Float32 cond
  | Isimd (Simd.Cmp_f64 cond) -> simd_float_cmp Cmm.Float64 cond
  | Isimd (Simd.Cmpz_s64 cond) -> simd_int_cmp 64 cond ~zero:true
  | Isimd (Simd.Cmpz_s32 cond) -> simd_int_cmp 32 cond ~zero:true
  | Isimd (Simd.Cmpz_s16 cond) -> simd_int_cmp 16 cond ~zero:true
  | Isimd (Simd.Cmpz_s8 cond) -> simd_int_cmp 8 cond ~zero:true
  | Isimd (Simd.Cmp_s64 cond) -> simd_int_cmp 64 cond ~zero:false
  | Isimd (Simd.Cmp_s32 cond) -> simd_int_cmp 32 cond ~zero:false
  | Isimd (Simd.Cmp_s16 cond) -> simd_int_cmp 16 cond ~zero:false
  | Isimd (Simd.Cmp_s8 cond) -> simd_int_cmp 8 cond ~zero:false
  | Isimd Simd.Minq_f32 -> simd_float_minmax Cmm.Float32 "minimum"
  | Isimd Simd.Maxq_f32 -> simd_float_minmax Cmm.Float32 "maximum"
  | Isimd Simd.Minq_f64 -> simd_float_minmax Cmm.Float64 "minimum"
  | Isimd Simd.Maxq_f64 -> simd_float_minmax Cmm.Float64 "maximum"
  | Isimd Simd.Sqrtq_f32 -> simd_float_unary_intrinsic Cmm.Float32 "sqrt"
  | Isimd Simd.Sqrtq_f64 -> simd_float_unary_intrinsic Cmm.Float64 "sqrt"
  | Isimd Simd.Recpeq_f32 ->
    simd_float_unary_intrinsic Cmm.Float32 "aarch64.neon.frecpe"
  | Isimd Simd.Rsqrteq_f32 ->
    simd_float_unary_intrinsic Cmm.Float32 "aarch64.neon.frsqrte"
  | Isimd Simd.Rsqrteq_f64 ->
    simd_float_unary_intrinsic Cmm.Float64 "aarch64.neon.frsqrte"
  | Isimd (Simd.Roundq_f32 mode) -> simd_float_round Cmm.Float32 mode
  | Isimd (Simd.Roundq_f64 mode) -> simd_float_round Cmm.Float64 mode
  | _ -> not_implemented_basic ~msg:"specific" i

(* CR yusumez: Implement atomic operations properly, since the current
   implementation is most likely incorrect. Check how the C++ memory model (or
   whatever LLVM supports the closest to ours) matches with OCaml's (memory.c is
   a nice place to start) *)
let atomic t (i : Cfg.basic Cfg.instruction) (op : Cmm.atomic_op) ~size ~addr =
  let ptr_arg_idx =
    match op with
    | Compare_set -> 2
    | Fetch_and_add -> 1
    | Add | Sub | Land | Lor | Lxor -> 1
    | Exchange -> 1
    | Compare_exchange -> 2
  in
  let typ =
    match (size : Cmm.atomic_bitwidth) with
    | Thirtytwo -> T.i32
    | Sixtyfour | Word -> T.i64
  in
  let arg = load_reg_to_temp ~typ t i.arg.(ptr_arg_idx - 1) in
  let ptr = load_address_from_reg t addr i.arg.(ptr_arg_idx) |> cast_to_ptr t in
  let do_atomicrmw ?(set_res = false) op =
    let res = emit_ins t (I.atomicrmw op ~ptr ~arg) in
    if set_res then store_into_reg t i.res.(0) res
  in
  let do_cmpxchg () =
    let compare_with = load_reg_to_temp ~typ t i.arg.(0) in
    let res = emit_ins t (I.cmpxchg ~ptr ~compare_with ~set_if_equal:arg) in
    match extract_struct t res [[0] (* loaded *); [1] (* success *)] with
    | [loaded; success] -> loaded, success
    | [] | _ :: _ -> fail "atomic.do_cmpxchg"
  in
  match op with
  | Fetch_and_add -> do_atomicrmw ~set_res:true Atomicrmw_add
  | Add -> do_atomicrmw Atomicrmw_add
  | Sub -> do_atomicrmw Atomicrmw_sub
  | Land -> do_atomicrmw Atomicrmw_and
  | Lor -> do_atomicrmw Atomicrmw_or
  | Lxor -> do_atomicrmw Atomicrmw_xor
  | Exchange -> do_atomicrmw ~set_res:true Atomicrmw_xchg
  | Compare_set ->
    let _, success = do_cmpxchg () in
    (* convert i1 -> i64 *)
    let res = emit_ins t (I.convert Zext ~arg:success ~to_:typ) in
    store_into_reg t i.res.(0) res
  | Compare_exchange ->
    let loaded, success = do_cmpxchg () in
    let orig = load_reg_to_temp ~typ t i.res.(0) in
    let selected =
      emit_ins t (I.select ~cond:success ~ifso:orig ~ifnot:loaded)
    in
    store_into_reg t i.res.(0) selected

let load t (i : Cfg.basic Cfg.instruction) (memory_chunk : Cmm.memory_chunk)
    (addr_mode : Arch.addressing_mode) ~(is_atomic : bool) =
  let ptr = load_address_from_reg t addr_mode i.arg.(0) |> cast_to_ptr t in
  let load_op typ =
    if is_atomic
    then
      match memory_chunk with
      | Word_int | Word_val ->
        emit_ins_no_res t (I.fence Acquire);
        I.load_atomic ~ordering:Seq_cst ~ptr ~typ
      | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ->
        fail_msg ~name:"load" "unsupported atomic load chunk"
    else I.load ~ptr ~typ
  in
  let basic typ =
    let loaded = emit_ins t (load_op typ) in
    store_into_reg t i.res.(0) loaded
  in
  let extend op ~from ~to_ =
    let loaded = emit_ins t (I.load ~ptr ~typ:from) in
    let extended = emit_ins t (I.convert op ~arg:loaded ~to_) in
    store_into_reg t i.res.(0) extended
  in
  match memory_chunk with
  | Word_int -> basic T.i64
  | Word_val -> basic T.val_ptr
  | Byte_unsigned -> extend Zext ~from:T.i8 ~to_:T.i64
  | Byte_signed -> extend Sext ~from:T.i8 ~to_:T.i64
  | Sixteen_unsigned -> extend Zext ~from:T.i16 ~to_:T.i64
  | Sixteen_signed -> extend Sext ~from:T.i16 ~to_:T.i64
  | Thirtytwo_unsigned -> extend Zext ~from:T.i32 ~to_:T.i64
  | Thirtytwo_signed -> extend Sext ~from:T.i32 ~to_:T.i64
  | Single { reg = Float32 } -> basic T.float
  | Double -> basic T.double
  | Single { reg = Float64 } -> extend Fpext ~from:T.float ~to_:T.double
  | Onetwentyeight_unaligned ->
    let loaded = emit_ins t (I.load_with_align ~align:1 ~ptr ~typ:T.vec128) in
    store_into_reg t i.res.(0) loaded
  | Onetwentyeight_aligned -> basic T.vec128
  | Twofiftysix_unaligned ->
    let loaded = emit_ins t (I.load_with_align ~align:1 ~ptr ~typ:T.vec256) in
    store_into_reg t i.res.(0) loaded
  | Twofiftysix_aligned -> basic T.vec256
  | Fivetwelve_unaligned ->
    let loaded = emit_ins t (I.load_with_align ~align:1 ~ptr ~typ:T.vec512) in
    store_into_reg t i.res.(0) loaded
  | Fivetwelve_aligned -> basic T.vec512

let store t (i : Cfg.basic Cfg.instruction) (memory_chunk : Cmm.memory_chunk)
    (addr_mode : Arch.addressing_mode) ~(is_modify : bool) =
  let ptr = load_address_from_reg t addr_mode i.arg.(1) |> cast_to_ptr t in
  let emit_modify_store_barrier () =
    if is_modify
    then
      match Target_system.architecture (), memory_chunk with
      | AArch64, (Word_int | Word_val) -> emit_ins_no_res t (I.fence Acquire)
      | (IA32 | X86_64 | ARM | POWER | Z | Riscv), _
      | ( AArch64,
          ( Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
          | Thirtytwo_unsigned | Thirtytwo_signed | Single _ | Double
          | Onetwentyeight_unaligned | Onetwentyeight_aligned
          | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
          | Fivetwelve_aligned ) ) ->
        ()
  in
  emit_modify_store_barrier ();
  let basic typ =
    let to_store = load_reg_to_temp ~typ t i.arg.(0) in
    emit_ins_no_res t (I.store ~ptr ~to_store)
  in
  let basic_raw_word typ =
    let to_store = load_reg_to_temp ~typ t i.arg.(0) in
    emit_ins_no_res t (I.store_volatile ~ptr ~to_store)
  in
  let trunc op to_ =
    let arg = load_reg_to_temp t i.arg.(0) in
    let to_store = emit_ins t (I.convert op ~arg ~to_) in
    emit_ins_no_res t (I.store ~ptr ~to_store)
  in
  match memory_chunk with
  | Word_int -> basic T.i64
  | Word_val ->
    let typ = T.of_reg i.arg.(0) in
    if T.equal typ T.val_ptr then basic typ else basic_raw_word typ
  | Byte_unsigned | Byte_signed -> trunc Trunc T.i8
  | Sixteen_unsigned | Sixteen_signed -> trunc Trunc T.i16
  | Thirtytwo_signed | Thirtytwo_unsigned -> trunc Trunc T.i32
  | Single { reg = Float32 } -> basic T.float
  | Double -> basic T.double
  | Single { reg = Float64 } -> trunc Fptrunc T.float
  | Onetwentyeight_unaligned ->
    let to_store = load_reg_to_temp ~typ:T.vec128 t i.arg.(0) in
    emit_ins_no_res t (I.store_with_align ~align:1 ~ptr ~to_store)
  | Onetwentyeight_aligned -> basic T.vec128
  | Twofiftysix_unaligned ->
    let to_store = load_reg_to_temp ~typ:T.vec256 t i.arg.(0) in
    emit_ins_no_res t (I.store_with_align ~align:1 ~ptr ~to_store)
  | Twofiftysix_aligned -> basic T.vec256
  | Fivetwelve_unaligned ->
    let to_store = load_reg_to_temp ~typ:T.vec512 t i.arg.(0) in
    emit_ins_no_res t (I.store_with_align ~align:1 ~ptr ~to_store)
  | Fivetwelve_aligned -> basic T.vec512

let call_runtime_for_basic_safepoint ?alloc_info ?unwind_label ~attrs ~cc t i
    name args res_types msg =
  let slow_path_roots, live_roots =
    roots_for_basic_safepoint ?unwind_label t i msg
  in
  store_slow_path_roots t slow_path_roots;
  let res =
    call_simple ~attrs ~live_roots ~slow_path_roots ?alloc_info ?unwind_label
      ~cc t name args res_types
  in
  refresh_slow_path_roots t slow_path_roots;
  res

let local_alloc t (i : Cfg.basic Cfg.instruction) num_bytes =
  (* Make space on the local stack *)
  let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
  let local_sp = emit_ins t (I.load ~ptr:local_sp_ptr ~typ:T.i64) in
  let new_local_sp =
    emit_ins t (I.binary Sub ~arg1:local_sp ~arg2:(V.of_int num_bytes))
  in
  emit_ins_no_res t (I.store ~ptr:local_sp_ptr ~to_store:new_local_sp);
  (* Check if new_local_sp exceeds local_limit *)
  let local_limit_ptr = load_domainstate_addr t Domain_local_limit in
  let local_limit = emit_ins t (I.load ~ptr:local_limit_ptr ~typ:T.i64) in
  let skip_realloc =
    emit_ins t (I.icmp Isle ~arg1:local_limit ~arg2:new_local_sp)
  in
  (* Let LLVM know calling realloc isn't likely *)
  let skip_realloc_expect =
    call_llvm_intrinsic t "expect.i1" [skip_realloc; V.of_int ~typ:T.i1 1] T.i1
  in
  (* Branch appropriately *)
  let call_realloc = V.of_label (Cmm.new_label ()) in
  let after_realloc = V.of_label (Cmm.new_label ()) in
  emit_ins_no_res t
    (I.br_cond ~cond:skip_realloc_expect ~ifso:after_realloc ~ifnot:call_realloc);
  (* Call realloc *)
  emit_label t call_realloc;
  (* CR yusumez: Handle SIMD regs appropriately once we have them *)
  add_referenced_symbol t "caml_call_local_realloc";
  call_runtime_for_basic_safepoint
    ~attrs:(gc_attr ~can_call_gc:true t i @ [LL.Fn_attr.Cold])
    ~cc:Oxcaml_alloc t i "caml_call_local_realloc" [] [] "local realloc"
  |> ignore;
  emit_ins_no_res t (I.br after_realloc);
  (* After alloc *)
  emit_label t after_realloc;
  let local_top_ptr = load_domainstate_addr t Domain_local_top in
  let local_top = emit_ins t (I.load ~ptr:local_top_ptr ~typ:T.i64) in
  (* Calculate the address of the object on the local stack *)
  let new_local_sp_addr =
    emit_ins t (I.binary Add ~arg1:new_local_sp ~arg2:local_top)
  in
  (* Skip the header word *)
  let res = do_offset t new_local_sp_addr T.val_ptr 8 in
  store_into_reg t i.res.(0) res

let call_gc_for_basic_safepoint ?alloc_info ?unwind_label ~attrs t i =
  add_referenced_symbol t "caml_call_gc";
  let slow_path_roots, live_roots =
    roots_for_basic_safepoint ?unwind_label t i "basic safepoint"
  in
  store_slow_path_roots t slow_path_roots;
  call_simple ~attrs ~live_roots ~slow_path_roots ?alloc_info ?unwind_label
    ~cc:Oxcaml_alloc t "caml_call_gc" [] []
  |> ignore;
  refresh_slow_path_roots t slow_path_roots

let heap_alloc ?unwind_label ?exn_entry t (i : Cfg.basic Cfg.instruction)
    num_bytes alloc_info =
  (* Make space on the minor heap *)
  let alloc_ptr = emit_ins t (I.load ~ptr:allocation_ptr ~typ:T.i64) in
  let new_alloc_ptr =
    emit_ins t (I.binary Sub ~arg1:alloc_ptr ~arg2:(V.of_int num_bytes))
  in
  emit_ins_no_res t (I.store ~ptr:allocation_ptr ~to_store:new_alloc_ptr);
  (* Check if we exceeded the limit *)
  let domain_young_limit =
    let ptr = load_domainstate_addr t Domain_young_limit in
    emit_ins t (I.load ~ptr ~typ:T.i64)
  in
  let skip_gc =
    emit_ins t (I.icmp Iule ~arg1:domain_young_limit ~arg2:new_alloc_ptr)
  in
  (* Let LLVM know calling GC isn't likely *)
  let skip_gc_expect =
    call_llvm_intrinsic t "expect.i1" [skip_gc; V.of_int ~typ:T.i1 1] T.i1
  in
  (* Branch appropriately *)
  let call_gc = V.of_label (Cmm.new_label ()) in
  let after_gc = V.of_label (Cmm.new_label ()) in
  emit_ins_no_res t
    (I.br_cond ~cond:skip_gc_expect ~ifso:after_gc ~ifnot:call_gc);
  (* Call GC *)
  emit_label t call_gc;
  call_gc_for_basic_safepoint
    ~attrs:(gc_attr ~alloc_info ~can_call_gc:true t i @ [LL.Fn_attr.Cold])
    ~alloc_info ?unwind_label t i;
  emit_ins_no_res t (I.br after_gc);
  emit_unwind_landingpad_after t unwind_label exn_entry;
  (* After GC *)
  emit_label t after_gc;
  (* Load alloc ptr again since GC call might have changed it *)
  let alloc_ptr = emit_ins t (I.load ~ptr:allocation_ptr ~typ:T.i64) in
  (* Skip the header word *)
  let res = do_offset t alloc_ptr T.val_ptr 8 in
  store_into_reg t i.res.(0) res

let poll ?unwind_label ?exn_entry t (i : Cfg.basic Cfg.instruction) =
  let alloc_ptr = emit_ins t (I.load ~ptr:allocation_ptr ~typ:T.i64) in
  let domain_young_limit =
    let ptr = load_domainstate_addr t Domain_young_limit in
    emit_ins t (I.load ~ptr ~typ:T.i64)
  in
  (* The normal amd64 backend calls into the GC/poll path when [alloc_ptr <=
     young_limit]. *)
  let skip_poll =
    emit_ins t (I.icmp Iult ~arg1:domain_young_limit ~arg2:alloc_ptr)
  in
  let skip_poll_expect =
    call_llvm_intrinsic t "expect.i1" [skip_poll; V.of_int ~typ:T.i1 1] T.i1
  in
  let call_gc = V.of_label (Cmm.new_label ()) in
  let after_poll = V.of_label (Cmm.new_label ()) in
  emit_ins_no_res t
    (I.br_cond ~cond:skip_poll_expect ~ifso:after_poll ~ifnot:call_gc);
  emit_label t call_gc;
  call_gc_for_basic_safepoint
    ~attrs:
      (gc_attr
         ~safepoint:
           (Safepoint.Poll
              { stack_offset = statepoint_stack_offset t i;
                active_trap_bytes = active_trap_bytes t i
              })
         ~can_call_gc:true t i
      @ [LL.Fn_attr.Cold])
    ?unwind_label t i;
  emit_ins_no_res t (I.br after_poll);
  emit_unwind_landingpad_after t unwind_label exn_entry;
  emit_label t after_poll

let stack_check ?unwind_label ?exn_entry t (i : Cfg.basic Cfg.instruction)
    max_frame_size_bytes =
  match Target_system.architecture () with
  | Target_system.AArch64 ->
    let threshold_offset =
      (Domainstate.stack_ctx_words * Arch.size_addr)
      + Stack_check.stack_threshold_size
    in
    let current_stack_ptr = load_domainstate_addr t Domain_current_stack in
    let current_stack = emit_ins t (I.load ~ptr:current_stack_ptr ~typ:T.i64) in
    let limit =
      emit_ins t
        (I.binary Add ~arg1:current_stack
           ~arg2:(V.of_int (threshold_offset + max_frame_size_bytes)))
    in
    let sp = read_stack_pointer t in
    let skip_realloc = emit_ins t (I.icmp Iuge ~arg1:sp ~arg2:limit) in
    let skip_realloc_expect =
      call_llvm_intrinsic t "expect.i1"
        [skip_realloc; V.of_int ~typ:T.i1 1]
        T.i1
    in
    let call_realloc = V.of_label (Cmm.new_label ()) in
    let after_realloc = V.of_label (Cmm.new_label ()) in
    emit_ins_no_res t
      (I.br_cond ~cond:skip_realloc_expect ~ifso:after_realloc
         ~ifnot:call_realloc);
    emit_label t call_realloc;
    let required_words =
      (Stack_check.stack_threshold_size + Misc.align max_frame_size_bytes 8) / 8
    in
    add_referenced_symbol t "caml_llvm_call_realloc_stack";
    call_runtime_for_basic_safepoint
      ~attrs:(gc_attr ~can_call_gc:true t i @ [LL.Fn_attr.Cold])
      ?unwind_label ~cc:Oxcaml_alloc t i "caml_llvm_call_realloc_stack"
      [V.of_int required_words] [] "stack check"
    |> ignore;
    emit_ins_no_res t (I.br after_realloc);
    emit_unwind_landingpad_after t unwind_label exn_entry;
    emit_label t after_realloc
  | Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
  | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
    fail_msg ~name:"stack_check" "unsupported architecture for LLVM backend"

let unwind_for_instruction t i =
  let exn_entry = exn_entry_for_instruction t i in
  unwind_label_and_landingpad_for_exn_entry exn_entry

let ordinary_trap_unwind_for_basic_safepoint t
    (i : Cfg.basic Cfg.instruction) =
  (* Only operations that may transfer through [Caml_state->exn_handler] should
     unwind to the active ordinary trap. Polls, minor-heap allocation slow paths
     and AArch64 LLVM stack-check failure use async/effect runtime transfer
     mechanisms instead. They still need their normal safepoint statepoint IDs
     and root metadata, but not an ordinary exception successor. *)
  match i.desc with
  | Op (Poll | Alloc { mode = Heap; _ }) | Stack_check _ -> None, None
  | Op (Alloc { mode = Local; _ })
  | Reloadretaddr | Prologue | Epilogue | Pushtrap _ | Poptrap _
  | Op
      ( Move | Spill | Reload | Opaque | Pause | Begin_region | End_region
      | Dls_get | Tls_get | Domain_index | Const_int _ | Const_float32 _
      | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
      | Const_vec512 _ | Stackoffset _ | Load _
      | Store (_, _, _)
      | Intop _ | Int128op _
      | Intop_imm (_, _)
      | Intop_atomic _
      | Floatop (_, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Specific _ | Name_for_debugger _ ) ->
    unwind_for_instruction t i

let aarch64_trap_recover_type = T.(Struct [val_ptr; i64; i64; i64])

let emit_aarch64_pushtrap t (i : Cfg.basic Cfg.instruction) lbl_handler =
  let try_label = V.of_label (Cmm.new_label ()) in
  let exn_entry = V.of_label (Cmm.new_label ()) in
  let fun_ident = E.get_fun_ident (get_fun_info t).emitter in
  let fun_name = LL.Ident.to_string_hum fun_ident in
  let label_name = LL.Ident.to_string_hum (V.get_ident_exn exn_entry) in
  let recover_rbp_asm_ident =
    LL.Ident.global (fun_name ^ ".recover_rbp_asm." ^ label_name)
  in
  let recover_rbp_var_ident =
    LL.Ident.global (fun_name ^ ".recover_rbp_var." ^ label_name)
  in
  let handler_bucket =
    match
      Label.Tbl.find_opt (get_fun_info t).trap_handler_exn_buckets lbl_handler
    with
    | Some handler_bucket -> handler_bucket
    | None -> fail_msg ~name:"pushtrap" "missing preallocated handler bucket"
  in
  let recovery_target =
    V.blockaddress ~func:fun_ident ~block:(V.get_ident_exn exn_entry)
  in
  call_llvm_intrinsic_no_res t "aarch64.oxcaml.push.trap" [recovery_target];
  emit_ins_no_res t (I.br try_label);
  emit_label t exn_entry;
  ignore (emit_ins t (I.landingpad ~typ:T._token ~cleanup:true));
  let recovered =
    call_llvm_intrinsic t "aarch64.oxcaml.trap.recover" []
      aarch64_trap_recover_type
  in
  let exn_bucket, recovered_alloc, recovered_ds =
    match extract_struct t recovered [[0]; [2]; [3]] with
    | [exn_bucket; recovered_alloc; recovered_ds] ->
      exn_bucket, recovered_alloc, recovered_ds
    | _ -> Misc.fatal_error "unexpected trap recover result arity"
  in
  emit_ins_no_res t (I.store ~ptr:handler_bucket ~to_store:exn_bucket);
  emit_ins_no_res t (I.store ~ptr:domainstate_ptr ~to_store:recovered_ds);
  emit_ins_no_res t (I.store ~ptr:allocation_ptr ~to_store:recovered_alloc);
  emit_ins_no_res t (I.br (V.of_label lbl_handler));
  emit_label t try_label;
  match
    InstructionId.Tbl.find_opt (get_fun_info t).aarch64_trap_blocks i.id
  with
  | Some _ -> fail_msg "duplicate pushtrap instruction id"
  | None ->
    InstructionId.Tbl.add (get_fun_info t).aarch64_trap_blocks i.id
      { trap_block = V.poison T.ptr;
        stacksave_ptr = None;
        exn_bucket;
        exn_entry;
        recover_rbp_asm_ident;
        recover_rbp_var_ident
      }

let basic_op t (i : Cfg.basic Cfg.instruction) (op : Operation.t) =
  match op with
  | Move -> load_reg_to_temp t i.arg.(0) |> store_into_reg t i.res.(0)
  | Opaque ->
    let temp = load_reg_to_temp t i.arg.(0) in
    let typ = V.get_type temp in
    let opaque_temp =
      match[@warning "-fragile-match"] typ with
      | T.Vector _ ->
        emit_ins t
          (I.inline_asm ~asm:"" ~constraints:"=w,0" ~args:[temp]
             ~res_type:(Some typ) ~sideeffect:false)
      | _ ->
        emit_ins t
          (I.inline_asm ~asm:"" ~constraints:"=r,0" ~args:[temp]
             ~res_type:(Some typ) ~sideeffect:false)
    in
    store_into_reg t i.res.(0) opaque_temp
  | Const_int n ->
    if preserve_reg_slot t i.res.(0)
    then store_into_reg t i.res.(0) (V.of_nativeint n);
    set_const_int_for_reg t i.res.(0) n
  | Const_symbol { sym_name; sym_global = _ } ->
    add_referenced_symbol t sym_name;
    store_into_reg t i.res.(0) (V.of_symbol sym_name)
  | Const_float32 bits -> store_into_reg t i.res.(0) (V.of_float32_bits bits)
  | Const_float bits -> store_into_reg t i.res.(0) (V.of_float64_bits bits)
  | Const_vec128 { word0; word1 } ->
    let elem word = Format.asprintf "i64 %Ld" word in
    let vector =
      V.imm T.vec128 (Format.asprintf "<%s, %s>" (elem word0) (elem word1))
    in
    store_into_reg t i.res.(0) vector
  | Const_vec256 _ | Const_vec512 _ -> not_implemented_basic ~msg:"const_vec" i
  (* [mutability] is used by CFG optimizations before final lowering. *)
  | Load { memory_chunk; addressing_mode; mutability = _; is_atomic } ->
    load t i memory_chunk addressing_mode ~is_atomic
  | Store (memory_chunk, addressing_mode, is_modify) ->
    store t i memory_chunk addressing_mode ~is_modify
  | Intop op -> int_op t i op ~imm:None
  | Int128op op -> int128_op t i op
  | Intop_imm (op, n) -> int_op t i op ~imm:(Some n)
  | Floatop (width, op) -> float_op t i width op
  | Begin_region ->
    let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
    let local_sp = emit_ins t (I.load ~ptr:local_sp_ptr ~typ:T.i64) in
    store_into_reg t i.res.(0) local_sp
  | End_region ->
    let local_sp_ptr = load_domainstate_addr t Domain_local_sp in
    let saved_local_sp = load_reg_to_temp t i.arg.(0) in
    emit_ins_no_res t (I.store ~ptr:local_sp_ptr ~to_store:saved_local_sp)
  | Alloc { bytes; dbginfo = _; mode = Local } -> local_alloc t i bytes
  | Alloc { bytes; dbginfo; mode = Heap } ->
    let unwind_label, exn_entry = ordinary_trap_unwind_for_basic_safepoint t i in
    heap_alloc ?unwind_label ?exn_entry t i bytes dbginfo
  | Csel test_op ->
    let typ = T.of_reg i.res.(0) in
    let len = Array.length i.arg in
    let ifso = load_reg_to_temp ~typ t i.arg.(len - 2) in
    let ifnot = load_reg_to_temp ~typ t i.arg.(len - 1) in
    let cond = test t test_op i in
    let res = emit_ins t (I.select ~cond ~ifso ~ifnot) in
    store_into_reg t i.res.(0) res
  | Static_cast cast_op -> (
    let do_conv op ~from ~to_ =
      let arg = load_reg_to_temp ~typ:from t i.arg.(0) in
      let converted = emit_ins t (I.convert op ~arg ~to_) in
      store_into_reg t i.res.(0) converted
    in
    let vec128_elem_type (vec128_type : Cmm.vec128_type) =
      match vec128_type with
      | Cmm.Int8x16 -> T.Int { width_in_bits = 8 }
      | Cmm.Int16x8 -> T.Int { width_in_bits = 16 }
      | Cmm.Int32x4 -> T.i32
      | Cmm.Int64x2 -> T.i64
      | Cmm.Float16x8 -> fail_msg ~name:"static cast" "float16 unsupported"
      | Cmm.Float32x4 -> T.float
      | Cmm.Float64x2 -> T.double
    in
    let llvm_vec128_type vec128_type =
      let elem_type = vec128_elem_type vec128_type in
      let width_in_bits =
        match[@warning "-fragile-match"] elem_type with
        | T.Int { width_in_bits } -> width_in_bits
        | T.Float -> 32
        | T.Double -> 64
        | T.Ptr _ | T.Struct _ | T.Array _ | T.Vector _ | T.Label | T.Token
        | T.Metadata ->
          Misc.fatal_error "unexpected vector element type"
      in
      T.Vector { num_of_elems = 128 / width_in_bits; elem_type }
    in
    let scalar_type_for_vec128 vec128_type =
      match vec128_type with
      | Cmm.Int8x16 | Cmm.Int16x8 | Cmm.Int32x4 | Cmm.Int64x2 -> T.i64
      | Cmm.Float16x8 -> fail_msg ~name:"static cast" "float16 unsupported"
      | Cmm.Float32x4 -> T.float
      | Cmm.Float64x2 -> T.double
    in
    match cast_op with
    | Float_of_int width ->
      do_conv Sitofp ~from:T.i64 ~to_:(T.of_float_width width)
    | Int_of_float width ->
      do_conv Fptosi ~from:(T.of_float_width width) ~to_:T.i64
    | Float_of_float32 -> do_conv Fpext ~from:T.float ~to_:T.double
    | Float32_of_float -> do_conv Fptrunc ~from:T.double ~to_:T.float
    | V128_of_scalar vec128_type ->
      let elem_type = vec128_elem_type vec128_type in
      let vector_type = llvm_vec128_type vec128_type in
      let scalar =
        load_reg_to_temp ~typ:(scalar_type_for_vec128 vec128_type) t i.arg.(0)
      in
      let elem =
        match[@warning "-fragile-match"] elem_type with
        | T.Int { width_in_bits } when width_in_bits < 64 ->
          emit_ins t (I.convert Trunc ~arg:scalar ~to_:elem_type)
        | _ -> scalar
      in
      let vector =
        emit_ins t
          (I.insertelement ~vector:(V.poison vector_type) ~index:(V.of_int 0)
             ~to_insert:elem)
      in
      let vector =
        if T.equal vector_type T.vec128
        then vector
        else emit_ins t (I.convert Bitcast ~arg:vector ~to_:T.vec128)
      in
      store_into_reg t i.res.(0) vector
    | Scalar_of_v128 vec128_type ->
      let elem_type = vec128_elem_type vec128_type in
      let vector_type = llvm_vec128_type vec128_type in
      let vector =
        load_reg_to_temp ~typ:T.vec128 t i.arg.(0) |> fun arg ->
        emit_ins t (I.convert Bitcast ~arg ~to_:vector_type)
      in
      let elem = emit_ins t (I.extractelement ~vector ~index:(V.of_int 0)) in
      let scalar =
        match[@warning "-fragile-match"] elem_type with
        | T.Int { width_in_bits } when width_in_bits < 64 ->
          emit_ins t (I.convert Zext ~arg:elem ~to_:T.i64)
        | _ -> elem
      in
      store_into_reg t i.res.(0) scalar
    | V256_of_scalar _ | Scalar_of_v256 _ | V512_of_scalar _ | Scalar_of_v512 _
      ->
      not_implemented_basic ~msg:"static cast" i)
  | Reinterpret_cast cast_op -> (
    let bitcast arg to_ = emit_ins t (I.convert Bitcast ~arg ~to_) in
    let trunc arg to_ = emit_ins t (I.convert Trunc ~arg ~to_) in
    let zext arg to_ = emit_ins t (I.convert Zext ~arg ~to_) in
    match cast_op with
    | Float32_of_int32 ->
      let arg = load_reg_to_temp ~typ:T.i64 t i.arg.(0) in
      let bits = trunc arg T.i32 in
      bitcast bits T.float |> store_into_reg t i.res.(0)
    | Int32_of_float32 ->
      let arg = load_reg_to_temp ~typ:T.float t i.arg.(0) in
      let bits = bitcast arg T.i32 in
      zext bits T.i64 |> store_into_reg t i.res.(0)
    | Float32_of_float ->
      let arg = load_reg_to_temp ~typ:T.double t i.arg.(0) in
      let bits64 = bitcast arg T.i64 in
      let bits32 = trunc bits64 T.i32 in
      bitcast bits32 T.float |> store_into_reg t i.res.(0)
    | Float_of_float32 ->
      let arg = load_reg_to_temp ~typ:T.float t i.arg.(0) in
      let bits32 = bitcast arg T.i32 in
      let bits64 = zext bits32 T.i64 in
      bitcast bits64 T.double |> store_into_reg t i.res.(0)
    | Int_of_value | Value_of_int | Float_of_int64 | Int64_of_float ->
      let arg = load_reg_to_temp t i.arg.(0) in
      let converted =
        emit_ins t (I.convert Bitcast ~arg ~to_:(T.of_reg i.res.(0)))
      in
      store_into_reg t i.res.(0) converted
    | V128_of_vec Vec128 ->
      let arg = load_reg_to_temp ~typ:T.vec128 t i.arg.(0) in
      let res_typ = T.of_reg i.res.(0) in
      let converted =
        if T.equal (V.get_type arg) res_typ then arg else bitcast arg res_typ
      in
      store_into_reg t i.res.(0) converted
    | V128_of_vec (Vec256 | Vec512) | V256_of_vec _ | V512_of_vec _ ->
      not_implemented_basic ~msg:"vector reinterpret cast" i)
  | Specific op -> specific t i op
  | Intop_atomic { op; size; addr } -> atomic t i op ~size ~addr
  | Pause -> (
    match Target_system.architecture () with
    | Target_system.X86_64 -> call_llvm_intrinsic_no_res t "x86.sse2.pause" []
    | Target_system.AArch64 ->
      emit_ins_no_res t
        (I.inline_asm ~asm:"yield" ~constraints:"" ~args:[]
           ~res_type:T.Or_void.void ~sideeffect:true)
    | Target_system.IA32 | Target_system.ARM | Target_system.POWER
    | Target_system.Z | Target_system.Riscv ->
      not_implemented_basic ~msg:"pause" i)
  | Dls_get ->
    let dls_state_ptr = load_domainstate_addr t Domain_dls_state in
    let dls_state = emit_ins t (I.load ~ptr:dls_state_ptr ~typ:T.i64) in
    store_into_reg t i.res.(0) dls_state
  | Tls_get ->
    let tls_state_ptr = load_domainstate_addr t Domain_tls_state in
    let tls_state = emit_ins t (I.load ~ptr:tls_state_ptr ~typ:T.i64) in
    store_into_reg t i.res.(0) tls_state
  | Domain_index ->
    let domain_id_ptr = load_domainstate_addr t Domain_id in
    let domain_id = emit_ins t (I.load ~ptr:domain_id_ptr ~typ:T.i64) in
    store_into_reg t i.res.(0) domain_id
  | Poll ->
    let unwind_label, exn_entry = ordinary_trap_unwind_for_basic_safepoint t i in
    poll ?unwind_label ?exn_entry t i
  | Stackoffset _ -> () (* Handled separately via [Safepoint.attr] *)
  | Spill | Reload -> not_implemented_basic ~msg:"spill / reload" i
  | Name_for_debugger _ -> ()
  | Probe_is_enabled _ -> not_implemented_basic i

let emit_basic t (i : Cfg.basic Cfg.instruction) =
  emit_comment t "%a" F.pp_dbg_instr_basic i;
  let fun_info = get_fun_info t in
  let prev_dbg_metadata = fun_info.current_dbg_metadata in
  fun_info.current_dbg_metadata
    <- create_debug_location t fun_info.subprogram_dbg_metadata_id i.dbg;
  Fun.protect
    ~finally:(fun () -> fun_info.current_dbg_metadata <- prev_dbg_metadata)
    (fun () ->
      match i.desc with
      | Op op -> basic_op t i op
      | Prologue | Epilogue | Reloadretaddr ->
        () (* LLVM handles these for us *)
      | Stack_check { max_frame_size_bytes } ->
        let unwind_label, exn_entry =
          ordinary_trap_unwind_for_basic_safepoint t i
        in
        stack_check ?unwind_label ?exn_entry t i max_frame_size_bytes
      | Poptrap { lbl_handler } -> (
        let trap_block_info =
          match Target_system.architecture () with
          | Target_system.AArch64 -> (
            match
              InstructionId.Tbl.find_opt (get_fun_info t).active_traps i.id
            with
            | Some (Some { pushtrap_id; trap_handler })
              when Label.equal trap_handler lbl_handler -> (
              match
                InstructionId.Tbl.find_opt (get_fun_info t).aarch64_trap_blocks
                  pushtrap_id
              with
              | Some trap_block_info -> Some trap_block_info
              | None -> None)
            | Some (Some _) | Some None | None -> None)
          | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
          | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
            Label.Tbl.find_opt (get_fun_info t).trap_blocks lbl_handler
        in
        match trap_block_info with
        | None -> fail_msg "unbalanced trap pop"
        | Some { trap_block; stacksave_ptr; _ } -> (
          match Target_system.architecture () with
          | Target_system.AArch64 ->
            call_llvm_intrinsic_no_res t "aarch64.oxcaml.pop.trap" []
          | Target_system.X86_64 -> (
            (* Restore previous exn handler sp (top word on trap block) *)
            let exn_sp_ptr = load_domainstate_addr t Domain_exn_handler in
            let prev_exn_sp = emit_ins t (I.load ~ptr:trap_block ~typ:T.i64) in
            emit_ins_no_res t (I.store ~ptr:exn_sp_ptr ~to_store:prev_exn_sp);
            write_trap_pointer_register t prev_exn_sp;
            (* Pop! *)
            match stacksave_ptr with
            | Some stacksave_ptr ->
              call_llvm_intrinsic_no_res t "stackrestore" [stacksave_ptr]
            | None -> fail_msg ~name:"poptrap" "missing stack save")
          | Target_system.IA32 | Target_system.ARM | Target_system.POWER
          | Target_system.Z | Target_system.Riscv ->
            fail_msg ~name:"poptrap" "unsupported architecture for LLVM backend"
          ))
      | Pushtrap { lbl_handler } -> (
        match Target_system.architecture () with
        | Target_system.AArch64 -> emit_aarch64_pushtrap t i lbl_handler
        | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
        | Target_system.POWER | Target_system.Z | Target_system.Riscv -> (
        (* Exception control flow is implemented in a way that emulates setjmp
           in C. Namely. we call a function that "returns twice", first falling
           through to the try block, second when an exception happens. This
           ensures that LLVM is aware of potential control flow to the handler
           so that it doesn't eliminate it as dead code or does other funny
           things moving blocks around.

           The wrapper function follows the OCaml calling conventions to handle
           spilling/reloading registers. Note that here, we assume spill
           locations are consistent across calls within the try block. We could
           solidify this assumption via using native instructions like [invoke]
           or [callbr].

           Due to an unfortunate series of interactions with LLVM (allocating
           after the entry block causes LLVM to force the use of frame pointers,
           and it fails to save RBP across OCaml calls when this happens), this
           only can work with frame pointers enabled.

           On x86_64, trap blocks allocated by this backend are 4 words wide,
           as opposed to 2, to account for RBP (and padding for alignment). These
           two words are placed below the normal trap block so that the top two
           words are handled as expected by the runtime and functions compiled
           without the LLVM backend. Pushtrap sets this up, while they get torn
           down at trap handler entry.

           To recover RBP in the case of an exception, we can't put that bit of
           code in the trap handler entry since things happen beforehand. So, we
           make exceptions jump to some extra bit of asm written in the
           module-level that recovers RBP. It reads the target from a global
           variable. *)
        let after_wrap_try = V.of_label (Cmm.new_label ()) in
        let wrap_try_res =
          match Target_system.architecture () with
          | Target_system.X86_64 ->
            let wrap_try_res =
              call_simple
                ~attrs:[Returns_twice; Gc_leaf_function]
                ~cc:Oxcaml t "wrap_try" [] [T.i64]
            in
            emit_ins_no_res t (I.br after_wrap_try);
            emit_label t after_wrap_try;
            wrap_try_res
          | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
          | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
            fail_msg ~name:"pushtrap"
              "unsupported architecture for LLVM backend"
        in
        (* Record labels here. Runtime exceptions resume at the machine return
           address of the [wrap_try] call, so LLVM's ordinary returns-twice call
           lowering handles x0/x28/x27 before reaching [after_wrap_try]. *)
        let try_label = V.of_label (Cmm.new_label ()) in
        let exn_entry = V.of_label (Cmm.new_label ()) in
        let fun_name =
          E.get_fun_ident (get_fun_info t).emitter |> LL.Ident.to_string_hum
        in
        let label_name = LL.Ident.to_string_hum (V.get_ident_exn exn_entry) in
        let recover_rbp_asm_ident =
          LL.Ident.global (fun_name ^ ".recover_rbp_asm." ^ label_name)
        in
        let recover_rbp_var_ident =
          LL.Ident.global (fun_name ^ ".recover_rbp_var." ^ label_name)
        in
        let exn_bucket =
          match Target_system.architecture () with
          | Target_system.X86_64 ->
            emit_ins_no_res t
              (I.inline_asm ~asm:"movq $0, %rax" ~constraints:"r"
                 ~args:wrap_try_res ~res_type:T.Or_void.void ~sideeffect:true);
            emit_ins_no_res t (I.br exn_entry);
            emit_label t exn_entry;
            let exn_bucket =
              emit_ins t
                (I.inline_asm ~asm:"mov %rax, $0" ~constraints:"=r" ~args:[]
                   ~res_type:(Some T.i64) ~sideeffect:true)
            in
            (* If it's nonzero, we have an exception. Otherwise, go to the try
               block. *)
            let exn_bucket_is_zero =
              emit_ins t (I.icmp Ieq ~arg1:exn_bucket ~arg2:(V.of_int 0))
            in
            let exn_label = V.of_label lbl_handler in
            emit_ins_no_res t
              (I.br_cond ~cond:exn_bucket_is_zero ~ifso:try_label
                 ~ifnot:exn_label);
            emit_label t try_label;
            exn_bucket
          | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
          | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
            fail_msg ~name:"pushtrap"
              "unsupported architecture for LLVM backend"
        in
        (match Target_system.architecture () with
        | Target_system.X86_64 ->
          (* Take the address of common entry and put it somewhere
             accessible. *)
          emit_ins_no_res t
            (I.store
               ~ptr:(V.of_ident ~typ:T.ptr recover_rbp_var_ident)
               ~to_store:
                 (V.blockaddress
                    ~func:(E.get_fun_ident (get_fun_info t).emitter)
                    ~block:(V.get_ident_exn exn_entry)))
        | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
        | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
          fail_msg ~name:"pushtrap" "unsupported architecture for LLVM backend");
        (* Allocate trap block on stack. It will get allocated at the top of the
           stack. The layout is [prev_sp; handler_addr; frame_pointer_recovery;
           padding] on targets where LLVM uses a frame pointer for spills. *)
        let trap_block_type = trap_block_type () in
        let stacksave_ptr, trap_block =
          match Target_system.architecture () with
          | Target_system.X86_64 ->
            let stacksave_ptr = call_llvm_intrinsic t "stacksave" [] T.ptr in
            let trap_block =
              emit_ins t (I.alloca ~count:(V.of_int 1) trap_block_type)
            in
            Some stacksave_ptr, trap_block
          | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
          | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
            fail_msg ~name:"pushtrap"
              "unsupported architecture for LLVM backend"
        in
        (* Slots on the trap block *)
        let rbp_slot = do_offset t trap_block T.ptr 16 in
        let handler_slot = do_offset t trap_block T.ptr 8 in
        let prev_sp_slot = do_offset t trap_block T.ptr 0 in
        (* Push my trap block to the exn handler list. *)
        let exn_sp_ptr = load_domainstate_addr t Domain_exn_handler in
        let prev_exn_sp =
          match Target_system.architecture () with
          | Target_system.X86_64 ->
            emit_ins t (I.load ~ptr:exn_sp_ptr ~typ:T.i64)
          | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
          | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
            fail_msg ~name:"pushtrap"
              "unsupported architecture for LLVM backend"
        in
        emit_ins_no_res t (I.store ~ptr:exn_sp_ptr ~to_store:trap_block);
        write_trap_pointer_register t trap_block;
        (* Fill up the slots *)
        let handler_addr =
          match Target_system.architecture () with
          | Target_system.X86_64 ->
            V.of_ident ~typ:T.ptr recover_rbp_asm_ident
          | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
          | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
            fail_msg ~name:"pushtrap"
              "unsupported architecture for LLVM backend"
        in
        emit_ins_no_res t (I.store ~ptr:handler_slot ~to_store:handler_addr);
        (match Target_system.architecture () with
        | Target_system.X86_64 ->
          emit_ins_no_res t
            (I.inline_asm ~asm:"mov %rbp, ($0)" ~constraints:"r"
               ~args:[rbp_slot] ~res_type:T.Or_void.void ~sideeffect:true)
        | Target_system.AArch64 | Target_system.IA32 | Target_system.ARM
        | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
          fail_msg ~name:"pushtrap" "unsupported architecture for LLVM backend");
        emit_ins_no_res t (I.store ~ptr:prev_sp_slot ~to_store:prev_exn_sp);
        (* Save the trap block in [t] *)
        match Label.Tbl.find_opt (get_fun_info t).trap_blocks lbl_handler with
        | Some _ -> fail_msg "multiple pushtraps for the same handler"
        | None ->
          Label.Tbl.add (get_fun_info t).trap_blocks lbl_handler
            { trap_block;
              stacksave_ptr;
              exn_bucket;
              exn_entry;
              recover_rbp_asm_ident;
              recover_rbp_var_ident
            })))

(* Cfg translation entry *)

let collect_body_regs cfg =
  Cfg.fold_blocks cfg
    ~f:(fun _ block regs ->
      let body_regs =
        DLL.fold_left block.body
          ~f:(fun regs (instr : Cfg.basic Cfg.instruction) ->
            Reg.add_set_array regs (Array.append instr.res instr.arg))
          ~init:Reg.Set.empty
      in
      let terminator_regs =
        Array.append block.terminator.res block.terminator.arg
      in
      Reg.add_set_array body_regs terminator_regs |> Reg.Set.union regs)
    ~init:Reg.Set.empty

let has_trap_handler cfg =
  Cfg.fold_blocks cfg
    ~f:(fun _ block acc -> acc || block.is_trap_handler)
    ~init:false

let compute_liveness cfg =
  match
    Cfg_liveness.Liveness.run cfg ~init:Cfg_liveness.Domain.bot
      ~map:Cfg_liveness.Liveness.Instr ()
  with
  | Ok liveness -> liveness
  | Aborted _ -> .
  | Max_iterations_reached ->
    fail_msg ~name:"compute_liveness"
      "liveness analysis did not reach a fixpoint"

let compute_active_traps (cfg : Cfg.t) =
  let active_traps = InstructionId.Tbl.create 0 in
  let active_trap_depths = InstructionId.Tbl.create 0 in
  let visited = Label.Tbl.create 0 in
  let equal_active_trap left right =
    InstructionId.equal left.pushtrap_id right.pushtrap_id
    && Label.equal left.trap_handler right.trap_handler
  in
  let top_trap = function [] -> None | lbl :: _ -> Some lbl in
  let record : type a. a Cfg.instruction -> active_trap list -> unit =
   fun i traps ->
    InstructionId.Tbl.replace active_traps i.id (top_trap traps);
    InstructionId.Tbl.replace active_trap_depths i.id (List.length traps)
  in
  let rec update_block label traps =
    match Label.Tbl.find_opt visited label with
    | Some traps' ->
      if not (List.equal equal_active_trap traps traps')
      then
        fail_msg ~name:"compute_active_traps"
          "block %a reached with incompatible trap stacks" Label.print label
    | None ->
      Label.Tbl.add visited label traps;
      let block = Cfg.get_block_exn cfg label in
      let traps =
        DLL.fold_left block.body ~init:traps
          ~f:(fun traps (instr : Cfg.basic Cfg.instruction) ->
            record instr traps;
            match (instr.desc : Cfg.basic) with
            | Cfg.Pushtrap { lbl_handler } ->
              update_block lbl_handler traps;
              { pushtrap_id = instr.id; trap_handler = lbl_handler } :: traps
            | Cfg.Poptrap { lbl_handler } -> (
              match traps with
              | top_trap :: traps when Label.equal lbl_handler top_trap.trap_handler ->
                traps
              | top_trap :: _ ->
                fail_msg ~name:"compute_active_traps"
                  "poptrap label %a does not match active trap %a" Label.print
                  lbl_handler Label.print top_trap.trap_handler
              | [] ->
                fail_msg ~name:"compute_active_traps"
                  "poptrap label %a with no active trap" Label.print lbl_handler
              )
            | Cfg.Op
                ( Stackoffset _ | Move | Spill | Reload | Const_int _
                | Const_float _ | Const_float32 _ | Const_symbol _
                | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Load _
                | Store _ | Intop _ | Int128op _ | Intop_imm _ | Intop_atomic _
                | Floatop _ | Csel _ | Static_cast _ | Reinterpret_cast _
                | Probe_is_enabled _ | Opaque | Begin_region | End_region
                | Specific _ | Name_for_debugger _ | Dls_get | Tls_get
                | Domain_index | Poll | Pause | Alloc _ )
            | Cfg.Reloadretaddr | Cfg.Prologue | Cfg.Epilogue
            | Cfg.Stack_check _ ->
              traps)
      in
      record block.terminator traps;
      Label.Set.iter
        (fun label -> update_block label traps)
        (Cfg.successor_labels ~normal:true ~exn:false block)
  in
  update_block cfg.entry_label [];
  active_traps, active_trap_depths

let live_val_regs_across liveness (i : 'a Cfg.instruction) =
  match InstructionId.Tbl.find_opt liveness i.id with
  | None -> Reg.Set.empty
  | Some { Cfg_liveness.before = _; across } ->
    Reg.Set.filter (fun reg -> Cmm.is_val reg.typ) across

let terminator_has_gc_safepoint (i : Cfg.terminator Cfg.instruction) =
  match i.desc with
  | Call _ -> true
  | Call_no_return { alloc; stack_ofs; _ } -> alloc || stack_ofs > 0
  | Prim { op = External { alloc; stack_ofs; _ }; _ } -> alloc || stack_ofs > 0
  | Invalid { stack_ofs; _ } -> stack_ofs > 0
  | Raise (Raise_regular | Raise_reraise) -> true
  | Raise Raise_notrace -> false
  | Tailcall_func _ | Tailcall_self _ | Return | Never | Always _
  | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ | Switch _
  | Prim { op = Probe _; _ } ->
    false

let preserved_reg_slots liveness active_traps cfg =
  let across_basic_safepoints =
    Cfg.fold_body_instructions cfg
      ~f:(fun acc i ->
        if
          basic_has_gc_safepoint i
          && not (eligible_basic_safepoint_for_slow_root_slots active_traps i)
        then Reg.Set.union acc (live_val_regs_across liveness i)
        else acc)
      ~init:Reg.Set.empty
  in
  let across_terminator_safepoints =
    Cfg.fold_blocks cfg
      ~f:(fun _ block acc ->
        let i = block.terminator in
        if terminator_has_gc_safepoint i
        then Reg.Set.union acc (live_val_regs_across liveness i)
        else acc)
      ~init:Reg.Set.empty
  in
  let across_traps =
    Cfg.fold_blocks cfg
      ~f:(fun _ block acc ->
        match block.exn with
        | None -> acc
        | Some _ ->
          if Cfg.can_raise_terminator block.terminator.desc
          then
            match InstructionId.Tbl.find_opt liveness block.terminator.id with
            | None -> acc
            | Some { Cfg_liveness.before = _; across } ->
              Reg.Set.union acc across
          else acc)
      ~init:Reg.Set.empty
  in
  Reg.Set.union
    (Reg.Set.union across_basic_safepoints across_terminator_safepoints)
    across_traps

let max_slow_path_root_slots liveness active_traps cfg =
  Cfg.fold_body_instructions cfg
    ~f:(fun max_roots i ->
      if
        eligible_basic_safepoint_for_slow_root_slots active_traps i
        (* [const_ints] and [reg2alloca] are populated while emitting
           instructions, so this pre-emission count can only use the
           conservative live root set. The actual slow-path bundle still filters
           known immediates and non-alloca roots at the call site. *)
      then
        Int.max max_roots (live_val_regs_across liveness i |> Reg.Set.cardinal)
      else max_roots)
    ~init:0

let reg_listed_in_signature (reg : Reg.t) =
  match reg.loc with
  | Reg _ | Stack (Incoming _) -> true
  | Stack (Local _ | Outgoing _ | Domainstate _) -> false
  | Unknown -> fail "reg_listed_in_signature"

let cfg_required_stack_check_bytes (cfg : Cfg.t) =
  Cfg.fold_body_instructions cfg ~init:0
    ~f:(fun max_bytes (i : Cfg.basic Cfg.instruction) ->
      match[@ocaml.warning "-fragile-match"] i.desc with
      | Cfg.Stack_check { max_frame_size_bytes } ->
        max max_bytes max_frame_size_bytes
      | _ -> max_bytes)

let cfg_stack_check_before_bytes (cfg : Cfg.t) =
  let visited = Label.Tbl.create 17 in
  let max_bytes = ref 0 in
  let rec visit label =
    if not (Label.Tbl.mem visited label)
    then (
      Label.Tbl.add visited label ();
      let block = Cfg.get_block_exn cfg label in
      let found_stack_check =
        DLL.fold_left block.body ~init:false
          ~f:(fun found_stack_check (i : Cfg.basic Cfg.instruction) ->
            if found_stack_check
            then true
            else (
              max_bytes := max !max_bytes i.stack_offset;
              match[@ocaml.warning "-fragile-match"] i.desc with
              | Cfg.Stack_check _ -> true
              | _ -> false))
      in
      if not found_stack_check
      then (
        max_bytes := max !max_bytes block.terminator.stack_offset;
        Cfg.successor_labels ~normal:true ~exn:true block
        |> Label.Set.iter visit))
  in
  visit cfg.entry_label;
  !max_bytes

let fun_attrs ~has_try:_ ~cfg_stack_check_bytes ~cfg_stack_check_before_bytes
    codegen_options =
  let open LL.Fn_attr in
  let safepoint_attrs =
    (* Statepoint IDs encode the active stack adjustment at the call site.
       Inlining can move a call into a context with a different adjustment. *)
    [Noinline]
  in
  let gc_attrs = [Gc gc_name] in
  let frame_pointer_attrs =
    match Target_system.architecture (), Config.no_stack_checks with
    | Target_system.AArch64, false ->
      Oxcaml_stack_check
      ::
      (if !Oxcaml_flags.cfg_stack_checks
       then
         Oxcaml_stack_check_bytes cfg_stack_check_bytes
         ::
         (if cfg_stack_check_bytes = 0
          then []
          else [Oxcaml_stack_check_before_bytes cfg_stack_check_before_bytes])
       else [])
    | Target_system.AArch64, true -> []
    | ( ( Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
        | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
        (_ : bool) ) ->
      []
  in
  let codegen_attrs =
    List.concat_map
      (fun opt : LL.Fn_attr.t list ->
        match (opt : Cfg.codegen_option) with
        | Cfg.Cold -> [Cold; Noinline]
        | Reduce_code_size | No_CSE | Use_linscan_regalloc | Use_regalloc _
        | Use_regalloc_param _ | Assume_zero_alloc _ | Check_zero_alloc _ ->
          [] (* CR yusumez: Do these require any attributes? *))
      codegen_options
  in
  safepoint_attrs @ frame_pointer_attrs @ gc_attrs @ codegen_attrs
  |> List.sort_uniq LL.Fn_attr.compare

(* Returns argument registers listed in the signature *)
let prepare_fun_info t (cfg : Cfg.t) =
  let { Cfg.blocks = _;
        entry_label = _;
        fun_name;
        fun_args;
        fun_codegen_options;
        fun_dbg;
        fun_contains_calls = _ (* not used at this point *);
        fun_num_stack_slots = _ (* only available after regalloc *);
        fun_poll = _ (* not needed after poll insertion *);
        next_instruction_id = _;
        fun_ret_type;
        allowed_to_be_irreducible = _;
        register_locations_are_set = _
      } =
    cfg
  in
  let has_try = has_trap_handler cfg in
  let arg_regs =
    Array.to_list fun_args |> List.filter reg_listed_in_signature
  in
  let arg_types = List.map T.of_reg arg_regs |> make_arg_types in
  let res_type = filter_ds_and_make_ret_type fun_ret_type in
  let cfg_stack_check_bytes = cfg_required_stack_check_bytes cfg in
  let cfg_stack_check_before_bytes = cfg_stack_check_before_bytes cfg in
  let attrs =
    fun_attrs ~has_try ~cfg_stack_check_bytes ~cfg_stack_check_before_bytes
      fun_codegen_options
  in
  let dbg_metadata_id = create_debug_subprogram t ~fun_name fun_dbg in
  let dbg_metadata =
    Option.map (fun id -> Printf.sprintf "!dbg !%d" id) dbg_metadata_id
  in
  let personality =
    match Target_system.architecture (), has_try with
    | Target_system.AArch64, true ->
      add_function_decl t
        (LL.Fundecl.create_varargs "caml_llvm_eh_personality" [] (Some T.i32));
      Some llvm_eh_personality
    | ( ( Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
        | Target_system.POWER | Target_system.Z | Target_system.Riscv ),
        _ )
    | Target_system.AArch64, false ->
      None
  in
  let emitter =
    E.create ~name:fun_name ~args:arg_types ~res:(Some res_type) ~cc:Oxcaml
      ~attrs ~personality ~dbg:fun_dbg ~dbg_metadata ~private_:false
  in
  reset_fun_info ?subprogram_dbg_metadata_id:dbg_metadata_id t emitter;
  let liveness = compute_liveness cfg in
  let active_traps, active_trap_depths = compute_active_traps cfg in
  (get_fun_info t).liveness <- Some liveness;
  (get_fun_info t).fun_args <- fun_args;
  (get_fun_info t).active_traps <- active_traps;
  (get_fun_info t).active_trap_depths <- active_trap_depths;
  let preserved_reg_slots = preserved_reg_slots liveness active_traps cfg in
  (get_fun_info t).preserved_reg_slots <- preserved_reg_slots;
  arg_regs

(* Emits [alloca]'s in the entry block for all [Reg.t]s in [cfg] and runtime
   registers and prepares the [t.reg2alloca] table. [arg_values] are all values
   listed in the signature in LLVM IR (including runtime registers). [arg_regs]
   are the [Reg.t]s corresponding to those present in the argument list.

   Our [Cfg.t] is not always in SSA form, and pinned registers also break SSA.
   Because of this, all [Reg.t]s and runtime registers are accessed through
   loads and stores to pointers returned by [alloca] instructions. These will
   then get optimised by LLVM as part of the mem2reg pass.

   Note that arguments not passed in registers (e.g. in Domainstate) will point
   to that block of memory instead of being allocated on the stack for
   uniformity. *)
let alloca_regs t (cfg : Cfg.t) arg_values arg_regs =
  let alloca_runtime_reg ~runtime_reg_ident ~arg_value =
    let alloca'ed =
      emit_ins ~res_ident:runtime_reg_ident t (I.alloca (V.get_type arg_value))
    in
    emit_ins_no_res t (I.store ~ptr:alloca'ed ~to_store:arg_value)
  in
  let alloca_reg ?init (reg : Reg.t) =
    match reg.loc with
    (* [Outgoing] is for extcalls only - these will get put on the stack by
       LLVM, so we treat them as normal temporaries. We don't expect OCaml calls
       to use the stack for us... *)
    | Unknown | Reg _ | Stack (Local _ | Incoming _ | Outgoing _) -> (
      let alloca'd =
        emit_ins
          ~comment:
            (F.asprintf "%a"
               (fun ppf reg -> F.pp_comment ppf "%a" Printreg.reg reg)
               reg)
          t
          (I.alloca (T.of_reg reg))
      in
      set_alloca_for_reg t reg alloca'd;
      match init with
      | None -> () (* Don't initialise *)
      | Some value -> store_into_reg t reg value)
    | Stack (Domainstate idx) ->
      (* Compute pointer to where [reg] is located - we can assume [ds] will not
         change through the run of this function *)
      let ds_loc = load_domainstate_addr ~offset:idx t Domain_extra_params in
      set_alloca_for_reg t reg ds_loc
  in
  (* First handle values passed from the arguments *)
  let open struct
    type arg =
      | Register of Reg.t
      | Runtime_ident of LL.Ident.t
  end in
  (* Runtime registers come first (since the rest might reference them) *)
  let args =
    List.map (fun ident -> Runtime_ident ident) runtime_reg_idents
    @ List.map (fun reg -> Register reg) arg_regs
  in
  (try
     List.iter2
       (fun arg_reg arg_value ->
         match arg_reg with
         | Register reg -> alloca_reg ~init:arg_value reg
         | Runtime_ident ident ->
           alloca_runtime_reg ~runtime_reg_ident:ident ~arg_value)
       args arg_values
   with _ ->
     fail_msg ~name:"alloca_regs" "argument count mismatch %d %d"
       (List.length args) (List.length arg_values));
  (* Handle remaining registers in the body *)
  let body_regs =
    let all_body_regs = collect_body_regs cfg in
    Reg.Set.diff all_body_regs (Reg.Set.of_list arg_regs)
  in
  Reg.Set.iter (fun reg -> alloca_reg reg) body_regs;
  (match (get_fun_info t).liveness with
  | Some liveness when slow_path_root_slots_enabled () ->
    let count =
      max_slow_path_root_slots liveness (get_fun_info t).active_traps cfg
    in
    (get_fun_info t).slow_path_root_slots
      <- List.init count (fun _ ->
             emit_ins ~comment:"slow path GC root slot" t (I.alloca T.val_ptr))
  | None | Some _ -> ());
  (match Target_system.architecture () with
  | Target_system.AArch64 ->
    Cfg.fold_body_instructions cfg
      ~f:(fun () (i : Cfg.basic Cfg.instruction) ->
        match[@ocaml.warning "-fragile-match"] i.desc with
        | Pushtrap { lbl_handler } ->
          if not (Label.Tbl.mem (get_fun_info t).trap_handler_exn_buckets lbl_handler)
          then
            Label.Tbl.add (get_fun_info t).trap_handler_exn_buckets lbl_handler
              (emit_ins t (I.alloca T.i64))
        | _ -> ())
      ~init:()
  | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
  | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
    ());
  (* Jump to entry block *)
  emit_ins_no_res t (I.br (V.of_label cfg.entry_label))

let trap_handler_entry t (block : Cfg.basic_block) label =
  let first_move =
    DLL.to_list block.body
    |> List.find_map (fun (i : _ Cfg.instruction) ->
        match[@warning "-4"] i.desc with Cfg.Op Move -> Some i | _ -> None)
  in
  match[@ocaml.warning "-fragile-match"]
    Option.map (fun (i : _ Cfg.instruction) -> i, i.desc) first_move
  with
  | Some (i, Op Move) -> (
    let exn_bucket =
      match Target_system.architecture () with
      | Target_system.AArch64 -> (
        match
          Label.Tbl.find_opt (get_fun_info t).trap_handler_exn_buckets label
        with
        | Some handler_bucket -> Some (emit_ins t (I.load ~ptr:handler_bucket ~typ:T.i64))
        | None -> None)
      | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv -> (
        match Label.Tbl.find_opt (get_fun_info t).trap_blocks label with
        | Some { exn_bucket; _ } -> Some exn_bucket
        | None -> None)
    in
    match exn_bucket with
    | Some exn_bucket ->
      (* Restore RBP (+ remove padding) *)
      (* emit_ins_no_res t (I.inline_asm ~asm:"pop %rbp; addq $$8, %rsp"
         ~constraints:"" ~args:[] ~res_type:T.Or_void.void ~sideeffect:true); *)
      (match Target_system.architecture () with
      | Target_system.AArch64 ->
        ()
      | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
      | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
        (* Restore allocation pointer *)
        let new_alloc_ptr = read_allocation_pointer_register t in
        emit_ins_no_res t (I.store ~ptr:allocation_ptr ~to_store:new_alloc_ptr));
      (* Move exn bucket to appropriate temp *)
      store_into_reg t i.arg.(0) exn_bucket
    | None -> ())
  | _ ->
    fail_msg ~name:"trap_handler_entry"
      "first instruction of trap handler not a move"

let emit_block t (cfg : Cfg.t) label =
  let block = Label.Tbl.find cfg.blocks label in
  emit_label t (V.of_label label);
  if block.is_trap_handler then trap_handler_entry t block label;
  DLL.iter ~f:(emit_basic t) block.body;
  emit_terminator t block block.terminator

let cfg (cl : CL.t) =
  let t = get_current_compilation_unit "cfg" in
  let layout = CL.layout cl in
  let cfg = CL.cfg cl in
  reject_addr_regs cfg.fun_args "fun args";
  let arg_regs = prepare_fun_info t cfg in
  (get_fun_info t).addr_regs_known_base <- collect_addr_regs_known_base cfg;
  (get_fun_info t).static_addr_regs <- collect_static_addr_regs cfg;
  let arg_values = E.get_args_as_values (get_fun_info t).emitter in
  alloca_regs t cfg arg_values arg_regs;
  DLL.iter ~f:(emit_block t cfg) layout;
  add_defined_symbol t cfg.fun_name;
  complete_func_def t

(* Data declarations *)

(* CR yusumez: We make some assumptions about the structure of the data items we
   receive and decode it manually here. Ideally, [data_item]s would be
   represented in a more structured manner, but this should do for now. *)
(* CR yusumez: [caml_startup_N] symbols are currently broken. *)
let make_temp_data_symbol =
  let idx = ref 0 in
  fun () ->
    let module_name =
      Compilation_unit.(get_current_or_dummy () |> name |> Name.to_string)
    in
    let res = Format.asprintf "temp.%s.%d" module_name !idx in
    incr idx;
    res

let llvm_values_of_data_item (d : Cmm.data_item) =
  let i64_of_bits n = V.imm T.i64 (Int64.to_string n) in
  match d with
  | Cdefine_symbol _ -> fail_msg ~name:"llvm_value_of_data_item" "define_symbol"
  | Calign _ | Csymbol_offset _ ->
    (* [Calign] and [Csymbol_offset] are never produced *)
    fail_msg ~name:"llvm_value_of_data_item" "unexpected data item"
  | Cint n -> [V.of_nativeint ~typ:T.i64 n]
  | Cint8 n -> [V.of_int ~typ:T.i8 n]
  | Cint16 n -> [V.of_int ~typ:T.i16 n]
  | Cint32 n -> [V.of_nativeint ~typ:T.i32 n]
  | Csymbol_address { sym_name; sym_global = _ } -> [V.of_symbol sym_name]
  | Cstring s -> [V.of_string_constant s]
  | Cskip size ->
    [V.zeroinitializer (T.Array { num_of_elems = size; elem_type = T.i8 })]
  | Csingle f -> [V.of_float ~typ:T.float f]
  | Cdouble f -> [V.of_float ~typ:T.double f]
  | Cvec128 { word0; word1 } -> [i64_of_bits word0; i64_of_bits word1]
  | Cvec256 { word0; word1; word2; word3 } ->
    [i64_of_bits word0; i64_of_bits word1; i64_of_bits word2; i64_of_bits word3]
  | Cvec512 { word0; word1; word2; word3; word4; word5; word6; word7 } ->
    [ i64_of_bits word0;
      i64_of_bits word1;
      i64_of_bits word2;
      i64_of_bits word3;
      i64_of_bits word4;
      i64_of_bits word5;
      i64_of_bits word6;
      i64_of_bits word7 ]

let define_symbol t ~private_ ~header ~symbol (contents : Cmm.data_item list) =
  let symbol =
    match symbol with None -> make_temp_data_symbol () | Some symbol -> symbol
  in
  (match header with
  | None -> ()
  | Some header ->
    let header_sym = "header." ^ symbol in
    add_data_def t
      (LL.Data.constant ~private_:true header_sym
         (V.of_nativeint ~typ:T.i64 header)));
  let value =
    V.struct_constant (List.concat_map llvm_values_of_data_item contents)
  in
  add_data_def t (LL.Data.constant ~private_ symbol value);
  add_defined_symbol t symbol;
  List.iter
    (fun (d : Cmm.data_item) ->
      match[@warning "-fragile-match"] d with
      | Csymbol_address { sym_name; sym_global = _ } ->
        add_referenced_symbol t sym_name
      | _ -> ())
    contents

let drop_trailing_align (contents : Cmm.data_item list) =
  match[@warning "-4"] List.rev contents with
  | Cmm.Calign _ :: contents -> List.rev contents
  | _ -> contents

let data (ds : Cmm.data_item list) =
  let t = get_current_compilation_unit "data" in
  let define_symbol ~private_ ~header ~symbol contents =
    if private_ && List.is_empty contents
    then () (* No need to declare a private symbol with no contents *)
    else define_symbol t ~private_ ~header ~symbol contents
  in
  let peek_int = function[@warning "-4"] Cmm.Cint n -> Some n | _ -> None in
  let peek_define_symbol = function[@warning "-4"]
    | Cmm.Cdefine_symbol { sym_name; sym_global = _ } -> Some sym_name
    | _ -> None
  in
  let eat_if peek = function
    | [] -> None
    | d :: ds -> peek d |> Option.map (fun i -> i, ds)
  in
  let eat_header_and_symbol ds =
    let header, ds = eat_if peek_int ds |> Option.get in
    let symbol, ds = eat_if peek_define_symbol ds |> Option.get in
    header, symbol, ds
  in
  let closure_block ds =
    let function_slot ds =
      let header, symbol, ds = eat_header_and_symbol ds in
      (*= A function slot is either:
          | code pointer | closinfo | (if the function has arity 0 or 1)
          | code pointer | closinfo | second code pointer | (arity >= 2)
          See mlvalues.h for details. *)
      let closinfo = List.nth ds 1 |> peek_int |> Option.get in
      let arity = Nativeint.shift_right_logical closinfo 56 in
      let slot_size = if arity <= 1n then 2 else 3 in
      let slot, tail = List.split_at slot_size ds in
      let is_last =
        Nativeint.(shift_right_logical (shift_left closinfo 8) (size - 1) = 1n)
      in
      let contents = if is_last then slot @ tail else slot in
      define_symbol ~private_:false ~header:(Some header) ~symbol:(Some symbol)
        contents;
      tail, is_last
    in
    let rec iter_slots ds =
      let tail, is_last = function_slot ds in
      if not is_last then iter_slots tail
    in
    iter_slots ds
  in
  (* Returns true if successful *)
  let caml_startup ds =
    let caml_startup_indices =
      List.mapi
        (fun i d ->
          Option.bind (peek_define_symbol d) (fun sym ->
              if String.begins_with ~prefix:"caml_startup" sym
              then Some i
              else None))
        ds
      |> List.filter_map (fun opt -> opt)
    in
    match caml_startup_indices with
    | [idx] ->
      let do_block ds =
        let header, symbol, contents = eat_header_and_symbol ds in
        define_symbol ~private_:false ~header:(Some header)
          ~symbol:(Some symbol) contents
      in
      let exn_ds, caml_startup_ds = List.split_at (idx - 1) ds in
      do_block exn_ds;
      do_block caml_startup_ds;
      true
    | [] | _ :: _ :: _ -> false
  in
  let no_header_symbols ds =
    let define_symbol_of_cmm_symbol symbol contents =
      define_symbol ~private_:false ~header:None
        ~symbol:(Some symbol.Cmm.sym_name)
        (drop_trailing_align contents)
    in
    let rec loop current_symbol current_contents ds =
      match[@warning "-4"] ds with
      | [] ->
        define_symbol_of_cmm_symbol current_symbol (List.rev current_contents)
      | Cmm.Cdefine_symbol symbol :: ds ->
        define_symbol_of_cmm_symbol current_symbol (List.rev current_contents);
        loop symbol [] ds
      | d :: ds -> loop current_symbol (d :: current_contents) ds
    in
    match[@warning "-4"] ds with
    | Cmm.Cdefine_symbol symbol :: ds ->
      loop symbol [] ds;
      true
    | _ -> false
  in
  let header_symbol_with_trailing_symbols header symbol contents =
    let rec split_at_next_symbol prefix = function[@warning "-4"]
      | [] -> None
      | Cmm.Cdefine_symbol _ :: _ as trailing_symbols ->
        Some (List.rev prefix, trailing_symbols)
      | d :: ds -> split_at_next_symbol (d :: prefix) ds
    in
    match split_at_next_symbol [] contents with
    | None -> false
    | Some (contents, trailing_symbols) ->
      define_symbol ~private_:false ~header:(Some header) ~symbol:(Some symbol)
        contents;
      if no_header_symbols trailing_symbols then true else assert false
  in
  let block ds =
    match eat_if peek_int ds with
    | Some (i, after_i) -> (
      match eat_if peek_define_symbol after_i with
      | Some (symbol, after_symbol) ->
        (* [i] is a header *)
        if Nativeint.(logand i 0xffn = of_int Obj.closure_tag)
        then closure_block ds
        else if caml_startup ds
        then ()
        else if header_symbol_with_trailing_symbols i symbol after_symbol
        then ()
        else
          define_symbol ~private_:false ~header:(Some i) ~symbol:(Some symbol)
            after_symbol
      | None ->
        (* [i] is not a header *)
        define_symbol ~private_:true ~header:None ~symbol:None ds)
    | None ->
      (* No header *)
      if no_header_symbols ds
      then ()
      else define_symbol ~private_:true ~header:None ~symbol:None ds
  in
  try block ds
  with _ ->
    fail_msg ~name:"data" "error while decoding data items: %a" Printcmm.data ds

(* Wrapping up loose ends *)

let define_c_call_wrappers t =
  String.Map.iter
    (fun wrapper_name { c_fun_name; args = c_arg_types; res = c_res_types } ->
      let wrapper_res_type = make_ret_type c_res_types in
      let wrapper_arg_types = make_arg_types c_arg_types in
      let emitter =
        E.create ~name:wrapper_name ~args:wrapper_arg_types
          ~res:(Some wrapper_res_type) ~cc:Oxcaml ~attrs:[Noinline]
          ~personality:None ~dbg:Debuginfo.none ~dbg_metadata:None
          ~private_:true
      in
      reset_fun_info t emitter;
      let runtime_args, c_fun_args =
        List.split_at (List.length runtime_regs) (E.get_args_as_values emitter)
      in
      let ds = List.nth runtime_args domainstate_idx in
      let c_sp =
        let c_sp_ptr = load_domainstate_addr ~ds_loc:ds t Domain_c_stack in
        emit_ins t (I.load ~ptr:c_sp_ptr ~typ:T.i64)
      in
      let ocaml_sp = read_stack_pointer t in
      write_stack_pointer t c_sp;
      let c_res =
        emit_ins t
          (I.call
             ~func:(V.of_symbol c_fun_name |> V.get_ident_exn)
             ~args:c_fun_args ~res_type:(Some (T.Struct c_res_types)) ~attrs:[]
             ~operand_bundles:[] ~cc:Default ~musttail:false)
      in
      write_stack_pointer t ocaml_sp;
      let wrapper_res =
        assemble_struct t wrapper_res_type
          (([1], c_res) :: List.mapi (fun i v -> [0; i], v) runtime_args)
      in
      emit_ins_no_res t (I.ret wrapper_res);
      add_defined_symbol t wrapper_name;
      complete_func_def t)
    t.c_call_wrappers

let define_wrap_try t =
  let arg_types = make_arg_types [] in
  let res_type = make_ret_type [T.i64] in
  let emitter =
    E.create ~name:"wrap_try" ~args:arg_types ~res:(Some res_type) ~cc:Oxcaml
      ~attrs:[Returns_twice; Noinline] ~personality:None ~dbg:Debuginfo.none
      ~dbg_metadata:None ~private_:true
  in
  reset_fun_info t emitter;
  let runtime_args =
    E.get_args_as_values emitter
    (* All are runtime regs *)
  in
  let try_res = V.of_int ~typ:T.i64 0 in
  let res =
    assemble_struct t res_type
      (([1; 0], try_res) :: List.mapi (fun i v -> [0; i], v) runtime_args)
  in
  emit_ins_no_res t (I.ret res);
  complete_func_def t

let define_restore_rbp t =
  let asm_symbol ident = LL.Ident.to_string_encoded ident in
  match Target_system.architecture () with
  | Target_system.AArch64 ->
    ()
  | Target_system.X86_64 ->
    List.iter
      (fun ({ recover_rbp_asm_ident; recover_rbp_var_ident; _ } :
             trap_block_info) ->
        let recover_rbp_asm = asm_symbol recover_rbp_asm_ident in
        let recover_rbp_var = asm_symbol recover_rbp_var_ident in
        add_module_asm t
          [ "  .text";
            recover_rbp_asm ^ ":";
            "  pop %rbp";
            "  addq $8, %rsp";
            "  movq " ^ recover_rbp_var ^ "(%rip), %rbx";
            "  jmpq *%rbx" ];
        add_data_def t
          (LL.Data.external_ (LL.Ident.to_string_hum recover_rbp_asm_ident));
        add_data_def t
          (LL.Data.constant
             (LL.Ident.to_string_hum recover_rbp_var_ident)
             (V.zeroinitializer T.ptr)))
      t.all_trap_blocks
  | Target_system.IA32 | Target_system.ARM | Target_system.POWER
  | Target_system.Z | Target_system.Riscv ->
    if not (List.is_empty t.all_trap_blocks)
    then
      fail_msg ~name:"define_restore_rbp"
        "unsupported architecture for LLVM backend"

(* Declare menitoned but not declared data items as extern *)
let declare_data t =
  String.Set.diff t.referenced_symbols t.defined_symbols
  |> String.Set.iter (fun sym -> add_data_def t (LL.Data.external_ sym))

let define_auxiliary_functions t =
  define_c_call_wrappers t;
  (match Target_system.architecture () with
  | Target_system.AArch64 -> ()
  | Target_system.X86_64 | Target_system.IA32 | Target_system.ARM
  | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
    define_wrap_try t);
  define_restore_rbp t

(* Interface with the rest of the compiler *)

let init ~output_prefix ~ppf_dump =
  fail_if_not ~msg:"stack checks not supported" "init"
    (Config.no_stack_checks
    ||
    match Target_system.architecture () with
    | Target_system.AArch64 -> true
    | Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
    | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
      false);
  fail_if_not ~msg:"runtime5 required" "init" Config.runtime5;
  let llvmir_filename = output_prefix ^ ".ll" in
  current_compilation_unit := Some (create ~llvmir_filename ~ppf_dump)

let close_out () =
  match !current_compilation_unit with
  | None -> ()
  | Some t ->
    (* Exception raised during llvmize, keep .ll file. *)
    Out_channel.close t.oc;
    current_compilation_unit := None

let open_out ~asm_filename =
  let t = get_current_compilation_unit "open_out" in
  t.asm_filename <- Some asm_filename

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

let invoke_clang_with_llvmir ~output_filename ~input_filename ~extra_flags =
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  let fixed_reg_flags =
    match Target_system.architecture () with
    | Target_system.AArch64 ->
      (* x16/x17 are reserved by the OxCaml AArch64 calling convention in the
         LLVM backend. x26 holds the trap pointer. Design 1 keeps allocation
         pointer and domain state as SSA values and lets the OxCaml calling
         convention place them in x27/x28 at runtime boundaries. *)
      ["-ffixed-x15"; "-ffixed-x26"]
    | Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
    | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
      []
  in
  let fp_flags =
    match Target_system.architecture () with
    | Target_system.AArch64 ->
      if Config.with_frame_pointers
      then ["-fno-omit-frame-pointer"; "-mno-omit-leaf-frame-pointer"]
      else ["-fomit-frame-pointer"; "-momit-leaf-frame-pointer"]
    | Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
    | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
      if Config.with_frame_pointers
      then ["-fno-omit-frame-pointer"]
      else ["-fomit-frame-pointer"; "-momit-leaf-frame-pointer"]
  in
  let branch_shape_flags =
    match Target_system.architecture () with
    | Target_system.AArch64 ->
      (* OxCaml's middle end has already chosen branch shape before LLVM. Keep
         asymmetric diamonds as branches instead of letting LLVM form selects
         that compute both arms on the hot path. *)
      [ "-mllvm";
        "-two-entry-phi-node-folding-threshold=0";
        "-mllvm";
        "-disable-early-ifcvt";
        "-mllvm";
        "-aarch64-enable-early-ifcvt=false";
        "-mllvm";
        "-oxcaml-avoid-i64-load-narrowing" ]
    | Target_system.IA32 | Target_system.X86_64 | Target_system.ARM
    | Target_system.POWER | Target_system.Z | Target_system.Riscv ->
      []
  in
  let llvm_flags = [!Oxcaml_flags.llvm_flags] in
  Ccomp.command
    (String.concat " "
       ([cmd]
       @ ["-o"; Filename.quote output_filename]
       @ ["-x ir"; Filename.quote input_filename]
       @ ["-O3"; "-S"; "-Wno-override-module"]
       @ fixed_reg_flags @ fp_flags @ branch_shape_flags @ llvm_flags
       @ extra_flags))

let llvmir_to_assembly t =
  match t.asm_filename with
  | None -> 0
  | Some asm_filename ->
    invoke_clang_with_llvmir ~input_filename:t.llvmir_filename
      ~output_filename:asm_filename ~extra_flags:[]

let dump_llvmir ~llvmir_filename ~message t =
  let ic = In_channel.open_text llvmir_filename in
  let contents = In_channel.input_all ic in
  Format.fprintf t.ppf_dump "\n*** %s\n\n%s" message contents;
  In_channel.close ic

let dump_llvmir_after_llvmize t =
  dump_llvmir ~llvmir_filename:t.llvmir_filename ~message:"After llvmize" t

let dump_llvmir_after_opt t =
  let opt_llvmir_filename = t.llvmir_filename ^ ".opt.ll" in
  let _cmd_ret =
    invoke_clang_with_llvmir ~input_filename:t.llvmir_filename
      ~output_filename:opt_llvmir_filename ~extra_flags:["-emit-llvm"]
  in
  dump_llvmir ~llvmir_filename:opt_llvmir_filename ~message:"After llopt" t;
  remove_file opt_llvmir_filename

let assemble_file ~asm_filename ~obj_filename =
  let cmd =
    match !Oxcaml_flags.llvm_path with Some path -> path | None -> Config.asm
  in
  Ccomp.command
    (String.concat " "
       [ cmd;
         "-c";
         "-g";
         "-Wno-trigraphs";
         "-o";
         Filename.quote obj_filename;
         Filename.quote asm_filename ])

(* CR yusumez: [begin_assembly] and [end_assembly] emit extra things to the .ll
   file, so they always need to be called. However, this will still generate an
   assembly file if -stop-after simplify_cfg or -stop_after linearization are
   passed, which it shouldn't do. *)

let begin_assembly ~is_startup:_ ~sourcefile =
  let t = get_current_compilation_unit "begin_asm" in
  t.sourcefile <- sourcefile

(* CR yusumez: Lift this to [Llvm_ir] when we have proper metadata support *)
let write_module_metadata t =
  let module_name_from_symbol symbol suffix =
    let prefix = "caml" in
    let prefix_len = String.length prefix in
    let suffix_len = String.length suffix in
    let symbol_len = String.length symbol in
    if
      symbol_len >= prefix_len + suffix_len
      && String.sub symbol 0 prefix_len = prefix
      && String.sub symbol (symbol_len - suffix_len) suffix_len = suffix
    then String.sub symbol prefix_len (symbol_len - prefix_len - suffix_len)
    else
      fail_msg "unexpected LLVM module symbol shape %S; expected caml...%s"
        symbol suffix
  in
  let encoded_symbol symbol =
    let symbol =
      Asm_targets.(Asm_symbol.create_global symbol |> Asm_symbol.encode)
    in
    if String.begins_with ~prefix:"_" symbol
    then String.sub symbol 1 (String.length symbol - 1)
    else symbol
  in
  let module_name =
    module_name_from_symbol
      (encoded_symbol (Cmm_helpers.make_symbol "code_begin"))
      "__code_begin"
  in
  F.pp_line t.ppf "";
  F.pp_line t.ppf {|!0 = !{ i32 1, !"oxcaml_module", !"%s" }|} module_name;
  let debug_module_flags = List.rev t.debug_module_flag_ids_rev in
  let pp_module_flag_id ppf id = Format.fprintf ppf "!%d" id in
  F.pp_line t.ppf {|!llvm.module.flags = !{ !0%a }|}
    (fun ppf ids ->
      List.iter (fun id -> Format.fprintf ppf ", %a" pp_module_flag_id id) ids)
    debug_module_flags;
  (match t.debug_compile_unit_id with
  | None -> ()
  | Some compile_unit_id ->
    F.pp_line t.ppf {|!llvm.dbg.cu = !{!%d}|} compile_unit_id);
  List.iter
    (fun metadata_def -> F.pp_line t.ppf "%s" metadata_def)
    (List.rev t.debug_metadata_defs_rev)

let write_llvmir_to_file t =
  (match t.sourcefile with
  | Some sourcefile -> F.pp_line t.ppf "source_filename = \"%s\"\n" sourcefile
  | None -> ());
  List.iter (LL.Function.pp_t t.ppf) (List.rev t.function_defs);
  List.iter (LL.Data.pp_t t.ppf) (List.rev t.data_defs);
  F.pp_line t.ppf "";
  String.Map.iter
    (fun _ fundecl -> LL.Fundecl.pp_t t.ppf fundecl)
    t.function_decls;
  String.Map.iter
    (fun _ fundecl -> LL.Fundecl.pp_t t.ppf fundecl)
    t.called_intrinsics;
  F.pp_line t.ppf "";
  List.iter
    (fun asm_line -> F.pp_line t.ppf {|module asm "%s"|} asm_line)
    t.module_asm;
  write_module_metadata t

let end_assembly () =
  let t = get_current_compilation_unit "end_asm" in
  define_auxiliary_functions t;
  declare_data t;
  write_llvmir_to_file t;
  Out_channel.close t.oc;
  invoke_expect_llvm_ir_callbacks ~llvmir_filename:t.llvmir_filename;
  (* Dump if -dllvmir passed *)
  if !Oxcaml_flags.dump_llvmir
  then (
    dump_llvmir_after_llvmize t;
    dump_llvmir_after_opt t);
  (* Call clang to compile .ll to .s *)
  let ret_code = llvmir_to_assembly t in
  if ret_code <> 0
  then
    raise
      (Error
         (Asm_generation
            ( Option.value ~default:"(no source file specified)" t.sourcefile,
              ret_code )));
  Option.iter
    (fun asm_filename -> invoke_expect_llvm_asm_callbacks ~asm_filename)
    t.asm_filename;
  if not !Oxcaml_flags.keep_llvmir then remove_file t.llvmir_filename;
  current_compilation_unit := None

(* CR-someday gyorsh: currently, llvm backend can be selected at the compilation
   unit granularity only. It could be controlled at the function granularity. *)
(* CR-someday gyorsh: Compiling directly to .o would involve more changes to
   [Asmgen], [Asmlink], and drivers. It would improve compilation speed but not
   as much as avoiding the textual representation entirely by linking in the
   llvm library statically. *)
(* CR-someday gyorsh: we could set [binary_backend_available] but it is
   currently too tightly coupled with the [internal_assembler], especially in
   [asmlink] for shared libraries. *)
(* CR gyorsh: assume 64-bit architecture *)
(* CR yusumez: We ignore whether symbols are local/global. *)

(* Error report *)

let report_error ppf = function
  | Asm_generation (fn, ret_code) ->
    Format_doc.fprintf ppf "Error producing assembly code for %s: %d" fn
      ret_code

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
