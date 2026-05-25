(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]
(* Specific operations for the ARM processor, 64-bit mode *)

open! Int_replace_polymorphic_compare

open Format

let macosx = String.equal Config.system "macosx"

let is_asan_enabled = ref false

(* CR gyorsh: refactor to use [Arch.Extension] like amd64 *)
let feat_cssc = ref false

(* Emit elf notes with trap handling information. *)
let trap_notes = ref true

(* Machine-specific command-line options *)

let command_line_options = [
  "-fno-asan",
    Arg.Clear is_asan_enabled,
    " Disable AddressSanitizer. This is only meaningful if the compiler was \
     built with AddressSanitizer support enabled."
  ;

  "-fcssc",
    Arg.Set feat_cssc,
    " Enable the Common Short Sequence Compression (CSSC) instructions."
]

(* Addressing modes *)

type addressing_mode =
  | Iindexed of Arm64_ast.Ast.DSL.Validated_mem_offset.t  (* reg + displ *)
  | Ibased of Asm_targets.Asm_symbol.t * int              (* symbol + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type cmm_label = Label.t
  (* Do not introduce a dependency to Cmm *)

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type prefetch_temporal_locality_hint = Nonlocal | Low | Moderate | High

type float_width = Cmm.float_width

(* Specific operations, including [Simd], must not raise. *)
type specific_operation =
  | Ilea of addressing_mode
  | Istore_int of nativeint * addressing_mode * bool
  | Ioffset_loc of int * addressing_mode
  | Ifloatarithmem of float_width * float_operation * addressing_mode
  | Ifar_poll
  | Ifar_alloc of { bytes : int; dbginfo : Cmm.alloc_dbginfo }
  | Ishiftarith of arith_operation * int
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of { bitwidth: bswap_bitwidth; } (* endianness conversion *)
  | Isextend32
  | Izextend32
  | Irdtsc
  | Irdpmc
  | Ilfence
  | Isfence
  | Imfence
  | Ipackf32
  | Imove32       (* 32-bit integer move *)
  | Isignext of int (* sign extension *)
  | Isimd of Simd.operation
  | Isimd_mem of Simd.Mem.operation * addressing_mode
  | Icldemote of addressing_mode
  | Iprefetch of
      { is_write: bool;
        locality: prefetch_temporal_locality_hint;
        addr: addressing_mode;
      }
  | Illvm_intrinsic of string

and arith_operation =
    Ishiftadd
  | Ishiftsub

and float_operation =
  | Ifloatadd
  | Ifloatsub
  | Ifloatmul
  | Ifloatdiv

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let size_vec128 = 16
let size_vec256 = 32
let size_vec512 = 64

let allow_unaligned_access = true

(* Whether Ocaml provides shift operations where the shift amount is interpreted
   modulo bitwidth. *)

let ocaml_shifts_are_wrapping = true

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

module Validated_mem_offset = Arm64_ast.Ast.DSL.Validated_mem_offset

(* Identity addressing: offset 0, which is valid for any scale. Use scale=1. *)
let identity_addressing =
  match Validated_mem_offset.create ~scale:1 ~offset:0 with
  | Some v -> Iindexed v
  | None -> assert false (* 0 is always valid *)

let offset_addressing addr delta =
  (* Resulting offset might not be representable, but that is the
     responsibility of the caller. *)
  match addr with
  | Iindexed v ->
    let offset = Validated_mem_offset.offset v + delta in
    let scale = Validated_mem_offset.scale v in
    (match Validated_mem_offset.create ~scale ~offset with
    | Some v' -> Iindexed v'
    | None ->
      Misc.fatal_errorf
        "Arch.offset_addressing: offset %d out of range for scale %d" offset
        scale)
  | Ibased (sym, i) -> Ibased (sym, i + delta)

let num_args_addressing = function
  | Iindexed _ -> 1
  | Ibased _ -> 0

let addressing_displacement_for_llvmize addr =
  if not !Clflags.llvm_backend
  then
    Misc.fatal_error
      "Arch.displacement_addressing_for_llvmize: should only be called with \
        -llvm-backend"
  else
    match addr with
    | Iindexed v -> Validated_mem_offset.offset v
    | Ibased _ ->
      Misc.fatal_error
        "Arch.displacement_addressing_for_llvmize: unexpected addressing mode"

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed v ->
    let n = Validated_mem_offset.offset v in
    printreg ppf arg.(0);
    if n <> 0 then fprintf ppf " + %i" n
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" (Asm_targets.Asm_symbol.encode s)
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" (Asm_targets.Asm_symbol.encode s) n

let int_of_bswap_bitwidth = function
  | Sixteen -> 16
  | Thirtytwo -> 32
  | Sixtyfour -> 64

let string_of_prefetch_temporal_locality_hint = function
  | Nonlocal -> "nonlocal"
  | Low -> "low"
  | Moderate -> "moderate"
  | High -> "high"

let print_specific_operation printreg op ppf arg =
  match op with
  | Ilea addr ->
    print_addressing printreg addr ppf arg
  | Istore_int (n, addr, _is_assign) ->
    fprintf ppf "%nd -> %a" n (print_addressing printreg addr) arg
  | Ioffset_loc (n, addr) ->
    fprintf ppf "%i + %a" n (print_addressing printreg addr) arg
  | Ifloatarithmem _ ->
    fprintf ppf "floatarithmem"
  | Ifar_poll ->
    fprintf ppf "(far) poll"
  | Ifar_alloc { bytes; dbginfo = _ } ->
    fprintf ppf "(far) alloc %i" bytes
  | Ishiftarith(op, shift) ->
      let op_name = function
      | Ishiftadd -> "+"
      | Ishiftsub -> "-" in
      let shift_mark =
       if shift >= 0
       then sprintf "<< %i" shift
       else sprintf ">> %i" (-shift) in
      fprintf ppf "%a %s %a %s"
       printreg arg.(0) (op_name op) printreg arg.(1) shift_mark
  | Imuladd ->
      fprintf ppf "(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsub ->
      fprintf ppf "-(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulf ->
      fprintf ppf "-f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
  | Imuladdf ->
      fprintf ppf "%a +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmuladdf ->
      fprintf ppf "(-f %a) -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsubf ->
      fprintf ppf "%a -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulsubf ->
      fprintf ppf "(-f %a) +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Isqrtf ->
      fprintf ppf "sqrtf %a"
        printreg arg.(0)
  | Ibswap { bitwidth } ->
      let n = int_of_bswap_bitwidth bitwidth in
      fprintf ppf "bswap%i %a" n
        printreg arg.(0)
  | Isextend32 ->
      fprintf ppf "sextend32 %a" printreg arg.(0)
  | Izextend32 ->
      fprintf ppf "zextend32 %a" printreg arg.(0)
  | Irdtsc -> fprintf ppf "rdtsc"
  | Irdpmc -> fprintf ppf "rdpmc"
  | Ilfence -> fprintf ppf "lfence"
  | Isfence -> fprintf ppf "sfence"
  | Imfence -> fprintf ppf "mfence"
  | Ipackf32 -> fprintf ppf "packf32"
  | Imove32 ->
      fprintf ppf "move32 %a"
        printreg arg.(0)
  | Isignext n ->
      fprintf ppf "signext%d %a"
        n printreg arg.(0)
  | Isimd op ->
    Simd.print_operation printreg op ppf arg
  | Isimd_mem _ ->
      fprintf ppf "simd_mem"
  | Icldemote addr ->
      fprintf ppf "cldemote %a" (print_addressing printreg addr) arg
  | Iprefetch { is_write; locality; addr } ->
      fprintf ppf "prefetch is_write=%b prefetch_temporal_locality_hint=%s %a"
        is_write (string_of_prefetch_temporal_locality_hint locality)
        (print_addressing printreg addr) arg
  | Illvm_intrinsic name ->
      fprintf ppf "llvm_intrinsic %s" name

let specific_operation_name : specific_operation -> string = fun op ->
  match op with
  | Ilea _ -> "lea"
  | Istore_int (n, _addr, _is_assign) -> "store_int " ^ Nativeint.to_string n
  | Ioffset_loc (n, _addr) -> "offset_loc " ^ string_of_int n
  | Ifloatarithmem _ -> "floatarithmem"
  | Ifar_poll -> "far poll"
  | Ifar_alloc { bytes; dbginfo = _ } ->
      Printf.sprintf "far alloc of %d bytes" bytes
  | Ishiftarith (op, shift) ->
      let op_name = function
        | Ishiftadd -> "+"
        | Ishiftsub -> "-" in
      let shift_mark =
        if shift >= 0
        then sprintf "<< %i" shift
        else sprintf ">> %i" (-shift) in
      Printf.sprintf "%s %s" (op_name op) shift_mark
  | Imuladd -> "muladd"
  | Imulsub -> "mulsub"
  | Inegmulf -> "negmulf"
  | Imuladdf -> "muladdf"
  | Inegmuladdf -> "negmuladdf"
  | Imulsubf -> "mulsubf"
  | Inegmulsubf -> "negmulsubf"
  | Isqrtf -> "sqrtf"
  | Ibswap _ -> "bswap"
  | Isextend32 -> "sextend32"
  | Izextend32 -> "zextend32"
  | Irdtsc -> "rdtsc"
  | Irdpmc -> "rdpmc"
  | Ilfence -> "lfence"
  | Isfence -> "sfence"
  | Imfence -> "mfence"
  | Ipackf32 -> "packf32"
  | Imove32 -> "move32"
  | Isignext _ -> "signext"
  | Isimd _ -> "simd"
  | Isimd_mem _ -> "simd_mem"
  | Icldemote _ -> "cldemote"
  | Iprefetch _ -> "prefetch"
  | Illvm_intrinsic _ -> "llvm_intrinsic"

let equal_addressing_mode left right =
  match left, right with
  | Iindexed left_v, Iindexed right_v ->
    Validated_mem_offset.equal left_v right_v
  | Ibased (left_sym, left_int), Ibased (right_sym, right_int) ->
    Asm_targets.Asm_symbol.equal left_sym right_sym
    && Int.equal left_int right_int
  | (Iindexed _ | Ibased _), _ -> false

let equal_arith_operation left right =
  match left, right with
  | Ishiftadd, Ishiftadd -> true
  | Ishiftsub, Ishiftsub -> true
  | (Ishiftadd | Ishiftsub), _ -> false

let equal_float_operation left right =
  match left, right with
  | Ifloatadd, Ifloatadd
  | Ifloatsub, Ifloatsub
  | Ifloatmul, Ifloatmul
  | Ifloatdiv, Ifloatdiv ->
    true
  | (Ifloatadd | Ifloatsub | Ifloatmul | Ifloatdiv), _ -> false

let equal_prefetch_temporal_locality_hint left right =
  match left, right with
  | Nonlocal, Nonlocal | Low, Low | Moderate, Moderate | High, High -> true
  | (Nonlocal | Low | Moderate | High), _ -> false

let equal_specific_operation left right =
  match left, right with
  | Ilea left, Ilea right -> equal_addressing_mode left right
  | Istore_int (left_n, left_addr, left_is_assign),
    Istore_int (right_n, right_addr, right_is_assign) ->
    Nativeint.equal left_n right_n
    && equal_addressing_mode left_addr right_addr
    && Bool.equal left_is_assign right_is_assign
  | Ioffset_loc (left_n, left_addr), Ioffset_loc (right_n, right_addr) ->
    Int.equal left_n right_n && equal_addressing_mode left_addr right_addr
  | Ifloatarithmem (left_width, left_op, left_addr),
    Ifloatarithmem (right_width, right_op, right_addr) ->
    Cmm.equal_float_width left_width right_width
    && equal_float_operation left_op right_op
    && equal_addressing_mode left_addr right_addr
  | Ifar_alloc { bytes = left_bytes; dbginfo = _; },
    Ifar_alloc { bytes = right_bytes; dbginfo = _; } ->
    Int.equal left_bytes right_bytes
  | Ishiftarith (left_arith_operation, left_int),
    Ishiftarith (right_arith_operation, right_int) ->
    equal_arith_operation left_arith_operation right_arith_operation
    && Int.equal left_int right_int
  | Imuladd, Imuladd -> true
  | Imulsub, Imulsub -> true
  | Inegmulf, Inegmulf -> true
  | Imuladdf, Imuladdf -> true
  | Inegmuladdf, Inegmuladdf -> true
  | Imulsubf, Imulsubf -> true
  | Inegmulsubf, Inegmulsubf -> true
  | Isqrtf, Isqrtf -> true
  | Ibswap { bitwidth = left }, Ibswap { bitwidth = right } ->
    Int.equal (int_of_bswap_bitwidth left) (int_of_bswap_bitwidth right)
  | Isextend32, Isextend32 -> true
  | Izextend32, Izextend32 -> true
  | Irdtsc, Irdtsc -> true
  | Irdpmc, Irdpmc -> true
  | Ilfence, Ilfence -> true
  | Isfence, Isfence -> true
  | Imfence, Imfence -> true
  | Ipackf32, Ipackf32 -> true
  | Imove32, Imove32 -> true
  | Isignext left, Isignext right -> Int.equal left right
  | Isimd left, Isimd right -> Simd.equal_operation left right
  | Isimd_mem ((), left_addr), Isimd_mem ((), right_addr) ->
    equal_addressing_mode left_addr right_addr
  | Icldemote left, Icldemote right -> equal_addressing_mode left right
  | Iprefetch
      { is_write = left_is_write; locality = left_locality; addr = left_addr },
    Iprefetch
      { is_write = right_is_write;
        locality = right_locality;
        addr = right_addr
      } ->
    Bool.equal left_is_write right_is_write
    && equal_prefetch_temporal_locality_hint left_locality right_locality
    && equal_addressing_mode left_addr right_addr
  | Illvm_intrinsic left, Illvm_intrinsic right -> String.equal left right
  | ( Ilea _ | Istore_int _ | Ioffset_loc _ | Ifloatarithmem _
    | Ifar_alloc _  | Ifar_poll  | Ishiftarith _
    | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
    | Inegmulsubf | Isqrtf | Ibswap _ | Isextend32 | Izextend32 | Irdtsc
    | Irdpmc | Ilfence | Isfence | Imfence | Ipackf32 | Imove32 | Isignext _
    | Isimd _ | Isimd_mem _ | Icldemote _ | Iprefetch _
    | Illvm_intrinsic _), _ -> false

let isomorphic_specific_operation op1 op2 =
  equal_specific_operation op1 op2

(* Specific operations that are pure *)

let operation_is_pure : specific_operation -> bool = function
  | Ilea _ | Ifloatarithmem _ | Ibswap _ | Isextend32 | Izextend32 | Irdtsc
  | Irdpmc | Ilfence | Isfence | Imfence | Ipackf32 | Isimd _
  | Icldemote _ | Iprefetch _ -> true
  | Istore_int _ | Ioffset_loc _ | Isimd_mem _ -> false
  | Ifar_alloc _ | Ifar_poll -> false
  | Ishiftarith _ -> true
  | Imuladd -> true
  | Imulsub -> true
  | Inegmulf -> true
  | Imuladdf -> true
  | Inegmuladdf -> true
  | Imulsubf -> true
  | Inegmulsubf -> true
  | Isqrtf -> true
  | Imove32 -> true
  | Isignext _ -> true
  | Illvm_intrinsic ("caml_sse2_float64_min" | "caml_sse2_float64_max") -> true
  | Illvm_intrinsic ("caml_rdtsc_unboxed" | "caml_rdpmc_unboxed") -> false
  | Illvm_intrinsic intr ->
      Misc.fatal_errorf "Arch.operation_is_pure: unknown llvm_intrinsic %s" intr

(* Specific operations that can raise *)

let operation_allocates = function
  | Ifar_alloc _ -> true
  | Ilea _
  | Istore_int _
  | Ioffset_loc _
  | Ifloatarithmem _
  | Ifar_poll
  | Imuladd
  | Imulsub
  | Inegmulf
  | Imuladdf
  | Inegmuladdf
  | Imulsubf
  | Inegmulsubf
  | Isqrtf
  | Imove32
  | Ishiftarith (_, _)
  | Isignext _
  | Ibswap _
  | Isextend32
  | Izextend32
  | Irdtsc
  | Irdpmc
  | Ilfence
  | Isfence
  | Imfence
  | Ipackf32
  | Isimd _
  | Isimd_mem _
  | Icldemote _
  | Iprefetch _ -> false
  | Illvm_intrinsic _intr ->
      (* Used by the zero_alloc checker that runs before the Llvmize. *)
      false

(* See `amd64/arch.ml`. *)
let equal_addressing_mode_without_displ (addressing_mode_1: addressing_mode)
      (addressing_mode_2 : addressing_mode) =
  match addressing_mode_1, addressing_mode_2 with
  | Iindexed _, Iindexed _ -> true
  | Ibased (sym1, _), Ibased (sym2, _) -> Asm_targets.Asm_symbol.equal sym1 sym2
  | (Iindexed _ | Ibased _), _ -> false

let addressing_offset_in_bytes (_addressing_mode_1: addressing_mode)
      (_addressing_mode_2 : addressing_mode) ~arg_offset_in_bytes:_ _ _ =
  None
