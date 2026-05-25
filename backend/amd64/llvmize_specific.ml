open Llvmize_specific_types

type float_operation =
  | Ifloatadd
  | Ifloatsub
  | Ifloatmul
  | Ifloatdiv

type shift_arith_operation =
  | Ishiftadd
  | Ishiftsub

type operation =
  | Amd64_lea of Arch.addressing_mode
  | Amd64_store_int of nativeint * Arch.addressing_mode * bool
  | Amd64_offset_loc of int * Arch.addressing_mode
  | Amd64_floatarithmem of
      Cmm.float_width * float_operation * Arch.addressing_mode
  | Bswap of Arch.bswap_bitwidth
  | Amd64_sextend32
  | Amd64_zextend32
  | Amd64_rdtsc
  | Amd64_rdpmc
  | Amd64_lfence
  | Amd64_sfence
  | Amd64_mfence
  | Amd64_packf32
  | Amd64_simd of Amd64_simd_instrs.id * int option
  | Amd64_simd_mem
  | Amd64_cldemote of Arch.addressing_mode
  | Amd64_prefetch of
      { is_write : bool;
        locality : amd64_prefetch_temporal_locality_hint;
        addr : Arch.addressing_mode
      }
  | Arm64_shiftarith of shift_arith_operation * int
  | Arm64_muladd
  | Arm64_mulsub
  | Arm64_negmulf
  | Arm64_muladdf
  | Arm64_negmuladdf
  | Arm64_mulsubf
  | Arm64_negmulsubf
  | Arm64_sqrtf
  | Arm64_move32
  | Arm64_signext of int
  | Arm64_simd of arm64_simd_operation
  | Llvm_intrinsic of string

let float_operation = function
  | Arch.Ifloatadd -> Ifloatadd
  | Arch.Ifloatsub -> Ifloatsub
  | Arch.Ifloatmul -> Ifloatmul
  | Arch.Ifloatdiv -> Ifloatdiv

let prefetch_temporal_locality_hint = function
  | Arch.Nonlocal -> Prefetch_nonlocal
  | Arch.Low -> Prefetch_low
  | Arch.Moderate -> Prefetch_moderate
  | Arch.High -> Prefetch_high

let classify (op : Arch.specific_operation) =
  match op with
  | Ilea addr -> Amd64_lea addr
  | Istore_int (n, addr, is_modify) -> Amd64_store_int (n, addr, is_modify)
  | Ioffset_loc (n, addr) -> Amd64_offset_loc (n, addr)
  | Ifloatarithmem (width, op, addr) ->
    Amd64_floatarithmem (width, float_operation op, addr)
  | Ibswap { bitwidth } -> Bswap bitwidth
  | Isextend32 -> Amd64_sextend32
  | Izextend32 -> Amd64_zextend32
  | Irdtsc -> Amd64_rdtsc
  | Irdpmc -> Amd64_rdpmc
  | Ilfence -> Amd64_lfence
  | Isfence -> Amd64_sfence
  | Imfence -> Amd64_mfence
  | Ipackf32 -> Amd64_packf32
  | Isimd simd ->
    Amd64_simd ((Simd.Pseudo_instr.instr simd.instr).id, simd.imm)
  | Isimd_mem _ -> Amd64_simd_mem
  | Icldemote addr -> Amd64_cldemote addr
  | Iprefetch { is_write; locality; addr } ->
    Amd64_prefetch
      { is_write; locality = prefetch_temporal_locality_hint locality; addr }
  | Illvm_intrinsic intrinsic_name -> Llvm_intrinsic intrinsic_name
