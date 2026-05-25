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
  | Amd64_cldemote
  | Amd64_prefetch
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
  | Arm64_simd of Simd.operation
  | Llvm_intrinsic of string

let shift_arith_operation = function
  | Arch.Ishiftadd -> Ishiftadd
  | Arch.Ishiftsub -> Ishiftsub

let classify (op : Arch.specific_operation) =
  match op with
  | Ifar_poll | Ifar_alloc _ ->
    Misc.fatal_error "Llvmize_specific.classify: unexpected ARM64 far op"
  | Ishiftarith (op, shift) ->
    Arm64_shiftarith (shift_arith_operation op, shift)
  | Imuladd -> Arm64_muladd
  | Imulsub -> Arm64_mulsub
  | Inegmulf -> Arm64_negmulf
  | Imuladdf -> Arm64_muladdf
  | Inegmuladdf -> Arm64_negmuladdf
  | Imulsubf -> Arm64_mulsubf
  | Inegmulsubf -> Arm64_negmulsubf
  | Isqrtf -> Arm64_sqrtf
  | Ibswap { bitwidth } -> Bswap bitwidth
  | Imove32 -> Arm64_move32
  | Isignext size -> Arm64_signext size
  | Isimd simd -> Arm64_simd simd
  | Illvm_intrinsic intrinsic_name -> Llvm_intrinsic intrinsic_name
