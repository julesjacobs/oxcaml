type float_operation =
  | Ifloatadd
  | Ifloatsub
  | Ifloatmul
  | Ifloatdiv

type shift_arith_operation =
  | Ishiftadd
  | Ishiftsub

type rounding_mode =
  | Round_current
  | Round_neg_inf
  | Round_pos_inf
  | Round_zero
  | Round_nearest

type int_cond =
  | Int_EQ
  | Int_GE
  | Int_GT
  | Int_LE
  | Int_LT

type float_cond =
  | Float_EQ
  | Float_GE
  | Float_GT
  | Float_LE
  | Float_LT
  | Float_NE
  | Float_CC
  | Float_CS
  | Float_LS
  | Float_HI

type arm64_simd_operation =
  | Round_f32 of rounding_mode
  | Round_f64 of rounding_mode
  | Roundq_f32 of rounding_mode
  | Roundq_f64 of rounding_mode
  | Cmp_f32 of float_cond
  | Cmp_f64 of float_cond
  | Cmp_s16 of int_cond
  | Cmp_s32 of int_cond
  | Cmp_s64 of int_cond
  | Cmp_s8 of int_cond
  | Cmpz_s16 of int_cond
  | Cmpz_s32 of int_cond
  | Cmpz_s64 of int_cond
  | Cmpz_s8 of int_cond
  | Extq_u8 of int
  | Shlq_n_u16 of int
  | Shlq_n_u32 of int
  | Shlq_n_u64 of int
  | Shlq_n_u8 of int
  | Shrq_n_s16 of int
  | Shrq_n_s32 of int
  | Shrq_n_s64 of int
  | Shrq_n_s8 of int
  | Shrq_n_u16 of int
  | Shrq_n_u32 of int
  | Shrq_n_u64 of int
  | Shrq_n_u8 of int
  | Copyq_laneq_s64 of { src_lane : int; dst_lane : int }
  | Dupq_lane_s16 of { lane : int }
  | Dupq_lane_s32 of { lane : int }
  | Dupq_lane_s64 of { lane : int }
  | Dupq_lane_s8 of { lane : int }
  | Getq_lane_s16 of { lane : int }
  | Getq_lane_s32 of { lane : int }
  | Getq_lane_s64 of { lane : int }
  | Getq_lane_s8 of { lane : int }
  | Setq_lane_s16 of { lane : int }
  | Setq_lane_s32 of { lane : int }
  | Setq_lane_s64 of { lane : int }
  | Setq_lane_s8 of { lane : int }
  | Absq_s16
  | Absq_s32
  | Absq_s64
  | Absq_s8
  | Addq_f32
  | Addq_f64
  | Addq_s16
  | Addq_s32
  | Addq_s64
  | Addq_s8
  | Andq_s16
  | Andq_s32
  | Andq_s64
  | Andq_s8
  | Cvt_f32_f64
  | Cvt_f64_f32
  | Cvtnq_s32_f32
  | Cvtnq_s64_f64
  | Cvtq_f32_s32
  | Cvtq_f64_s64
  | Cvtq_s32_f32
  | Cvtq_s64_f64
  | Divq_f32
  | Divq_f64
  | Eorq_s16
  | Eorq_s32
  | Eorq_s64
  | Eorq_s8
  | Fmax_f32
  | Fmax_f64
  | Fmin_f32
  | Fmin_f64
  | Max_scalar_f32
  | Max_scalar_f64
  | Maxq_f32
  | Maxq_f64
  | Maxq_s16
  | Maxq_s32
  | Maxq_s8
  | Maxq_u16
  | Maxq_u32
  | Maxq_u8
  | Min_scalar_f32
  | Min_scalar_f64
  | Minq_f32
  | Minq_f64
  | Minq_s16
  | Minq_s32
  | Minq_s8
  | Minq_u16
  | Minq_u32
  | Minq_u8
  | Movl_s16
  | Movl_s32
  | Movl_s8
  | Movl_u16
  | Movl_u32
  | Movl_u8
  | Movn_high_s16
  | Movn_high_s32
  | Movn_high_s64
  | Movn_s16
  | Movn_s32
  | Movn_s64
  | Mullq_high_s16
  | Mullq_high_u16
  | Mullq_s16
  | Mullq_u16
  | Mulq_f32
  | Mulq_f64
  | Mulq_s16
  | Mulq_s32
  | Mvnq_s16
  | Mvnq_s32
  | Mvnq_s64
  | Mvnq_s8
  | Negq_s16
  | Negq_s32
  | Negq_s64
  | Negq_s8
  | Orrq_s16
  | Orrq_s32
  | Orrq_s64
  | Orrq_s8
  | Paddq_f32
  | Paddq_f64
  | Paddq_s16
  | Paddq_s32
  | Paddq_s64
  | Paddq_s8
  | Qaddq_s16
  | Qaddq_s8
  | Qaddq_u16
  | Qaddq_u8
  | Qmovn_high_s16
  | Qmovn_high_s32
  | Qmovn_high_s64
  | Qmovn_high_u16
  | Qmovn_high_u32
  | Qmovn_s16
  | Qmovn_s32
  | Qmovn_s64
  | Qmovn_u16
  | Qmovn_u32
  | Qsubq_s16
  | Qsubq_s8
  | Qsubq_u16
  | Qsubq_u8
  | Recpeq_f32
  | Round_f32_s64
  | Round_f64_s64
  | Rsqrteq_f32
  | Rsqrteq_f64
  | Shlq_s16
  | Shlq_s32
  | Shlq_s64
  | Shlq_s8
  | Shlq_u16
  | Shlq_u32
  | Shlq_u64
  | Shlq_u8
  | Sqrtq_f32
  | Sqrtq_f64
  | Subq_f32
  | Subq_f64
  | Subq_s16
  | Subq_s32
  | Subq_s64
  | Subq_s8
  | Zip1_f32
  | Zip1q_f32
  | Zip1q_f64
  | Zip1q_s16
  | Zip1q_s8
  | Zip2q_f32
  | Zip2q_f64
  | Zip2q_s16
  | Zip2q_s8
  | Unsupported_arm64_simd

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
  | Arm64_simd of arm64_simd_operation
  | Llvm_intrinsic of string

let float_operation = function
  | Arch.Ifloatadd -> Ifloatadd
  | Arch.Ifloatsub -> Ifloatsub
  | Arch.Ifloatmul -> Ifloatmul
  | Arch.Ifloatdiv -> Ifloatdiv

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
  | Icldemote _ -> Amd64_cldemote
  | Iprefetch _ -> Amd64_prefetch
  | Illvm_intrinsic intrinsic_name -> Llvm_intrinsic intrinsic_name
