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

let shift_arith_operation = function
  | Arch.Ishiftadd -> Ishiftadd
  | Arch.Ishiftsub -> Ishiftsub

let rounding_mode = function
  | Simd.Rounding_mode.Current -> Round_current
  | Simd.Rounding_mode.Neg_inf -> Round_neg_inf
  | Simd.Rounding_mode.Pos_inf -> Round_pos_inf
  | Simd.Rounding_mode.Zero -> Round_zero
  | Simd.Rounding_mode.Nearest -> Round_nearest

let int_cond = function
  | Simd.Cond.EQ -> Int_EQ
  | Simd.Cond.GE -> Int_GE
  | Simd.Cond.GT -> Int_GT
  | Simd.Cond.LE -> Int_LE
  | Simd.Cond.LT -> Int_LT

let float_cond = function
  | Simd.Float_cond.EQ -> Float_EQ
  | Simd.Float_cond.GE -> Float_GE
  | Simd.Float_cond.GT -> Float_GT
  | Simd.Float_cond.LE -> Float_LE
  | Simd.Float_cond.LT -> Float_LT
  | Simd.Float_cond.NE -> Float_NE
  | Simd.Float_cond.CC -> Float_CC
  | Simd.Float_cond.CS -> Float_CS
  | Simd.Float_cond.LS -> Float_LS
  | Simd.Float_cond.HI -> Float_HI

let arm64_simd_operation = function
  | Simd.Round_f32 mode -> Round_f32 (rounding_mode mode)
  | Simd.Round_f64 mode -> Round_f64 (rounding_mode mode)
  | Simd.Roundq_f32 mode -> Roundq_f32 (rounding_mode mode)
  | Simd.Roundq_f64 mode -> Roundq_f64 (rounding_mode mode)
  | Simd.Cmp_f32 cond -> Cmp_f32 (float_cond cond)
  | Simd.Cmp_f64 cond -> Cmp_f64 (float_cond cond)
  | Simd.Cmp_s16 cond -> Cmp_s16 (int_cond cond)
  | Simd.Cmp_s32 cond -> Cmp_s32 (int_cond cond)
  | Simd.Cmp_s64 cond -> Cmp_s64 (int_cond cond)
  | Simd.Cmp_s8 cond -> Cmp_s8 (int_cond cond)
  | Simd.Cmpz_s16 cond -> Cmpz_s16 (int_cond cond)
  | Simd.Cmpz_s32 cond -> Cmpz_s32 (int_cond cond)
  | Simd.Cmpz_s64 cond -> Cmpz_s64 (int_cond cond)
  | Simd.Cmpz_s8 cond -> Cmpz_s8 (int_cond cond)
  | Simd.Extq_u8 n -> Extq_u8 n
  | Simd.Shlq_n_u16 n -> Shlq_n_u16 n
  | Simd.Shlq_n_u32 n -> Shlq_n_u32 n
  | Simd.Shlq_n_u64 n -> Shlq_n_u64 n
  | Simd.Shlq_n_u8 n -> Shlq_n_u8 n
  | Simd.Shrq_n_s16 n -> Shrq_n_s16 n
  | Simd.Shrq_n_s32 n -> Shrq_n_s32 n
  | Simd.Shrq_n_s64 n -> Shrq_n_s64 n
  | Simd.Shrq_n_s8 n -> Shrq_n_s8 n
  | Simd.Shrq_n_u16 n -> Shrq_n_u16 n
  | Simd.Shrq_n_u32 n -> Shrq_n_u32 n
  | Simd.Shrq_n_u64 n -> Shrq_n_u64 n
  | Simd.Shrq_n_u8 n -> Shrq_n_u8 n
  | Simd.Copyq_laneq_s64 { src_lane; dst_lane } -> Copyq_laneq_s64 { src_lane; dst_lane }
  | Simd.Dupq_lane_s16 { lane } -> Dupq_lane_s16 { lane }
  | Simd.Dupq_lane_s32 { lane } -> Dupq_lane_s32 { lane }
  | Simd.Dupq_lane_s64 { lane } -> Dupq_lane_s64 { lane }
  | Simd.Dupq_lane_s8 { lane } -> Dupq_lane_s8 { lane }
  | Simd.Getq_lane_s16 { lane } -> Getq_lane_s16 { lane }
  | Simd.Getq_lane_s32 { lane } -> Getq_lane_s32 { lane }
  | Simd.Getq_lane_s64 { lane } -> Getq_lane_s64 { lane }
  | Simd.Getq_lane_s8 { lane } -> Getq_lane_s8 { lane }
  | Simd.Setq_lane_s16 { lane } -> Setq_lane_s16 { lane }
  | Simd.Setq_lane_s32 { lane } -> Setq_lane_s32 { lane }
  | Simd.Setq_lane_s64 { lane } -> Setq_lane_s64 { lane }
  | Simd.Setq_lane_s8 { lane } -> Setq_lane_s8 { lane }
  | Simd.Absq_s16 -> Absq_s16
  | Simd.Absq_s32 -> Absq_s32
  | Simd.Absq_s64 -> Absq_s64
  | Simd.Absq_s8 -> Absq_s8
  | Simd.Addq_f32 -> Addq_f32
  | Simd.Addq_f64 -> Addq_f64
  | Simd.Addq_s16 -> Addq_s16
  | Simd.Addq_s32 -> Addq_s32
  | Simd.Addq_s64 -> Addq_s64
  | Simd.Addq_s8 -> Addq_s8
  | Simd.Andq_s16 -> Andq_s16
  | Simd.Andq_s32 -> Andq_s32
  | Simd.Andq_s64 -> Andq_s64
  | Simd.Andq_s8 -> Andq_s8
  | Simd.Cvt_f32_f64 -> Cvt_f32_f64
  | Simd.Cvt_f64_f32 -> Cvt_f64_f32
  | Simd.Cvtnq_s32_f32 -> Cvtnq_s32_f32
  | Simd.Cvtnq_s64_f64 -> Cvtnq_s64_f64
  | Simd.Cvtq_f32_s32 -> Cvtq_f32_s32
  | Simd.Cvtq_f64_s64 -> Cvtq_f64_s64
  | Simd.Cvtq_s32_f32 -> Cvtq_s32_f32
  | Simd.Cvtq_s64_f64 -> Cvtq_s64_f64
  | Simd.Divq_f32 -> Divq_f32
  | Simd.Divq_f64 -> Divq_f64
  | Simd.Eorq_s16 -> Eorq_s16
  | Simd.Eorq_s32 -> Eorq_s32
  | Simd.Eorq_s64 -> Eorq_s64
  | Simd.Eorq_s8 -> Eorq_s8
  | Simd.Fmax_f32 -> Fmax_f32
  | Simd.Fmax_f64 -> Fmax_f64
  | Simd.Fmin_f32 -> Fmin_f32
  | Simd.Fmin_f64 -> Fmin_f64
  | Simd.Max_scalar_f32 -> Max_scalar_f32
  | Simd.Max_scalar_f64 -> Max_scalar_f64
  | Simd.Maxq_f32 -> Maxq_f32
  | Simd.Maxq_f64 -> Maxq_f64
  | Simd.Maxq_s16 -> Maxq_s16
  | Simd.Maxq_s32 -> Maxq_s32
  | Simd.Maxq_s8 -> Maxq_s8
  | Simd.Maxq_u16 -> Maxq_u16
  | Simd.Maxq_u32 -> Maxq_u32
  | Simd.Maxq_u8 -> Maxq_u8
  | Simd.Min_scalar_f32 -> Min_scalar_f32
  | Simd.Min_scalar_f64 -> Min_scalar_f64
  | Simd.Minq_f32 -> Minq_f32
  | Simd.Minq_f64 -> Minq_f64
  | Simd.Minq_s16 -> Minq_s16
  | Simd.Minq_s32 -> Minq_s32
  | Simd.Minq_s8 -> Minq_s8
  | Simd.Minq_u16 -> Minq_u16
  | Simd.Minq_u32 -> Minq_u32
  | Simd.Minq_u8 -> Minq_u8
  | Simd.Movl_s16 -> Movl_s16
  | Simd.Movl_s32 -> Movl_s32
  | Simd.Movl_s8 -> Movl_s8
  | Simd.Movl_u16 -> Movl_u16
  | Simd.Movl_u32 -> Movl_u32
  | Simd.Movl_u8 -> Movl_u8
  | Simd.Movn_high_s16 -> Movn_high_s16
  | Simd.Movn_high_s32 -> Movn_high_s32
  | Simd.Movn_high_s64 -> Movn_high_s64
  | Simd.Movn_s16 -> Movn_s16
  | Simd.Movn_s32 -> Movn_s32
  | Simd.Movn_s64 -> Movn_s64
  | Simd.Mullq_high_s16 -> Mullq_high_s16
  | Simd.Mullq_high_u16 -> Mullq_high_u16
  | Simd.Mullq_s16 -> Mullq_s16
  | Simd.Mullq_u16 -> Mullq_u16
  | Simd.Mulq_f32 -> Mulq_f32
  | Simd.Mulq_f64 -> Mulq_f64
  | Simd.Mulq_s16 -> Mulq_s16
  | Simd.Mulq_s32 -> Mulq_s32
  | Simd.Mvnq_s16 -> Mvnq_s16
  | Simd.Mvnq_s32 -> Mvnq_s32
  | Simd.Mvnq_s64 -> Mvnq_s64
  | Simd.Mvnq_s8 -> Mvnq_s8
  | Simd.Negq_s16 -> Negq_s16
  | Simd.Negq_s32 -> Negq_s32
  | Simd.Negq_s64 -> Negq_s64
  | Simd.Negq_s8 -> Negq_s8
  | Simd.Orrq_s16 -> Orrq_s16
  | Simd.Orrq_s32 -> Orrq_s32
  | Simd.Orrq_s64 -> Orrq_s64
  | Simd.Orrq_s8 -> Orrq_s8
  | Simd.Paddq_f32 -> Paddq_f32
  | Simd.Paddq_f64 -> Paddq_f64
  | Simd.Paddq_s16 -> Paddq_s16
  | Simd.Paddq_s32 -> Paddq_s32
  | Simd.Paddq_s64 -> Paddq_s64
  | Simd.Paddq_s8 -> Paddq_s8
  | Simd.Qaddq_s16 -> Qaddq_s16
  | Simd.Qaddq_s8 -> Qaddq_s8
  | Simd.Qaddq_u16 -> Qaddq_u16
  | Simd.Qaddq_u8 -> Qaddq_u8
  | Simd.Qmovn_high_s16 -> Qmovn_high_s16
  | Simd.Qmovn_high_s32 -> Qmovn_high_s32
  | Simd.Qmovn_high_s64 -> Qmovn_high_s64
  | Simd.Qmovn_high_u16 -> Qmovn_high_u16
  | Simd.Qmovn_high_u32 -> Qmovn_high_u32
  | Simd.Qmovn_s16 -> Qmovn_s16
  | Simd.Qmovn_s32 -> Qmovn_s32
  | Simd.Qmovn_s64 -> Qmovn_s64
  | Simd.Qmovn_u16 -> Qmovn_u16
  | Simd.Qmovn_u32 -> Qmovn_u32
  | Simd.Qsubq_s16 -> Qsubq_s16
  | Simd.Qsubq_s8 -> Qsubq_s8
  | Simd.Qsubq_u16 -> Qsubq_u16
  | Simd.Qsubq_u8 -> Qsubq_u8
  | Simd.Recpeq_f32 -> Recpeq_f32
  | Simd.Round_f32_s64 -> Round_f32_s64
  | Simd.Round_f64_s64 -> Round_f64_s64
  | Simd.Rsqrteq_f32 -> Rsqrteq_f32
  | Simd.Rsqrteq_f64 -> Rsqrteq_f64
  | Simd.Shlq_s16 -> Shlq_s16
  | Simd.Shlq_s32 -> Shlq_s32
  | Simd.Shlq_s64 -> Shlq_s64
  | Simd.Shlq_s8 -> Shlq_s8
  | Simd.Shlq_u16 -> Shlq_u16
  | Simd.Shlq_u32 -> Shlq_u32
  | Simd.Shlq_u64 -> Shlq_u64
  | Simd.Shlq_u8 -> Shlq_u8
  | Simd.Sqrtq_f32 -> Sqrtq_f32
  | Simd.Sqrtq_f64 -> Sqrtq_f64
  | Simd.Subq_f32 -> Subq_f32
  | Simd.Subq_f64 -> Subq_f64
  | Simd.Subq_s16 -> Subq_s16
  | Simd.Subq_s32 -> Subq_s32
  | Simd.Subq_s64 -> Subq_s64
  | Simd.Subq_s8 -> Subq_s8
  | Simd.Zip1_f32 -> Zip1_f32
  | Simd.Zip1q_f32 -> Zip1q_f32
  | Simd.Zip1q_f64 -> Zip1q_f64
  | Simd.Zip1q_s16 -> Zip1q_s16
  | Simd.Zip1q_s8 -> Zip1q_s8
  | Simd.Zip2q_f32 -> Zip2q_f32
  | Simd.Zip2q_f64 -> Zip2q_f64
  | Simd.Zip2q_s16 -> Zip2q_s16
  | Simd.Zip2q_s8 -> Zip2q_s8
  | _ -> Unsupported_arm64_simd

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
  | Isimd simd -> Arm64_simd (arm64_simd_operation simd)
  | Illvm_intrinsic intrinsic_name -> Llvm_intrinsic intrinsic_name
