(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let string_equal (a : string) (b : string) = String.equal a b;;

[%%expect{|
val string_equal : string -> string -> bool = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__string_equal_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %6
  %7 = alloca ptr addrspace(1)
  %8 = alloca ptr addrspace(1)
  %9 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L101
L101:
  %10 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %10, ptr %7
  %11 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %11, ptr %8
  %12 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %12, ptr %5
  %13 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %13, ptr %6
  %14 = ptrtoint ptr @"\01_caml_string_equal" to i64
  %15 = load ptr addrspace(1), ptr %5
  %16 = load ptr addrspace(1), ptr %6
  %17 = ptrtoint ptr addrspace(1) %15 to i64
  %18 = ptrtoint ptr addrspace(1) %16 to i64
  %19 = icmp eq i64 %17, %18
  br i1 %19, label %L105, label %L106
L106:
  %20 = getelementptr i8, ptr addrspace(1) %15, i64 -8
  %21 = load atomic i64, ptr addrspace(1) %20 monotonic, align 8
  %22 = and i64 %21, 72057594037926912
  %23 = lshr i64 %22, 10
  %24 = shl i64 %23, 3
  %25 = sub i64 %24, 1
  %26 = getelementptr i8, ptr addrspace(1) %15, i64 %25
  %27 = load i8, ptr addrspace(1) %26, align 1
  %28 = zext i8 %27 to i64
  %29 = sub i64 %25, %28
  %30 = getelementptr i8, ptr addrspace(1) %16, i64 -8
  %31 = load atomic i64, ptr addrspace(1) %30 monotonic, align 8
  %32 = and i64 %31, 72057594037926912
  %33 = lshr i64 %32, 10
  %34 = shl i64 %33, 3
  %35 = sub i64 %34, 1
  %36 = getelementptr i8, ptr addrspace(1) %16, i64 %35
  %37 = load i8, ptr addrspace(1) %36, align 1
  %38 = zext i8 %37 to i64
  %39 = sub i64 %35, %38
  %40 = icmp eq i64 %29, %39
  br i1 %40, label %L108, label %L107
L108:
  %41 = icmp eq i64 %29, 0
  br i1 %41, label %L105, label %L111
L111:
  %42 = icmp ugt i64 %29, 24
  br i1 %42, label %L109, label %L112
L109:
  %43 = sub i64 8, 8
  %44 = shl i64 %43, 3
  %45 = shl i64 -1, %44
  %46 = load i64, ptr addrspace(1) %15, align 8
  %47 = call  i64 @llvm.bswap.i64(i64 %46)
  %48 = load i64, ptr addrspace(1) %16, align 8
  %49 = call  i64 @llvm.bswap.i64(i64 %48)
  %50 = and i64 %47, %45
  %51 = and i64 %49, %45
  %52 = icmp ne i64 %50, %51
  %53 = icmp ult i64 %50, %51
  br i1 %52, label %L107, label %L110
L110:
  %54 = sub i64 %29, 8
  %55 = addrspacecast ptr addrspace(1) %15 to ptr
  %56 = getelementptr i8, ptr %55, i64 8
  %57 = addrspacecast ptr addrspace(1) %16 to ptr
  %58 = getelementptr i8, ptr %57, i64 8
  %59 = load i64, ptr %ds
  %60 = load i64, ptr %alloc
  %61 = call oxcaml_c_directcc { i64, i64, i32 } @"\01_memcmp"(i64 %59, i64 %60, ptr %56, ptr %58, i64 %54) "gc-leaf-function"="true"
  %62 = extractvalue { i64, i64, i32 } %61, 0
  %63 = extractvalue { i64, i64, i32 } %61, 1
  store i64 %62, ptr %ds
  store i64 %63, ptr %alloc
  %64 = extractvalue { i64, i64, i32 } %61, 2
  %65 = icmp eq i32 %64, 0
  %66 = select i1 %65, i64 3, i64 1
  %67 = inttoptr i64 %66 to ptr addrspace(1)
  store ptr addrspace(1) %67, ptr %5
  br label %L104
L112:
  %68 = icmp ugt i64 %29, 8
  %69 = select i1 %68, i64 8, i64 %29
  %70 = sub i64 8, %69
  %71 = shl i64 %70, 3
  %72 = shl i64 -1, %71
  %73 = load i64, ptr addrspace(1) %15, align 8
  %74 = call  i64 @llvm.bswap.i64(i64 %73)
  %75 = load i64, ptr addrspace(1) %16, align 8
  %76 = call  i64 @llvm.bswap.i64(i64 %75)
  %77 = and i64 %74, %72
  %78 = and i64 %76, %72
  %79 = icmp ne i64 %77, %78
  %80 = icmp ult i64 %77, %78
  br i1 %79, label %L107, label %L113
L113:
  br i1 %68, label %L114, label %L105
L114:
  %81 = icmp ugt i64 %29, 16
  %82 = sub i64 %29, 8
  %83 = select i1 %81, i64 8, i64 %82
  %84 = sub i64 8, %83
  %85 = shl i64 %84, 3
  %86 = shl i64 -1, %85
  %87 = getelementptr i8, ptr addrspace(1) %15, i64 8
  %88 = load i64, ptr addrspace(1) %87, align 8
  %89 = call  i64 @llvm.bswap.i64(i64 %88)
  %90 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %91 = load i64, ptr addrspace(1) %90, align 8
  %92 = call  i64 @llvm.bswap.i64(i64 %91)
  %93 = and i64 %89, %86
  %94 = and i64 %92, %86
  %95 = icmp ne i64 %93, %94
  %96 = icmp ult i64 %93, %94
  br i1 %95, label %L107, label %L115
L115:
  br i1 %81, label %L116, label %L105
L116:
  %97 = sub i64 %29, 16
  %98 = sub i64 8, %97
  %99 = shl i64 %98, 3
  %100 = shl i64 -1, %99
  %101 = getelementptr i8, ptr addrspace(1) %15, i64 16
  %102 = load i64, ptr addrspace(1) %101, align 8
  %103 = call  i64 @llvm.bswap.i64(i64 %102)
  %104 = getelementptr i8, ptr addrspace(1) %16, i64 16
  %105 = load i64, ptr addrspace(1) %104, align 8
  %106 = call  i64 @llvm.bswap.i64(i64 %105)
  %107 = and i64 %103, %100
  %108 = and i64 %106, %100
  %109 = icmp ne i64 %107, %108
  %110 = icmp ult i64 %107, %108
  br i1 %109, label %L107, label %L105
L107:
  %111 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %111, ptr %5
  br label %L104
L105:
  %112 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %112, ptr %5
  br label %L104
L104:
  br label %L103
L103:
  %113 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %113, ptr %9
  %114 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %114, ptr %5
  %115 = load ptr addrspace(1), ptr %5
  %116 = ptrtoint ptr addrspace(1) %115 to i64
  %117 = load i64, ptr %ds
  %118 = load i64, ptr %alloc
  %119 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %117, 0, 0
  %120 = insertvalue { { i64, i64 }, { i64 } } %119, i64 %118, 0, 1
  %121 = insertvalue { { i64, i64 }, { i64 } } %120, i64 %116, 1, 0
  ret { { i64, i64 }, { i64 } } %121
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__string_equal_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	cmp	x0, x1
	b.eq	LBB0_3
; %bb.1:                                ; %L106
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x9, x8, #1
	ldrb	w10, [x0, x9]
	sub	x8, x9, x10
	ldur	x11, [x1, #-8]
	lsr	x11, x11, #7
	and	x11, x11, #0x1fffffffffff8
	sub	x11, x11, #1
	ldrb	w12, [x1, x11]
	sub	x11, x11, x12
	cmp	x8, x11
	b.ne	LBB0_8
; %bb.2:                                ; %L108
	cmp	x9, x10
	b.ne	LBB0_4
LBB0_3:                                 ; %L105
	mov	w0, #3
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_4:                                 ; %L111
	cmp	x8, #25
	b.lo	LBB0_7
; %bb.5:                                ; %L109
	ldr	x9, [x0]
	ldr	x10, [x1]
	cmp	x9, x10
	b.ne	LBB0_8
; %bb.6:                                ; %L110
	sub	x2, x8, #8
	add	x0, x0, #8
	add	x1, x1, #8
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_memcmp
	mov	sp, x29
	.cfi_restore_state
	cmp	w0, #0
	mov	w8, #3
	csinc	x0, x8, xzr, eq
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_7:                                 ; %L112
	mov	w9, #8
	subs	x9, x9, x8
	csel	x9, xzr, x9, lo
	lsl	x9, x9, #3
	ldr	x10, [x0]
	ldr	x11, [x1]
	eor	x10, x11, x10
	rev	x10, x10
	lsr	x9, x10, x9
	cbz	x9, LBB0_9
LBB0_8:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_9:                                 ; %L113
	cmp	x8, #9
	b.lo	LBB0_3
; %bb.10:                               ; %L114
	mov	w9, #16
	subs	x9, x9, x8
	csel	x9, xzr, x9, lo
	lsl	x9, x9, #3
	ldr	x10, [x0, #8]
	ldr	x11, [x1, #8]
	eor	x10, x11, x10
	rev	x10, x10
	lsr	x9, x10, x9
	cbnz	x9, LBB0_8
; %bb.11:                               ; %L115
	cmp	x8, #17
	b.lo	LBB0_3
; %bb.12:                               ; %L116
	lsl	x8, x8, #3
	ldr	x9, [x0, #16]
	ldr	x10, [x1, #16]
	eor	x9, x10, x9
	rev	x9, x9
	neg	x8, x8
	lsr	x8, x9, x8
	cbnz	x8, LBB0_8
	b	LBB0_3
	.cfi_endproc|}]

let bytes_equal (a : bytes) (b : bytes) = Bytes.equal a b;;

[%%expect{|
val bytes_equal : bytes -> bytes -> bool = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__bytes_equal_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %6
  %7 = alloca ptr addrspace(1)
  %8 = alloca ptr addrspace(1)
  %9 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L127
L127:
  %10 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %10, ptr %7
  %11 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %11, ptr %8
  %12 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %12, ptr %5
  %13 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %13, ptr %6
  %14 = ptrtoint ptr @"\01_caml_bytes_equal" to i64
  %15 = load ptr addrspace(1), ptr %5
  %16 = load ptr addrspace(1), ptr %6
  %17 = ptrtoint ptr addrspace(1) %15 to i64
  %18 = ptrtoint ptr addrspace(1) %16 to i64
  %19 = icmp eq i64 %17, %18
  br i1 %19, label %L131, label %L132
L132:
  %20 = getelementptr i8, ptr addrspace(1) %15, i64 -8
  %21 = load atomic i64, ptr addrspace(1) %20 monotonic, align 8
  %22 = and i64 %21, 72057594037926912
  %23 = lshr i64 %22, 10
  %24 = shl i64 %23, 3
  %25 = sub i64 %24, 1
  %26 = getelementptr i8, ptr addrspace(1) %15, i64 %25
  %27 = load i8, ptr addrspace(1) %26, align 1
  %28 = zext i8 %27 to i64
  %29 = sub i64 %25, %28
  %30 = getelementptr i8, ptr addrspace(1) %16, i64 -8
  %31 = load atomic i64, ptr addrspace(1) %30 monotonic, align 8
  %32 = and i64 %31, 72057594037926912
  %33 = lshr i64 %32, 10
  %34 = shl i64 %33, 3
  %35 = sub i64 %34, 1
  %36 = getelementptr i8, ptr addrspace(1) %16, i64 %35
  %37 = load i8, ptr addrspace(1) %36, align 1
  %38 = zext i8 %37 to i64
  %39 = sub i64 %35, %38
  %40 = icmp eq i64 %29, %39
  br i1 %40, label %L134, label %L133
L134:
  %41 = icmp eq i64 %29, 0
  br i1 %41, label %L131, label %L137
L137:
  %42 = icmp ugt i64 %29, 24
  br i1 %42, label %L135, label %L138
L135:
  %43 = sub i64 8, 8
  %44 = shl i64 %43, 3
  %45 = shl i64 -1, %44
  %46 = load i64, ptr addrspace(1) %15, align 8
  %47 = call  i64 @llvm.bswap.i64(i64 %46)
  %48 = load i64, ptr addrspace(1) %16, align 8
  %49 = call  i64 @llvm.bswap.i64(i64 %48)
  %50 = and i64 %47, %45
  %51 = and i64 %49, %45
  %52 = icmp ne i64 %50, %51
  %53 = icmp ult i64 %50, %51
  br i1 %52, label %L133, label %L136
L136:
  %54 = sub i64 %29, 8
  %55 = addrspacecast ptr addrspace(1) %15 to ptr
  %56 = getelementptr i8, ptr %55, i64 8
  %57 = addrspacecast ptr addrspace(1) %16 to ptr
  %58 = getelementptr i8, ptr %57, i64 8
  %59 = load i64, ptr %ds
  %60 = load i64, ptr %alloc
  %61 = call oxcaml_c_directcc { i64, i64, i32 } @"\01_memcmp"(i64 %59, i64 %60, ptr %56, ptr %58, i64 %54) "gc-leaf-function"="true"
  %62 = extractvalue { i64, i64, i32 } %61, 0
  %63 = extractvalue { i64, i64, i32 } %61, 1
  store i64 %62, ptr %ds
  store i64 %63, ptr %alloc
  %64 = extractvalue { i64, i64, i32 } %61, 2
  %65 = icmp eq i32 %64, 0
  %66 = select i1 %65, i64 3, i64 1
  %67 = inttoptr i64 %66 to ptr addrspace(1)
  store ptr addrspace(1) %67, ptr %5
  br label %L130
L138:
  %68 = icmp ugt i64 %29, 8
  %69 = select i1 %68, i64 8, i64 %29
  %70 = sub i64 8, %69
  %71 = shl i64 %70, 3
  %72 = shl i64 -1, %71
  %73 = load i64, ptr addrspace(1) %15, align 8
  %74 = call  i64 @llvm.bswap.i64(i64 %73)
  %75 = load i64, ptr addrspace(1) %16, align 8
  %76 = call  i64 @llvm.bswap.i64(i64 %75)
  %77 = and i64 %74, %72
  %78 = and i64 %76, %72
  %79 = icmp ne i64 %77, %78
  %80 = icmp ult i64 %77, %78
  br i1 %79, label %L133, label %L139
L139:
  br i1 %68, label %L140, label %L131
L140:
  %81 = icmp ugt i64 %29, 16
  %82 = sub i64 %29, 8
  %83 = select i1 %81, i64 8, i64 %82
  %84 = sub i64 8, %83
  %85 = shl i64 %84, 3
  %86 = shl i64 -1, %85
  %87 = getelementptr i8, ptr addrspace(1) %15, i64 8
  %88 = load i64, ptr addrspace(1) %87, align 8
  %89 = call  i64 @llvm.bswap.i64(i64 %88)
  %90 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %91 = load i64, ptr addrspace(1) %90, align 8
  %92 = call  i64 @llvm.bswap.i64(i64 %91)
  %93 = and i64 %89, %86
  %94 = and i64 %92, %86
  %95 = icmp ne i64 %93, %94
  %96 = icmp ult i64 %93, %94
  br i1 %95, label %L133, label %L141
L141:
  br i1 %81, label %L142, label %L131
L142:
  %97 = sub i64 %29, 16
  %98 = sub i64 8, %97
  %99 = shl i64 %98, 3
  %100 = shl i64 -1, %99
  %101 = getelementptr i8, ptr addrspace(1) %15, i64 16
  %102 = load i64, ptr addrspace(1) %101, align 8
  %103 = call  i64 @llvm.bswap.i64(i64 %102)
  %104 = getelementptr i8, ptr addrspace(1) %16, i64 16
  %105 = load i64, ptr addrspace(1) %104, align 8
  %106 = call  i64 @llvm.bswap.i64(i64 %105)
  %107 = and i64 %103, %100
  %108 = and i64 %106, %100
  %109 = icmp ne i64 %107, %108
  %110 = icmp ult i64 %107, %108
  br i1 %109, label %L133, label %L131
L133:
  %111 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %111, ptr %5
  br label %L130
L131:
  %112 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %112, ptr %5
  br label %L130
L130:
  br label %L129
L129:
  %113 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %113, ptr %9
  %114 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %114, ptr %5
  %115 = load ptr addrspace(1), ptr %5
  %116 = ptrtoint ptr addrspace(1) %115 to i64
  %117 = load i64, ptr %ds
  %118 = load i64, ptr %alloc
  %119 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %117, 0, 0
  %120 = insertvalue { { i64, i64 }, { i64 } } %119, i64 %118, 0, 1
  %121 = insertvalue { { i64, i64 }, { i64 } } %120, i64 %116, 1, 0
  ret { { i64, i64 }, { i64 } } %121
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__bytes_equal_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	cmp	x0, x1
	b.eq	LBB0_3
; %bb.1:                                ; %L132
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x9, x8, #1
	ldrb	w10, [x0, x9]
	sub	x8, x9, x10
	ldur	x11, [x1, #-8]
	lsr	x11, x11, #7
	and	x11, x11, #0x1fffffffffff8
	sub	x11, x11, #1
	ldrb	w12, [x1, x11]
	sub	x11, x11, x12
	cmp	x8, x11
	b.ne	LBB0_8
; %bb.2:                                ; %L134
	cmp	x9, x10
	b.ne	LBB0_4
LBB0_3:                                 ; %L131
	mov	w0, #3
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_4:                                 ; %L137
	cmp	x8, #25
	b.lo	LBB0_7
; %bb.5:                                ; %L135
	ldr	x9, [x0]
	ldr	x10, [x1]
	cmp	x9, x10
	b.ne	LBB0_8
; %bb.6:                                ; %L136
	sub	x2, x8, #8
	add	x0, x0, #8
	add	x1, x1, #8
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_memcmp
	mov	sp, x29
	.cfi_restore_state
	cmp	w0, #0
	mov	w8, #3
	csinc	x0, x8, xzr, eq
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_7:                                 ; %L138
	mov	w9, #8
	subs	x9, x9, x8
	csel	x9, xzr, x9, lo
	lsl	x9, x9, #3
	ldr	x10, [x0]
	ldr	x11, [x1]
	eor	x10, x11, x10
	rev	x10, x10
	lsr	x9, x10, x9
	cbz	x9, LBB0_9
LBB0_8:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_9:                                 ; %L139
	cmp	x8, #9
	b.lo	LBB0_3
; %bb.10:                               ; %L140
	mov	w9, #16
	subs	x9, x9, x8
	csel	x9, xzr, x9, lo
	lsl	x9, x9, #3
	ldr	x10, [x0, #8]
	ldr	x11, [x1, #8]
	eor	x10, x11, x10
	rev	x10, x10
	lsr	x9, x10, x9
	cbnz	x9, LBB0_8
; %bb.11:                               ; %L141
	cmp	x8, #17
	b.lo	LBB0_3
; %bb.12:                               ; %L142
	lsl	x8, x8, #3
	ldr	x9, [x0, #16]
	ldr	x10, [x1, #16]
	eor	x9, x10, x9
	rev	x9, x9
	neg	x8, x8
	lsr	x8, x9, x8
	cbnz	x8, LBB0_8
	b	LBB0_3
	.cfi_endproc|}]
