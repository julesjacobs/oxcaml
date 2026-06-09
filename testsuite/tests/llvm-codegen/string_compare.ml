(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let string_compare (a : string) (b : string) = String.compare a b;;

[%%expect{|
val string_compare : string -> string -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__string_compare_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %14 = ptrtoint ptr @"\01_caml_string_compare" to i64
  %15 = load ptr addrspace(1), ptr %5
  %16 = load ptr addrspace(1), ptr %6
  %17 = ptrtoint ptr addrspace(1) %15 to i64
  %18 = ptrtoint ptr addrspace(1) %16 to i64
  %19 = icmp eq i64 %17, %18
  br i1 %19, label %L105, label %L106
L105:
  %20 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %20, ptr %5
  br label %L104
L106:
  %21 = getelementptr i8, ptr addrspace(1) %15, i64 -8
  %22 = load atomic i64, ptr addrspace(1) %21 monotonic, align 8
  %23 = and i64 %22, 72057594037926912
  %24 = lshr i64 %23, 10
  %25 = shl i64 %24, 3
  %26 = sub i64 %25, 1
  %27 = getelementptr i8, ptr addrspace(1) %15, i64 %26
  %28 = load i8, ptr addrspace(1) %27, align 1
  %29 = zext i8 %28 to i64
  %30 = sub i64 %26, %29
  %31 = getelementptr i8, ptr addrspace(1) %16, i64 -8
  %32 = load atomic i64, ptr addrspace(1) %31 monotonic, align 8
  %33 = and i64 %32, 72057594037926912
  %34 = lshr i64 %33, 10
  %35 = shl i64 %34, 3
  %36 = sub i64 %35, 1
  %37 = getelementptr i8, ptr addrspace(1) %16, i64 %36
  %38 = load i8, ptr addrspace(1) %37, align 1
  %39 = zext i8 %38 to i64
  %40 = sub i64 %36, %39
  %41 = icmp ult i64 %30, %40
  %42 = select i1 %41, i64 %30, i64 %40
  %43 = icmp ugt i64 %42, 16
  br i1 %43, label %L107, label %L110
L107:
  %44 = sub i64 8, 8
  %45 = shl i64 %44, 3
  %46 = shl i64 -1, %45
  %47 = load i64, ptr addrspace(1) %15, align 8
  %48 = call  i64 @llvm.bswap.i64(i64 %47)
  %49 = load i64, ptr addrspace(1) %16, align 8
  %50 = call  i64 @llvm.bswap.i64(i64 %49)
  %51 = and i64 %48, %46
  %52 = and i64 %50, %46
  %53 = icmp ne i64 %51, %52
  %54 = icmp ult i64 %51, %52
  br i1 %53, label %L108, label %L109
L108:
  %55 = select i1 %54, i64 -1, i64 3
  %56 = inttoptr i64 %55 to ptr addrspace(1)
  store ptr addrspace(1) %56, ptr %5
  br label %L104
L109:
  %57 = sub i64 %42, 8
  %58 = addrspacecast ptr addrspace(1) %15 to ptr
  %59 = getelementptr i8, ptr %58, i64 8
  %60 = addrspacecast ptr addrspace(1) %16 to ptr
  %61 = getelementptr i8, ptr %60, i64 8
  %62 = load i64, ptr %ds
  %63 = load i64, ptr %alloc
  %64 = call oxcaml_c_directcc { i64, i64, i32 } @"\01_memcmp"(i64 %62, i64 %63, ptr %59, ptr %61, i64 %57) "gc-leaf-function"="true"
  %65 = extractvalue { i64, i64, i32 } %64, 0
  %66 = extractvalue { i64, i64, i32 } %64, 1
  store i64 %65, ptr %ds
  store i64 %66, ptr %alloc
  %67 = extractvalue { i64, i64, i32 } %64, 2
  %68 = icmp slt i32 %67, 0
  %69 = icmp ne i32 %67, 0
  %70 = select i1 %68, i64 -1, i64 3
  %71 = icmp ult i64 %30, %40
  %72 = icmp ugt i64 %30, %40
  %73 = select i1 %72, i64 3, i64 1
  %74 = select i1 %71, i64 -1, i64 %73
  %75 = select i1 %69, i64 %70, i64 %74
  %76 = inttoptr i64 %75 to ptr addrspace(1)
  store ptr addrspace(1) %76, ptr %5
  br label %L104
L110:
  %77 = icmp eq i64 %42, 0
  br i1 %77, label %L111, label %L112
L112:
  %78 = icmp ugt i64 %42, 8
  %79 = select i1 %78, i64 8, i64 %42
  %80 = sub i64 8, %79
  %81 = shl i64 %80, 3
  %82 = shl i64 -1, %81
  %83 = load i64, ptr addrspace(1) %15, align 8
  %84 = call  i64 @llvm.bswap.i64(i64 %83)
  %85 = load i64, ptr addrspace(1) %16, align 8
  %86 = call  i64 @llvm.bswap.i64(i64 %85)
  %87 = and i64 %84, %82
  %88 = and i64 %86, %82
  %89 = icmp ne i64 %87, %88
  %90 = icmp ult i64 %87, %88
  br i1 %89, label %L113, label %L114
L113:
  %91 = select i1 %90, i64 -1, i64 3
  %92 = inttoptr i64 %91 to ptr addrspace(1)
  store ptr addrspace(1) %92, ptr %5
  br label %L104
L114:
  br i1 %78, label %L115, label %L111
L115:
  %93 = sub i64 %42, 8
  %94 = sub i64 8, %93
  %95 = shl i64 %94, 3
  %96 = shl i64 -1, %95
  %97 = getelementptr i8, ptr addrspace(1) %15, i64 8
  %98 = load i64, ptr addrspace(1) %97, align 8
  %99 = call  i64 @llvm.bswap.i64(i64 %98)
  %100 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %101 = load i64, ptr addrspace(1) %100, align 8
  %102 = call  i64 @llvm.bswap.i64(i64 %101)
  %103 = and i64 %99, %96
  %104 = and i64 %102, %96
  %105 = icmp ne i64 %103, %104
  %106 = icmp ult i64 %103, %104
  br i1 %105, label %L116, label %L111
L116:
  %107 = select i1 %106, i64 -1, i64 3
  %108 = inttoptr i64 %107 to ptr addrspace(1)
  store ptr addrspace(1) %108, ptr %5
  br label %L104
L111:
  %109 = icmp ult i64 %30, %40
  %110 = icmp ugt i64 %30, %40
  %111 = select i1 %110, i64 3, i64 1
  %112 = select i1 %109, i64 -1, i64 %111
  %113 = inttoptr i64 %112 to ptr addrspace(1)
  store ptr addrspace(1) %113, ptr %5
  br label %L104
L104:
  br label %L103
L103:
  %114 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %114, ptr %9
  %115 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %115, ptr %5
  %116 = load ptr addrspace(1), ptr %5
  %117 = ptrtoint ptr addrspace(1) %116 to i64
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %118, 0, 0
  %121 = insertvalue { { i64, i64 }, { i64 } } %120, i64 %119, 0, 1
  %122 = insertvalue { { i64, i64 }, { i64 } } %121, i64 %117, 1, 0
  ret { { i64, i64 }, { i64 } } %122
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__string_compare_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	cmp	x0, x1
	b.eq	LBB0_4
; %bb.1:                                ; %L106
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x19, x8, x9
	ldur	x8, [x1, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x1, x8]
	sub	x20, x8, x9
	cmp	x19, x20
	csel	x8, x19, x20, lo
	cmp	x8, #17
	b.lo	LBB0_5
; %bb.2:                                ; %L107
	ldr	x9, [x0]
	ldr	x10, [x1]
	cmp	x9, x10
	b.ne	LBB0_10
; %bb.3:                                ; %L109
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
	cmp	x19, x20
	mov	w8, #3
	csinc	x9, x8, xzr, hi
	csinv	x9, x9, xzr, hs
	cmp	w0, #0
	csinv	x8, x8, xzr, ge
	csel	x0, x9, x8, eq
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_4:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_5:                                 ; %L110
	cbz	x8, LBB0_9
; %bb.6:                                ; %L112
	mov	w9, #8
	subs	x9, x9, x8
	csel	x9, xzr, x9, lo
	lsl	x9, x9, #3
	mov	x10, #-1
	lsl	x9, x10, x9
	ldr	x10, [x0]
	rev	x10, x10
	ldr	x11, [x1]
	rev	x11, x11
	and	x10, x10, x9
	and	x9, x11, x9
	cmp	x10, x9
	b.ne	LBB0_11
; %bb.7:                                ; %L114
	cmp	x8, #9
	b.lo	LBB0_9
; %bb.8:                                ; %L115
	lsl	x8, x8, #3
	neg	x8, x8
	mov	x9, #-1
	lsl	x8, x9, x8
	ldr	x9, [x0, #8]
	rev	x9, x9
	ldr	x10, [x1, #8]
	rev	x10, x10
	and	x9, x9, x8
	and	x8, x10, x8
	cmp	x9, x8
	b.ne	LBB0_11
LBB0_9:                                 ; %L111
	cmp	x19, x20
	mov	w8, #3
	csinc	x8, x8, xzr, hi
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_10:                                ; %L108
	rev	x8, x9
	rev	x9, x10
	cmp	x8, x9
LBB0_11:                                ; %L113
	mov	w8, #3
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc|}]

let bytes_compare (a : bytes) (b : bytes) = Bytes.compare a b;;

[%%expect{|
val bytes_compare : bytes -> bytes -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__bytes_compare_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %14 = ptrtoint ptr @"\01_caml_bytes_compare" to i64
  %15 = load ptr addrspace(1), ptr %5
  %16 = load ptr addrspace(1), ptr %6
  %17 = ptrtoint ptr addrspace(1) %15 to i64
  %18 = ptrtoint ptr addrspace(1) %16 to i64
  %19 = icmp eq i64 %17, %18
  br i1 %19, label %L131, label %L132
L131:
  %20 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %20, ptr %5
  br label %L130
L132:
  %21 = getelementptr i8, ptr addrspace(1) %15, i64 -8
  %22 = load atomic i64, ptr addrspace(1) %21 monotonic, align 8
  %23 = and i64 %22, 72057594037926912
  %24 = lshr i64 %23, 10
  %25 = shl i64 %24, 3
  %26 = sub i64 %25, 1
  %27 = getelementptr i8, ptr addrspace(1) %15, i64 %26
  %28 = load i8, ptr addrspace(1) %27, align 1
  %29 = zext i8 %28 to i64
  %30 = sub i64 %26, %29
  %31 = getelementptr i8, ptr addrspace(1) %16, i64 -8
  %32 = load atomic i64, ptr addrspace(1) %31 monotonic, align 8
  %33 = and i64 %32, 72057594037926912
  %34 = lshr i64 %33, 10
  %35 = shl i64 %34, 3
  %36 = sub i64 %35, 1
  %37 = getelementptr i8, ptr addrspace(1) %16, i64 %36
  %38 = load i8, ptr addrspace(1) %37, align 1
  %39 = zext i8 %38 to i64
  %40 = sub i64 %36, %39
  %41 = icmp ult i64 %30, %40
  %42 = select i1 %41, i64 %30, i64 %40
  %43 = icmp ugt i64 %42, 16
  br i1 %43, label %L133, label %L136
L133:
  %44 = sub i64 8, 8
  %45 = shl i64 %44, 3
  %46 = shl i64 -1, %45
  %47 = load i64, ptr addrspace(1) %15, align 8
  %48 = call  i64 @llvm.bswap.i64(i64 %47)
  %49 = load i64, ptr addrspace(1) %16, align 8
  %50 = call  i64 @llvm.bswap.i64(i64 %49)
  %51 = and i64 %48, %46
  %52 = and i64 %50, %46
  %53 = icmp ne i64 %51, %52
  %54 = icmp ult i64 %51, %52
  br i1 %53, label %L134, label %L135
L134:
  %55 = select i1 %54, i64 -1, i64 3
  %56 = inttoptr i64 %55 to ptr addrspace(1)
  store ptr addrspace(1) %56, ptr %5
  br label %L130
L135:
  %57 = sub i64 %42, 8
  %58 = addrspacecast ptr addrspace(1) %15 to ptr
  %59 = getelementptr i8, ptr %58, i64 8
  %60 = addrspacecast ptr addrspace(1) %16 to ptr
  %61 = getelementptr i8, ptr %60, i64 8
  %62 = load i64, ptr %ds
  %63 = load i64, ptr %alloc
  %64 = call oxcaml_c_directcc { i64, i64, i32 } @"\01_memcmp"(i64 %62, i64 %63, ptr %59, ptr %61, i64 %57) "gc-leaf-function"="true"
  %65 = extractvalue { i64, i64, i32 } %64, 0
  %66 = extractvalue { i64, i64, i32 } %64, 1
  store i64 %65, ptr %ds
  store i64 %66, ptr %alloc
  %67 = extractvalue { i64, i64, i32 } %64, 2
  %68 = icmp slt i32 %67, 0
  %69 = icmp ne i32 %67, 0
  %70 = select i1 %68, i64 -1, i64 3
  %71 = icmp ult i64 %30, %40
  %72 = icmp ugt i64 %30, %40
  %73 = select i1 %72, i64 3, i64 1
  %74 = select i1 %71, i64 -1, i64 %73
  %75 = select i1 %69, i64 %70, i64 %74
  %76 = inttoptr i64 %75 to ptr addrspace(1)
  store ptr addrspace(1) %76, ptr %5
  br label %L130
L136:
  %77 = icmp eq i64 %42, 0
  br i1 %77, label %L137, label %L138
L138:
  %78 = icmp ugt i64 %42, 8
  %79 = select i1 %78, i64 8, i64 %42
  %80 = sub i64 8, %79
  %81 = shl i64 %80, 3
  %82 = shl i64 -1, %81
  %83 = load i64, ptr addrspace(1) %15, align 8
  %84 = call  i64 @llvm.bswap.i64(i64 %83)
  %85 = load i64, ptr addrspace(1) %16, align 8
  %86 = call  i64 @llvm.bswap.i64(i64 %85)
  %87 = and i64 %84, %82
  %88 = and i64 %86, %82
  %89 = icmp ne i64 %87, %88
  %90 = icmp ult i64 %87, %88
  br i1 %89, label %L139, label %L140
L139:
  %91 = select i1 %90, i64 -1, i64 3
  %92 = inttoptr i64 %91 to ptr addrspace(1)
  store ptr addrspace(1) %92, ptr %5
  br label %L130
L140:
  br i1 %78, label %L141, label %L137
L141:
  %93 = sub i64 %42, 8
  %94 = sub i64 8, %93
  %95 = shl i64 %94, 3
  %96 = shl i64 -1, %95
  %97 = getelementptr i8, ptr addrspace(1) %15, i64 8
  %98 = load i64, ptr addrspace(1) %97, align 8
  %99 = call  i64 @llvm.bswap.i64(i64 %98)
  %100 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %101 = load i64, ptr addrspace(1) %100, align 8
  %102 = call  i64 @llvm.bswap.i64(i64 %101)
  %103 = and i64 %99, %96
  %104 = and i64 %102, %96
  %105 = icmp ne i64 %103, %104
  %106 = icmp ult i64 %103, %104
  br i1 %105, label %L142, label %L137
L142:
  %107 = select i1 %106, i64 -1, i64 3
  %108 = inttoptr i64 %107 to ptr addrspace(1)
  store ptr addrspace(1) %108, ptr %5
  br label %L130
L137:
  %109 = icmp ult i64 %30, %40
  %110 = icmp ugt i64 %30, %40
  %111 = select i1 %110, i64 3, i64 1
  %112 = select i1 %109, i64 -1, i64 %111
  %113 = inttoptr i64 %112 to ptr addrspace(1)
  store ptr addrspace(1) %113, ptr %5
  br label %L130
L130:
  br label %L129
L129:
  %114 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %114, ptr %9
  %115 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %115, ptr %5
  %116 = load ptr addrspace(1), ptr %5
  %117 = ptrtoint ptr addrspace(1) %116 to i64
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %118, 0, 0
  %121 = insertvalue { { i64, i64 }, { i64 } } %120, i64 %119, 0, 1
  %122 = insertvalue { { i64, i64 }, { i64 } } %121, i64 %117, 1, 0
  ret { { i64, i64 }, { i64 } } %122
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__bytes_compare_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	cmp	x0, x1
	b.eq	LBB0_4
; %bb.1:                                ; %L132
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x19, x8, x9
	ldur	x8, [x1, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x1, x8]
	sub	x20, x8, x9
	cmp	x19, x20
	csel	x8, x19, x20, lo
	cmp	x8, #17
	b.lo	LBB0_5
; %bb.2:                                ; %L133
	ldr	x9, [x0]
	ldr	x10, [x1]
	cmp	x9, x10
	b.ne	LBB0_10
; %bb.3:                                ; %L135
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
	cmp	x19, x20
	mov	w8, #3
	csinc	x9, x8, xzr, hi
	csinv	x9, x9, xzr, hs
	cmp	w0, #0
	csinv	x8, x8, xzr, ge
	csel	x0, x9, x8, eq
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_4:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_5:                                 ; %L136
	cbz	x8, LBB0_9
; %bb.6:                                ; %L138
	mov	w9, #8
	subs	x9, x9, x8
	csel	x9, xzr, x9, lo
	lsl	x9, x9, #3
	mov	x10, #-1
	lsl	x9, x10, x9
	ldr	x10, [x0]
	rev	x10, x10
	ldr	x11, [x1]
	rev	x11, x11
	and	x10, x10, x9
	and	x9, x11, x9
	cmp	x10, x9
	b.ne	LBB0_11
; %bb.7:                                ; %L140
	cmp	x8, #9
	b.lo	LBB0_9
; %bb.8:                                ; %L141
	lsl	x8, x8, #3
	neg	x8, x8
	mov	x9, #-1
	lsl	x8, x9, x8
	ldr	x9, [x0, #8]
	rev	x9, x9
	ldr	x10, [x1, #8]
	rev	x10, x10
	and	x9, x9, x8
	and	x8, x10, x8
	cmp	x9, x8
	b.ne	LBB0_11
LBB0_9:                                 ; %L137
	cmp	x19, x20
	mov	w8, #3
	csinc	x8, x8, xzr, hi
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_10:                                ; %L134
	rev	x8, x9
	rev	x9, x10
	cmp	x8, x9
LBB0_11:                                ; %L139
	mov	w8, #3
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc|}]
