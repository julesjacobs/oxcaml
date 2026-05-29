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
  %21 = addrspacecast ptr addrspace(1) %15 to ptr
  %22 = ptrtoint ptr %21 to i64
  %23 = add i64 %22, -8
  %24 = inttoptr i64 %23 to ptr
  %25 = load atomic i64, ptr %24 monotonic, align 8
  %26 = and i64 %25, 72057594037926912
  %27 = lshr i64 %26, 10
  %28 = shl i64 %27, 3
  %29 = sub i64 %28, 1
  %30 = getelementptr i8, ptr %21, i64 %29
  %31 = load i8, ptr %30, align 1
  %32 = zext i8 %31 to i64
  %33 = sub i64 %29, %32
  %34 = addrspacecast ptr addrspace(1) %16 to ptr
  %35 = ptrtoint ptr %34 to i64
  %36 = add i64 %35, -8
  %37 = inttoptr i64 %36 to ptr
  %38 = load atomic i64, ptr %37 monotonic, align 8
  %39 = and i64 %38, 72057594037926912
  %40 = lshr i64 %39, 10
  %41 = shl i64 %40, 3
  %42 = sub i64 %41, 1
  %43 = getelementptr i8, ptr %34, i64 %42
  %44 = load i8, ptr %43, align 1
  %45 = zext i8 %44 to i64
  %46 = sub i64 %42, %45
  %47 = icmp ult i64 %33, %46
  %48 = select i1 %47, i64 %33, i64 %46
  %49 = icmp ugt i64 %48, 15
  br i1 %49, label %L107, label %L108
L107:
  %50 = load ptr addrspace(1), ptr %5
  %51 = load ptr addrspace(1), ptr %6
  %52 = load i64, ptr %ds
  %53 = load i64, ptr %alloc
  %54 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %52, i64 %53, ptr addrspace(1) %50, ptr addrspace(1) %51) "gc-leaf-function"="true"
  %55 = extractvalue { i64, i64, ptr addrspace(1) } %54, 0
  %56 = extractvalue { i64, i64, ptr addrspace(1) } %54, 1
  store i64 %55, ptr %ds
  store i64 %56, ptr %alloc
  %57 = extractvalue { i64, i64, ptr addrspace(1) } %54, 2
  store ptr addrspace(1) %57, ptr %5
  br label %L104
L108:
  %58 = icmp eq i64 %48, 0
  br i1 %58, label %L109, label %L110
L110:
  %59 = icmp ugt i64 %48, 8
  %60 = select i1 %59, i64 8, i64 %48
  %61 = sub i64 8, %60
  %62 = shl i64 %61, 3
  %63 = shl i64 -1, %62
  %64 = addrspacecast ptr addrspace(1) %15 to ptr
  %65 = load i64, ptr %64, align 8
  %66 = call  i64 @llvm.bswap.i64(i64 %65)
  %67 = addrspacecast ptr addrspace(1) %16 to ptr
  %68 = load i64, ptr %67, align 8
  %69 = call  i64 @llvm.bswap.i64(i64 %68)
  %70 = and i64 %66, %63
  %71 = and i64 %69, %63
  %72 = icmp ne i64 %70, %71
  %73 = icmp ult i64 %70, %71
  br i1 %72, label %L111, label %L112
L111:
  %74 = select i1 %73, i64 -1, i64 3
  %75 = inttoptr i64 %74 to ptr addrspace(1)
  store ptr addrspace(1) %75, ptr %5
  br label %L104
L112:
  br i1 %59, label %L113, label %L109
L113:
  %76 = sub i64 %48, 8
  %77 = sub i64 8, %76
  %78 = shl i64 %77, 3
  %79 = shl i64 -1, %78
  %80 = addrspacecast ptr addrspace(1) %15 to ptr
  %81 = ptrtoint ptr %80 to i64
  %82 = add i64 %81, 8
  %83 = inttoptr i64 %82 to ptr
  %84 = load i64, ptr %83, align 8
  %85 = call  i64 @llvm.bswap.i64(i64 %84)
  %86 = addrspacecast ptr addrspace(1) %16 to ptr
  %87 = ptrtoint ptr %86 to i64
  %88 = add i64 %87, 8
  %89 = inttoptr i64 %88 to ptr
  %90 = load i64, ptr %89, align 8
  %91 = call  i64 @llvm.bswap.i64(i64 %90)
  %92 = and i64 %85, %79
  %93 = and i64 %91, %79
  %94 = icmp ne i64 %92, %93
  %95 = icmp ult i64 %92, %93
  br i1 %94, label %L114, label %L109
L114:
  %96 = select i1 %95, i64 -1, i64 3
  %97 = inttoptr i64 %96 to ptr addrspace(1)
  store ptr addrspace(1) %97, ptr %5
  br label %L104
L109:
  %98 = icmp ult i64 %33, %46
  %99 = icmp ugt i64 %33, %46
  %100 = select i1 %99, i64 3, i64 1
  %101 = select i1 %98, i64 -1, i64 %100
  %102 = inttoptr i64 %101 to ptr addrspace(1)
  store ptr addrspace(1) %102, ptr %5
  br label %L104
L104:
  br label %L103
L103:
  %103 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %103, ptr %9
  %104 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %104, ptr %5
  %105 = load i64, ptr %5
  %106 = load i64, ptr %ds
  %107 = load i64, ptr %alloc
  %108 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %106, 0, 0
  %109 = insertvalue { { i64, i64 }, { i64 } } %108, i64 %107, 0, 1
  %110 = insertvalue { { i64, i64 }, { i64 } } %109, i64 %105, 1, 0
  ret { { i64, i64 }, { i64 } } %110
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__string_compare_0_1_code:
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
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	ldur	x9, [x1, #-8]
	lsr	x9, x9, #7
	and	x9, x9, #0x1fffffffffff8
	sub	x9, x9, #1
	ldrb	w10, [x1, x9]
	sub	x9, x9, x10
	cmp	x8, x9
	csel	x10, x8, x9, lo
	cmp	x10, #16
	b.lo	LBB0_4
; %bb.2:                                ; %L107
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_string_compare
	mov	sp, x29
	.cfi_restore_state
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_3:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_4:                                 ; %L108
	cbz	x10, LBB0_8
; %bb.5:                                ; %L110
	mov	w11, #8
	subs	x11, x11, x10
	csel	x11, xzr, x11, lo
	lsl	x11, x11, #3
	mov	x12, #-1
	lsl	x11, x12, x11
	ldr	x12, [x0]
	rev	x12, x12
	ldr	x13, [x1]
	rev	x13, x13
	and	x12, x12, x11
	and	x11, x13, x11
	cmp	x12, x11
	b.ne	LBB0_9
; %bb.6:                                ; %L112
	cmp	x10, #9
	b.lo	LBB0_8
; %bb.7:                                ; %L113
	lsl	x10, x10, #3
	neg	x10, x10
	mov	x11, #-1
	lsl	x10, x11, x10
	ldr	x11, [x0, #8]
	rev	x11, x11
	ldr	x12, [x1, #8]
	rev	x12, x12
	and	x11, x11, x10
	and	x10, x12, x10
	cmp	x11, x10
	b.ne	LBB0_9
LBB0_8:                                 ; %L109
	cmp	x8, x9
	mov	w8, #3
	csinc	x8, x8, xzr, hi
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_9:                                 ; %L111
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
  br label %L125
L125:
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
  br i1 %19, label %L129, label %L130
L129:
  %20 = inttoptr i64 1 to ptr addrspace(1)
  store ptr addrspace(1) %20, ptr %5
  br label %L128
L130:
  %21 = addrspacecast ptr addrspace(1) %15 to ptr
  %22 = ptrtoint ptr %21 to i64
  %23 = add i64 %22, -8
  %24 = inttoptr i64 %23 to ptr
  %25 = load atomic i64, ptr %24 monotonic, align 8
  %26 = and i64 %25, 72057594037926912
  %27 = lshr i64 %26, 10
  %28 = shl i64 %27, 3
  %29 = sub i64 %28, 1
  %30 = getelementptr i8, ptr %21, i64 %29
  %31 = load i8, ptr %30, align 1
  %32 = zext i8 %31 to i64
  %33 = sub i64 %29, %32
  %34 = addrspacecast ptr addrspace(1) %16 to ptr
  %35 = ptrtoint ptr %34 to i64
  %36 = add i64 %35, -8
  %37 = inttoptr i64 %36 to ptr
  %38 = load atomic i64, ptr %37 monotonic, align 8
  %39 = and i64 %38, 72057594037926912
  %40 = lshr i64 %39, 10
  %41 = shl i64 %40, 3
  %42 = sub i64 %41, 1
  %43 = getelementptr i8, ptr %34, i64 %42
  %44 = load i8, ptr %43, align 1
  %45 = zext i8 %44 to i64
  %46 = sub i64 %42, %45
  %47 = icmp ult i64 %33, %46
  %48 = select i1 %47, i64 %33, i64 %46
  %49 = icmp ugt i64 %48, 15
  br i1 %49, label %L131, label %L132
L131:
  %50 = load ptr addrspace(1), ptr %5
  %51 = load ptr addrspace(1), ptr %6
  %52 = load i64, ptr %ds
  %53 = load i64, ptr %alloc
  %54 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_bytes_compare"(i64 %52, i64 %53, ptr addrspace(1) %50, ptr addrspace(1) %51) "gc-leaf-function"="true"
  %55 = extractvalue { i64, i64, ptr addrspace(1) } %54, 0
  %56 = extractvalue { i64, i64, ptr addrspace(1) } %54, 1
  store i64 %55, ptr %ds
  store i64 %56, ptr %alloc
  %57 = extractvalue { i64, i64, ptr addrspace(1) } %54, 2
  store ptr addrspace(1) %57, ptr %5
  br label %L128
L132:
  %58 = icmp eq i64 %48, 0
  br i1 %58, label %L133, label %L134
L134:
  %59 = icmp ugt i64 %48, 8
  %60 = select i1 %59, i64 8, i64 %48
  %61 = sub i64 8, %60
  %62 = shl i64 %61, 3
  %63 = shl i64 -1, %62
  %64 = addrspacecast ptr addrspace(1) %15 to ptr
  %65 = load i64, ptr %64, align 8
  %66 = call  i64 @llvm.bswap.i64(i64 %65)
  %67 = addrspacecast ptr addrspace(1) %16 to ptr
  %68 = load i64, ptr %67, align 8
  %69 = call  i64 @llvm.bswap.i64(i64 %68)
  %70 = and i64 %66, %63
  %71 = and i64 %69, %63
  %72 = icmp ne i64 %70, %71
  %73 = icmp ult i64 %70, %71
  br i1 %72, label %L135, label %L136
L135:
  %74 = select i1 %73, i64 -1, i64 3
  %75 = inttoptr i64 %74 to ptr addrspace(1)
  store ptr addrspace(1) %75, ptr %5
  br label %L128
L136:
  br i1 %59, label %L137, label %L133
L137:
  %76 = sub i64 %48, 8
  %77 = sub i64 8, %76
  %78 = shl i64 %77, 3
  %79 = shl i64 -1, %78
  %80 = addrspacecast ptr addrspace(1) %15 to ptr
  %81 = ptrtoint ptr %80 to i64
  %82 = add i64 %81, 8
  %83 = inttoptr i64 %82 to ptr
  %84 = load i64, ptr %83, align 8
  %85 = call  i64 @llvm.bswap.i64(i64 %84)
  %86 = addrspacecast ptr addrspace(1) %16 to ptr
  %87 = ptrtoint ptr %86 to i64
  %88 = add i64 %87, 8
  %89 = inttoptr i64 %88 to ptr
  %90 = load i64, ptr %89, align 8
  %91 = call  i64 @llvm.bswap.i64(i64 %90)
  %92 = and i64 %85, %79
  %93 = and i64 %91, %79
  %94 = icmp ne i64 %92, %93
  %95 = icmp ult i64 %92, %93
  br i1 %94, label %L138, label %L133
L138:
  %96 = select i1 %95, i64 -1, i64 3
  %97 = inttoptr i64 %96 to ptr addrspace(1)
  store ptr addrspace(1) %97, ptr %5
  br label %L128
L133:
  %98 = icmp ult i64 %33, %46
  %99 = icmp ugt i64 %33, %46
  %100 = select i1 %99, i64 3, i64 1
  %101 = select i1 %98, i64 -1, i64 %100
  %102 = inttoptr i64 %101 to ptr addrspace(1)
  store ptr addrspace(1) %102, ptr %5
  br label %L128
L128:
  br label %L127
L127:
  %103 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %103, ptr %9
  %104 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %104, ptr %5
  %105 = load i64, ptr %5
  %106 = load i64, ptr %ds
  %107 = load i64, ptr %alloc
  %108 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %106, 0, 0
  %109 = insertvalue { { i64, i64 }, { i64 } } %108, i64 %107, 0, 1
  %110 = insertvalue { { i64, i64 }, { i64 } } %109, i64 %105, 1, 0
  ret { { i64, i64 }, { i64 } } %110
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__bytes_compare_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	cmp	x0, x1
	b.eq	LBB0_3
; %bb.1:                                ; %L130
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	ldur	x9, [x1, #-8]
	lsr	x9, x9, #7
	and	x9, x9, #0x1fffffffffff8
	sub	x9, x9, #1
	ldrb	w10, [x1, x9]
	sub	x9, x9, x10
	cmp	x8, x9
	csel	x10, x8, x9, lo
	cmp	x10, #16
	b.lo	LBB0_4
; %bb.2:                                ; %L131
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_bytes_compare
	mov	sp, x29
	.cfi_restore_state
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_3:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_4:                                 ; %L132
	cbz	x10, LBB0_8
; %bb.5:                                ; %L134
	mov	w11, #8
	subs	x11, x11, x10
	csel	x11, xzr, x11, lo
	lsl	x11, x11, #3
	mov	x12, #-1
	lsl	x11, x12, x11
	ldr	x12, [x0]
	rev	x12, x12
	ldr	x13, [x1]
	rev	x13, x13
	and	x12, x12, x11
	and	x11, x13, x11
	cmp	x12, x11
	b.ne	LBB0_9
; %bb.6:                                ; %L136
	cmp	x10, #9
	b.lo	LBB0_8
; %bb.7:                                ; %L137
	lsl	x10, x10, #3
	neg	x10, x10
	mov	x11, #-1
	lsl	x10, x11, x10
	ldr	x11, [x0, #8]
	rev	x11, x11
	ldr	x12, [x1, #8]
	rev	x12, x12
	and	x11, x11, x10
	and	x10, x12, x10
	cmp	x11, x10
	b.ne	LBB0_9
LBB0_8:                                 ; %L133
	cmp	x8, x9
	mov	w8, #3
	csinc	x8, x8, xzr, hi
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_9:                                 ; %L135
	mov	w8, #3
	csinv	x0, x8, xzr, hs
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc|}]
