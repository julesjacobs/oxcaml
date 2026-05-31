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
  %43 = icmp ugt i64 %42, 15
  br i1 %43, label %L107, label %L108
L107:
  %44 = load ptr addrspace(1), ptr %5
  %45 = load ptr addrspace(1), ptr %6
  %46 = load i64, ptr %ds
  %47 = load i64, ptr %alloc
  %48 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %46, i64 %47, ptr addrspace(1) %44, ptr addrspace(1) %45) "gc-leaf-function"="true"
  %49 = extractvalue { i64, i64, ptr addrspace(1) } %48, 0
  %50 = extractvalue { i64, i64, ptr addrspace(1) } %48, 1
  store i64 %49, ptr %ds
  store i64 %50, ptr %alloc
  %51 = extractvalue { i64, i64, ptr addrspace(1) } %48, 2
  store ptr addrspace(1) %51, ptr %5
  br label %L104
L108:
  %52 = icmp eq i64 %42, 0
  br i1 %52, label %L109, label %L110
L110:
  %53 = icmp ugt i64 %42, 8
  %54 = select i1 %53, i64 8, i64 %42
  %55 = sub i64 8, %54
  %56 = shl i64 %55, 3
  %57 = shl i64 -1, %56
  %58 = load i64, ptr addrspace(1) %15, align 8
  %59 = call  i64 @llvm.bswap.i64(i64 %58)
  %60 = load i64, ptr addrspace(1) %16, align 8
  %61 = call  i64 @llvm.bswap.i64(i64 %60)
  %62 = and i64 %59, %57
  %63 = and i64 %61, %57
  %64 = icmp ne i64 %62, %63
  %65 = icmp ult i64 %62, %63
  br i1 %64, label %L111, label %L112
L111:
  %66 = select i1 %65, i64 -1, i64 3
  %67 = inttoptr i64 %66 to ptr addrspace(1)
  store ptr addrspace(1) %67, ptr %5
  br label %L104
L112:
  br i1 %53, label %L113, label %L109
L113:
  %68 = sub i64 %42, 8
  %69 = sub i64 8, %68
  %70 = shl i64 %69, 3
  %71 = shl i64 -1, %70
  %72 = getelementptr i8, ptr addrspace(1) %15, i64 8
  %73 = load i64, ptr addrspace(1) %72, align 8
  %74 = call  i64 @llvm.bswap.i64(i64 %73)
  %75 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %76 = load i64, ptr addrspace(1) %75, align 8
  %77 = call  i64 @llvm.bswap.i64(i64 %76)
  %78 = and i64 %74, %71
  %79 = and i64 %77, %71
  %80 = icmp ne i64 %78, %79
  %81 = icmp ult i64 %78, %79
  br i1 %80, label %L114, label %L109
L114:
  %82 = select i1 %81, i64 -1, i64 3
  %83 = inttoptr i64 %82 to ptr addrspace(1)
  store ptr addrspace(1) %83, ptr %5
  br label %L104
L109:
  %84 = icmp ult i64 %30, %40
  %85 = icmp ugt i64 %30, %40
  %86 = select i1 %85, i64 3, i64 1
  %87 = select i1 %84, i64 -1, i64 %86
  %88 = inttoptr i64 %87 to ptr addrspace(1)
  store ptr addrspace(1) %88, ptr %5
  br label %L104
L104:
  br label %L103
L103:
  %89 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %89, ptr %9
  %90 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %90, ptr %5
  %91 = load ptr addrspace(1), ptr %5
  %92 = ptrtoint ptr addrspace(1) %91 to i64
  %93 = load i64, ptr %ds
  %94 = load i64, ptr %alloc
  %95 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %93, 0, 0
  %96 = insertvalue { { i64, i64 }, { i64 } } %95, i64 %94, 0, 1
  %97 = insertvalue { { i64, i64 }, { i64 } } %96, i64 %92, 1, 0
  ret { { i64, i64 }, { i64 } } %97
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
  %43 = icmp ugt i64 %42, 15
  br i1 %43, label %L131, label %L132
L131:
  %44 = load ptr addrspace(1), ptr %5
  %45 = load ptr addrspace(1), ptr %6
  %46 = load i64, ptr %ds
  %47 = load i64, ptr %alloc
  %48 = call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_bytes_compare"(i64 %46, i64 %47, ptr addrspace(1) %44, ptr addrspace(1) %45) "gc-leaf-function"="true"
  %49 = extractvalue { i64, i64, ptr addrspace(1) } %48, 0
  %50 = extractvalue { i64, i64, ptr addrspace(1) } %48, 1
  store i64 %49, ptr %ds
  store i64 %50, ptr %alloc
  %51 = extractvalue { i64, i64, ptr addrspace(1) } %48, 2
  store ptr addrspace(1) %51, ptr %5
  br label %L128
L132:
  %52 = icmp eq i64 %42, 0
  br i1 %52, label %L133, label %L134
L134:
  %53 = icmp ugt i64 %42, 8
  %54 = select i1 %53, i64 8, i64 %42
  %55 = sub i64 8, %54
  %56 = shl i64 %55, 3
  %57 = shl i64 -1, %56
  %58 = load i64, ptr addrspace(1) %15, align 8
  %59 = call  i64 @llvm.bswap.i64(i64 %58)
  %60 = load i64, ptr addrspace(1) %16, align 8
  %61 = call  i64 @llvm.bswap.i64(i64 %60)
  %62 = and i64 %59, %57
  %63 = and i64 %61, %57
  %64 = icmp ne i64 %62, %63
  %65 = icmp ult i64 %62, %63
  br i1 %64, label %L135, label %L136
L135:
  %66 = select i1 %65, i64 -1, i64 3
  %67 = inttoptr i64 %66 to ptr addrspace(1)
  store ptr addrspace(1) %67, ptr %5
  br label %L128
L136:
  br i1 %53, label %L137, label %L133
L137:
  %68 = sub i64 %42, 8
  %69 = sub i64 8, %68
  %70 = shl i64 %69, 3
  %71 = shl i64 -1, %70
  %72 = getelementptr i8, ptr addrspace(1) %15, i64 8
  %73 = load i64, ptr addrspace(1) %72, align 8
  %74 = call  i64 @llvm.bswap.i64(i64 %73)
  %75 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %76 = load i64, ptr addrspace(1) %75, align 8
  %77 = call  i64 @llvm.bswap.i64(i64 %76)
  %78 = and i64 %74, %71
  %79 = and i64 %77, %71
  %80 = icmp ne i64 %78, %79
  %81 = icmp ult i64 %78, %79
  br i1 %80, label %L138, label %L133
L138:
  %82 = select i1 %81, i64 -1, i64 3
  %83 = inttoptr i64 %82 to ptr addrspace(1)
  store ptr addrspace(1) %83, ptr %5
  br label %L128
L133:
  %84 = icmp ult i64 %30, %40
  %85 = icmp ugt i64 %30, %40
  %86 = select i1 %85, i64 3, i64 1
  %87 = select i1 %84, i64 -1, i64 %86
  %88 = inttoptr i64 %87 to ptr addrspace(1)
  store ptr addrspace(1) %88, ptr %5
  br label %L128
L128:
  br label %L127
L127:
  %89 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %89, ptr %9
  %90 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %90, ptr %5
  %91 = load ptr addrspace(1), ptr %5
  %92 = ptrtoint ptr addrspace(1) %91 to i64
  %93 = load i64, ptr %ds
  %94 = load i64, ptr %alloc
  %95 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %93, 0, 0
  %96 = insertvalue { { i64, i64 }, { i64 } } %95, i64 %94, 0, 1
  %97 = insertvalue { { i64, i64 }, { i64 } } %96, i64 %92, 1, 0
  ret { { i64, i64 }, { i64 } } %97
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
