(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let make_pair x y = x, y;;

[%%expect{|
val make_pair : 'a -> 'b -> 'a * 'b = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__make_pair_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  %10 = alloca i64
  br label %L1
L1:
  br label %L101
L101:
  %11 = load ptr addrspace(1), ptr %5
  store volatile ptr addrspace(1) %11, ptr %7
  %12 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %12, ptr %8
  %13 = load i64, ptr %alloc
  %14 = load i64, ptr %ds
  %15 = add i64 %14, 8
  %16 = inttoptr i64 %15 to ptr
  store i64 %13, ptr %16
  %17 = load i64, ptr %ds
  %18 = load i64, ptr %alloc
  %19 = call oxcaml_nofpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap.0..0"(i64 %17, i64 %18) "gc-leaf-function"="true"
  %20 = extractvalue { { i64, i64 }, {  } } %19, 0, 0
  %21 = extractvalue { { i64, i64 }, {  } } %19, 0, 1
  store i64 %20, ptr %ds
  store i64 %21, ptr %alloc
  %22 = load i64, ptr %alloc
  %23 = load i64, ptr %ds
  %24 = load i64, ptr %alloc
  %25 = call oxcaml_nofpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0"(i64 %23, i64 %24, i64 %22, i64 345357463, i64 5) "gc-leaf-function"="true"
  %26 = extractvalue { { i64, i64 }, {  } } %25, 0, 0
  %27 = extractvalue { { i64, i64 }, {  } } %25, 0, 1
  store i64 %26, ptr %ds
  store i64 %27, ptr %alloc
  %28 = load i64, ptr %alloc
  %29 = sub i64 %28, 24
  store i64 %29, ptr %alloc
  %30 = load i64, ptr %ds
  %31 = inttoptr i64 %30 to ptr
  %32 = load i64, ptr %31
  %33 = icmp ule i64 %32, %29
  %34 = call  i1 @llvm.expect.i1(i1 %33, i1 1)
  br i1 %34, label %L105, label %L104
L104:
  %35 = load i64, ptr %ds
  %36 = load i64, ptr %alloc
  %37 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %35, i64 %36) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 1, i64 0, i64 20, i64 24, i64 0, i64 24, i64 0, i64 14, i64 5263188, i64 7155249, i64 6646625, i64 6385759, i64 29289), "gc-live"(ptr %7, ptr %8) ]
  %38 = extractvalue { { i64, i64 }, {  } } %37, 0, 0
  %39 = extractvalue { { i64, i64 }, {  } } %37, 0, 1
  store i64 %38, ptr %ds
  store i64 %39, ptr %alloc
  br label %L105
L105:
  %40 = load i64, ptr %alloc
  %41 = add i64 %40, 8
  %42 = inttoptr i64 %41 to ptr addrspace(1)
  store ptr addrspace(1) %42, ptr %9
  %43 = load ptr addrspace(1), ptr %9
  %44 = ptrtoint ptr addrspace(1) %43 to i64
  %45 = add i64 %44, -8
  %46 = inttoptr i64 %45 to ptr
  store volatile i64 2048, ptr %46
  %47 = load ptr addrspace(1), ptr %9
  %48 = addrspacecast ptr addrspace(1) %47 to ptr
  %49 = load volatile ptr addrspace(1), ptr %7
  store ptr addrspace(1) %49, ptr %48
  %50 = load ptr addrspace(1), ptr %9
  %51 = ptrtoint ptr addrspace(1) %50 to i64
  %52 = add i64 %51, 8
  %53 = inttoptr i64 %52 to ptr
  %54 = load volatile ptr addrspace(1), ptr %8
  store ptr addrspace(1) %54, ptr %53
  %55 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %55, ptr %5
  %56 = load ptr addrspace(1), ptr %5
  %57 = load i64, ptr %ds
  %58 = load i64, ptr %alloc
  %59 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %57, 0, 0
  %60 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %59, i64 %58, 0, 1
  %61 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %60, ptr addrspace(1) %56, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %61
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__make_pair_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -8
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	str	x0, [sp, #16]
	str	x1, [sp, #8]
	str	x27, [x28, #8]
	bl	_c_call_wrapper.caml_debug_check_minor_heap.0..0
	mov	x0, x27
	mov	x27, x0
	mov	w1, #48279
	movk	w1, #5269, lsl #16
	mov	w2, #5
	bl	_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0
	mov	x1, x28
	sub	x8, x27, #24
	ldr	x9, [x28]
	cmp	x9, x8
	b.hi	LBB0_2
LBB0_1:                                 ; %L105
	mov	w9, #2048
	str	x9, [x8]
	ldr	x9, [sp, #16]
	mov	x0, x8
	str	x9, [x0, #8]!
	ldr	x9, [sp, #8]
	str	x9, [x8, #16]
	mov	x28, x1
	mov	x27, x8
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L104
	mov	x28, x1
	mov	x27, x8
	bl	_caml_call_gc
Ltmp1:
	mov	x8, x27
	mov	x1, x28
	b	LBB0_1
	.cfi_endproc|}]
