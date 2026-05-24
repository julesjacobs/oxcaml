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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__make_pair_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  %14 = sub i64 %13, 24
  store i64 %14, ptr %alloc
  %15 = load i64, ptr %ds
  %16 = inttoptr i64 %15 to ptr
  %17 = load i64, ptr %16
  %18 = icmp ule i64 %17, %14
  %19 = call  i1 @llvm.expect.i1(i1 %18, i1 1)
  br i1 %19, label %L105, label %L104
L104:
  %20 = load i64, ptr %ds
  %21 = load i64, ptr %alloc
  %22 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %20, i64 %21) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 1, i64 0, i64 20, i64 24, i64 0, i64 24, i64 0, i64 14, i64 5263188, i64 7155249, i64 6646625, i64 6385759, i64 29289), "gc-live"(ptr %7, ptr %8) ]
  %23 = extractvalue { { i64, i64 }, {  } } %22, 0, 0
  %24 = extractvalue { { i64, i64 }, {  } } %22, 0, 1
  store i64 %23, ptr %ds
  store i64 %24, ptr %alloc
  br label %L105
L105:
  %25 = load i64, ptr %alloc
  %26 = add i64 %25, 8
  %27 = inttoptr i64 %26 to ptr addrspace(1)
  store ptr addrspace(1) %27, ptr %9
  %28 = load ptr addrspace(1), ptr %9
  %29 = ptrtoint ptr addrspace(1) %28 to i64
  %30 = add i64 %29, -8
  %31 = inttoptr i64 %30 to ptr
  store volatile i64 2048, ptr %31
  %32 = load ptr addrspace(1), ptr %9
  %33 = addrspacecast ptr addrspace(1) %32 to ptr
  %34 = load volatile ptr addrspace(1), ptr %7
  store ptr addrspace(1) %34, ptr %33
  %35 = load ptr addrspace(1), ptr %9
  %36 = ptrtoint ptr addrspace(1) %35 to i64
  %37 = add i64 %36, 8
  %38 = inttoptr i64 %37 to ptr
  %39 = load volatile ptr addrspace(1), ptr %8
  store ptr addrspace(1) %39, ptr %38
  %40 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %40, ptr %5
  %41 = load ptr addrspace(1), ptr %5
  %42 = load i64, ptr %ds
  %43 = load i64, ptr %alloc
  %44 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %42, 0, 0
  %45 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %44, i64 %43, 0, 1
  %46 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %45, ptr addrspace(1) %41, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %46
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
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	str	x0, [sp, #8]
	str	x1, [sp]
	sub	x8, x27, #24
	ldr	x9, [x28]
	cmp	x9, x8
	b.hi	LBB0_2
LBB0_1:                                 ; %L105
	mov	x1, x28
	mov	w9, #2048
	str	x9, [x8]
	ldr	x9, [sp, #8]
	mov	x0, x8
	str	x9, [x0, #8]!
	ldr	x9, [sp]
	str	x9, [x8, #16]
	mov	x28, x1
	mov	x27, x8
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L104
	mov	x27, x8
	bl	_caml_call_gc
Ltmp1:
	mov	x8, x27
	b	LBB0_1
	.cfi_endproc|}]
