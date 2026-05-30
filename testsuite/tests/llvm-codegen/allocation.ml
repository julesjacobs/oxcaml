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

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__make_pair_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L101
L101:
  %13 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %13, ptr %7
  %14 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %14, ptr %8
  %15 = load i64, ptr %alloc
  %16 = sub i64 %15, 24
  store i64 %16, ptr %alloc
  %17 = load i64, ptr %ds
  %18 = inttoptr i64 %17 to ptr
  %19 = load i64, ptr %18
  %20 = icmp ule i64 %19, %16
  %21 = call  i1 @llvm.expect.i1(i1 %20, i1 1)
  br i1 %21, label %L105, label %L104
L104:
  %22 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %22, ptr %11
  %23 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %23, ptr %12
  %24 = load i64, ptr %ds
  %25 = load i64, ptr %alloc
  %26 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %24, i64 %25) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 1, i64 0, i64 20, i64 24, i64 0, i64 24, i64 0, i64 14, i64 5263188, i64 7155249, i64 6646625, i64 6385759, i64 29289), "gc-live"(ptr %11, ptr %12) ]
  %27 = extractvalue { { i64, i64 }, {  } } %26, 0, 0
  %28 = extractvalue { { i64, i64 }, {  } } %26, 0, 1
  store i64 %27, ptr %ds
  store i64 %28, ptr %alloc
  %29 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %29, ptr %7
  %30 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %30, ptr %8
  br label %L105
L105:
  %31 = load i64, ptr %alloc
  %32 = add i64 %31, 8
  %33 = inttoptr i64 %32 to ptr addrspace(1)
  store ptr addrspace(1) %33, ptr %9
  %34 = load ptr addrspace(1), ptr %9
  %35 = getelementptr i8, ptr addrspace(1) %34, i64 -8
  store volatile i64 2048, ptr addrspace(1) %35
  %36 = load ptr addrspace(1), ptr %9
  %37 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %37, ptr addrspace(1) %36
  %38 = load ptr addrspace(1), ptr %9
  %39 = getelementptr i8, ptr addrspace(1) %38, i64 8
  %40 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %40, ptr addrspace(1) %39
  %41 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %41, ptr %5
  %42 = load ptr addrspace(1), ptr %5
  %43 = load i64, ptr %ds
  %44 = load i64, ptr %alloc
  %45 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %43, 0, 0
  %46 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %45, i64 %44, 0, 1
  %47 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %46, ptr addrspace(1) %42, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %47
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__make_pair_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_2
LBB0_1:                                 ; %L105
	mov	w8, #2048
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	str	x1, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L104
	str	x0, [sp, #8]
	str	x1, [sp]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	ldr	x1, [sp]
	b	LBB0_1
	.cfi_endproc|}]
