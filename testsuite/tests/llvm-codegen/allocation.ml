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
  br label %L1
L1:
  br label %L101
L101:
  %11 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %11, ptr %7
  %12 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %12, ptr %8
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
  %22 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %20, i64 %21) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 1, i64 0, i64 20, i64 24, i64 0, i64 24, i64 0, i64 14, i64 5263188, i64 7155249, i64 6646625, i64 6385759, i64 29289) ]
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
  %29 = getelementptr i8, ptr addrspace(1) %28, i64 -8
  store volatile i64 2048, ptr addrspace(1) %29
  %30 = load ptr addrspace(1), ptr %9
  %31 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %31, ptr addrspace(1) %30
  %32 = load ptr addrspace(1), ptr %9
  %33 = getelementptr i8, ptr addrspace(1) %32, i64 8
  %34 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %34, ptr addrspace(1) %33
  %35 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %35, ptr %5
  %36 = load ptr addrspace(1), ptr %5
  %37 = load i64, ptr %ds
  %38 = load i64, ptr %alloc
  %39 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %37, 0, 0
  %40 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %39, i64 %38, 0, 1
  %41 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %40, ptr addrspace(1) %36, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %41
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
	str	x1, [sp]
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x1, [sp]
	ldr	x0, [sp, #8]
	b	LBB0_1
	.cfi_endproc|}]
