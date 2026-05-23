(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

type r = { mutable i : int; mutable s : string };;

[%%expect{|
type r = { mutable i : int; mutable s : string; }
|}]

let set_i r v = r.i <- v;;

[%%expect{|
val set_i : r -> int -> unit = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__set_i_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca i64
  store i64 %3, ptr %6
  %7 = alloca i64
  %8 = alloca ptr addrspace(1)
  %9 = alloca i64
  %10 = alloca i64
  %11 = alloca i64
  %12 = alloca i64
  br label %L1
L1:
  br label %L110
L110:
  %13 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %13, ptr %8
  %14 = load i64, ptr %6
  store i64 %14, ptr %9
  %15 = load ptr addrspace(1), ptr %8
  %16 = addrspacecast ptr addrspace(1) %15 to ptr
  fence acquire
  %17 = load i64, ptr %9
  store i64 %17, ptr %16
  store i64 1, ptr %11
  store i64 1, ptr %7
  %18 = load i64, ptr %7
  %19 = load i64, ptr %ds
  %20 = load i64, ptr %alloc
  %21 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %19, 0, 0
  %22 = insertvalue { { i64, i64 }, { i64 } } %21, i64 %20, 0, 1
  %23 = insertvalue { { i64, i64 }, { i64 } } %22, i64 %18, 1, 0
  ret { { i64, i64 }, { i64 } } %23
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__set_i_0_1_code:
; %bb.0:                                ; %L1
	dmb	ishld
	str	x1, [x0]
	mov	w0, #1
	ret|}]

let set_s r v = r.s <- v;;

[%%expect{|
val set_s : r -> string -> unit = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__set_s_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %6
  %7 = alloca i64
  %8 = alloca ptr addrspace(1)
  %9 = alloca ptr addrspace(1)
  %10 = alloca i64
  %11 = alloca i64
  %12 = alloca i64
  %13 = alloca i64
  br label %L1
L1:
  br label %L123
L123:
  %14 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %14, ptr %8
  %15 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %15, ptr %9
  %16 = load ptr addrspace(1), ptr %8
  %17 = getelementptr i8, ptr addrspace(1) %16, i64 8
  %18 = ptrtoint ptr addrspace(1) %17 to i64
  store i64 %18, ptr %10
  %19 = load i64, ptr %10
  store i64 %19, ptr %7
  %20 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %20, ptr %6
  %21 = ptrtoint ptr @"\01_caml_modify" to i64
  %22 = load i64, ptr %7
  %23 = load ptr addrspace(1), ptr %6
  %24 = load i64, ptr %ds
  %25 = load i64, ptr %alloc
  %26 = call oxcaml_nofpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_modify.2.i64.ptr_addrspace_1_.0"(i64 %24, i64 %25, i64 %22, ptr addrspace(1) %23) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 1, i64 0, i64 16, i64 24, i64 0, i64 24, i64 0, i64 10, i64 5263188, i64 7548467, i64 6255717, i64 115) ]
  %27 = extractvalue { { i64, i64 }, {  } } %26, 0, 0
  %28 = extractvalue { { i64, i64 }, {  } } %26, 0, 1
  store i64 %27, ptr %ds
  store i64 %28, ptr %alloc
  br label %L125
L125:
  store i64 1, ptr %12
  store i64 1, ptr %7
  %29 = load i64, ptr %7
  %30 = load i64, ptr %ds
  %31 = load i64, ptr %alloc
  %32 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %30, 0, 0
  %33 = insertvalue { { i64, i64 }, { i64 } } %32, i64 %31, 0, 1
  %34 = insertvalue { { i64, i64 }, { i64 } } %33, i64 %29, 1, 0
  ret { { i64, i64 }, { i64 } } %34
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__set_s_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #34
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #34
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	add	x0, x0, #8
	bl	_c_call_wrapper.caml_modify.2.i64.ptr_addrspace_1_.0
Ltmp1:
	mov	w0, #1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc|}]
