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
  %20 = load ptr addrspace(1), ptr %5
  %21 = load ptr addrspace(1), ptr %6
  %22 = load i64, ptr %ds
  %23 = load i64, ptr %alloc
  %24 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_c_call_wrapper.caml_string_equal.2.ptr_addrspace_1_.ptr_addrspace_1_.ptr_addrspace_1_.1"(i64 %22, i64 %23, ptr addrspace(1) %20, ptr addrspace(1) %21) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 1, i64 0, i64 45, i64 61, i64 0, i64 61, i64 0, i64 17, i64 5263188, i64 7548465, i64 6910580, i64 6252398, i64 7696741, i64 27745) ]
  %25 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 0
  %26 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 1
  store i64 %25, ptr %ds
  store i64 %26, ptr %alloc
  %27 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 1, 0
  store ptr addrspace(1) %27, ptr %5
  br label %L104
L105:
  %28 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %28, ptr %5
  br label %L104
L104:
  br label %L103
L103:
  %29 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %29, ptr %9
  %30 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %30, ptr %5
  %31 = load i64, ptr %5
  %32 = load i64, ptr %ds
  %33 = load i64, ptr %alloc
  %34 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %32, 0, 0
  %35 = insertvalue { { i64, i64 }, { i64 } } %34, i64 %33, 0, 1
  %36 = insertvalue { { i64, i64 }, { i64 } } %35, i64 %31, 1, 0
  ret { { i64, i64 }, { i64 } } %36
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__string_equal_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	cmp	x0, x1
	b.eq	LBB0_2
; %bb.1:                                ; %L106
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	bl	_c_call_wrapper.caml_string_equal.2.ptr_addrspace_1_.ptr_addrspace_1_.ptr_addrspace_1_.1
Ltmp0:
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_2:
	.cfi_def_cfa wsp, 0
	.cfi_same_value w30
	mov	w0, #3
	ret
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
  br label %L117
L117:
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
  br i1 %19, label %L121, label %L122
L122:
  %20 = load ptr addrspace(1), ptr %5
  %21 = load ptr addrspace(1), ptr %6
  %22 = load i64, ptr %ds
  %23 = load i64, ptr %alloc
  %24 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_c_call_wrapper.caml_bytes_equal.2.ptr_addrspace_1_.ptr_addrspace_1_.ptr_addrspace_1_.1"(i64 %22, i64 %23, ptr addrspace(1) %20, ptr addrspace(1) %21) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 412, i64 0, i64 0, i64 76, i64 0, i64 76, i64 8, i64 7633250, i64 3044197, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4349791, i64 6648953, i64 2633331, i64 7496048, i64 6383988, i64 10604, i64 1, i64 0, i64 42, i64 57, i64 0, i64 57, i64 0, i64 16, i64 5263188, i64 6434354, i64 6648953, i64 6643571, i64 6387057, i64 108) ]
  %25 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 0
  %26 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 1
  store i64 %25, ptr %ds
  store i64 %26, ptr %alloc
  %27 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 1, 0
  store ptr addrspace(1) %27, ptr %5
  br label %L120
L121:
  %28 = inttoptr i64 3 to ptr addrspace(1)
  store ptr addrspace(1) %28, ptr %5
  br label %L120
L120:
  br label %L119
L119:
  %29 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %29, ptr %9
  %30 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %30, ptr %5
  %31 = load i64, ptr %5
  %32 = load i64, ptr %ds
  %33 = load i64, ptr %alloc
  %34 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %32, 0, 0
  %35 = insertvalue { { i64, i64 }, { i64 } } %34, i64 %33, 0, 1
  %36 = insertvalue { { i64, i64 }, { i64 } } %35, i64 %31, 1, 0
  ret { { i64, i64 }, { i64 } } %36
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__bytes_equal_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	cmp	x0, x1
	b.eq	LBB0_2
; %bb.1:                                ; %L122
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	bl	_c_call_wrapper.caml_bytes_equal.2.ptr_addrspace_1_.ptr_addrspace_1_.ptr_addrspace_1_.1
Ltmp0:
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_2:
	.cfi_def_cfa wsp, 0
	.cfi_same_value w30
	mov	w0, #3
	ret
	.cfi_endproc|}]
