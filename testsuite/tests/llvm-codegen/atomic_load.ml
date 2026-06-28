(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let get_int (a : int Atomic.t) = Atomic.get a;;

[%%expect{|
val get_int : int Atomic.t -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__get_int_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca i64
  %6 = alloca ptr addrspace(1)
  %7 = alloca i64
  br label %L1
L1:
  br label %L101
L101:
  %8 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %8, ptr %6
  %9 = load ptr addrspace(1), ptr %6
  fence acquire
  %10 = load atomic i64, ptr addrspace(1) %9 seq_cst, align 8
  store i64 %10, ptr %7
  %11 = load i64, ptr %7
  store i64 %11, ptr %5
  %12 = load i64, ptr %5
  %13 = load i64, ptr %ds
  %14 = load i64, ptr %alloc
  %15 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %13, 0, 0
  %16 = insertvalue { { i64, i64 }, { i64 } } %15, i64 %14, 0, 1
  %17 = insertvalue { { i64, i64 }, { i64 } } %16, i64 %12, 1, 0
  ret { { i64, i64 }, { i64 } } %17
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__get_int_0_1_code:
; %bb.0:                                ; %L1
	dmb	ishld
	ldar	x0, [x0]
	ret|}]

let get_string (a : string Atomic.t) = Atomic.get a;;

[%%expect{|
val get_string : string Atomic.t -> string = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__get_string_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca ptr addrspace(1)
  %6 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L114
L114:
  %7 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %7, ptr %5
  %8 = load ptr addrspace(1), ptr %5
  fence acquire
  %9 = load atomic ptr addrspace(1), ptr addrspace(1) %8 seq_cst, align 8
  store ptr addrspace(1) %9, ptr %6
  %10 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %10, ptr %4
  %11 = load ptr addrspace(1), ptr %4
  %12 = load i64, ptr %ds
  %13 = load i64, ptr %alloc
  %14 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %12, 0, 0
  %15 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %14, i64 %13, 0, 1
  %16 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %15, ptr addrspace(1) %11, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %16
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__get_string_2_3_code:
; %bb.0:                                ; %L1
	dmb	ishld
	ldar	x0, [x0]
	ret|}]
