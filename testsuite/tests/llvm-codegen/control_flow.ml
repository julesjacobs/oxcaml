(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let choose b x y = if b then x else y;;

[%%expect{|
val choose : bool -> 'a -> 'a -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__choose_0_1_code"(i64 %0, i64 %1, i64 %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca i64
  store i64 %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca ptr addrspace(1)
  %10 = alloca i64
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L101
L101:
  %13 = load i64, ptr %6
  store i64 %13, ptr %10
  %14 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %14, ptr %11
  %15 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %15, ptr %12
  %16 = load i64, ptr %10
  %17 = icmp slt i64 %16, 1
  br i1 %17, label %L105, label %L108
L108:
  %18 = load i64, ptr %10
  %19 = icmp sgt i64 %18, 1
  br i1 %19, label %L105, label %L103
L103:
  %20 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %20, ptr %9
  %21 = load ptr addrspace(1), ptr %9
  %22 = load i64, ptr %ds
  %23 = load i64, ptr %alloc
  %24 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %22, 0, 0
  %25 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %24, i64 %23, 0, 1
  %26 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %25, ptr addrspace(1) %21, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %26
L105:
  %27 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %27, ptr %9
  %28 = load ptr addrspace(1), ptr %9
  %29 = load i64, ptr %ds
  %30 = load i64, ptr %alloc
  %31 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %29, 0, 0
  %32 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %31, i64 %30, 0, 1
  %33 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %32, ptr addrspace(1) %28, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %33
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__choose_0_1_code:
; %bb.0:                                ; %L1
	cmp	x0, #1
	csel	x0, x2, x1, eq
	ret|}]
