(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let add1 x = x + 1;;

[%%expect{|
val add1 : int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__add1_0_1_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64
  store i64 %2, ptr %4
  %5 = alloca i64
  %6 = alloca i64
  br label %L1
L1:
  br label %L101
L101:
  %7 = load i64, ptr %4
  store i64 %7, ptr %5
  %8 = load i64, ptr %5
  %9 = add i64 %8, 2
  store i64 %9, ptr %6
  %10 = load i64, ptr %6
  store i64 %10, ptr %4
  %11 = load i64, ptr %4
  %12 = load i64, ptr %ds
  %13 = load i64, ptr %alloc
  %14 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %12, 0, 0
  %15 = insertvalue { { i64, i64 }, { i64 } } %14, i64 %13, 0, 1
  %16 = insertvalue { { i64, i64 }, { i64 } } %15, i64 %11, 1, 0
  ret { { i64, i64 }, { i64 } } %16
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__add1_0_1_code:
; %bb.0:                                ; %L1
	add	x0, x0, #2
	ret|}]
