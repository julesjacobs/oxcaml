(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let catch_exit b x = try if b then raise Exit else x + 1 with Exit -> 0;;

[%%expect{|
val catch_exit : bool -> int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__catch_exit_0_1_code"(i64 %0, i64 %1, i64 %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca i64
  store i64 %2, ptr %5
  %6 = alloca i64
  store i64 %3, ptr %6
  %7 = alloca i64
  %8 = alloca i64
  %9 = alloca i64
  %10 = alloca i64
  %11 = alloca i64
  %12 = alloca i64
  %13 = alloca i64
  %14 = alloca i64
  br label %L1
L1:
  br label %L101
L101:
  %15 = load i64, ptr %5
  store i64 %15, ptr %7
  %16 = load i64, ptr %6
  store i64 %16, ptr %8
  %17 = load i64, ptr %ds
  %18 = add i64 %17, 64
  %19 = inttoptr i64 %18 to ptr
  %20 = load i64, ptr %19
  store i64 %20, ptr %9
  %21 = load i64, ptr %9
  store i64 %21, ptr %10
  %22 = load i64, ptr %7
  %23 = icmp slt i64 %22, 1
  br i1 %23, label %L107, label %L111
L111:
  %24 = load i64, ptr %7
  %25 = icmp sgt i64 %24, 1
  br i1 %25, label %L107, label %L104
L104:
  %26 = load i64, ptr %8
  %27 = add i64 %26, 2
  store i64 %27, ptr %11
  %28 = load i64, ptr %11
  store i64 %28, ptr %5
  %29 = load i64, ptr %5
  %30 = load i64, ptr %ds
  %31 = load i64, ptr %alloc
  %32 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %30, 0, 0
  %33 = insertvalue { { i64, i64 }, { i64 } } %32, i64 %31, 0, 1
  %34 = insertvalue { { i64, i64 }, { i64 } } %33, i64 %29, 1, 0
  ret { { i64, i64 }, { i64 } } %34
L107:
  %35 = load i64, ptr %ds
  %36 = add i64 %35, 64
  %37 = inttoptr i64 %36 to ptr
  %38 = load i64, ptr %10
  store i64 %38, ptr %37
  store i64 1, ptr %13
  store i64 1, ptr %5
  %39 = load i64, ptr %5
  %40 = load i64, ptr %ds
  %41 = load i64, ptr %alloc
  %42 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %40, 0, 0
  %43 = insertvalue { { i64, i64 }, { i64 } } %42, i64 %41, 0, 1
  %44 = insertvalue { { i64, i64 }, { i64 } } %43, i64 %39, 1, 0
  ret { { i64, i64 }, { i64 } } %44
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__catch_exit_0_1_code:
; %bb.0:                                ; %L1
	add	x8, x1, #2
	cmp	x0, #1
	csinc	x0, x8, xzr, eq
	ret|}]
