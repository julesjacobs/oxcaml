(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let call f x = f x;;

[%%expect{|
val call : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__call_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %9 = alloca i64
  br label %L1
L1:
  br label %L101
L101:
  %10 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %10, ptr %7
  %11 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %11, ptr %8
  %12 = load ptr addrspace(1), ptr %7
  %13 = addrspacecast ptr addrspace(1) %12 to ptr
  %14 = load i64, ptr %13
  store i64 %14, ptr %9
  %15 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %15, ptr %5
  %16 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %16, ptr %6
  %17 = load ptr addrspace(1), ptr %5
  %18 = load ptr addrspace(1), ptr %6
  %19 = load i64, ptr %ds
  %20 = load i64, ptr %alloc
  %21 = load ptr, ptr %9
  %22 = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } %21(i64 %19, i64 %20, ptr addrspace(1) %17, ptr addrspace(1) %18) "statepoint-id"="0"
  ret { { i64, i64 }, { ptr addrspace(1) } } %22
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__call_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	mov	x8, x0
	ldr	x2, [x0]
	mov	x0, x1
	mov	x1, x8
	br	x2
	.cfi_endproc|}]
