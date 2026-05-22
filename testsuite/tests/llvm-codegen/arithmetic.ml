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

[%%expect_llvm_ir AArch64{|source_filename = "<source>"

define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP1__add1_0_1_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
}

define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP1__entry"(i64 %0, i64 %1) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %3 = alloca i64
  %4 = alloca ptr addrspace(1)
  %5 = alloca i64
  %6 = alloca i64
  %7 = alloca i64
  br label %L1
L1:
  br label %L108
L108:
  %8 = ptrtoint ptr @"\01_camlTOP1" to i64
  store i64 %8, ptr %5
  %9 = load i64, ptr %5
  store i64 %9, ptr %6
  %10 = load i64, ptr %6
  %11 = inttoptr i64 %10 to ptr addrspace(1)
  store ptr addrspace(1) %11, ptr %4
  store i64 1, ptr %3
  %12 = load ptr addrspace(1), ptr %3
  %13 = load i64, ptr %ds
  %14 = load i64, ptr %alloc
  %15 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %13, 0, 0
  %16 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %15, i64 %14, 0, 1
  %17 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %16, ptr addrspace(1) %12, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %17
}

define private oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_wrap_try"(i64 %0, i64 %1) returns_twice noinline {
  %3 = insertvalue { { i64, i64 }, { i64 } } poison, i64 0, 1, 0
  %4 = insertvalue { { i64, i64 }, { i64 } } %3, i64 %0, 0, 0
  %5 = insertvalue { { i64, i64 }, { i64 } } %4, i64 %1, 0, 1
  ret { { i64, i64 }, { i64 } } %5
}

@"\01_camlTOP1__gc_roots" = global { i64 } { i64 0 }, section "__DATA,__data", align 8
@"\01_header.camlTOP1" = global i64 1792, section "__DATA,__data", align 8
@"\01_camlTOP1" = global { ptr } { ptr @"\01_camlTOP1__add1_1" }, section "__DATA,__data", align 8
@"\01_header.camlTOP1__add1_1" = global i64 3063, section "__DATA,__data", align 8
@"\01_camlTOP1__add1_1" = global { ptr, i64 } { ptr @"\01_camlTOP1__add1_0_1_code", i64 108086391056891909 }, section "__DATA,__data", align 8



!0 = !{ i32 1, !"oxcaml_module", !"TOP1" }
!llvm.module.flags = !{ !0 }
|}]

[%%expect_llvm_asm AArch64{|	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, <version>
	.globl	_camlTOP1__code_begin
_camlTOP1__code_begin:
	.section	__DATA,__data
	.globl	_camlTOP1__data_begin
_camlTOP1__data_begin:
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlTOP1__add1_0_1_code        ; -- Begin function _camlTOP1__add1_0_1_code
	.p2align	2
_camlTOP1__add1_0_1_code:               ; @"\01_camlTOP1__add1_0_1_code"
; %bb.0:                                ; %L1
	add	x0, x0, #2
	ret
                                        ; -- End function
	.globl	_camlTOP1__entry                ; -- Begin function _camlTOP1__entry
	.p2align	2
_camlTOP1__entry:                       ; @"\01_camlTOP1__entry"
; %bb.0:                                ; %L1
	mov	w0, #1
	ret
                                        ; -- End function
	.section	__DATA,__data
	.globl	_camlTOP1__gc_roots             ; @"\01_camlTOP1__gc_roots"
	.p2align	3, 0x0
_camlTOP1__gc_roots:
	.space	8

	.globl	_header.camlTOP1                ; @"\01_header.camlTOP1"
	.p2align	3, 0x0
_header.camlTOP1:
	.quad	1792                            ; 0x700

	.globl	_camlTOP1                       ; @"\01_camlTOP1"
	.p2align	3, 0x0
_camlTOP1:
	.quad	_camlTOP1__add1_1

	.globl	_header.camlTOP1__add1_1        ; @"\01_header.camlTOP1__add1_1"
	.p2align	3, 0x0
_header.camlTOP1__add1_1:
	.quad	3063                            ; 0xbf7

	.globl	_camlTOP1__add1_1               ; @"\01_camlTOP1__add1_1"
	.p2align	3, 0x0
_camlTOP1__add1_1:
	.quad	_camlTOP1__add1_0_1_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_camlTOP1__frametable
_camlTOP1__frametable:
	.quad	0

	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlTOP1__code_end
_camlTOP1__code_end:
	.section	__DATA,__data
	.globl	_camlTOP1__data_end
_camlTOP1__data_end:
.subsections_via_symbols
|}]
