(* TEST
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

let tag (x : Obj.t) = Obj.tag x;;

[%%expect{|
val tag : Obj.t -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__tag_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  br label %L101
L101:
  %7 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %7, ptr %5
  %8 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %8, ptr %4
  %9 = ptrtoint ptr @"\01_caml_obj_tag" to i64
  %10 = load ptr addrspace(1), ptr %4
  %11 = ptrtoint ptr addrspace(1) %10 to i64
  %12 = icmp eq i64 %11, 0
  br i1 %12, label %L105, label %L106
L105:
  %13 = inttoptr i64 2021 to ptr addrspace(1)
  store ptr addrspace(1) %13, ptr %4
  br label %L104
L106:
  %14 = and i64 %11, 1
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %L107, label %L108
L107:
  %16 = inttoptr i64 2001 to ptr addrspace(1)
  store ptr addrspace(1) %16, ptr %4
  br label %L104
L108:
  %17 = and i64 %11, 7
  %18 = icmp ne i64 %17, 0
  br i1 %18, label %L109, label %L110
L109:
  %19 = inttoptr i64 2005 to ptr addrspace(1)
  store ptr addrspace(1) %19, ptr %4
  br label %L104
L110:
  %20 = inttoptr i64 %11 to ptr
  %21 = ptrtoint ptr %20 to i64
  %22 = add i64 %21, -8
  %23 = inttoptr i64 %22 to ptr
  %24 = load atomic i64, ptr %23 acquire, align 8
  %25 = and i64 %24, 255
  %26 = shl i64 %25, 1
  %27 = or i64 %26, 1
  %28 = inttoptr i64 %27 to ptr addrspace(1)
  store ptr addrspace(1) %28, ptr %4
  br label %L104
L104:
  br label %L103
L103:
  %29 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %29, ptr %6
  %30 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %30, ptr %4
  %31 = load i64, ptr %4
  %32 = load i64, ptr %ds
  %33 = load i64, ptr %alloc
  %34 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %32, 0, 0
  %35 = insertvalue { { i64, i64 }, { i64 } } %34, i64 %33, 0, 1
  %36 = insertvalue { { i64, i64 }, { i64 } } %35, i64 %31, 1, 0
  ret { { i64, i64 }, { i64 } } %36
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__tag_0_1_code:
; %bb.0:                                ; %L1
	cbz	x0, LBB0_4
; %bb.1:                                ; %L106
	tbnz	w0, #0, LBB0_5
; %bb.2:                                ; %L108
	tst	x0, #0x7
	b.eq	LBB0_6
; %bb.3:
	mov	w0, #2005
	ret
LBB0_4:
	mov	w0, #2021
	ret
LBB0_5:
	mov	w0, #2001
	ret
LBB0_6:                                 ; %L110
	sub	x8, x0, #8
	ldapr	x8, [x8]
                                        ; kill: def $w8 killed $w8 killed $x8 def $x8
	mov	w0, #1
	bfi	x0, x8, #1, #8
	ret|}]
