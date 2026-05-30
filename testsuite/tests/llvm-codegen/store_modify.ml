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

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__set_i_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  fence acquire
  %16 = load i64, ptr %9
  store i64 %16, ptr addrspace(1) %15
  store i64 1, ptr %11
  store i64 1, ptr %7
  %17 = load i64, ptr %7
  %18 = load i64, ptr %ds
  %19 = load i64, ptr %alloc
  %20 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %18, 0, 0
  %21 = insertvalue { { i64, i64 }, { i64 } } %20, i64 %19, 0, 1
  %22 = insertvalue { { i64, i64 }, { i64 } } %21, i64 %17, 1, 0
  ret { { i64, i64 }, { i64 } } %22
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

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__set_s_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %10 = alloca ptr addrspace(1)
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
  store ptr addrspace(1) %17, ptr %10
  %18 = load ptr addrspace(1), ptr %10
  %19 = ptrtoint ptr addrspace(1) %18 to i64
  store i64 %19, ptr %7
  %20 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %20, ptr %6
  %21 = ptrtoint ptr @"\01_caml_modify" to i64
  %22 = load i32, ptr @"\01_caml_llvm_helper_profile_enabled"
  %23 = icmp ne i32 %22, 0
  %24 = call  i1 @llvm.expect.i1(i1 %23, i1 0)
  br i1 %24, label %L128, label %L129
L128:
  %25 = load i64, ptr %7
  %26 = load ptr addrspace(1), ptr %6
  %27 = load i64, ptr %ds
  %28 = load i64, ptr %alloc
  %29 = call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %27, i64 %28, i64 %25, ptr addrspace(1) %26) "gc-leaf-function"="true"
  %30 = extractvalue { i64, i64 } %29, 0
  %31 = extractvalue { i64, i64 } %29, 1
  store i64 %30, ptr %ds
  store i64 %31, ptr %alloc
  br label %L127
L129:
  %32 = load i64, ptr %7
  %33 = inttoptr i64 %32 to ptr
  %34 = load ptr addrspace(1), ptr %6
  %35 = ptrtoint ptr addrspace(1) %34 to i64
  %36 = load i64, ptr %33
  %37 = load i64, ptr @"\01_caml_minor_heaps_start"
  %38 = load i64, ptr @"\01_caml_minor_heaps_end"
  %39 = icmp ugt i64 %32, %37
  %40 = icmp ult i64 %32, %38
  %41 = and i1 %39, %40
  %42 = icmp eq i1 %41, 0
  %43 = and i64 %36, 1
  %44 = icmp eq i64 %43, 0
  %45 = icmp ne i64 %36, 0
  %46 = and i1 %44, %45
  %47 = and i64 %36, 1
  %48 = icmp eq i64 %47, 0
  %49 = icmp ne i64 %36, 0
  %50 = and i1 %48, %49
  %51 = load i64, ptr @"\01_caml_minor_heaps_start"
  %52 = load i64, ptr @"\01_caml_minor_heaps_end"
  %53 = icmp ugt i64 %36, %51
  %54 = icmp ult i64 %36, %52
  %55 = and i1 %53, %54
  %56 = and i1 %50, %55
  %57 = icmp eq i1 %56, 0
  %58 = and i1 %46, %57
  %59 = and i64 %35, 1
  %60 = icmp eq i64 %59, 0
  %61 = icmp ne i64 %35, 0
  %62 = and i1 %60, %61
  %63 = load i64, ptr @"\01_caml_minor_heaps_start"
  %64 = load i64, ptr @"\01_caml_minor_heaps_end"
  %65 = icmp ugt i64 %35, %63
  %66 = icmp ult i64 %35, %64
  %67 = and i1 %65, %66
  %68 = and i1 %62, %67
  %69 = load i32, ptr @"\01_caml_gc_phase"
  %70 = icmp ne i32 %69, 0
  %71 = and i1 %58, %70
  %72 = or i1 %68, %71
  %73 = and i1 %42, %72
  %74 = and i1 %73, %57
  %75 = call  i1 @llvm.expect.i1(i1 %74, i1 0)
  br i1 %75, label %L130, label %L131
L130:
  %76 = load i64, ptr %ds
  %77 = load i64, ptr %alloc
  %78 = call oxcaml_nofpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_modify_slow_barrier.3.i64.i64.i64.0"(i64 %76, i64 %77, i64 %32, i64 %36, i64 %35) "gc-leaf-function"="true" cold [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 1, i64 0, i64 16, i64 24, i64 0, i64 24, i64 0, i64 10, i64 5263188, i64 7548467, i64 6255717, i64 115) ]
  %79 = extractvalue { { i64, i64 }, {  } } %78, 0, 0
  %80 = extractvalue { { i64, i64 }, {  } } %78, 0, 1
  store i64 %79, ptr %ds
  store i64 %80, ptr %alloc
  br label %L131
L131:
  fence acquire
  store atomic i64 %35, ptr %33 release, align 8
  br label %L127
L127:
  br label %L125
L125:
  store i64 1, ptr %12
  store i64 1, ptr %7
  %81 = load i64, ptr %7
  %82 = load i64, ptr %ds
  %83 = load i64, ptr %alloc
  %84 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %82, 0, 0
  %85 = insertvalue { { i64, i64 }, { i64 } } %84, i64 %83, 0, 1
  %86 = insertvalue { { i64, i64 }, { i64 } } %85, i64 %81, 1, 0
  ret { { i64, i64 }, { i64 } } %86
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__set_s_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	mov	x13, x1
	add	x0, x0, #8
Lloh0:
	adrp	x8, _caml_llvm_helper_profile_enabled@GOTPAGE
Lloh1:
	ldr	x8, [x8, _caml_llvm_helper_profile_enabled@GOTPAGEOFF]
Lloh2:
	ldr	w8, [x8]
	cbnz	w8, LBB0_3
; %bb.1:                                ; %L129
	ldr	x1, [x0]
Lloh3:
	adrp	x8, _caml_minor_heaps_start@GOTPAGE
Lloh4:
	ldr	x8, [x8, _caml_minor_heaps_start@GOTPAGEOFF]
Lloh5:
	ldr	x8, [x8]
Lloh6:
	adrp	x9, _caml_minor_heaps_end@GOTPAGE
Lloh7:
	ldr	x9, [x9, _caml_minor_heaps_end@GOTPAGEOFF]
Lloh8:
	ldr	x9, [x9]
	tst	x1, #0x1
	cset	w10, eq
	cmp	x1, #0
	cset	w11, ne
	cmp	x1, x8
	ccmp	x1, x9, #2, hi
	cset	w12, lo
	csel	w10, wzr, w10, hs
	eor	w11, w11, w12
Lloh9:
	adrp	x12, _caml_gc_phase@GOTPAGE
Lloh10:
	ldr	x12, [x12, _caml_gc_phase@GOTPAGEOFF]
Lloh11:
	ldr	w12, [x12]
	cmp	w12, #0
	csel	w11, wzr, w11, eq
	tst	x1, #0x1
	csel	w11, wzr, w11, ne
	tst	x13, #0x1
	ccmp	x13, #0, #4, eq
	cset	w12, ne
	cmp	x8, x13
	ccmp	x9, x13, #0, lo
	csel	w12, wzr, w12, ls
	cmp	x9, x0
	ccmp	x8, x0, #2, hi
	orr	w8, w12, w11
	ccmp	w8, #0, #4, hs
	ccmp	w10, #0, #0, ne
	b.eq	LBB0_4
; %bb.2:                                ; %L131
	dmb	ishld
	stlr	x13, [x0]
	mov	w0, #1
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_3:                                 ; %L128
	mov	x1, x13
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_modify
	mov	sp, x29
	.cfi_restore_state
	mov	w0, #1
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_4:                                 ; %L130
	str	x0, [sp]                        ; 8-byte Folded Spill
	str	x13, [sp, #8]                   ; 8-byte Folded Spill
	mov	x2, x13
	bl	_c_call_wrapper.caml_modify_slow_barrier.3.i64.i64.i64.0
Ltmp0:
	ldr	x0, [sp]                        ; 8-byte Folded Reload
	ldr	x13, [sp, #8]                   ; 8-byte Folded Reload
	dmb	ishld
	stlr	x13, [x0]
	mov	w0, #1
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
	.loh AdrpLdrGotLdr	Lloh0, Lloh1, Lloh2
	.loh AdrpLdrGotLdr	Lloh9, Lloh10, Lloh11
	.loh AdrpLdrGotLdr	Lloh6, Lloh7, Lloh8
	.loh AdrpLdrGotLdr	Lloh3, Lloh4, Lloh5
	.cfi_endproc|}]
