(* TEST
 reason = "Brittle golden IR test: encoded statepoint-id values differ across self-stage compilers.";
 skip;
 macos;
 arch_arm64;
 flags += " -O3 -llvm-backend";
 expect.opt;
*)

external poll : unit -> unit = "%poll";;

[%%expect{|
external poll : unit -> unit = "%poll"
|}]

let poll_select x y n =
  poll ();
  if n = 0 then x else y
;;

[%%expect{|
val poll_select : 'a -> 'a -> int -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_select_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, i64 %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca i64
  store i64 %4, ptr %8
  %9 = alloca ptr addrspace(1)
  %10 = alloca ptr addrspace(1)
  %11 = alloca i64
  %12 = alloca i64
  %13 = alloca i64
  %14 = alloca ptr addrspace(1)
  %15 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L110
L110:
  %16 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %16, ptr %9
  %17 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %17, ptr %10
  %18 = load i64, ptr %8
  store i64 %18, ptr %11
  %19 = load i64, ptr %alloc
  %20 = load i64, ptr %ds
  %21 = inttoptr i64 %20 to ptr
  %22 = load i64, ptr %21
  %23 = icmp ult i64 %22, %19
  %24 = call  i1 @llvm.expect.i1(i1 %23, i1 1)
  br i1 %24, label %L119, label %L118
L118:
  %25 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %25, ptr %14
  %26 = load ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %26, ptr %15
  %27 = load i64, ptr %ds
  %28 = load i64, ptr %alloc
  %29 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %27, i64 %28) "statepoint-id"="1" cold [ "gc-live"(ptr %14, ptr %15) ]
  %30 = extractvalue { { i64, i64 }, {  } } %29, 0, 0
  %31 = extractvalue { { i64, i64 }, {  } } %29, 0, 1
  store i64 %30, ptr %ds
  store i64 %31, ptr %alloc
  %32 = load volatile ptr addrspace(1), ptr %14
  store ptr addrspace(1) %32, ptr %9
  %33 = load volatile ptr addrspace(1), ptr %15
  store ptr addrspace(1) %33, ptr %10
  br label %L119
L119:
  store i64 1, ptr %13
  %34 = load i64, ptr %11
  %35 = icmp slt i64 %34, 1
  br i1 %35, label %L115, label %L120
L120:
  %36 = load i64, ptr %11
  %37 = icmp sgt i64 %36, 1
  br i1 %37, label %L115, label %L113
L113:
  %38 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %38, ptr %6
  %39 = load ptr addrspace(1), ptr %6
  %40 = load i64, ptr %ds
  %41 = load i64, ptr %alloc
  %42 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %40, 0, 0
  %43 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %42, i64 %41, 0, 1
  %44 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %43, ptr addrspace(1) %39, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %44
L115:
  %45 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %45, ptr %6
  %46 = load ptr addrspace(1), ptr %6
  %47 = load i64, ptr %ds
  %48 = load i64, ptr %alloc
  %49 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %47, 0, 0
  %50 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %49, i64 %48, 0, 1
  %51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %50, ptr addrspace(1) %46, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %51
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_select_0_1_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_2
; %bb.1:                                ; %L119
	cmp	x2, #1
	csel	x0, x0, x1, eq
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L118
	str	x0, [sp, #8]
	str	x1, [sp]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	ldr	x1, [sp]
	cmp	x2, #1
	csel	x0, x0, x1, eq
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc|}]

let poll_const_int x =
  let n = 41 in
  poll ();
  String.length x + n
;;

[%%expect{|
val poll_const_int : String.t -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__poll_const_int_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca i64
  %6 = alloca ptr addrspace(1)
  %7 = alloca i64
  %8 = alloca i64
  %9 = alloca i64
  %10 = alloca i64
  %11 = alloca i64
  %12 = alloca i64
  %13 = alloca i64
  %14 = alloca i64
  %15 = alloca i64
  %16 = alloca i64
  %17 = alloca i64
  %18 = alloca i64
  %19 = alloca i64
  %20 = alloca i64
  %21 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L131
L131:
  %22 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %22, ptr %6
  %23 = load i64, ptr %alloc
  %24 = load i64, ptr %ds
  %25 = inttoptr i64 %24 to ptr
  %26 = load i64, ptr %25
  %27 = icmp ult i64 %26, %23
  %28 = call  i1 @llvm.expect.i1(i1 %27, i1 1)
  br i1 %28, label %L145, label %L144
L144:
  %29 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %29, ptr %21
  %30 = load i64, ptr %ds
  %31 = load i64, ptr %alloc
  %32 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %30, i64 %31) "statepoint-id"="1" cold [ "gc-live"(ptr %21) ]
  %33 = extractvalue { { i64, i64 }, {  } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, {  } } %32, 0, 1
  store i64 %33, ptr %ds
  store i64 %34, ptr %alloc
  %35 = load volatile ptr addrspace(1), ptr %21
  store ptr addrspace(1) %35, ptr %6
  br label %L145
L145:
  store i64 1, ptr %8
  %36 = load ptr addrspace(1), ptr %6
  %37 = getelementptr i8, ptr addrspace(1) %36, i64 -8
  %38 = ptrtoint ptr addrspace(1) %37 to i64
  store i64 %38, ptr %9
  %39 = load i64, ptr %9
  %40 = inttoptr i64 %39 to ptr
  %41 = load i64, ptr %40
  store i64 %41, ptr %10
  %42 = load i64, ptr %10
  %43 = shl i64 %42, 8
  store i64 %43, ptr %11
  %44 = load i64, ptr %11
  %45 = lshr i64 %44, 18
  store i64 %45, ptr %12
  %46 = load i64, ptr %12
  %47 = shl i64 %46, 3
  store i64 %47, ptr %13
  %48 = load i64, ptr %13
  %49 = sub i64 %48, 1
  store i64 %49, ptr %14
  %50 = load i64, ptr %14
  store i64 %50, ptr %15
  %51 = load ptr addrspace(1), ptr %6
  %52 = load i64, ptr %15
  %53 = getelementptr i8, ptr addrspace(1) %51, i64 %52
  %54 = ptrtoint ptr addrspace(1) %53 to i64
  store i64 %54, ptr %16
  %55 = load i64, ptr %16
  %56 = inttoptr i64 %55 to ptr
  %57 = load i8, ptr %56
  %58 = zext i8 %57 to i64
  store i64 %58, ptr %17
  %59 = load i64, ptr %15
  %60 = load i64, ptr %17
  %61 = sub i64 %59, %60
  store i64 %61, ptr %18
  %62 = load i64, ptr %18
  %63 = shl i64 %62, 1
  %64 = add i64 83, %63
  store i64 %64, ptr %20
  %65 = load i64, ptr %20
  store i64 %65, ptr %5
  %66 = load i64, ptr %5
  %67 = load i64, ptr %ds
  %68 = load i64, ptr %alloc
  %69 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %67, 0, 0
  %70 = insertvalue { { i64, i64 }, { i64 } } %69, i64 %68, 0, 1
  %71 = insertvalue { { i64, i64 }, { i64 } } %70, i64 %66, 1, 0
  ret { { i64, i64 }, { i64 } } %71
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_const_int_2_3_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_2
LBB0_1:                                 ; %L145
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	lsl	x8, x8, #1
	add	x0, x8, #83
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L144
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	b	LBB0_1
	.cfi_endproc|}]

let poll_under_trap x y f =
  try
    poll ();
    f ();
    y
  with _ -> x
;;

[%%expect{|
val poll_under_trap : 'a -> 'a -> (unit -> 'b) -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_under_trap_4_5_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="48" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca i64
  %10 = alloca ptr addrspace(1)
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  %13 = alloca i64
  %14 = alloca i64
  %15 = alloca ptr addrspace(1)
  %16 = alloca i64
  %17 = alloca i64
  %18 = alloca i64
  %19 = alloca i64
  %20 = alloca ptr addrspace(1)
  %21 = alloca ptr addrspace(1)
  %22 = alloca i64
  %23 = alloca i64
  %24 = alloca i64
  br label %L1
L1:
  br label %L156
L156:
  %25 = load i64, ptr %ds
  %26 = add i64 %25, 40
  %27 = inttoptr i64 %26 to ptr
  %28 = load i64, ptr %27
  %29 = add i64 %28, 408
  %30 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %31 = icmp uge i64 %30, %29
  %32 = call  i1 @llvm.expect.i1(i1 %31, i1 1)
  br i1 %32, label %L176, label %L175
L175:
  %33 = load i64, ptr %ds
  %34 = load i64, ptr %alloc
  %35 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %33, i64 %34, i64 38) "gc-leaf-function"="true" cold
  %36 = extractvalue { { i64, i64 }, {  } } %35, 0, 0
  %37 = extractvalue { { i64, i64 }, {  } } %35, 0, 1
  store i64 %36, ptr %ds
  store i64 %37, ptr %alloc
  br label %L176
L176:
  %38 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %38, ptr %10
  %39 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %39, ptr %11
  %40 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %40, ptr %12
  %41 = load i64, ptr %ds
  %42 = add i64 %41, 64
  %43 = inttoptr i64 %42 to ptr
  %44 = load i64, ptr %43
  store i64 %44, ptr %13
  %45 = load i64, ptr %13
  store volatile i64 %45, ptr %14
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTOP__poll_under_trap_4_5_code", %L179))
  br label %L177
L178:
  %46 = landingpad token cleanup
  br label %L179
L179:
  %47 = call  { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %48 = extractvalue { i64, i64, i64, i64 } %47, 0
  %49 = extractvalue { i64, i64, i64, i64 } %47, 1
  %50 = extractvalue { i64, i64, i64, i64 } %47, 2
  %51 = extractvalue { i64, i64, i64, i64 } %47, 3
  store i64 %48, ptr %24
  store i64 %51, ptr %ds
  store i64 %50, ptr %alloc
  call void asm sideeffect "mov x26, $0", "r"(i64 %49) "gc-leaf-function"="true"
  br label %L159
L177:
  %52 = load i64, ptr %alloc
  %53 = load i64, ptr %ds
  %54 = inttoptr i64 %53 to ptr
  %55 = load i64, ptr %54
  %56 = icmp ult i64 %55, %52
  %57 = call  i1 @llvm.expect.i1(i1 %56, i1 1)
  br i1 %57, label %L181, label %L180
L180:
  %58 = load i64, ptr %ds
  %59 = load i64, ptr %alloc
  %60 = invoke oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %58, i64 %59) "statepoint-id"="33" cold [ "gc-live"(ptr %10, ptr %11, ptr %12) ] to label %L182 unwind label %L178
L182:
  %61 = extractvalue { { i64, i64 }, {  } } %60, 0, 0
  %62 = extractvalue { { i64, i64 }, {  } } %60, 0, 1
  store i64 %61, ptr %ds
  store i64 %62, ptr %alloc
  br label %L181
L181:
  store i64 1, ptr %17
  %63 = load volatile ptr addrspace(1), ptr %12
  %64 = addrspacecast ptr addrspace(1) %63 to ptr
  %65 = load i64, ptr %64
  store i64 %65, ptr %19
  store i64 1, ptr %9
  %66 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %66, ptr %7
  %67 = load i64, ptr %9
  %68 = load ptr addrspace(1), ptr %7
  %69 = load i64, ptr %ds
  %70 = load i64, ptr %alloc
  %71 = load ptr, ptr %19
  %72 = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } %71(i64 %69, i64 %70, i64 %67, ptr addrspace(1) %68) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 4, i64 0, i64 4, i64 8, i64 0, i64 8, i64 0, i64 20, i64 5263188, i64 7351860, i64 7105647, i64 7239007, i64 7497060, i64 7500895, i64 28769), "gc-live"(ptr %10, ptr %11) ] to label %L183 unwind label %L178
L183:
  %73 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %72, 0, 0
  %74 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %72, 0, 1
  store i64 %73, ptr %ds
  store i64 %74, ptr %alloc
  %75 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %72, 1, 0
  store ptr addrspace(1) %75, ptr %6
  br label %L168
L168:
  %76 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %76, ptr %20
  %77 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %77, ptr %21
  call  void @llvm.aarch64.oxcaml.pop.trap()
  %78 = load volatile ptr addrspace(1), ptr %11
  store ptr addrspace(1) %78, ptr %6
  %79 = load ptr addrspace(1), ptr %6
  %80 = load i64, ptr %ds
  %81 = load i64, ptr %alloc
  %82 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %80, 0, 0
  %83 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %82, i64 %81, 0, 1
  %84 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %83, ptr addrspace(1) %79, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %84
L159:
  %85 = load i64, ptr %24
  store i64 %85, ptr %9
  %86 = load i64, ptr %9
  %87 = inttoptr i64 %86 to ptr addrspace(1)
  store ptr addrspace(1) %87, ptr %15
  %88 = load i64, ptr %ds
  %89 = add i64 %88, 64
  %90 = inttoptr i64 %89 to ptr
  %91 = load volatile i64, ptr %14
  store i64 %91, ptr %90
  store i64 1, ptr %23
  %92 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %92, ptr %6
  %93 = load ptr addrspace(1), ptr %6
  %94 = load i64, ptr %ds
  %95 = load i64, ptr %alloc
  %96 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %94, 0, 0
  %97 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %96, i64 %95, 0, 1
  %98 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %97, ptr addrspace(1) %93, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %98
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_under_trap_4_5_code:
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception0
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	mov	x10, x0
	mov	x8, x27
	mov	x9, x28
	ldr	x11, [x28, #40]
	add	x11, x11, #408
	; InlineAsm Start
	mov	x12, sp
	; InlineAsm End
	cmp	x12, x11
	b.lo	LBB0_4
LBB0_1:                                 ; %L176
	str	x10, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	ldr	x10, [x9, #64]
	str	x10, [sp]
	adr	x16, LBB0_7
	stp	x26, x16, [sp, #-16]!
	mov	x26, sp
	ldr	x10, [x9]
	cmp	x10, x8
	b.hs	LBB0_5
LBB0_2:                                 ; %L181
	ldr	x10, [sp, #24]
	ldr	x10, [x10]
	ldr	x1, [sp, #24]
	mov	x28, x9
	mov	x27, x8
	mov	w0, #1
	blr	x10
Ltmp1:
; %bb.3:                                ; %L183
	ldr	x26, [sp], #16
	add	x8, sp, #16
	ldr	x0, [x8]
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_4:                                 ; %L175
	mov	x28, x9
	mov	x27, x8
	mov	w0, #38
	bl	_caml_llvm_call_realloc_stack
	mov	x9, x28
	mov	x8, x27
	b	LBB0_1
LBB0_5:                                 ; %L180
	mov	x28, x9
	mov	x27, x8
	bl	_caml_call_gc
Ltmp2:
; %bb.6:                                ; %L182
	mov	x9, x28
	mov	x8, x27
	b	LBB0_2
Ltmp3:                                  ; Block address taken
LBB0_7:                                 ; %L179
                                        ; Label of block must be emitted
                                        ; implicit-def: $q0
                                        ; implicit-def: $q1
                                        ; implicit-def: $q2
                                        ; implicit-def: $q3
                                        ; implicit-def: $q4
                                        ; implicit-def: $q5
                                        ; implicit-def: $q6
                                        ; implicit-def: $q7
                                        ; implicit-def: $q8
                                        ; implicit-def: $q9
                                        ; implicit-def: $q10
                                        ; implicit-def: $q11
                                        ; implicit-def: $q12
                                        ; implicit-def: $q13
                                        ; implicit-def: $q14
                                        ; implicit-def: $q15
                                        ; implicit-def: $q16
                                        ; implicit-def: $q17
                                        ; implicit-def: $q18
                                        ; implicit-def: $q19
                                        ; implicit-def: $q20
                                        ; implicit-def: $q21
                                        ; implicit-def: $q22
                                        ; implicit-def: $q23
                                        ; implicit-def: $q24
                                        ; implicit-def: $q25
                                        ; implicit-def: $q26
                                        ; implicit-def: $q27
                                        ; implicit-def: $q28
                                        ; implicit-def: $q29
                                        ; implicit-def: $q30
                                        ; implicit-def: $q31
                                        ; implicit-def: $x1
                                        ; implicit-def: $x2
                                        ; implicit-def: $x3
                                        ; implicit-def: $x4
                                        ; implicit-def: $x5
                                        ; implicit-def: $x6
                                        ; implicit-def: $x7
                                        ; implicit-def: $x8
                                        ; implicit-def: $x9
                                        ; implicit-def: $x10
                                        ; implicit-def: $x11
                                        ; implicit-def: $x12
                                        ; implicit-def: $x13
                                        ; implicit-def: $x14
                                        ; implicit-def: $x19
                                        ; implicit-def: $x20
                                        ; implicit-def: $x21
                                        ; implicit-def: $x22
                                        ; implicit-def: $x23
                                        ; implicit-def: $x24
                                        ; implicit-def: $x25
	; InlineAsm Start
	mov	x26, x26
	; InlineAsm End
	ldr	x8, [sp]
	str	x8, [x28, #64]
	add	x8, sp, #24
	ldr	x0, [x8]
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
Lfunc_end0:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table0:
Lexception0:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end0-Lcst_begin0
Lcst_begin0:
	.uleb128 Lfunc_begin0-Lfunc_begin0      ; >> Call Site 1 <<
	.uleb128 Lfunc_end0-Lfunc_begin0        ;   Call between Lfunc_begin0 and Lfunc_end0
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end0:
	.p2align	2, 0x0|}]

let poll_no_roots n =
  poll ();
  n + 1
;;

[%%expect{|
val poll_no_roots : int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__poll_no_roots_6_7_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64
  store i64 %2, ptr %4
  %5 = alloca i64
  %6 = alloca i64
  %7 = alloca i64
  %8 = alloca i64
  br label %L1
L1:
  br label %L194
L194:
  %9 = load i64, ptr %4
  store i64 %9, ptr %5
  %10 = load i64, ptr %alloc
  %11 = load i64, ptr %ds
  %12 = inttoptr i64 %11 to ptr
  %13 = load i64, ptr %12
  %14 = icmp ult i64 %13, %10
  %15 = call  i1 @llvm.expect.i1(i1 %14, i1 1)
  br i1 %15, label %L199, label %L198
L198:
  %16 = load i64, ptr %ds
  %17 = load i64, ptr %alloc
  %18 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %16, i64 %17) "statepoint-id"="1" cold
  %19 = extractvalue { { i64, i64 }, {  } } %18, 0, 0
  %20 = extractvalue { { i64, i64 }, {  } } %18, 0, 1
  store i64 %19, ptr %ds
  store i64 %20, ptr %alloc
  br label %L199
L199:
  store i64 1, ptr %7
  %21 = load i64, ptr %5
  %22 = add i64 %21, 2
  store i64 %22, ptr %8
  %23 = load i64, ptr %8
  store i64 %23, ptr %4
  %24 = load i64, ptr %4
  %25 = load i64, ptr %ds
  %26 = load i64, ptr %alloc
  %27 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %25, 0, 0
  %28 = insertvalue { { i64, i64 }, { i64 } } %27, i64 %26, 0, 1
  %29 = insertvalue { { i64, i64 }, { i64 } } %28, i64 %24, 1, 0
  ret { { i64, i64 }, { i64 } } %29
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_no_roots_6_7_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_2
; %bb.1:                                ; %L199
	add	x0, x0, #2
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_2:                                 ; %L198
	bl	_caml_call_gc
Ltmp0:
	add	x0, x0, #2
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc|}]

let poll_boxed_float_root (x : float) n =
  poll ();
  if x > 0. then n + 1 else n - 1
;;

[%%expect{|
val poll_boxed_float_root : float -> int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__poll_boxed_float_root_8_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %12 = alloca double
  %13 = alloca double
  %14 = alloca i64
  %15 = alloca i64
  %16 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L210
L210:
  %17 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %17, ptr %8
  %18 = load i64, ptr %6
  store i64 %18, ptr %9
  %19 = load i64, ptr %alloc
  %20 = load i64, ptr %ds
  %21 = inttoptr i64 %20 to ptr
  %22 = load i64, ptr %21
  %23 = icmp ult i64 %22, %19
  %24 = call  i1 @llvm.expect.i1(i1 %23, i1 1)
  br i1 %24, label %L222, label %L221
L221:
  %25 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %25, ptr %16
  %26 = load i64, ptr %ds
  %27 = load i64, ptr %alloc
  %28 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %26, i64 %27) "statepoint-id"="1" cold [ "gc-live"(ptr %16) ]
  %29 = extractvalue { { i64, i64 }, {  } } %28, 0, 0
  %30 = extractvalue { { i64, i64 }, {  } } %28, 0, 1
  store i64 %29, ptr %ds
  store i64 %30, ptr %alloc
  %31 = load volatile ptr addrspace(1), ptr %16
  store ptr addrspace(1) %31, ptr %8
  br label %L222
L222:
  store i64 1, ptr %11
  store double 0x0, ptr %12
  %32 = load ptr addrspace(1), ptr %8
  %33 = addrspacecast ptr addrspace(1) %32 to ptr
  %34 = load double, ptr %33
  store double %34, ptr %13
  %35 = load double, ptr %13
  %36 = load double, ptr %12
  %37 = fcmp olt double %35, %36
  br i1 %37, label %L217, label %L223
L223:
  %38 = load double, ptr %13
  %39 = load double, ptr %12
  %40 = fcmp ogt double %38, %39
  br i1 %40, label %L214, label %L224
L224:
  %41 = load double, ptr %13
  %42 = load double, ptr %12
  %43 = fcmp oeq double %41, %42
  br i1 %43, label %L217, label %L217
L214:
  %44 = load i64, ptr %9
  %45 = add i64 %44, 2
  store i64 %45, ptr %14
  %46 = load i64, ptr %14
  store i64 %46, ptr %7
  %47 = load i64, ptr %7
  %48 = load i64, ptr %ds
  %49 = load i64, ptr %alloc
  %50 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %48, 0, 0
  %51 = insertvalue { { i64, i64 }, { i64 } } %50, i64 %49, 0, 1
  %52 = insertvalue { { i64, i64 }, { i64 } } %51, i64 %47, 1, 0
  ret { { i64, i64 }, { i64 } } %52
L217:
  %53 = load i64, ptr %9
  %54 = add i64 %53, -2
  store i64 %54, ptr %15
  %55 = load i64, ptr %15
  store i64 %55, ptr %7
  %56 = load i64, ptr %7
  %57 = load i64, ptr %ds
  %58 = load i64, ptr %alloc
  %59 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %57, 0, 0
  %60 = insertvalue { { i64, i64 }, { i64 } } %59, i64 %58, 0, 1
  %61 = insertvalue { { i64, i64 }, { i64 } } %60, i64 %56, 1, 0
  ret { { i64, i64 }, { i64 } } %61
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_boxed_float_root_8_9_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_2
LBB0_1:                                 ; %L222
	ldr	d0, [x0]
	fcmp	d0, #0.0
	mov	w8, #2
	mov	x9, #-2
	csel	x8, x8, x9, gt
	add	x0, x8, x1
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L221
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	b	LBB0_1
	.cfi_endproc|}]

let poll_many_roots x y z n =
  poll ();
  if n = 0 then x else if n = 1 then y else z
;;

[%%expect{|
val poll_many_roots : 'a -> 'a -> 'a -> int -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_many_roots_10_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4, i64 %5) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %8
  %9 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %9
  %10 = alloca i64
  store i64 %5, ptr %10
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  %13 = alloca ptr addrspace(1)
  %14 = alloca i64
  %15 = alloca i64
  %16 = alloca i64
  %17 = alloca ptr addrspace(1)
  %18 = alloca ptr addrspace(1)
  %19 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L235
L235:
  %20 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %20, ptr %11
  %21 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %21, ptr %12
  %22 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %22, ptr %13
  %23 = load i64, ptr %10
  store i64 %23, ptr %14
  %24 = load i64, ptr %alloc
  %25 = load i64, ptr %ds
  %26 = inttoptr i64 %25 to ptr
  %27 = load i64, ptr %26
  %28 = icmp ult i64 %27, %24
  %29 = call  i1 @llvm.expect.i1(i1 %28, i1 1)
  br i1 %29, label %L249, label %L248
L248:
  %30 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %30, ptr %17
  %31 = load ptr addrspace(1), ptr %12
  store volatile ptr addrspace(1) %31, ptr %18
  %32 = load ptr addrspace(1), ptr %13
  store volatile ptr addrspace(1) %32, ptr %19
  %33 = load i64, ptr %ds
  %34 = load i64, ptr %alloc
  %35 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %33, i64 %34) "statepoint-id"="1" cold [ "gc-live"(ptr %17, ptr %18, ptr %19) ]
  %36 = extractvalue { { i64, i64 }, {  } } %35, 0, 0
  %37 = extractvalue { { i64, i64 }, {  } } %35, 0, 1
  store i64 %36, ptr %ds
  store i64 %37, ptr %alloc
  %38 = load volatile ptr addrspace(1), ptr %17
  store ptr addrspace(1) %38, ptr %11
  %39 = load volatile ptr addrspace(1), ptr %18
  store ptr addrspace(1) %39, ptr %12
  %40 = load volatile ptr addrspace(1), ptr %19
  store ptr addrspace(1) %40, ptr %13
  br label %L249
L249:
  store i64 1, ptr %16
  %41 = load i64, ptr %14
  %42 = icmp slt i64 %41, 1
  br i1 %42, label %L240, label %L250
L250:
  %43 = load i64, ptr %14
  %44 = icmp sgt i64 %43, 1
  br i1 %44, label %L240, label %L238
L238:
  %45 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %45, ptr %7
  %46 = load ptr addrspace(1), ptr %7
  %47 = load i64, ptr %ds
  %48 = load i64, ptr %alloc
  %49 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %47, 0, 0
  %50 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %49, i64 %48, 0, 1
  %51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %50, ptr addrspace(1) %46, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %51
L240:
  %52 = load i64, ptr %14
  %53 = icmp slt i64 %52, 3
  br i1 %53, label %L244, label %L251
L251:
  %54 = load i64, ptr %14
  %55 = icmp sgt i64 %54, 3
  br i1 %55, label %L244, label %L242
L242:
  %56 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %56, ptr %7
  %57 = load ptr addrspace(1), ptr %7
  %58 = load i64, ptr %ds
  %59 = load i64, ptr %alloc
  %60 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %58, 0, 0
  %61 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %60, i64 %59, 0, 1
  %62 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %61, ptr addrspace(1) %57, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %62
L244:
  %63 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %63, ptr %7
  %64 = load ptr addrspace(1), ptr %7
  %65 = load i64, ptr %ds
  %66 = load i64, ptr %alloc
  %67 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %65, 0, 0
  %68 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %67, i64 %66, 0, 1
  %69 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %68, ptr addrspace(1) %64, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %69
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_many_roots_10_11_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_5
; %bb.1:                                ; %L249
	cmp	x3, #3
	b.eq	LBB0_4
LBB0_2:                                 ; %L249
	mov	x1, x0
	cmp	x3, #1
	b.eq	LBB0_4
; %bb.3:                                ; %L244
	mov	x1, x2
LBB0_4:                                 ; %common.ret
	mov	x0, x1
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_5:                                 ; %L248
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	cmp	x3, #3
	b.ne	LBB0_2
	b	LBB0_4
	.cfi_endproc|}]

let poll_aliased_roots x n =
  let y = Sys.opaque_identity x in
  poll ();
  if n = 0 then x else y
;;

[%%expect{|
val poll_aliased_roots : 'a -> int -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_aliased_roots_12_13_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %5 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %5
  %6 = alloca i64
  store i64 %3, ptr %6
  %7 = alloca ptr addrspace(1)
  %8 = alloca i64
  %9 = alloca ptr addrspace(1)
  %10 = alloca i64
  %11 = alloca i64
  %12 = alloca ptr addrspace(1)
  %13 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L262
L262:
  %14 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %14, ptr %7
  %15 = load i64, ptr %6
  store i64 %15, ptr %8
  %16 = load ptr addrspace(1), ptr %7
  %17 = call ptr addrspace(1) asm  "", "=r,0"(ptr addrspace(1) %16) "gc-leaf-function"="true"
  store ptr addrspace(1) %17, ptr %7
  %18 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %18, ptr %9
  %19 = load i64, ptr %alloc
  %20 = load i64, ptr %ds
  %21 = inttoptr i64 %20 to ptr
  %22 = load i64, ptr %21
  %23 = icmp ult i64 %22, %19
  %24 = call  i1 @llvm.expect.i1(i1 %23, i1 1)
  br i1 %24, label %L271, label %L270
L270:
  %25 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %25, ptr %12
  %26 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %26, ptr %13
  %27 = load i64, ptr %ds
  %28 = load i64, ptr %alloc
  %29 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %27, i64 %28) "statepoint-id"="1" cold [ "gc-live"(ptr %12, ptr %13) ]
  %30 = extractvalue { { i64, i64 }, {  } } %29, 0, 0
  %31 = extractvalue { { i64, i64 }, {  } } %29, 0, 1
  store i64 %30, ptr %ds
  store i64 %31, ptr %alloc
  %32 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %32, ptr %7
  %33 = load volatile ptr addrspace(1), ptr %13
  store ptr addrspace(1) %33, ptr %9
  br label %L271
L271:
  store i64 1, ptr %11
  %34 = load i64, ptr %8
  %35 = icmp slt i64 %34, 1
  br i1 %35, label %L267, label %L272
L272:
  %36 = load i64, ptr %8
  %37 = icmp sgt i64 %36, 1
  br i1 %37, label %L267, label %L265
L265:
  %38 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %38, ptr %5
  %39 = load ptr addrspace(1), ptr %5
  %40 = load i64, ptr %ds
  %41 = load i64, ptr %alloc
  %42 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %40, 0, 0
  %43 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %42, i64 %41, 0, 1
  %44 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %43, ptr addrspace(1) %39, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %44
L267:
  %45 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %45, ptr %5
  %46 = load ptr addrspace(1), ptr %5
  %47 = load i64, ptr %ds
  %48 = load i64, ptr %alloc
  %49 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %47, 0, 0
  %50 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %49, i64 %48, 0, 1
  %51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %50, ptr addrspace(1) %46, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %51
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_aliased_roots_12_13_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	; InlineAsm Start
	; InlineAsm End
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_2
; %bb.1:
	cmp	x1, #1
	csel	x0, x0, x0, eq
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L270
	str	x0, [sp, #8]
	str	x0, [sp]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	ldr	x8, [sp]
	cmp	x1, #1
	csel	x0, x0, x8, eq
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc|}]

let poll_twice_varying_roots x y z n =
  poll ();
  let first = if n = 0 then x else y in
  poll ();
  if n = 1 then first else z
;;

[%%expect{|
val poll_twice_varying_roots : 'a -> 'a -> 'a -> int -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_twice_varying_roots_14_15_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4, i64 %5) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %8
  %9 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %9
  %10 = alloca i64
  store i64 %5, ptr %10
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  %13 = alloca ptr addrspace(1)
  %14 = alloca i64
  %15 = alloca i64
  %16 = alloca i64
  %17 = alloca ptr addrspace(1)
  %18 = alloca ptr addrspace(1)
  %19 = alloca ptr addrspace(1)
  %20 = alloca i64
  %21 = alloca i64
  %22 = alloca ptr addrspace(1)
  %23 = alloca ptr addrspace(1)
  %24 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L283
L283:
  %25 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %25, ptr %11
  %26 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %26, ptr %12
  %27 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %27, ptr %13
  %28 = load i64, ptr %10
  store i64 %28, ptr %14
  %29 = load i64, ptr %alloc
  %30 = load i64, ptr %ds
  %31 = inttoptr i64 %30 to ptr
  %32 = load i64, ptr %31
  %33 = icmp ult i64 %32, %29
  %34 = call  i1 @llvm.expect.i1(i1 %33, i1 1)
  br i1 %34, label %L304, label %L303
L303:
  %35 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %35, ptr %22
  %36 = load ptr addrspace(1), ptr %12
  store volatile ptr addrspace(1) %36, ptr %23
  %37 = load ptr addrspace(1), ptr %13
  store volatile ptr addrspace(1) %37, ptr %24
  %38 = load i64, ptr %ds
  %39 = load i64, ptr %alloc
  %40 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %38, i64 %39) "statepoint-id"="1" cold [ "gc-live"(ptr %22, ptr %23, ptr %24) ]
  %41 = extractvalue { { i64, i64 }, {  } } %40, 0, 0
  %42 = extractvalue { { i64, i64 }, {  } } %40, 0, 1
  store i64 %41, ptr %ds
  store i64 %42, ptr %alloc
  %43 = load volatile ptr addrspace(1), ptr %22
  store ptr addrspace(1) %43, ptr %11
  %44 = load volatile ptr addrspace(1), ptr %23
  store ptr addrspace(1) %44, ptr %12
  %45 = load volatile ptr addrspace(1), ptr %24
  store ptr addrspace(1) %45, ptr %13
  br label %L304
L304:
  store i64 1, ptr %16
  %46 = load i64, ptr %14
  %47 = icmp slt i64 %46, 1
  br i1 %47, label %L291, label %L305
L305:
  %48 = load i64, ptr %14
  %49 = icmp sgt i64 %48, 1
  br i1 %49, label %L291, label %L289
L289:
  %50 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %50, ptr %18
  %51 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %51, ptr %17
  br label %L294
L291:
  %52 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %52, ptr %19
  %53 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %53, ptr %17
  br label %L294
L294:
  %54 = load i64, ptr %alloc
  %55 = load i64, ptr %ds
  %56 = inttoptr i64 %55 to ptr
  %57 = load i64, ptr %56
  %58 = icmp ult i64 %57, %54
  %59 = call  i1 @llvm.expect.i1(i1 %58, i1 1)
  br i1 %59, label %L307, label %L306
L306:
  %60 = load ptr addrspace(1), ptr %13
  store volatile ptr addrspace(1) %60, ptr %22
  %61 = load ptr addrspace(1), ptr %17
  store volatile ptr addrspace(1) %61, ptr %23
  %62 = load i64, ptr %ds
  %63 = load i64, ptr %alloc
  %64 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %62, i64 %63) "statepoint-id"="1" cold [ "gc-live"(ptr %22, ptr %23) ]
  %65 = extractvalue { { i64, i64 }, {  } } %64, 0, 0
  %66 = extractvalue { { i64, i64 }, {  } } %64, 0, 1
  store i64 %65, ptr %ds
  store i64 %66, ptr %alloc
  %67 = load volatile ptr addrspace(1), ptr %22
  store ptr addrspace(1) %67, ptr %13
  %68 = load volatile ptr addrspace(1), ptr %23
  store ptr addrspace(1) %68, ptr %17
  br label %L307
L307:
  store i64 1, ptr %21
  %69 = load i64, ptr %14
  %70 = icmp slt i64 %69, 3
  br i1 %70, label %L299, label %L308
L308:
  %71 = load i64, ptr %14
  %72 = icmp sgt i64 %71, 3
  br i1 %72, label %L299, label %L297
L297:
  %73 = load ptr addrspace(1), ptr %17
  store ptr addrspace(1) %73, ptr %7
  %74 = load ptr addrspace(1), ptr %7
  %75 = load i64, ptr %ds
  %76 = load i64, ptr %alloc
  %77 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %75, 0, 0
  %78 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %77, i64 %76, 0, 1
  %79 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %78, ptr addrspace(1) %74, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %79
L299:
  %80 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %80, ptr %7
  %81 = load ptr addrspace(1), ptr %7
  %82 = load i64, ptr %ds
  %83 = load i64, ptr %alloc
  %84 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %82, 0, 0
  %85 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %84, i64 %83, 0, 1
  %86 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %85, ptr addrspace(1) %81, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %86
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_twice_varying_roots_14_15_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:                                ; %L304
	cmp	x3, #1
	csel	x9, x0, x1, eq
	cmp	x8, x27
	b.hs	LBB0_4
LBB0_2:                                 ; %L307
	cmp	x3, #3
	csel	x0, x9, x2, eq
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_3:                                 ; %L303
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	ldr	x8, [x28]
	cmp	x3, #1
	csel	x9, x0, x1, eq
	cmp	x8, x27
	b.lo	LBB0_2
LBB0_4:                                 ; %L306
	str	x2, [sp, #24]
	str	x9, [sp, #16]
	bl	_caml_call_gc
Ltmp1:
	ldr	x2, [sp, #24]
	ldr	x9, [sp, #16]
	cmp	x3, #3
	csel	x0, x9, x2, eq
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
	.cfi_endproc|}]

let alloc_triple_live_roots x y z =
  (x, y, z)
;;

[%%expect{|
val alloc_triple_live_roots : 'a -> 'b -> 'c -> 'a * 'b * 'c = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_triple_live_roots_16_17_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca ptr addrspace(1)
  %10 = alloca ptr addrspace(1)
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  %13 = alloca i64
  %14 = alloca ptr addrspace(1)
  %15 = alloca ptr addrspace(1)
  %16 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L319
L319:
  %17 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %17, ptr %9
  %18 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %18, ptr %10
  %19 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %19, ptr %11
  %20 = load i64, ptr %alloc
  %21 = sub i64 %20, 32
  store i64 %21, ptr %alloc
  %22 = load i64, ptr %ds
  %23 = inttoptr i64 %22 to ptr
  %24 = load i64, ptr %23
  %25 = icmp ule i64 %24, %21
  %26 = call  i1 @llvm.expect.i1(i1 %25, i1 1)
  br i1 %26, label %L323, label %L322
L322:
  %27 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %27, ptr %14
  %28 = load ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %28, ptr %15
  %29 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %29, ptr %16
  %30 = load i64, ptr %ds
  %31 = load i64, ptr %alloc
  %32 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %30, i64 %31) "statepoint-id"="262145" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 4, i64 1, i64 2, i64 0, i64 2, i64 11, i64 0, i64 11, i64 0, i64 29, i64 5263188, i64 3026993, i64 7105633, i64 6251375, i64 6910580, i64 6646896, i64 6909023, i64 6251894, i64 7303026, i64 29556), "gc-live"(ptr %14, ptr %15, ptr %16) ]
  %33 = extractvalue { { i64, i64 }, {  } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, {  } } %32, 0, 1
  store i64 %33, ptr %ds
  store i64 %34, ptr %alloc
  %35 = load volatile ptr addrspace(1), ptr %14
  store ptr addrspace(1) %35, ptr %9
  %36 = load volatile ptr addrspace(1), ptr %15
  store ptr addrspace(1) %36, ptr %10
  %37 = load volatile ptr addrspace(1), ptr %16
  store ptr addrspace(1) %37, ptr %11
  br label %L323
L323:
  %38 = load i64, ptr %alloc
  %39 = add i64 %38, 8
  %40 = inttoptr i64 %39 to ptr addrspace(1)
  store ptr addrspace(1) %40, ptr %12
  %41 = load ptr addrspace(1), ptr %12
  %42 = ptrtoint ptr addrspace(1) %41 to i64
  %43 = add i64 %42, -8
  %44 = inttoptr i64 %43 to ptr
  store volatile i64 3072, ptr %44
  %45 = load ptr addrspace(1), ptr %12
  %46 = addrspacecast ptr addrspace(1) %45 to ptr
  %47 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %47, ptr %46
  %48 = load ptr addrspace(1), ptr %12
  %49 = ptrtoint ptr addrspace(1) %48 to i64
  %50 = add i64 %49, 8
  %51 = inttoptr i64 %50 to ptr
  %52 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %52, ptr %51
  %53 = load ptr addrspace(1), ptr %12
  %54 = ptrtoint ptr addrspace(1) %53 to i64
  %55 = add i64 %54, 16
  %56 = inttoptr i64 %55 to ptr
  %57 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %57, ptr %56
  %58 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %58, ptr %6
  %59 = load ptr addrspace(1), ptr %6
  %60 = load i64, ptr %ds
  %61 = load i64, ptr %alloc
  %62 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %60, 0, 0
  %63 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %62, i64 %61, 0, 1
  %64 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %63, ptr addrspace(1) %59, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %64
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__alloc_triple_live_roots_16_17_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	sub	x27, x27, #32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_2
LBB0_1:                                 ; %L323
	mov	w8, #3072
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	stp	x1, x2, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_2:                                 ; %L322
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	b	LBB0_1
	.cfi_endproc|}]

let call_with_live_roots x y f =
  f x;
  y
;;

[%%expect{|
val call_with_live_roots : 'a -> 'b -> ('a -> 'c) -> 'b = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__call_with_live_roots_18_19_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca ptr addrspace(1)
  %10 = alloca ptr addrspace(1)
  %11 = alloca ptr addrspace(1)
  %12 = alloca i64
  %13 = alloca ptr addrspace(1)
  %14 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L334
L334:
  %15 = load i64, ptr %ds
  %16 = add i64 %15, 40
  %17 = inttoptr i64 %16 to ptr
  %18 = load i64, ptr %17
  %19 = add i64 %18, 376
  %20 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %21 = icmp uge i64 %20, %19
  %22 = call  i1 @llvm.expect.i1(i1 %21, i1 1)
  br i1 %22, label %L339, label %L338
L338:
  %23 = load i64, ptr %ds
  %24 = load i64, ptr %alloc
  %25 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %23, i64 %24, i64 34) "gc-leaf-function"="true" cold
  %26 = extractvalue { { i64, i64 }, {  } } %25, 0, 0
  %27 = extractvalue { { i64, i64 }, {  } } %25, 0, 1
  store i64 %26, ptr %ds
  store i64 %27, ptr %alloc
  br label %L339
L339:
  %28 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %28, ptr %9
  %29 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %29, ptr %10
  %30 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %30, ptr %11
  %31 = load ptr addrspace(1), ptr %11
  %32 = addrspacecast ptr addrspace(1) %31 to ptr
  %33 = load i64, ptr %32
  store i64 %33, ptr %12
  %34 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %34, ptr %6
  %35 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %35, ptr %7
  %36 = load ptr addrspace(1), ptr %6
  %37 = load ptr addrspace(1), ptr %7
  %38 = load i64, ptr %ds
  %39 = load i64, ptr %alloc
  %40 = load ptr, ptr %12
  %41 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } %40(i64 %38, i64 %39, ptr addrspace(1) %36, ptr addrspace(1) %37) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 2, i64 0, i64 2, i64 5, i64 0, i64 5, i64 0, i64 26, i64 5263188, i64 3027249, i64 7102819, i64 7823212, i64 6845545, i64 6909023, i64 6251894, i64 7303026, i64 29556), "gc-live"(ptr %10) ]
  %42 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %41, 0, 0
  %43 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %41, 0, 1
  store i64 %42, ptr %ds
  store i64 %43, ptr %alloc
  %44 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %41, 1, 0
  store ptr addrspace(1) %44, ptr %6
  br label %L336
L336:
  %45 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %45, ptr %13
  %46 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %46, ptr %14
  %47 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %47, ptr %6
  %48 = load ptr addrspace(1), ptr %6
  %49 = load i64, ptr %ds
  %50 = load i64, ptr %alloc
  %51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %49, 0, 0
  %52 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %51, i64 %50, 0, 1
  %53 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %52, ptr addrspace(1) %48, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %53
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__call_with_live_roots_18_19_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	mov	x9, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB0_2
LBB0_1:                                 ; %L339
	str	x1, [sp, #8]
	ldr	x8, [x2]
	mov	x0, x9
	mov	x1, x2
	blr	x8
Ltmp0:
	ldr	x0, [sp, #8]
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L338
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
	b	LBB0_1
	.cfi_endproc|}]

let alloc_no_roots n =
  (n, n + 1)
;;

[%%expect{|
val alloc_no_roots : int -> int * int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_no_roots_20_21_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca i64
  store i64 %2, ptr %4
  %5 = alloca ptr addrspace(1)
  %6 = alloca i64
  %7 = alloca ptr addrspace(1)
  %8 = alloca i64
  %9 = alloca i64
  br label %L1
L1:
  br label %L350
L350:
  %10 = load i64, ptr %4
  store i64 %10, ptr %6
  %11 = load i64, ptr %alloc
  %12 = sub i64 %11, 24
  store i64 %12, ptr %alloc
  %13 = load i64, ptr %ds
  %14 = inttoptr i64 %13 to ptr
  %15 = load i64, ptr %14
  %16 = icmp ule i64 %15, %12
  %17 = call  i1 @llvm.expect.i1(i1 %16, i1 1)
  br i1 %17, label %L355, label %L354
L354:
  %18 = load i64, ptr %ds
  %19 = load i64, ptr %alloc
  %20 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %18, i64 %19) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 2, i64 12, i64 0, i64 12, i64 0, i64 20, i64 5263188, i64 3027505, i64 7105633, i64 6251375, i64 6254446, i64 7303026, i64 29556) ]
  %21 = extractvalue { { i64, i64 }, {  } } %20, 0, 0
  %22 = extractvalue { { i64, i64 }, {  } } %20, 0, 1
  store i64 %21, ptr %ds
  store i64 %22, ptr %alloc
  br label %L355
L355:
  %23 = load i64, ptr %alloc
  %24 = add i64 %23, 8
  %25 = inttoptr i64 %24 to ptr addrspace(1)
  store ptr addrspace(1) %25, ptr %7
  %26 = load ptr addrspace(1), ptr %7
  %27 = ptrtoint ptr addrspace(1) %26 to i64
  %28 = add i64 %27, -8
  %29 = inttoptr i64 %28 to ptr
  store volatile i64 2048, ptr %29
  %30 = load ptr addrspace(1), ptr %7
  %31 = addrspacecast ptr addrspace(1) %30 to ptr
  %32 = load i64, ptr %6
  store volatile i64 %32, ptr %31
  %33 = load i64, ptr %6
  %34 = add i64 %33, 2
  store i64 %34, ptr %9
  %35 = load ptr addrspace(1), ptr %7
  %36 = ptrtoint ptr addrspace(1) %35 to i64
  %37 = add i64 %36, 8
  %38 = inttoptr i64 %37 to ptr
  %39 = load i64, ptr %9
  store volatile i64 %39, ptr %38
  %40 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %40, ptr %5
  %41 = load ptr addrspace(1), ptr %5
  %42 = load i64, ptr %ds
  %43 = load i64, ptr %alloc
  %44 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %42, 0, 0
  %45 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %44, i64 %43, 0, 1
  %46 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %45, ptr addrspace(1) %41, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %46
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_no_roots_20_21_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_2
LBB0_1:                                 ; %L355
	mov	w8, #2048
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	add	x9, x0, #2
	str	x9, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB0_2:                                 ; %L354
	bl	_caml_call_gc
Ltmp0:
	b	LBB0_1
	.cfi_endproc|}]

let alloc_const_int_filter x =
  let n = 42 in
  (x, n)
;;

[%%expect{|
val alloc_const_int_filter : 'a -> 'a * int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_const_int_filter_22_23_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca ptr addrspace(1)
  %6 = alloca ptr addrspace(1)
  %7 = alloca i64
  %8 = alloca i64
  %9 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L366
L366:
  %10 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %10, ptr %5
  %11 = load i64, ptr %alloc
  %12 = sub i64 %11, 24
  store i64 %12, ptr %alloc
  %13 = load i64, ptr %ds
  %14 = inttoptr i64 %13 to ptr
  %15 = load i64, ptr %14
  %16 = icmp ule i64 %15, %12
  %17 = call  i1 @llvm.expect.i1(i1 %16, i1 1)
  br i1 %17, label %L370, label %L369
L369:
  %18 = load ptr addrspace(1), ptr %5
  store volatile ptr addrspace(1) %18, ptr %9
  %19 = load i64, ptr %ds
  %20 = load i64, ptr %alloc
  %21 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %19, i64 %20) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 3, i64 0, i64 2, i64 8, i64 0, i64 8, i64 0, i64 28, i64 5263188, i64 3027761, i64 7105633, i64 6251375, i64 7237475, i64 6255731, i64 7630441, i64 6907487, i64 6648940, i64 114), "gc-live"(ptr %9) ]
  %22 = extractvalue { { i64, i64 }, {  } } %21, 0, 0
  %23 = extractvalue { { i64, i64 }, {  } } %21, 0, 1
  store i64 %22, ptr %ds
  store i64 %23, ptr %alloc
  %24 = load volatile ptr addrspace(1), ptr %9
  store ptr addrspace(1) %24, ptr %5
  br label %L370
L370:
  %25 = load i64, ptr %alloc
  %26 = add i64 %25, 8
  %27 = inttoptr i64 %26 to ptr addrspace(1)
  store ptr addrspace(1) %27, ptr %6
  %28 = load ptr addrspace(1), ptr %6
  %29 = ptrtoint ptr addrspace(1) %28 to i64
  %30 = add i64 %29, -8
  %31 = inttoptr i64 %30 to ptr
  store volatile i64 2048, ptr %31
  %32 = load ptr addrspace(1), ptr %6
  %33 = addrspacecast ptr addrspace(1) %32 to ptr
  %34 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %34, ptr %33
  %35 = load ptr addrspace(1), ptr %6
  %36 = ptrtoint ptr addrspace(1) %35 to i64
  %37 = add i64 %36, 8
  %38 = inttoptr i64 %37 to ptr
  store volatile i64 85, ptr %38
  %39 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %39, ptr %4
  %40 = load ptr addrspace(1), ptr %4
  %41 = load i64, ptr %ds
  %42 = load i64, ptr %alloc
  %43 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %41, 0, 0
  %44 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %43, i64 %42, 0, 1
  %45 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %44, ptr addrspace(1) %40, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %45
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_const_int_filter_22_23_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_2
LBB0_1:                                 ; %L370
	mov	w8, #2048
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	mov	w9, #85
	str	x9, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L369
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	b	LBB0_1
	.cfi_endproc|}]

let alloc_under_trap x y f =
  try
    let pair = x, y in
    f pair
  with _ -> x
;;

[%%expect{|
val alloc_under_trap : 'a -> 'b -> ('a * 'b -> 'a) -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_under_trap_24_25_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="48" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca i64
  %10 = alloca ptr addrspace(1)
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  %13 = alloca i64
  %14 = alloca i64
  %15 = alloca ptr addrspace(1)
  %16 = alloca ptr addrspace(1)
  %17 = alloca i64
  %18 = alloca ptr addrspace(1)
  %19 = alloca i64
  %20 = alloca ptr addrspace(1)
  %21 = alloca ptr addrspace(1)
  %22 = alloca i64
  %23 = alloca i64
  %24 = alloca i64
  br label %L1
L1:
  br label %L381
L381:
  %25 = load i64, ptr %ds
  %26 = add i64 %25, 40
  %27 = inttoptr i64 %26 to ptr
  %28 = load i64, ptr %27
  %29 = add i64 %28, 408
  %30 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %31 = icmp uge i64 %30, %29
  %32 = call  i1 @llvm.expect.i1(i1 %31, i1 1)
  br i1 %32, label %L401, label %L400
L400:
  %33 = load i64, ptr %ds
  %34 = load i64, ptr %alloc
  %35 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %33, i64 %34, i64 38) "gc-leaf-function"="true" cold
  %36 = extractvalue { { i64, i64 }, {  } } %35, 0, 0
  %37 = extractvalue { { i64, i64 }, {  } } %35, 0, 1
  store i64 %36, ptr %ds
  store i64 %37, ptr %alloc
  br label %L401
L401:
  %38 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %38, ptr %10
  %39 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %39, ptr %11
  %40 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %40, ptr %12
  %41 = load i64, ptr %ds
  %42 = add i64 %41, 64
  %43 = inttoptr i64 %42 to ptr
  %44 = load i64, ptr %43
  store i64 %44, ptr %13
  %45 = load i64, ptr %13
  store volatile i64 %45, ptr %14
  call  void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTOP__alloc_under_trap_24_25_code", %L404))
  br label %L402
L403:
  %46 = landingpad token cleanup
  br label %L404
L404:
  %47 = call  { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %48 = extractvalue { i64, i64, i64, i64 } %47, 0
  %49 = extractvalue { i64, i64, i64, i64 } %47, 1
  %50 = extractvalue { i64, i64, i64, i64 } %47, 2
  %51 = extractvalue { i64, i64, i64, i64 } %47, 3
  store i64 %48, ptr %24
  store i64 %51, ptr %ds
  store i64 %50, ptr %alloc
  call void asm sideeffect "mov x26, $0", "r"(i64 %49) "gc-leaf-function"="true"
  br label %L384
L402:
  %52 = load i64, ptr %alloc
  %53 = sub i64 %52, 24
  store i64 %53, ptr %alloc
  %54 = load i64, ptr %ds
  %55 = inttoptr i64 %54 to ptr
  %56 = load i64, ptr %55
  %57 = icmp ule i64 %56, %53
  %58 = call  i1 @llvm.expect.i1(i1 %57, i1 1)
  br i1 %58, label %L406, label %L405
L405:
  %59 = load i64, ptr %ds
  %60 = load i64, ptr %alloc
  %61 = invoke oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %59, i64 %60) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 3, i64 0, i64 15, i64 19, i64 0, i64 19, i64 0, i64 22, i64 5263188, i64 3028017, i64 7105633, i64 6251375, i64 6581877, i64 6255205, i64 6386292, i64 112), "gc-live"(ptr %10, ptr %11, ptr %12) ] to label %L407 unwind label %L403
L407:
  %62 = extractvalue { { i64, i64 }, {  } } %61, 0, 0
  %63 = extractvalue { { i64, i64 }, {  } } %61, 0, 1
  store i64 %62, ptr %ds
  store i64 %63, ptr %alloc
  br label %L406
L406:
  %64 = load i64, ptr %alloc
  %65 = add i64 %64, 8
  %66 = inttoptr i64 %65 to ptr addrspace(1)
  store ptr addrspace(1) %66, ptr %16
  %67 = load ptr addrspace(1), ptr %16
  %68 = ptrtoint ptr addrspace(1) %67 to i64
  %69 = add i64 %68, -8
  %70 = inttoptr i64 %69 to ptr
  store volatile i64 2048, ptr %70
  %71 = load ptr addrspace(1), ptr %16
  %72 = addrspacecast ptr addrspace(1) %71 to ptr
  %73 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %73, ptr %72
  %74 = load ptr addrspace(1), ptr %16
  %75 = ptrtoint ptr addrspace(1) %74 to i64
  %76 = add i64 %75, 8
  %77 = inttoptr i64 %76 to ptr
  %78 = load volatile ptr addrspace(1), ptr %11
  store ptr addrspace(1) %78, ptr %77
  %79 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %79, ptr %18
  %80 = load volatile ptr addrspace(1), ptr %12
  %81 = addrspacecast ptr addrspace(1) %80 to ptr
  %82 = load i64, ptr %81
  store i64 %82, ptr %19
  %83 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %83, ptr %6
  %84 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %84, ptr %7
  %85 = load ptr addrspace(1), ptr %6
  %86 = load ptr addrspace(1), ptr %7
  %87 = load i64, ptr %ds
  %88 = load i64, ptr %alloc
  %89 = load ptr, ptr %19
  %90 = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } %89(i64 %87, i64 %88, ptr addrspace(1) %85, ptr addrspace(1) %86) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 4, i64 0, i64 4, i64 10, i64 0, i64 10, i64 0, i64 22, i64 5263188, i64 3028017, i64 7105633, i64 6251375, i64 6581877, i64 6255205, i64 6386292, i64 112), "gc-live"(ptr %10) ] to label %L408 unwind label %L403
L408:
  %91 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %90, 0, 0
  %92 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %90, 0, 1
  store i64 %91, ptr %ds
  store i64 %92, ptr %alloc
  %93 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %90, 1, 0
  store ptr addrspace(1) %93, ptr %6
  br label %L393
L393:
  %94 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %94, ptr %20
  %95 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %95, ptr %21
  call  void @llvm.aarch64.oxcaml.pop.trap()
  %96 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %96, ptr %6
  %97 = load ptr addrspace(1), ptr %6
  %98 = load i64, ptr %ds
  %99 = load i64, ptr %alloc
  %100 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %98, 0, 0
  %101 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %100, i64 %99, 0, 1
  %102 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %101, ptr addrspace(1) %97, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %102
L384:
  %103 = load i64, ptr %24
  store i64 %103, ptr %9
  %104 = load i64, ptr %9
  %105 = inttoptr i64 %104 to ptr addrspace(1)
  store ptr addrspace(1) %105, ptr %15
  %106 = load i64, ptr %ds
  %107 = add i64 %106, 64
  %108 = inttoptr i64 %107 to ptr
  %109 = load volatile i64, ptr %14
  store i64 %109, ptr %108
  store i64 1, ptr %23
  %110 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %110, ptr %6
  %111 = load ptr addrspace(1), ptr %6
  %112 = load i64, ptr %ds
  %113 = load i64, ptr %alloc
  %114 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %112, 0, 0
  %115 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %114, i64 %113, 0, 1
  %116 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %115, ptr addrspace(1) %111, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %116
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_under_trap_24_25_code:
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception0
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	mov	x9, x0
	mov	x8, x28
	ldr	x10, [x28, #40]
	add	x10, x10, #408
	; InlineAsm Start
	mov	x11, sp
	; InlineAsm End
	cmp	x11, x10
	b.lo	LBB0_4
LBB0_1:                                 ; %L401
	str	x9, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	ldr	x9, [x8, #64]
	str	x9, [sp]
	adr	x16, LBB0_7
	stp	x26, x16, [sp, #-16]!
	mov	x26, sp
	sub	x27, x27, #24
	ldr	x9, [x8]
	cmp	x9, x27
	b.hi	LBB0_5
LBB0_2:                                 ; %L406
	mov	w9, #2048
	str	x9, [x27]
	ldr	x9, [sp, #40]
	mov	x0, x27
	str	x9, [x0, #8]!
	ldr	x9, [sp, #32]
	str	x9, [x27, #16]
	ldr	x9, [sp, #24]
	ldr	x9, [x9]
	ldr	x1, [sp, #24]
	mov	x28, x8
	blr	x9
Ltmp1:
; %bb.3:                                ; %L408
	ldr	x26, [sp], #16
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_4:                                 ; %L400
	mov	x28, x8
	mov	w0, #38
	bl	_caml_llvm_call_realloc_stack
	mov	x8, x28
	b	LBB0_1
LBB0_5:                                 ; %L405
	mov	x28, x8
	bl	_caml_call_gc
Ltmp2:
; %bb.6:                                ; %L407
	mov	x8, x28
	b	LBB0_2
Ltmp3:                                  ; Block address taken
LBB0_7:                                 ; %L404
                                        ; Label of block must be emitted
                                        ; implicit-def: $q0
                                        ; implicit-def: $q1
                                        ; implicit-def: $q2
                                        ; implicit-def: $q3
                                        ; implicit-def: $q4
                                        ; implicit-def: $q5
                                        ; implicit-def: $q6
                                        ; implicit-def: $q7
                                        ; implicit-def: $q8
                                        ; implicit-def: $q9
                                        ; implicit-def: $q10
                                        ; implicit-def: $q11
                                        ; implicit-def: $q12
                                        ; implicit-def: $q13
                                        ; implicit-def: $q14
                                        ; implicit-def: $q15
                                        ; implicit-def: $q16
                                        ; implicit-def: $q17
                                        ; implicit-def: $q18
                                        ; implicit-def: $q19
                                        ; implicit-def: $q20
                                        ; implicit-def: $q21
                                        ; implicit-def: $q22
                                        ; implicit-def: $q23
                                        ; implicit-def: $q24
                                        ; implicit-def: $q25
                                        ; implicit-def: $q26
                                        ; implicit-def: $q27
                                        ; implicit-def: $q28
                                        ; implicit-def: $q29
                                        ; implicit-def: $q30
                                        ; implicit-def: $q31
                                        ; implicit-def: $x1
                                        ; implicit-def: $x2
                                        ; implicit-def: $x3
                                        ; implicit-def: $x4
                                        ; implicit-def: $x5
                                        ; implicit-def: $x6
                                        ; implicit-def: $x7
                                        ; implicit-def: $x8
                                        ; implicit-def: $x9
                                        ; implicit-def: $x10
                                        ; implicit-def: $x11
                                        ; implicit-def: $x12
                                        ; implicit-def: $x13
                                        ; implicit-def: $x14
                                        ; implicit-def: $x19
                                        ; implicit-def: $x20
                                        ; implicit-def: $x21
                                        ; implicit-def: $x22
                                        ; implicit-def: $x23
                                        ; implicit-def: $x24
                                        ; implicit-def: $x25
	; InlineAsm Start
	mov	x26, x26
	; InlineAsm End
	ldr	x8, [sp]
	str	x8, [x28, #64]
	ldr	x0, [sp, #24]
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
Lfunc_end0:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table0:
Lexception0:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end0-Lcst_begin0
Lcst_begin0:
	.uleb128 Lfunc_begin0-Lfunc_begin0      ; >> Call Site 1 <<
	.uleb128 Lfunc_end0-Lfunc_begin0        ;   Call between Lfunc_begin0 and Lfunc_end0
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end0:
	.p2align	2, 0x0|}]

let call_then_alloc_with_live_roots x y f =
  let pair = x, y in
  f x;
  pair
;;

[%%expect{|
val call_then_alloc_with_live_roots : 'a -> 'b -> ('a -> 'c) -> 'a * 'b =
  <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__call_then_alloc_with_live_roots_26_27_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-before-bytes"="0" "oxcaml-stack-check-bytes"="16" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca ptr addrspace(1)
  %10 = alloca ptr addrspace(1)
  %11 = alloca ptr addrspace(1)
  %12 = alloca i64
  %13 = alloca ptr addrspace(1)
  %14 = alloca ptr addrspace(1)
  %15 = alloca ptr addrspace(1)
  %16 = alloca i64
  %17 = alloca ptr addrspace(1)
  %18 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L419
L419:
  %19 = load i64, ptr %ds
  %20 = add i64 %19, 40
  %21 = inttoptr i64 %20 to ptr
  %22 = load i64, ptr %21
  %23 = add i64 %22, 376
  %24 = call i64 asm sideeffect "mov $0, sp", "=r"() "gc-leaf-function"="true"
  %25 = icmp uge i64 %24, %23
  %26 = call  i1 @llvm.expect.i1(i1 %25, i1 1)
  br i1 %26, label %L425, label %L424
L424:
  %27 = load i64, ptr %ds
  %28 = load i64, ptr %alloc
  %29 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_llvm_call_realloc_stack"(i64 %27, i64 %28, i64 34) "gc-leaf-function"="true" cold
  %30 = extractvalue { { i64, i64 }, {  } } %29, 0, 0
  %31 = extractvalue { { i64, i64 }, {  } } %29, 0, 1
  store i64 %30, ptr %ds
  store i64 %31, ptr %alloc
  br label %L425
L425:
  %32 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %32, ptr %9
  %33 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %33, ptr %10
  %34 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %34, ptr %11
  %35 = load ptr addrspace(1), ptr %11
  %36 = addrspacecast ptr addrspace(1) %35 to ptr
  %37 = load i64, ptr %36
  store i64 %37, ptr %12
  %38 = load volatile ptr addrspace(1), ptr %9
  store ptr addrspace(1) %38, ptr %6
  %39 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %39, ptr %7
  %40 = load ptr addrspace(1), ptr %6
  %41 = load ptr addrspace(1), ptr %7
  %42 = load i64, ptr %ds
  %43 = load i64, ptr %alloc
  %44 = load ptr, ptr %12
  %45 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } %44(i64 %42, i64 %43, ptr addrspace(1) %40, ptr addrspace(1) %41) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 3, i64 0, i64 2, i64 5, i64 0, i64 5, i64 0, i64 37, i64 5263188, i64 3028273, i64 7102819, i64 7626604, i64 7234920, i64 7102815, i64 6516588, i64 6911839, i64 6252660, i64 7760236, i64 7495525, i64 7630703, i64 115), "gc-live"(ptr %9, ptr %10) ]
  %46 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %45, 0, 0
  %47 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %45, 0, 1
  store i64 %46, ptr %ds
  store i64 %47, ptr %alloc
  %48 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %45, 1, 0
  store ptr addrspace(1) %48, ptr %6
  br label %L421
L421:
  %49 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %49, ptr %13
  %50 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %50, ptr %14
  %51 = load i64, ptr %alloc
  %52 = sub i64 %51, 24
  store i64 %52, ptr %alloc
  %53 = load i64, ptr %ds
  %54 = inttoptr i64 %53 to ptr
  %55 = load i64, ptr %54
  %56 = icmp ule i64 %55, %52
  %57 = call  i1 @llvm.expect.i1(i1 %56, i1 1)
  br i1 %57, label %L427, label %L426
L426:
  %58 = load volatile ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %58, ptr %17
  %59 = load volatile ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %59, ptr %18
  %60 = load i64, ptr %ds
  %61 = load i64, ptr %alloc
  %62 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %60, i64 %61) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 13, i64 17, i64 0, i64 17, i64 0, i64 37, i64 5263188, i64 3028273, i64 7102819, i64 7626604, i64 7234920, i64 7102815, i64 6516588, i64 6911839, i64 6252660, i64 7760236, i64 7495525, i64 7630703, i64 115), "gc-live"(ptr %17, ptr %18) ]
  %63 = extractvalue { { i64, i64 }, {  } } %62, 0, 0
  %64 = extractvalue { { i64, i64 }, {  } } %62, 0, 1
  store i64 %63, ptr %ds
  store i64 %64, ptr %alloc
  %65 = load volatile ptr addrspace(1), ptr %17
  store volatile ptr addrspace(1) %65, ptr %9
  %66 = load volatile ptr addrspace(1), ptr %18
  store volatile ptr addrspace(1) %66, ptr %10
  br label %L427
L427:
  %67 = load i64, ptr %alloc
  %68 = add i64 %67, 8
  %69 = inttoptr i64 %68 to ptr addrspace(1)
  store ptr addrspace(1) %69, ptr %15
  %70 = load ptr addrspace(1), ptr %15
  %71 = ptrtoint ptr addrspace(1) %70 to i64
  %72 = add i64 %71, -8
  %73 = inttoptr i64 %72 to ptr
  store volatile i64 2048, ptr %73
  %74 = load ptr addrspace(1), ptr %15
  %75 = addrspacecast ptr addrspace(1) %74 to ptr
  %76 = load volatile ptr addrspace(1), ptr %9
  store ptr addrspace(1) %76, ptr %75
  %77 = load ptr addrspace(1), ptr %15
  %78 = ptrtoint ptr addrspace(1) %77 to i64
  %79 = add i64 %78, 8
  %80 = inttoptr i64 %79 to ptr
  %81 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %81, ptr %80
  %82 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %82, ptr %6
  %83 = load ptr addrspace(1), ptr %6
  %84 = load i64, ptr %ds
  %85 = load i64, ptr %alloc
  %86 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %84, 0, 0
  %87 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %86, i64 %85, 0, 1
  %88 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %87, ptr addrspace(1) %83, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %88
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__call_then_alloc_with_live_roots_26_27_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	mov	x9, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB0_3
LBB0_1:                                 ; %L425
	str	x9, [sp, #24]
	str	x1, [sp, #16]
	ldr	x8, [x2]
	ldr	x0, [sp, #24]
	mov	x1, x2
	blr	x8
Ltmp0:
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_4
LBB0_2:                                 ; %L427
	mov	w8, #2048
	str	x8, [x27]
	ldr	x8, [sp, #24]
	mov	x0, x27
	str	x8, [x0, #8]!
	ldr	x8, [sp, #16]
	str	x8, [x27, #16]
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_3:                                 ; %L424
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
	b	LBB0_1
LBB0_4:                                 ; %L426
	ldr	x8, [sp, #24]
	str	x8, [sp, #8]
	ldr	x8, [sp, #16]
	str	x8, [sp]
	bl	_caml_call_gc
Ltmp1:
	ldr	x8, [sp, #8]
	str	x8, [sp, #24]
	ldr	x8, [sp]
	str	x8, [sp, #16]
	b	LBB0_2
	.cfi_endproc|}]

let alloc_boxed_float_root (x : float) y =
  (x, y)
;;

[%%expect{|
val alloc_boxed_float_root : float -> 'a -> float * 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_boxed_float_root_28_29_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %10 = alloca i64
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L438
L438:
  %13 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %13, ptr %7
  %14 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %14, ptr %8
  %15 = load i64, ptr %alloc
  %16 = sub i64 %15, 24
  store i64 %16, ptr %alloc
  %17 = load i64, ptr %ds
  %18 = inttoptr i64 %17 to ptr
  %19 = load i64, ptr %18
  %20 = icmp ule i64 %19, %16
  %21 = call  i1 @llvm.expect.i1(i1 %20, i1 1)
  br i1 %21, label %L442, label %L441
L441:
  %22 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %22, ptr %11
  %23 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %23, ptr %12
  %24 = load i64, ptr %ds
  %25 = load i64, ptr %alloc
  %26 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %24, i64 %25) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 2, i64 8, i64 0, i64 8, i64 0, i64 28, i64 5263188, i64 3028529, i64 7105633, i64 6251375, i64 7892834, i64 6251621, i64 7302246, i64 6255713, i64 7303026, i64 116), "gc-live"(ptr %11, ptr %12) ]
  %27 = extractvalue { { i64, i64 }, {  } } %26, 0, 0
  %28 = extractvalue { { i64, i64 }, {  } } %26, 0, 1
  store i64 %27, ptr %ds
  store i64 %28, ptr %alloc
  %29 = load volatile ptr addrspace(1), ptr %11
  store ptr addrspace(1) %29, ptr %7
  %30 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %30, ptr %8
  br label %L442
L442:
  %31 = load i64, ptr %alloc
  %32 = add i64 %31, 8
  %33 = inttoptr i64 %32 to ptr addrspace(1)
  store ptr addrspace(1) %33, ptr %9
  %34 = load ptr addrspace(1), ptr %9
  %35 = ptrtoint ptr addrspace(1) %34 to i64
  %36 = add i64 %35, -8
  %37 = inttoptr i64 %36 to ptr
  store volatile i64 2048, ptr %37
  %38 = load ptr addrspace(1), ptr %9
  %39 = addrspacecast ptr addrspace(1) %38 to ptr
  %40 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %40, ptr %39
  %41 = load ptr addrspace(1), ptr %9
  %42 = ptrtoint ptr addrspace(1) %41 to i64
  %43 = add i64 %42, 8
  %44 = inttoptr i64 %43 to ptr
  %45 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %45, ptr %44
  %46 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %46, ptr %5
  %47 = load ptr addrspace(1), ptr %5
  %48 = load i64, ptr %ds
  %49 = load i64, ptr %alloc
  %50 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %48, 0, 0
  %51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %50, i64 %49, 0, 1
  %52 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %51, ptr addrspace(1) %47, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %52
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_boxed_float_root_28_29_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_2
LBB0_1:                                 ; %L442
	mov	w8, #2048
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	str	x1, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L441
	str	x0, [sp, #8]
	str	x1, [sp]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	ldr	x1, [sp]
	b	LBB0_1
	.cfi_endproc|}]

let alloc_aliased_roots x n =
  let y = Sys.opaque_identity x in
  (x, y, n)
;;

[%%expect{|
val alloc_aliased_roots : 'a -> 'b -> 'a * 'a * 'b = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_aliased_roots_30_31_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
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
  %10 = alloca ptr addrspace(1)
  %11 = alloca i64
  %12 = alloca ptr addrspace(1)
  %13 = alloca ptr addrspace(1)
  %14 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L453
L453:
  %15 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %15, ptr %7
  %16 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %16, ptr %8
  %17 = load ptr addrspace(1), ptr %7
  %18 = call ptr addrspace(1) asm  "", "=r,0"(ptr addrspace(1) %17) "gc-leaf-function"="true"
  store ptr addrspace(1) %18, ptr %7
  %19 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %19, ptr %9
  %20 = load i64, ptr %alloc
  %21 = sub i64 %20, 32
  store i64 %21, ptr %alloc
  %22 = load i64, ptr %ds
  %23 = inttoptr i64 %22 to ptr
  %24 = load i64, ptr %23
  %25 = icmp ule i64 %24, %21
  %26 = call  i1 @llvm.expect.i1(i1 %25, i1 1)
  br i1 %26, label %L457, label %L456
L456:
  %27 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %27, ptr %12
  %28 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %28, ptr %13
  %29 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %29, ptr %14
  %30 = load i64, ptr %ds
  %31 = load i64, ptr %alloc
  %32 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %30, i64 %31) "statepoint-id"="262145" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 4, i64 1, i64 3, i64 0, i64 2, i64 11, i64 0, i64 11, i64 0, i64 25, i64 5263188, i64 3028785, i64 7105633, i64 6251375, i64 6909025, i64 6648673, i64 7495524, i64 7630703, i64 115), "gc-live"(ptr %12, ptr %13, ptr %14) ]
  %33 = extractvalue { { i64, i64 }, {  } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, {  } } %32, 0, 1
  store i64 %33, ptr %ds
  store i64 %34, ptr %alloc
  %35 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %35, ptr %7
  %36 = load volatile ptr addrspace(1), ptr %13
  store ptr addrspace(1) %36, ptr %8
  %37 = load volatile ptr addrspace(1), ptr %14
  store ptr addrspace(1) %37, ptr %9
  br label %L457
L457:
  %38 = load i64, ptr %alloc
  %39 = add i64 %38, 8
  %40 = inttoptr i64 %39 to ptr addrspace(1)
  store ptr addrspace(1) %40, ptr %10
  %41 = load ptr addrspace(1), ptr %10
  %42 = ptrtoint ptr addrspace(1) %41 to i64
  %43 = add i64 %42, -8
  %44 = inttoptr i64 %43 to ptr
  store volatile i64 3072, ptr %44
  %45 = load ptr addrspace(1), ptr %10
  %46 = addrspacecast ptr addrspace(1) %45 to ptr
  %47 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %47, ptr %46
  %48 = load ptr addrspace(1), ptr %10
  %49 = ptrtoint ptr addrspace(1) %48 to i64
  %50 = add i64 %49, 8
  %51 = inttoptr i64 %50 to ptr
  %52 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %52, ptr %51
  %53 = load ptr addrspace(1), ptr %10
  %54 = ptrtoint ptr addrspace(1) %53 to i64
  %55 = add i64 %54, 16
  %56 = inttoptr i64 %55 to ptr
  %57 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %57, ptr %56
  %58 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %58, ptr %5
  %59 = load ptr addrspace(1), ptr %5
  %60 = load i64, ptr %ds
  %61 = load i64, ptr %alloc
  %62 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %60, 0, 0
  %63 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %62, i64 %61, 0, 1
  %64 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %63, ptr addrspace(1) %59, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %64
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_aliased_roots_30_31_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	; InlineAsm Start
	; InlineAsm End
	sub	x27, x27, #32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_3
; %bb.1:
	mov	x9, x0
LBB0_2:                                 ; %L457
	mov	w8, #3072
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	stp	x9, x1, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_3:                                 ; %L456
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x9, [sp, #8]
	b	LBB0_2
	.cfi_endproc|}]

let alloc_twice_varying_roots x y z n =
  let first = if n = 0 then (x, y) else (y, z) in
  (first, z)
;;

[%%expect{|
val alloc_twice_varying_roots : 'a -> 'a -> 'a -> int -> ('a * 'a) * 'a =
  <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_twice_varying_roots_32_33_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4, i64 %5) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %8
  %9 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %9
  %10 = alloca i64
  store i64 %5, ptr %10
  %11 = alloca ptr addrspace(1)
  %12 = alloca ptr addrspace(1)
  %13 = alloca ptr addrspace(1)
  %14 = alloca i64
  %15 = alloca ptr addrspace(1)
  %16 = alloca ptr addrspace(1)
  %17 = alloca i64
  %18 = alloca ptr addrspace(1)
  %19 = alloca ptr addrspace(1)
  %20 = alloca ptr addrspace(1)
  %21 = alloca i64
  %22 = alloca ptr addrspace(1)
  %23 = alloca ptr addrspace(1)
  %24 = alloca ptr addrspace(1)
  %25 = alloca i64
  %26 = alloca ptr addrspace(1)
  %27 = alloca ptr addrspace(1)
  %28 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L468
L468:
  %29 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %29, ptr %11
  %30 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %30, ptr %12
  %31 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %31, ptr %13
  %32 = load i64, ptr %10
  store i64 %32, ptr %14
  %33 = load i64, ptr %14
  %34 = icmp slt i64 %33, 1
  br i1 %34, label %L476, label %L484
L484:
  %35 = load i64, ptr %14
  %36 = icmp sgt i64 %35, 1
  br i1 %36, label %L476, label %L473
L473:
  %37 = load i64, ptr %alloc
  %38 = sub i64 %37, 24
  store i64 %38, ptr %alloc
  %39 = load i64, ptr %ds
  %40 = inttoptr i64 %39 to ptr
  %41 = load i64, ptr %40
  %42 = icmp ule i64 %41, %38
  %43 = call  i1 @llvm.expect.i1(i1 %42, i1 1)
  br i1 %43, label %L486, label %L485
L485:
  %44 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %44, ptr %26
  %45 = load ptr addrspace(1), ptr %12
  store volatile ptr addrspace(1) %45, ptr %27
  %46 = load ptr addrspace(1), ptr %13
  store volatile ptr addrspace(1) %46, ptr %28
  %47 = load i64, ptr %ds
  %48 = load i64, ptr %alloc
  %49 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %47, i64 %48) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 28, i64 34, i64 0, i64 34, i64 0, i64 31, i64 5263188, i64 3029041, i64 7105633, i64 6251375, i64 6911860, i64 6251875, i64 7496054, i64 7235961, i64 7495527, i64 7630703, i64 115), "gc-live"(ptr %26, ptr %27, ptr %28) ]
  %50 = extractvalue { { i64, i64 }, {  } } %49, 0, 0
  %51 = extractvalue { { i64, i64 }, {  } } %49, 0, 1
  store i64 %50, ptr %ds
  store i64 %51, ptr %alloc
  %52 = load volatile ptr addrspace(1), ptr %26
  store ptr addrspace(1) %52, ptr %11
  %53 = load volatile ptr addrspace(1), ptr %27
  store ptr addrspace(1) %53, ptr %12
  %54 = load volatile ptr addrspace(1), ptr %28
  store ptr addrspace(1) %54, ptr %13
  br label %L486
L486:
  %55 = load i64, ptr %alloc
  %56 = add i64 %55, 8
  %57 = inttoptr i64 %56 to ptr addrspace(1)
  store ptr addrspace(1) %57, ptr %16
  %58 = load ptr addrspace(1), ptr %16
  %59 = ptrtoint ptr addrspace(1) %58 to i64
  %60 = add i64 %59, -8
  %61 = inttoptr i64 %60 to ptr
  store volatile i64 2048, ptr %61
  %62 = load ptr addrspace(1), ptr %16
  %63 = addrspacecast ptr addrspace(1) %62 to ptr
  %64 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %64, ptr %63
  %65 = load ptr addrspace(1), ptr %16
  %66 = ptrtoint ptr addrspace(1) %65 to i64
  %67 = add i64 %66, 8
  %68 = inttoptr i64 %67 to ptr
  %69 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %69, ptr %68
  %70 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %70, ptr %18
  %71 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %71, ptr %19
  %72 = load ptr addrspace(1), ptr %19
  store ptr addrspace(1) %72, ptr %15
  br label %L480
L476:
  %73 = load i64, ptr %alloc
  %74 = sub i64 %73, 24
  store i64 %74, ptr %alloc
  %75 = load i64, ptr %ds
  %76 = inttoptr i64 %75 to ptr
  %77 = load i64, ptr %76
  %78 = icmp ule i64 %77, %74
  %79 = call  i1 @llvm.expect.i1(i1 %78, i1 1)
  br i1 %79, label %L488, label %L487
L487:
  %80 = load ptr addrspace(1), ptr %12
  store volatile ptr addrspace(1) %80, ptr %26
  %81 = load ptr addrspace(1), ptr %13
  store volatile ptr addrspace(1) %81, ptr %27
  %82 = load i64, ptr %ds
  %83 = load i64, ptr %alloc
  %84 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %82, i64 %83) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 40, i64 46, i64 0, i64 46, i64 0, i64 31, i64 5263188, i64 3029041, i64 7105633, i64 6251375, i64 6911860, i64 6251875, i64 7496054, i64 7235961, i64 7495527, i64 7630703, i64 115), "gc-live"(ptr %26, ptr %27) ]
  %85 = extractvalue { { i64, i64 }, {  } } %84, 0, 0
  %86 = extractvalue { { i64, i64 }, {  } } %84, 0, 1
  store i64 %85, ptr %ds
  store i64 %86, ptr %alloc
  %87 = load volatile ptr addrspace(1), ptr %26
  store ptr addrspace(1) %87, ptr %12
  %88 = load volatile ptr addrspace(1), ptr %27
  store ptr addrspace(1) %88, ptr %13
  br label %L488
L488:
  %89 = load i64, ptr %alloc
  %90 = add i64 %89, 8
  %91 = inttoptr i64 %90 to ptr addrspace(1)
  store ptr addrspace(1) %91, ptr %20
  %92 = load ptr addrspace(1), ptr %20
  %93 = ptrtoint ptr addrspace(1) %92 to i64
  %94 = add i64 %93, -8
  %95 = inttoptr i64 %94 to ptr
  store volatile i64 2048, ptr %95
  %96 = load ptr addrspace(1), ptr %20
  %97 = addrspacecast ptr addrspace(1) %96 to ptr
  %98 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %98, ptr %97
  %99 = load ptr addrspace(1), ptr %20
  %100 = ptrtoint ptr addrspace(1) %99 to i64
  %101 = add i64 %100, 8
  %102 = inttoptr i64 %101 to ptr
  %103 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %103, ptr %102
  %104 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %104, ptr %22
  %105 = load ptr addrspace(1), ptr %22
  store ptr addrspace(1) %105, ptr %23
  %106 = load ptr addrspace(1), ptr %23
  store ptr addrspace(1) %106, ptr %15
  br label %L480
L480:
  %107 = load i64, ptr %alloc
  %108 = sub i64 %107, 24
  store i64 %108, ptr %alloc
  %109 = load i64, ptr %ds
  %110 = inttoptr i64 %109 to ptr
  %111 = load i64, ptr %110
  %112 = icmp ule i64 %111, %108
  %113 = call  i1 @llvm.expect.i1(i1 %112, i1 1)
  br i1 %113, label %L490, label %L489
L489:
  %114 = load ptr addrspace(1), ptr %13
  store volatile ptr addrspace(1) %114, ptr %26
  %115 = load ptr addrspace(1), ptr %15
  store volatile ptr addrspace(1) %115, ptr %27
  %116 = load i64, ptr %ds
  %117 = load i64, ptr %alloc
  %118 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %116, i64 %117) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 3, i64 0, i64 2, i64 12, i64 0, i64 12, i64 0, i64 31, i64 5263188, i64 3029041, i64 7105633, i64 6251375, i64 6911860, i64 6251875, i64 7496054, i64 7235961, i64 7495527, i64 7630703, i64 115), "gc-live"(ptr %26, ptr %27) ]
  %119 = extractvalue { { i64, i64 }, {  } } %118, 0, 0
  %120 = extractvalue { { i64, i64 }, {  } } %118, 0, 1
  store i64 %119, ptr %ds
  store i64 %120, ptr %alloc
  %121 = load volatile ptr addrspace(1), ptr %26
  store ptr addrspace(1) %121, ptr %13
  %122 = load volatile ptr addrspace(1), ptr %27
  store ptr addrspace(1) %122, ptr %15
  br label %L490
L490:
  %123 = load i64, ptr %alloc
  %124 = add i64 %123, 8
  %125 = inttoptr i64 %124 to ptr addrspace(1)
  store ptr addrspace(1) %125, ptr %24
  %126 = load ptr addrspace(1), ptr %24
  %127 = ptrtoint ptr addrspace(1) %126 to i64
  %128 = add i64 %127, -8
  %129 = inttoptr i64 %128 to ptr
  store volatile i64 2048, ptr %129
  %130 = load ptr addrspace(1), ptr %24
  %131 = addrspacecast ptr addrspace(1) %130 to ptr
  %132 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %132, ptr %131
  %133 = load ptr addrspace(1), ptr %24
  %134 = ptrtoint ptr addrspace(1) %133 to i64
  %135 = add i64 %134, 8
  %136 = inttoptr i64 %135 to ptr
  %137 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %137, ptr %136
  %138 = load ptr addrspace(1), ptr %24
  store ptr addrspace(1) %138, ptr %7
  %139 = load ptr addrspace(1), ptr %7
  %140 = load i64, ptr %ds
  %141 = load i64, ptr %alloc
  %142 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %140, 0, 0
  %143 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %142, i64 %141, 0, 1
  %144 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %143, ptr addrspace(1) %139, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %144
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_twice_varying_roots_32_33_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x3, #1
	b.ne	LBB0_3
; %bb.1:                                ; %L473
	cmp	x8, x27
	b.ls	LBB0_5
; %bb.2:                                ; %L485
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	b	LBB0_5
LBB0_3:                                 ; %L476
	cmp	x8, x27
	b.hi	LBB0_8
; %bb.4:
	mov	x0, x1
	mov	x1, x2
LBB0_5:                                 ; %L480
	mov	x8, x27
	str	x0, [x8, #8]!
	mov	w9, #2048
	str	x9, [x27]
	str	x1, [x27, #16]
	sub	x27, x27, #24
	ldr	x9, [x28]
	cmp	x9, x27
	b.hi	LBB0_7
LBB0_6:                                 ; %L490
	mov	w9, #2048
	str	x9, [x27]
	mov	x0, x27
	str	x8, [x0, #8]!
	str	x2, [x27, #16]
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_7:                                 ; %L489
	str	x2, [sp, #24]
	str	x8, [sp, #16]
	bl	_caml_call_gc
Ltmp1:
	ldr	x2, [sp, #24]
	ldr	x8, [sp, #16]
	b	LBB0_6
LBB0_8:                                 ; %L487
	str	x1, [sp, #24]
	str	x2, [sp, #16]
	bl	_caml_call_gc
Ltmp2:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	mov	x2, x1
	b	LBB0_5
	.cfi_endproc|}]

let alloc_after_poll_reuses_slots x y n =
  poll ();
  (x, y, n)
;;

[%%expect{|
val alloc_after_poll_reuses_slots : 'a -> 'b -> 'c -> 'a * 'b * 'c = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_after_poll_reuses_slots_34_35_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %6 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %6
  %7 = alloca ptr addrspace(1)
  store ptr addrspace(1) %3, ptr %7
  %8 = alloca ptr addrspace(1)
  store ptr addrspace(1) %4, ptr %8
  %9 = alloca ptr addrspace(1)
  %10 = alloca ptr addrspace(1)
  %11 = alloca ptr addrspace(1)
  %12 = alloca i64
  %13 = alloca i64
  %14 = alloca ptr addrspace(1)
  %15 = alloca i64
  %16 = alloca ptr addrspace(1)
  %17 = alloca ptr addrspace(1)
  %18 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L501
L501:
  %19 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %19, ptr %9
  %20 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %20, ptr %10
  %21 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %21, ptr %11
  %22 = load i64, ptr %alloc
  %23 = load i64, ptr %ds
  %24 = inttoptr i64 %23 to ptr
  %25 = load i64, ptr %24
  %26 = icmp ult i64 %25, %22
  %27 = call  i1 @llvm.expect.i1(i1 %26, i1 1)
  br i1 %27, label %L506, label %L505
L505:
  %28 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %28, ptr %16
  %29 = load ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %29, ptr %17
  %30 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %30, ptr %18
  %31 = load i64, ptr %ds
  %32 = load i64, ptr %alloc
  %33 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %31, i64 %32) "statepoint-id"="1" cold [ "gc-live"(ptr %16, ptr %17, ptr %18) ]
  %34 = extractvalue { { i64, i64 }, {  } } %33, 0, 0
  %35 = extractvalue { { i64, i64 }, {  } } %33, 0, 1
  store i64 %34, ptr %ds
  store i64 %35, ptr %alloc
  %36 = load volatile ptr addrspace(1), ptr %16
  store ptr addrspace(1) %36, ptr %9
  %37 = load volatile ptr addrspace(1), ptr %17
  store ptr addrspace(1) %37, ptr %10
  %38 = load volatile ptr addrspace(1), ptr %18
  store ptr addrspace(1) %38, ptr %11
  br label %L506
L506:
  store i64 1, ptr %13
  %39 = load i64, ptr %alloc
  %40 = sub i64 %39, 32
  store i64 %40, ptr %alloc
  %41 = load i64, ptr %ds
  %42 = inttoptr i64 %41 to ptr
  %43 = load i64, ptr %42
  %44 = icmp ule i64 %43, %40
  %45 = call  i1 @llvm.expect.i1(i1 %44, i1 1)
  br i1 %45, label %L508, label %L507
L507:
  %46 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %46, ptr %16
  %47 = load ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %47, ptr %17
  %48 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %48, ptr %18
  %49 = load i64, ptr %ds
  %50 = load i64, ptr %alloc
  %51 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %49, i64 %50) "statepoint-id"="262145" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 4, i64 1, i64 3, i64 0, i64 2, i64 11, i64 0, i64 11, i64 0, i64 35, i64 5263188, i64 3029297, i64 7105633, i64 6251375, i64 7628385, i64 6255205, i64 7106416, i64 7495532, i64 7566693, i64 6255461, i64 7302259, i64 29556), "gc-live"(ptr %16, ptr %17, ptr %18) ]
  %52 = extractvalue { { i64, i64 }, {  } } %51, 0, 0
  %53 = extractvalue { { i64, i64 }, {  } } %51, 0, 1
  store i64 %52, ptr %ds
  store i64 %53, ptr %alloc
  %54 = load volatile ptr addrspace(1), ptr %16
  store ptr addrspace(1) %54, ptr %9
  %55 = load volatile ptr addrspace(1), ptr %17
  store ptr addrspace(1) %55, ptr %10
  %56 = load volatile ptr addrspace(1), ptr %18
  store ptr addrspace(1) %56, ptr %11
  br label %L508
L508:
  %57 = load i64, ptr %alloc
  %58 = add i64 %57, 8
  %59 = inttoptr i64 %58 to ptr addrspace(1)
  store ptr addrspace(1) %59, ptr %14
  %60 = load ptr addrspace(1), ptr %14
  %61 = ptrtoint ptr addrspace(1) %60 to i64
  %62 = add i64 %61, -8
  %63 = inttoptr i64 %62 to ptr
  store volatile i64 3072, ptr %63
  %64 = load ptr addrspace(1), ptr %14
  %65 = addrspacecast ptr addrspace(1) %64 to ptr
  %66 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %66, ptr %65
  %67 = load ptr addrspace(1), ptr %14
  %68 = ptrtoint ptr addrspace(1) %67 to i64
  %69 = add i64 %68, 8
  %70 = inttoptr i64 %69 to ptr
  %71 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %71, ptr %70
  %72 = load ptr addrspace(1), ptr %14
  %73 = ptrtoint ptr addrspace(1) %72 to i64
  %74 = add i64 %73, 16
  %75 = inttoptr i64 %74 to ptr
  %76 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %76, ptr %75
  %77 = load ptr addrspace(1), ptr %14
  store ptr addrspace(1) %77, ptr %6
  %78 = load ptr addrspace(1), ptr %6
  %79 = load i64, ptr %ds
  %80 = load i64, ptr %alloc
  %81 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %79, 0, 0
  %82 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %81, i64 %80, 0, 1
  %83 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %82, ptr addrspace(1) %78, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %83
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_after_poll_reuses_slots_34_35_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:                                ; %L506
	sub	x27, x27, #32
	cmp	x8, x27
	b.hi	LBB0_4
LBB0_2:                                 ; %L508
	mov	w8, #3072
	str	x8, [x27]
	mov	x8, x27
	str	x0, [x8, #8]!
	stp	x1, x2, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_3:                                 ; %L505
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	ldr	x8, [x28]
	sub	x27, x27, #32
	cmp	x8, x27
	b.ls	LBB0_2
LBB0_4:                                 ; %L507
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp1:
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	b	LBB0_2
	.cfi_endproc|}]

let alloc_const_int_only_filter n =
  let x = 1 in
  let y = 2 in
  (x, y, n)
;;

[%%expect{|
val alloc_const_int_only_filter : 'a -> int * int * 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_const_int_only_filter_36_37_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" "oxcaml-stack-check-bytes"="0" noinline gc "oxcaml" {
  %ds = alloca i64
  store i64 %0, ptr %ds
  %alloc = alloca i64
  store i64 %1, ptr %alloc
  %4 = alloca ptr addrspace(1)
  store ptr addrspace(1) %2, ptr %4
  %5 = alloca ptr addrspace(1)
  %6 = alloca ptr addrspace(1)
  %7 = alloca i64
  %8 = alloca i64
  %9 = alloca i64
  %10 = alloca ptr addrspace(1)
  br label %L1
L1:
  br label %L519
L519:
  %11 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %11, ptr %5
  %12 = load i64, ptr %alloc
  %13 = sub i64 %12, 32
  store i64 %13, ptr %alloc
  %14 = load i64, ptr %ds
  %15 = inttoptr i64 %14 to ptr
  %16 = load i64, ptr %15
  %17 = icmp ule i64 %16, %13
  %18 = call  i1 @llvm.expect.i1(i1 %17, i1 1)
  br i1 %18, label %L523, label %L522
L522:
  %19 = load ptr addrspace(1), ptr %5
  store volatile ptr addrspace(1) %19, ptr %10
  %20 = load i64, ptr %ds
  %21 = load i64, ptr %alloc
  %22 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %20, i64 %21) "statepoint-id"="262145" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 4, i64 1, i64 4, i64 0, i64 2, i64 11, i64 0, i64 11, i64 0, i64 33, i64 5263188, i64 3026994, i64 7105633, i64 6251375, i64 7237475, i64 6255731, i64 7630441, i64 7237471, i64 6257004, i64 7104870, i64 7497076), "gc-live"(ptr %10) ]
  %23 = extractvalue { { i64, i64 }, {  } } %22, 0, 0
  %24 = extractvalue { { i64, i64 }, {  } } %22, 0, 1
  store i64 %23, ptr %ds
  store i64 %24, ptr %alloc
  %25 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %25, ptr %5
  br label %L523
L523:
  %26 = load i64, ptr %alloc
  %27 = add i64 %26, 8
  %28 = inttoptr i64 %27 to ptr addrspace(1)
  store ptr addrspace(1) %28, ptr %6
  %29 = load ptr addrspace(1), ptr %6
  %30 = ptrtoint ptr addrspace(1) %29 to i64
  %31 = add i64 %30, -8
  %32 = inttoptr i64 %31 to ptr
  store volatile i64 3072, ptr %32
  %33 = load ptr addrspace(1), ptr %6
  %34 = addrspacecast ptr addrspace(1) %33 to ptr
  store volatile i64 3, ptr %34
  %35 = load ptr addrspace(1), ptr %6
  %36 = ptrtoint ptr addrspace(1) %35 to i64
  %37 = add i64 %36, 8
  %38 = inttoptr i64 %37 to ptr
  store volatile i64 5, ptr %38
  %39 = load ptr addrspace(1), ptr %6
  %40 = ptrtoint ptr addrspace(1) %39 to i64
  %41 = add i64 %40, 16
  %42 = inttoptr i64 %41 to ptr
  %43 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %43, ptr %42
  %44 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %44, ptr %4
  %45 = load ptr addrspace(1), ptr %4
  %46 = load i64, ptr %ds
  %47 = load i64, ptr %alloc
  %48 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %46, 0, 0
  %49 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %48, i64 %47, 0, 1
  %50 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %49, ptr addrspace(1) %45, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %50
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_const_int_only_filter_36_37_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	sub	x27, x27, #32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB0_2
LBB0_1:                                 ; %L523
	mov	w8, #3072
	str	x8, [x27]
	mov	w9, #3
	mov	x8, x27
	str	x9, [x8, #8]!
	mov	w9, #5
	str	x9, [x27, #16]
	str	x0, [x27, #24]
	mov	x0, x8
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L522
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp0:
	ldr	x0, [sp, #8]
	b	LBB0_1
	.cfi_endproc|}]
