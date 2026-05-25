(* TEST
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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_select_0_1_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, i64 %4) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:
	mov	x3, x27
	mov	x4, x28
LBB0_2:                                 ; %L119
	cmp	x2, #1
	csel	x0, x0, x1, eq
	mov	x28, x4
	mov	x27, x3
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_3:                                 ; %L118
	str	x0, [sp, #8]
	str	x1, [sp]
	bl	_caml_call_gc
Ltmp1:
	mov	x3, x27
	mov	x4, x28
	ldr	x0, [sp, #8]
	ldr	x1, [sp]
	b	LBB0_2
	.cfi_endproc|}]

let poll_const_int x =
  let n = 41 in
  poll ();
  String.length x + n
;;

[%%expect{|
val poll_const_int : String.t -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__poll_const_int_2_3_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:
	mov	x1, x27
	mov	x2, x28
LBB0_2:                                 ; %L145
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	lsl	x8, x8, #1
	add	x0, x8, #83
	mov	x28, x2
	mov	x27, x1
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_3:                                 ; %L144
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp1:
	mov	x1, x27
	mov	x2, x28
	ldr	x0, [sp, #8]
	b	LBB0_2
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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_under_trap_4_5_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
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
  %24 = alloca i8, i64 48
  %25 = ptrtoint ptr %24 to i64
  %26 = add i64 %25, 15
  %27 = and i64 %26, -16
  %28 = inttoptr i64 %27 to ptr
  br label %L1
L1:
  br label %L156
L156:
  %29 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %29, ptr %10
  %30 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %30, ptr %11
  %31 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %31, ptr %12
  %32 = load i64, ptr %ds
  %33 = add i64 %32, 64
  %34 = inttoptr i64 %33 to ptr
  %35 = load i64, ptr %34
  store i64 %35, ptr %13
  %36 = load i64, ptr %13
  store volatile i64 %36, ptr %14
  %37 = load i64, ptr %ds
  %38 = load i64, ptr %alloc
  %39 = call oxcaml_fpcc { { i64, i64 }, { i64 } } @"\01_wrap_try"(i64 %37, i64 %38) returns_twice "gc-leaf-function"="true"
  %40 = extractvalue { { i64, i64 }, { i64 } } %39, 0, 0
  %41 = extractvalue { { i64, i64 }, { i64 } } %39, 0, 1
  store i64 %40, ptr %ds
  store i64 %41, ptr %alloc
  %42 = extractvalue { { i64, i64 }, { i64 } } %39, 1, 0
  br label %L175
L175:
  %43 = icmp eq i64 %42, 0
  br i1 %43, label %L176, label %L177
L177:
  %44 = call i64 asm sideeffect "mov $0, x0", "=r"() "gc-leaf-function"="true"
  %45 = call i64 asm sideeffect "mov $0, x28", "=r"() "gc-leaf-function"="true"
  %46 = call i64 asm sideeffect "mov $0, x27", "=r"() "gc-leaf-function"="true"
  %47 = call i64 asm sideeffect "mov $0, x26", "=r"() "gc-leaf-function"="true"
  %48 = add i64 %45, 48
  %49 = inttoptr i64 %48 to ptr
  store i64 %47, ptr %49
  call void asm sideeffect "", "~{x0},~{x1},~{x2},~{x3},~{x4},~{x5},~{x6},~{x7},~{x8},~{x9},~{x10},~{x11},~{x12},~{x13},~{x14},~{x15},~{x16},~{x17},~{x19},~{x20},~{x21},~{x22},~{x23},~{x24},~{x25},~{memory}"() "gc-leaf-function"="true"
  store i64 %45, ptr %ds
  store i64 %46, ptr %alloc
  br label %L159
L176:
  store ptr blockaddress(@"\01_camlTOP__poll_under_trap_4_5_code", %L177), ptr @"\01_camlTOP__poll_under_trap_4_5_code.recover_rbp_var.L177"
  %50 = ptrtoint ptr %28 to i64
  %51 = add i64 %50, 16
  %52 = inttoptr i64 %51 to ptr
  %53 = ptrtoint ptr %28 to i64
  %54 = add i64 %53, 24
  %55 = inttoptr i64 %54 to ptr
  %56 = ptrtoint ptr %28 to i64
  %57 = add i64 %56, 8
  %58 = inttoptr i64 %57 to ptr
  %59 = load i64, ptr %ds
  %60 = add i64 %59, 48
  %61 = inttoptr i64 %60 to ptr
  %62 = call i64 asm sideeffect "mov $0, x26", "=r"() "gc-leaf-function"="true"
  store ptr %28, ptr %61
  %63 = ptrtoint ptr %28 to i64
  call void asm sideeffect "mov x26, $0", "r"(i64 %63) "gc-leaf-function"="true"
  store ptr @"\01_camlTOP__poll_under_trap_4_5_code.recover_rbp_asm.L177", ptr %58
  %64 = call  i64 @llvm.read_register.i64(metadata !{!"sp\00"})
  %65 = ptrtoint ptr %28 to i64
  %66 = sub i64 %64, %65
  store i64 %66, ptr %52
  call void asm sideeffect "str x29, [$0]", "r"(ptr %55) "gc-leaf-function"="true"
  store i64 %62, ptr %28
  %67 = load i64, ptr %alloc
  %68 = load i64, ptr %ds
  %69 = inttoptr i64 %68 to ptr
  %70 = load i64, ptr %69
  %71 = icmp ult i64 %70, %67
  %72 = call  i1 @llvm.expect.i1(i1 %71, i1 1)
  br i1 %72, label %L180, label %L179
L179:
  %73 = load i64, ptr %ds
  %74 = load i64, ptr %alloc
  %75 = invoke oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %73, i64 %74) "statepoint-id"="33" cold [ "gc-live"(ptr %10, ptr %11, ptr %12) ] to label %L181 unwind label %L178
L181:
  %76 = extractvalue { { i64, i64 }, {  } } %75, 0, 0
  %77 = extractvalue { { i64, i64 }, {  } } %75, 0, 1
  store i64 %76, ptr %ds
  store i64 %77, ptr %alloc
  br label %L180
L178:
  %78 = landingpad { ptr, i32 } cleanup
  br label %L177
L180:
  store i64 1, ptr %17
  %79 = load volatile ptr addrspace(1), ptr %12
  %80 = addrspacecast ptr addrspace(1) %79 to ptr
  %81 = load i64, ptr %80
  store i64 %81, ptr %19
  store i64 1, ptr %9
  %82 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %82, ptr %7
  %83 = load i64, ptr %9
  %84 = load ptr addrspace(1), ptr %7
  %85 = load i64, ptr %ds
  %86 = load i64, ptr %alloc
  %87 = load ptr, ptr %19
  %88 = invoke oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } %87(i64 %85, i64 %86, i64 %83, ptr addrspace(1) %84) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 4, i64 0, i64 4, i64 8, i64 0, i64 8, i64 0, i64 20, i64 5263188, i64 7351860, i64 7105647, i64 7239007, i64 7497060, i64 7500895, i64 28769), "gc-live"(ptr %10, ptr %11) ] to label %L183 unwind label %L182
L183:
  %89 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %88, 0, 0
  %90 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %88, 0, 1
  store i64 %89, ptr %ds
  store i64 %90, ptr %alloc
  %91 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %88, 1, 0
  store ptr addrspace(1) %91, ptr %6
  br label %L168
L182:
  %92 = landingpad { ptr, i32 } cleanup
  br label %L177
L168:
  %93 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %93, ptr %20
  %94 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %94, ptr %21
  %95 = load i64, ptr %ds
  %96 = add i64 %95, 48
  %97 = inttoptr i64 %96 to ptr
  %98 = call i64 asm sideeffect "mov $0, x26", "=r"() "gc-leaf-function"="true"
  %99 = inttoptr i64 %98 to ptr
  %100 = load i64, ptr %99
  store i64 %100, ptr %97
  call void asm sideeffect "mov x26, $0", "r"(i64 %100) "gc-leaf-function"="true"
  %101 = load volatile ptr addrspace(1), ptr %11
  store ptr addrspace(1) %101, ptr %6
  %102 = load ptr addrspace(1), ptr %6
  %103 = load i64, ptr %ds
  %104 = load i64, ptr %alloc
  %105 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %103, 0, 0
  %106 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %105, i64 %104, 0, 1
  %107 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %106, ptr addrspace(1) %102, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %107
L159:
  %108 = call i64 asm sideeffect "mov $0, x27", "=r"() "gc-leaf-function"="true"
  store i64 %108, ptr %alloc
  store i64 %44, ptr %9
  %109 = load i64, ptr %9
  %110 = inttoptr i64 %109 to ptr addrspace(1)
  store ptr addrspace(1) %110, ptr %15
  %111 = load i64, ptr %ds
  %112 = add i64 %111, 64
  %113 = inttoptr i64 %112 to ptr
  %114 = load volatile i64, ptr %14
  store i64 %114, ptr %113
  store i64 1, ptr %23
  %115 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %115, ptr %6
  %116 = load ptr addrspace(1), ptr %6
  %117 = load i64, ptr %ds
  %118 = load i64, ptr %alloc
  %119 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %117, 0, 0
  %120 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %119, i64 %118, 0, 1
  %121 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %120, ptr addrspace(1) %116, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %121
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__poll_under_trap_4_5_code:
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception0
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #44
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp6
	mov	x16, #44
	bl	_caml_llvm_prologue_realloc_stack
Ltmp6:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #80
	.cfi_def_cfa_offset 96
	mov	x8, x27
	mov	x9, x28
	str	x0, [sp, #72]
	str	x1, [sp, #64]
	str	x2, [sp, #56]
	ldr	x10, [x9, #64]
	str	x10, [sp, #48]
	mov	x28, x9
	mov	x27, x8
	bl	_wrap_try
	cbz	x0, LBB0_2
Ltmp5:                                  ; Block address taken
LBB0_1:                                 ; %L177
	; InlineAsm Start
	mov	x8, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x30, x28
	; InlineAsm End
	; InlineAsm Start
	mov	x8, x27
	; InlineAsm End
	; InlineAsm Start
	mov	x8, x26
	; InlineAsm End
	str	x8, [x30, #48]
	; InlineAsm Start
	; InlineAsm End
	; InlineAsm Start
	mov	x1, x27
	; InlineAsm End
	ldr	x9, [sp, #48]
	str	x9, [x30, #64]
	add	x9, sp, #72
	mov	x0, x30
	mov	x8, x9
	b	LBB0_5
LBB0_2:                                 ; %L176
	mov	x8, x28
	mov	x10, x27
	mov	x11, sp
	mov	x9, x10
	mov	x12, sp
	add	x12, x12, #15
Lloh0:
	adrp	x13, lCPI0_0@PAGE
Lloh1:
	ldr	x13, [x13, lCPI0_0@PAGEOFF]
	and	x12, x12, #0xfffffffffffffff0
	adrp	x14, _camlTOP__poll_under_trap_4_5_code.recover_rbp_var.L177@PAGE
	str	x13, [x14, _camlTOP__poll_under_trap_4_5_code.recover_rbp_var.L177@PAGEOFF]
	add	x13, x12, #24
	; InlineAsm Start
	mov	x14, x26
	; InlineAsm End
	str	x12, [x8, #48]
	mov	x19, x12
	; InlineAsm Start
	mov	x26, x12
	; InlineAsm End
Lloh2:
	adrp	x19, _camlTOP__poll_under_trap_4_5_code.recover_rbp_asm.L177@GOTPAGE
Lloh3:
	ldr	x19, [x19, _camlTOP__poll_under_trap_4_5_code.recover_rbp_asm.L177@GOTPAGEOFF]
	sub	x11, x11, x12
	stp	x19, x11, [x12, #8]
	; InlineAsm Start
	str	x29, [x13]
	; InlineAsm End
	str	x14, [x12]
	ldr	x11, [x8]
	cmp	x11, x10
	b.hs	LBB0_6
LBB0_3:                                 ; %L180
	ldr	x10, [sp, #56]
	ldr	x10, [x10]
	ldr	x1, [sp, #56]
Ltmp2:
	mov	w11, #1
	mov	x0, x11
	mov	x28, x8
	mov	x27, x9
	blr	x10
Ltmp7:
	mov	x0, x28
	mov	x1, x27
Ltmp3:
; %bb.4:                                ; %L183
	; InlineAsm Start
	mov	x8, x26
	; InlineAsm End
	ldr	x8, [x8]
	str	x8, [x0, #48]
	; InlineAsm Start
	mov	x26, x8
	; InlineAsm End
	add	x8, sp, #64
LBB0_5:                                 ; %common.ret
	mov	x2, x0
	ldr	x0, [x8]
	mov	x28, x2
	mov	x27, x1
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	add	sp, sp, #96
	ret
LBB0_6:                                 ; %L179
Ltmp0:
	mov	x28, x8
	mov	x27, x9
	bl	_caml_call_gc
Ltmp8:
	mov	x0, x28
	mov	x1, x27
Ltmp1:
; %bb.7:                                ; %L181
	mov	x9, x1
	mov	x8, x0
	b	LBB0_3
LBB0_8:                                 ; %L182.split-lp
Ltmp4:
	b	LBB0_1
	.loh AdrpLdrGot	Lloh2, Lloh3
	.loh AdrpLdr	Lloh0, Lloh1
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
	.uleb128 Ltmp2-Lfunc_begin0             ; >> Call Site 1 <<
	.uleb128 Ltmp1-Ltmp2                    ;   Call between Ltmp2 and Ltmp1
	.uleb128 Ltmp4-Lfunc_begin0             ;     jumps to Ltmp4
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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__poll_no_roots_6_7_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #34
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #34
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_2
LBB0_1:                                 ; %L199
	mov	x1, x27
	mov	x2, x28
	add	x0, x0, #2
	mov	x28, x2
	mov	x27, x1
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
LBB0_2:                                 ; %L198
	bl	_caml_call_gc
Ltmp1:
	b	LBB0_1
	.cfi_endproc|}]

let poll_boxed_float_root (x : float) n =
  poll ();
  if x > 0. then n + 1 else n - 1
;;

[%%expect{|
val poll_boxed_float_root : float -> int -> int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { i64 } } @"\01_camlTOP__poll_boxed_float_root_8_9_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:
	mov	x2, x27
	mov	x3, x28
LBB0_2:                                 ; %L222
	ldr	d0, [x0]
	fcmp	d0, #0.0
	mov	w8, #2
	mov	x9, #-2
	csel	x8, x8, x9, gt
	add	x0, x8, x1
	mov	x28, x3
	mov	x27, x2
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_3:                                 ; %L221
	str	x0, [sp, #8]
	bl	_caml_call_gc
Ltmp1:
	mov	x2, x27
	mov	x3, x28
	ldr	x0, [sp, #8]
	b	LBB0_2
	.cfi_endproc|}]

let poll_many_roots x y z n =
  poll ();
  if n = 0 then x else if n = 1 then y else z
;;

[%%expect{|
val poll_many_roots : 'a -> 'a -> 'a -> int -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_many_roots_10_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4, i64 %5) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #38
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #38
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:
	mov	x4, x27
	mov	x5, x28
LBB0_2:                                 ; %L249
	cmp	x3, #1
	csel	x8, x2, x0, ne
	cmp	x3, #3
	csel	x0, x1, x8, eq
	mov	x28, x5
	mov	x27, x4
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_3:                                 ; %L248
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp1:
	mov	x4, x27
	mov	x5, x28
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	b	LBB0_2
	.cfi_endproc|}]

let poll_aliased_roots x n =
  let y = Sys.opaque_identity x in
  poll ();
  if n = 0 then x else y
;;

[%%expect{|
val poll_aliased_roots : 'a -> int -> 'a = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_aliased_roots_12_13_code"(i64 %0, i64 %1, ptr addrspace(1) %2, i64 %3) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	; InlineAsm Start
	; InlineAsm End
	ldr	x8, [x28]
	cmp	x8, x27
	b.hs	LBB0_3
; %bb.1:
	mov	x2, x27
	mov	x3, x28
	mov	x8, x0
LBB0_2:                                 ; %L271
	cmp	x1, #1
	csel	x0, x0, x8, eq
	mov	x28, x3
	mov	x27, x2
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_3:                                 ; %L270
	str	x0, [sp, #8]
	str	x0, [sp]
	bl	_caml_call_gc
Ltmp1:
	mov	x2, x27
	mov	x3, x28
	ldr	x0, [sp, #8]
	ldr	x8, [sp]
	b	LBB0_2
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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__poll_twice_varying_roots_14_15_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4, i64 %5) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #38
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #38
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	ldr	x9, [x28]
	cmp	x9, x27
	b.hs	LBB0_3
; %bb.1:
	mov	x8, x27
	mov	x4, x28
	cmp	x3, #1
	csel	x10, x0, x1, eq
	cmp	x9, x8
	b.hs	LBB0_4
LBB0_2:                                 ; %L307
	cmp	x3, #3
	csel	x0, x10, x2, eq
	mov	x28, x4
	mov	x27, x8
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_3:                                 ; %L303
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	str	x2, [sp, #8]
	bl	_caml_call_gc
Ltmp1:
	mov	x8, x27
	mov	x4, x28
	ldr	x0, [sp, #24]
	ldr	x1, [sp, #16]
	ldr	x2, [sp, #8]
	ldr	x9, [x28]
	cmp	x3, #1
	csel	x10, x0, x1, eq
	cmp	x9, x8
	b.lo	LBB0_2
LBB0_4:                                 ; %L306
	str	x2, [sp, #24]
	str	x10, [sp, #16]
	mov	x28, x4
	mov	x27, x8
	bl	_caml_call_gc
Ltmp2:
	mov	x8, x27
	mov	x4, x28
	ldr	x2, [sp, #24]
	ldr	x10, [sp, #16]
	b	LBB0_2
	.cfi_endproc|}]

let alloc_triple_live_roots x y z =
  (x, y, z)
;;

[%%expect{|
val alloc_triple_live_roots : 'a -> 'b -> 'c -> 'a * 'b * 'c = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_triple_live_roots_16_17_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  %21 = load i64, ptr %ds
  %22 = add i64 %21, 8
  %23 = inttoptr i64 %22 to ptr
  store i64 %20, ptr %23
  %24 = load i64, ptr %ds
  %25 = load i64, ptr %alloc
  %26 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap.0..0"(i64 %24, i64 %25) "gc-leaf-function"="true"
  %27 = extractvalue { { i64, i64 }, {  } } %26, 0, 0
  %28 = extractvalue { { i64, i64 }, {  } } %26, 0, 1
  store i64 %27, ptr %ds
  store i64 %28, ptr %alloc
  %29 = load i64, ptr %alloc
  %30 = load i64, ptr %ds
  %31 = load i64, ptr %alloc
  %32 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0"(i64 %30, i64 %31, i64 %29, i64 503177722, i64 6) "gc-leaf-function"="true"
  %33 = extractvalue { { i64, i64 }, {  } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, {  } } %32, 0, 1
  store i64 %33, ptr %ds
  store i64 %34, ptr %alloc
  %35 = load i64, ptr %alloc
  %36 = sub i64 %35, 32
  store i64 %36, ptr %alloc
  %37 = load i64, ptr %ds
  %38 = inttoptr i64 %37 to ptr
  %39 = load i64, ptr %38
  %40 = icmp ule i64 %39, %36
  %41 = call  i1 @llvm.expect.i1(i1 %40, i1 1)
  br i1 %41, label %L323, label %L322
L322:
  %42 = load ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %42, ptr %14
  %43 = load ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %43, ptr %15
  %44 = load ptr addrspace(1), ptr %11
  store volatile ptr addrspace(1) %44, ptr %16
  %45 = load i64, ptr %ds
  %46 = load i64, ptr %alloc
  %47 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %45, i64 %46) "statepoint-id"="262145" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 4, i64 1, i64 2, i64 0, i64 2, i64 11, i64 0, i64 11, i64 0, i64 29, i64 5263188, i64 3026993, i64 7105633, i64 6251375, i64 6910580, i64 6646896, i64 6909023, i64 6251894, i64 7303026, i64 29556), "gc-live"(ptr %14, ptr %15, ptr %16) ]
  %48 = extractvalue { { i64, i64 }, {  } } %47, 0, 0
  %49 = extractvalue { { i64, i64 }, {  } } %47, 0, 1
  store i64 %48, ptr %ds
  store i64 %49, ptr %alloc
  %50 = load volatile ptr addrspace(1), ptr %14
  store ptr addrspace(1) %50, ptr %9
  %51 = load volatile ptr addrspace(1), ptr %15
  store ptr addrspace(1) %51, ptr %10
  %52 = load volatile ptr addrspace(1), ptr %16
  store ptr addrspace(1) %52, ptr %11
  br label %L323
L323:
  %53 = load i64, ptr %alloc
  %54 = add i64 %53, 8
  %55 = inttoptr i64 %54 to ptr addrspace(1)
  store ptr addrspace(1) %55, ptr %12
  %56 = load ptr addrspace(1), ptr %12
  %57 = ptrtoint ptr addrspace(1) %56 to i64
  %58 = add i64 %57, -8
  %59 = inttoptr i64 %58 to ptr
  store volatile i64 3072, ptr %59
  %60 = load ptr addrspace(1), ptr %12
  %61 = addrspacecast ptr addrspace(1) %60 to ptr
  %62 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %62, ptr %61
  %63 = load ptr addrspace(1), ptr %12
  %64 = ptrtoint ptr addrspace(1) %63 to i64
  %65 = add i64 %64, 8
  %66 = inttoptr i64 %65 to ptr
  %67 = load ptr addrspace(1), ptr %10
  store ptr addrspace(1) %67, ptr %66
  %68 = load ptr addrspace(1), ptr %12
  %69 = ptrtoint ptr addrspace(1) %68 to i64
  %70 = add i64 %69, 16
  %71 = inttoptr i64 %70 to ptr
  %72 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %72, ptr %71
  %73 = load ptr addrspace(1), ptr %12
  store ptr addrspace(1) %73, ptr %6
  %74 = load ptr addrspace(1), ptr %6
  %75 = load i64, ptr %ds
  %76 = load i64, ptr %alloc
  %77 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %75, 0, 0
  %78 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %77, i64 %76, 0, 1
  %79 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %78, ptr addrspace(1) %74, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %79
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__alloc_triple_live_roots_16_17_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #40
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #40
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #48
	.cfi_def_cfa_offset 64
	stp	x1, x2, [sp, #8]                ; 16-byte Folded Spill
	str	x0, [sp]                        ; 8-byte Folded Spill
	str	x27, [x28, #8]
	bl	_c_call_wrapper.caml_debug_check_minor_heap.0..0
	mov	x0, x27
	mov	x27, x0
	mov	w1, #57850
	movk	w1, #7677, lsl #16
	mov	w2, #6
	bl	_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0
	mov	x1, x28
	sub	x8, x27, #32
	ldr	x9, [x28]
	cmp	x9, x8
	b.hi	LBB0_3
; %bb.1:
	ldp	x11, x10, [sp, #8]              ; 16-byte Folded Reload
	ldr	x12, [sp]                       ; 8-byte Folded Reload
LBB0_2:                                 ; %L323
	mov	w9, #3072
	str	x9, [x8]
	mov	x0, x8
	str	x12, [x0, #8]!
	stp	x11, x10, [x8, #16]
	mov	x28, x1
	mov	x27, x8
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB0_3:                                 ; %L322
	ldr	x9, [sp]                        ; 8-byte Folded Reload
	str	x9, [sp, #40]
	ldr	x9, [sp, #8]                    ; 8-byte Folded Reload
	str	x9, [sp, #32]
	ldr	x9, [sp, #16]                   ; 8-byte Folded Reload
	str	x9, [sp, #24]
	mov	x28, x1
	mov	x27, x8
	bl	_caml_call_gc
Ltmp1:
	mov	x8, x27
	mov	x1, x28
	ldr	x12, [sp, #40]
	ldr	x11, [sp, #32]
	ldr	x10, [sp, #24]
	b	LBB0_2
	.cfi_endproc|}]

let call_with_live_roots x y f =
  f x;
  y
;;

[%%expect{|
val call_with_live_roots : 'a -> 'b -> ('a -> 'c) -> 'b = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__call_with_live_roots_18_19_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  %15 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %15, ptr %9
  %16 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %16, ptr %10
  %17 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %17, ptr %11
  %18 = load ptr addrspace(1), ptr %11
  %19 = addrspacecast ptr addrspace(1) %18 to ptr
  %20 = load i64, ptr %19
  store i64 %20, ptr %12
  %21 = load ptr addrspace(1), ptr %9
  store ptr addrspace(1) %21, ptr %6
  %22 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %22, ptr %7
  %23 = load ptr addrspace(1), ptr %6
  %24 = load ptr addrspace(1), ptr %7
  %25 = load i64, ptr %ds
  %26 = load i64, ptr %alloc
  %27 = load ptr, ptr %12
  %28 = call oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } %27(i64 %25, i64 %26, ptr addrspace(1) %23, ptr addrspace(1) %24) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 2, i64 0, i64 2, i64 5, i64 0, i64 5, i64 0, i64 26, i64 5263188, i64 3027249, i64 7102819, i64 7823212, i64 6845545, i64 6909023, i64 6251894, i64 7303026, i64 29556), "gc-live"(ptr %10) ]
  %29 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %30 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  store i64 %29, ptr %ds
  store i64 %30, ptr %alloc
  %31 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 1, 0
  store ptr addrspace(1) %31, ptr %6
  br label %L336
L336:
  %32 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %32, ptr %13
  %33 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %33, ptr %14
  %34 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %34, ptr %6
  %35 = load ptr addrspace(1), ptr %6
  %36 = load i64, ptr %ds
  %37 = load i64, ptr %alloc
  %38 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %36, 0, 0
  %39 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %38, i64 %37, 0, 1
  %40 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %39, ptr addrspace(1) %35, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %40
}|}]

[%%expect_llvm_asm AArch64{|_camlTOP__call_with_live_roots_18_19_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	str	x1, [sp, #8]
	ldr	x8, [x2]
	mov	x1, x2
	blr	x8
Ltmp1:
	ldr	x0, [sp, #8]
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.cfi_endproc|}]

let alloc_no_roots n =
  (n, n + 1)
;;

[%%expect{|
val alloc_no_roots : int -> int * int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_no_roots_20_21_code"(i64 %0, i64 %1, i64 %2) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  br label %L348
L348:
  %10 = load i64, ptr %4
  store i64 %10, ptr %6
  %11 = load i64, ptr %alloc
  %12 = load i64, ptr %ds
  %13 = add i64 %12, 8
  %14 = inttoptr i64 %13 to ptr
  store i64 %11, ptr %14
  %15 = load i64, ptr %ds
  %16 = load i64, ptr %alloc
  %17 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap.0..0"(i64 %15, i64 %16) "gc-leaf-function"="true"
  %18 = extractvalue { { i64, i64 }, {  } } %17, 0, 0
  %19 = extractvalue { { i64, i64 }, {  } } %17, 0, 1
  store i64 %18, ptr %ds
  store i64 %19, ptr %alloc
  %20 = load i64, ptr %alloc
  %21 = load i64, ptr %ds
  %22 = load i64, ptr %alloc
  %23 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0"(i64 %21, i64 %22, i64 %20, i64 530578372, i64 4) "gc-leaf-function"="true"
  %24 = extractvalue { { i64, i64 }, {  } } %23, 0, 0
  %25 = extractvalue { { i64, i64 }, {  } } %23, 0, 1
  store i64 %24, ptr %ds
  store i64 %25, ptr %alloc
  %26 = load i64, ptr %alloc
  %27 = sub i64 %26, 24
  store i64 %27, ptr %alloc
  %28 = load i64, ptr %ds
  %29 = inttoptr i64 %28 to ptr
  %30 = load i64, ptr %29
  %31 = icmp ule i64 %30, %27
  %32 = call  i1 @llvm.expect.i1(i1 %31, i1 1)
  br i1 %32, label %L353, label %L352
L352:
  %33 = load i64, ptr %ds
  %34 = load i64, ptr %alloc
  %35 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %33, i64 %34) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 2, i64 12, i64 0, i64 12, i64 0, i64 20, i64 5263188, i64 3027505, i64 7105633, i64 6251375, i64 6254446, i64 7303026, i64 29556) ]
  %36 = extractvalue { { i64, i64 }, {  } } %35, 0, 0
  %37 = extractvalue { { i64, i64 }, {  } } %35, 0, 1
  store i64 %36, ptr %ds
  store i64 %37, ptr %alloc
  br label %L353
L353:
  %38 = load i64, ptr %alloc
  %39 = add i64 %38, 8
  %40 = inttoptr i64 %39 to ptr addrspace(1)
  store ptr addrspace(1) %40, ptr %7
  %41 = load ptr addrspace(1), ptr %7
  %42 = ptrtoint ptr addrspace(1) %41 to i64
  %43 = add i64 %42, -8
  %44 = inttoptr i64 %43 to ptr
  store volatile i64 2048, ptr %44
  %45 = load ptr addrspace(1), ptr %7
  %46 = addrspacecast ptr addrspace(1) %45 to ptr
  %47 = load i64, ptr %6
  store volatile i64 %47, ptr %46
  %48 = load i64, ptr %6
  %49 = add i64 %48, 2
  store i64 %49, ptr %9
  %50 = load ptr addrspace(1), ptr %7
  %51 = ptrtoint ptr addrspace(1) %50 to i64
  %52 = add i64 %51, 8
  %53 = inttoptr i64 %52 to ptr
  %54 = load i64, ptr %9
  store volatile i64 %54, ptr %53
  %55 = load ptr addrspace(1), ptr %7
  store ptr addrspace(1) %55, ptr %5
  %56 = load ptr addrspace(1), ptr %5
  %57 = load i64, ptr %ds
  %58 = load i64, ptr %alloc
  %59 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %57, 0, 0
  %60 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %59, i64 %58, 0, 1
  %61 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %60, ptr addrspace(1) %56, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %61
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_no_roots_20_21_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	str	x27, [x28, #8]
	bl	_c_call_wrapper.caml_debug_check_minor_heap.0..0
	mov	x0, x27
	mov	x27, x0
	mov	w1, #64452
	movk	w1, #8095, lsl #16
	mov	w2, #4
	bl	_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0
	mov	x1, x28
	sub	x8, x27, #24
	ldr	x9, [x28]
	cmp	x9, x8
	b.hi	LBB0_2
LBB0_1:                                 ; %L353
	mov	w9, #2048
	str	x9, [x8]
	mov	x0, x8
	ldr	x9, [sp, #8]                    ; 8-byte Folded Reload
	str	x9, [x0, #8]!
	add	x9, x9, #2
	str	x9, [x8, #16]
	mov	x28, x1
	mov	x27, x8
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_2:                                 ; %L352
	mov	x28, x1
	mov	x27, x8
	bl	_caml_call_gc
Ltmp1:
	mov	x8, x27
	mov	x1, x28
	b	LBB0_1
	.cfi_endproc|}]

let alloc_const_int_filter x =
  let n = 42 in
  (x, n)
;;

[%%expect{|
val alloc_const_int_filter : 'a -> 'a * int = <fun>
|}]

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_const_int_filter_22_23_code"(i64 %0, i64 %1, ptr addrspace(1) %2) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  br label %L364
L364:
  %10 = load ptr addrspace(1), ptr %4
  store ptr addrspace(1) %10, ptr %5
  %11 = load i64, ptr %alloc
  %12 = load i64, ptr %ds
  %13 = add i64 %12, 8
  %14 = inttoptr i64 %13 to ptr
  store i64 %11, ptr %14
  %15 = load i64, ptr %ds
  %16 = load i64, ptr %alloc
  %17 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap.0..0"(i64 %15, i64 %16) "gc-leaf-function"="true"
  %18 = extractvalue { { i64, i64 }, {  } } %17, 0, 0
  %19 = extractvalue { { i64, i64 }, {  } } %17, 0, 1
  store i64 %18, ptr %ds
  store i64 %19, ptr %alloc
  %20 = load i64, ptr %alloc
  %21 = load i64, ptr %ds
  %22 = load i64, ptr %alloc
  %23 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0"(i64 %21, i64 %22, i64 %20, i64 147638872, i64 4) "gc-leaf-function"="true"
  %24 = extractvalue { { i64, i64 }, {  } } %23, 0, 0
  %25 = extractvalue { { i64, i64 }, {  } } %23, 0, 1
  store i64 %24, ptr %ds
  store i64 %25, ptr %alloc
  %26 = load i64, ptr %alloc
  %27 = sub i64 %26, 24
  store i64 %27, ptr %alloc
  %28 = load i64, ptr %ds
  %29 = inttoptr i64 %28 to ptr
  %30 = load i64, ptr %29
  %31 = icmp ule i64 %30, %27
  %32 = call  i1 @llvm.expect.i1(i1 %31, i1 1)
  br i1 %32, label %L368, label %L367
L367:
  %33 = load ptr addrspace(1), ptr %5
  store volatile ptr addrspace(1) %33, ptr %9
  %34 = load i64, ptr %ds
  %35 = load i64, ptr %alloc
  %36 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %34, i64 %35) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 3, i64 0, i64 2, i64 8, i64 0, i64 8, i64 0, i64 28, i64 5263188, i64 3027761, i64 7105633, i64 6251375, i64 7237475, i64 6255731, i64 7630441, i64 6907487, i64 6648940, i64 114), "gc-live"(ptr %9) ]
  %37 = extractvalue { { i64, i64 }, {  } } %36, 0, 0
  %38 = extractvalue { { i64, i64 }, {  } } %36, 0, 1
  store i64 %37, ptr %ds
  store i64 %38, ptr %alloc
  %39 = load volatile ptr addrspace(1), ptr %9
  store ptr addrspace(1) %39, ptr %5
  br label %L368
L368:
  %40 = load i64, ptr %alloc
  %41 = add i64 %40, 8
  %42 = inttoptr i64 %41 to ptr addrspace(1)
  store ptr addrspace(1) %42, ptr %6
  %43 = load ptr addrspace(1), ptr %6
  %44 = ptrtoint ptr addrspace(1) %43 to i64
  %45 = add i64 %44, -8
  %46 = inttoptr i64 %45 to ptr
  store volatile i64 2048, ptr %46
  %47 = load ptr addrspace(1), ptr %6
  %48 = addrspacecast ptr addrspace(1) %47 to ptr
  %49 = load ptr addrspace(1), ptr %5
  store ptr addrspace(1) %49, ptr %48
  %50 = load ptr addrspace(1), ptr %6
  %51 = ptrtoint ptr addrspace(1) %50 to i64
  %52 = add i64 %51, 8
  %53 = inttoptr i64 %52 to ptr
  store volatile i64 85, ptr %53
  %54 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %54, ptr %4
  %55 = load ptr addrspace(1), ptr %4
  %56 = load i64, ptr %ds
  %57 = load i64, ptr %alloc
  %58 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %56, 0, 0
  %59 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %58, i64 %57, 0, 1
  %60 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %59, ptr addrspace(1) %55, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %60
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_const_int_filter_22_23_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #36
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #36
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	str	x0, [sp, #-16]!                 ; 8-byte Folded Spill
	.cfi_def_cfa_offset 32
	str	x27, [x28, #8]
	bl	_c_call_wrapper.caml_debug_check_minor_heap.0..0
	mov	x0, x27
	mov	x27, x0
	mov	w1, #51800
	movk	w1, #2252, lsl #16
	mov	w2, #4
	bl	_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0
	mov	x1, x28
	sub	x8, x27, #24
	ldr	x9, [x28]
	cmp	x9, x8
	b.hi	LBB0_3
; %bb.1:
	ldr	x10, [sp]                       ; 8-byte Folded Reload
LBB0_2:                                 ; %L368
	mov	w9, #2048
	str	x9, [x8]
	mov	x0, x8
	str	x10, [x0, #8]!
	mov	w9, #85
	str	x9, [x8, #16]
	mov	x28, x1
	mov	x27, x8
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
LBB0_3:                                 ; %L367
	ldr	x9, [sp]                        ; 8-byte Folded Reload
	str	x9, [sp, #8]
	mov	x28, x1
	mov	x27, x8
	bl	_caml_call_gc
Ltmp1:
	mov	x8, x27
	mov	x1, x28
	ldr	x10, [sp, #8]
	b	LBB0_2
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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__alloc_under_trap_24_25_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" noinline gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
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
  %24 = alloca i8, i64 48
  %25 = ptrtoint ptr %24 to i64
  %26 = add i64 %25, 15
  %27 = and i64 %26, -16
  %28 = inttoptr i64 %27 to ptr
  br label %L1
L1:
  br label %L379
L379:
  %29 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %29, ptr %10
  %30 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %30, ptr %11
  %31 = load ptr addrspace(1), ptr %8
  store volatile ptr addrspace(1) %31, ptr %12
  %32 = load i64, ptr %ds
  %33 = add i64 %32, 64
  %34 = inttoptr i64 %33 to ptr
  %35 = load i64, ptr %34
  store i64 %35, ptr %13
  %36 = load i64, ptr %13
  store volatile i64 %36, ptr %14
  %37 = load i64, ptr %ds
  %38 = load i64, ptr %alloc
  %39 = call oxcaml_fpcc { { i64, i64 }, { i64 } } @"\01_wrap_try"(i64 %37, i64 %38) returns_twice "gc-leaf-function"="true"
  %40 = extractvalue { { i64, i64 }, { i64 } } %39, 0, 0
  %41 = extractvalue { { i64, i64 }, { i64 } } %39, 0, 1
  store i64 %40, ptr %ds
  store i64 %41, ptr %alloc
  %42 = extractvalue { { i64, i64 }, { i64 } } %39, 1, 0
  br label %L398
L398:
  %43 = icmp eq i64 %42, 0
  br i1 %43, label %L399, label %L400
L400:
  %44 = call i64 asm sideeffect "mov $0, x0", "=r"() "gc-leaf-function"="true"
  %45 = call i64 asm sideeffect "mov $0, x28", "=r"() "gc-leaf-function"="true"
  %46 = call i64 asm sideeffect "mov $0, x27", "=r"() "gc-leaf-function"="true"
  %47 = call i64 asm sideeffect "mov $0, x26", "=r"() "gc-leaf-function"="true"
  %48 = add i64 %45, 48
  %49 = inttoptr i64 %48 to ptr
  store i64 %47, ptr %49
  call void asm sideeffect "", "~{x0},~{x1},~{x2},~{x3},~{x4},~{x5},~{x6},~{x7},~{x8},~{x9},~{x10},~{x11},~{x12},~{x13},~{x14},~{x15},~{x16},~{x17},~{x19},~{x20},~{x21},~{x22},~{x23},~{x24},~{x25},~{memory}"() "gc-leaf-function"="true"
  store i64 %45, ptr %ds
  store i64 %46, ptr %alloc
  br label %L382
L399:
  store ptr blockaddress(@"\01_camlTOP__alloc_under_trap_24_25_code", %L400), ptr @"\01_camlTOP__alloc_under_trap_24_25_code.recover_rbp_var.L400"
  %50 = ptrtoint ptr %28 to i64
  %51 = add i64 %50, 16
  %52 = inttoptr i64 %51 to ptr
  %53 = ptrtoint ptr %28 to i64
  %54 = add i64 %53, 24
  %55 = inttoptr i64 %54 to ptr
  %56 = ptrtoint ptr %28 to i64
  %57 = add i64 %56, 8
  %58 = inttoptr i64 %57 to ptr
  %59 = load i64, ptr %ds
  %60 = add i64 %59, 48
  %61 = inttoptr i64 %60 to ptr
  %62 = call i64 asm sideeffect "mov $0, x26", "=r"() "gc-leaf-function"="true"
  store ptr %28, ptr %61
  %63 = ptrtoint ptr %28 to i64
  call void asm sideeffect "mov x26, $0", "r"(i64 %63) "gc-leaf-function"="true"
  store ptr @"\01_camlTOP__alloc_under_trap_24_25_code.recover_rbp_asm.L400", ptr %58
  %64 = call  i64 @llvm.read_register.i64(metadata !{!"sp\00"})
  %65 = ptrtoint ptr %28 to i64
  %66 = sub i64 %64, %65
  store i64 %66, ptr %52
  call void asm sideeffect "str x29, [$0]", "r"(ptr %55) "gc-leaf-function"="true"
  store i64 %62, ptr %28
  %67 = load i64, ptr %alloc
  %68 = load i64, ptr %ds
  %69 = add i64 %68, 8
  %70 = inttoptr i64 %69 to ptr
  store i64 %67, ptr %70
  %71 = load i64, ptr %ds
  %72 = load i64, ptr %alloc
  %73 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap.0..0"(i64 %71, i64 %72) "gc-leaf-function"="true"
  %74 = extractvalue { { i64, i64 }, {  } } %73, 0, 0
  %75 = extractvalue { { i64, i64 }, {  } } %73, 0, 1
  store i64 %74, ptr %ds
  store i64 %75, ptr %alloc
  %76 = load i64, ptr %alloc
  %77 = load i64, ptr %ds
  %78 = load i64, ptr %alloc
  %79 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0"(i64 %77, i64 %78, i64 %76, i64 599343299, i64 16) "gc-leaf-function"="true"
  %80 = extractvalue { { i64, i64 }, {  } } %79, 0, 0
  %81 = extractvalue { { i64, i64 }, {  } } %79, 0, 1
  store i64 %80, ptr %ds
  store i64 %81, ptr %alloc
  %82 = load i64, ptr %alloc
  %83 = sub i64 %82, 24
  store i64 %83, ptr %alloc
  %84 = load i64, ptr %ds
  %85 = inttoptr i64 %84 to ptr
  %86 = load i64, ptr %85
  %87 = icmp ule i64 %86, %83
  %88 = call  i1 @llvm.expect.i1(i1 %87, i1 1)
  br i1 %88, label %L403, label %L402
L402:
  %89 = load i64, ptr %ds
  %90 = load i64, ptr %alloc
  %91 = invoke oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %89, i64 %90) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 3, i64 0, i64 15, i64 19, i64 0, i64 19, i64 0, i64 22, i64 5263188, i64 3028017, i64 7105633, i64 6251375, i64 6581877, i64 6255205, i64 6386292, i64 112), "gc-live"(ptr %10, ptr %11, ptr %12) ] to label %L404 unwind label %L401
L404:
  %92 = extractvalue { { i64, i64 }, {  } } %91, 0, 0
  %93 = extractvalue { { i64, i64 }, {  } } %91, 0, 1
  store i64 %92, ptr %ds
  store i64 %93, ptr %alloc
  br label %L403
L401:
  %94 = landingpad { ptr, i32 } cleanup
  br label %L400
L403:
  %95 = load i64, ptr %alloc
  %96 = add i64 %95, 8
  %97 = inttoptr i64 %96 to ptr addrspace(1)
  store ptr addrspace(1) %97, ptr %16
  %98 = load ptr addrspace(1), ptr %16
  %99 = ptrtoint ptr addrspace(1) %98 to i64
  %100 = add i64 %99, -8
  %101 = inttoptr i64 %100 to ptr
  store volatile i64 2048, ptr %101
  %102 = load ptr addrspace(1), ptr %16
  %103 = addrspacecast ptr addrspace(1) %102 to ptr
  %104 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %104, ptr %103
  %105 = load ptr addrspace(1), ptr %16
  %106 = ptrtoint ptr addrspace(1) %105 to i64
  %107 = add i64 %106, 8
  %108 = inttoptr i64 %107 to ptr
  %109 = load volatile ptr addrspace(1), ptr %11
  store ptr addrspace(1) %109, ptr %108
  %110 = load ptr addrspace(1), ptr %16
  store ptr addrspace(1) %110, ptr %18
  %111 = load volatile ptr addrspace(1), ptr %12
  %112 = addrspacecast ptr addrspace(1) %111 to ptr
  %113 = load i64, ptr %112
  store i64 %113, ptr %19
  %114 = load ptr addrspace(1), ptr %18
  store ptr addrspace(1) %114, ptr %6
  %115 = load volatile ptr addrspace(1), ptr %12
  store ptr addrspace(1) %115, ptr %7
  %116 = load ptr addrspace(1), ptr %6
  %117 = load ptr addrspace(1), ptr %7
  %118 = load i64, ptr %ds
  %119 = load i64, ptr %alloc
  %120 = load ptr, ptr %19
  %121 = invoke oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } %120(i64 %118, i64 %119, ptr addrspace(1) %116, ptr addrspace(1) %117) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 4, i64 0, i64 4, i64 10, i64 0, i64 10, i64 0, i64 22, i64 5263188, i64 3028017, i64 7105633, i64 6251375, i64 6581877, i64 6255205, i64 6386292, i64 112), "gc-live"(ptr %10) ] to label %L406 unwind label %L405
L406:
  %122 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %121, 0, 0
  %123 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %121, 0, 1
  store i64 %122, ptr %ds
  store i64 %123, ptr %alloc
  %124 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %121, 1, 0
  store ptr addrspace(1) %124, ptr %6
  br label %L391
L405:
  %125 = landingpad { ptr, i32 } cleanup
  br label %L400
L391:
  %126 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %126, ptr %20
  %127 = load ptr addrspace(1), ptr %20
  store ptr addrspace(1) %127, ptr %21
  %128 = load i64, ptr %ds
  %129 = add i64 %128, 48
  %130 = inttoptr i64 %129 to ptr
  %131 = call i64 asm sideeffect "mov $0, x26", "=r"() "gc-leaf-function"="true"
  %132 = inttoptr i64 %131 to ptr
  %133 = load i64, ptr %132
  store i64 %133, ptr %130
  call void asm sideeffect "mov x26, $0", "r"(i64 %133) "gc-leaf-function"="true"
  %134 = load ptr addrspace(1), ptr %21
  store ptr addrspace(1) %134, ptr %6
  %135 = load ptr addrspace(1), ptr %6
  %136 = load i64, ptr %ds
  %137 = load i64, ptr %alloc
  %138 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %136, 0, 0
  %139 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %138, i64 %137, 0, 1
  %140 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %139, ptr addrspace(1) %135, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %140
L382:
  %141 = call i64 asm sideeffect "mov $0, x27", "=r"() "gc-leaf-function"="true"
  store i64 %141, ptr %alloc
  store i64 %44, ptr %9
  %142 = load i64, ptr %9
  %143 = inttoptr i64 %142 to ptr addrspace(1)
  store ptr addrspace(1) %143, ptr %15
  %144 = load i64, ptr %ds
  %145 = add i64 %144, 64
  %146 = inttoptr i64 %145 to ptr
  %147 = load volatile i64, ptr %14
  store i64 %147, ptr %146
  store i64 1, ptr %23
  %148 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %148, ptr %6
  %149 = load ptr addrspace(1), ptr %6
  %150 = load i64, ptr %ds
  %151 = load i64, ptr %alloc
  %152 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %150, 0, 0
  %153 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %152, i64 %151, 0, 1
  %154 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %153, ptr addrspace(1) %149, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %154
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__alloc_under_trap_24_25_code:
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception0
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #44
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp6
	mov	x16, #44
	bl	_caml_llvm_prologue_realloc_stack
Ltmp6:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #80
	.cfi_def_cfa_offset 96
	mov	x8, x27
	mov	x9, x28
	str	x0, [sp, #72]
	str	x1, [sp, #64]
	str	x2, [sp, #56]
	ldr	x10, [x9, #64]
	str	x10, [sp, #48]
	mov	x28, x9
	mov	x27, x8
	bl	_wrap_try
	cbz	x0, LBB0_2
Ltmp5:                                  ; Block address taken
LBB0_1:                                 ; %L400
	; InlineAsm Start
	mov	x8, x0
	; InlineAsm End
	; InlineAsm Start
	mov	x30, x28
	; InlineAsm End
	; InlineAsm Start
	mov	x8, x27
	; InlineAsm End
	; InlineAsm Start
	mov	x8, x26
	; InlineAsm End
	str	x8, [x30, #48]
	; InlineAsm Start
	; InlineAsm End
	; InlineAsm Start
	mov	x2, x27
	; InlineAsm End
	ldr	x9, [sp, #48]
	str	x9, [x30, #64]
	ldr	x0, [sp, #72]
	mov	x1, x30
	mov	x28, x1
	mov	x27, x2
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	add	sp, sp, #96
	ret
LBB0_2:                                 ; %L399
	mov	x1, x28
	mov	x2, x27
	mov	x8, sp
	mov	x9, sp
	add	x9, x9, #15
	and	x9, x9, #0xfffffffffffffff0
Lloh0:
	adrp	x10, lCPI0_0@PAGE
Lloh1:
	ldr	x10, [x10, lCPI0_0@PAGEOFF]
Lloh2:
	adrp	x11, _camlTOP__alloc_under_trap_24_25_code.recover_rbp_var.L400@PAGE
	str	x10, [x11, _camlTOP__alloc_under_trap_24_25_code.recover_rbp_var.L400@PAGEOFF]
	; InlineAsm Start
	mov	x10, x26
	; InlineAsm End
	str	x9, [x1, #48]
	; InlineAsm Start
	mov	x26, x9
	; InlineAsm End
Lloh3:
	adrp	x11, _camlTOP__alloc_under_trap_24_25_code.recover_rbp_asm.L400@GOTPAGE
Lloh4:
	ldr	x11, [x11, _camlTOP__alloc_under_trap_24_25_code.recover_rbp_asm.L400@GOTPAGEOFF]
	add	x12, x9, #24
	sub	x8, x8, x9
	stp	x11, x8, [x9, #8]
	; InlineAsm Start
	str	x29, [x12]
	; InlineAsm End
	str	x10, [x9]
	str	x2, [x1, #8]
	mov	x28, x1
	mov	x27, x2
	bl	_c_call_wrapper.caml_debug_check_minor_heap.0..0
	mov	x3, x28
	mov	x0, x27
	mov	w8, #16579
	movk	w8, #9145, lsl #16
	mov	x1, x8
	mov	w8, #16
	mov	x2, x8
	mov	x28, x3
	mov	x27, x0
	bl	_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0
	mov	x1, x28
	mov	x8, x27
	ldr	x9, [x1]
	sub	x10, x8, #24
	mov	x8, x10
	cmp	x9, x10
	b.hi	LBB0_5
LBB0_3:                                 ; %L403
	mov	w9, #2048
                                        ; kill: def $x9 killed $w9
	str	x9, [x8]
	ldr	x9, [sp, #72]
	mov	x0, x8
	str	x9, [x0, #8]!
	mov	x2, x1
	ldr	x9, [sp, #64]
	str	x9, [x8, #16]
	ldr	x9, [sp, #56]
	ldr	x9, [x9]
	ldr	x1, [sp, #56]
Ltmp2:
	mov	x28, x2
	mov	x27, x8
	blr	x9
Ltmp7:
	mov	x1, x28
Ltmp3:
; %bb.4:                                ; %L406
	mov	x2, x27
	; InlineAsm Start
	mov	x8, x26
	; InlineAsm End
	ldr	x8, [x8]
	str	x8, [x1, #48]
	; InlineAsm Start
	mov	x26, x8
	; InlineAsm End
	mov	x28, x1
	mov	x27, x2
	ldp	x29, x30, [sp, #80]             ; 16-byte Folded Reload
	add	sp, sp, #96
	ret
LBB0_5:                                 ; %L402
Ltmp0:
	mov	x28, x1
	mov	x27, x8
	bl	_caml_call_gc
Ltmp8:
	mov	x0, x28
	mov	x1, x27
Ltmp1:
; %bb.6:                                ; %L404
	mov	x8, x1
	mov	x1, x0
	b	LBB0_3
LBB0_7:                                 ; %L405.split-lp
Ltmp4:
	b	LBB0_1
	.loh AdrpLdrGot	Lloh3, Lloh4
	.loh AdrpAdrp	Lloh2, Lloh3
	.loh AdrpLdr	Lloh0, Lloh1
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
	.uleb128 Ltmp2-Lfunc_begin0             ;   Call between Lfunc_begin0 and Ltmp2
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
	.uleb128 Ltmp2-Lfunc_begin0             ; >> Call Site 2 <<
	.uleb128 Ltmp1-Ltmp2                    ;   Call between Ltmp2 and Ltmp1
	.uleb128 Ltmp4-Lfunc_begin0             ;     jumps to Ltmp4
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

[%%expect_llvm_ir AArch64{|define  oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlTOP__call_then_alloc_with_live_roots_26_27_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3, ptr addrspace(1) %4) "oxcaml-stack-check"="true" noinline gc "oxcaml" {
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
  br label %L417
L417:
  %19 = load ptr addrspace(1), ptr %6
  store volatile ptr addrspace(1) %19, ptr %9
  %20 = load ptr addrspace(1), ptr %7
  store volatile ptr addrspace(1) %20, ptr %10
  %21 = load ptr addrspace(1), ptr %8
  store ptr addrspace(1) %21, ptr %11
  %22 = load ptr addrspace(1), ptr %11
  %23 = addrspacecast ptr addrspace(1) %22 to ptr
  %24 = load i64, ptr %23
  store i64 %24, ptr %12
  %25 = load volatile ptr addrspace(1), ptr %9
  store ptr addrspace(1) %25, ptr %6
  %26 = load ptr addrspace(1), ptr %11
  store ptr addrspace(1) %26, ptr %7
  %27 = load ptr addrspace(1), ptr %6
  %28 = load ptr addrspace(1), ptr %7
  %29 = load i64, ptr %ds
  %30 = load i64, ptr %alloc
  %31 = load ptr, ptr %12
  %32 = call oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } %31(i64 %29, i64 %30, ptr addrspace(1) %27, ptr addrspace(1) %28) "statepoint-id"="0" [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 3, i64 0, i64 2, i64 5, i64 0, i64 5, i64 0, i64 37, i64 5263188, i64 3028273, i64 7102819, i64 7626604, i64 7234920, i64 7102815, i64 6516588, i64 6911839, i64 6252660, i64 7760236, i64 7495525, i64 7630703, i64 115), "gc-live"(ptr %9, ptr %10) ]
  %33 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 1
  store i64 %33, ptr %ds
  store i64 %34, ptr %alloc
  %35 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 1, 0
  store ptr addrspace(1) %35, ptr %6
  br label %L419
L419:
  %36 = load ptr addrspace(1), ptr %6
  store ptr addrspace(1) %36, ptr %13
  %37 = load ptr addrspace(1), ptr %13
  store ptr addrspace(1) %37, ptr %14
  %38 = load i64, ptr %alloc
  %39 = load i64, ptr %ds
  %40 = add i64 %39, 8
  %41 = inttoptr i64 %40 to ptr
  store i64 %38, ptr %41
  %42 = load i64, ptr %ds
  %43 = load i64, ptr %alloc
  %44 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap.0..0"(i64 %42, i64 %43) "gc-leaf-function"="true"
  %45 = extractvalue { { i64, i64 }, {  } } %44, 0, 0
  %46 = extractvalue { { i64, i64 }, {  } } %44, 0, 1
  store i64 %45, ptr %ds
  store i64 %46, ptr %alloc
  %47 = load i64, ptr %alloc
  %48 = load i64, ptr %ds
  %49 = load i64, ptr %alloc
  %50 = call oxcaml_fpcc { { i64, i64 }, {  } } @"\01_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0"(i64 %48, i64 %49, i64 %47, i64 886829140, i64 13) "gc-leaf-function"="true"
  %51 = extractvalue { { i64, i64 }, {  } } %50, 0, 0
  %52 = extractvalue { { i64, i64 }, {  } } %50, 0, 1
  store i64 %51, ptr %ds
  store i64 %52, ptr %alloc
  %53 = load i64, ptr %alloc
  %54 = sub i64 %53, 24
  store i64 %54, ptr %alloc
  %55 = load i64, ptr %ds
  %56 = inttoptr i64 %55 to ptr
  %57 = load i64, ptr %56
  %58 = icmp ule i64 %57, %54
  %59 = call  i1 @llvm.expect.i1(i1 %58, i1 1)
  br i1 %59, label %L423, label %L422
L422:
  %60 = load volatile ptr addrspace(1), ptr %9
  store volatile ptr addrspace(1) %60, ptr %17
  %61 = load volatile ptr addrspace(1), ptr %10
  store volatile ptr addrspace(1) %61, ptr %18
  %62 = load i64, ptr %ds
  %63 = load i64, ptr %alloc
  %64 = call oxcaml_alloccc { { i64, i64 }, {  } } @"\01_caml_call_gc"(i64 %62, i64 %63) "statepoint-id"="196609" cold [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 2, i64 0, i64 13, i64 17, i64 0, i64 17, i64 0, i64 37, i64 5263188, i64 3028273, i64 7102819, i64 7626604, i64 7234920, i64 7102815, i64 6516588, i64 6911839, i64 6252660, i64 7760236, i64 7495525, i64 7630703, i64 115), "gc-live"(ptr %17, ptr %18) ]
  %65 = extractvalue { { i64, i64 }, {  } } %64, 0, 0
  %66 = extractvalue { { i64, i64 }, {  } } %64, 0, 1
  store i64 %65, ptr %ds
  store i64 %66, ptr %alloc
  %67 = load volatile ptr addrspace(1), ptr %17
  store volatile ptr addrspace(1) %67, ptr %9
  %68 = load volatile ptr addrspace(1), ptr %18
  store volatile ptr addrspace(1) %68, ptr %10
  br label %L423
L423:
  %69 = load i64, ptr %alloc
  %70 = add i64 %69, 8
  %71 = inttoptr i64 %70 to ptr addrspace(1)
  store ptr addrspace(1) %71, ptr %15
  %72 = load ptr addrspace(1), ptr %15
  %73 = ptrtoint ptr addrspace(1) %72 to i64
  %74 = add i64 %73, -8
  %75 = inttoptr i64 %74 to ptr
  store volatile i64 2048, ptr %75
  %76 = load ptr addrspace(1), ptr %15
  %77 = addrspacecast ptr addrspace(1) %76 to ptr
  %78 = load volatile ptr addrspace(1), ptr %9
  store ptr addrspace(1) %78, ptr %77
  %79 = load ptr addrspace(1), ptr %15
  %80 = ptrtoint ptr addrspace(1) %79 to i64
  %81 = add i64 %80, 8
  %82 = inttoptr i64 %81 to ptr
  %83 = load volatile ptr addrspace(1), ptr %10
  store ptr addrspace(1) %83, ptr %82
  %84 = load ptr addrspace(1), ptr %15
  store ptr addrspace(1) %84, ptr %6
  %85 = load ptr addrspace(1), ptr %6
  %86 = load i64, ptr %ds
  %87 = load i64, ptr %alloc
  %88 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %86, 0, 0
  %89 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %88, i64 %87, 0, 1
  %90 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %89, ptr addrspace(1) %85, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %90
}|}]
[%%expect_llvm_asm AArch64{|_camlTOP__call_then_alloc_with_live_roots_26_27_code:
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	mov	x17, x30
	ldr	x16, [x28, #40]
	adrp	x30, _caml_plat_pagesize@GOTPAGE
	ldr	x30, [x30, _caml_plat_pagesize@GOTPAGEOFF]
	ldr	x30, [x30]
	add	x16, x16, x30, lsl #1
	mov	x30, #38
	add	x16, x16, x30, lsl #3
	cmp	sp, x16
	b.hs	Ltmp0
	mov	x16, #38
	bl	_caml_llvm_prologue_realloc_stack
Ltmp0:
	mov	x30, x17
	; InlineAsm End
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	mov	x29, sp
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	str	x0, [sp, #24]
	str	x1, [sp, #16]
	ldr	x8, [x2]
	ldr	x0, [sp, #24]
	mov	x1, x2
	blr	x8
Ltmp1:
	str	x27, [x28, #8]
	bl	_c_call_wrapper.caml_debug_check_minor_heap.0..0
	mov	x0, x27
	mov	x27, x0
	mov	w1, #61524
	movk	w1, #13531, lsl #16
	mov	w2, #13
	bl	_c_call_wrapper.caml_debug_check_minor_heap_head.3.ptr.i64.i64.0
	mov	x1, x28
	sub	x8, x27, #24
	ldr	x9, [x28]
	cmp	x9, x8
	b.hi	LBB0_2
LBB0_1:                                 ; %L423
	mov	w9, #2048
	str	x9, [x8]
	ldr	x9, [sp, #24]
	mov	x0, x8
	str	x9, [x0, #8]!
	ldr	x9, [sp, #16]
	str	x9, [x8, #16]
	mov	x28, x1
	mov	x27, x8
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	add	sp, sp, #48
	ret
LBB0_2:                                 ; %L422
	ldr	x9, [sp, #24]
	str	x9, [sp, #8]
	ldr	x9, [sp, #16]
	str	x9, [sp]
	mov	x28, x1
	mov	x27, x8
	bl	_caml_call_gc
Ltmp2:
	mov	x8, x27
	mov	x1, x28
	ldr	x9, [sp, #8]
	str	x9, [sp, #24]
	ldr	x9, [sp]
	str	x9, [sp, #16]
	b	LBB0_1
	.cfi_endproc|}]
