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
