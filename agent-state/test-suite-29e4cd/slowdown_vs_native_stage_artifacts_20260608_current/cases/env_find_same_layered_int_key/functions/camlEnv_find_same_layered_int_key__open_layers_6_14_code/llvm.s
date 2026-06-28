_camlEnv_find_same_layered_int_key__open_layers_6_14_code: ; @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	cmp	x0, #1
	b.eq	LBB6_10
; %bb.1:                                ; %L193
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	mov	x9, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB6_5
; %bb.2:                                ; %L214
	and	x8, x9, #0x3
	sub	x0, x9, #2
	cmp	x8, #1
	b.ne	LBB6_6
LBB6_3:                                 ; %L196
	bl	_camlEnv_find_same_layered_int_key__open_layers_6_14_code
Ltmp6:
	sub	x27, x27, #16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB6_12
LBB6_4:                                 ; %L217
	mov	x9, x27
	str	x0, [x9, #8]!
	mov	w8, #1024
	b	LBB6_8
LBB6_5:                                 ; %L213
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp7:
	and	x8, x9, #0x3
	sub	x0, x9, #2
	cmp	x8, #1
	b.eq	LBB6_3
LBB6_6:                                 ; %L201
	bl	_camlEnv_find_same_layered_int_key__open_layers_6_14_code
Ltmp8:
	sub	x27, x27, #16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB6_13
LBB6_7:                                 ; %L219
	mov	x9, x27
	str	x0, [x9, #8]!
	mov	w8, #1025
LBB6_8:                                 ; %L207
	str	x8, [x27], #-24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB6_11
LBB6_9:                                 ; %L221
	mov	w8, #2048
	str	x8, [x27]
	mov	w8, #1
	mov	x1, x27
	str	x8, [x1, #8]!
	str	x9, [x1, #8]
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
LBB6_10:                                ; %common.ret
	.cfi_def_cfa wsp, 0
	.cfi_same_value w30
	mov	x0, x1
	ret
LBB6_11:                                ; %L220
	bl	_caml_call_gc
Ltmp9:
	b	LBB6_9
LBB6_12:                                ; %L216
	bl	_caml_call_gc
Ltmp10:
	b	LBB6_4
LBB6_13:                                ; %L218
	bl	_caml_call_gc
Ltmp11:
	b	LBB6_7
	.cfi_endproc
                                        ; -- End function
	.globl	_camlEnv_find_same_layered_int_key__run_7_15_code ; -- Begin function _camlEnv_find_same_layered_int_key__run_7_15_code
	.p2align	2
