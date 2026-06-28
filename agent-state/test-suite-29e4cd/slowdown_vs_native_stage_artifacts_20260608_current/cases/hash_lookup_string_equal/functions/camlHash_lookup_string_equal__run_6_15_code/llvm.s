_camlHash_lookup_string_equal__run_6_15_code: ; @"\01_camlHash_lookup_string_equal__run_6_15_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #96
	.cfi_def_cfa_offset 112
	str	x0, [sp, #32]                   ; 8-byte Folded Spill
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	str	x1, [sp, #40]                   ; 8-byte Folded Spill
	b.lo	LBB5_23
LBB5_1:                                 ; %L280
Lloh4:
	adrp	x2, _camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a24$2c17$2d$2d70$5d_15@PAGE
Lloh5:
	add	x2, x2, _camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a24$2c17$2d$2d70$5d_15@PAGEOFF
	mov	w0, #1
	mov	w1, #63
	bl	_camlStdlib__List__init_11_109_code
Ltmp2:
	mov	x1, x0
	sub	x27, x27, #16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB5_24
LBB5_2:                                 ; %L282
	mov	w8, #1024
	str	x8, [x27]
	mov	x8, x27
	str	x1, [x8, #8]!
	str	x8, [sp, #88]
Lloh6:
	adrp	x0, _camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a27$2c37$2d$2d40$5d_16@PAGE
Lloh7:
	add	x0, x0, _camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a27$2c37$2d$2d40$5d_16@PAGEOFF
	bl	_camlStdlib__List__map_15_113_code
Ltmp3:
	tbnz	w0, #0, LBB5_13
; %bb.3:                                ; %L172.preheader
	mov	w9, #1
	mov	x8, x0
LBB5_4:                                 ; %L172
                                        ; =>This Inner Loop Header: Depth=1
	ldr	x8, [x8, #8]
	add	x9, x9, #2
	tbz	w8, #0, LBB5_4
; %bb.5:                                ; %L170
	ldr	x1, [x0]
	str	x9, [sp, #72]
	str	x0, [sp, #80]
	str	x1, [sp, #64]
Lloh8:
	adrp	x8, _caml_array_make@GOTPAGE
Lloh9:
	ldr	x8, [x8, _caml_array_make@GOTPAGEOFF]
	mov	x0, x9
	bl	_caml_c_call
Ltmp4:
	mov	x20, x0
	ldr	x19, [sp, #88]
	ldr	x8, [sp, #80]
	ldr	x22, [x8, #8]
	tbnz	w22, #0, LBB5_10
; %bb.6:                                ; %L192.preheader
	add	x21, x20, #8
	b	LBB5_8
LBB5_7:                                 ; %L207
                                        ;   in Loop: Header=BB5_8 Depth=1
	mov	x0, x21
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_modify
	mov	sp, x29
	.cfi_restore_state
	ldr	x22, [x22, #8]
	add	x21, x21, #8
	tbnz	w22, #0, LBB5_10
LBB5_8:                                 ; %L192
                                        ; =>This Inner Loop Header: Depth=1
	ldr	x1, [x22]
	ldurb	w8, [x20, #-8]
	cmp	w8, #254
	b.ne	LBB5_7
; %bb.9:                                ; %L200
                                        ;   in Loop: Header=BB5_8 Depth=1
	ldr	d0, [x1]
	str	d0, [x21], #8
	ldr	x22, [x22, #8]
	tbz	w22, #0, LBB5_8
LBB5_10:                                ; %L190
	ldr	x8, [sp, #40]                   ; 8-byte Folded Reload
	cmp	x8, #3
	b.ge	LBB5_16
; %bb.11:
	mov	w0, #1
LBB5_12:                                ; %common.ret
	ldr	x30, [sp, #104]                 ; 8-byte Folded Reload
	add	sp, sp, #112
	ret
LBB5_13:                                ; %L158
	ldr	x8, [sp, #40]                   ; 8-byte Folded Reload
	cmp	x8, #3
	b.ge	LBB5_15
; %bb.14:
	mov	w0, #1
	ldr	x30, [sp, #104]                 ; 8-byte Folded Reload
	add	sp, sp, #112
	ret
LBB5_15:
	ldr	x19, [sp, #88]
Lloh10:
	adrp	x20, _camlStdlib__Array__empty_array50@GOTPAGE
Lloh11:
	ldr	x20, [x20, _camlStdlib__Array__empty_array50@GOTPAGEOFF]
LBB5_16:                                ; %L226
	lsr	x9, x8, #1
	ldr	x8, [sp, #32]                   ; 8-byte Folded Reload
	cmp	x8, #3
	b.ge	LBB5_19
; %bb.17:                               ; %L232.us.preheader
	mov	w0, #1
	mov	w8, #1
LBB5_18:                                ; %L232.us
                                        ; =>This Inner Loop Header: Depth=1
	add	x8, x8, #1
	cmp	x8, x9
	b.le	LBB5_18
	b	LBB5_12
LBB5_19:                                ; %L232.preheader
	lsr	x8, x8, #1
	mov	w0, #1
	mov	w10, #1
	str	x9, [sp]                        ; 8-byte Folded Spill
	str	x8, [sp, #16]                   ; 8-byte Folded Spill
LBB5_20:                                ; %L232
                                        ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB5_21 Depth 2
	str	x19, [sp, #48]
	str	x20, [sp, #56]
	mov	w9, #1
	str	x10, [sp, #8]                   ; 8-byte Folded Spill
	orr	x9, x9, x10, lsl #1
	str	x9, [sp, #24]                   ; 8-byte Folded Spill
	mov	w9, #1
LBB5_21:                                ; %L244
                                        ;   Parent Loop BB5_20 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	str	x0, [sp, #32]                   ; 8-byte Folded Spill
	str	x9, [sp, #40]                   ; 8-byte Folded Spill
	ldr	x8, [sp, #24]                   ; 8-byte Folded Reload
	add	w8, w8, w9, lsl #1
	ubfiz	x8, x8, #2, #6
	add	x8, x20, x8
	ldur	x0, [x8, #-4]
	mov	x1, x19
	bl	_camlHash_lookup_string_equal__find_4_13_code
Ltmp5:
	ldr	x9, [sp, #40]                   ; 8-byte Folded Reload
	ldr	x19, [sp, #48]
	ldr	x20, [sp, #56]
	ldr	x8, [sp, #32]                   ; 8-byte Folded Reload
	add	x8, x8, x0
	sub	x0, x8, #1
	ldr	x8, [sp, #16]                   ; 8-byte Folded Reload
	add	x9, x9, #1
	cmp	x9, x8
	b.le	LBB5_21
; %bb.22:                               ; %L265.loopexit
                                        ;   in Loop: Header=BB5_20 Depth=1
	ldr	x10, [sp, #8]                   ; 8-byte Folded Reload
	add	x10, x10, #1
	ldr	x9, [sp]                        ; 8-byte Folded Reload
	cmp	x10, x9
	b.le	LBB5_20
	b	LBB5_12
LBB5_23:                                ; %L279
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp6:
	b	LBB5_1
LBB5_24:                                ; %L281
	bl	_caml_call_gc
Ltmp7:
	b	LBB5_2
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh6, Lloh7
	.loh AdrpLdrGot	Lloh8, Lloh9
	.loh AdrpLdrGot	Lloh10, Lloh11
	.cfi_endproc
                                        ; -- End function
	.globl	_camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a24$2c17$2d$2d70$5d_7_16_code ; -- Begin function _camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a24$2c17$2d$2d70$5d_7_16_code
	.p2align	2
