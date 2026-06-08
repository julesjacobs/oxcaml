_camlBoyer__rewrite_with_lemmas_13_33_code: ; @"\01_camlBoyer__rewrite_with_lemmas_13_33_code"
Lfunc_begin1:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception1
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #64
	.cfi_def_cfa_offset 80
	mov	x9, x0
	mov	w8, #1
	str	x8, [sp, #56]
	str	x8, [sp, #48]
	str	x8, [sp, #40]
	str	x8, [sp, #32]
	ldr	x8, [x28, #40]
	add	x8, x8, #408
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB12_13
; %bb.1:                                ; %L671
	tbnz	w1, #0, LBB12_12
LBB12_2:
	mov	x0, x9
LBB12_3:                                ; %L637
                                        ; =>This Inner Loop Header: Depth=1
	ldr	x8, [x1]
	ldr	x10, [x28, #64]
	str	x10, [sp, #8]                   ; 8-byte Folded Spill
	adr	x16, LBB12_8
	sub	sp, sp, #16
	str	x26, [sp]
	str	x16, [sp, #8]
	mov	x26, sp
	ldp	x8, x10, [x8]
	str	x1, [sp, #64]
	str	x0, [sp, #56]
	str	x0, [sp, #48]
	str	x9, [sp, #72]
	str	x10, [sp, #40]
	str	x9, [sp, #32]
	mov	x1, x8
	bl	_camlBoyer__unify_9_29_code
Ltmp31:
; %bb.4:                                ; %L674
                                        ;   in Loop: Header=BB12_3 Depth=1
	mov	x9, x0
	ldr	x0, [sp, #40]
	ldr	x11, [sp, #64]
	ldr	x10, [sp, #56]
	ldr	x12, [sp, #32]
	sub	x27, x27, #40
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB12_7
LBB12_5:                                ; %L676
                                        ;   in Loop: Header=BB12_3 Depth=1
	mov	w8, #4343
	str	x8, [x27]
	mov	x1, x27
Lloh55:
	adrp	x8, _camlBoyer__as_rec_8_34_code@PAGE
Lloh56:
	add	x8, x8, _camlBoyer__as_rec_8_34_code@PAGEOFF
	str	x8, [x1, #8]!
	mov	x8, #5
	movk	x8, #384, lsl #48
	str	x8, [x27, #16]
	stp	x0, x9, [x27, #24]
	str	x11, [sp, #64]
	str	x10, [sp, #56]
	str	x12, [sp, #48]
	str	x12, [sp, #72]
	str	x10, [sp, #40]
	bl	_camlBoyer__as_rec_8_34_code
Ltmp32:
; %bb.6:                                ; %L677
                                        ;   in Loop: Header=BB12_3 Depth=1
	ldr	x8, [sp, #40]
	ldr	x9, [sp, #64]
	ldr	x10, [sp, #56]
	str	x9, [sp, #64]
	str	x8, [sp, #56]
	str	x10, [sp, #48]
	str	x10, [sp, #72]
	bl	_camlBoyer__rewrite_12_32_code
Ltmp33:
	b	LBB12_11
LBB12_7:                                ; %L675
                                        ;   in Loop: Header=BB12_3 Depth=1
	bl	_caml_call_gc
Ltmp34:
	b	LBB12_5
Ltmp35:                                 ; Block address taken
LBB12_8:                                ; %L673
                                        ;   in Loop: Header=BB12_3 Depth=1
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
Ltmp1:
	ldr	x9, [sp, #56]
	ldr	x8, [sp, #48]
	ldr	x10, [sp, #8]                   ; 8-byte Folded Reload
	str	x10, [x28, #64]
Lloh57:
	adrp	x10, _camlBoyer__Unify3423@PAGE
Lloh58:
	add	x10, x10, _camlBoyer__Unify3423@PAGEOFF
	cmp	x0, x10
	b.ne	LBB12_14
; %bb.9:                                ; %L660
                                        ;   in Loop: Header=BB12_3 Depth=1
	ldr	x1, [x8, #8]
	ldr	x0, [sp, #40]
	tbz	w1, #0, LBB12_3
; %bb.10:                               ; %L635.loopexit
	ldr	x9, [sp, #32]
	b	LBB12_12
LBB12_11:                               ; %L678
	mov	x9, x0
	ldr	x8, [sp, #56]
	ldr	x26, [sp], #16
LBB12_12:                               ; %common.ret
	mov	x0, x9
	ldr	x30, [sp, #72]                  ; 8-byte Folded Reload
	add	sp, sp, #80
	ret
LBB12_13:                               ; %L670
	mov	w0, #38
	bl	_caml_llvm_call_realloc_stack
Ltmp36:
	tbz	w1, #0, LBB12_2
	b	LBB12_12
LBB12_14:                               ; %L665
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpAdd	Lloh55, Lloh56
	.loh AdrpAdd	Lloh57, Lloh58
Lfunc_end1:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table12:
Lexception1:
	.byte	255                             ; @LPStart Encoding = omit
	.byte	255                             ; @TType Encoding = omit
	.byte	1                               ; Call site Encoding = uleb128
	.uleb128 Lcst_end1-Lcst_begin1
Lcst_begin1:
	.uleb128 Lfunc_begin1-Lfunc_begin1      ; >> Call Site 1 <<
	.uleb128 Lfunc_end1-Lfunc_begin1        ;   Call between Lfunc_begin1 and Lfunc_end1
	.byte	0                               ;     has no landing pad
	.byte	0                               ;   On action: cleanup
Lcst_end1:
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlBoyer__cterm_to_term_14_35_code ; -- Begin function _camlBoyer__cterm_to_term_14_35_code
	.p2align	2
