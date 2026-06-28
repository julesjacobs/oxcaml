_camlBoyer__unify1_10_30_code:          ; @"\01_camlBoyer__unify1_10_30_code"
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception0
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #48
	.cfi_def_cfa_offset 64
	mov	x9, x0
	mov	w8, #1
	str	x8, [sp, #40]
	str	x8, [sp, #32]
	str	x8, [sp, #24]
	ldurb	w8, [x1, #-8]
	cbz	w8, LBB9_4
; %bb.1:                                ; %L490
	ldurb	w8, [x9, #-8]
	cbz	w8, LBB9_12
; %bb.2:                                ; %L494
	ldr	x8, [x1]
	ldr	x10, [x9]
	cmp	x10, x8
	b.ne	LBB9_12
; %bb.3:                                ; %L498
	ldr	x1, [x1, #8]
	ldr	x0, [x9, #8]
	ldr	x30, [sp, #56]                  ; 8-byte Folded Reload
	add	sp, sp, #64
	b	_camlBoyer__unify1_lst_11_31_code
LBB9_4:                                 ; %L511
	ldr	x8, [x1]
	ldr	x13, [x28, #64]
	adr	x16, LBB9_14
	sub	sp, sp, #16
	str	x26, [sp]
	str	x16, [sp, #8]
	mov	x26, sp
	str	x13, [sp, #16]                  ; 8-byte Folded Spill
	tbnz	w2, #0, LBB9_8
; %bb.5:
	mov	x10, x2
LBB9_6:                                 ; %L533
                                        ; =>This Inner Loop Header: Depth=1
	ldr	x11, [x10]
	ldr	x12, [x11]
	cmp	x8, x12
	b.eq	LBB9_9
; %bb.7:                                ; %L541
                                        ;   in Loop: Header=BB9_6 Depth=1
	ldr	x10, [x10, #8]
	tbz	w10, #0, LBB9_6
LBB9_8:                                 ; %L531
	str	x8, [sp, #56]
	str	x9, [sp, #48]
Lloh41:
	adrp	x8, _camlBoyer__Pmakeblock3333@PAGE
Lloh42:
	add	x8, x8, _camlBoyer__Pmakeblock3333@PAGEOFF
	str	x2, [sp, #40]
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
LBB9_9:                                 ; %L537
	ldr	x0, [x11, #8]
	str	x8, [sp, #56]
	str	x9, [sp, #48]
	str	x2, [sp, #40]
	str	x0, [sp, #32]
	str	x9, [sp, #24]
Lloh43:
	adrp	x8, _caml_equal@GOTPAGE
Lloh44:
	ldr	x8, [x8, _caml_equal@GOTPAGEOFF]
	mov	x1, x9
	bl	_caml_c_call
Ltmp23:
; %bb.10:                               ; %L576
	mov	x8, x0
	ldr	x9, [sp, #56]
	ldr	x10, [sp, #48]
	ldr	x0, [sp, #40]
	cmp	x8, #1
	b.eq	LBB9_13
; %bb.11:                               ; %L553
	ldr	x26, [sp], #16
	ldr	x30, [sp, #56]                  ; 8-byte Folded Reload
	add	sp, sp, #64
	ret
LBB9_12:                                ; %L508
Lloh45:
	adrp	x8, _camlBoyer__Unify3423@PAGE
Lloh46:
	add	x8, x8, _camlBoyer__Unify3423@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
LBB9_13:                                ; %L551
	str	x9, [sp, #56]
	str	x10, [sp, #48]
Lloh47:
	adrp	x8, _camlBoyer__Unify3423@PAGE
Lloh48:
	add	x8, x8, _camlBoyer__Unify3423@PAGEOFF
	str	x0, [sp, #40]
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
Ltmp24:                                 ; Block address taken
LBB9_14:                                ; %L573
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
Ltmp0:
	ldr	x11, [sp, #40]
	ldr	x10, [sp, #32]
	ldr	x9, [sp, #24]
	ldr	x8, [sp]                        ; 8-byte Folded Reload
	str	x8, [x28, #64]
	ldr	x8, [x0]
Lloh49:
	adrp	x12, _caml_exn_Failure@GOTPAGE
Lloh50:
	ldr	x12, [x12, _caml_exn_Failure@GOTPAGEOFF]
	cmp	x8, x12
	b.ne	LBB9_18
; %bb.15:                               ; %L562
	sub	x27, x27, #48
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB9_17
LBB9_16:                                ; %L581
	mov	x8, x27
	str	x11, [x8, #32]!
	mov	x0, x27
	str	x8, [x0, #8]!
	mov	w8, #2048
	str	x8, [x27, #24]
	str	x10, [x27, #40]
	str	x8, [x27]
	str	x9, [x27, #16]
	ldr	x30, [sp, #56]                  ; 8-byte Folded Reload
	add	sp, sp, #64
	ret
LBB9_17:                                ; %L580
	bl	_caml_call_gc
Ltmp25:
	b	LBB9_16
LBB9_18:                                ; %L566
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpAdd	Lloh41, Lloh42
	.loh AdrpLdrGot	Lloh43, Lloh44
	.loh AdrpAdd	Lloh45, Lloh46
	.loh AdrpAdd	Lloh47, Lloh48
	.loh AdrpLdrGot	Lloh49, Lloh50
Lfunc_end0:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table9:
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
	.p2align	2, 0x0
                                        ; -- End function
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlBoyer__unify1_lst_11_31_code ; -- Begin function _camlBoyer__unify1_lst_11_31_code
	.p2align	2
