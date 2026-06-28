_camlTry_raise_cross_function_caught__find_5_12_code: ; @"\01_camlTry_raise_cross_function_caught__find_5_12_code"
Lfunc_begin0:
	.cfi_startproc
	.cfi_personality 155, _caml_llvm_eh_personality
	.cfi_lsda 16, Lexception0
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	mov	x9, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #408
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB5_3
; %bb.1:                                ; %L151
	ldr	x8, [x28, #64]
	str	x8, [sp, #8]                    ; 8-byte Folded Spill
	adr	x16, LBB5_4
	sub	sp, sp, #16
	str	x26, [sp]
	str	x16, [sp, #8]
	mov	x26, sp
	mov	x0, x9
	bl	_camlTry_raise_cross_function_caught__probe_4_11_code
Ltmp3:
LBB5_2:                                 ; %L154
	ldr	x26, [sp], #16
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB5_3:                                 ; %L150
	mov	w0, #38
	bl	_caml_llvm_call_realloc_stack
Ltmp4:
	ldr	x8, [x28, #64]
	str	x8, [sp, #8]                    ; 8-byte Folded Spill
	adr	x16, LBB5_4
	sub	sp, sp, #16
	str	x26, [sp]
	str	x16, [sp, #8]
	mov	x26, sp
	mov	x0, x9
	bl	_camlTry_raise_cross_function_caught__probe_4_11_code
Ltmp5:
	b	LBB5_2
Ltmp6:                                  ; Block address taken
LBB5_4:                                 ; %L153
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
	ldr	x8, [sp, #8]                    ; 8-byte Folded Reload
	str	x8, [x28, #64]
Lloh6:
	adrp	x8, _camlTry_raise_cross_function_caught__Miss245@PAGE
Lloh7:
	add	x8, x8, _camlTry_raise_cross_function_caught__Miss245@PAGEOFF
	cmp	x0, x8
	b.ne	LBB5_6
; %bb.5:                                ; %L144
	mov	w0, #3
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB5_6:                                 ; %L146
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpAdd	Lloh6, Lloh7
Lfunc_end0:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.p2align	2, 0x0
GCC_except_table5:
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
	.globl	_camlTry_raise_cross_function_caught__run_6_13_code ; -- Begin function _camlTry_raise_cross_function_caught__run_6_13_code
	.p2align	2
