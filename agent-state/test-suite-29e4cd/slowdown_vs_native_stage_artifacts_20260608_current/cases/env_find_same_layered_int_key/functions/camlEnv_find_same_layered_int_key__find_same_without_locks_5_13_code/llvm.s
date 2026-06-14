_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code: ; @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code"
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
	mov	w8, #1
	str	x8, [sp, #24]
	str	x8, [sp, #16]
	ldr	x8, [x28, #40]
	add	x8, x8, #408
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB5_6
LBB5_1:                                 ; %L147
                                        ; =>This Inner Loop Header: Depth=1
	ldp	x1, x8, [x1]
	ldr	x10, [x28, #64]
	str	x10, [sp, #8]                   ; 8-byte Folded Spill
	adr	x16, LBB5_2
	sub	sp, sp, #16
	str	x26, [sp]
	str	x16, [sp, #8]
	mov	x26, sp
	str	x8, [sp, #40]
	str	x9, [sp, #32]
	mov	x0, x9
	bl	_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code
Ltmp3:
	b	LBB5_5
Ltmp4:                                  ; Block address taken
LBB5_2:                                 ; %L182
                                        ;   in Loop: Header=BB5_1 Depth=1
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
	ldr	x8, [sp, #24]
	ldr	x9, [sp, #8]                    ; 8-byte Folded Reload
	str	x9, [x28, #64]
Lloh6:
	adrp	x9, _camlEnv_find_same_layered_int_key__Not_found_same292@PAGE
Lloh7:
	add	x9, x9, _camlEnv_find_same_layered_int_key__Not_found_same292@PAGEOFF
	cmp	x0, x9
	b.ne	LBB5_7
; %bb.3:                                ; %L163
                                        ;   in Loop: Header=BB5_1 Depth=1
	tbnz	w8, #0, LBB5_8
; %bb.4:                                ; %L167
                                        ;   in Loop: Header=BB5_1 Depth=1
	ldr	x1, [x8]
	ldr	x9, [sp, #16]
	b	LBB5_1
LBB5_5:                                 ; %L183
	ldr	x26, [sp], #16
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB5_6:                                 ; %L179
	mov	w0, #38
	bl	_caml_llvm_call_realloc_stack
Ltmp5:
	b	LBB5_1
LBB5_7:                                 ; %L174
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
LBB5_8:                                 ; %L165
Lloh8:
	adrp	x8, _camlEnv_find_same_layered_int_key__Not_found_same292@PAGE
Lloh9:
	add	x8, x8, _camlEnv_find_same_layered_int_key__Not_found_same292@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpAdd	Lloh6, Lloh7
	.loh AdrpAdd	Lloh8, Lloh9
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
	.globl	_camlEnv_find_same_layered_int_key__open_layers_6_14_code ; -- Begin function _camlEnv_find_same_layered_int_key__open_layers_6_14_code
	.p2align	2
