	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	_camlTry_raise_cross_function_caught__code_begin
_camlTry_raise_cross_function_caught__code_begin:
	.section	__DATA,__data
	.globl	_camlTry_raise_cross_function_caught__data_begin
_camlTry_raise_cross_function_caught__data_begin:
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlTry_raise_cross_function_caught__black_box_int_0_7_code ; -- Begin function _camlTry_raise_cross_function_caught__black_box_int_0_7_code
	.p2align	2
_camlTry_raise_cross_function_caught__black_box_int_0_7_code: ; @"\01_camlTry_raise_cross_function_caught__black_box_int_0_7_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__black_box_string_1_8_code ; -- Begin function _camlTry_raise_cross_function_caught__black_box_string_1_8_code
	.p2align	2
_camlTry_raise_cross_function_caught__black_box_string_1_8_code: ; @"\01_camlTry_raise_cross_function_caught__black_box_string_1_8_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__black_box_2_9_code ; -- Begin function _camlTry_raise_cross_function_caught__black_box_2_9_code
	.p2align	2
_camlTry_raise_cross_function_caught__black_box_2_9_code: ; @"\01_camlTry_raise_cross_function_caught__black_box_2_9_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__print_result_3_10_code ; -- Begin function _camlTry_raise_cross_function_caught__print_result_3_10_code
	.p2align	2
_camlTry_raise_cross_function_caught__print_result_3_10_code: ; @"\01_camlTry_raise_cross_function_caught__print_result_3_10_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	b.lo	LBB3_2
LBB3_1:                                 ; %L117
Lloh0:
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
Lloh1:
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
Lloh2:
	adrp	x2, _camlTry_raise_cross_function_caught__const_block66@PAGE
Lloh3:
	add	x2, x2, _camlTry_raise_cross_function_caught__const_block66@PAGEOFF
	mov	w1, #1
	bl	_camlCamlinternalFormat__make_printf_120_401_code
Ltmp1:
	mov	x1, x0
	ldr	x8, [sp, #8]                    ; 8-byte Folded Reload
	and	x0, x8, #0x7fffffff
	ldr	x2, [x1]
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	br	x2
LBB3_2:                                 ; %L116
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp2:
	b	LBB3_1
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpLdrGot	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__probe_4_11_code ; -- Begin function _camlTry_raise_cross_function_caught__probe_4_11_code
	.p2align	2
_camlTry_raise_cross_function_caught__probe_4_11_code: ; @"\01_camlTry_raise_cross_function_caught__probe_4_11_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	cmp	x0, #1
	b.eq	LBB4_2
; %bb.1:                                ; %L123
	ret
LBB4_2:                                 ; %L121
Lloh4:
	adrp	x8, _camlTry_raise_cross_function_caught__Miss245@PAGE
Lloh5:
	add	x8, x8, _camlTry_raise_cross_function_caught__Miss245@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpAdd	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__find_5_12_code ; -- Begin function _camlTry_raise_cross_function_caught__find_5_12_code
	.p2align	2
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
_camlTry_raise_cross_function_caught__run_6_13_code: ; @"\01_camlTry_raise_cross_function_caught__run_6_13_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #48
	.cfi_def_cfa_offset 64
	cmp	x1, #3
	b.ge	LBB6_3
; %bb.1:
	mov	w0, #1
LBB6_2:                                 ; %common.ret
	ldr	x30, [sp, #56]                  ; 8-byte Folded Reload
	add	sp, sp, #64
	ret
LBB6_3:                                 ; %L159
	mov	x10, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	b.lo	LBB6_10
LBB6_4:                                 ; %L206
	lsr	x8, x1, #1
	mov	w0, #1
	mov	w9, #1
	cmp	x10, #3
	str	x10, [sp, #8]                   ; 8-byte Folded Spill
	str	x8, [sp]                        ; 8-byte Folded Spill
	b.ge	LBB6_7
LBB6_5:                                 ; %L191
	add	x9, x9, #1
	cmp	x9, x8
	b.gt	LBB6_2
LBB6_6:                                 ; %L194
	cmp	x10, #3
	b.lt	LBB6_5
LBB6_7:                                 ; %L170
	str	x9, [sp, #16]                   ; 8-byte Folded Spill
	lsr	x8, x10, #1
	str	x8, [sp, #24]                   ; 8-byte Folded Spill
	mov	w9, #1
LBB6_8:                                 ; %L176
                                        ; =>This Inner Loop Header: Depth=1
	str	x0, [sp, #32]                   ; 8-byte Folded Spill
	str	x9, [sp, #40]                   ; 8-byte Folded Spill
	mov	w0, #1
	bl	_camlTry_raise_cross_function_caught__find_5_12_code
Ltmp7:
	ldr	x9, [sp, #40]                   ; 8-byte Folded Reload
	ldr	x8, [sp, #32]                   ; 8-byte Folded Reload
	add	x8, x8, x0
	sub	x0, x8, #1
	add	x9, x9, #1
	ldr	x8, [sp, #24]                   ; 8-byte Folded Reload
	cmp	x9, x8
	b.le	LBB6_8
; %bb.9:                                ; %L191.loopexit
	ldr	x10, [sp, #8]                   ; 8-byte Folded Reload
	ldr	x8, [sp]                        ; 8-byte Folded Reload
	ldr	x9, [sp, #16]                   ; 8-byte Folded Reload
	add	x9, x9, #1
	cmp	x9, x8
	b.le	LBB6_6
	b	LBB6_2
LBB6_10:                                ; %L205
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp8:
	b	LBB6_4
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__entry ; -- Begin function _camlTry_raise_cross_function_caught__entry
	.p2align	2
_camlTry_raise_cross_function_caught__entry: ; @"\01_camlTry_raise_cross_function_caught__entry"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
Lloh8:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh9:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp9:
	ldur	x8, [x0, #-8]
	tst	x8, #0xfffffffffff800
	b.eq	LBB7_3
; %bb.1:                                ; %L225
Lloh10:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh11:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp10:
	ldur	x8, [x0, #-8]
	tst	x8, #0xfffffffffff800
	b.eq	LBB7_11
; %bb.2:                                ; %L235
	ldr	x0, [x0, #8]
	str	x0, [sp, #24]
Lloh12:
	adrp	x8, _caml_int_of_string@GOTPAGE
Lloh13:
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp11:
	mov	x20, x0
	b	LBB7_4
LBB7_3:
	mov	w20, #3393
	movk	w20, #3, lsl #16
LBB7_4:                                 ; %L249
Lloh14:
	adrp	x0, _camlTry_raise_cross_function_caught@PAGE+24
Lloh15:
	add	x0, x0, _camlTry_raise_cross_function_caught@PAGEOFF+24
	mov	x1, x20
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x29
	.cfi_restore_state
Lloh16:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh17:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp12:
	ldur	x8, [x0, #-8]
	and	x8, x8, #0xfffffffffffc00
	cmp	x8, #2561
	b.lo	LBB7_7
; %bb.5:                                ; %L261
Lloh18:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh19:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp13:
	ldur	x8, [x0, #-8]
	and	x8, x8, #0xfffffffffffc00
	cmp	x8, #2561
	b.lo	LBB7_11
; %bb.6:                                ; %L271
	ldr	x0, [x0, #16]
	str	x0, [sp, #24]
Lloh20:
	adrp	x8, _caml_int_of_string@GOTPAGE
Lloh21:
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp14:
	mov	x19, x0
	b	LBB7_8
LBB7_7:
	mov	w19, #21
LBB7_8:                                 ; %L285
Lloh22:
	adrp	x0, _camlTry_raise_cross_function_caught@PAGE+32
Lloh23:
	add	x0, x0, _camlTry_raise_cross_function_caught@PAGEOFF+32
	mov	x1, x19
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x29
	.cfi_restore_state
	mov	w0, #1
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_fresh_oo_id
	mov	sp, x29
	.cfi_restore_state
	mov	x1, x0
Lloh24:
	adrp	x0, _camlTry_raise_cross_function_caught__Miss245@PAGE+8
Lloh25:
	add	x0, x0, _camlTry_raise_cross_function_caught__Miss245@PAGEOFF+8
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x29
	.cfi_restore_state
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	str	x20, [sp, #16]                  ; 8-byte Folded Spill
	b.lo	LBB7_10
LBB7_9:                                 ; %L309
	mov	x0, x19
	bl	_camlTry_raise_cross_function_caught__black_box_int_0_7_code
Ltmp15:
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	ldr	x0, [sp, #16]                   ; 8-byte Folded Reload
	bl	_camlTry_raise_cross_function_caught__black_box_int_0_7_code
Ltmp16:
	ldr	x1, [sp, #8]                    ; 8-byte Folded Reload
	bl	_camlTry_raise_cross_function_caught__run_6_13_code
Ltmp17:
	str	x0, [sp, #16]                   ; 8-byte Folded Spill
Lloh26:
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
Lloh27:
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
Lloh28:
	adrp	x2, _camlTry_raise_cross_function_caught__const_block66@PAGE
Lloh29:
	add	x2, x2, _camlTry_raise_cross_function_caught__const_block66@PAGEOFF
	mov	w1, #1
	bl	_camlCamlinternalFormat__make_printf_120_401_code
Ltmp18:
	mov	x1, x0
	ldr	x8, [sp, #16]                   ; 8-byte Folded Reload
	and	x0, x8, #0x7fffffff
	ldr	x8, [x1]
	blr	x8
Ltmp19:
	mov	w0, #1
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB7_10:                                ; %L308
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp20:
	b	LBB7_9
LBB7_11:                                ; %L279
Lloh30:
	adrp	x8, _camlTry_raise_cross_function_caught__block35@PAGE
Lloh31:
	add	x8, x8, _camlTry_raise_cross_function_caught__block35@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpLdrGot	Lloh8, Lloh9
	.loh AdrpLdrGot	Lloh10, Lloh11
	.loh AdrpLdrGot	Lloh12, Lloh13
	.loh AdrpLdrGot	Lloh16, Lloh17
	.loh AdrpAdd	Lloh14, Lloh15
	.loh AdrpLdrGot	Lloh18, Lloh19
	.loh AdrpLdrGot	Lloh20, Lloh21
	.loh AdrpAdd	Lloh24, Lloh25
	.loh AdrpAdd	Lloh22, Lloh23
	.loh AdrpAdd	Lloh28, Lloh29
	.loh AdrpLdrGot	Lloh26, Lloh27
	.loh AdrpAdd	Lloh30, Lloh31
	.cfi_endproc
                                        ; -- End function
	.section	__DATA,__data
	.globl	_camlTry_raise_cross_function_caught__gc_roots ; @"\01_camlTry_raise_cross_function_caught__gc_roots"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__gc_roots:
	.quad	_camlTry_raise_cross_function_caught
	.quad	_camlTry_raise_cross_function_caught__Miss245
	.quad	0                               ; 0x0

	.globl	_header.camlTry_raise_cross_function_caught ; @"\01_header.camlTry_raise_cross_function_caught"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught:
	.quad	11008                           ; 0x2b00

	.globl	_camlTry_raise_cross_function_caught ; @"\01_camlTry_raise_cross_function_caught"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught:
	.quad	_camlTry_raise_cross_function_caught__black_box_int_7
	.quad	_camlTry_raise_cross_function_caught__black_box_string_8
	.quad	_camlTry_raise_cross_function_caught__black_box_9
	.quad	1                               ; 0x1
	.quad	1                               ; 0x1
	.quad	_camlTry_raise_cross_function_caught__print_result_10
	.quad	_camlTry_raise_cross_function_caught__Miss245
	.quad	_camlTry_raise_cross_function_caught__probe_11
	.quad	_camlTry_raise_cross_function_caught__find_12
	.quad	_camlTry_raise_cross_function_caught__run_13

	.globl	_header.camlTry_raise_cross_function_caught__run_13 ; @"\01_header.camlTry_raise_cross_function_caught__run_13"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__run_13:
	.quad	4087                            ; 0xff7

	.globl	_camlTry_raise_cross_function_caught__run_13 ; @"\01_camlTry_raise_cross_function_caught__run_13"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__run_13:
	.quad	_caml_curry2
	.quad	180143985094819847              ; 0x280000000000007
	.quad	_camlTry_raise_cross_function_caught__run_6_13_code

	.globl	_header.camlTry_raise_cross_function_caught__find_12 ; @"\01_header.camlTry_raise_cross_function_caught__find_12"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__find_12:
	.quad	3063                            ; 0xbf7

	.globl	_camlTry_raise_cross_function_caught__find_12 ; @"\01_camlTry_raise_cross_function_caught__find_12"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__find_12:
	.quad	_camlTry_raise_cross_function_caught__find_5_12_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlTry_raise_cross_function_caught__probe_11 ; @"\01_header.camlTry_raise_cross_function_caught__probe_11"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__probe_11:
	.quad	3063                            ; 0xbf7

	.globl	_camlTry_raise_cross_function_caught__probe_11 ; @"\01_camlTry_raise_cross_function_caught__probe_11"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__probe_11:
	.quad	_camlTry_raise_cross_function_caught__probe_4_11_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlTry_raise_cross_function_caught__Miss245 ; @"\01_header.camlTry_raise_cross_function_caught__Miss245"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__Miss245:
	.quad	3064                            ; 0xbf8

	.globl	_camlTry_raise_cross_function_caught__Miss245 ; @"\01_camlTry_raise_cross_function_caught__Miss245"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__Miss245:
	.quad	_camlTry_raise_cross_function_caught__immstring74
	.quad	1                               ; 0x1

	.globl	_header.camlTry_raise_cross_function_caught__print_result_10 ; @"\01_header.camlTry_raise_cross_function_caught__print_result_10"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__print_result_10:
	.quad	3063                            ; 0xbf7

	.globl	_camlTry_raise_cross_function_caught__print_result_10 ; @"\01_camlTry_raise_cross_function_caught__print_result_10"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__print_result_10:
	.quad	_camlTry_raise_cross_function_caught__print_result_3_10_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlTry_raise_cross_function_caught__black_box_9 ; @"\01_header.camlTry_raise_cross_function_caught__black_box_9"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__black_box_9:
	.quad	3063                            ; 0xbf7

	.globl	_camlTry_raise_cross_function_caught__black_box_9 ; @"\01_camlTry_raise_cross_function_caught__black_box_9"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__black_box_9:
	.quad	_camlTry_raise_cross_function_caught__black_box_2_9_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlTry_raise_cross_function_caught__black_box_string_8 ; @"\01_header.camlTry_raise_cross_function_caught__black_box_string_8"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__black_box_string_8:
	.quad	3063                            ; 0xbf7

	.globl	_camlTry_raise_cross_function_caught__black_box_string_8 ; @"\01_camlTry_raise_cross_function_caught__black_box_string_8"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__black_box_string_8:
	.quad	_camlTry_raise_cross_function_caught__black_box_string_1_8_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlTry_raise_cross_function_caught__black_box_int_7 ; @"\01_header.camlTry_raise_cross_function_caught__black_box_int_7"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__black_box_int_7:
	.quad	3063                            ; 0xbf7

	.globl	_camlTry_raise_cross_function_caught__black_box_int_7 ; @"\01_camlTry_raise_cross_function_caught__black_box_int_7"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__black_box_int_7:
	.quad	_camlTry_raise_cross_function_caught__black_box_int_0_7_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlTry_raise_cross_function_caught__block35 ; @"\01_header.camlTry_raise_cross_function_caught__block35"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__block35:
	.quad	2816                            ; 0xb00

	.globl	_camlTry_raise_cross_function_caught__block35 ; @"\01_camlTry_raise_cross_function_caught__block35"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__block35:
	.quad	_caml_exn_Invalid_argument
	.quad	_camlTry_raise_cross_function_caught__string33

	.globl	_header.camlTry_raise_cross_function_caught__string33 ; @"\01_header.camlTry_raise_cross_function_caught__string33"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__string33:
	.quad	4092                            ; 0xffc

	.globl	_camlTry_raise_cross_function_caught__string33 ; @"\01_camlTry_raise_cross_function_caught__string33"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__string33:
	.ascii	"index out of bounds"
	.space	4
	.byte	4                               ; 0x4

	.globl	_header.camlTry_raise_cross_function_caught__immstring74 ; @"\01_header.camlTry_raise_cross_function_caught__immstring74"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__immstring74:
	.quad	6140                            ; 0x17fc

	.globl	_camlTry_raise_cross_function_caught__immstring74 ; @"\01_camlTry_raise_cross_function_caught__immstring74"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__immstring74:
	.ascii	"Try_raise_cross_function_caught.Miss"
	.space	3
	.byte	3                               ; 0x3

	.globl	_header.camlTry_raise_cross_function_caught__const_block66 ; @"\01_header.camlTry_raise_cross_function_caught__const_block66"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__const_block66:
	.quad	4868                            ; 0x1304

	.globl	_camlTry_raise_cross_function_caught__const_block66 ; @"\01_camlTry_raise_cross_function_caught__const_block66"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__const_block66:
	.quad	1                               ; 0x1
	.quad	1                               ; 0x1
	.quad	1                               ; 0x1
	.quad	_camlTry_raise_cross_function_caught__const_block64

	.globl	_header.camlTry_raise_cross_function_caught__const_block64 ; @"\01_header.camlTry_raise_cross_function_caught__const_block64"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__const_block64:
	.quad	2828                            ; 0xb0c

	.globl	_camlTry_raise_cross_function_caught__const_block64 ; @"\01_camlTry_raise_cross_function_caught__const_block64"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__const_block64:
	.quad	21                              ; 0x15
	.quad	_camlTry_raise_cross_function_caught__const_block62

	.globl	_header.camlTry_raise_cross_function_caught__const_block62 ; @"\01_header.camlTry_raise_cross_function_caught__const_block62"
	.p2align	3, 0x0
_header.camlTry_raise_cross_function_caught__const_block62:
	.quad	1802                            ; 0x70a

	.globl	_camlTry_raise_cross_function_caught__const_block62 ; @"\01_camlTry_raise_cross_function_caught__const_block62"
	.p2align	3, 0x0
_camlTry_raise_cross_function_caught__const_block62:
	.quad	1                               ; 0x1

	.quad	0
	.globl	_camlTry_raise_cross_function_caught__data_end
_camlTry_raise_cross_function_caught__data_end:
	.quad	0
	.p2align	3, 0x0
	.globl	_camlTry_raise_cross_function_caught__frametable
_camlTry_raise_cross_function_caught__frametable:
	.quad	19
Ltmp21:
	.long	Ltmp1-Ltmp21
	.short	33
	.short	0
	.p2align	2, 0x0
Ltmp23:
	.long	Ltmp22-Ltmp23
	.p2align	3, 0x0
Ltmp24:
	.long	Ltmp2-Ltmp24
	.short	32
	.short	0
	.p2align	3, 0x0
Ltmp25:
	.long	Ltmp3-Ltmp25
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp27:
	.long	Ltmp26-Ltmp27
	.p2align	3, 0x0
Ltmp28:
	.long	Ltmp4-Ltmp28
	.short	32
	.short	0
	.p2align	3, 0x0
Ltmp29:
	.long	Ltmp5-Ltmp29
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp31:
	.long	Ltmp30-Ltmp31
	.p2align	3, 0x0
Ltmp32:
	.long	Ltmp7-Ltmp32
	.short	65
	.short	0
	.p2align	2, 0x0
Ltmp34:
	.long	Ltmp33-Ltmp34
	.p2align	3, 0x0
Ltmp35:
	.long	Ltmp8-Ltmp35
	.short	64
	.short	0
	.p2align	3, 0x0
Ltmp36:
	.long	Ltmp9-Ltmp36
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp38:
	.long	Ltmp37-Ltmp38
	.p2align	3, 0x0
Ltmp39:
	.long	Ltmp10-Ltmp39
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp41:
	.long	Ltmp40-Ltmp41
	.p2align	3, 0x0
Ltmp42:
	.long	Ltmp11-Ltmp42
	.short	49
	.short	1
	.short	24
	.p2align	2, 0x0
Ltmp44:
	.long	Ltmp43-Ltmp44
	.p2align	3, 0x0
Ltmp45:
	.long	Ltmp12-Ltmp45
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp47:
	.long	Ltmp46-Ltmp47
	.p2align	3, 0x0
Ltmp48:
	.long	Ltmp13-Ltmp48
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp50:
	.long	Ltmp49-Ltmp50
	.p2align	3, 0x0
Ltmp51:
	.long	Ltmp14-Ltmp51
	.short	49
	.short	1
	.short	24
	.p2align	2, 0x0
Ltmp53:
	.long	Ltmp52-Ltmp53
	.p2align	3, 0x0
Ltmp54:
	.long	Ltmp15-Ltmp54
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp56:
	.long	Ltmp55-Ltmp56
	.p2align	3, 0x0
Ltmp57:
	.long	Ltmp16-Ltmp57
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp59:
	.long	Ltmp58-Ltmp59
	.p2align	3, 0x0
Ltmp60:
	.long	Ltmp17-Ltmp60
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp62:
	.long	Ltmp61-Ltmp62
	.p2align	3, 0x0
Ltmp63:
	.long	Ltmp18-Ltmp63
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp65:
	.long	Ltmp64-Ltmp65
	.p2align	3, 0x0
Ltmp66:
	.long	Ltmp19-Ltmp66
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp68:
	.long	Ltmp67-Ltmp68
	.p2align	3, 0x0
Ltmp69:
	.long	Ltmp20-Ltmp69
	.short	48
	.short	0
	.p2align	3, 0x0
	.p2align	2, 0x0
Ltmp22:
Ltmp78:
	.long	(Ltmp70-Ltmp78)+1
	.long	14158328
Ltmp79:
	.long	(Ltmp72-Ltmp79)+1
	.long	17847640
Ltmp80:
	.long	(Ltmp74-Ltmp80)+1
	.long	19940632
Ltmp81:
	.long	(Ltmp76-Ltmp81)+0
	.long	5789176
	.p2align	2, 0x0
Ltmp70:
	.long	Ltmp71-Ltmp70
	.ascii	"Stdlib__Printf.kfprintf"
	.byte	0
Ltmp71:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp72:
	.long	Ltmp73-Ltmp72
	.ascii	"Stdlib__Printf.fprintf"
	.byte	0
Ltmp73:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp74:
	.long	Ltmp75-Ltmp74
	.ascii	"Stdlib__Printf.printf"
	.byte	0
Ltmp75:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp76:
	.long	Ltmp77-Ltmp76
	.ascii	"Try_raise_cross_function_caught.print_result"
	.byte	0
Ltmp77:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp26:
Ltmp84:
	.long	(Ltmp82-Ltmp84)+0
	.long	10492008
	.p2align	2, 0x0
Ltmp82:
	.long	Ltmp83-Ltmp82
	.ascii	"Try_raise_cross_function_caught.find"
	.byte	0
Ltmp83:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp30:
Ltmp87:
	.long	(Ltmp85-Ltmp87)+0
	.long	10492008
	.p2align	2, 0x0
Ltmp85:
	.long	Ltmp86-Ltmp85
	.ascii	"Try_raise_cross_function_caught.find"
	.byte	0
Ltmp86:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp33:
Ltmp90:
	.long	(Ltmp88-Ltmp90)+0
	.long	13652176
	.p2align	2, 0x0
Ltmp88:
	.long	Ltmp89-Ltmp88
	.ascii	"Try_raise_cross_function_caught.run"
	.byte	0
Ltmp89:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp37:
Ltmp93:
	.long	(Ltmp91-Ltmp93)+0
	.long	3164368
	.p2align	2, 0x0
Ltmp91:
	.long	Ltmp92-Ltmp91
	.ascii	"Try_raise_cross_function_caught.n"
	.byte	0
Ltmp92:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp40:
Ltmp96:
	.long	(Ltmp94-Ltmp96)+0
	.long	3197392
	.p2align	2, 0x0
Ltmp94:
	.long	Ltmp95-Ltmp94
	.ascii	"Try_raise_cross_function_caught.n"
	.byte	0
Ltmp95:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp43:
Ltmp99:
	.long	(Ltmp97-Ltmp99)+0
	.long	3183088
	.p2align	2, 0x0
Ltmp97:
	.long	Ltmp98-Ltmp97
	.ascii	"Try_raise_cross_function_caught.n"
	.byte	0
Ltmp98:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp46:
Ltmp102:
	.long	(Ltmp100-Ltmp102)+0
	.long	4737232
	.p2align	2, 0x0
Ltmp100:
	.long	Ltmp101-Ltmp100
	.ascii	"Try_raise_cross_function_caught.reps"
	.byte	0
Ltmp101:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp49:
Ltmp105:
	.long	(Ltmp103-Ltmp105)+0
	.long	4770256
	.p2align	2, 0x0
Ltmp103:
	.long	Ltmp104-Ltmp103
	.ascii	"Try_raise_cross_function_caught.reps"
	.byte	0
Ltmp104:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp52:
Ltmp108:
	.long	(Ltmp106-Ltmp108)+0
	.long	4755952
	.p2align	2, 0x0
Ltmp106:
	.long	Ltmp107-Ltmp106
	.ascii	"Try_raise_cross_function_caught.reps"
	.byte	0
Ltmp107:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp55:
Ltmp111:
	.long	(Ltmp109-Ltmp111)+0
	.long	16299528
	.p2align	2, 0x0
Ltmp109:
	.long	Ltmp110-Ltmp109
	.ascii	"Try_raise_cross_function_caught"
	.byte	0
Ltmp110:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp58:
Ltmp114:
	.long	(Ltmp112-Ltmp114)+0
	.long	16280928
	.p2align	2, 0x0
Ltmp112:
	.long	Ltmp113-Ltmp112
	.ascii	"Try_raise_cross_function_caught"
	.byte	0
Ltmp113:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp61:
Ltmp117:
	.long	(Ltmp115-Ltmp117)+0
	.long	16275984
	.p2align	2, 0x0
Ltmp115:
	.long	Ltmp116-Ltmp115
	.ascii	"Try_raise_cross_function_caught"
	.byte	0
Ltmp116:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp64:
Ltmp128:
	.long	(Ltmp118-Ltmp128)+1
	.long	14158328
Ltmp129:
	.long	(Ltmp120-Ltmp129)+1
	.long	17847640
Ltmp130:
	.long	(Ltmp122-Ltmp130)+1
	.long	19940632
Ltmp131:
	.long	(Ltmp124-Ltmp131)+1
	.long	5789176
Ltmp132:
	.long	(Ltmp126-Ltmp132)+0
	.long	16262672
	.p2align	2, 0x0
Ltmp118:
	.long	Ltmp119-Ltmp118
	.ascii	"Stdlib__Printf.kfprintf"
	.byte	0
Ltmp119:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp120:
	.long	Ltmp121-Ltmp120
	.ascii	"Stdlib__Printf.fprintf"
	.byte	0
Ltmp121:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp122:
	.long	Ltmp123-Ltmp122
	.ascii	"Stdlib__Printf.printf"
	.byte	0
Ltmp123:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp124:
	.long	Ltmp125-Ltmp124
	.ascii	"Try_raise_cross_function_caught.print_result"
	.byte	0
Ltmp125:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp126:
	.long	Ltmp127-Ltmp126
	.ascii	"Try_raise_cross_function_caught"
	.byte	0
Ltmp127:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp67:
Ltmp137:
	.long	(Ltmp133-Ltmp137)+1
	.long	5789176
Ltmp138:
	.long	(Ltmp135-Ltmp138)+0
	.long	16262672
	.p2align	2, 0x0
Ltmp133:
	.long	Ltmp134-Ltmp133
	.ascii	"Try_raise_cross_function_caught.print_result"
	.byte	0
Ltmp134:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp135:
	.long	Ltmp136-Ltmp135
	.ascii	"Try_raise_cross_function_caught"
	.byte	0
Ltmp136:
	.ascii	"try_raise_cross_function_caught.ml"
	.byte	0

	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlTry_raise_cross_function_caught__code_end
_camlTry_raise_cross_function_caught__code_end:
.subsections_via_symbols
