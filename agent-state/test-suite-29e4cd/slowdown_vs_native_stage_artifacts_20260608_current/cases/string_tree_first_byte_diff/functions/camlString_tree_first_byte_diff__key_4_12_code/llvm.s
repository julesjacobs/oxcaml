_camlString_tree_first_byte_diff__key_4_12_code: ; @"\01_camlString_tree_first_byte_diff__key_4_12_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	mov	x20, x0
	str	x0, [sp, #24]
Lloh4:
	adrp	x8, _caml_format_int@GOTPAGE
Lloh5:
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
Lloh6:
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
Lloh7:
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	mov	x1, x20
	bl	_caml_c_call
Ltmp2:
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	lsl	x9, x8, #1
	mov	w24, #1
	mov	w21, #1
	bfi	x21, x8, #1, #63
	add	x9, x9, #29
	str	x0, [sp, #24]
	str	x9, [sp, #16]
Lloh8:
	adrp	x19, _caml_create_bytes@GOTPAGE
Lloh9:
	ldr	x19, [x19, _caml_create_bytes@GOTPAGEOFF]
	mov	x8, x19
	mov	x0, x9
	bl	_caml_c_call
Ltmp3:
	mov	x22, x0
	ldr	x23, [sp, #24]
Lloh10:
	adrp	x0, _camlString_tree_first_byte_diff__immstring83@PAGE
Lloh11:
	add	x0, x0, _camlString_tree_first_byte_diff__immstring83@PAGEOFF
	mov	w1, #1
	mov	x2, x22
	mov	w3, #1
	mov	w4, #29
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	mov	x0, x23
	mov	w1, #1
	mov	x2, x22
	mov	w3, #29
	mov	x4, x21
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	mov	w8, #51
	and	x8, x20, x8
	add	x20, x8, #130
	str	x22, [sp, #24]
	mov	x8, x19
	mov	w0, #3
	bl	_caml_c_call
Ltmp4:
	mov	x21, x0
	ldr	x22, [sp, #24]
	mov	w1, #1
	mov	w2, #3
	mov	x3, x20
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_fill_bytes
	mov	sp, x29
	.cfi_restore_state
	ldur	x8, [x21, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x21, x8]
	sub	x8, x8, x9
	mov	w20, #1
	bfi	x20, x8, #1, #63
	ldur	x9, [x22, #-8]
	lsr	x9, x9, #7
	and	x9, x9, #0x1fffffffffff8
	sub	x9, x9, #1
	ldrb	w10, [x22, x9]
	lsl	x8, x8, #1
	sub	x9, x9, x10
	orr	x22, x24, x9, lsl #1
	add	x0, x8, x22
	str	x21, [sp, #16]
	str	x0, [sp, #8]
	mov	x8, x19
	bl	_caml_c_call
Ltmp5:
	mov	x19, x0
	ldr	x21, [sp, #24]
	ldr	x0, [sp, #16]
	mov	w1, #1
	mov	x2, x19
	mov	w3, #1
	mov	x4, x20
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	mov	x0, x21
	mov	w1, #1
	mov	x2, x19
	mov	x3, x20
	mov	x4, x22
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	mov	x0, x19
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
	.loh AdrpAdd	Lloh10, Lloh11
	.loh AdrpLdrGot	Lloh8, Lloh9
	.loh AdrpLdrGot	Lloh6, Lloh7
	.loh AdrpLdrGot	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
	.globl	_camlString_tree_first_byte_diff__build_5_13_code ; -- Begin function _camlString_tree_first_byte_diff__build_5_13_code
	.p2align	2
