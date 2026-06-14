_camlString_tree_first_byte_diff__key_4_12_code:
L_camlString_tree_first_byte_diff__key_4_12_code:
	.loc	1	16	8
	.cfi_startproc
	sub	sp, sp, #48
	.cfi_adjust_cfa_offset 48
	.cfi_offset 30, -8
	str	lr, [sp, #40]
	orr	x1, xzr, x0
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	str	x1, [sp, #16]
	.file	3	"stdlib.ml"
	.loc	3	280	2
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L185:
	.loc	3	225	37
	ldr	x1, [x0, #-8]
	.loc	3	225	37
	ubfm	x1, x1, #56, #55
	.loc	3	225	37
	ubfm	x1, x1, #18, #63
	.loc	3	225	37
	ubfm	x1, x1, #61, #60
	.loc	3	225	37
	sub	x1, x1, #1
	.loc	3	225	37
	add	x2, x0, x1
	.loc	3	225	37
	str	x0, [sp, #0]
	.loc	3	225	37
	ldrb	w0, [x2, #0]
	.loc	3	225	37
	sub	x0, x1, x0
	orr	x1, xzr, #1
	.loc	3	225	37
	add	x1, x1, x0, lsl #1
	.loc	3	226	23
	add	x0, x1, #28
	.loc	3	226	23
	str	x1, [sp, #8]
	.loc	3	226	10
	adrp	x8, _caml_create_bytes@GOTPAGE
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	bl	_caml_c_call
L186:
	orr	x19, xzr, x0
	movz	x4, #29, lsl #0
	orr	x3, xzr, #1
	orr	x1, xzr, #1
	adrp	x0, _camlString_tree_first_byte_diff__immstring83@GOTPAGE
	ldr	x0, [x0, _camlString_tree_first_byte_diff__immstring83@GOTPAGEOFF]
	orr	x2, xzr, x19
	.loc	3	227	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	movz	x3, #29, lsl #0
	orr	x1, xzr, #1
	ldr	x0, [sp, #0]
	orr	x2, xzr, x19
	ldr	x4, [sp, #8]
	.loc	3	228	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	movz	x0, #51, lsl #0
	.loc	1	17	32
	ldr	x1, [sp, #16]
	.loc	1	17	32
	and	x0, x1, x0
	.loc	1	17	26
	add	x0, x0, #130
	subs	xzr, x0, #1
	b.ge	L144
	adrp	x0, _camlStdlib__Char__Pmakeblock168@GOTPAGE
	ldr	x0, [x0, _camlStdlib__Char__Pmakeblock168@GOTPAGEOFF]
	.loc	3	40	20
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L144:
	subs	xzr, x0, #511
	b.le	L148
	adrp	x0, _camlStdlib__Char__Pmakeblock168@GOTPAGE
	ldr	x0, [x0, _camlStdlib__Char__Pmakeblock168@GOTPAGEOFF]
	.loc	3	40	20
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L148:
	str	x0, [sp, #0]
	str	x19, [sp, #8]
	orr	x0, xzr, #3
	.file	4	"bytes.ml"
	.loc	4	60	10
	adrp	x8, _caml_create_bytes@GOTPAGE
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	bl	_caml_c_call
L187:
	orr	x19, xzr, x0
	orr	x2, xzr, #3
	orr	x1, xzr, #1
	orr	x0, xzr, x19
	ldr	x3, [sp, #0]
	.loc	4	61	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_fill_bytes
	add	sp, fp, #0
	.cfi_restore_state
	.loc	3	225	11
	ldr	x0, [x19, #-8]
	.loc	3	225	11
	ubfm	x0, x0, #56, #55
	.loc	3	225	11
	ubfm	x0, x0, #18, #63
	.loc	3	225	11
	ubfm	x0, x0, #61, #60
	.loc	3	225	11
	sub	x0, x0, #1
	.loc	3	225	11
	add	x1, x19, x0
	.loc	3	225	11
	str	x19, [sp, #0]
	.loc	3	225	11
	ldrb	w1, [x1, #0]
	.loc	3	225	11
	sub	x0, x0, x1
	orr	x1, xzr, #1
	.loc	3	225	11
	add	x0, x1, x0, lsl #1
	.loc	3	225	37
	ldr	x2, [sp, #8]
	.loc	3	225	37
	ldr	x1, [x2, #-8]
	.loc	3	225	37
	ubfm	x1, x1, #56, #55
	.loc	3	225	37
	ubfm	x1, x1, #18, #63
	.loc	3	225	37
	ubfm	x1, x1, #61, #60
	.loc	3	225	37
	sub	x1, x1, #1
	.loc	3	225	37
	add	x2, x2, x1
	.loc	3	225	37
	ldrb	w2, [x2, #0]
	.loc	3	225	37
	sub	x1, x1, x2
	orr	x2, xzr, #1
	.loc	3	225	37
	add	x1, x2, x1, lsl #1
	.loc	3	226	23
	add	x2, x0, x1
	.loc	3	226	23
	str	x1, [sp, #24]
	.loc	3	226	23
	str	x0, [sp, #16]
	.loc	3	226	23
	sub	x0, x2, #1
	.loc	3	226	10
	adrp	x8, _caml_create_bytes@GOTPAGE
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	bl	_caml_c_call
L188:
	orr	x19, xzr, x0
	orr	x3, xzr, #1
	orr	x1, xzr, #1
	ldr	x0, [sp, #0]
	orr	x2, xzr, x19
	ldr	x20, [sp, #16]
	orr	x4, xzr, x20
	.loc	3	227	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	orr	x1, xzr, #1
	ldr	x0, [sp, #8]
	orr	x2, xzr, x19
	orr	x3, xzr, x20
	ldr	x4, [sp, #24]
	.loc	3	228	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	orr	x0, xzr, x19
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	ret
	.cfi_adjust_cfa_offset 48
	.cfi_endproc
	.text
	.align	3
	.globl	_camlString_tree_first_byte_diff__build_5_13_code
