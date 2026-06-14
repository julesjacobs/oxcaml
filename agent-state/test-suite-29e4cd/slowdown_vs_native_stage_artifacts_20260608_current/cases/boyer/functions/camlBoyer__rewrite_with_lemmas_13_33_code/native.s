_camlBoyer__rewrite_with_lemmas_13_33_code:
L_camlBoyer__rewrite_with_lemmas_13_33_code:
	.loc	1	103	24
	.cfi_startproc
	ldr	x16, [x28, #40]
	add	x16, x16, #424
	subs	xzr, sp, x16
	b.cc	L707
L708:
	sub	sp, sp, #48
	.cfi_adjust_cfa_offset 48
	.cfi_offset 30, -8
	str	lr, [sp, #40]
L661:
	tbz	x1, #0, L665
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	ret
	.cfi_adjust_cfa_offset 48
L665:
	.loc	1	106	4
	ldr	x2, [x1, #0]
	.loc	1	106	4
	str	x1, [sp, #8]
	ldr	x1, [x28, #64]
	str	x1, [sp, #16]
	adr	x16, L669
	stp	x26, x16, [sp, #-16]!
	.cfi_adjust_cfa_offset 16
	add	x26, sp, #0
	.loc	1	106	4
	ldr	x1, [x2, #8]
	str	x1, [sp, #40]
	.loc	1	106	4
	ldr	x1, [x2, #0]
	str	x0, [sp, #16]
	.loc	1	107	31
	bl	_camlBoyer__unify_9_29_code
L709:
	orr	x2, xzr, x0
	.loc	1	71	19
	ldr	x16, [x28, #0]
	sub	x27, x27, #40
	subs	xzr, x27, x16
	b.cc	L712
L711:
	add	x1, x27, #8
	movz	x0, #4343, lsl #0
	.loc	1	71	19
	str	x0, [x1, #-8]
	adrp	x0, L_camlBoyer__as_rec_8_34_code@PAGE
	add	x0, x0, L_camlBoyer__as_rec_8_34_code@PAGEOFF
	.loc	1	71	19
	str	x0, [x1, #0]
	movz	x0, #5, lsl #0
	movk	x0, #384, lsl #48
	.loc	1	71	19
	str	x0, [x1, #8]
	.loc	1	71	19
	ldr	x0, [sp, #40]
	.loc	1	71	19
	str	x0, [x1, #16]
	.loc	1	71	19
	str	x2, [x1, #24]
	.loc	1	75	2
	bl	_camlBoyer__as_rec_8_34_code
L713:
	.loc	1	107	10
	bl	_camlBoyer__rewrite_12_32_code
L714:
	ldr	x26, [sp], #16
	.cfi_adjust_cfa_offset -16
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	ret
	.cfi_adjust_cfa_offset 48
L669:
	ldr	x1, [sp, #16]
	str	x1, [x28, #64]
	adrp	x1, _camlBoyer__Unify3423@GOTPAGE
	ldr	x1, [x1, _camlBoyer__Unify3423@GOTPAGEOFF]
	ldr	x2, [sp, #0]
	ldr	x3, [sp, #8]
	subs	xzr, x0, x1
	b.ne	L691
	.loc	1	106	4
	ldr	x1, [x3, #8]
	orr	x0, xzr, x2
	.loc	1	108	20
	b	L661
L691:
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L712:
	bl	_caml_call_gc
L710:
	b	L711
L707:
	movz	x16, #64
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L708
	.cfi_endproc
	.text
	.align	3
	.globl	_camlBoyer__cterm_to_term_14_35_code
