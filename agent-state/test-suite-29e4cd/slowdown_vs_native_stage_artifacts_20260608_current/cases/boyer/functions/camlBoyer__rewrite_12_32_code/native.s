_camlBoyer__rewrite_12_32_code:
L_camlBoyer__rewrite_12_32_code:
	.loc	1	98	18
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	ldrb	w1, [x0, #-8]
	cbz	x1, L644
	ldr	x16, [x28, #40]
	add	x16, x16, #392
	subs	xzr, sp, x16
	b.cc	L654
L655:
	.loc	1	100	4
	ldr	x1, [x0, #0]
	.loc	1	101	63
	ldr	x2, [x1, #8]
	.loc	1	101	63
	str	x1, [sp, #0]
	str	x2, [sp, #8]
	.loc	1	100	4
	ldr	x1, [x0, #8]
	adrp	x0, _camlBoyer__rewrite_28@GOTPAGE
	ldr	x0, [x0, _camlBoyer__rewrite_28@GOTPAGEOFF]
	.loc	1	101	39
	bl	_camlStdlib__List__map_15_113_code
L656:
	orr	x1, xzr, x0
	.loc	1	101	26
	ldr	x16, [x28, #0]
	sub	x27, x27, #24
	subs	xzr, x27, x16
	b.cc	L659
L658:
	add	x0, x27, #8
	movz	x2, #2049, lsl #0
	.loc	1	101	26
	str	x2, [x0, #-8]
	.loc	1	101	26
	ldr	x2, [sp, #0]
	.loc	1	101	26
	str	x2, [x0, #0]
	.loc	1	101	26
	str	x1, [x0, #8]
	ldr	x1, [sp, #8]
	.loc	1	101	6
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	b	_camlBoyer__rewrite_with_lemmas_13_33_code
	.cfi_adjust_cfa_offset 32
L644:
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L659:
	bl	_caml_call_gc
L657:
	b	L658
L654:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L655
	.cfi_endproc
	.text
	.align	3
	.globl	_camlBoyer__rewrite_with_lemmas_13_33_code
