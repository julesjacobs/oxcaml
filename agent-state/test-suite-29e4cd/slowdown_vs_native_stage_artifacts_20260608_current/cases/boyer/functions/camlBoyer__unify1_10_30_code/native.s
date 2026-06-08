_camlBoyer__unify1_10_30_code:
L_camlBoyer__unify1_10_30_code:
	.loc	1	81	11
	.cfi_startproc
	sub	sp, sp, #48
	.cfi_adjust_cfa_offset 48
	.cfi_offset 30, -8
	str	lr, [sp, #40]
	orr	x3, xzr, x0
	ldrb	w0, [x1, #-8]
	cbz	x0, L534
	ldrb	w0, [x3, #-8]
	cbz	x0, L531
	.loc	1	86	4
	ldr	x0, [x1, #0]
	.loc	1	89	8
	ldr	x4, [x3, #0]
	subs	xzr, x4, x0
	b.ne	L528
	.loc	1	86	4
	ldr	x1, [x1, #8]
	.loc	1	89	8
	ldr	x0, [x3, #8]
	.loc	1	90	33
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	b	_camlBoyer__unify1_lst_11_31_code
	.cfi_adjust_cfa_offset 48
L528:
	adrp	x0, _camlBoyer__Unify3423@GOTPAGE
	ldr	x0, [x0, _camlBoyer__Unify3423@GOTPAGEOFF]
	.loc	1	90	73
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L531:
	adrp	x0, _camlBoyer__Unify3423@GOTPAGE
	ldr	x0, [x0, _camlBoyer__Unify3423@GOTPAGEOFF]
	.loc	1	88	17
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L534:
	.loc	1	83	4
	ldr	x0, [x1, #0]
	ldr	x1, [x28, #64]
	adr	x16, L538
	stp	x26, x16, [sp, #-16]!
	.cfi_adjust_cfa_offset 16
	add	x26, sp, #0
	orr	x4, xzr, x2
	tbz	x4, #0, L556
L554:
	str	x1, [sp, #40]
	str	x0, [sp, #32]
	str	x2, [sp, #24]
	str	x3, [sp, #16]
	adrp	x0, _camlBoyer__Pmakeblock3333@GOTPAGE
	ldr	x0, [x0, _camlBoyer__Pmakeblock3333@GOTPAGEOFF]
	.loc	2	39	17
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L556:
	.loc	1	66	6
	ldr	x5, [x4, #0]
	.loc	1	66	6
	ldr	x6, [x5, #0]
	subs	xzr, x0, x6
	b.ne	L563
	.loc	1	66	6
	str	x1, [sp, #40]
	.loc	1	66	6
	str	x0, [sp, #32]
	.loc	1	66	6
	str	x2, [sp, #24]
	.loc	1	66	6
	ldr	x0, [x5, #8]
	orr	x1, xzr, x3
	str	x3, [sp, #16]
	.loc	1	84	13
	adrp	x8, _caml_equal@GOTPAGE
	ldr	x8, [x8, _caml_equal@GOTPAGEOFF]
	bl	_caml_c_call
L597:
	.loc	1	84	13
	b	L571
L563:
	.loc	1	66	6
	ldr	x4, [x4, #8]
	tbz	x4, #0, L556
	b	L554
L571:
	orr	x1, xzr, x0
	ldr	x0, [sp, #24]
	subs	xzr, x1, #1
	b.ne	L574
	adrp	x0, _camlBoyer__Unify3423@GOTPAGE
	ldr	x0, [x0, _camlBoyer__Unify3423@GOTPAGEOFF]
	.loc	1	84	69
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L574:
	ldr	x26, [sp], #16
	.cfi_adjust_cfa_offset -16
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	ret
	.cfi_adjust_cfa_offset 48
L538:
	ldr	x1, [sp, #24]
	str	x1, [x28, #64]
	adrp	x1, _caml_exn_Failure@GOTPAGE
	ldr	x1, [x1, _caml_exn_Failure@GOTPAGEOFF]
	.loc	1	85	11
	ldr	x2, [x0, #0]
	ldr	x3, [sp, #0]
	ldr	x4, [sp, #8]
	ldr	x5, [sp, #16]
	subs	xzr, x2, x1
	b.ne	L587
	.loc	1	85	24
	ldr	x16, [x28, #0]
	sub	x27, x27, #48
	subs	xzr, x27, x16
	b.cc	L600
L599:
	add	x1, x27, #8
	.loc	1	85	24
	add	x1, x1, #24
	orr	x0, xzr, #2048
	.loc	1	85	24
	str	x0, [x1, #-8]
	.loc	1	85	24
	str	x5, [x1, #0]
	.loc	1	85	24
	str	x3, [x1, #8]
	.loc	1	85	24
	sub	x0, x1, #24
	orr	x2, xzr, #2048
	.loc	1	85	24
	str	x2, [x0, #-8]
	.loc	1	85	24
	str	x1, [x0, #0]
	.loc	1	85	24
	str	x4, [x0, #8]
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	ret
	.cfi_adjust_cfa_offset 48
L587:
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L600:
	bl	_caml_call_gc
L598:
	b	L599
	.cfi_endproc
	.text
	.align	3
	.globl	_camlBoyer__unify1_lst_11_31_code
