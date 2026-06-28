_camlArray_binary_search_string__find_4_13_code:
L_camlArray_binary_search_string__find_4_13_code:
	.loc	1	14	24
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x19, xzr, x0
	orr	x20, xzr, x1
	.loc	1	23	10
	ldr	x0, [x20, #-8]
	.loc	1	23	10
	ubfm	x0, x0, #56, #55
	.loc	1	23	10
	ubfm	x0, x0, #17, #63
	.loc	1	23	10
	orr	x0, x0, #1
	.loc	1	23	9
	sub	x21, x0, #2
	orr	x22, xzr, #1
	subs	xzr, x22, x21
	b.le	L138
L136:
	movn	x0, #0
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L138:
	.loc	1	18	16
	add	x0, x22, x21
	.loc	1	18	16
	sub	x0, x0, #1
	.loc	1	18	16
	ubfm	x0, x0, #1, #63
	.loc	1	18	16
	orr	x23, x0, #1
	.loc	1	19	17
	add	x0, x20, x23, lsl #2
	.loc	1	19	17
	ldr	x24, [x0, #-4]
	.loc	1	19	10
	ldr	x1, [x24, #0]
	orr	x0, xzr, x19
	.loc	1	20	14
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_string_compare
	add	sp, fp, #0
	.cfi_restore_state
	subs	xzr, x0, #1
	b.ne	L151
	.loc	1	19	10
	ldr	x0, [x24, #8]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L151:
	subs	xzr, x0, #1
	b.ge	L156
	.loc	1	21	49
	sub	x21, x23, #2
	subs	xzr, x22, x21
	b.le	L138
	b	L136
L156:
	.loc	1	21	69
	add	x22, x23, #2
	subs	xzr, x22, x21
	b.le	L138
	b	L136
	.cfi_endproc
	.text
	.align	3
	.globl	_camlArray_binary_search_string__run_6_15_code
