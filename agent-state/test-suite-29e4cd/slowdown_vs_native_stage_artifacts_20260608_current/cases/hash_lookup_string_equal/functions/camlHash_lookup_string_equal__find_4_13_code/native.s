_camlHash_lookup_string_equal__find_4_13_code:
L_camlHash_lookup_string_equal__find_4_13_code:
	.loc	1	14	24
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x19, xzr, x0
	.loc	1	15	15
	ldr	x20, [x1, #0]
	tbz	x20, #0, L134
L132:
	movn	x0, #0
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L134:
	.loc	1	18	6
	ldr	x21, [x20, #0]
	.loc	1	18	6
	ldr	x1, [x21, #0]
	orr	x0, xzr, x19
	.loc	1	18	27
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_string_equal
	add	sp, fp, #0
	.cfi_restore_state
	subs	xzr, x0, #1
	b.ne	L142
	.loc	1	18	6
	ldr	x20, [x20, #8]
	tbz	x20, #0, L134
	b	L132
L142:
	.loc	1	18	6
	ldr	x0, [x21, #8]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
	.cfi_endproc
	.text
	.align	3
	.globl	_camlHash_lookup_string_equal__run_6_15_code
