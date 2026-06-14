_camlString_tree_first_byte_diff__find_6_14_code:
L_camlString_tree_first_byte_diff__find_6_14_code:
	.loc	1	25	28
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x19, xzr, x0
	orr	x20, xzr, x1
	tbz	x20, #0, L239
L237:
	movn	x0, #0
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L239:
	.loc	1	27	4
	ldr	x1, [x20, #8]
	orr	x0, xzr, x19
	.loc	1	28	12
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_string_compare
	add	sp, fp, #0
	.cfi_restore_state
	subs	xzr, x0, #1
	b.ne	L246
	.loc	1	27	4
	ldr	x0, [x20, #16]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L246:
	subs	xzr, x0, #1
	b.ge	L251
	.loc	1	27	4
	ldr	x20, [x20, #0]
	tbz	x20, #0, L239
	b	L237
L251:
	.loc	1	27	4
	ldr	x20, [x20, #24]
	tbz	x20, #0, L239
	b	L237
	.cfi_endproc
	.text
	.align	3
	.globl	_camlString_tree_first_byte_diff__run_7_15_code
