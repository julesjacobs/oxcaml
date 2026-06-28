_camlString_map_equal_content__find_opt_16_34_code:
L_camlString_map_equal_content__find_opt_16_34_code:
	.loc	1	224	21
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x19, xzr, x0
	orr	x20, xzr, x1
	tbz	x20, #0, L206
L204:
	orr	x0, xzr, #1
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L206:
	.loc	1	227	13
	ldr	x1, [x20, #8]
	orr	x0, xzr, x19
	.loc	2	14	21
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_string_compare
	add	sp, fp, #0
	.cfi_restore_state
	subs	xzr, x0, #1
	b.ne	L214
	.loc	1	229	24
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L231
L230:
	add	x0, x27, #8
	orr	x1, xzr, #1024
	.loc	1	229	24
	str	x1, [x0, #-8]
	.loc	1	227	13
	ldr	x1, [x20, #16]
	.loc	1	229	24
	str	x1, [x0, #0]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L214:
	subs	xzr, x0, #1
	b.ge	L219
	.loc	1	227	13
	ldr	x20, [x20, #0]
	tbz	x20, #0, L206
	b	L204
L219:
	.loc	1	227	13
	ldr	x20, [x20, #24]
	tbz	x20, #0, L206
	b	L204
L231:
	bl	_caml_call_gc
L229:
	b	L230
	.cfi_endproc
	.text
	.align	3
	.globl	_camlString_map_equal_content__mem_17_35_code
