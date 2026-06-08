_camlLayered_try_raise_hit_only__open_layers_6_14_code:
L_camlLayered_try_raise_hit_only__open_layers_6_14_code:
	.loc	1	30	20
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	subs	xzr, x0, #1
	b.ne	L184
	orr	x0, xzr, x1
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L184:
	ldr	x16, [x28, #40]
	add	x16, x16, #376
	subs	xzr, sp, x16
	b.cc	L218
L219:
	.loc	1	35	11
	and	x2, x0, #3
	subs	xzr, x2, #1
	b.ne	L192
	.loc	1	36	31
	sub	x0, x0, #2
	.loc	1	36	18
	bl	_camlLayered_try_raise_hit_only__open_layers_6_14_code
L220:
	.loc	1	36	13
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L223
L222:
	add	x1, x27, #8
	orr	x2, xzr, #1024
	.loc	1	36	13
	str	x2, [x1, #-8]
	.loc	1	36	13
	str	x0, [x1, #0]
	b	L198
L192:
	.loc	1	37	31
	sub	x0, x0, #2
	.loc	1	37	18
	bl	_camlLayered_try_raise_hit_only__open_layers_6_14_code
L224:
	.loc	1	37	13
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L227
L226:
	add	x1, x27, #8
	movz	x2, #1025, lsl #0
	.loc	1	37	13
	str	x2, [x1, #-8]
	.loc	1	37	13
	str	x0, [x1, #0]
L198:
	.loc	1	33	4
	ldr	x16, [x28, #0]
	sub	x27, x27, #24
	subs	xzr, x27, x16
	b.cc	L230
L229:
	add	x0, x27, #8
	orr	x2, xzr, #2048
	.loc	1	33	4
	str	x2, [x0, #-8]
	orr	x2, xzr, #1
	.loc	1	33	4
	str	x2, [x0, #0]
	.loc	1	33	4
	str	x1, [x0, #8]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L230:
	bl	_caml_call_gc
L228:
	b	L229
L227:
	bl	_caml_call_gc
L225:
	b	L226
L223:
	bl	_caml_call_gc
L221:
	b	L222
L218:
	movz	x16, #16
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L219
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLayered_try_raise_hit_only__run_7_15_code
