_camlEnv_find_same_layered_int_key__open_layers_6_14_code:
L_camlEnv_find_same_layered_int_key__open_layers_6_14_code:
	.loc	1	34	20
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	subs	xzr, x0, #1
	b.ne	L194
	orr	x0, xzr, x1
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L194:
	ldr	x16, [x28, #40]
	add	x16, x16, #376
	subs	xzr, sp, x16
	b.cc	L228
L229:
	.loc	1	39	11
	and	x2, x0, #3
	subs	xzr, x2, #1
	b.ne	L202
	.loc	1	40	31
	sub	x0, x0, #2
	.loc	1	40	18
	bl	_camlEnv_find_same_layered_int_key__open_layers_6_14_code
L230:
	.loc	1	40	13
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L233
L232:
	add	x1, x27, #8
	orr	x2, xzr, #1024
	.loc	1	40	13
	str	x2, [x1, #-8]
	.loc	1	40	13
	str	x0, [x1, #0]
	b	L208
L202:
	.loc	1	41	31
	sub	x0, x0, #2
	.loc	1	41	18
	bl	_camlEnv_find_same_layered_int_key__open_layers_6_14_code
L234:
	.loc	1	41	13
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L237
L236:
	add	x1, x27, #8
	movz	x2, #1025, lsl #0
	.loc	1	41	13
	str	x2, [x1, #-8]
	.loc	1	41	13
	str	x0, [x1, #0]
L208:
	.loc	1	37	4
	ldr	x16, [x28, #0]
	sub	x27, x27, #24
	subs	xzr, x27, x16
	b.cc	L240
L239:
	add	x0, x27, #8
	orr	x2, xzr, #2048
	.loc	1	37	4
	str	x2, [x0, #-8]
	orr	x2, xzr, #1
	.loc	1	37	4
	str	x2, [x0, #0]
	.loc	1	37	4
	str	x1, [x0, #8]
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L240:
	bl	_caml_call_gc
L238:
	b	L239
L237:
	bl	_caml_call_gc
L235:
	b	L236
L233:
	bl	_caml_call_gc
L231:
	b	L232
L228:
	movz	x16, #16
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L229
	.cfi_endproc
	.text
	.align	3
	.globl	_camlEnv_find_same_layered_int_key__run_7_15_code
