_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code:
L_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code:
	.loc	1	26	47
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	ldr	x16, [x28, #40]
	add	x16, x16, #408
	subs	xzr, sp, x16
	b.cc	L183
L184:
	orr	x2, xzr, x0
	str	x2, [sp, #0]
	orr	x0, xzr, x1
	ldr	x1, [x0, #0]
	ldr	x3, [x0, #8]
L148:
	str	x3, [sp, #8]
	ldr	x0, [x28, #64]
	str	x0, [sp, #16]
	adr	x16, L151
	stp	x26, x16, [sp, #-16]!
	.cfi_adjust_cfa_offset 16
	add	x26, sp, #0
	orr	x0, xzr, x2
	.loc	1	27	6
	bl	_camlEnv_find_same_layered_int_key__ident_find_same_4_12_code
L185:
	ldr	x26, [sp], #16
	.cfi_adjust_cfa_offset -16
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L151:
	ldr	x1, [sp, #16]
	str	x1, [x28, #64]
	adrp	x1, _camlEnv_find_same_layered_int_key__Not_found_same292@GOTPAGE
	ldr	x1, [x1, _camlEnv_find_same_layered_int_key__Not_found_same292@GOTPAGEOFF]
	ldr	x2, [sp, #0]
	ldr	x3, [sp, #8]
	subs	xzr, x0, x1
	b.ne	L174
	tbz	x3, #0, L168
	orr	x0, xzr, x1
	.loc	1	32	17
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L168:
	.loc	1	30	6
	ldr	x0, [x3, #0]
	ldr	x1, [x0, #0]
	ldr	x3, [x0, #8]
	b	L148
L174:
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L183:
	movz	x16, #48
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L184
	.cfi_endproc
	.text
	.align	3
	.globl	_camlEnv_find_same_layered_int_key__open_layers_6_14_code
