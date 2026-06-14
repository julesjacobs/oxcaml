_camlLayered_try_raise_hit_only__find_5_13_code:
L_camlLayered_try_raise_hit_only__find_5_13_code:
	.loc	1	22	28
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	ldr	x16, [x28, #40]
	add	x16, x16, #408
	subs	xzr, sp, x16
	b.cc	L173
L174:
	ldr	x1, [x0, #8]
L139:
	str	x1, [sp, #0]
	ldr	x1, [x28, #64]
	str	x1, [sp, #8]
	adr	x16, L142
	stp	x26, x16, [sp, #-16]!
	.cfi_adjust_cfa_offset 16
	add	x26, sp, #0
	.loc	1	23	6
	bl	_camlLayered_try_raise_hit_only__probe_4_12_code
L175:
	ldr	x26, [sp], #16
	.cfi_adjust_cfa_offset -16
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L142:
	ldr	x1, [sp, #8]
	str	x1, [x28, #64]
	adrp	x2, _camlLayered_try_raise_hit_only__Not_found_same275@GOTPAGE
	ldr	x2, [x2, _camlLayered_try_raise_hit_only__Not_found_same275@GOTPAGEOFF]
	ldr	x1, [sp, #0]
	subs	xzr, x0, x2
	b.ne	L164
	tbz	x1, #0, L159
	orr	x0, xzr, x2
	.loc	1	28	17
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L159:
	.loc	1	26	6
	ldr	x0, [x1, #0]
	ldr	x1, [x0, #8]
	b	L139
L164:
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L173:
	movz	x16, #48
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L174
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLayered_try_raise_hit_only__open_layers_6_14_code
