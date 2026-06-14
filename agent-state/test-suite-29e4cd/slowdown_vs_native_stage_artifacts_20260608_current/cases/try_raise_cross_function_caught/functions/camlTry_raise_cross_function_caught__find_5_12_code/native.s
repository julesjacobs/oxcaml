_camlTry_raise_cross_function_caught__find_5_12_code:
L_camlTry_raise_cross_function_caught__find_5_12_code:
	.loc	1	19	24
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	ldr	x16, [x28, #40]
	add	x16, x16, #392
	subs	xzr, sp, x16
	b.cc	L158
L159:
	ldr	x1, [x28, #64]
	str	x1, [sp, #0]
	adr	x16, L135
	stp	x26, x16, [sp, #-16]!
	.cfi_adjust_cfa_offset 16
	add	x26, sp, #0
	.loc	1	20	6
	bl	_camlTry_raise_cross_function_caught__probe_4_11_code
L160:
	ldr	x26, [sp], #16
	.cfi_adjust_cfa_offset -16
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L135:
	ldr	x1, [sp, #0]
	str	x1, [x28, #64]
	adrp	x1, _camlTry_raise_cross_function_caught__Miss245@GOTPAGE
	ldr	x1, [x1, _camlTry_raise_cross_function_caught__Miss245@GOTPAGEOFF]
	subs	xzr, x0, x1
	b.ne	L150
	orr	x0, xzr, #3
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
L150:
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L158:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L159
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTry_raise_cross_function_caught__run_6_13_code
