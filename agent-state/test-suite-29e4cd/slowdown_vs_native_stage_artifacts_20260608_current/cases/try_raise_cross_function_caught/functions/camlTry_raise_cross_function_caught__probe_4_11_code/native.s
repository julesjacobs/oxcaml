_camlTry_raise_cross_function_caught__probe_4_11_code:
L_camlTry_raise_cross_function_caught__probe_4_11_code:
	.loc	1	16	25
	.cfi_startproc
	subs	xzr, x0, #1
	b.ne	L128
	adrp	x0, _camlTry_raise_cross_function_caught__Miss245@GOTPAGE
	ldr	x0, [x0, _camlTry_raise_cross_function_caught__Miss245@GOTPAGEOFF]
	.loc	1	17	16
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L128:
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTry_raise_cross_function_caught__find_5_12_code
