_camlTry_find_miss_rare__scan_4_11_code:
L_camlTry_find_miss_rare__scan_4_11_code:
	.loc	1	16	28
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	orr	x3, xzr, x0
	str	x3, [sp, #0]
	str	x1, [sp, #8]
	orr	x0, xzr, x2
L129:
	.loc	1	17	9
	ldr	x2, [x1, #-8]
	.loc	1	17	9
	ubfm	x2, x2, #56, #55
	.loc	1	17	9
	ubfm	x2, x2, #17, #63
	.loc	1	17	9
	orr	x2, x2, #1
	subs	xzr, x0, x2
	b.ne	L140
	adrp	x0, _camlTry_find_miss_rare__Miss281@GOTPAGE
	ldr	x0, [x0, _camlTry_find_miss_rare__Miss281@GOTPAGEOFF]
	.loc	1	17	29
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L140:
	.loc	1	18	10
	ldrb	w2, [x1, #-8]
	subs	xzr, x2, #254
	b.ne	L148
	.loc	1	18	10
	ldr	x16, [x28, #0]
	sub	x27, x27, #16
	subs	xzr, x27, x16
	b.cc	L172
L171:
	add	x2, x27, #8
	movz	x4, #1277, lsl #0
	.loc	1	18	10
	str	x4, [x2, #-8]
	.loc	1	18	10
	add	x1, x1, x0, lsl #2
	.loc	1	18	10
	ldr	d0, [x1, #-4]
	.loc	1	18	10
	str	d0, [x2, #0]
	b	L153
L148:
	.loc	1	18	10
	add	x1, x1, x0, lsl #2
	.loc	1	18	10
	ldr	x2, [x1, #-4]
L153:
	str	x0, [sp, #16]
	orr	x0, xzr, x2
	orr	x1, xzr, x3
	.loc	1	18	10
	adrp	x8, _caml_equal@GOTPAGE
	ldr	x8, [x8, _caml_equal@GOTPAGEOFF]
	bl	_caml_c_call
L173:
	orr	x2, xzr, x0
	ldr	x3, [sp, #0]
	ldr	x1, [sp, #8]
	ldr	x0, [sp, #16]
	subs	xzr, x2, #1
	b.ne	L159
	.loc	1	19	18
	add	x0, x0, #2
	b	L129
L159:
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L172:
	bl	_caml_call_gc
L170:
	b	L171
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTry_find_miss_rare__run_5_12_code
