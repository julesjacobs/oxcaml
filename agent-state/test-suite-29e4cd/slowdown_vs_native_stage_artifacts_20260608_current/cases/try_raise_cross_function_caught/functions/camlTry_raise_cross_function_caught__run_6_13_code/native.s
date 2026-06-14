_camlTry_raise_cross_function_caught__run_6_13_code:
L_camlTry_raise_cross_function_caught__run_6_13_code:
	.loc	1	22	23
	.cfi_startproc
	sub	sp, sp, #64
	.cfi_adjust_cfa_offset 64
	.cfi_offset 30, -8
	str	lr, [sp, #56]
	orr	x2, xzr, x0
	subs	xzr, x1, #3
	b.lt	L206
	ldr	x16, [x28, #40]
	add	x16, x16, #424
	subs	xzr, sp, x16
	b.cc	L213
L214:
	.loc	1	24	2
	str	x2, [sp, #0]
	.loc	1	24	2
	sbfm	x1, x1, #1, #63
	str	x1, [sp, #8]
	orr	x0, xzr, #1
	orr	x3, xzr, #1
	subs	xzr, x2, #3
	b.lt	L196
	b	L175
L173:
	subs	xzr, x2, #3
	b.lt	L196
L175:
	.loc	1	25	4
	str	x3, [sp, #40]
	.loc	1	25	4
	sbfm	x1, x2, #1, #63
	str	x1, [sp, #16]
	orr	x3, xzr, #1
L181:
	str	x0, [sp, #32]
	str	x3, [sp, #24]
	orr	x0, xzr, #1
	.loc	1	26	20
	bl	_camlTry_raise_cross_function_caught__find_5_12_code
L215:
	orr	x1, xzr, x0
	.loc	1	26	13
	ldr	x0, [sp, #32]
	.loc	1	26	13
	add	x0, x0, x1
	.loc	1	26	13
	sub	x0, x0, #1
	.loc	1	25	4
	ldr	x3, [sp, #24]
	.loc	1	25	4
	add	x3, x3, #1
	ldr	x2, [sp, #0]
	ldr	x1, [sp, #8]
	ldr	x4, [sp, #16]
	subs	xzr, x3, x4
	b.le	L181
	ldr	x3, [sp, #40]
L196:
	.loc	1	24	2
	add	x3, x3, #1
	subs	xzr, x3, x1
	b.le	L173
	ldr	lr, [sp, #56]
	add	sp, sp, #64
	.cfi_adjust_cfa_offset -64
	ret
	.cfi_adjust_cfa_offset 64
L206:
	orr	x0, xzr, #1
	ldr	lr, [sp, #56]
	add	sp, sp, #64
	.cfi_adjust_cfa_offset -64
	ret
	.cfi_adjust_cfa_offset 64
L213:
	movz	x16, #64
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L214
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTry_raise_cross_function_caught__entry
_camlTry_raise_cross_function_caught__entry:
L_camlTry_raise_cross_function_caught__entry:
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	orr	x0, xzr, #1
	.loc	1	6	18
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L327:
	.loc	1	6	5
	ldr	x0, [x0, #-8]
	.loc	1	6	5
	ubfm	x0, x0, #56, #55
	.loc	1	6	5
	ubfm	x0, x0, #17, #63
	subs	xzr, x0, #3
	b.le	L248
	orr	x0, xzr, #1
	.loc	1	6	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L328:
	.loc	1	6	50
	ldr	x1, [x0, #-8]
	.loc	1	6	50
	ubfm	x1, x1, #56, #55
	.loc	1	6	50
	ubfm	x1, x1, #17, #63
	subs	xzr, x1, #3
	b.ls	L245
	.loc	1	6	50
	ldr	x0, [x0, #8]
	.loc	1	6	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L329:
	orr	x2, xzr, x0
	b	L251
L245:
	adrp	x0, L_camlTry_raise_cross_function_caught__block35@PAGE
	add	x0, x0, L_camlTry_raise_cross_function_caught__block35@PAGEOFF
	.loc	1	6	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L248:
	movz	x0, #3393, lsl #0
	movk	x0, #3, lsl #16
	orr	x2, xzr, x0
L251:
	adrp	x0, _camlTry_raise_cross_function_caught@GOTPAGE
	ldr	x0, [x0, _camlTry_raise_cross_function_caught@GOTPAGEOFF]
	add	x0, x0, #24
	orr	x1, xzr, x2
	str	x2, [sp, #0]
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_initialize
	add	sp, fp, #0
	.cfi_restore_state
	orr	x0, xzr, #1
	.loc	1	9	18
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L330:
	.loc	1	9	5
	ldr	x0, [x0, #-8]
	.loc	1	9	5
	ubfm	x0, x0, #56, #55
	.loc	1	9	5
	ubfm	x0, x0, #17, #63
	ldr	x1, [sp, #0]
	subs	xzr, x0, #5
	b.le	L281
	str	x1, [sp, #0]
	orr	x0, xzr, #1
	.loc	1	9	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L331:
	.loc	1	9	50
	ldr	x1, [x0, #-8]
	.loc	1	9	50
	ubfm	x1, x1, #56, #55
	.loc	1	9	50
	ubfm	x1, x1, #17, #63
	ldr	x2, [sp, #0]
	subs	xzr, x1, #5
	b.ls	L278
	.loc	1	9	50
	str	x2, [sp, #0]
	.loc	1	9	50
	ldr	x0, [x0, #16]
	.loc	1	9	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L332:
	orr	x19, xzr, x0
	ldr	x0, [sp, #0]
	orr	x2, xzr, x0
	b	L284
L278:
	adrp	x0, L_camlTry_raise_cross_function_caught__block35@PAGE
	add	x0, x0, L_camlTry_raise_cross_function_caught__block35@PAGEOFF
	.loc	1	9	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L281:
	movz	x0, #21, lsl #0
	orr	x19, xzr, x0
	orr	x2, xzr, x1
L284:
	str	x2, [sp, #0]
	adrp	x0, _camlTry_raise_cross_function_caught@GOTPAGE
	ldr	x0, [x0, _camlTry_raise_cross_function_caught@GOTPAGEOFF]
	add	x0, x0, #32
	orr	x1, xzr, x19
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_initialize
	add	sp, fp, #0
	.cfi_restore_state
	orr	x0, xzr, #1
	.loc	1	14	0
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_fresh_oo_id
	add	sp, fp, #0
	.cfi_restore_state
	orr	x1, xzr, x0
	adrp	x0, _camlTry_raise_cross_function_caught__Miss245@GOTPAGE
	ldr	x0, [x0, _camlTry_raise_cross_function_caught__Miss245@GOTPAGEOFF]
	.loc	1	14	0
	add	x0, x0, #8
	.loc	1	14	0
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_initialize
	add	sp, fp, #0
	.cfi_restore_state
	ldr	x16, [x28, #40]
	add	x16, x16, #392
	subs	xzr, sp, x16
	b.cc	L333
L334:
	orr	x0, xzr, x19
	.loc	1	31	45
	bl	_camlTry_raise_cross_function_caught__black_box_int_0_7_code
L335:
	str	x0, [sp, #8]
	ldr	x0, [sp, #0]
	.loc	1	31	27
	bl	_camlTry_raise_cross_function_caught__black_box_int_0_7_code
L336:
	ldr	x1, [sp, #8]
	.loc	1	31	22
	bl	_camlTry_raise_cross_function_caught__run_6_13_code
L337:
	str	x0, [sp, #0]
	adrp	x2, _camlTry_raise_cross_function_caught__const_block66@GOTPAGE
	ldr	x2, [x2, _camlTry_raise_cross_function_caught__const_block66@GOTPAGEOFF]
	orr	x1, xzr, #1
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
	.loc	2	27	2
	bl	_camlCamlinternalFormat__make_printf_120_401_code
L338:
	orr	x1, xzr, x0
	.loc	1	11	44
	ldr	x0, [sp, #0]
	.loc	1	11	44
	and	x0, x0, #2147483647
	.loc	1	11	21
	ldr	x2, [x1, #0]
	.loc	1	11	21
	blr	x2
L339:
	orr	x0, xzr, #1
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L333:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L334
	.cfi_endproc
	.data
	.align	3
	.text
	.globl	_camlTry_raise_cross_function_caught__code_end
_camlTry_raise_cross_function_caught__code_end:
	.data
	.8byte	0
	.globl	_camlTry_raise_cross_function_caught__data_end
_camlTry_raise_cross_function_caught__data_end:
	.8byte	0
	.align	3
	.globl	_camlTry_raise_cross_function_caught__frametable
_camlTry_raise_cross_function_caught__frametable:
	.8byte	14
	.4byte	(L339 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L340 - .) + 0
	.align	3
	.4byte	(L338 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L341 - .) + 0
	.align	3
	.4byte	(L337 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L342 - .) + 0
	.align	3
	.4byte	(L336 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L343 - .) + 0
	.align	3
	.4byte	(L335 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L344 - .) + 0
	.align	3
	.4byte	(L332 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L345 - .) + 0
	.align	3
	.4byte	(L331 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L346 - .) + 0
	.align	3
	.4byte	(L330 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L347 - .) + 0
	.align	3
	.4byte	(L329 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L348 - .) + 0
	.align	3
	.4byte	(L328 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L349 - .) + 0
	.align	3
	.4byte	(L327 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L350 - .) + 0
	.align	3
	.4byte	(L215 - .) + 0
	.2byte	65
	.2byte	0
	.align	2
	.4byte	(L351 - .) + 0
	.align	3
	.4byte	(L160 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L352 - .) + 0
	.align	3
	.4byte	(L122 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L353 - .) + 0
	.align	3
	.align	2
L353:
	.4byte	(L355 - .) + 1
	.4byte	14158328
	.4byte	(L356 - .) + 1
	.4byte	17847640
	.4byte	(L357 - .) + 1
	.4byte	19940632
	.4byte	(L359 - .) + 0
	.4byte	5789176
	.align	2
L341:
	.4byte	(L355 - .) + 1
	.4byte	14158328
	.4byte	(L356 - .) + 1
	.4byte	17847640
	.4byte	(L357 - .) + 1
	.4byte	19940632
	.4byte	(L359 - .) + 1
	.4byte	5789176
	.4byte	(L360 - .) + 0
	.4byte	16262672
	.align	2
L346:
	.4byte	(L361 - .) + 0
	.4byte	4770256
	.align	2
L348:
	.4byte	(L362 - .) + 0
	.4byte	3183088
	.align	2
L352:
	.4byte	(L363 - .) + 0
	.4byte	10492008
	.align	2
L344:
	.4byte	(L360 - .) + 0
	.4byte	16299528
	.align	2
L340:
	.4byte	(L359 - .) + 1
	.4byte	5789176
	.4byte	(L360 - .) + 0
	.4byte	16262672
	.align	2
L342:
	.4byte	(L360 - .) + 0
	.4byte	16275984
	.align	2
L343:
	.4byte	(L360 - .) + 0
	.4byte	16280928
	.align	2
L351:
	.4byte	(L364 - .) + 0
	.4byte	13652176
	.align	2
L349:
	.4byte	(L362 - .) + 0
	.4byte	3197392
	.align	2
L347:
	.4byte	(L361 - .) + 0
	.4byte	4737232
	.align	2
L345:
	.4byte	(L361 - .) + 0
	.4byte	4755952
	.align	2
L350:
	.4byte	(L362 - .) + 0
	.4byte	3164368
L354:
	.ascii	"printf.ml\0"
L358:
	.ascii	"try_raise_cross_function_caught.ml\0"
	.align	2
L363:
	.4byte	(L358 - .) + 0
	.ascii	"Try_raise_cross_function_caught.find\0"
	.align	2
L360:
	.4byte	(L358 - .) + 0
	.ascii	"Try_raise_cross_function_caught\0"
	.align	2
L361:
	.4byte	(L358 - .) + 0
	.ascii	"Try_raise_cross_function_caught.reps\0"
	.align	2
L357:
	.4byte	(L354 - .) + 0
	.ascii	"Stdlib__Printf.printf\0"
	.align	2
L356:
	.4byte	(L354 - .) + 0
	.ascii	"Stdlib__Printf.fprintf\0"
	.align	2
L355:
	.4byte	(L354 - .) + 0
	.ascii	"Stdlib__Printf.kfprintf\0"
	.align	2
L364:
	.4byte	(L358 - .) + 0
	.ascii	"Try_raise_cross_function_caught.run\0"
	.align	2
L362:
	.4byte	(L358 - .) + 0
	.ascii	"Try_raise_cross_function_caught.n\0"
	.align	2
L359:
	.4byte	(L358 - .) + 0
	.ascii	"Try_raise_cross_function_caught.print_result\0"
	.align	3
