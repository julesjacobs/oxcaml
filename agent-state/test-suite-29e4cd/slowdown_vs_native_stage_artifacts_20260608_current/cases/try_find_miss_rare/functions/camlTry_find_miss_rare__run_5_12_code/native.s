_camlTry_find_miss_rare__run_5_12_code:
L_camlTry_find_miss_rare__run_5_12_code:
	.loc	1	21	23
	.cfi_startproc
	sub	sp, sp, #96
	.cfi_adjust_cfa_offset 96
	.cfi_offset 30, -8
	str	lr, [sp, #88]
	orr	x19, xzr, x0
	str	x19, [sp, #0]
	str	x1, [sp, #8]
	ldr	x20, [x28, #64]
	str	x20, [sp, #16]
	orr	x1, xzr, #1
	movz	x0, #33, lsl #0
	.file	3	"array.ml"
	.loc	3	85	13
	adrp	x8, _caml_array_make@GOTPAGE
	ldr	x8, [x8, _caml_array_make@GOTPAGEOFF]
	bl	_caml_c_call
L312:
	ldr	x16, [x28, #40]
	add	x16, x16, #472
	subs	xzr, sp, x16
	b.cc	L313
L314:
	orr	x21, xzr, x0
	orr	x22, xzr, #1
	ldr	x19, [sp, #0]
	ldr	x23, [sp, #8]
	ldr	x20, [sp, #16]
L185:
	orr	x0, xzr, #1
	.loc	3	86	3
	add	x0, x0, x22, lsl #1
	.loc	3	87	5
	ldrb	w1, [x21, #-8]
	subs	xzr, x1, #254
	b.ne	L191
	adrp	x0, L_camlTry_find_miss_rare__invalid605@PAGE
	add	x0, x0, L_camlTry_find_miss_rare__invalid605@PAGEOFF
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_flambda2_invalid
	add	sp, fp, #0
	.cfi_restore_state
L191:
	movn	x1, #0
	.loc	1	22	34
	add	x1, x1, x0, lsl #1
	.loc	3	87	5
	add	x0, x21, x0, lsl #2
	.loc	3	87	5
	sub	x0, x0, #4
	.loc	3	87	5
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_modify
	add	sp, fp, #0
	.cfi_restore_state
	.loc	3	86	3
	add	x22, x22, #1
	subs	xzr, x22, #15
	b.le	L185
	orr	x1, xzr, x21
	subs	xzr, x23, #3
	b.lt	L298
	.loc	1	24	2
	str	x1, [sp, #8]
	.loc	1	24	2
	sbfm	x2, x23, #1, #63
	str	x2, [sp, #24]
	orr	x0, xzr, #1
	orr	x3, xzr, #1
L216:
	orr	x4, xzr, #1
	.loc	1	24	2
	add	x4, x4, x3, lsl #1
	subs	xzr, x19, #3
	b.lt	L288
	.loc	1	25	4
	str	x4, [sp, #32]
	.loc	1	25	4
	str	x3, [sp, #80]
	.loc	1	25	4
	sbfm	x3, x19, #1, #63
	str	x3, [sp, #40]
	orr	x5, xzr, #1
L228:
	orr	x2, xzr, #1
	.loc	1	25	4
	add	x2, x2, x5, lsl #1
	.loc	1	26	19
	and	x3, x2, #511
	subs	xzr, x3, #1
	b.ne	L237
	movn	x3, #0
	b	L244
L237:
	.loc	1	26	49
	add	x3, x2, x4
	.loc	1	26	49
	sub	x3, x3, #1
	.loc	1	26	48
	and	x3, x3, #31
	movn	x4, #0
	.loc	1	26	47
	add	x3, x4, x3, lsl #1
L244:
	str	x2, [sp, #64]
	str	x0, [sp, #56]
	str	x5, [sp, #48]
	ldr	x0, [x28, #64]
	str	x0, [sp, #72]
	adr	x16, L250
	stp	x26, x16, [sp, #-16]!
	.cfi_adjust_cfa_offset 16
	add	x26, sp, #0
	orr	x2, xzr, #1
	orr	x0, xzr, x3
	.loc	1	27	24
	bl	_camlTry_find_miss_rare__scan_4_11_code
L315:
	.loc	1	27	17
	ldr	x1, [sp, #72]
	.loc	1	27	17
	add	x0, x1, x0
	.loc	1	27	17
	sub	x0, x0, #1
	ldr	x26, [sp], #16
	.cfi_adjust_cfa_offset -16
	ldr	x19, [sp, #0]
	ldr	x1, [sp, #8]
	ldr	x2, [sp, #24]
	ldr	x4, [sp, #32]
	ldr	x3, [sp, #40]
	ldr	x5, [sp, #48]
	b	L274
L250:
	ldr	x1, [sp, #72]
	str	x1, [x28, #64]
	adrp	x6, _camlTry_find_miss_rare__Miss281@GOTPAGE
	ldr	x6, [x6, _camlTry_find_miss_rare__Miss281@GOTPAGEOFF]
	ldr	x19, [sp, #0]
	ldr	x1, [sp, #8]
	ldr	x2, [sp, #24]
	ldr	x4, [sp, #32]
	ldr	x3, [sp, #40]
	ldr	x5, [sp, #48]
	ldr	x7, [sp, #56]
	ldr	x8, [sp, #64]
	subs	xzr, x0, x6
	b.ne	L270
	.loc	1	28	26
	add	x0, x7, x8
	.loc	1	28	26
	sub	x0, x0, #1
	.loc	1	28	26
	and	x0, x0, #2147483647
	b	L274
L270:
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L274:
	.loc	1	25	4
	add	x5, x5, #1
	subs	xzr, x5, x3
	b.le	L228
	ldr	x3, [sp, #80]
L288:
	.loc	1	24	2
	add	x3, x3, #1
	subs	xzr, x3, x2
	b.le	L216
	ldr	x20, [sp, #16]
	b	L301
L298:
	orr	x0, xzr, #1
L301:
	str	x20, [x28, #64]
	ldr	lr, [sp, #88]
	add	sp, sp, #96
	.cfi_adjust_cfa_offset -96
	ret
	.cfi_adjust_cfa_offset 96
L313:
	movz	x16, #112
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L314
	.cfi_endproc
	.text
	.align	3
	.globl	_camlTry_find_miss_rare__entry
_camlTry_find_miss_rare__entry:
L_camlTry_find_miss_rare__entry:
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
L427:
	.loc	1	6	5
	ldr	x0, [x0, #-8]
	.loc	1	6	5
	ubfm	x0, x0, #56, #55
	.loc	1	6	5
	ubfm	x0, x0, #17, #63
	subs	xzr, x0, #3
	b.le	L348
	orr	x0, xzr, #1
	.loc	1	6	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L428:
	.loc	1	6	50
	ldr	x1, [x0, #-8]
	.loc	1	6	50
	ubfm	x1, x1, #56, #55
	.loc	1	6	50
	ubfm	x1, x1, #17, #63
	subs	xzr, x1, #3
	b.ls	L345
	.loc	1	6	50
	ldr	x0, [x0, #8]
	.loc	1	6	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L429:
	orr	x2, xzr, x0
	b	L351
L345:
	adrp	x0, L_camlTry_find_miss_rare__block35@PAGE
	add	x0, x0, L_camlTry_find_miss_rare__block35@PAGEOFF
	.loc	1	6	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L348:
	movz	x0, #3393, lsl #0
	movk	x0, #3, lsl #16
	orr	x2, xzr, x0
L351:
	adrp	x0, _camlTry_find_miss_rare@GOTPAGE
	ldr	x0, [x0, _camlTry_find_miss_rare@GOTPAGEOFF]
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
L430:
	.loc	1	9	5
	ldr	x0, [x0, #-8]
	.loc	1	9	5
	ubfm	x0, x0, #56, #55
	.loc	1	9	5
	ubfm	x0, x0, #17, #63
	ldr	x1, [sp, #0]
	subs	xzr, x0, #5
	b.le	L381
	str	x1, [sp, #0]
	orr	x0, xzr, #1
	.loc	1	9	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L431:
	.loc	1	9	50
	ldr	x1, [x0, #-8]
	.loc	1	9	50
	ubfm	x1, x1, #56, #55
	.loc	1	9	50
	ubfm	x1, x1, #17, #63
	ldr	x2, [sp, #0]
	subs	xzr, x1, #5
	b.ls	L378
	.loc	1	9	50
	str	x2, [sp, #0]
	.loc	1	9	50
	ldr	x0, [x0, #16]
	.loc	1	9	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L432:
	orr	x19, xzr, x0
	ldr	x0, [sp, #0]
	orr	x2, xzr, x0
	b	L384
L378:
	adrp	x0, L_camlTry_find_miss_rare__block35@PAGE
	add	x0, x0, L_camlTry_find_miss_rare__block35@PAGEOFF
	.loc	1	9	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L381:
	movz	x0, #21, lsl #0
	orr	x19, xzr, x0
	orr	x2, xzr, x1
L384:
	str	x2, [sp, #0]
	adrp	x0, _camlTry_find_miss_rare@GOTPAGE
	ldr	x0, [x0, _camlTry_find_miss_rare@GOTPAGEOFF]
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
	adrp	x0, _camlTry_find_miss_rare__Miss281@GOTPAGE
	ldr	x0, [x0, _camlTry_find_miss_rare__Miss281@GOTPAGEOFF]
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
	b.cc	L433
L434:
	orr	x0, xzr, x19
	.loc	1	33	45
	bl	_camlTry_find_miss_rare__black_box_int_0_7_code
L435:
	str	x0, [sp, #8]
	ldr	x0, [sp, #0]
	.loc	1	33	27
	bl	_camlTry_find_miss_rare__black_box_int_0_7_code
L436:
	ldr	x1, [sp, #8]
	.loc	1	33	22
	bl	_camlTry_find_miss_rare__run_5_12_code
L437:
	str	x0, [sp, #0]
	adrp	x2, _camlTry_find_miss_rare__const_block66@GOTPAGE
	ldr	x2, [x2, _camlTry_find_miss_rare__const_block66@GOTPAGEOFF]
	orr	x1, xzr, #1
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
	.loc	2	27	2
	bl	_camlCamlinternalFormat__make_printf_120_401_code
L438:
	orr	x1, xzr, x0
	.loc	1	11	44
	ldr	x0, [sp, #0]
	.loc	1	11	44
	and	x0, x0, #2147483647
	.loc	1	11	21
	ldr	x2, [x1, #0]
	.loc	1	11	21
	blr	x2
L439:
	orr	x0, xzr, #1
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L433:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L434
	.cfi_endproc
	.data
	.align	3
	.text
	.globl	_camlTry_find_miss_rare__code_end
_camlTry_find_miss_rare__code_end:
	.data
	.8byte	0
	.globl	_camlTry_find_miss_rare__data_end
_camlTry_find_miss_rare__data_end:
	.8byte	0
	.align	3
	.globl	_camlTry_find_miss_rare__frametable
_camlTry_find_miss_rare__frametable:
	.8byte	16
	.4byte	(L439 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L440 - .) + 0
	.align	3
	.4byte	(L438 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L441 - .) + 0
	.align	3
	.4byte	(L437 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L442 - .) + 0
	.align	3
	.4byte	(L436 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L443 - .) + 0
	.align	3
	.4byte	(L435 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L444 - .) + 0
	.align	3
	.4byte	(L432 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L445 - .) + 0
	.align	3
	.4byte	(L431 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L446 - .) + 0
	.align	3
	.4byte	(L430 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L447 - .) + 0
	.align	3
	.4byte	(L429 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L448 - .) + 0
	.align	3
	.4byte	(L428 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L449 - .) + 0
	.align	3
	.4byte	(L427 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L450 - .) + 0
	.align	3
	.4byte	(L315 - .) + 0
	.2byte	113
	.2byte	1
	.2byte	24
	.align	2
	.4byte	(L451 - .) + 0
	.align	3
	.4byte	(L312 - .) + 0
	.2byte	97
	.2byte	0
	.align	2
	.4byte	(L452 - .) + 0
	.align	3
	.4byte	(L173 - .) + 0
	.2byte	33
	.2byte	2
	.2byte	0
	.2byte	8
	.align	2
	.4byte	(L453 - .) + 0
	.align	3
	.4byte	(L170 - .) + 0
	.2byte	34
	.2byte	4
	.2byte	0
	.2byte	3
	.2byte	7
	.2byte	8
	.byte	1
	.byte	0
	.align	3
	.4byte	(L122 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L454 - .) + 0
	.align	3
	.align	2
L440:
	.4byte	(L456 - .) + 1
	.4byte	5789176
	.4byte	(L457 - .) + 0
	.4byte	17311248
	.align	2
L443:
	.4byte	(L457 - .) + 0
	.4byte	17329504
	.align	2
L450:
	.4byte	(L458 - .) + 0
	.4byte	3164368
	.align	2
L444:
	.4byte	(L457 - .) + 0
	.4byte	17348104
	.align	2
L447:
	.4byte	(L459 - .) + 0
	.4byte	4737232
	.align	2
L446:
	.4byte	(L459 - .) + 0
	.4byte	4770256
	.align	2
L442:
	.4byte	(L457 - .) + 0
	.4byte	17324560
	.align	2
L448:
	.4byte	(L458 - .) + 0
	.4byte	3183088
	.align	2
L449:
	.4byte	(L458 - .) + 0
	.4byte	3197392
	.align	2
L441:
	.4byte	(L461 - .) + 1
	.4byte	14158328
	.4byte	(L462 - .) + 1
	.4byte	17847640
	.4byte	(L463 - .) + 1
	.4byte	19940632
	.4byte	(L456 - .) + 1
	.4byte	5789176
	.4byte	(L457 - .) + 0
	.4byte	17311248
	.align	2
L451:
	.4byte	(L464 - .) + 0
	.4byte	14180640
	.align	2
L452:
	.4byte	(L466 - .) + 1
	.4byte	44578008
	.4byte	(L464 - .) + 0
	.4byte	11544896
	.align	2
L445:
	.4byte	(L459 - .) + 0
	.4byte	4755952
	.align	2
L454:
	.4byte	(L461 - .) + 1
	.4byte	14158328
	.4byte	(L462 - .) + 1
	.4byte	17847640
	.4byte	(L463 - .) + 1
	.4byte	19940632
	.4byte	(L456 - .) + 0
	.4byte	5789176
	.align	2
L453:
	.4byte	(L467 - .) + 0
	.4byte	9447712
L455:
	.ascii	"try_find_miss_rare.ml\0"
L460:
	.ascii	"printf.ml\0"
L465:
	.ascii	"array.ml\0"
	.align	2
L458:
	.4byte	(L455 - .) + 0
	.ascii	"Try_find_miss_rare.n\0"
	.align	2
L466:
	.4byte	(L465 - .) + 0
	.ascii	"Stdlib__Array.init\0"
	.align	2
L464:
	.4byte	(L455 - .) + 0
	.ascii	"Try_find_miss_rare.run\0"
	.align	2
L463:
	.4byte	(L460 - .) + 0
	.ascii	"Stdlib__Printf.printf\0"
	.align	2
L462:
	.4byte	(L460 - .) + 0
	.ascii	"Stdlib__Printf.fprintf\0"
	.align	2
L459:
	.4byte	(L455 - .) + 0
	.ascii	"Try_find_miss_rare.reps\0"
	.align	2
L461:
	.4byte	(L460 - .) + 0
	.ascii	"Stdlib__Printf.kfprintf\0"
	.align	2
L456:
	.4byte	(L455 - .) + 0
	.ascii	"Try_find_miss_rare.print_result\0"
	.align	2
L467:
	.4byte	(L455 - .) + 0
	.ascii	"Try_find_miss_rare.scan\0"
	.align	2
L457:
	.4byte	(L455 - .) + 0
	.ascii	"Try_find_miss_rare\0"
	.align	3
