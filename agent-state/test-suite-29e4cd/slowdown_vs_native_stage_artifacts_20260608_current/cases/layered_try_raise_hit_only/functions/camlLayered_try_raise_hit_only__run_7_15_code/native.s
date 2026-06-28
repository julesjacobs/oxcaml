_camlLayered_try_raise_hit_only__run_7_15_code:
L_camlLayered_try_raise_hit_only__run_7_15_code:
	.loc	1	39	23
	.cfi_startproc
	sub	sp, sp, #64
	.cfi_adjust_cfa_offset 64
	.cfi_offset 30, -8
	str	lr, [sp, #56]
	ldr	x16, [x28, #40]
	add	x16, x16, #424
	subs	xzr, sp, x16
	b.cc	L286
L287:
	orr	x2, xzr, x0
	str	x2, [sp, #0]
	str	x1, [sp, #8]
	adrp	x1, L_camlLayered_try_raise_hit_only__const_block139@PAGE
	add	x1, x1, L_camlLayered_try_raise_hit_only__const_block139@PAGEOFF
	movz	x0, #13, lsl #0
	.loc	1	40	12
	bl	_camlLayered_try_raise_hit_only__open_layers_6_14_code
L288:
	orr	x1, xzr, x0
	ldr	x2, [sp, #0]
	ldr	x0, [sp, #8]
	subs	xzr, x0, #3
	b.lt	L277
	.loc	1	42	2
	str	x1, [sp, #8]
	.loc	1	42	2
	sbfm	x3, x0, #1, #63
	str	x3, [sp, #16]
	orr	x0, xzr, #1
	orr	x4, xzr, #1
	subs	xzr, x2, #3
	b.lt	L267
	b	L246
L244:
	subs	xzr, x2, #3
	b.lt	L267
L246:
	.loc	1	43	4
	str	x4, [sp, #48]
	.loc	1	43	4
	sbfm	x2, x2, #1, #63
	str	x2, [sp, #24]
	orr	x4, xzr, #1
L252:
	str	x0, [sp, #40]
	str	x4, [sp, #32]
	orr	x0, xzr, x1
	.loc	1	44	20
	bl	_camlLayered_try_raise_hit_only__find_5_13_code
L289:
	orr	x1, xzr, x0
	.loc	1	44	13
	ldr	x0, [sp, #40]
	.loc	1	44	13
	add	x0, x0, x1
	.loc	1	44	13
	sub	x0, x0, #1
	.loc	1	43	4
	ldr	x4, [sp, #32]
	.loc	1	43	4
	add	x4, x4, #1
	ldr	x2, [sp, #0]
	ldr	x1, [sp, #8]
	ldr	x3, [sp, #16]
	ldr	x5, [sp, #24]
	subs	xzr, x4, x5
	b.le	L252
	ldr	x4, [sp, #48]
L267:
	.loc	1	42	2
	add	x4, x4, #1
	subs	xzr, x4, x3
	b.le	L244
	ldr	lr, [sp, #56]
	add	sp, sp, #64
	.cfi_adjust_cfa_offset -64
	ret
	.cfi_adjust_cfa_offset 64
L277:
	orr	x0, xzr, #1
	ldr	lr, [sp, #56]
	add	sp, sp, #64
	.cfi_adjust_cfa_offset -64
	ret
	.cfi_adjust_cfa_offset 64
L286:
	movz	x16, #64
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L287
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLayered_try_raise_hit_only__entry
_camlLayered_try_raise_hit_only__entry:
L_camlLayered_try_raise_hit_only__entry:
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
L401:
	.loc	1	6	5
	ldr	x0, [x0, #-8]
	.loc	1	6	5
	ubfm	x0, x0, #56, #55
	.loc	1	6	5
	ubfm	x0, x0, #17, #63
	subs	xzr, x0, #3
	b.le	L322
	orr	x0, xzr, #1
	.loc	1	6	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L402:
	.loc	1	6	50
	ldr	x1, [x0, #-8]
	.loc	1	6	50
	ubfm	x1, x1, #56, #55
	.loc	1	6	50
	ubfm	x1, x1, #17, #63
	subs	xzr, x1, #3
	b.ls	L319
	.loc	1	6	50
	ldr	x0, [x0, #8]
	.loc	1	6	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L403:
	orr	x2, xzr, x0
	b	L325
L319:
	adrp	x0, L_camlLayered_try_raise_hit_only__block35@PAGE
	add	x0, x0, L_camlLayered_try_raise_hit_only__block35@PAGEOFF
	.loc	1	6	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L322:
	movz	x0, #3393, lsl #0
	movk	x0, #3, lsl #16
	orr	x2, xzr, x0
L325:
	adrp	x0, _camlLayered_try_raise_hit_only@GOTPAGE
	ldr	x0, [x0, _camlLayered_try_raise_hit_only@GOTPAGEOFF]
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
L404:
	.loc	1	9	5
	ldr	x0, [x0, #-8]
	.loc	1	9	5
	ubfm	x0, x0, #56, #55
	.loc	1	9	5
	ubfm	x0, x0, #17, #63
	ldr	x1, [sp, #0]
	subs	xzr, x0, #5
	b.le	L355
	str	x1, [sp, #0]
	orr	x0, xzr, #1
	.loc	1	9	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L405:
	.loc	1	9	50
	ldr	x1, [x0, #-8]
	.loc	1	9	50
	ubfm	x1, x1, #56, #55
	.loc	1	9	50
	ubfm	x1, x1, #17, #63
	ldr	x2, [sp, #0]
	subs	xzr, x1, #5
	b.ls	L352
	.loc	1	9	50
	str	x2, [sp, #0]
	.loc	1	9	50
	ldr	x0, [x0, #16]
	.loc	1	9	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L406:
	orr	x19, xzr, x0
	ldr	x0, [sp, #0]
	orr	x2, xzr, x0
	b	L358
L352:
	adrp	x0, L_camlLayered_try_raise_hit_only__block35@PAGE
	add	x0, x0, L_camlLayered_try_raise_hit_only__block35@PAGEOFF
	.loc	1	9	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L355:
	movz	x0, #21, lsl #0
	orr	x19, xzr, x0
	orr	x2, xzr, x1
L358:
	str	x2, [sp, #0]
	adrp	x0, _camlLayered_try_raise_hit_only@GOTPAGE
	ldr	x0, [x0, _camlLayered_try_raise_hit_only@GOTPAGEOFF]
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
	adrp	x0, _camlLayered_try_raise_hit_only__Not_found_same275@GOTPAGE
	ldr	x0, [x0, _camlLayered_try_raise_hit_only__Not_found_same275@GOTPAGEOFF]
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
	b.cc	L407
L408:
	orr	x0, xzr, x19
	.loc	1	49	45
	bl	_camlLayered_try_raise_hit_only__black_box_int_0_8_code
L409:
	str	x0, [sp, #8]
	ldr	x0, [sp, #0]
	.loc	1	49	27
	bl	_camlLayered_try_raise_hit_only__black_box_int_0_8_code
L410:
	ldr	x1, [sp, #8]
	.loc	1	49	22
	bl	_camlLayered_try_raise_hit_only__run_7_15_code
L411:
	str	x0, [sp, #0]
	adrp	x2, _camlLayered_try_raise_hit_only__const_block66@GOTPAGE
	ldr	x2, [x2, _camlLayered_try_raise_hit_only__const_block66@GOTPAGEOFF]
	orr	x1, xzr, #1
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
	.loc	2	27	2
	bl	_camlCamlinternalFormat__make_printf_120_401_code
L412:
	orr	x1, xzr, x0
	.loc	1	11	44
	ldr	x0, [sp, #0]
	.loc	1	11	44
	and	x0, x0, #2147483647
	.loc	1	11	21
	ldr	x2, [x1, #0]
	.loc	1	11	21
	blr	x2
L413:
	orr	x0, xzr, #1
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L407:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L408
	.cfi_endproc
	.data
	.align	3
	.text
	.globl	_camlLayered_try_raise_hit_only__code_end
_camlLayered_try_raise_hit_only__code_end:
	.data
	.8byte	0
	.globl	_camlLayered_try_raise_hit_only__data_end
_camlLayered_try_raise_hit_only__data_end:
	.8byte	0
	.align	3
	.globl	_camlLayered_try_raise_hit_only__frametable
_camlLayered_try_raise_hit_only__frametable:
	.8byte	20
	.4byte	(L413 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L414 - .) + 0
	.align	3
	.4byte	(L412 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L415 - .) + 0
	.align	3
	.4byte	(L411 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L416 - .) + 0
	.align	3
	.4byte	(L410 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L417 - .) + 0
	.align	3
	.4byte	(L409 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L418 - .) + 0
	.align	3
	.4byte	(L406 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L419 - .) + 0
	.align	3
	.4byte	(L405 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L420 - .) + 0
	.align	3
	.4byte	(L404 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L421 - .) + 0
	.align	3
	.4byte	(L403 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L422 - .) + 0
	.align	3
	.4byte	(L402 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L423 - .) + 0
	.align	3
	.4byte	(L401 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L424 - .) + 0
	.align	3
	.4byte	(L289 - .) + 0
	.2byte	65
	.2byte	1
	.2byte	8
	.align	2
	.4byte	(L425 - .) + 0
	.align	3
	.4byte	(L288 - .) + 0
	.2byte	65
	.2byte	0
	.align	2
	.4byte	(L426 - .) + 0
	.align	3
	.4byte	(L228 - .) + 0
	.2byte	18
	.2byte	1
	.2byte	3
	.byte	1
	.byte	1
	.align	3
	.4byte	(L225 - .) + 0
	.2byte	18
	.2byte	1
	.2byte	1
	.byte	1
	.byte	0
	.align	3
	.4byte	(L224 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L427 - .) + 0
	.align	3
	.4byte	(L221 - .) + 0
	.2byte	18
	.2byte	1
	.2byte	1
	.byte	1
	.byte	0
	.align	3
	.4byte	(L220 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L428 - .) + 0
	.align	3
	.4byte	(L175 - .) + 0
	.2byte	49
	.2byte	1
	.2byte	16
	.align	2
	.4byte	(L429 - .) + 0
	.align	3
	.4byte	(L122 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L430 - .) + 0
	.align	3
	.align	2
L422:
	.4byte	(L432 - .) + 0
	.4byte	3183088
	.align	2
L427:
	.4byte	(L433 - .) + 0
	.4byte	19417440
	.align	2
L417:
	.4byte	(L434 - .) + 0
	.4byte	25718112
	.align	2
L425:
	.4byte	(L435 - .) + 0
	.4byte	23089376
	.align	2
L423:
	.4byte	(L432 - .) + 0
	.4byte	3197392
	.align	2
L414:
	.4byte	(L436 - .) + 1
	.4byte	5789176
	.4byte	(L434 - .) + 0
	.4byte	25699856
	.align	2
L419:
	.4byte	(L437 - .) + 0
	.4byte	4755952
	.align	2
L421:
	.4byte	(L437 - .) + 0
	.4byte	4737232
	.align	2
L418:
	.4byte	(L434 - .) + 0
	.4byte	25736712
	.align	2
L416:
	.4byte	(L434 - .) + 0
	.4byte	25713168
	.align	2
L429:
	.4byte	(L438 - .) + 0
	.4byte	12064888
	.align	2
L426:
	.4byte	(L435 - .) + 0
	.4byte	20984264
	.align	2
L420:
	.4byte	(L437 - .) + 0
	.4byte	4770256
	.align	2
L430:
	.4byte	(L440 - .) + 1
	.4byte	14158328
	.4byte	(L441 - .) + 1
	.4byte	17847640
	.4byte	(L442 - .) + 1
	.4byte	19940632
	.4byte	(L436 - .) + 0
	.4byte	5789176
	.align	2
L428:
	.4byte	(L433 - .) + 0
	.4byte	18893152
	.align	2
L424:
	.4byte	(L432 - .) + 0
	.4byte	3164368
	.align	2
L415:
	.4byte	(L440 - .) + 1
	.4byte	14158328
	.4byte	(L441 - .) + 1
	.4byte	17847640
	.4byte	(L442 - .) + 1
	.4byte	19940632
	.4byte	(L436 - .) + 1
	.4byte	5789176
	.4byte	(L434 - .) + 0
	.4byte	25699856
L431:
	.ascii	"layered_try_raise_hit_only.ml\0"
L439:
	.ascii	"printf.ml\0"
	.align	2
L434:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only\0"
	.align	2
L435:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only.run\0"
	.align	2
L442:
	.4byte	(L439 - .) + 0
	.ascii	"Stdlib__Printf.printf\0"
	.align	2
L441:
	.4byte	(L439 - .) + 0
	.ascii	"Stdlib__Printf.fprintf\0"
	.align	2
L440:
	.4byte	(L439 - .) + 0
	.ascii	"Stdlib__Printf.kfprintf\0"
	.align	2
L433:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only.open_layers\0"
	.align	2
L437:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only.reps\0"
	.align	2
L436:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only.print_result\0"
	.align	2
L438:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only.find\0"
	.align	2
L432:
	.4byte	(L431 - .) + 0
	.ascii	"Layered_try_raise_hit_only.n\0"
	.align	3
