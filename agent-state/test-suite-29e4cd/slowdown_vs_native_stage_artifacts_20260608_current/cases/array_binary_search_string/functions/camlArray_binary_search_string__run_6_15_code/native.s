_camlArray_binary_search_string__run_6_15_code:
L_camlArray_binary_search_string__run_6_15_code:
	.loc	1	25	23
	.cfi_startproc
	sub	sp, sp, #80
	.cfi_adjust_cfa_offset 80
	.cfi_offset 30, -8
	str	lr, [sp, #72]
	orr	x19, xzr, x0
	str	x19, [sp, #40]
	str	x1, [sp, #48]
	orr	x1, xzr, #1
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	.file	3	"stdlib.ml"
	.loc	3	280	2
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L340:
	.loc	3	225	37
	ldr	x1, [x0, #-8]
	.loc	3	225	37
	ubfm	x1, x1, #56, #55
	.loc	3	225	37
	ubfm	x1, x1, #18, #63
	.loc	3	225	37
	ubfm	x1, x1, #61, #60
	.loc	3	225	37
	sub	x1, x1, #1
	.loc	3	225	37
	add	x2, x0, x1
	.loc	3	225	37
	str	x0, [sp, #0]
	.loc	3	225	37
	ldrb	w0, [x2, #0]
	.loc	3	225	37
	sub	x0, x1, x0
	orr	x1, xzr, #1
	.loc	3	225	37
	add	x1, x1, x0, lsl #1
	.loc	3	226	23
	add	x0, x1, #34
	.loc	3	226	23
	str	x1, [sp, #8]
	.loc	3	226	10
	adrp	x8, _caml_create_bytes@GOTPAGE
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	bl	_caml_c_call
L341:
	orr	x19, xzr, x0
	movz	x4, #35, lsl #0
	orr	x3, xzr, #1
	orr	x1, xzr, #1
	adrp	x0, L_camlArray_binary_search_string__immstring129@PAGE
	add	x0, x0, L_camlArray_binary_search_string__immstring129@PAGEOFF
	orr	x2, xzr, x19
	.loc	3	227	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	movz	x3, #35, lsl #0
	orr	x1, xzr, #1
	ldr	x0, [sp, #0]
	orr	x2, xzr, x19
	ldr	x4, [sp, #8]
	.loc	3	228	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	.loc	1	27	28
	ldr	x16, [x28, #0]
	sub	x27, x27, #24
	subs	xzr, x27, x16
	b.cc	L344
L343:
	add	x1, x27, #8
	orr	x0, xzr, #2048
	.loc	1	27	28
	str	x0, [x1, #-8]
	.loc	1	27	28
	str	x19, [x1, #0]
	orr	x0, xzr, #1
	.loc	1	27	28
	str	x0, [x1, #8]
	movz	x0, #129, lsl #0
	.file	4	"array.ml"
	.loc	4	85	13
	adrp	x8, _caml_array_make@GOTPAGE
	ldr	x8, [x8, _caml_array_make@GOTPAGEOFF]
	bl	_caml_c_call
L345:
	ldr	x16, [x28, #40]
	add	x16, x16, #440
	subs	xzr, sp, x16
	b.cc	L346
L347:
	str	x0, [sp, #0]
	orr	x19, xzr, #1
L188:
	orr	x0, xzr, #1
	.loc	4	86	3
	add	x1, x0, x19, lsl #1
	.loc	4	86	3
	str	x19, [sp, #24]
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	str	x1, [sp, #32]
	.loc	3	280	2
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L348:
	.loc	3	225	37
	ldr	x1, [x0, #-8]
	.loc	3	225	37
	ubfm	x1, x1, #56, #55
	.loc	3	225	37
	ubfm	x1, x1, #18, #63
	.loc	3	225	37
	ubfm	x1, x1, #61, #60
	.loc	3	225	37
	sub	x1, x1, #1
	.loc	3	225	37
	add	x2, x0, x1
	.loc	3	225	37
	str	x0, [sp, #8]
	.loc	3	225	37
	ldrb	w0, [x2, #0]
	.loc	3	225	37
	sub	x0, x1, x0
	orr	x1, xzr, #1
	.loc	3	225	37
	add	x1, x1, x0, lsl #1
	.loc	3	226	23
	add	x0, x1, #34
	.loc	3	226	23
	str	x1, [sp, #16]
	.loc	3	226	10
	adrp	x8, _caml_create_bytes@GOTPAGE
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	bl	_caml_c_call
L349:
	orr	x20, xzr, x0
	movz	x4, #35, lsl #0
	orr	x3, xzr, #1
	orr	x1, xzr, #1
	adrp	x0, L_camlArray_binary_search_string__immstring129@PAGE
	add	x0, x0, L_camlArray_binary_search_string__immstring129@PAGEOFF
	orr	x2, xzr, x20
	.loc	3	227	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	movz	x3, #35, lsl #0
	orr	x1, xzr, #1
	ldr	x0, [sp, #8]
	orr	x2, xzr, x20
	ldr	x4, [sp, #16]
	.loc	3	228	2
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_blit_string
	add	sp, fp, #0
	.cfi_restore_state
	.loc	4	87	5
	ldr	x21, [sp, #0]
	.loc	4	87	5
	ldrb	w0, [x21, #-8]
	ldr	x19, [sp, #24]
	ldr	x2, [sp, #32]
	subs	xzr, x0, #254
	b.ne	L208
	adrp	x0, L_camlArray_binary_search_string__invalid776@PAGE
	add	x0, x0, L_camlArray_binary_search_string__invalid776@PAGEOFF
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_flambda2_invalid
	add	sp, fp, #0
	.cfi_restore_state
L208:
	.loc	1	27	28
	ldr	x16, [x28, #0]
	sub	x27, x27, #24
	subs	xzr, x27, x16
	b.cc	L352
L351:
	add	x1, x27, #8
	orr	x0, xzr, #2048
	.loc	1	27	28
	str	x0, [x1, #-8]
	.loc	1	27	28
	str	x20, [x1, #0]
	.loc	1	27	28
	str	x2, [x1, #8]
	.loc	4	87	5
	add	x0, x21, x2, lsl #2
	.loc	4	87	5
	sub	x0, x0, #4
	.loc	4	87	5
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_modify
	add	sp, fp, #0
	.cfi_restore_state
	.loc	4	86	3
	add	x19, x19, #1
	subs	xzr, x19, #63
	b.le	L188
	.loc	4	155	23
	ldr	x0, [x21, #0]
	.loc	4	155	23
	str	x21, [sp, #0]
	.loc	1	29	23
	ldr	x1, [x0, #0]
	movz	x0, #129, lsl #0
	.loc	4	155	12
	adrp	x8, _caml_array_make@GOTPAGE
	ldr	x8, [x8, _caml_array_make@GOTPAGEOFF]
	bl	_caml_c_call
L353:
	orr	x20, xzr, x0
	orr	x22, xzr, #1
	ldr	x19, [sp, #40]
	ldr	x23, [sp, #48]
	ldr	x21, [sp, #0]
L233:
	orr	x0, xzr, #1
	.loc	4	156	4
	add	x0, x0, x22, lsl #1
	.loc	4	157	23
	add	x1, x21, x0, lsl #2
	.loc	4	157	23
	ldr	x1, [x1, #-4]
	.loc	1	29	23
	ldr	x1, [x1, #0]
	.loc	4	157	6
	ldrb	w2, [x20, #-8]
	subs	xzr, x2, #254
	b.ne	L248
	.loc	4	157	6
	add	x0, x20, x0, lsl #2
	.loc	4	157	6
	ldr	d0, [x1, #0]
	.loc	4	157	6
	str	d0, [x0, #-4]
	b	L254
L248:
	.loc	4	157	6
	add	x0, x20, x0, lsl #2
	.loc	4	157	6
	sub	x0, x0, #4
	.loc	4	157	6
	add	fp, sp, #0
	.cfi_remember_state
	.cfi_def_cfa_register %29
	ldr	x16, [x28, #104]
	add	sp, x16, #0
	bl	_caml_modify
	add	sp, fp, #0
	.cfi_restore_state
L254:
	.loc	4	156	4
	add	x22, x22, #1
	subs	xzr, x22, #63
	b.le	L233
	subs	xzr, x23, #3
	b.lt	L313
	.loc	1	31	2
	str	x20, [sp, #8]
	.loc	1	31	2
	sbfm	x1, x23, #1, #63
	str	x1, [sp, #16]
	orr	x0, xzr, #1
	orr	x2, xzr, #1
L272:
	orr	x3, xzr, #1
	.loc	1	31	2
	add	x3, x3, x2, lsl #1
	subs	xzr, x19, #3
	b.lt	L303
	.loc	1	32	4
	str	x3, [sp, #24]
	.loc	1	32	4
	str	x2, [sp, #64]
	.loc	1	32	4
	sbfm	x1, x19, #1, #63
	str	x1, [sp, #32]
	orr	x2, xzr, #1
L284:
	.loc	1	33	49
	str	x0, [sp, #56]
	.loc	1	33	49
	add	x0, x3, x2, lsl #1
	.loc	1	33	49
	str	x2, [sp, #48]
	.loc	1	33	48
	and	x0, x0, #127
	.loc	1	33	25
	add	x0, x20, x0, lsl #2
	.loc	1	33	25
	ldr	x0, [x0, #-4]
	orr	x1, xzr, x21
	.loc	1	33	20
	bl	_camlArray_binary_search_string__find_4_13_code
L354:
	orr	x1, xzr, x0
	.loc	1	33	13
	ldr	x0, [sp, #56]
	.loc	1	33	13
	add	x0, x0, x1
	.loc	1	33	13
	sub	x0, x0, #1
	.loc	1	32	4
	ldr	x2, [sp, #48]
	.loc	1	32	4
	add	x2, x2, #1
	ldr	x19, [sp, #40]
	ldr	x21, [sp, #0]
	ldr	x20, [sp, #8]
	ldr	x1, [sp, #16]
	ldr	x3, [sp, #24]
	ldr	x4, [sp, #32]
	subs	xzr, x2, x4
	b.le	L284
	ldr	x2, [sp, #64]
L303:
	.loc	1	31	2
	add	x2, x2, #1
	subs	xzr, x2, x1
	b.le	L272
	ldr	lr, [sp, #72]
	add	sp, sp, #80
	.cfi_adjust_cfa_offset -80
	ret
	.cfi_adjust_cfa_offset 80
L313:
	orr	x0, xzr, #1
	ldr	lr, [sp, #72]
	add	sp, sp, #80
	.cfi_adjust_cfa_offset -80
	ret
	.cfi_adjust_cfa_offset 80
L352:
	bl	_caml_call_gc
L350:
	b	L351
L344:
	bl	_caml_call_gc
L342:
	b	L343
L346:
	movz	x16, #80
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L347
	.cfi_endproc
	.text
	.align	3
	.globl	_camlArray_binary_search_string__entry
_camlArray_binary_search_string__entry:
L_camlArray_binary_search_string__entry:
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
L463:
	.loc	1	6	5
	ldr	x0, [x0, #-8]
	.loc	1	6	5
	ubfm	x0, x0, #56, #55
	.loc	1	6	5
	ubfm	x0, x0, #17, #63
	subs	xzr, x0, #3
	b.le	L387
	orr	x0, xzr, #1
	.loc	1	6	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L464:
	.loc	1	6	50
	ldr	x1, [x0, #-8]
	.loc	1	6	50
	ubfm	x1, x1, #56, #55
	.loc	1	6	50
	ubfm	x1, x1, #17, #63
	subs	xzr, x1, #3
	b.ls	L384
	.loc	1	6	50
	ldr	x0, [x0, #8]
	.loc	1	6	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L465:
	orr	x2, xzr, x0
	b	L390
L384:
	adrp	x0, L_camlArray_binary_search_string__block35@PAGE
	add	x0, x0, L_camlArray_binary_search_string__block35@PAGEOFF
	.loc	1	6	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L387:
	movz	x0, #3393, lsl #0
	movk	x0, #3, lsl #16
	orr	x2, xzr, x0
L390:
	adrp	x0, _camlArray_binary_search_string@GOTPAGE
	ldr	x0, [x0, _camlArray_binary_search_string@GOTPAGEOFF]
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
L466:
	.loc	1	9	5
	ldr	x0, [x0, #-8]
	.loc	1	9	5
	ubfm	x0, x0, #56, #55
	.loc	1	9	5
	ubfm	x0, x0, #17, #63
	ldr	x1, [sp, #0]
	subs	xzr, x0, #5
	b.le	L420
	str	x1, [sp, #0]
	orr	x0, xzr, #1
	.loc	1	9	50
	adrp	x8, _caml_sys_argv@GOTPAGE
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	bl	_caml_c_call
L467:
	.loc	1	9	50
	ldr	x1, [x0, #-8]
	.loc	1	9	50
	ubfm	x1, x1, #56, #55
	.loc	1	9	50
	ubfm	x1, x1, #17, #63
	ldr	x2, [sp, #0]
	subs	xzr, x1, #5
	b.ls	L417
	.loc	1	9	50
	str	x2, [sp, #0]
	.loc	1	9	50
	ldr	x0, [x0, #16]
	.loc	1	9	36
	adrp	x8, _caml_int_of_string@GOTPAGE
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
L468:
	orr	x19, xzr, x0
	ldr	x0, [sp, #0]
	orr	x2, xzr, x0
	b	L423
L417:
	adrp	x0, L_camlArray_binary_search_string__block35@PAGE
	add	x0, x0, L_camlArray_binary_search_string__block35@PAGEOFF
	.loc	1	9	50
	add	sp, x26, #0
	ldp	x26, x16, [sp], #16
	br	x16
L420:
	movz	x0, #21, lsl #0
	orr	x19, xzr, x0
	orr	x2, xzr, x1
L423:
	str	x2, [sp, #0]
	adrp	x0, _camlArray_binary_search_string@GOTPAGE
	ldr	x0, [x0, _camlArray_binary_search_string@GOTPAGEOFF]
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
	ldr	x16, [x28, #40]
	add	x16, x16, #392
	subs	xzr, sp, x16
	b.cc	L469
L470:
	orr	x0, xzr, x19
	.loc	1	38	45
	bl	_camlArray_binary_search_string__black_box_int_0_9_code
L471:
	str	x0, [sp, #8]
	ldr	x0, [sp, #0]
	.loc	1	38	27
	bl	_camlArray_binary_search_string__black_box_int_0_9_code
L472:
	ldr	x1, [sp, #8]
	.loc	1	38	22
	bl	_camlArray_binary_search_string__run_6_15_code
L473:
	str	x0, [sp, #0]
	adrp	x2, _camlArray_binary_search_string__const_block66@GOTPAGE
	ldr	x2, [x2, _camlArray_binary_search_string__const_block66@GOTPAGEOFF]
	orr	x1, xzr, #1
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
	.loc	2	27	2
	bl	_camlCamlinternalFormat__make_printf_120_401_code
L474:
	orr	x1, xzr, x0
	.loc	1	11	44
	ldr	x0, [sp, #0]
	.loc	1	11	44
	and	x0, x0, #2147483647
	.loc	1	11	21
	ldr	x2, [x1, #0]
	.loc	1	11	21
	blr	x2
L475:
	orr	x0, xzr, #1
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L469:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L470
	.cfi_endproc
	.data
	.align	3
	.text
	.globl	_camlArray_binary_search_string__code_end
_camlArray_binary_search_string__code_end:
	.data
	.8byte	0
	.globl	_camlArray_binary_search_string__data_end
_camlArray_binary_search_string__data_end:
	.8byte	0
	.align	3
	.globl	_camlArray_binary_search_string__frametable
_camlArray_binary_search_string__frametable:
	.8byte	21
	.4byte	(L475 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L476 - .) + 0
	.align	3
	.4byte	(L474 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L477 - .) + 0
	.align	3
	.4byte	(L473 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L478 - .) + 0
	.align	3
	.4byte	(L472 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L479 - .) + 0
	.align	3
	.4byte	(L471 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L480 - .) + 0
	.align	3
	.4byte	(L468 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L481 - .) + 0
	.align	3
	.4byte	(L467 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L482 - .) + 0
	.align	3
	.4byte	(L466 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L483 - .) + 0
	.align	3
	.4byte	(L465 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L484 - .) + 0
	.align	3
	.4byte	(L464 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L485 - .) + 0
	.align	3
	.4byte	(L463 - .) + 0
	.2byte	33
	.2byte	0
	.align	2
	.4byte	(L486 - .) + 0
	.align	3
	.4byte	(L354 - .) + 0
	.2byte	81
	.2byte	2
	.2byte	0
	.2byte	8
	.align	2
	.4byte	(L487 - .) + 0
	.align	3
	.4byte	(L353 - .) + 0
	.2byte	81
	.2byte	1
	.2byte	0
	.align	2
	.4byte	(L488 - .) + 0
	.align	3
	.4byte	(L350 - .) + 0
	.2byte	82
	.2byte	3
	.2byte	0
	.2byte	35
	.2byte	37
	.byte	1
	.byte	1
	.align	3
	.4byte	(L349 - .) + 0
	.2byte	81
	.2byte	2
	.2byte	0
	.2byte	8
	.align	2
	.4byte	(L489 - .) + 0
	.align	3
	.4byte	(L348 - .) + 0
	.2byte	81
	.2byte	1
	.2byte	0
	.align	2
	.4byte	(L490 - .) + 0
	.align	3
	.4byte	(L345 - .) + 0
	.2byte	81
	.2byte	0
	.align	2
	.4byte	(L491 - .) + 0
	.align	3
	.4byte	(L342 - .) + 0
	.2byte	82
	.2byte	1
	.2byte	33
	.byte	1
	.byte	1
	.align	3
	.4byte	(L341 - .) + 0
	.2byte	81
	.2byte	1
	.2byte	0
	.align	2
	.4byte	(L492 - .) + 0
	.align	3
	.4byte	(L340 - .) + 0
	.2byte	81
	.2byte	0
	.align	2
	.4byte	(L493 - .) + 0
	.align	3
	.4byte	(L122 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L494 - .) + 0
	.align	3
	.align	2
L477:
	.4byte	(L496 - .) + 1
	.4byte	14158328
	.4byte	(L497 - .) + 1
	.4byte	17847640
	.4byte	(L498 - .) + 1
	.4byte	19940632
	.4byte	(L500 - .) + 1
	.4byte	5789176
	.4byte	(L501 - .) + 0
	.4byte	19932688
	.align	2
L492:
	.4byte	(L503 - .) + 1
	.4byte	118499584
	.4byte	(L504 - .) + 1
	.4byte	14184968
	.4byte	(L506 - .) + 1
	.4byte	44587224
	.4byte	(L507 - .) + 0
	.4byte	14160424
	.align	2
L483:
	.4byte	(L508 - .) + 0
	.4byte	4737232
	.align	2
L494:
	.4byte	(L496 - .) + 1
	.4byte	14158328
	.4byte	(L497 - .) + 1
	.4byte	17847640
	.4byte	(L498 - .) + 1
	.4byte	19940632
	.4byte	(L500 - .) + 0
	.4byte	5789176
	.align	2
L481:
	.4byte	(L508 - .) + 0
	.4byte	4755952
	.align	2
L490:
	.4byte	(L509 - .) + 1
	.4byte	146802840
	.4byte	(L504 - .) + 1
	.4byte	14207496
	.4byte	(L506 - .) + 1
	.4byte	45635800
	.4byte	(L507 - .) + 0
	.4byte	14160424
	.align	2
L487:
	.4byte	(L507 - .) + 0
	.4byte	17322528
	.align	2
L488:
	.4byte	(L510 - .) + 1
	.4byte	81277248
	.4byte	(L507 - .) + 0
	.4byte	15217888
	.align	2
L486:
	.4byte	(L511 - .) + 0
	.4byte	3164368
	.align	2
L491:
	.4byte	(L506 - .) + 1
	.4byte	44578008
	.4byte	(L507 - .) + 0
	.4byte	14160424
	.align	2
L482:
	.4byte	(L508 - .) + 0
	.4byte	4770256
	.align	2
L485:
	.4byte	(L511 - .) + 0
	.4byte	3197392
	.align	2
L479:
	.4byte	(L501 - .) + 0
	.4byte	19950944
	.align	2
L478:
	.4byte	(L501 - .) + 0
	.4byte	19946000
	.align	2
L480:
	.4byte	(L501 - .) + 0
	.4byte	19969544
	.align	2
L489:
	.4byte	(L503 - .) + 1
	.4byte	118499584
	.4byte	(L504 - .) + 1
	.4byte	14184968
	.4byte	(L506 - .) + 1
	.4byte	45635800
	.4byte	(L507 - .) + 0
	.4byte	14160424
	.align	2
L493:
	.4byte	(L509 - .) + 1
	.4byte	146802840
	.4byte	(L504 - .) + 1
	.4byte	14207496
	.4byte	(L506 - .) + 1
	.4byte	44587224
	.4byte	(L507 - .) + 0
	.4byte	14160424
	.align	2
L484:
	.4byte	(L511 - .) + 0
	.4byte	3183088
	.align	2
L476:
	.4byte	(L500 - .) + 1
	.4byte	5789176
	.4byte	(L501 - .) + 0
	.4byte	19932688
L502:
	.ascii	"stdlib.ml\0"
L495:
	.ascii	"printf.ml\0"
L499:
	.ascii	"array_binary_search_string.ml\0"
L505:
	.ascii	"array.ml\0"
	.align	2
L510:
	.4byte	(L505 - .) + 0
	.ascii	"Stdlib__Array.map\0"
	.align	2
L503:
	.4byte	(L502 - .) + 0
	.ascii	"Stdlib.(^)\0"
	.align	2
L501:
	.4byte	(L499 - .) + 0
	.ascii	"Array_binary_search_string\0"
	.align	2
L506:
	.4byte	(L505 - .) + 0
	.ascii	"Stdlib__Array.init\0"
	.align	2
L508:
	.4byte	(L499 - .) + 0
	.ascii	"Array_binary_search_string.reps\0"
	.align	2
L504:
	.4byte	(L499 - .) + 0
	.ascii	"Array_binary_search_string.run.(fun)\0"
	.align	2
L498:
	.4byte	(L495 - .) + 0
	.ascii	"Stdlib__Printf.printf\0"
	.align	2
L497:
	.4byte	(L495 - .) + 0
	.ascii	"Stdlib__Printf.fprintf\0"
	.align	2
L509:
	.4byte	(L502 - .) + 0
	.ascii	"Stdlib.string_of_int\0"
	.align	2
L500:
	.4byte	(L499 - .) + 0
	.ascii	"Array_binary_search_string.print_result\0"
	.align	2
L496:
	.4byte	(L495 - .) + 0
	.ascii	"Stdlib__Printf.kfprintf\0"
	.align	2
L511:
	.4byte	(L499 - .) + 0
	.ascii	"Array_binary_search_string.n\0"
	.align	2
L507:
	.4byte	(L499 - .) + 0
	.ascii	"Array_binary_search_string.run\0"
	.align	3
