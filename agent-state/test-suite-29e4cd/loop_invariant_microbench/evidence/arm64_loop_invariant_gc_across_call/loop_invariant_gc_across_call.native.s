	.file	""
	.data
	.globl	_camlLoop_invariant_gc_across_call__data_begin
_camlLoop_invariant_gc_across_call__data_begin:
	.text
	.globl	_camlLoop_invariant_gc_across_call__code_begin
_camlLoop_invariant_gc_across_call__code_begin:
	nop
	.align	3
	.data
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__gc_roots
_camlLoop_invariant_gc_across_call__gc_roots:
L_camlLoop_invariant_gc_across_call__gc_roots:
	.8byte	0
	.data
	.align	3
	.8byte	4864
	.globl	_camlLoop_invariant_gc_across_call
_camlLoop_invariant_gc_across_call:
L_camlLoop_invariant_gc_across_call:
	.8byte	_camlLoop_invariant_gc_across_call__tick_4
	.8byte	_camlLoop_invariant_gc_across_call__print_result_5
	.8byte	_camlLoop_invariant_gc_across_call__loop_6
	.8byte	_camlLoop_invariant_gc_across_call__run_7
	.data
	.align	3
	.8byte	4087
	.globl	_camlLoop_invariant_gc_across_call__run_7
_camlLoop_invariant_gc_across_call__run_7:
L_camlLoop_invariant_gc_across_call__run_7:
	.8byte	_caml_curry2
	.8byte	0x280000000000007
	.8byte	_camlLoop_invariant_gc_across_call__run_3_7_code
	.data
	.align	3
	.8byte	4087
	.globl	_camlLoop_invariant_gc_across_call__loop_6
_camlLoop_invariant_gc_across_call__loop_6:
L_camlLoop_invariant_gc_across_call__loop_6:
	.8byte	_caml_curry3
	.8byte	0x380000000000007
	.8byte	_camlLoop_invariant_gc_across_call__loop_2_6_code
	.data
	.align	3
	.8byte	3063
	.globl	_camlLoop_invariant_gc_across_call__print_result_5
_camlLoop_invariant_gc_across_call__print_result_5:
L_camlLoop_invariant_gc_across_call__print_result_5:
	.8byte	_camlLoop_invariant_gc_across_call__print_result_1_5_code
	.8byte	0x180000000000005
	.data
	.align	3
	.8byte	3063
	.globl	_camlLoop_invariant_gc_across_call__tick_4
_camlLoop_invariant_gc_across_call__tick_4:
L_camlLoop_invariant_gc_across_call__tick_4:
	.8byte	_camlLoop_invariant_gc_across_call__tick_0_4_code
	.8byte	0x180000000000005
	.data
	.align	3
	.8byte	4092
	.globl	_camlLoop_invariant_gc_across_call__immstring48
_camlLoop_invariant_gc_across_call__immstring48:
L_camlLoop_invariant_gc_across_call__immstring48:
	.ascii	"loop_invariant_payload"
	.space	1
	.byte	1
	.text
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__tick_0_4_code
_camlLoop_invariant_gc_across_call__tick_0_4_code:
L_camlLoop_invariant_gc_across_call__tick_0_4_code:
	.file	1	"loop_invariant_gc_across_call.ml"
	.loc	1	1	24
	.cfi_startproc
	.loc	1	2	22
	add	x0, x0, #2
	.loc	1	2	2
	ret
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__print_result_1_5_code
_camlLoop_invariant_gc_across_call__print_result_1_5_code:
L_camlLoop_invariant_gc_across_call__print_result_1_5_code:
	.loc	1	4	17
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_adjust_cfa_offset 16
	.cfi_offset 30, -8
	str	lr, [sp, #8]
	orr	x1, xzr, x0
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	.file	2	"stdlib.ml"
	.loc	2	280	2
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L134:
	orr	x1, xzr, x0
	.loc	2	387	30
	ldr	x0, [x1, #-8]
	.loc	2	387	30
	ubfm	x0, x0, #56, #55
	.loc	2	387	30
	ubfm	x0, x0, #18, #63
	.loc	2	387	30
	ubfm	x0, x0, #61, #60
	.loc	2	387	30
	sub	x0, x0, #1
	.loc	2	387	30
	add	x2, x1, x0
	.loc	2	387	30
	ldrb	w2, [x2, #0]
	.loc	2	387	30
	sub	x0, x0, x2
	orr	x2, xzr, #1
	.loc	2	387	30
	add	x3, x2, x0, lsl #1
	orr	x2, xzr, #1
	adrp	x0, _camlStdlib__print_int_136@GOTPAGE
	ldr	x0, [x0, _camlStdlib__print_int_136@GOTPAGEOFF]
	.loc	1	5	2
	add	x0, x0, #16
	.loc	1	5	2
	ldr	x0, [x0, #0]
	.loc	2	387	2
	adrp	x8, _caml_ml_output@GOTPAGE
	ldr	x8, [x8, _caml_ml_output@GOTPAGEOFF]
	bl	_caml_c_call
L135:
	movz	x1, #21, lsl #0
	adrp	x0, _camlStdlib__print_newline_139@GOTPAGE
	ldr	x0, [x0, _camlStdlib__print_newline_139@GOTPAGEOFF]
	.loc	1	6	2
	add	x0, x0, #16
	.loc	1	6	2
	ldr	x0, [x0, #0]
	.loc	2	511	23
	adrp	x8, _caml_ml_output_char@GOTPAGE
	ldr	x8, [x8, _caml_ml_output_char@GOTPAGEOFF]
	bl	_caml_c_call
L136:
	adrp	x0, _camlStdlib__print_newline_139@GOTPAGE
	ldr	x0, [x0, _camlStdlib__print_newline_139@GOTPAGEOFF]
	.loc	1	6	2
	add	x0, x0, #16
	.loc	1	6	2
	ldr	x0, [x0, #0]
	.loc	2	511	48
	adrp	x8, _caml_ml_flush@GOTPAGE
	ldr	x8, [x8, _caml_ml_flush@GOTPAGEOFF]
	bl	_caml_c_call
L137:
	ldr	lr, [sp, #8]
	add	sp, sp, #16
	.cfi_adjust_cfa_offset -16
	ret
	.cfi_adjust_cfa_offset 16
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__loop_2_6_code
_camlLoop_invariant_gc_across_call__loop_2_6_code:
L_camlLoop_invariant_gc_across_call__loop_2_6_code:
	.loc	1	9	13
	.cfi_startproc
	sub	sp, sp, #32
	.cfi_adjust_cfa_offset 32
	.cfi_offset 30, -8
	str	lr, [sp, #24]
	ldr	x16, [x28, #40]
	add	x16, x16, #392
	subs	xzr, sp, x16
	b.cc	L169
L170:
	str	x0, [sp, #0]
	orr	x0, xzr, x1
	subs	xzr, x0, #1
	b.gt	L148
L146:
	orr	x0, xzr, x2
	ldr	lr, [sp, #24]
	add	sp, sp, #32
	.cfi_adjust_cfa_offset -32
	ret
	.cfi_adjust_cfa_offset 32
L148:
	str	x2, [sp, #16]
	str	x0, [sp, #8]
	.loc	1	12	12
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code
L171:
	.loc	1	13	26
	ldr	x2, [sp, #0]
	.loc	1	13	26
	ldr	x1, [x2, #-8]
	.loc	1	13	26
	ubfm	x1, x1, #56, #55
	.loc	1	13	26
	ubfm	x1, x1, #18, #63
	.loc	1	13	26
	ubfm	x1, x1, #61, #60
	.loc	1	13	26
	sub	x1, x1, #1
	.loc	1	13	26
	add	x2, x2, x1
	.loc	1	13	26
	ldrb	w2, [x2, #0]
	.loc	1	13	26
	sub	x1, x1, x2
	.loc	1	13	20
	ldr	x2, [sp, #16]
	.loc	1	13	20
	add	x1, x2, x1, lsl #1
	.loc	1	13	19
	add	x0, x1, x0
	.loc	1	13	19
	sub	x2, x0, #1
	.loc	1	13	11
	ldr	x0, [sp, #8]
	.loc	1	13	11
	sub	x0, x0, #2
	subs	xzr, x0, #1
	b.gt	L148
	b	L146
L169:
	movz	x16, #32
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L170
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__run_3_7_code
_camlLoop_invariant_gc_across_call__run_3_7_code:
L_camlLoop_invariant_gc_across_call__run_3_7_code:
	.loc	1	15	8
	.cfi_startproc
	sub	sp, sp, #64
	.cfi_adjust_cfa_offset 64
	.cfi_offset 30, -8
	str	lr, [sp, #56]
	orr	x2, xzr, x0
	adrp	x0, _camlLoop_invariant_gc_across_call__immstring48@GOTPAGE
	ldr	x0, [x0, _camlLoop_invariant_gc_across_call__immstring48@GOTPAGEOFF]
	.loc	1	16	10
	subs	xzr, x1, #3
	b.lt	L222
	ldr	x16, [x28, #40]
	add	x16, x16, #424
	subs	xzr, sp, x16
	b.cc	L229
L230:
	.loc	1	18	2
	str	x0, [sp, #24]
	.loc	1	18	2
	str	x2, [sp, #16]
	.loc	1	18	2
	sbfm	x1, x1, #1, #63
	str	x1, [sp, #32]
	orr	x0, xzr, #1
	orr	x3, xzr, #1
L187:
	str	x0, [sp, #8]
	str	x3, [sp, #0]
	orr	x3, xzr, #1
	orr	x0, xzr, x2
	subs	xzr, x0, #1
	b.gt	L193
L191:
	.loc	1	19	11
	ldr	x0, [sp, #8]
	.loc	1	19	11
	add	x0, x0, x3
	.loc	1	19	11
	sub	x0, x0, #1
	.loc	1	18	2
	ldr	x3, [sp, #0]
	.loc	1	18	2
	add	x3, x3, #1
	subs	xzr, x3, x1
	b.gt	L217
	b	L187
L193:
	str	x3, [sp, #48]
	str	x0, [sp, #40]
	.loc	1	12	12
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code
L231:
	.loc	1	13	26
	ldr	x2, [sp, #24]
	.loc	1	13	26
	ldr	x1, [x2, #-8]
	.loc	1	13	26
	ubfm	x1, x1, #56, #55
	.loc	1	13	26
	ubfm	x1, x1, #18, #63
	.loc	1	13	26
	ubfm	x1, x1, #61, #60
	.loc	1	13	26
	sub	x1, x1, #1
	.loc	1	13	26
	add	x2, x2, x1
	.loc	1	13	26
	ldrb	w2, [x2, #0]
	.loc	1	13	26
	sub	x1, x1, x2
	.loc	1	13	20
	ldr	x3, [sp, #48]
	.loc	1	13	20
	add	x1, x3, x1, lsl #1
	.loc	1	13	19
	add	x0, x1, x0
	.loc	1	13	19
	sub	x3, x0, #1
	.loc	1	13	11
	ldr	x0, [sp, #40]
	.loc	1	13	11
	sub	x0, x0, #2
	ldr	x2, [sp, #16]
	ldr	x1, [sp, #32]
	subs	xzr, x0, #1
	b.gt	L193
	b	L191
L217:
	ldr	lr, [sp, #56]
	add	sp, sp, #64
	.cfi_adjust_cfa_offset -64
	ret
	.cfi_adjust_cfa_offset 64
L222:
	orr	x0, xzr, #1
	ldr	lr, [sp, #56]
	add	sp, sp, #64
	.cfi_adjust_cfa_offset -64
	ret
	.cfi_adjust_cfa_offset 64
L229:
	movz	x16, #64
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L230
	.cfi_endproc
	.text
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__entry
_camlLoop_invariant_gc_across_call__entry:
L_camlLoop_invariant_gc_across_call__entry:
	.cfi_startproc
	sub	sp, sp, #48
	.cfi_adjust_cfa_offset 48
	.cfi_offset 30, -8
	str	lr, [sp, #40]
	ldr	x16, [x28, #40]
	add	x16, x16, #408
	subs	xzr, sp, x16
	b.cc	L322
L323:
	adrp	x0, _camlLoop_invariant_gc_across_call__immstring48@GOTPAGE
	ldr	x0, [x0, _camlLoop_invariant_gc_across_call__immstring48@GOTPAGEOFF]
	.loc	1	16	10
	str	x0, [sp, #16]
	orr	x0, xzr, #1
	orr	x1, xzr, #1
L250:
	str	x0, [sp, #8]
	str	x1, [sp, #0]
	orr	x0, xzr, #1
	movz	x1, #13825, lsl #0
	movk	x1, #366, lsl #16
	b	L256
L254:
	.loc	1	19	11
	ldr	x2, [sp, #8]
	.loc	1	19	11
	add	x0, x2, x0
	.loc	1	19	11
	sub	x0, x0, #1
	.loc	1	18	2
	ldr	x1, [sp, #0]
	.loc	1	18	2
	add	x1, x1, #1
	subs	xzr, x1, #5
	b.gt	L280
	b	L278
L256:
	str	x0, [sp, #32]
	orr	x0, xzr, x1
	str	x1, [sp, #24]
	.loc	1	12	12
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code
L324:
	.loc	1	13	26
	ldr	x1, [sp, #16]
	.loc	1	13	26
	ldr	x2, [x1, #-8]
	.loc	1	13	26
	ubfm	x2, x2, #56, #55
	.loc	1	13	26
	ubfm	x2, x2, #18, #63
	.loc	1	13	26
	ubfm	x2, x2, #61, #60
	.loc	1	13	26
	sub	x2, x2, #1
	.loc	1	13	26
	add	x1, x1, x2
	.loc	1	13	26
	ldrb	w1, [x1, #0]
	.loc	1	13	26
	sub	x1, x2, x1
	.loc	1	13	20
	ldr	x2, [sp, #32]
	.loc	1	13	20
	add	x1, x2, x1, lsl #1
	.loc	1	13	19
	add	x0, x1, x0
	.loc	1	13	19
	sub	x0, x0, #1
	.loc	1	13	11
	ldr	x1, [sp, #24]
	.loc	1	13	11
	sub	x1, x1, #2
	orr	x2, xzr, x0
	subs	xzr, x1, #1
	b.gt	L256
	b	L254
L278:
	orr	x2, xzr, x0
	b	L250
L280:
	orr	x1, xzr, x0
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	.loc	2	280	2
	adrp	x8, _caml_format_int@GOTPAGE
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
	bl	_caml_c_call
L325:
	orr	x1, xzr, x0
	.loc	2	387	30
	ldr	x0, [x1, #-8]
	.loc	2	387	30
	ubfm	x0, x0, #56, #55
	.loc	2	387	30
	ubfm	x0, x0, #18, #63
	.loc	2	387	30
	ubfm	x0, x0, #61, #60
	.loc	2	387	30
	sub	x0, x0, #1
	.loc	2	387	30
	add	x2, x1, x0
	.loc	2	387	30
	ldrb	w2, [x2, #0]
	.loc	2	387	30
	sub	x0, x0, x2
	orr	x2, xzr, #1
	.loc	2	387	30
	add	x3, x2, x0, lsl #1
	orr	x2, xzr, #1
	adrp	x0, _camlStdlib__print_int_136@GOTPAGE
	ldr	x0, [x0, _camlStdlib__print_int_136@GOTPAGEOFF]
	.loc	1	5	2
	add	x0, x0, #16
	.loc	1	5	2
	ldr	x0, [x0, #0]
	.loc	2	387	2
	adrp	x8, _caml_ml_output@GOTPAGE
	ldr	x8, [x8, _caml_ml_output@GOTPAGEOFF]
	bl	_caml_c_call
L326:
	movz	x1, #21, lsl #0
	adrp	x0, _camlStdlib__print_int_136@GOTPAGE
	ldr	x0, [x0, _camlStdlib__print_int_136@GOTPAGEOFF]
	.loc	1	5	2
	add	x0, x0, #16
	.loc	1	5	2
	ldr	x0, [x0, #0]
	.loc	2	511	23
	adrp	x8, _caml_ml_output_char@GOTPAGE
	ldr	x8, [x8, _caml_ml_output_char@GOTPAGEOFF]
	bl	_caml_c_call
L327:
	adrp	x0, _camlStdlib__print_int_136@GOTPAGE
	ldr	x0, [x0, _camlStdlib__print_int_136@GOTPAGEOFF]
	.loc	1	5	2
	add	x0, x0, #16
	.loc	1	5	2
	ldr	x0, [x0, #0]
	.loc	2	511	48
	adrp	x8, _caml_ml_flush@GOTPAGE
	ldr	x8, [x8, _caml_ml_flush@GOTPAGEOFF]
	bl	_caml_c_call
L328:
	orr	x0, xzr, #1
	ldr	lr, [sp, #40]
	add	sp, sp, #48
	.cfi_adjust_cfa_offset -48
	ret
	.cfi_adjust_cfa_offset 48
L322:
	movz	x16, #48
	stp	x16, lr, [sp, #-16]!
	bl	_caml_call_realloc_stack
	ldp	x16, lr, [sp], #16
	b	L323
	.cfi_endproc
	.data
	.align	3
	.text
	.globl	_camlLoop_invariant_gc_across_call__code_end
_camlLoop_invariant_gc_across_call__code_end:
	.data
	.8byte	0
	.globl	_camlLoop_invariant_gc_across_call__data_end
_camlLoop_invariant_gc_across_call__data_end:
	.8byte	0
	.align	3
	.globl	_camlLoop_invariant_gc_across_call__frametable
_camlLoop_invariant_gc_across_call__frametable:
	.8byte	11
	.4byte	(L328 - .) + 0
	.2byte	49
	.2byte	0
	.align	2
	.4byte	(L329 - .) + 0
	.align	3
	.4byte	(L327 - .) + 0
	.2byte	49
	.2byte	0
	.align	2
	.4byte	(L330 - .) + 0
	.align	3
	.4byte	(L326 - .) + 0
	.2byte	49
	.2byte	0
	.align	2
	.4byte	(L331 - .) + 0
	.align	3
	.4byte	(L325 - .) + 0
	.2byte	49
	.2byte	0
	.align	2
	.4byte	(L332 - .) + 0
	.align	3
	.4byte	(L324 - .) + 0
	.2byte	49
	.2byte	1
	.2byte	16
	.align	2
	.4byte	(L333 - .) + 0
	.align	3
	.4byte	(L231 - .) + 0
	.2byte	65
	.2byte	1
	.2byte	24
	.align	2
	.4byte	(L334 - .) + 0
	.align	3
	.4byte	(L171 - .) + 0
	.2byte	33
	.2byte	1
	.2byte	0
	.align	2
	.4byte	(L335 - .) + 0
	.align	3
	.4byte	(L137 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L336 - .) + 0
	.align	3
	.4byte	(L136 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L337 - .) + 0
	.align	3
	.4byte	(L135 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L338 - .) + 0
	.align	3
	.4byte	(L134 - .) + 0
	.2byte	17
	.2byte	0
	.align	2
	.4byte	(L339 - .) + 0
	.align	3
	.align	2
L338:
	.4byte	(L341 - .) + 1
	.4byte	202901880
	.4byte	(L342 - .) + 1
	.4byte	265832896
	.4byte	(L344 - .) + 0
	.4byte	2623592
	.align	2
L335:
	.4byte	(L345 - .) + 0
	.4byte	6303888
	.align	2
L332:
	.4byte	(L346 - .) + 1
	.4byte	146802840
	.4byte	(L342 - .) + 1
	.4byte	265854400
	.4byte	(L344 - .) + 1
	.4byte	2623592
	.4byte	(L347 - .) + 0
	.4byte	12068144
	.align	2
L334:
	.4byte	(L345 - .) + 1
	.4byte	6303888
	.4byte	(L348 - .) + 0
	.4byte	9980128
	.align	2
L331:
	.4byte	(L341 - .) + 1
	.4byte	202901880
	.4byte	(L342 - .) + 1
	.4byte	265832896
	.4byte	(L344 - .) + 1
	.4byte	2623592
	.4byte	(L347 - .) + 0
	.4byte	12068144
	.align	2
L339:
	.4byte	(L346 - .) + 1
	.4byte	146802840
	.4byte	(L342 - .) + 1
	.4byte	265854400
	.4byte	(L344 - .) + 0
	.4byte	2623592
	.align	2
L329:
	.4byte	(L349 - .) + 1
	.4byte	267960800
	.4byte	(L344 - .) + 1
	.4byte	3147920
	.4byte	(L347 - .) + 0
	.4byte	12068144
	.align	2
L330:
	.4byte	(L349 - .) + 1
	.4byte	267935088
	.4byte	(L344 - .) + 1
	.4byte	3147920
	.4byte	(L347 - .) + 0
	.4byte	12068144
	.align	2
L333:
	.4byte	(L345 - .) + 1
	.4byte	6303888
	.4byte	(L348 - .) + 1
	.4byte	9980128
	.4byte	(L347 - .) + 0
	.4byte	12081456
	.align	2
L337:
	.4byte	(L349 - .) + 1
	.4byte	267935088
	.4byte	(L344 - .) + 0
	.4byte	3147920
	.align	2
L336:
	.4byte	(L349 - .) + 1
	.4byte	267960800
	.4byte	(L344 - .) + 0
	.4byte	3147920
L340:
	.ascii	"stdlib.ml\0"
L343:
	.ascii	"loop_invariant_gc_across_call.ml\0"
	.align	2
L342:
	.4byte	(L340 - .) + 0
	.ascii	"Stdlib.print_int\0"
	.align	2
L348:
	.4byte	(L343 - .) + 0
	.ascii	"Loop_invariant_gc_across_call.run\0"
	.align	2
L344:
	.4byte	(L343 - .) + 0
	.ascii	"Loop_invariant_gc_across_call.print_result\0"
	.align	2
L347:
	.4byte	(L343 - .) + 0
	.ascii	"Loop_invariant_gc_across_call\0"
	.align	2
L349:
	.4byte	(L340 - .) + 0
	.ascii	"Stdlib.print_newline\0"
	.align	2
L345:
	.4byte	(L343 - .) + 0
	.ascii	"Loop_invariant_gc_across_call.loop\0"
	.align	2
L346:
	.4byte	(L340 - .) + 0
	.ascii	"Stdlib.string_of_int\0"
	.align	2
L341:
	.4byte	(L340 - .) + 0
	.ascii	"Stdlib.output_string\0"
	.align	3
