_camlBoyer__rewrite_12_32_code:         ; @"\01_camlBoyer__rewrite_12_32_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	mov	x9, x0
	ldurb	w8, [x0, #-8]
	cbz	w8, LBB11_4
; %bb.1:                                ; %L615
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x10, sp
	; InlineAsm End
	cmp	x10, x8
	b.lo	LBB11_5
LBB11_2:                                ; %L629
	ldp	x8, x1, [x9]
	ldr	x9, [x8, #8]
	str	x9, [sp]
	str	x8, [sp, #8]
Lloh53:
	adrp	x0, _camlBoyer__rewrite_28@PAGE
Lloh54:
	add	x0, x0, _camlBoyer__rewrite_28@PAGEOFF
	bl	_camlStdlib__List__map_15_113_code
Ltmp28:
	ldr	x1, [sp]
	ldr	x9, [sp, #8]
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB11_6
LBB11_3:                                ; %L631
	mov	w8, #2049
	str	x8, [x27]
	mov	x8, x27
	str	x9, [x8, #8]!
	str	x0, [x27, #16]
	mov	x0, x8
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	b	_camlBoyer__rewrite_with_lemmas_13_33_code
LBB11_4:                                ; %L625
	mov	x0, x9
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	ret
LBB11_5:                                ; %L628
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp29:
	b	LBB11_2
LBB11_6:                                ; %L630
	bl	_caml_call_gc
Ltmp30:
	b	LBB11_3
	.loh AdrpAdd	Lloh53, Lloh54
	.cfi_endproc
                                        ; -- End function
	.globl	_camlBoyer__rewrite_with_lemmas_13_33_code ; -- Begin function _camlBoyer__rewrite_with_lemmas_13_33_code
	.p2align	2
