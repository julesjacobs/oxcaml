_camlTry_raise_cross_function_caught__probe_4_11_code: ; @"\01_camlTry_raise_cross_function_caught__probe_4_11_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	cmp	x0, #1
	b.eq	LBB4_2
; %bb.1:                                ; %L123
	ret
LBB4_2:                                 ; %L121
Lloh4:
	adrp	x8, _camlTry_raise_cross_function_caught__Miss245@PAGE
Lloh5:
	add	x8, x8, _camlTry_raise_cross_function_caught__Miss245@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpAdd	Lloh4, Lloh5
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_raise_cross_function_caught__find_5_12_code ; -- Begin function _camlTry_raise_cross_function_caught__find_5_12_code
	.p2align	2
