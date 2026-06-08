_camlTry_find_miss_rare__scan_4_11_code: ; @"\01_camlTry_find_miss_rare__scan_4_11_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
	mov	x9, x1
	ldur	x8, [x1, #-8]
	lsr	x10, x8, #9
	and	x10, x10, #0x7ffffffffffe
	orr	x10, x10, #0x1
	cmp	x10, x2
	b.eq	LBB4_10
; %bb.1:
	mov	x19, x2
	mov	x1, x0
	mov	w21, #1277
Lloh4:
	adrp	x20, _caml_equal@GOTPAGE
Lloh5:
	ldr	x20, [x20, _caml_equal@GOTPAGEOFF]
LBB4_2:                                 ; %L136
                                        ; =>This Inner Loop Header: Depth=1
	and	x8, x8, #0xff
	cmp	x8, #254
	b.ne	LBB4_5
; %bb.3:                                ; %L140
                                        ;   in Loop: Header=BB4_2 Depth=1
	sub	x27, x27, #16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB4_8
LBB4_4:                                 ; %L169
                                        ;   in Loop: Header=BB4_2 Depth=1
	str	x21, [x27]
	add	x8, x9, x19, lsl #2
	ldur	d0, [x8, #-4]
	mov	x0, x27
	str	d0, [x0, #8]!
	b	LBB4_6
LBB4_5:                                 ; %L147
                                        ;   in Loop: Header=BB4_2 Depth=1
	add	x8, x9, x19, lsl #2
	ldur	x0, [x8, #-4]
LBB4_6:                                 ; %L154
                                        ;   in Loop: Header=BB4_2 Depth=1
	str	x9, [sp, #16]
	str	x1, [sp, #24]
	str	x0, [sp, #8]
	mov	x8, x20
	bl	_caml_c_call
Ltmp3:
	cmp	x0, #1
	b.ne	LBB4_9
; %bb.7:                                ; %L157
                                        ;   in Loop: Header=BB4_2 Depth=1
	ldr	x9, [sp, #16]
	ldr	x1, [sp, #24]
	add	x19, x19, #2
	ldur	x8, [x9, #-8]
	lsr	x10, x8, #9
	and	x10, x10, #0x7ffffffffffe
	orr	x10, x10, #0x1
	cmp	x19, x10
	b.ne	LBB4_2
	b	LBB4_10
LBB4_8:                                 ; %L168
                                        ;   in Loop: Header=BB4_2 Depth=1
	bl	_caml_call_gc
Ltmp4:
	b	LBB4_4
LBB4_9:                                 ; %L160
	mov	x0, x19
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB4_10:                                ; %L131
Lloh6:
	adrp	x8, _camlTry_find_miss_rare__Miss281@PAGE
Lloh7:
	add	x8, x8, _camlTry_find_miss_rare__Miss281@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpLdrGot	Lloh4, Lloh5
	.loh AdrpAdd	Lloh6, Lloh7
	.cfi_endproc
                                        ; -- End function
	.globl	_camlTry_find_miss_rare__run_5_12_code ; -- Begin function _camlTry_find_miss_rare__run_5_12_code
	.p2align	2
