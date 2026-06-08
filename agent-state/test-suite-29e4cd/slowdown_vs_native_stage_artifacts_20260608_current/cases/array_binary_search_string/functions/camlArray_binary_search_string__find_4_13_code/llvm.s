_camlArray_binary_search_string__find_4_13_code: ; @"\01_camlArray_binary_search_string__find_4_13_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	mov	x19, x0
	ldur	x8, [x1, #-8]
	lsr	x8, x8, #9
	and	x8, x8, #0x7ffffffffffe
	cmp	x8, #2
	b.ls	LBB4_2
; %bb.1:
	sub	x20, x8, #1
	b	LBB4_4
LBB4_2:                                 ; %L162
	cmp	x8, #2
	b.ne	LBB4_20
; %bb.3:
	mov	w20, #1
LBB4_4:                                 ; %L134.preheader
	sub	x21, x1, #4
	mov	w22, #1
	sub	x24, x20, #1
LBB4_5:                                 ; %L134
                                        ; =>This Inner Loop Header: Depth=1
	add	x8, x24, x22
	lsr	x8, x8, #1
	orr	x23, x8, #0x1
	lsl	x8, x23, #2
	ldr	x25, [x21, x8]
	ldr	x1, [x25]
	cmp	x1, x19
	b.eq	LBB4_18
; %bb.6:                                ; %L165
                                        ;   in Loop: Header=BB4_5 Depth=1
	ldur	x8, [x19, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x19, x8]
	sub	x8, x8, x9
	ldur	x9, [x1, #-8]
	lsr	x9, x9, #7
	and	x9, x9, #0x1fffffffffff8
	sub	x9, x9, #1
	ldrb	w10, [x1, x9]
	sub	x9, x9, x10
	cmp	x8, x9
	csel	x10, x8, x9, lo
	cmp	x10, #16
	b.lo	LBB4_8
; %bb.7:                                ; %L166
                                        ;   in Loop: Header=BB4_5 Depth=1
	mov	x0, x19
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_string_compare
	mov	sp, x29
	.cfi_restore_state
	cmp	x0, #1
	b.ne	LBB4_14
	b	LBB4_18
LBB4_8:                                 ; %L167
                                        ;   in Loop: Header=BB4_5 Depth=1
	cbz	x10, LBB4_12
; %bb.9:                                ; %L169
                                        ;   in Loop: Header=BB4_5 Depth=1
	mov	w11, #8
	subs	x11, x11, x10
	csel	x11, xzr, x11, lo
	lsl	x11, x11, #3
	mov	x12, #-1
	lsl	x11, x12, x11
	ldr	x12, [x19]
	rev	x12, x12
	ldr	x13, [x1]
	rev	x13, x13
	and	x12, x12, x11
	and	x11, x13, x11
	cmp	x12, x11
	b.ne	LBB4_13
; %bb.10:                               ; %L171
                                        ;   in Loop: Header=BB4_5 Depth=1
	cmp	x10, #9
	b.lo	LBB4_12
; %bb.11:                               ; %L172
                                        ;   in Loop: Header=BB4_5 Depth=1
	lsl	x10, x10, #3
	neg	x10, x10
	mov	x11, #-1
	lsl	x10, x11, x10
	ldr	x11, [x19, #8]
	rev	x11, x11
	ldr	x12, [x1, #8]
	rev	x12, x12
	and	x11, x11, x10
	and	x10, x12, x10
	cmp	x11, x10
	b.ne	LBB4_13
LBB4_12:                                ; %L168
                                        ;   in Loop: Header=BB4_5 Depth=1
	cmp	x8, x9
	mov	w8, #3
	csinc	x8, x8, xzr, hi
	csinv	x0, x8, xzr, hs
	cmp	x0, #1
	b.ne	LBB4_14
	b	LBB4_18
LBB4_13:                                ; %L170
                                        ;   in Loop: Header=BB4_5 Depth=1
	mov	w8, #3
	csinv	x0, x8, xzr, hs
	cmp	x0, #1
	b.eq	LBB4_18
LBB4_14:                                ; %L150
                                        ;   in Loop: Header=BB4_5 Depth=1
	cmp	x0, #0
	b.le	LBB4_16
; %bb.15:                               ; %L155
                                        ;   in Loop: Header=BB4_5 Depth=1
	add	x22, x23, #2
	cmp	x22, x20
	b.le	LBB4_5
	b	LBB4_20
LBB4_16:                                ; %L152
                                        ;   in Loop: Header=BB4_5 Depth=1
	sub	x20, x23, #2
	mov	x0, #-1
	cmp	x22, x20
	b.gt	LBB4_19
; %bb.17:                               ; %L134.outer
                                        ;   in Loop: Header=BB4_5 Depth=1
	sub	x24, x20, #1
	b	LBB4_5
LBB4_18:                                ; %L146
	ldr	x0, [x25, #8]
LBB4_19:                                ; %common.ret
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB4_20:
	mov	x0, #-1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlArray_binary_search_string__run_6_15_code ; -- Begin function _camlArray_binary_search_string__run_6_15_code
	.p2align	2
