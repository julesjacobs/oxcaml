_camlString_map_equal_content__find_opt_16_34_code: ; @"\01_camlString_map_equal_content__find_opt_16_34_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	mov	x19, x1
	tbnz	w19, #0, LBB2_16
; %bb.1:                                ; %L226.preheader
	mov	x21, x0
	ldr	x1, [x19, #8]
	cmp	x1, x0
	b.eq	LBB2_17
; %bb.2:
	mov	w20, #1
	mov	w22, #3
	mov	w23, #8
	mov	x24, #-1
	b	LBB2_5
LBB2_3:                                 ; %L238
                                        ;   in Loop: Header=BB2_5 Depth=1
	ldr	x19, [x19]
	tbnz	w19, #0, LBB2_15
LBB2_4:                                 ; %L226.backedge
                                        ;   in Loop: Header=BB2_5 Depth=1
	ldr	x1, [x19, #8]
	cmp	x1, x21
	b.eq	LBB2_17
LBB2_5:                                 ; %L251
                                        ; =>This Inner Loop Header: Depth=1
	ldur	x8, [x21, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x21, x8]
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
	b.lo	LBB2_7
; %bb.6:                                ; %L252
                                        ;   in Loop: Header=BB2_5 Depth=1
	mov	x0, x21
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_string_compare
	mov	sp, x29
	.cfi_restore_state
	cmp	x0, #1
	b.ne	LBB2_13
	b	LBB2_17
LBB2_7:                                 ; %L253
                                        ;   in Loop: Header=BB2_5 Depth=1
	cbz	x10, LBB2_11
; %bb.8:                                ; %L255
                                        ;   in Loop: Header=BB2_5 Depth=1
	subs	x11, x23, x10
	csel	x11, xzr, x11, lo
	lsl	x11, x11, #3
	lsl	x11, x24, x11
	ldr	x12, [x21]
	rev	x12, x12
	ldr	x13, [x1]
	rev	x13, x13
	and	x12, x12, x11
	and	x11, x13, x11
	cmp	x12, x11
	b.ne	LBB2_12
; %bb.9:                                ; %L257
                                        ;   in Loop: Header=BB2_5 Depth=1
	cmp	x10, #9
	b.lo	LBB2_11
; %bb.10:                               ; %L258
                                        ;   in Loop: Header=BB2_5 Depth=1
	lsl	x10, x10, #3
	neg	x10, x10
	lsl	x10, x24, x10
	ldr	x11, [x21, #8]
	rev	x11, x11
	ldr	x12, [x1, #8]
	rev	x12, x12
	and	x11, x11, x10
	and	x10, x12, x10
	cmp	x11, x10
	b.ne	LBB2_12
LBB2_11:                                ; %L254
                                        ;   in Loop: Header=BB2_5 Depth=1
	cmp	x8, x9
	csinc	x8, x22, xzr, hi
	csinv	x0, x8, xzr, hs
	cmp	x0, #1
	b.ne	LBB2_13
	b	LBB2_17
LBB2_12:                                ; %L259
                                        ;   in Loop: Header=BB2_5 Depth=1
	csinv	x0, x22, xzr, hs
	cmp	x0, #1
	b.eq	LBB2_17
LBB2_13:                                ; %L236
                                        ;   in Loop: Header=BB2_5 Depth=1
	cmp	x0, #0
	b.le	LBB2_3
; %bb.14:                               ; %L241
                                        ;   in Loop: Header=BB2_5 Depth=1
	ldr	x19, [x19, #24]
	tbz	w19, #0, LBB2_4
LBB2_15:                                ; %common.ret
	mov	x0, x20
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB2_16:
	mov	w20, #1
	mov	x0, x20
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB2_17:                                ; %L231
	sub	x27, x27, #16
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB2_19
LBB2_18:                                ; %L262
	mov	w8, #1024
	str	x8, [x27]
	ldr	x8, [x19, #16]
	mov	x20, x27
	str	x8, [x20, #8]!
	mov	x0, x20
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB2_19:                                ; %L261
	bl	_caml_call_gc
Ltmp5:
	b	LBB2_18
	.cfi_endproc
                                        ; -- End function
	.globl	_camlString_map_equal_content__mem_17_35_code ; -- Begin function _camlString_map_equal_content__mem_17_35_code
	.p2align	2
