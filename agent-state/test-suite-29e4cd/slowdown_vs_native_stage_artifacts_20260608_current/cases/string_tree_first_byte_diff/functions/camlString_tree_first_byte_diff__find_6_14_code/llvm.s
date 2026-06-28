_camlString_tree_first_byte_diff__find_6_14_code: ; @"\01_camlString_tree_first_byte_diff__find_6_14_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	mov	x19, x1
	tbnz	w19, #0, LBB6_15
; %bb.1:                                ; %L213.preheader
	mov	x20, x0
	ldr	x1, [x19, #8]
	cmp	x1, x0
	b.eq	LBB6_16
; %bb.2:
	mov	w21, #3
	mov	w22, #8
	mov	x23, #-1
	b	LBB6_5
LBB6_3:                                 ; %L227
                                        ;   in Loop: Header=BB6_5 Depth=1
	ldr	x19, [x19, #24]
	tbnz	w19, #0, LBB6_15
LBB6_4:                                 ; %L213.backedge
                                        ;   in Loop: Header=BB6_5 Depth=1
	ldr	x1, [x19, #8]
	cmp	x1, x20
	b.eq	LBB6_16
LBB6_5:                                 ; %L237
                                        ; =>This Inner Loop Header: Depth=1
	ldur	x8, [x20, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x20, x8]
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
	b.lo	LBB6_7
; %bb.6:                                ; %L238
                                        ;   in Loop: Header=BB6_5 Depth=1
	mov	x0, x20
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_string_compare
	mov	sp, x29
	.cfi_restore_state
	cmp	x0, #1
	b.ne	LBB6_13
	b	LBB6_16
LBB6_7:                                 ; %L239
                                        ;   in Loop: Header=BB6_5 Depth=1
	cbz	x10, LBB6_11
; %bb.8:                                ; %L241
                                        ;   in Loop: Header=BB6_5 Depth=1
	subs	x11, x22, x10
	csel	x11, xzr, x11, lo
	lsl	x11, x11, #3
	lsl	x11, x23, x11
	ldr	x12, [x20]
	rev	x12, x12
	ldr	x13, [x1]
	rev	x13, x13
	and	x12, x12, x11
	and	x11, x13, x11
	cmp	x12, x11
	b.ne	LBB6_12
; %bb.9:                                ; %L243
                                        ;   in Loop: Header=BB6_5 Depth=1
	cmp	x10, #9
	b.lo	LBB6_11
; %bb.10:                               ; %L244
                                        ;   in Loop: Header=BB6_5 Depth=1
	lsl	x10, x10, #3
	neg	x10, x10
	lsl	x10, x23, x10
	ldr	x11, [x20, #8]
	rev	x11, x11
	ldr	x12, [x1, #8]
	rev	x12, x12
	and	x11, x11, x10
	and	x10, x12, x10
	cmp	x11, x10
	b.ne	LBB6_12
LBB6_11:                                ; %L240
                                        ;   in Loop: Header=BB6_5 Depth=1
	cmp	x8, x9
	csinc	x8, x21, xzr, hi
	csinv	x0, x8, xzr, hs
	cmp	x0, #1
	b.ne	LBB6_13
	b	LBB6_16
LBB6_12:                                ; %L245
                                        ;   in Loop: Header=BB6_5 Depth=1
	csinv	x0, x21, xzr, hs
	cmp	x0, #1
	b.eq	LBB6_16
LBB6_13:                                ; %L222
                                        ;   in Loop: Header=BB6_5 Depth=1
	cmp	x0, #0
	b.gt	LBB6_3
; %bb.14:                               ; %L224
                                        ;   in Loop: Header=BB6_5 Depth=1
	ldr	x19, [x19]
	tbz	w19, #0, LBB6_4
LBB6_15:
	mov	x0, #-1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB6_16:                                ; %L218
	ldr	x0, [x19, #16]
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlString_tree_first_byte_diff__run_7_15_code ; -- Begin function _camlString_tree_first_byte_diff__run_7_15_code
	.p2align	2
