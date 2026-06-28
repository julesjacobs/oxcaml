_camlHash_lookup_string_equal__find_4_13_code: ; @"\01_camlHash_lookup_string_equal__find_4_13_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	ldr	x20, [x1]
	tbnz	w20, #0, LBB4_5
; %bb.1:
	mov	x19, x0
LBB4_2:                                 ; %L129
                                        ; =>This Inner Loop Header: Depth=1
	ldr	x21, [x20]
	ldr	x1, [x21]
	cmp	x1, x19
	b.eq	LBB4_6
; %bb.3:                                ; %L132
                                        ;   in Loop: Header=BB4_2 Depth=1
	mov	x0, x19
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_string_equal
	mov	sp, x29
	.cfi_restore_state
	cmp	x0, #1
	b.ne	LBB4_6
; %bb.4:                                ; %L134
                                        ;   in Loop: Header=BB4_2 Depth=1
	ldr	x20, [x20, #8]
	tbz	w20, #0, LBB4_2
LBB4_5:                                 ; %common.ret.loopexit
	mov	x0, #-1
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
LBB4_6:                                 ; %L138
	ldr	x0, [x21, #8]
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlHash_lookup_string_equal__run_6_15_code ; -- Begin function _camlHash_lookup_string_equal__run_6_15_code
	.p2align	2
