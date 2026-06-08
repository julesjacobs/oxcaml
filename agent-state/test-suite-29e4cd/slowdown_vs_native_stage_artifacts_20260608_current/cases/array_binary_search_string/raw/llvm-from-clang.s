	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	_camlArray_binary_search_string__code_begin
_camlArray_binary_search_string__code_begin:
	.section	__DATA,__data
	.globl	_camlArray_binary_search_string__data_begin
_camlArray_binary_search_string__data_begin:
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlArray_binary_search_string__black_box_int_0_9_code ; -- Begin function _camlArray_binary_search_string__black_box_int_0_9_code
	.p2align	2
_camlArray_binary_search_string__black_box_int_0_9_code: ; @"\01_camlArray_binary_search_string__black_box_int_0_9_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlArray_binary_search_string__black_box_string_1_10_code ; -- Begin function _camlArray_binary_search_string__black_box_string_1_10_code
	.p2align	2
_camlArray_binary_search_string__black_box_string_1_10_code: ; @"\01_camlArray_binary_search_string__black_box_string_1_10_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlArray_binary_search_string__black_box_2_11_code ; -- Begin function _camlArray_binary_search_string__black_box_2_11_code
	.p2align	2
_camlArray_binary_search_string__black_box_2_11_code: ; @"\01_camlArray_binary_search_string__black_box_2_11_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlArray_binary_search_string__print_result_3_12_code ; -- Begin function _camlArray_binary_search_string__print_result_3_12_code
	.p2align	2
_camlArray_binary_search_string__print_result_3_12_code: ; @"\01_camlArray_binary_search_string__print_result_3_12_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #16
	.cfi_def_cfa_offset 32
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	b.lo	LBB3_2
LBB3_1:                                 ; %L117
Lloh0:
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
Lloh1:
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
Lloh2:
	adrp	x2, _camlArray_binary_search_string__const_block66@PAGE
Lloh3:
	add	x2, x2, _camlArray_binary_search_string__const_block66@PAGEOFF
	mov	w1, #1
	bl	_camlCamlinternalFormat__make_printf_120_401_code
Ltmp0:
	mov	x1, x0
	ldr	x8, [sp, #8]                    ; 8-byte Folded Reload
	and	x0, x8, #0x7fffffff
	ldr	x2, [x1]
	ldr	x30, [sp, #24]                  ; 8-byte Folded Reload
	add	sp, sp, #32
	br	x2
LBB3_2:                                 ; %L116
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp1:
	b	LBB3_1
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpLdrGot	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_camlArray_binary_search_string__find_4_13_code ; -- Begin function _camlArray_binary_search_string__find_4_13_code
	.p2align	2
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
_camlArray_binary_search_string__run_6_15_code: ; @"\01_camlArray_binary_search_string__run_6_15_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #96
	.cfi_def_cfa_offset 112
	str	x1, [sp, #48]                   ; 8-byte Folded Spill
	str	x0, [sp, #40]                   ; 8-byte Folded Spill
Lloh4:
	adrp	x8, _caml_format_int@GOTPAGE
Lloh5:
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
Lloh6:
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
Lloh7:
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	mov	w20, #1
	mov	w1, #1
	bl	_caml_c_call
Ltmp2:
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	lsl	x9, x8, #1
	bfi	x20, x8, #1, #63
	add	x9, x9, #35
	str	x0, [sp, #88]
	str	x9, [sp, #80]
Lloh8:
	adrp	x8, _caml_create_bytes@GOTPAGE
Lloh9:
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	mov	x0, x9
	bl	_caml_c_call
Ltmp3:
	mov	x19, x0
	ldr	x21, [sp, #88]
Lloh10:
	adrp	x0, _camlArray_binary_search_string__immstring129@PAGE
Lloh11:
	add	x0, x0, _camlArray_binary_search_string__immstring129@PAGEOFF
	mov	w1, #1
	mov	x2, x19
	mov	w3, #1
	mov	w4, #35
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	mov	x0, x21
	mov	w1, #1
	mov	x2, x19
	mov	w3, #35
	mov	x4, x20
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB5_21
LBB5_1:                                 ; %L346
	mov	w8, #2048
	str	x8, [x27]
	mov	x1, x27
	str	x19, [x1, #8]!
	mov	w8, #1
	str	x8, [x27, #16]
	str	x1, [sp, #88]
Lloh12:
	adrp	x8, _caml_array_make@GOTPAGE
Lloh13:
	ldr	x8, [x8, _caml_array_make@GOTPAGEOFF]
	mov	w0, #129
	bl	_caml_c_call
Ltmp4:
	mov	x21, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	b.lo	LBB5_22
LBB5_2:                                 ; %L204.preheader
	mov	w24, #1
Lloh14:
	adrp	x25, _camlArray_binary_search_string__immstring129@PAGE
Lloh15:
	add	x25, x25, _camlArray_binary_search_string__immstring129@PAGEOFF
LBB5_3:                                 ; %L204
                                        ; =>This Inner Loop Header: Depth=1
	mov	w8, #1
	orr	x19, x8, x24, lsl #1
	str	x21, [sp, #88]
	str	x19, [sp, #80]
Lloh16:
	adrp	x8, _caml_format_int@GOTPAGE
Lloh17:
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
Lloh18:
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
Lloh19:
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	mov	x1, x19
	bl	_caml_c_call
Ltmp5:
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	lsl	x9, x8, #1
	mov	w22, #1
	bfi	x22, x8, #1, #63
	add	x9, x9, #35
	str	x0, [sp, #80]
	str	x9, [sp, #72]
Lloh20:
	adrp	x8, _caml_create_bytes@GOTPAGE
Lloh21:
	ldr	x8, [x8, _caml_create_bytes@GOTPAGEOFF]
	mov	x0, x9
	bl	_caml_c_call
Ltmp6:
	mov	x20, x0
	ldr	x21, [sp, #88]
	ldr	x23, [sp, #80]
	mov	x0, x25
	mov	w1, #1
	mov	x2, x20
	mov	w3, #1
	mov	w4, #35
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	mov	x0, x23
	mov	w1, #1
	mov	x2, x20
	mov	w3, #35
	mov	x4, x22
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_blit_string
	mov	sp, x29
	.cfi_restore_state
	ldurb	w8, [x21, #-8]
	cmp	w8, #254
	b.eq	LBB5_23
; %bb.4:                                ; %L226
                                        ;   in Loop: Header=BB5_3 Depth=1
	sub	x27, x27, #24
	ldr	x8, [x28]
	cmp	x8, x27
	b.hi	LBB5_6
LBB5_5:                                 ; %L351
                                        ;   in Loop: Header=BB5_3 Depth=1
	mov	w8, #2048
	str	x8, [x27]
	mov	x1, x27
	str	x20, [x1, #8]!
	str	x19, [x27, #16]
	add	x8, x21, x19, lsl #2
	sub	x0, x8, #4
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_modify
	mov	sp, x29
	.cfi_restore_state
	add	x24, x24, #1
	cmp	x24, #64
	b.ne	LBB5_3
	b	LBB5_7
LBB5_6:                                 ; %L350
                                        ;   in Loop: Header=BB5_3 Depth=1
	bl	_caml_call_gc
Ltmp7:
	b	LBB5_5
LBB5_7:                                 ; %L236
	ldr	x8, [x21]
	ldr	x1, [x8]
	str	x1, [sp, #80]
	str	x21, [sp, #88]
Lloh22:
	adrp	x8, _caml_array_make@GOTPAGE
Lloh23:
	ldr	x8, [x8, _caml_array_make@GOTPAGEOFF]
	mov	w0, #129
	bl	_caml_c_call
Ltmp8:
	mov	x22, x0
	ldr	x21, [sp, #88]
	mov	w19, #8
	b	LBB5_9
LBB5_8:                                 ; %L272
                                        ;   in Loop: Header=BB5_9 Depth=1
	add	x0, x22, x19
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_modify
	mov	sp, x29
	.cfi_restore_state
	add	x19, x19, #8
	cmp	x19, #512
	b.eq	LBB5_11
LBB5_9:                                 ; %L252
                                        ; =>This Inner Loop Header: Depth=1
	ldr	x8, [x21, x19]
	ldr	x1, [x8]
	ldurb	w8, [x22, #-8]
	cmp	w8, #254
	b.ne	LBB5_8
; %bb.10:                               ; %L265
                                        ;   in Loop: Header=BB5_9 Depth=1
	ldr	d0, [x1]
	str	d0, [x22, x19]
	add	x19, x19, #8
	cmp	x19, #512
	b.ne	LBB5_9
LBB5_11:                                ; %L284
	ldr	x8, [sp, #48]                   ; 8-byte Folded Reload
	cmp	x8, #3
	b.ge	LBB5_13
; %bb.12:
	mov	w0, #1
	b	LBB5_20
LBB5_13:                                ; %L291
	lsr	x9, x8, #1
	ldr	x8, [sp, #40]                   ; 8-byte Folded Reload
	cmp	x8, #3
	b.ge	LBB5_16
; %bb.14:                               ; %L297.us.preheader
	mov	w0, #1
	mov	w8, #1
LBB5_15:                                ; %L297.us
                                        ; =>This Inner Loop Header: Depth=1
	add	x8, x8, #1
	cmp	x8, x9
	b.le	LBB5_15
	b	LBB5_20
LBB5_16:                                ; %L297.preheader
	lsr	x8, x8, #1
	mov	w0, #1
	mov	w10, #1
	str	x9, [sp, #8]                    ; 8-byte Folded Spill
	str	x8, [sp, #24]                   ; 8-byte Folded Spill
LBB5_17:                                ; %L297
                                        ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB5_18 Depth 2
	str	x22, [sp, #56]
	str	x21, [sp, #64]
	mov	w9, #1
	str	x10, [sp, #16]                  ; 8-byte Folded Spill
	orr	x9, x9, x10, lsl #1
	str	x9, [sp, #32]                   ; 8-byte Folded Spill
	mov	w9, #1
LBB5_18:                                ; %L309
                                        ;   Parent Loop BB5_17 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	str	x0, [sp, #40]                   ; 8-byte Folded Spill
	str	x9, [sp, #48]                   ; 8-byte Folded Spill
	ldr	x8, [sp, #32]                   ; 8-byte Folded Reload
	add	w8, w8, w9, lsl #1
	ubfiz	x8, x8, #2, #7
	add	x8, x22, x8
	ldur	x0, [x8, #-4]
	mov	x1, x21
	bl	_camlArray_binary_search_string__find_4_13_code
Ltmp9:
	ldr	x9, [sp, #48]                   ; 8-byte Folded Reload
	ldr	x21, [sp, #64]
	ldr	x22, [sp, #56]
	ldr	x8, [sp, #40]                   ; 8-byte Folded Reload
	add	x8, x8, x0
	sub	x0, x8, #1
	ldr	x8, [sp, #24]                   ; 8-byte Folded Reload
	add	x9, x9, #1
	cmp	x9, x8
	b.le	LBB5_18
; %bb.19:                               ; %L330.loopexit
                                        ;   in Loop: Header=BB5_17 Depth=1
	ldr	x10, [sp, #16]                  ; 8-byte Folded Reload
	add	x10, x10, #1
	ldr	x9, [sp, #8]                    ; 8-byte Folded Reload
	cmp	x10, x9
	b.le	LBB5_17
LBB5_20:                                ; %common.ret
	ldr	x30, [sp, #104]                 ; 8-byte Folded Reload
	add	sp, sp, #112
	ret
LBB5_21:                                ; %L345
	bl	_caml_call_gc
Ltmp10:
	b	LBB5_1
LBB5_22:                                ; %L347
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp11:
	b	LBB5_2
LBB5_23:                                ; %L224
Lloh24:
	adrp	x0, _camlArray_binary_search_string__invalid776@PAGE
Lloh25:
	add	x0, x0, _camlArray_binary_search_string__invalid776@PAGEOFF
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_flambda2_invalid
	mov	sp, x29
	.cfi_restore_state
	brk	#0x1
	.loh AdrpAdd	Lloh10, Lloh11
	.loh AdrpLdrGot	Lloh8, Lloh9
	.loh AdrpLdrGot	Lloh6, Lloh7
	.loh AdrpLdrGot	Lloh4, Lloh5
	.loh AdrpLdrGot	Lloh12, Lloh13
	.loh AdrpAdd	Lloh14, Lloh15
	.loh AdrpLdrGot	Lloh20, Lloh21
	.loh AdrpLdrGot	Lloh18, Lloh19
	.loh AdrpLdrGot	Lloh16, Lloh17
	.loh AdrpLdrGot	Lloh22, Lloh23
	.loh AdrpAdd	Lloh24, Lloh25
	.cfi_endproc
                                        ; -- End function
	.globl	_camlArray_binary_search_string__entry ; -- Begin function _camlArray_binary_search_string__entry
	.p2align	2
_camlArray_binary_search_string__entry: ; @"\01_camlArray_binary_search_string__entry"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	sub	sp, sp, #32
	.cfi_def_cfa_offset 48
Lloh26:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh27:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp12:
	ldur	x8, [x0, #-8]
	tst	x8, #0xfffffffffff800
	b.eq	LBB6_3
; %bb.1:                                ; %L373
Lloh28:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh29:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp13:
	ldur	x8, [x0, #-8]
	tst	x8, #0xfffffffffff800
	b.eq	LBB6_11
; %bb.2:                                ; %L383
	ldr	x0, [x0, #8]
	str	x0, [sp, #24]
Lloh30:
	adrp	x8, _caml_int_of_string@GOTPAGE
Lloh31:
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp14:
	mov	x20, x0
	b	LBB6_4
LBB6_3:
	mov	w20, #3393
	movk	w20, #3, lsl #16
LBB6_4:                                 ; %L397
Lloh32:
	adrp	x0, _camlArray_binary_search_string@PAGE+24
Lloh33:
	add	x0, x0, _camlArray_binary_search_string@PAGEOFF+24
	mov	x1, x20
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x29
	.cfi_restore_state
Lloh34:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh35:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp15:
	ldur	x8, [x0, #-8]
	and	x8, x8, #0xfffffffffffc00
	cmp	x8, #2561
	b.lo	LBB6_7
; %bb.5:                                ; %L409
Lloh36:
	adrp	x8, _caml_sys_argv@GOTPAGE
Lloh37:
	ldr	x8, [x8, _caml_sys_argv@GOTPAGEOFF]
	mov	w0, #1
	bl	_caml_c_call
Ltmp16:
	ldur	x8, [x0, #-8]
	and	x8, x8, #0xfffffffffffc00
	cmp	x8, #2561
	b.lo	LBB6_11
; %bb.6:                                ; %L419
	ldr	x0, [x0, #16]
	str	x0, [sp, #24]
Lloh38:
	adrp	x8, _caml_int_of_string@GOTPAGE
Lloh39:
	ldr	x8, [x8, _caml_int_of_string@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp17:
	mov	x19, x0
	b	LBB6_8
LBB6_7:
	mov	w19, #21
LBB6_8:                                 ; %L433
Lloh40:
	adrp	x0, _camlArray_binary_search_string@PAGE+32
Lloh41:
	add	x0, x0, _camlArray_binary_search_string@PAGEOFF+32
	mov	x1, x19
	mov	x29, sp
	.cfi_remember_state
	.cfi_def_cfa_register w29
	ldr	x16, [x28, #104]
	mov	sp, x16
	bl	_caml_initialize
	mov	sp, x29
	.cfi_restore_state
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	; InlineAsm Start
	mov	x9, sp
	; InlineAsm End
	cmp	x9, x8
	str	x20, [sp, #16]                  ; 8-byte Folded Spill
	b.lo	LBB6_10
LBB6_9:                                 ; %L454
	mov	x0, x19
	bl	_camlArray_binary_search_string__black_box_int_0_9_code
Ltmp18:
	str	x0, [sp, #8]                    ; 8-byte Folded Spill
	ldr	x0, [sp, #16]                   ; 8-byte Folded Reload
	bl	_camlArray_binary_search_string__black_box_int_0_9_code
Ltmp19:
	ldr	x1, [sp, #8]                    ; 8-byte Folded Reload
	bl	_camlArray_binary_search_string__run_6_15_code
Ltmp20:
	str	x0, [sp, #16]                   ; 8-byte Folded Spill
Lloh42:
	adrp	x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGE
Lloh43:
	ldr	x0, [x0, _camlStdlib__Printf__fn$5b$2fworkspace_root$2fprintf.ml$3a27$2c14$2d$2d48$5d_31@GOTPAGEOFF]
Lloh44:
	adrp	x2, _camlArray_binary_search_string__const_block66@PAGE
Lloh45:
	add	x2, x2, _camlArray_binary_search_string__const_block66@PAGEOFF
	mov	w1, #1
	bl	_camlCamlinternalFormat__make_printf_120_401_code
Ltmp21:
	mov	x1, x0
	ldr	x8, [sp, #16]                   ; 8-byte Folded Reload
	and	x0, x8, #0x7fffffff
	ldr	x8, [x1]
	blr	x8
Ltmp22:
	mov	w0, #1
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB6_10:                                ; %L453
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp23:
	b	LBB6_9
LBB6_11:                                ; %L427
Lloh46:
	adrp	x8, _camlArray_binary_search_string__block35@PAGE
Lloh47:
	add	x8, x8, _camlArray_binary_search_string__block35@PAGEOFF
	mov	x0, x8
	mov	sp, x26
	ldp	x26, x16, [sp], #16
	br	x16
	.loh AdrpLdrGot	Lloh26, Lloh27
	.loh AdrpLdrGot	Lloh28, Lloh29
	.loh AdrpLdrGot	Lloh30, Lloh31
	.loh AdrpLdrGot	Lloh34, Lloh35
	.loh AdrpAdd	Lloh32, Lloh33
	.loh AdrpLdrGot	Lloh36, Lloh37
	.loh AdrpLdrGot	Lloh38, Lloh39
	.loh AdrpAdd	Lloh40, Lloh41
	.loh AdrpAdd	Lloh44, Lloh45
	.loh AdrpLdrGot	Lloh42, Lloh43
	.loh AdrpAdd	Lloh46, Lloh47
	.cfi_endproc
                                        ; -- End function
	.section	__DATA,__data
	.globl	_camlArray_binary_search_string__gc_roots ; @"\01_camlArray_binary_search_string__gc_roots"
	.p2align	3, 0x0
_camlArray_binary_search_string__gc_roots:
	.quad	_camlArray_binary_search_string
	.quad	0                               ; 0x0

	.globl	_header.camlArray_binary_search_string ; @"\01_header.camlArray_binary_search_string"
	.p2align	3, 0x0
_header.camlArray_binary_search_string:
	.quad	8960                            ; 0x2300

	.globl	_camlArray_binary_search_string ; @"\01_camlArray_binary_search_string"
	.p2align	3, 0x0
_camlArray_binary_search_string:
	.quad	_camlArray_binary_search_string__black_box_int_9
	.quad	_camlArray_binary_search_string__black_box_string_10
	.quad	_camlArray_binary_search_string__black_box_11
	.quad	1                               ; 0x1
	.quad	1                               ; 0x1
	.quad	_camlArray_binary_search_string__print_result_12
	.quad	_camlArray_binary_search_string__find_13
	.quad	_camlArray_binary_search_string__run_14

	.globl	_header.camlArray_binary_search_string__run_14 ; @"\01_header.camlArray_binary_search_string__run_14"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__run_14:
	.quad	4087                            ; 0xff7

	.globl	_camlArray_binary_search_string__run_14 ; @"\01_camlArray_binary_search_string__run_14"
	.p2align	3, 0x0
_camlArray_binary_search_string__run_14:
	.quad	_caml_curry2
	.quad	180143985094819847              ; 0x280000000000007
	.quad	_camlArray_binary_search_string__run_6_15_code

	.globl	_header.camlArray_binary_search_string__invalid776 ; @"\01_header.camlArray_binary_search_string__invalid776"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__invalid776:
	.quad	16380                           ; 0x3ffc

	.globl	_camlArray_binary_search_string__invalid776 ; @"\01_camlArray_binary_search_string__invalid776"
	.p2align	3, 0x0
_camlArray_binary_search_string__invalid776:
	.ascii	"(Defining_expr_of_let (bound_pattern prim/452N)\n (defining_expr ((Unbox_float apply_result/435N) array.ml:87,5--27)))"
	.space	2
	.byte	2                               ; 0x2

	.globl	_header.camlArray_binary_search_string__find_13 ; @"\01_header.camlArray_binary_search_string__find_13"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__find_13:
	.quad	4087                            ; 0xff7

	.globl	_camlArray_binary_search_string__find_13 ; @"\01_camlArray_binary_search_string__find_13"
	.p2align	3, 0x0
_camlArray_binary_search_string__find_13:
	.quad	_caml_curry2
	.quad	180143985094819847              ; 0x280000000000007
	.quad	_camlArray_binary_search_string__find_4_13_code

	.globl	_header.camlArray_binary_search_string__print_result_12 ; @"\01_header.camlArray_binary_search_string__print_result_12"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__print_result_12:
	.quad	3063                            ; 0xbf7

	.globl	_camlArray_binary_search_string__print_result_12 ; @"\01_camlArray_binary_search_string__print_result_12"
	.p2align	3, 0x0
_camlArray_binary_search_string__print_result_12:
	.quad	_camlArray_binary_search_string__print_result_3_12_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlArray_binary_search_string__black_box_11 ; @"\01_header.camlArray_binary_search_string__black_box_11"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__black_box_11:
	.quad	3063                            ; 0xbf7

	.globl	_camlArray_binary_search_string__black_box_11 ; @"\01_camlArray_binary_search_string__black_box_11"
	.p2align	3, 0x0
_camlArray_binary_search_string__black_box_11:
	.quad	_camlArray_binary_search_string__black_box_2_11_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlArray_binary_search_string__black_box_string_10 ; @"\01_header.camlArray_binary_search_string__black_box_string_10"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__black_box_string_10:
	.quad	3063                            ; 0xbf7

	.globl	_camlArray_binary_search_string__black_box_string_10 ; @"\01_camlArray_binary_search_string__black_box_string_10"
	.p2align	3, 0x0
_camlArray_binary_search_string__black_box_string_10:
	.quad	_camlArray_binary_search_string__black_box_string_1_10_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlArray_binary_search_string__black_box_int_9 ; @"\01_header.camlArray_binary_search_string__black_box_int_9"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__black_box_int_9:
	.quad	3063                            ; 0xbf7

	.globl	_camlArray_binary_search_string__black_box_int_9 ; @"\01_camlArray_binary_search_string__black_box_int_9"
	.p2align	3, 0x0
_camlArray_binary_search_string__black_box_int_9:
	.quad	_camlArray_binary_search_string__black_box_int_0_9_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlArray_binary_search_string__block35 ; @"\01_header.camlArray_binary_search_string__block35"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__block35:
	.quad	2816                            ; 0xb00

	.globl	_camlArray_binary_search_string__block35 ; @"\01_camlArray_binary_search_string__block35"
	.p2align	3, 0x0
_camlArray_binary_search_string__block35:
	.quad	_caml_exn_Invalid_argument
	.quad	_camlArray_binary_search_string__string33

	.globl	_header.camlArray_binary_search_string__string33 ; @"\01_header.camlArray_binary_search_string__string33"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__string33:
	.quad	4092                            ; 0xffc

	.globl	_camlArray_binary_search_string__string33 ; @"\01_camlArray_binary_search_string__string33"
	.p2align	3, 0x0
_camlArray_binary_search_string__string33:
	.ascii	"index out of bounds"
	.space	4
	.byte	4                               ; 0x4

	.globl	_header.camlArray_binary_search_string__immstring129 ; @"\01_header.camlArray_binary_search_string__immstring129"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__immstring129:
	.quad	4092                            ; 0xffc

	.globl	_camlArray_binary_search_string__immstring129 ; @"\01_camlArray_binary_search_string__immstring129"
	.p2align	3, 0x0
_camlArray_binary_search_string__immstring129:
	.ascii	"array_binary_key_"
	.space	6
	.byte	6                               ; 0x6

	.globl	_header.camlArray_binary_search_string__const_block66 ; @"\01_header.camlArray_binary_search_string__const_block66"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__const_block66:
	.quad	4868                            ; 0x1304

	.globl	_camlArray_binary_search_string__const_block66 ; @"\01_camlArray_binary_search_string__const_block66"
	.p2align	3, 0x0
_camlArray_binary_search_string__const_block66:
	.quad	1                               ; 0x1
	.quad	1                               ; 0x1
	.quad	1                               ; 0x1
	.quad	_camlArray_binary_search_string__const_block64

	.globl	_header.camlArray_binary_search_string__const_block64 ; @"\01_header.camlArray_binary_search_string__const_block64"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__const_block64:
	.quad	2828                            ; 0xb0c

	.globl	_camlArray_binary_search_string__const_block64 ; @"\01_camlArray_binary_search_string__const_block64"
	.p2align	3, 0x0
_camlArray_binary_search_string__const_block64:
	.quad	21                              ; 0x15
	.quad	_camlArray_binary_search_string__const_block62

	.globl	_header.camlArray_binary_search_string__const_block62 ; @"\01_header.camlArray_binary_search_string__const_block62"
	.p2align	3, 0x0
_header.camlArray_binary_search_string__const_block62:
	.quad	1802                            ; 0x70a

	.globl	_camlArray_binary_search_string__const_block62 ; @"\01_camlArray_binary_search_string__const_block62"
	.p2align	3, 0x0
_camlArray_binary_search_string__const_block62:
	.quad	1                               ; 0x1

	.quad	0
	.globl	_camlArray_binary_search_string__data_end
_camlArray_binary_search_string__data_end:
	.quad	0
	.p2align	3, 0x0
	.globl	_camlArray_binary_search_string__frametable
_camlArray_binary_search_string__frametable:
	.quad	24
Ltmp24:
	.long	Ltmp0-Ltmp24
	.short	33
	.short	0
	.p2align	2, 0x0
Ltmp26:
	.long	Ltmp25-Ltmp26
	.p2align	3, 0x0
Ltmp27:
	.long	Ltmp1-Ltmp27
	.short	32
	.short	0
	.p2align	3, 0x0
Ltmp28:
	.long	Ltmp2-Ltmp28
	.short	113
	.short	0
	.p2align	2, 0x0
Ltmp30:
	.long	Ltmp29-Ltmp30
	.p2align	3, 0x0
Ltmp31:
	.long	Ltmp3-Ltmp31
	.short	113
	.short	2
	.short	88
	.short	80
	.p2align	2, 0x0
Ltmp33:
	.long	Ltmp32-Ltmp33
	.p2align	3, 0x0
Ltmp34:
	.long	Ltmp4-Ltmp34
	.short	113
	.short	1
	.short	88
	.p2align	2, 0x0
Ltmp36:
	.long	Ltmp35-Ltmp36
	.p2align	3, 0x0
Ltmp37:
	.long	Ltmp5-Ltmp37
	.short	113
	.short	2
	.short	88
	.short	80
	.p2align	2, 0x0
Ltmp39:
	.long	Ltmp38-Ltmp39
	.p2align	3, 0x0
Ltmp40:
	.long	Ltmp6-Ltmp40
	.short	113
	.short	3
	.short	80
	.short	88
	.short	72
	.p2align	2, 0x0
Ltmp42:
	.long	Ltmp41-Ltmp42
	.p2align	3, 0x0
Ltmp43:
	.long	Ltmp7-Ltmp43
	.short	115
	.short	2
	.short	35
	.short	37
	.byte	1
	.byte	1
	.p2align	2, 0x0
Ltmp45:
	.long	Ltmp44-Ltmp45
	.p2align	3, 0x0
Ltmp46:
	.long	Ltmp8-Ltmp46
	.short	113
	.short	2
	.short	88
	.short	80
	.p2align	2, 0x0
Ltmp48:
	.long	Ltmp47-Ltmp48
	.p2align	3, 0x0
Ltmp49:
	.long	Ltmp9-Ltmp49
	.short	113
	.short	2
	.short	56
	.short	64
	.p2align	2, 0x0
Ltmp51:
	.long	Ltmp50-Ltmp51
	.p2align	3, 0x0
Ltmp52:
	.long	Ltmp10-Ltmp52
	.short	115
	.short	1
	.short	33
	.byte	1
	.byte	1
	.p2align	2, 0x0
Ltmp54:
	.long	Ltmp53-Ltmp54
	.p2align	3, 0x0
Ltmp55:
	.long	Ltmp11-Ltmp55
	.short	112
	.short	1
	.short	37
	.p2align	3, 0x0
Ltmp56:
	.long	Ltmp12-Ltmp56
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp58:
	.long	Ltmp57-Ltmp58
	.p2align	3, 0x0
Ltmp59:
	.long	Ltmp13-Ltmp59
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp61:
	.long	Ltmp60-Ltmp61
	.p2align	3, 0x0
Ltmp62:
	.long	Ltmp14-Ltmp62
	.short	49
	.short	1
	.short	24
	.p2align	2, 0x0
Ltmp64:
	.long	Ltmp63-Ltmp64
	.p2align	3, 0x0
Ltmp65:
	.long	Ltmp15-Ltmp65
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp67:
	.long	Ltmp66-Ltmp67
	.p2align	3, 0x0
Ltmp68:
	.long	Ltmp16-Ltmp68
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp70:
	.long	Ltmp69-Ltmp70
	.p2align	3, 0x0
Ltmp71:
	.long	Ltmp17-Ltmp71
	.short	49
	.short	1
	.short	24
	.p2align	2, 0x0
Ltmp73:
	.long	Ltmp72-Ltmp73
	.p2align	3, 0x0
Ltmp74:
	.long	Ltmp18-Ltmp74
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp76:
	.long	Ltmp75-Ltmp76
	.p2align	3, 0x0
Ltmp77:
	.long	Ltmp19-Ltmp77
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp79:
	.long	Ltmp78-Ltmp79
	.p2align	3, 0x0
Ltmp80:
	.long	Ltmp20-Ltmp80
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp82:
	.long	Ltmp81-Ltmp82
	.p2align	3, 0x0
Ltmp83:
	.long	Ltmp21-Ltmp83
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp85:
	.long	Ltmp84-Ltmp85
	.p2align	3, 0x0
Ltmp86:
	.long	Ltmp22-Ltmp86
	.short	49
	.short	0
	.p2align	2, 0x0
Ltmp88:
	.long	Ltmp87-Ltmp88
	.p2align	3, 0x0
Ltmp89:
	.long	Ltmp23-Ltmp89
	.short	48
	.short	0
	.p2align	3, 0x0
	.p2align	2, 0x0
Ltmp25:
Ltmp98:
	.long	(Ltmp90-Ltmp98)+1
	.long	14158328
Ltmp99:
	.long	(Ltmp92-Ltmp99)+1
	.long	17847640
Ltmp100:
	.long	(Ltmp94-Ltmp100)+1
	.long	19940632
Ltmp101:
	.long	(Ltmp96-Ltmp101)+0
	.long	5789176
	.p2align	2, 0x0
Ltmp90:
	.long	Ltmp91-Ltmp90
	.ascii	"Stdlib__Printf.kfprintf"
	.byte	0
Ltmp91:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp92:
	.long	Ltmp93-Ltmp92
	.ascii	"Stdlib__Printf.fprintf"
	.byte	0
Ltmp93:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp94:
	.long	Ltmp95-Ltmp94
	.ascii	"Stdlib__Printf.printf"
	.byte	0
Ltmp95:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp96:
	.long	Ltmp97-Ltmp96
	.ascii	"Array_binary_search_string.print_result"
	.byte	0
Ltmp97:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp29:
Ltmp110:
	.long	(Ltmp102-Ltmp110)+1
	.long	146802840
Ltmp111:
	.long	(Ltmp104-Ltmp111)+1
	.long	14207496
Ltmp112:
	.long	(Ltmp106-Ltmp112)+1
	.long	44587224
Ltmp113:
	.long	(Ltmp108-Ltmp113)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp102:
	.long	Ltmp103-Ltmp102
	.ascii	"Stdlib.string_of_int"
	.byte	0
Ltmp103:
	.ascii	"stdlib.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp104:
	.long	Ltmp105-Ltmp104
	.ascii	"Array_binary_search_string.run.(fun)"
	.byte	0
Ltmp105:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp106:
	.long	Ltmp107-Ltmp106
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp107:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp108:
	.long	Ltmp109-Ltmp108
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp109:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp32:
Ltmp122:
	.long	(Ltmp114-Ltmp122)+1
	.long	118499584
Ltmp123:
	.long	(Ltmp116-Ltmp123)+1
	.long	14184968
Ltmp124:
	.long	(Ltmp118-Ltmp124)+1
	.long	44587224
Ltmp125:
	.long	(Ltmp120-Ltmp125)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp114:
	.long	Ltmp115-Ltmp114
	.ascii	"Stdlib.(^)"
	.byte	0
Ltmp115:
	.ascii	"stdlib.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp116:
	.long	Ltmp117-Ltmp116
	.ascii	"Array_binary_search_string.run.(fun)"
	.byte	0
Ltmp117:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp118:
	.long	Ltmp119-Ltmp118
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp119:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp120:
	.long	Ltmp121-Ltmp120
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp121:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp35:
Ltmp130:
	.long	(Ltmp126-Ltmp130)+1
	.long	44578008
Ltmp131:
	.long	(Ltmp128-Ltmp131)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp126:
	.long	Ltmp127-Ltmp126
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp127:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp128:
	.long	Ltmp129-Ltmp128
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp129:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp38:
Ltmp140:
	.long	(Ltmp132-Ltmp140)+1
	.long	146802840
Ltmp141:
	.long	(Ltmp134-Ltmp141)+1
	.long	14207496
Ltmp142:
	.long	(Ltmp136-Ltmp142)+1
	.long	45635800
Ltmp143:
	.long	(Ltmp138-Ltmp143)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp132:
	.long	Ltmp133-Ltmp132
	.ascii	"Stdlib.string_of_int"
	.byte	0
Ltmp133:
	.ascii	"stdlib.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp134:
	.long	Ltmp135-Ltmp134
	.ascii	"Array_binary_search_string.run.(fun)"
	.byte	0
Ltmp135:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp136:
	.long	Ltmp137-Ltmp136
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp137:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp138:
	.long	Ltmp139-Ltmp138
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp139:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp41:
Ltmp152:
	.long	(Ltmp144-Ltmp152)+1
	.long	118499584
Ltmp153:
	.long	(Ltmp146-Ltmp153)+1
	.long	14184968
Ltmp154:
	.long	(Ltmp148-Ltmp154)+1
	.long	45635800
Ltmp155:
	.long	(Ltmp150-Ltmp155)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp144:
	.long	Ltmp145-Ltmp144
	.ascii	"Stdlib.(^)"
	.byte	0
Ltmp145:
	.ascii	"stdlib.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp146:
	.long	Ltmp147-Ltmp146
	.ascii	"Array_binary_search_string.run.(fun)"
	.byte	0
Ltmp147:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp148:
	.long	Ltmp149-Ltmp148
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp149:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp150:
	.long	Ltmp151-Ltmp150
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp151:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp44:
Ltmp162:
	.long	(Ltmp156-Ltmp162)+1
	.long	14184992
Ltmp163:
	.long	(Ltmp158-Ltmp163)+1
	.long	45635800
Ltmp164:
	.long	(Ltmp160-Ltmp164)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp156:
	.long	Ltmp157-Ltmp156
	.ascii	"Array_binary_search_string.run.(fun)"
	.byte	0
Ltmp157:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp158:
	.long	Ltmp159-Ltmp158
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp159:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp160:
	.long	Ltmp161-Ltmp160
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp161:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp47:
Ltmp169:
	.long	(Ltmp165-Ltmp169)+1
	.long	81277248
Ltmp170:
	.long	(Ltmp167-Ltmp170)+0
	.long	15217888
	.p2align	2, 0x0
Ltmp165:
	.long	Ltmp166-Ltmp165
	.ascii	"Stdlib__Array.map"
	.byte	0
Ltmp166:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp167:
	.long	Ltmp168-Ltmp167
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp168:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp50:
Ltmp173:
	.long	(Ltmp171-Ltmp173)+0
	.long	17322528
	.p2align	2, 0x0
Ltmp171:
	.long	Ltmp172-Ltmp171
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp172:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp53:
Ltmp180:
	.long	(Ltmp174-Ltmp180)+1
	.long	14184992
Ltmp181:
	.long	(Ltmp176-Ltmp181)+1
	.long	44587224
Ltmp182:
	.long	(Ltmp178-Ltmp182)+0
	.long	14160424
	.p2align	2, 0x0
Ltmp174:
	.long	Ltmp175-Ltmp174
	.ascii	"Array_binary_search_string.run.(fun)"
	.byte	0
Ltmp175:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp176:
	.long	Ltmp177-Ltmp176
	.ascii	"Stdlib__Array.init"
	.byte	0
Ltmp177:
	.ascii	"array.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp178:
	.long	Ltmp179-Ltmp178
	.ascii	"Array_binary_search_string.run"
	.byte	0
Ltmp179:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp57:
Ltmp185:
	.long	(Ltmp183-Ltmp185)+0
	.long	3164368
	.p2align	2, 0x0
Ltmp183:
	.long	Ltmp184-Ltmp183
	.ascii	"Array_binary_search_string.n"
	.byte	0
Ltmp184:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp60:
Ltmp188:
	.long	(Ltmp186-Ltmp188)+0
	.long	3197392
	.p2align	2, 0x0
Ltmp186:
	.long	Ltmp187-Ltmp186
	.ascii	"Array_binary_search_string.n"
	.byte	0
Ltmp187:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp63:
Ltmp191:
	.long	(Ltmp189-Ltmp191)+0
	.long	3183088
	.p2align	2, 0x0
Ltmp189:
	.long	Ltmp190-Ltmp189
	.ascii	"Array_binary_search_string.n"
	.byte	0
Ltmp190:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp66:
Ltmp194:
	.long	(Ltmp192-Ltmp194)+0
	.long	4737232
	.p2align	2, 0x0
Ltmp192:
	.long	Ltmp193-Ltmp192
	.ascii	"Array_binary_search_string.reps"
	.byte	0
Ltmp193:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp69:
Ltmp197:
	.long	(Ltmp195-Ltmp197)+0
	.long	4770256
	.p2align	2, 0x0
Ltmp195:
	.long	Ltmp196-Ltmp195
	.ascii	"Array_binary_search_string.reps"
	.byte	0
Ltmp196:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp72:
Ltmp200:
	.long	(Ltmp198-Ltmp200)+0
	.long	4755952
	.p2align	2, 0x0
Ltmp198:
	.long	Ltmp199-Ltmp198
	.ascii	"Array_binary_search_string.reps"
	.byte	0
Ltmp199:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp75:
Ltmp203:
	.long	(Ltmp201-Ltmp203)+0
	.long	19969544
	.p2align	2, 0x0
Ltmp201:
	.long	Ltmp202-Ltmp201
	.ascii	"Array_binary_search_string"
	.byte	0
Ltmp202:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp78:
Ltmp206:
	.long	(Ltmp204-Ltmp206)+0
	.long	19950944
	.p2align	2, 0x0
Ltmp204:
	.long	Ltmp205-Ltmp204
	.ascii	"Array_binary_search_string"
	.byte	0
Ltmp205:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp81:
Ltmp209:
	.long	(Ltmp207-Ltmp209)+0
	.long	19946000
	.p2align	2, 0x0
Ltmp207:
	.long	Ltmp208-Ltmp207
	.ascii	"Array_binary_search_string"
	.byte	0
Ltmp208:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp84:
Ltmp220:
	.long	(Ltmp210-Ltmp220)+1
	.long	14158328
Ltmp221:
	.long	(Ltmp212-Ltmp221)+1
	.long	17847640
Ltmp222:
	.long	(Ltmp214-Ltmp222)+1
	.long	19940632
Ltmp223:
	.long	(Ltmp216-Ltmp223)+1
	.long	5789176
Ltmp224:
	.long	(Ltmp218-Ltmp224)+0
	.long	19932688
	.p2align	2, 0x0
Ltmp210:
	.long	Ltmp211-Ltmp210
	.ascii	"Stdlib__Printf.kfprintf"
	.byte	0
Ltmp211:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp212:
	.long	Ltmp213-Ltmp212
	.ascii	"Stdlib__Printf.fprintf"
	.byte	0
Ltmp213:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp214:
	.long	Ltmp215-Ltmp214
	.ascii	"Stdlib__Printf.printf"
	.byte	0
Ltmp215:
	.ascii	"printf.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp216:
	.long	Ltmp217-Ltmp216
	.ascii	"Array_binary_search_string.print_result"
	.byte	0
Ltmp217:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp218:
	.long	Ltmp219-Ltmp218
	.ascii	"Array_binary_search_string"
	.byte	0
Ltmp219:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp87:
Ltmp229:
	.long	(Ltmp225-Ltmp229)+1
	.long	5789176
Ltmp230:
	.long	(Ltmp227-Ltmp230)+0
	.long	19932688
	.p2align	2, 0x0
Ltmp225:
	.long	Ltmp226-Ltmp225
	.ascii	"Array_binary_search_string.print_result"
	.byte	0
Ltmp226:
	.ascii	"array_binary_search_string.ml"
	.byte	0
	.p2align	2, 0x0
Ltmp227:
	.long	Ltmp228-Ltmp227
	.ascii	"Array_binary_search_string"
	.byte	0
Ltmp228:
	.ascii	"array_binary_search_string.ml"
	.byte	0

	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlArray_binary_search_string__code_end
_camlArray_binary_search_string__code_end:
.subsections_via_symbols
