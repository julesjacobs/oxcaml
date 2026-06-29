	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 16, 0
	.globl	_camlLoop_invariant_gc_across_call__code_begin
_camlLoop_invariant_gc_across_call__code_begin:
	.section	__DATA,__data
	.globl	_camlLoop_invariant_gc_across_call__data_begin
_camlLoop_invariant_gc_across_call__data_begin:
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; -- Begin function _camlLoop_invariant_gc_across_call__tick_0_4_code
	.p2align	2
_camlLoop_invariant_gc_across_call__tick_0_4_code: ; @"\01_camlLoop_invariant_gc_across_call__tick_0_4_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	add	x0, x0, #2
	; InlineAsm Start
	; InlineAsm End
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_camlLoop_invariant_gc_across_call__print_result_1_5_code ; -- Begin function _camlLoop_invariant_gc_across_call__print_result_1_5_code
	.p2align	2
_camlLoop_invariant_gc_across_call__print_result_1_5_code: ; @"\01_camlLoop_invariant_gc_across_call__print_result_1_5_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	str	x30, [sp, #8]                   ; 8-byte Folded Spill
	.cfi_offset w30, -16
	mov	x1, x0
Lloh0:
	adrp	x8, _caml_format_int@GOTPAGE
Lloh1:
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
Lloh2:
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
Lloh3:
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp0:
	mov	x1, x0
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	mov	w9, #1
	orr	x3, x9, x8, lsl #1
Lloh4:
	adrp	x8, _camlStdlib__print_int_136@GOTPAGE
Lloh5:
	ldr	x8, [x8, _camlStdlib__print_int_136@GOTPAGEOFF]
Lloh6:
	ldr	x0, [x8, #16]
Lloh7:
	adrp	x8, _caml_ml_output@GOTPAGE
Lloh8:
	ldr	x8, [x8, _caml_ml_output@GOTPAGEOFF]
	mov	w2, #1
	bl	_caml_c_call
Ltmp1:
Lloh9:
	adrp	x19, _camlStdlib__print_newline_139@GOTPAGE
Lloh10:
	ldr	x19, [x19, _camlStdlib__print_newline_139@GOTPAGEOFF]
	ldr	x0, [x19, #16]
Lloh11:
	adrp	x8, _caml_ml_output_char@GOTPAGE
Lloh12:
	ldr	x8, [x8, _caml_ml_output_char@GOTPAGEOFF]
	mov	w1, #21
	bl	_caml_c_call
Ltmp2:
	ldr	x0, [x19, #16]
Lloh13:
	adrp	x8, _caml_ml_flush@GOTPAGE
Lloh14:
	ldr	x8, [x8, _caml_ml_flush@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp3:
	ldr	x30, [sp, #8]                   ; 8-byte Folded Reload
	add	sp, sp, #16
	ret
	.loh AdrpLdrGot	Lloh13, Lloh14
	.loh AdrpLdrGot	Lloh11, Lloh12
	.loh AdrpLdrGot	Lloh9, Lloh10
	.loh AdrpLdrGot	Lloh7, Lloh8
	.loh AdrpLdrGotLdr	Lloh4, Lloh5, Lloh6
	.loh AdrpLdrGot	Lloh2, Lloh3
	.loh AdrpLdrGot	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_camlLoop_invariant_gc_across_call__loop_2_6_code ; -- Begin function _camlLoop_invariant_gc_across_call__loop_2_6_code
	.p2align	2
_camlLoop_invariant_gc_across_call__loop_2_6_code: ; @"\01_camlLoop_invariant_gc_across_call__loop_2_6_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #48
	.cfi_def_cfa_offset 48
	str	x30, [sp, #40]                  ; 8-byte Folded Spill
	.cfi_offset w30, -16
	mov	x10, x0
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	cmp	sp, x8
	b.lo	LBB2_4
; %bb.1:                                ; %L156
	cmp	x1, #2
	b.lt	LBB2_3
LBB2_2:                                 ; %L137
                                        ; =>This Inner Loop Header: Depth=1
	str	x2, [sp, #8]                    ; 8-byte Folded Spill
	str	x10, [sp, #24]                  ; 8-byte Folded Spill
	str	x1, [sp, #16]                   ; 8-byte Folded Spill
	mov	x0, x1
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp4:
	ldr	x10, [sp, #24]                  ; 8-byte Folded Reload
	ldr	x1, [sp, #16]                   ; 8-byte Folded Reload
	ldur	x8, [x10, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x10, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #8]                    ; 8-byte Folded Reload
	add	x9, x9, x0
	add	x8, x9, x8, lsl #1
	sub	x2, x8, #1
	sub	x1, x1, #2
	cmp	x1, #1
	b.hi	LBB2_2
LBB2_3:                                 ; %L135
	mov	x0, x2
	ldr	x30, [sp, #40]                  ; 8-byte Folded Reload
	add	sp, sp, #48
	ret
LBB2_4:                                 ; %L155
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp5:
	cmp	x1, #2
	b.ge	LBB2_2
	b	LBB2_3
	.cfi_endproc
                                        ; -- End function
	.globl	_camlLoop_invariant_gc_across_call__run_3_7_code ; -- Begin function _camlLoop_invariant_gc_across_call__run_3_7_code
	.p2align	2
_camlLoop_invariant_gc_across_call__run_3_7_code: ; @"\01_camlLoop_invariant_gc_across_call__run_3_7_code"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #80
	.cfi_def_cfa_offset 80
	str	x30, [sp, #72]                  ; 8-byte Folded Spill
	.cfi_offset w30, -16
	str	x0, [sp, #16]                   ; 8-byte Folded Spill
	cmp	x1, #3
	b.ge	LBB3_2
; %bb.1:
	mov	w0, #1
	b	LBB3_12
LBB3_2:                                 ; %L162
Lloh15:
	adrp	x10, _camlLoop_invariant_gc_across_call__immstring48@PAGE
Lloh16:
	add	x10, x10, _camlLoop_invariant_gc_across_call__immstring48@PAGEOFF
	; InlineAsm Start
	; InlineAsm End
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	cmp	sp, x8
	b.lo	LBB3_9
; %bb.3:                                ; %L215
	lsr	x9, x1, #1
	ldr	x8, [sp, #16]                   ; 8-byte Folded Reload
	cmp	x8, #1
	b.le	LBB3_10
LBB3_4:                                 ; %L174.us.preheader
	str	x9, [sp, #8]                    ; 8-byte Folded Spill
	mov	w8, #1
	str	x8, [sp, #32]                   ; 8-byte Folded Spill
	mov	w8, #1
	str	x8, [sp, #24]                   ; 8-byte Folded Spill
LBB3_5:                                 ; %L174.us
                                        ; =>This Loop Header: Depth=1
                                        ;     Child Loop BB3_6 Depth 2
	mov	w9, #1
	ldr	x11, [sp, #16]                  ; 8-byte Folded Reload
LBB3_6:                                 ; %L180.us
                                        ;   Parent Loop BB3_5 Depth=1
                                        ; =>  This Inner Loop Header: Depth=2
	str	x9, [sp, #40]                   ; 8-byte Folded Spill
	str	x11, [sp, #48]                  ; 8-byte Folded Spill
	str	x10, [sp, #56]                  ; 8-byte Folded Spill
	mov	x0, x11
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp6:
	ldr	x10, [sp, #56]                  ; 8-byte Folded Reload
	ldur	x8, [x10, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x10, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #40]                   ; 8-byte Folded Reload
	add	x9, x9, x0
	ldr	x11, [sp, #48]                  ; 8-byte Folded Reload
	add	x8, x9, x8, lsl #1
	sub	x9, x8, #1
	sub	x11, x11, #2
	cmp	x11, #1
	b.hi	LBB3_6
; %bb.7:                                ; %L178.loopexit.us
                                        ;   in Loop: Header=BB3_5 Depth=1
	ldr	x8, [sp, #32]                   ; 8-byte Folded Reload
	add	x8, x8, x9
	sub	x0, x8, #1
	ldr	x9, [sp, #24]                   ; 8-byte Folded Reload
	add	x9, x9, #1
	ldr	x8, [sp, #8]                    ; 8-byte Folded Reload
	cmp	x9, x8
	b.gt	LBB3_12
; %bb.8:                                ;   in Loop: Header=BB3_5 Depth=1
	str	x9, [sp, #24]                   ; 8-byte Folded Spill
	str	x0, [sp, #32]                   ; 8-byte Folded Spill
	b	LBB3_5
LBB3_9:                                 ; %L214
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp7:
	lsr	x9, x1, #1
	ldr	x8, [sp, #16]                   ; 8-byte Folded Reload
	cmp	x8, #1
	b.gt	LBB3_4
LBB3_10:                                ; %L174.preheader
	mov	w0, #1
	mov	w8, #1
LBB3_11:                                ; %L174
                                        ; =>This Inner Loop Header: Depth=1
	add	x8, x8, #1
	cmp	x8, x9
	b.le	LBB3_11
LBB3_12:                                ; %common.ret
	ldr	x30, [sp, #72]                  ; 8-byte Folded Reload
	add	sp, sp, #80
	ret
	.loh AdrpAdd	Lloh15, Lloh16
	.cfi_endproc
                                        ; -- End function
	.globl	_camlLoop_invariant_gc_across_call__entry ; -- Begin function _camlLoop_invariant_gc_across_call__entry
	.p2align	2
_camlLoop_invariant_gc_across_call__entry: ; @"\01_camlLoop_invariant_gc_across_call__entry"
	.cfi_startproc
; %bb.0:                                ; %L1
	sub	sp, sp, #80
	.cfi_def_cfa_offset 80
	str	x30, [sp, #72]                  ; 8-byte Folded Spill
	.cfi_offset w30, -16
	ldr	x8, [x28, #40]
	add	x8, x8, #376
	cmp	sp, x8
	b.lo	LBB4_12
LBB4_1:                                 ; %L300
Lloh17:
	adrp	x11, _camlLoop_invariant_gc_across_call__immstring48@PAGE
Lloh18:
	add	x11, x11, _camlLoop_invariant_gc_across_call__immstring48@PAGEOFF
	; InlineAsm Start
	; InlineAsm End
	mov	w8, #1
	mov	w10, #13825
	movk	w10, #366, lsl #16
LBB4_2:                                 ; %L243
                                        ; =>This Inner Loop Header: Depth=1
	str	x8, [sp, #40]                   ; 8-byte Folded Spill
	str	x10, [sp, #48]                  ; 8-byte Folded Spill
	str	x11, [sp, #56]                  ; 8-byte Folded Spill
	mov	x0, x10
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp8:
	ldr	x11, [sp, #56]                  ; 8-byte Folded Reload
	ldur	x8, [x11, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x11, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #40]                   ; 8-byte Folded Reload
	add	x9, x9, x0
	ldr	x10, [sp, #48]                  ; 8-byte Folded Reload
	add	x8, x9, x8, lsl #1
	sub	x8, x8, #1
	sub	x10, x10, #2
	cmp	x10, #1
	b.hi	LBB4_2
; %bb.3:                                ; %L243.1.preheader
	str	x8, [sp, #40]                   ; 8-byte Folded Spill
	mov	w8, #1
	mov	w10, #13825
	movk	w10, #366, lsl #16
LBB4_4:                                 ; %L243.1
                                        ; =>This Inner Loop Header: Depth=1
	str	x8, [sp, #32]                   ; 8-byte Folded Spill
	str	x10, [sp, #48]                  ; 8-byte Folded Spill
	str	x11, [sp, #56]                  ; 8-byte Folded Spill
	mov	x0, x10
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp9:
	ldr	x11, [sp, #56]                  ; 8-byte Folded Reload
	ldur	x8, [x11, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x11, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #32]                   ; 8-byte Folded Reload
	add	x9, x9, x0
	ldr	x10, [sp, #48]                  ; 8-byte Folded Reload
	add	x8, x9, x8, lsl #1
	sub	x8, x8, #1
	sub	x10, x10, #2
	cmp	x10, #1
	b.hi	LBB4_4
; %bb.5:                                ; %L243.2.preheader
	str	x8, [sp, #32]                   ; 8-byte Folded Spill
	mov	w8, #1
	mov	w10, #13825
	movk	w10, #366, lsl #16
LBB4_6:                                 ; %L243.2
                                        ; =>This Inner Loop Header: Depth=1
	str	x8, [sp, #24]                   ; 8-byte Folded Spill
	str	x10, [sp, #48]                  ; 8-byte Folded Spill
	str	x11, [sp, #56]                  ; 8-byte Folded Spill
	mov	x0, x10
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp10:
	ldr	x11, [sp, #56]                  ; 8-byte Folded Reload
	ldur	x8, [x11, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x11, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #24]                   ; 8-byte Folded Reload
	add	x9, x9, x0
	ldr	x10, [sp, #48]                  ; 8-byte Folded Reload
	add	x8, x9, x8, lsl #1
	sub	x8, x8, #1
	sub	x10, x10, #2
	cmp	x10, #1
	b.hi	LBB4_6
; %bb.7:                                ; %L243.3.preheader
	str	x8, [sp, #24]                   ; 8-byte Folded Spill
	mov	w8, #1
	mov	w10, #13825
	movk	w10, #366, lsl #16
LBB4_8:                                 ; %L243.3
                                        ; =>This Inner Loop Header: Depth=1
	str	x8, [sp, #16]                   ; 8-byte Folded Spill
	str	x10, [sp, #48]                  ; 8-byte Folded Spill
	str	x11, [sp, #56]                  ; 8-byte Folded Spill
	mov	x0, x10
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp11:
	ldr	x11, [sp, #56]                  ; 8-byte Folded Reload
	ldur	x8, [x11, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x11, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #16]                   ; 8-byte Folded Reload
	add	x9, x9, x0
	ldr	x10, [sp, #48]                  ; 8-byte Folded Reload
	add	x8, x9, x8, lsl #1
	sub	x8, x8, #1
	sub	x10, x10, #2
	cmp	x10, #1
	b.hi	LBB4_8
; %bb.9:                                ; %L243.4.preheader
	str	x8, [sp, #16]                   ; 8-byte Folded Spill
	mov	w12, #1
	mov	w10, #13825
	movk	w10, #366, lsl #16
LBB4_10:                                ; %L243.4
                                        ; =>This Inner Loop Header: Depth=1
	str	x12, [sp, #8]                   ; 8-byte Folded Spill
	str	x11, [sp, #56]                  ; 8-byte Folded Spill
	str	x10, [sp, #48]                  ; 8-byte Folded Spill
	mov	x0, x10
	bl	_camlLoop_invariant_gc_across_call__tick_0_4_code ; 8-byte Folded Reload
Ltmp12:
	ldr	x11, [sp, #56]                  ; 8-byte Folded Reload
	ldur	x8, [x11, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x11, x8]
	sub	x8, x8, x9
	ldr	x9, [sp, #8]                    ; 8-byte Folded Reload
	add	x9, x9, x0
	ldr	x10, [sp, #48]                  ; 8-byte Folded Reload
	add	x8, x9, x8, lsl #1
	sub	x12, x8, #1
	sub	x10, x10, #2
	cmp	x10, #1
	b.hi	LBB4_10
; %bb.11:                               ; %L241.4
	ldr	x8, [sp, #24]                   ; 8-byte Folded Reload
	ldr	x9, [sp, #16]                   ; 8-byte Folded Reload
	add	x8, x9, x8
	ldr	x9, [sp, #40]                   ; 8-byte Folded Reload
	ldr	x10, [sp, #32]                  ; 8-byte Folded Reload
	add	x9, x10, x9
	add	x8, x8, x9
	add	x8, x12, x8
	sub	x1, x8, #4
Lloh19:
	adrp	x8, _caml_format_int@GOTPAGE
Lloh20:
	ldr	x8, [x8, _caml_format_int@GOTPAGEOFF]
Lloh21:
	adrp	x0, _camlStdlib__immstring191@GOTPAGE
Lloh22:
	ldr	x0, [x0, _camlStdlib__immstring191@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp13:
	mov	x1, x0
	ldur	x8, [x0, #-8]
	lsr	x8, x8, #7
	and	x8, x8, #0x1fffffffffff8
	sub	x8, x8, #1
	ldrb	w9, [x0, x8]
	sub	x8, x8, x9
	mov	w9, #1
	orr	x3, x9, x8, lsl #1
Lloh23:
	adrp	x19, _camlStdlib__print_int_136@GOTPAGE
Lloh24:
	ldr	x19, [x19, _camlStdlib__print_int_136@GOTPAGEOFF]
	ldr	x0, [x19, #16]
Lloh25:
	adrp	x8, _caml_ml_output@GOTPAGE
Lloh26:
	ldr	x8, [x8, _caml_ml_output@GOTPAGEOFF]
	mov	w2, #1
	bl	_caml_c_call
Ltmp14:
	ldr	x0, [x19, #16]
Lloh27:
	adrp	x8, _caml_ml_output_char@GOTPAGE
Lloh28:
	ldr	x8, [x8, _caml_ml_output_char@GOTPAGEOFF]
	mov	w1, #21
	bl	_caml_c_call
Ltmp15:
	ldr	x0, [x19, #16]
Lloh29:
	adrp	x8, _caml_ml_flush@GOTPAGE
Lloh30:
	ldr	x8, [x8, _caml_ml_flush@GOTPAGEOFF]
	bl	_caml_c_call
Ltmp16:
	mov	w0, #1
	ldr	x30, [sp, #72]                  ; 8-byte Folded Reload
	add	sp, sp, #80
	ret
LBB4_12:                                ; %L299
	mov	w0, #34
	bl	_caml_llvm_call_realloc_stack
Ltmp17:
	b	LBB4_1
	.loh AdrpAdd	Lloh17, Lloh18
	.loh AdrpLdrGot	Lloh29, Lloh30
	.loh AdrpLdrGot	Lloh27, Lloh28
	.loh AdrpLdrGot	Lloh25, Lloh26
	.loh AdrpLdrGot	Lloh23, Lloh24
	.loh AdrpLdrGot	Lloh21, Lloh22
	.loh AdrpLdrGot	Lloh19, Lloh20
	.cfi_endproc
                                        ; -- End function
	.section	__DATA,__data
	.globl	_camlLoop_invariant_gc_across_call__gc_roots ; @"\01_camlLoop_invariant_gc_across_call__gc_roots"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call__gc_roots:
	.space	8

	.globl	_header.camlLoop_invariant_gc_across_call ; @"\01_header.camlLoop_invariant_gc_across_call"
	.p2align	3, 0x0
_header.camlLoop_invariant_gc_across_call:
	.quad	4864                            ; 0x1300

	.globl	_camlLoop_invariant_gc_across_call ; @"\01_camlLoop_invariant_gc_across_call"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call:
	.quad	_camlLoop_invariant_gc_across_call__tick_4
	.quad	_camlLoop_invariant_gc_across_call__print_result_5
	.quad	_camlLoop_invariant_gc_across_call__loop_6
	.quad	_camlLoop_invariant_gc_across_call__run_7

	.globl	_header.camlLoop_invariant_gc_across_call__run_7 ; @"\01_header.camlLoop_invariant_gc_across_call__run_7"
	.p2align	3, 0x0
_header.camlLoop_invariant_gc_across_call__run_7:
	.quad	4087                            ; 0xff7

	.globl	_camlLoop_invariant_gc_across_call__run_7 ; @"\01_camlLoop_invariant_gc_across_call__run_7"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call__run_7:
	.quad	_caml_curry2
	.quad	180143985094819847              ; 0x280000000000007
	.quad	_camlLoop_invariant_gc_across_call__run_3_7_code

	.globl	_header.camlLoop_invariant_gc_across_call__loop_6 ; @"\01_header.camlLoop_invariant_gc_across_call__loop_6"
	.p2align	3, 0x0
_header.camlLoop_invariant_gc_across_call__loop_6:
	.quad	4087                            ; 0xff7

	.globl	_camlLoop_invariant_gc_across_call__loop_6 ; @"\01_camlLoop_invariant_gc_across_call__loop_6"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call__loop_6:
	.quad	_caml_curry3
	.quad	252201579132747783              ; 0x380000000000007
	.quad	_camlLoop_invariant_gc_across_call__loop_2_6_code

	.globl	_header.camlLoop_invariant_gc_across_call__print_result_5 ; @"\01_header.camlLoop_invariant_gc_across_call__print_result_5"
	.p2align	3, 0x0
_header.camlLoop_invariant_gc_across_call__print_result_5:
	.quad	3063                            ; 0xbf7

	.globl	_camlLoop_invariant_gc_across_call__print_result_5 ; @"\01_camlLoop_invariant_gc_across_call__print_result_5"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call__print_result_5:
	.quad	_camlLoop_invariant_gc_across_call__print_result_1_5_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlLoop_invariant_gc_across_call__tick_4 ; @"\01_header.camlLoop_invariant_gc_across_call__tick_4"
	.p2align	3, 0x0
_header.camlLoop_invariant_gc_across_call__tick_4:
	.quad	3063                            ; 0xbf7

	.globl	_camlLoop_invariant_gc_across_call__tick_4 ; @"\01_camlLoop_invariant_gc_across_call__tick_4"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call__tick_4:
	.quad	_camlLoop_invariant_gc_across_call__tick_0_4_code
	.quad	108086391056891909              ; 0x180000000000005

	.globl	_header.camlLoop_invariant_gc_across_call__immstring48 ; @"\01_header.camlLoop_invariant_gc_across_call__immstring48"
	.p2align	3, 0x0
_header.camlLoop_invariant_gc_across_call__immstring48:
	.quad	4092                            ; 0xffc

	.globl	_camlLoop_invariant_gc_across_call__immstring48 ; @"\01_camlLoop_invariant_gc_across_call__immstring48"
	.p2align	3, 0x0
_camlLoop_invariant_gc_across_call__immstring48:
	.ascii	"loop_invariant_payload"
	.space	1
	.byte	1                               ; 0x1

	.quad	0
	.globl	_camlLoop_invariant_gc_across_call__data_end
_camlLoop_invariant_gc_across_call__data_end:
	.quad	0
	.p2align	3, 0x0
	.globl	_camlLoop_invariant_gc_across_call__frametable
_camlLoop_invariant_gc_across_call__frametable:
	.quad	18
Ltmp18:
	.long	Ltmp0-Ltmp18
	.short	17
	.short	0
	.p2align	2, 0x0
Ltmp20:
	.long	Ltmp19-Ltmp20
	.p2align	3, 0x0
Ltmp21:
	.long	Ltmp1-Ltmp21
	.short	17
	.short	0
	.p2align	2, 0x0
Ltmp23:
	.long	Ltmp22-Ltmp23
	.p2align	3, 0x0
Ltmp24:
	.long	Ltmp2-Ltmp24
	.short	17
	.short	0
	.p2align	2, 0x0
Ltmp26:
	.long	Ltmp25-Ltmp26
	.p2align	3, 0x0
Ltmp27:
	.long	Ltmp3-Ltmp27
	.short	17
	.short	0
	.p2align	2, 0x0
Ltmp29:
	.long	Ltmp28-Ltmp29
	.p2align	3, 0x0
Ltmp30:
	.long	Ltmp4-Ltmp30
	.short	49
	.short	1
	.short	24
	.p2align	2, 0x0
Ltmp32:
	.long	Ltmp31-Ltmp32
	.p2align	3, 0x0
Ltmp33:
	.long	Ltmp5-Ltmp33
	.short	48
	.short	1
	.short	21
	.p2align	3, 0x0
Ltmp34:
	.long	Ltmp6-Ltmp34
	.short	81
	.short	1
	.short	56
	.p2align	2, 0x0
Ltmp36:
	.long	Ltmp35-Ltmp36
	.p2align	3, 0x0
Ltmp37:
	.long	Ltmp7-Ltmp37
	.short	80
	.short	1
	.short	21
	.p2align	3, 0x0
Ltmp38:
	.long	Ltmp8-Ltmp38
	.short	81
	.short	1
	.short	56
	.p2align	2, 0x0
Ltmp40:
	.long	Ltmp39-Ltmp40
	.p2align	3, 0x0
Ltmp41:
	.long	Ltmp9-Ltmp41
	.short	81
	.short	1
	.short	56
	.p2align	2, 0x0
Ltmp42:
	.long	Ltmp39-Ltmp42
	.p2align	3, 0x0
Ltmp43:
	.long	Ltmp10-Ltmp43
	.short	81
	.short	1
	.short	56
	.p2align	2, 0x0
Ltmp44:
	.long	Ltmp39-Ltmp44
	.p2align	3, 0x0
Ltmp45:
	.long	Ltmp11-Ltmp45
	.short	81
	.short	1
	.short	56
	.p2align	2, 0x0
Ltmp46:
	.long	Ltmp39-Ltmp46
	.p2align	3, 0x0
Ltmp47:
	.long	Ltmp12-Ltmp47
	.short	81
	.short	1
	.short	56
	.p2align	2, 0x0
Ltmp48:
	.long	Ltmp39-Ltmp48
	.p2align	3, 0x0
Ltmp49:
	.long	Ltmp13-Ltmp49
	.short	81
	.short	0
	.p2align	2, 0x0
Ltmp51:
	.long	Ltmp50-Ltmp51
	.p2align	3, 0x0
Ltmp52:
	.long	Ltmp14-Ltmp52
	.short	81
	.short	0
	.p2align	2, 0x0
Ltmp54:
	.long	Ltmp53-Ltmp54
	.p2align	3, 0x0
Ltmp55:
	.long	Ltmp15-Ltmp55
	.short	81
	.short	0
	.p2align	2, 0x0
Ltmp57:
	.long	Ltmp56-Ltmp57
	.p2align	3, 0x0
Ltmp58:
	.long	Ltmp16-Ltmp58
	.short	81
	.short	0
	.p2align	2, 0x0
Ltmp60:
	.long	Ltmp59-Ltmp60
	.p2align	3, 0x0
Ltmp61:
	.long	Ltmp17-Ltmp61
	.short	80
	.short	0
	.p2align	3, 0x0
	.p2align	2, 0x0
Ltmp19:
Ltmp67:
	.long	(Ltmp62-Ltmp67)+1
	.long	146802840
Ltmp68:
	.long	(Ltmp64-Ltmp68)+1
	.long	265854400
Ltmp69:
	.long	(Ltmp65-Ltmp69)+0
	.long	2623592
	.p2align	2, 0x0
Ltmp22:
Ltmp71:
	.long	(Ltmp70-Ltmp71)+1
	.long	202901880
Ltmp72:
	.long	(Ltmp64-Ltmp72)+1
	.long	265832896
Ltmp73:
	.long	(Ltmp65-Ltmp73)+0
	.long	2623592
	.p2align	2, 0x0
Ltmp25:
Ltmp75:
	.long	(Ltmp74-Ltmp75)+1
	.long	267935088
Ltmp76:
	.long	(Ltmp65-Ltmp76)+0
	.long	3147920
	.p2align	2, 0x0
Ltmp28:
Ltmp77:
	.long	(Ltmp74-Ltmp77)+1
	.long	267960800
Ltmp78:
	.long	(Ltmp65-Ltmp78)+0
	.long	3147920
	.p2align	2, 0x0
Ltmp31:
Ltmp80:
	.long	(Ltmp79-Ltmp80)+0
	.long	6303888
	.p2align	2, 0x0
Ltmp35:
Ltmp82:
	.long	(Ltmp79-Ltmp82)+1
	.long	6303888
Ltmp83:
	.long	(Ltmp81-Ltmp83)+0
	.long	9980128
	.p2align	2, 0x0
Ltmp39:
Ltmp85:
	.long	(Ltmp79-Ltmp85)+1
	.long	6303888
Ltmp86:
	.long	(Ltmp81-Ltmp86)+1
	.long	9980128
Ltmp87:
	.long	(Ltmp84-Ltmp87)+0
	.long	12081456
	.p2align	2, 0x0
Ltmp50:
Ltmp88:
	.long	(Ltmp62-Ltmp88)+1
	.long	146802840
Ltmp89:
	.long	(Ltmp64-Ltmp89)+1
	.long	265854400
Ltmp90:
	.long	(Ltmp65-Ltmp90)+1
	.long	2623592
Ltmp91:
	.long	(Ltmp84-Ltmp91)+0
	.long	12068144
	.p2align	2, 0x0
Ltmp53:
Ltmp92:
	.long	(Ltmp70-Ltmp92)+1
	.long	202901880
Ltmp93:
	.long	(Ltmp64-Ltmp93)+1
	.long	265832896
Ltmp94:
	.long	(Ltmp65-Ltmp94)+1
	.long	2623592
Ltmp95:
	.long	(Ltmp84-Ltmp95)+0
	.long	12068144
	.p2align	2, 0x0
Ltmp56:
Ltmp96:
	.long	(Ltmp74-Ltmp96)+1
	.long	267935088
Ltmp97:
	.long	(Ltmp65-Ltmp97)+1
	.long	3147920
Ltmp98:
	.long	(Ltmp84-Ltmp98)+0
	.long	12068144
	.p2align	2, 0x0
Ltmp59:
Ltmp99:
	.long	(Ltmp74-Ltmp99)+1
	.long	267960800
Ltmp100:
	.long	(Ltmp65-Ltmp100)+1
	.long	3147920
Ltmp101:
	.long	(Ltmp84-Ltmp101)+0
	.long	12068144
	.p2align	2, 0x0
Ltmp62:
	.long	Ltmp63-Ltmp62
	.ascii	"Stdlib.string_of_int"
	.byte	0
	.p2align	2, 0x0
Ltmp64:
	.long	Ltmp63-Ltmp64
	.ascii	"Stdlib.print_int"
	.byte	0
	.p2align	2, 0x0
Ltmp65:
	.long	Ltmp66-Ltmp65
	.ascii	"Loop_invariant_gc_across_call.print_result"
	.byte	0
	.p2align	2, 0x0
Ltmp70:
	.long	Ltmp63-Ltmp70
	.ascii	"Stdlib.output_string"
	.byte	0
	.p2align	2, 0x0
Ltmp74:
	.long	Ltmp63-Ltmp74
	.ascii	"Stdlib.print_newline"
	.byte	0
	.p2align	2, 0x0
Ltmp79:
	.long	Ltmp66-Ltmp79
	.ascii	"Loop_invariant_gc_across_call.loop"
	.byte	0
	.p2align	2, 0x0
Ltmp81:
	.long	Ltmp66-Ltmp81
	.ascii	"Loop_invariant_gc_across_call.run"
	.byte	0
	.p2align	2, 0x0
Ltmp84:
	.long	Ltmp66-Ltmp84
	.ascii	"Loop_invariant_gc_across_call"
	.byte	0
Ltmp66:
	.ascii	"loop_invariant_gc_across_call.ml"
	.byte	0
Ltmp63:
	.ascii	"stdlib.ml"
	.byte	0

	.section	__TEXT,__text,regular,pure_instructions
	.globl	_camlLoop_invariant_gc_across_call__code_end
_camlLoop_invariant_gc_across_call__code_end:
.subsections_via_symbols
