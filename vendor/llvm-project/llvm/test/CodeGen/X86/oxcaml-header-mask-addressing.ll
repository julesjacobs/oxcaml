; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu | FileCheck %s
; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -mcpu=znver1 | FileCheck %s --check-prefix=BEXTR

define i8 @ocaml_string_offset_mask(ptr %base, i64 %header) {
; CHECK-LABEL: ocaml_string_offset_mask:
; CHECK:       # %bb.0:
; CHECK-NEXT:    shlq $8, %rsi
; CHECK-NEXT:    shrq $18, %rsi
; CHECK-NEXT:    movzbl -1(%rdi,%rsi,8), %eax
; CHECK-NEXT:    retq
; BEXTR-LABEL: ocaml_string_offset_mask:
; BEXTR:       # %bb.0:
; BEXTR-NEXT:    movl $11786, %eax
; BEXTR-NEXT:    bextrq %rax, %rsi, %rax
; BEXTR-NEXT:    movzbl -1(%rdi,%rax,8), %eax
; BEXTR-NEXT:    retq
entry:
  %shifted = lshr i64 %header, 7
  %masked = and i64 %shifted, 562949953421304
  %idx = add i64 %masked, -1
  %p = getelementptr i8, ptr %base, i64 %idx
  %v = load i8, ptr %p, align 1
  ret i8 %v
}

define { i64, i8 } @ocaml_string_offset_mask_keeps_scaled_value(ptr %base, i64 %header) {
; CHECK-LABEL: ocaml_string_offset_mask_keeps_scaled_value:
; CHECK:       # %bb.0:
; CHECK-NEXT:    shlq $8, %rsi
; CHECK-NEXT:    shrq $18, %rsi
; CHECK-NEXT:    movzbl -1(%rdi,%rsi,8), %edx
; CHECK-NEXT:    leaq (,%rsi,8), %rax
; CHECK-NEXT:    retq
; BEXTR-LABEL: ocaml_string_offset_mask_keeps_scaled_value:
; BEXTR:       # %bb.0:
; BEXTR-NEXT:    shlq $8, %rsi
; BEXTR-NEXT:    shrq $18, %rsi
; BEXTR-NEXT:    movzbl -1(%rdi,%rsi,8), %edx
; BEXTR-NEXT:    leaq (,%rsi,8), %rax
; BEXTR-NEXT:    retq
entry:
  %shifted = lshr i64 %header, 7
  %masked = and i64 %shifted, 562949953421304
  %idx = add i64 %masked, -1
  %p = getelementptr i8, ptr %base, i64 %idx
  %v = load i8, ptr %p, align 1
  %pair0 = insertvalue { i64, i8 } poison, i64 %masked, 0
  %pair1 = insertvalue { i64, i8 } %pair0, i8 %v, 1
  ret { i64, i8 } %pair1
}
