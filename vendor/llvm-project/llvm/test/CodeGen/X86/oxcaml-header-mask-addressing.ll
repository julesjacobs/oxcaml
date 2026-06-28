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

define i64 @ocaml_string_offset_hoisted_mask(ptr addrspace(1) %base, i64 %n) {
; CHECK-LABEL: ocaml_string_offset_hoisted_mask:
; CHECK:       # %bb.0:
; CHECK-NEXT:    xorl %eax, %eax
; CHECK:       .LBB2_1:
; CHECK-NOT:     andq
; CHECK:         shlq $8, %rcx
; CHECK-NEXT:    shrq $18, %rcx
; CHECK-NEXT:    leaq -1(,%rcx,8), %rdx
; CHECK-NEXT:    movzbl -1(%rdi,%rcx,8), %ecx
; CHECK:         ja .LBB2_1
; BEXTR-LABEL: ocaml_string_offset_hoisted_mask:
; BEXTR:       # %bb.0:
; BEXTR-NEXT:    xorl %eax, %eax
; BEXTR-NEXT:    movl $11786, %ecx
; BEXTR:       .LBB2_1:
; BEXTR-NOT:     andq
; BEXTR:         bextrq %rcx, -8(%rdi), %rdx
; BEXTR:         leaq -1(,%rdx,8), %r8
; BEXTR-NEXT:    movzbl -1(%rdi,%rdx,8), %edx
; BEXTR:         ja .LBB2_1
entry:
  %const = bitcast i64 562949953421304 to i64
  br label %loop

loop:
  %i = phi i64 [ %n, %entry ], [ %next, %loop ]
  %acc = phi i64 [ 0, %entry ], [ %sum, %loop ]
  %hptr = getelementptr i8, ptr addrspace(1) %base, i64 -8
  %h = load i64, ptr addrspace(1) %hptr, align 4
  %s = lshr i64 %h, 7
  %m = and i64 %s, %const
  %idx = add nsw i64 %m, -1
  %p = getelementptr i8, ptr addrspace(1) %base, i64 %idx
  %b = load i8, ptr addrspace(1) %p, align 1
  %z = zext i8 %b to i64
  %len = sub nsw i64 %idx, %z
  %sum = add i64 %acc, %len
  %next = add i64 %i, -1
  %keep = icmp ugt i64 %next, 1
  br i1 %keep, label %loop, label %exit

exit:
  ret i64 %sum
}

define i8 @hoisted_mask_non_shift_address(ptr %base, i64 %x, i64 %n) {
; CHECK-LABEL: hoisted_mask_non_shift_address:
; CHECK:       # %bb.0:
; CHECK:         movabsq $562949953421304
; CHECK:         andq
; CHECK:       .LBB3_1:
; CHECK:         addb (%rdi,
; CHECK:         ja .LBB3_1
; BEXTR-LABEL: hoisted_mask_non_shift_address:
; BEXTR:       # %bb.0:
; BEXTR:         movabsq $562949953421304
; BEXTR:         andq
; BEXTR:       .LBB3_1:
; BEXTR:         addb (%rdi,
; BEXTR:         ja .LBB3_1
entry:
  %const = bitcast i64 562949953421304 to i64
  br label %loop

loop:
  %i = phi i64 [ %n, %entry ], [ %next, %loop ]
  %acc = phi i8 [ 0, %entry ], [ %sum, %loop ]
  %m = and i64 %x, %const
  %p = getelementptr i8, ptr %base, i64 %m
  %b = load i8, ptr %p, align 1
  %sum = add i8 %acc, %b
  %next = add i64 %i, -1
  %keep = icmp ugt i64 %next, 1
  br i1 %keep, label %loop, label %exit

exit:
  ret i8 %sum
}

define i8 @ocaml_hoisted_mask_before_shift(ptr %base, i64 %header, i64 %n) {
; CHECK-LABEL: ocaml_hoisted_mask_before_shift:
; CHECK:       # %bb.0:
; CHECK-NEXT:    shlq $8, %rsi
; CHECK-NEXT:    shrq $18, %rsi
; CHECK-NOT:     andq
; CHECK:         movzbl -1(%rdi,%rsi,8), %eax
; BEXTR-LABEL: ocaml_hoisted_mask_before_shift:
; BEXTR:       # %bb.0:
; BEXTR-NEXT:    shlq $8, %rsi
; BEXTR-NEXT:    shrq $18, %rsi
; BEXTR-NOT:     andq
; BEXTR:         movzbl -1(%rdi,%rsi,8), %eax
entry:
  %const = bitcast i64 72057594037926912 to i64
  br label %loop

loop:
  %i = phi i64 [ %n, %entry ], [ %next, %loop ]
  %masked = and i64 %header, %const
  %idx0 = lshr i64 %masked, 7
  %idx = add i64 %idx0, -1
  %p = getelementptr i8, ptr %base, i64 %idx
  %b = load i8, ptr %p, align 1
  %next = add i64 %i, -1
  %keep = icmp ugt i64 %next, 1
  br i1 %keep, label %loop, label %exit

exit:
  ret i8 %b
}
