; RUN: llc < %s -mtriple=aarch64-apple-darwin -oxcaml-callee-saved-x19-x25 -stop-after=finalize-isel 2>&1 | FileCheck %s

declare oxcaml_fpcc i64 @callee(i64)

define oxcaml_fpcc i64 @custom_abi_call(i64 %a, i64 %b) gc "ocaml" {
entry:
  %x = add i64 %a, %b
  %r = call oxcaml_fpcc i64 @callee(i64 %x)
  %z = add i64 %x, %r
  ret i64 %z
}

define oxcaml_fpcc i64 @old_abi_call(i64 %a, i64 %b) gc "ocaml" {
entry:
  %x = add i64 %a, %b
  %r = call oxcaml_fpcc i64 @callee(i64 %x) "oxcaml-old-abi"="true"
  %z = add i64 %x, %r
  ret i64 %z
}

; CHECK-LABEL: name: custom_abi_call
; CHECK: BL @callee, CustomRegMask(
; CHECK-SAME: $x19
; CHECK-SAME: $x25

; CHECK-LABEL: name: old_abi_call
; CHECK: BL @callee, csr_aarch64_oxcaml_withfp
; CHECK-NOT: CustomRegMask
