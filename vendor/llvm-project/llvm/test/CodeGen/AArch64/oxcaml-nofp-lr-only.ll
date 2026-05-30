; RUN: llc -mtriple=arm64-apple-macosx14.0.0 -verify-machineinstrs < %s | FileCheck %s

declare oxcaml_nofpcc i64 @callee(i64)

define oxcaml_nofpcc i64 @f(i64 %x) {
; CHECK-LABEL: _f:
; CHECK: sub sp, sp, #16
; CHECK: str x30, [sp, #8]
; CHECK: bl _callee
; CHECK: ldr x30, [sp, #8]
; CHECK: add sp, sp, #16
; CHECK: ret
entry:
  %y = call oxcaml_nofpcc i64 @callee(i64 %x)
  %z = add i64 %y, 2
  ret i64 %z
}
