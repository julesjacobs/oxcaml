; RUN: llc < %s -mtriple=x86_64-linux-gnu | FileCheck %s

@caml_call_gc = external global ptr

define oxcaml_fpcc { { i64, i64 }, {} } @camlPoll__poll(i64 %domain, i64 %alloc) gc "oxcaml" {
  %token = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 1, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @caml_call_gc, i32 2, i32 0, i64 %domain, i64 %alloc, i32 0, i32 0)
  %result = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %token)
  ret { { i64, i64 }, {} } %result
}

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)
declare { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token)

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"Poll"}

; CHECK-LABEL: camlPoll__frametable:
; CHECK-NEXT:  .quad 1
; CHECK:       .byte 0
