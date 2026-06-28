; RUN: llc -mtriple=arm64-apple-macosx < %s | FileCheck %s

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"Test"}

declare void @callee(ptr addrspace(1))
declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr,
                                                  i32 immarg, i32 immarg, ...)

define void @call_arg_root(ptr addrspace(1) %arg) gc "oxcaml" {
entry:
  %token = call token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0,
          ptr elementtype(void (ptr addrspace(1))) @callee,
          i32 1, i32 0,
          ptr addrspace(1) %arg,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %arg) ]
  ret void
}

; CHECK-LABEL: _camlTest__frametable:
; CHECK:      .quad 1
; CHECK:      .long Ltmp{{[0-9]+}}-Ltmp{{[0-9]+}}
; CHECK-NEXT: .short {{[0-9]+}}
; CHECK-NEXT: .short 1
; CHECK-NEXT: .short {{[0-9]+}}
