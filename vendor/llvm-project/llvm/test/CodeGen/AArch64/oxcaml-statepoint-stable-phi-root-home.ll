; The stable-root-home machinery exists only for the ISel pool-spilling
; scheme; in-place statepoint lowering (the default) bypasses it. Pin to
; the slot scheme while it remains supported.
; RUN: llc -mtriple=arm64-apple-macosx -oxcaml-statepoint-inplace=0 -oxcaml-statepoint-inplace-calls=0 -verify-machineinstrs -stop-before=greedy < %s | FileCheck %s

@callee = external global ptr

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr,
                                                  i32 immarg, i32 immarg, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare void @use(ptr addrspace(1))

define void @stable_phi_root_home(ptr addrspace(1) %seed, i64 %n) gc "oxcaml" {
entry:
  br label %loop

loop:
  %root = phi ptr addrspace(1) [ %seed, %entry ], [ %root.rel2, %loop ]
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  %fn = load ptr, ptr @callee
  %tok1 = call token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0, ptr elementtype(void ()) %fn,
          i32 0, i32 0,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root) ]
  %root.rel = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok1, i32 0, i32 0)
  %tok2 = call token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 2, i32 0, ptr elementtype(void (ptr addrspace(1))) @use,
          i32 1, i32 0,
          ptr addrspace(1) %root.rel,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root.rel) ]
  %root.rel2 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok2, i32 0, i32 0)
  %i.next = add nuw i64 %i, 1
  %done = icmp eq i64 %i.next, %n
  br i1 %done, label %exit, label %loop

exit:
  ret void
}

; CHECK-LABEL: name: stable_phi_root_home
; CHECK-LABEL: bb.0.entry:
; CHECK: STRXui {{.*}} %stack.[[SLOT:[0-9]+]]
; CHECK-LABEL: bb.1.loop:
; CHECK-NOT: STRXui {{.*}} %stack.[[SLOT]]
; CHECK: STATEPOINT {{.*}} %stack.[[SLOT]], 0
; CHECK-NOT: STRXui {{.*}} %stack.[[SLOT]]
; CHECK: STATEPOINT {{.*}} %stack.[[SLOT]], 0
