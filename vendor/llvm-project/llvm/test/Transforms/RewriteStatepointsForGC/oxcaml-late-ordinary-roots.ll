; RUN: opt -S -O2 < %s | opt -S -passes=rewrite-statepoints-for-gc,verify | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()
declare void @use(ptr addrspace(1)) "gc-leaf-function"

define ptr addrspace(1) @ordinary_call_live_value(
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @ordinary_call_live_value(
; CHECK: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: ret ptr addrspace(1) %obj.relocated
entry:
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  ret ptr addrspace(1) %obj
}

define ptr addrspace(1) @loop_carried_value_across_poll(
    ptr addrspace(1) %obj,
    i1 %again) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @loop_carried_value_across_poll(
; CHECK: loop:
; CHECK-NEXT: %.0 = phi ptr addrspace(1) [ %obj, %entry ], [ %obj.relocated, %loop ]
; CHECK-NEXT: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %.0) ]
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: br i1 %again, label %loop, label %exit
; CHECK: exit:
; CHECK-NEXT: ret ptr addrspace(1) %obj.relocated
entry:
  br label %loop

loop:
  %cur = phi ptr addrspace(1) [ %obj, %entry ], [ %cur, %loop ]
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  br i1 %again, label %loop, label %exit

exit:
  ret ptr addrspace(1) %cur
}

define void @value_live_only_to_call_after_safepoint(
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define void @value_live_only_to_call_after_safepoint(
; CHECK: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: tail call void @use(ptr addrspace(1) %obj.relocated)
; CHECK-NEXT: ret void
entry:
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  call void @use(ptr addrspace(1) %obj)
  ret void
}
