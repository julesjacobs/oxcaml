; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()

define ptr addrspace(1) @ordinary_promotable_slot(
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @ordinary_promotable_slot(
; CHECK-NOT: alloca ptr addrspace(1)
; CHECK: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: ret ptr addrspace(1) %obj.relocated
entry:
  %slot = alloca ptr addrspace(1)
  store ptr addrspace(1) %obj, ptr %slot
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %after = load ptr addrspace(1), ptr %slot
  ret ptr addrspace(1) %after
}

define void @explicit_root_slot_is_allowed(
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define void @explicit_root_slot_is_allowed(
; CHECK: %slot = alloca ptr addrspace(1), align 8
; CHECK: store ptr addrspace(1) %obj, ptr %slot, align 8
; CHECK: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr %slot) ]
; CHECK-NEXT: ret void
entry:
  %slot = alloca ptr addrspace(1)
  store ptr addrspace(1) %obj, ptr %slot
  call void @may_gc() "statepoint-id"="0" [ "deopt"(), "gc-live"(ptr %slot) ]
  ret void
}
