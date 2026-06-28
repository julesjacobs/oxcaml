; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-canonicalize-oxcaml-raw-heap-addresses < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()

define ptr addrspace(1) @raw_heap_address_after_statepoint(
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @raw_heap_address_after_statepoint(
; CHECK: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: %[[FIELD_ADDR:.*]] = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 24
; CHECK-NEXT: %field = load ptr addrspace(1), ptr addrspace(1) %[[FIELD_ADDR]], align 8
; CHECK-NEXT: ret ptr addrspace(1) %field
entry:
  %field.addr.as1 = getelementptr i8, ptr addrspace(1) %obj, i64 24
  %field.addr.int = ptrtoint ptr addrspace(1) %field.addr.as1 to i64
  %field.addr.raw = inttoptr i64 %field.addr.int to ptr
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %field = load ptr addrspace(1), ptr %field.addr.raw, align 8
  ret ptr addrspace(1) %field
}

define void @loop_carried_raw_heap_address(
    ptr addrspace(1) %obj,
    i1 %again) gc "oxcaml" {
; CHECK-LABEL: define void @loop_carried_raw_heap_address(
; CHECK: loop:
; CHECK-NEXT: %.0 = phi ptr addrspace(1) [ %obj, %entry ], [ %obj.relocated, %loop ]
; CHECK-NEXT: %[[FIELD_ADDR:.*]] = getelementptr i8, ptr addrspace(1) %.0, i64 24
; CHECK-NEXT: %field = load ptr addrspace(1), ptr addrspace(1) %[[FIELD_ADDR]], align 8
; CHECK-NEXT: %statepoint_token = call {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %.0) ]
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: br i1 %again, label %loop, label %exit
entry:
  %field.addr.as1 = getelementptr i8, ptr addrspace(1) %obj, i64 24
  %field.addr.raw = addrspacecast ptr addrspace(1) %field.addr.as1 to ptr
  br label %loop

loop:
  %field = load ptr addrspace(1), ptr %field.addr.raw, align 8
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  br i1 %again, label %loop, label %exit

exit:
  ret void
}
