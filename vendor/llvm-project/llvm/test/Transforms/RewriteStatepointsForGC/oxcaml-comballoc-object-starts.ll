; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128-p1:64:64"

declare void @foo()

define ptr addrspace(1) @two_combined_object_starts(ptr addrspace(1) %alloc, i1 %c) gc "statepoint-example" {
; CHECK-LABEL: define ptr addrspace(1) @two_combined_object_starts
; CHECK-SAME: (ptr addrspace(1) [[ALLOC:%.*]], i1 [[C:%.*]]) gc "statepoint-example" {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[OBJ1:%.*]] = getelementptr i8, ptr addrspace(1) [[ALLOC]], i64 16, !is_base_value !0
; CHECK-NEXT:    [[OBJ2:%.*]] = getelementptr i8, ptr addrspace(1) [[ALLOC]], i64 40, !is_base_value !0
; CHECK-NEXT:    [[STATEPOINT_TOKEN:%.*]] = call token {{.*}} @llvm.experimental.gc.statepoint.p0({{.*}}) [ "deopt"(), "gc-live"(ptr addrspace(1) [[OBJ1]], ptr addrspace(1) [[OBJ2]]) ]
; CHECK-NEXT:    [[OBJ1_RELOCATED:%.*]] = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token [[STATEPOINT_TOKEN]], i32 0, i32 0)
; CHECK-NEXT:    [[OBJ2_RELOCATED:%.*]] = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token [[STATEPOINT_TOKEN]], i32 1, i32 1)
; CHECK-NEXT:    [[RET:%.*]] = select i1 [[C]], ptr addrspace(1) [[OBJ1_RELOCATED]], ptr addrspace(1) [[OBJ2_RELOCATED]]
; CHECK-NEXT:    ret ptr addrspace(1) [[RET]]
;
entry:
  %obj1 = getelementptr i8, ptr addrspace(1) %alloc, i64 16, !is_base_value !0
  %obj2 = getelementptr i8, ptr addrspace(1) %alloc, i64 40, !is_base_value !0
  call void @foo() [ "deopt"() ]
  %ret = select i1 %c, ptr addrspace(1) %obj1, ptr addrspace(1) %obj2
  ret ptr addrspace(1) %ret
}

define ptr addrspace(1) @one_object_start_one_derived_field(ptr addrspace(1) %alloc) gc "statepoint-example" {
; CHECK-LABEL: define ptr addrspace(1) @one_object_start_one_derived_field
; CHECK-SAME: (ptr addrspace(1) [[ALLOC:%.*]]) gc "statepoint-example" {
; CHECK-NEXT:  entry:
; The field is rematerialized from its own object start's relocation: the
; remat chain stops at the is_base_value GEP (the object start), never
; walking through it to a different object's relocation.
; CHECK-NEXT:    [[OBJ:%.*]] = getelementptr i8, ptr addrspace(1) [[ALLOC]], i64 16, !is_base_value !0
; CHECK-NEXT:    [[FIELD:%.*]] = getelementptr i8, ptr addrspace(1) [[OBJ]], i64 8
; CHECK-NEXT:    [[STATEPOINT_TOKEN:%.*]] = call token {{.*}} @llvm.experimental.gc.statepoint.p0({{.*}}) [ "deopt"(), "gc-live"(ptr addrspace(1) [[OBJ]]) ]
; CHECK-NEXT:    [[OBJ_RELOCATED:%.*]] = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token [[STATEPOINT_TOKEN]], i32 0, i32 0)
; CHECK-NEXT:    [[FIELD_REMAT:%.*]] = getelementptr i8, ptr addrspace(1) [[OBJ_RELOCATED]], i64 8
; CHECK-NEXT:    ret ptr addrspace(1) [[FIELD_REMAT]]
;
entry:
  %obj = getelementptr i8, ptr addrspace(1) %alloc, i64 16, !is_base_value !0
  %field = getelementptr i8, ptr addrspace(1) %obj, i64 8
  call void @foo() [ "deopt"() ]
  ret ptr addrspace(1) %field
}

!0 = !{}
