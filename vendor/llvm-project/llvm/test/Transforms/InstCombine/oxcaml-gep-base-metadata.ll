; RUN: opt -passes=instcombine -S < %s | FileCheck %s

define ptr addrspace(1) @preserve_base_metadata(ptr addrspace(1) %base) {
; CHECK-LABEL: @preserve_base_metadata(
; CHECK-NEXT:    [[OBJ:%.*]] = getelementptr i8, ptr addrspace(1) [[BASE:%.*]], i64 40, !is_base_value !0
; CHECK-NEXT:    ret ptr addrspace(1) [[OBJ]]
;
  %sibling = getelementptr i8, ptr addrspace(1) %base, i64 56
  %obj = getelementptr i8, ptr addrspace(1) %sibling, i64 -16, !is_base_value !0
  ret ptr addrspace(1) %obj
}

define ptr addrspace(1) @do_not_fold_through_source_base_metadata(ptr addrspace(1) %base) {
; CHECK-LABEL: @do_not_fold_through_source_base_metadata(
; CHECK-NEXT:    [[OBJ:%.*]] = getelementptr i8, ptr addrspace(1) [[BASE:%.*]], i64 40, !is_base_value !0
; CHECK-NEXT:    [[FIELD:%.*]] = getelementptr i8, ptr addrspace(1) [[OBJ]], i64 8{{$}}
; CHECK-NEXT:    ret ptr addrspace(1) [[FIELD]]
;
  %obj = getelementptr i8, ptr addrspace(1) %base, i64 40, !is_base_value !0
  %field = getelementptr i8, ptr addrspace(1) %obj, i64 8
  ret ptr addrspace(1) %field
}

define ptr addrspace(1) @allow_nested_base_metadata(ptr addrspace(1) %base) {
; CHECK-LABEL: @allow_nested_base_metadata(
; CHECK-NEXT:    [[NESTED:%.*]] = getelementptr i8, ptr addrspace(1) [[BASE:%.*]], i64 48, !is_base_value !0
; CHECK-NEXT:    ret ptr addrspace(1) [[NESTED]]
;
  %obj = getelementptr i8, ptr addrspace(1) %base, i64 40, !is_base_value !0
  %nested = getelementptr i8, ptr addrspace(1) %obj, i64 8, !is_base_value !0
  ret ptr addrspace(1) %nested
}

!0 = !{}
