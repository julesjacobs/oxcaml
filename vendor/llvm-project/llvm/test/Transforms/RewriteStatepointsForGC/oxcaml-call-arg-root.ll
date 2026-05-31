; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(i64, i64, ptr addrspace(1))

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @call_arg_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %arg) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @call_arg_root(
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @callee
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr addrspace(1) %arg) ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %arg)
      "statepoint-id"="0" [ "deopt"() ]
  ret { { i64, i64 }, { i64 } } %res
}

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @derived_call_arg_roots_base(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %base) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_call_arg_roots_base(
; CHECK: %[[DERIVED:.*]] = getelementptr i8, ptr addrspace(1) %base, i64 16
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @callee
; CHECK-SAME: ptr addrspace(1) %[[DERIVED]]
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr addrspace(1) %base) ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %derived = getelementptr i8, ptr addrspace(1) %base, i64 16
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %derived)
      "statepoint-id"="0" [ "deopt"() ]
  ret { { i64, i64 }, { i64 } } %res
}
