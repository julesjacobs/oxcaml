; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_ccc { { i64, i64 }, {} } @caml_c_call(i64, i64, i64, ptr addrspace(1))
declare void @primitive()

define oxcaml_nofpcc { { i64, i64 }, {} } @c_call_arg_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @c_call_arg_root(
; CHECK: %obj.cargroot = alloca ptr addrspace(1), align 8
; CHECK: store ptr addrspace(1) %obj, ptr %obj.cargroot, align 8
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} [ "deopt"(), "gc-live"(ptr %obj.cargroot) ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %fn = ptrtoint ptr @primitive to i64
  %res = call oxcaml_ccc { { i64, i64 }, {} } @caml_c_call(
      i64 %ds, i64 %alloc, i64 %fn, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
  %ds2 = extractvalue { { i64, i64 }, {} } %res, 0, 0
  %alloc2 = extractvalue { { i64, i64 }, {} } %res, 0, 1
  %ret0 = insertvalue { { i64, i64 }, {} } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, {} } %ret0, i64 %alloc2, 0, 1
  ret { { i64, i64 }, {} } %ret1
}

define oxcaml_nofpcc { { i64, i64 }, {} } @derived_c_call_arg_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_c_call_arg_root(
; CHECK: %[[SLOT:.*cargroot]] = alloca ptr addrspace(1), align 8
; CHECK: %[[DERIVED:.*]] = getelementptr i8, ptr addrspace(1) %obj, i64 8
; CHECK: store ptr addrspace(1) %[[DERIVED]], ptr %[[SLOT]], align 8
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} [ "deopt"(), "gc-live"(ptr %[[SLOT]]) ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %derived = getelementptr i8, ptr addrspace(1) %obj, i64 8
  %fn = ptrtoint ptr @primitive to i64
  %res = call oxcaml_ccc { { i64, i64 }, {} } @caml_c_call(
      i64 %ds, i64 %alloc, i64 %fn, ptr addrspace(1) %derived)
      "statepoint-id"="0" [ "deopt"() ]
  %ds2 = extractvalue { { i64, i64 }, {} } %res, 0, 0
  %alloc2 = extractvalue { { i64, i64 }, {} } %res, 0, 1
  %ret0 = insertvalue { { i64, i64 }, {} } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, {} } %ret0, i64 %alloc2, 0, 1
  ret { { i64, i64 }, {} } %ret1
}
