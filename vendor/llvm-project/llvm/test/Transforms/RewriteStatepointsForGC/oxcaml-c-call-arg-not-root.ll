; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_ccc { { i64, i64 }, {} } @caml_c_call(i64, i64, i64, ptr addrspace(1))
declare void @primitive()

define oxcaml_nofpcc { { i64, i64 }, {} } @c_call_arg_root_not_relocated(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @c_call_arg_root_not_relocated(
; CHECK-NOT: cargroot
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
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

define oxcaml_nofpcc { { i64, i64 }, {} } @derived_c_call_arg_root_not_relocated(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_c_call_arg_root_not_relocated(
; CHECK-NOT: cargroot
; CHECK: %[[DERIVED:.*]] = getelementptr i8, ptr addrspace(1) %obj, i64 8
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
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

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @c_call_arg_live_after(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @c_call_arg_live_after(
; CHECK-NOT: cargroot
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: insertvalue { { i64, i64 }, { ptr addrspace(1) } } %{{.*}}, ptr addrspace(1) %obj.relocated, 1, 0
; CHECK: ret
entry:
  %fn = ptrtoint ptr @primitive to i64
  %res = call oxcaml_ccc { { i64, i64 }, {} } @caml_c_call(
      i64 %ds, i64 %alloc, i64 %fn, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
  %ds2 = extractvalue { { i64, i64 }, {} } %res, 0, 0
  %alloc2 = extractvalue { { i64, i64 }, {} } %res, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %obj, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @derived_c_call_arg_live_after(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_c_call_arg_live_after(
; CHECK-NOT: cargroot
; CHECK: %[[DERIVED_ARG:.*]] = getelementptr i8, ptr addrspace(1) %obj, i64 8
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} ptr addrspace(1) %[[DERIVED_ARG]]{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: %derived.remat = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 8
; CHECK: insertvalue { { i64, i64 }, { ptr addrspace(1) } } %{{.*}}, ptr addrspace(1) %derived.remat, 1, 0
; CHECK: ret
entry:
  %derived = getelementptr i8, ptr addrspace(1) %obj, i64 8
  %fn = ptrtoint ptr @primitive to i64
  %res = call oxcaml_ccc { { i64, i64 }, {} } @caml_c_call(
      i64 %ds, i64 %alloc, i64 %fn, ptr addrspace(1) %derived)
      "statepoint-id"="0" [ "deopt"() ]
  %ds2 = extractvalue { { i64, i64 }, {} } %res, 0, 0
  %alloc2 = extractvalue { { i64, i64 }, {} } %res, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %derived, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
