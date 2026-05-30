; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(i64, i64)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @addr_after_alloc(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @addr_after_alloc(
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: %field.addr.remat = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 24
entry:
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 24
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
