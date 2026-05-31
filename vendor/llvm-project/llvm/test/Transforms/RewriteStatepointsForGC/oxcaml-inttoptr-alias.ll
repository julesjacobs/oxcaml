; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
    i64, i64, ptr addrspace(1), ptr addrspace(1), ptr addrspace(1))

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @dominating_inttoptr_alias(
    i64 %ds,
    i64 %alloc,
    i64 %raw,
    ptr addrspace(1) %arg,
    ptr addrspace(1) %clos) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @dominating_inttoptr_alias(
; CHECK: %obj0 = inttoptr i64 %raw to ptr addrspace(1)
; CHECK-NOT: %obj1 = inttoptr i64 %raw to ptr addrspace(1)
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @callee
; CHECK-SAME: ptr addrspace(1) %arg, ptr addrspace(1) %obj0, ptr addrspace(1) %clos
; CHECK-SAME: [ "deopt"(), "gc-live"(
; CHECK: %obj0.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1
; CHECK: %statepoint_token{{[0-9]*}} = call oxcaml_nofpcc {{.*}} @callee
; CHECK-SAME: ptr addrspace(1) %arg.relocated, ptr addrspace(1) %obj0.relocated, ptr addrspace(1) %clos.relocated
entry:
  %obj0 = inttoptr i64 %raw to ptr addrspace(1)
  %res0 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %arg, ptr addrspace(1) %obj0,
      ptr addrspace(1) %clos) "statepoint-id"="0" [ "deopt"() ]
  %pair0 = extractvalue { { i64, i64 }, { i64 } } %res0, 0
  %ds1 = extractvalue { i64, i64 } %pair0, 0
  %alloc1 = extractvalue { i64, i64 } %pair0, 1
  %obj1 = inttoptr i64 %raw to ptr addrspace(1)
  %res1 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds1, i64 %alloc1, ptr addrspace(1) %arg, ptr addrspace(1) %obj1,
      ptr addrspace(1) %clos) "statepoint-id"="0" [ "deopt"() ]
  ret { { i64, i64 }, { i64 } } %res1
}
