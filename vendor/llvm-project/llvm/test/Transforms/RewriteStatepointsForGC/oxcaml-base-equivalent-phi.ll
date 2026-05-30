; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(i64, i64)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @base_equiv_lcssa_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %which,
    i1 %more) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @base_equiv_lcssa_phi(
; CHECK: %same.base = phi ptr addrspace(1)
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %base, ptr addrspace(1) %same.base) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %base.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: %same.base.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 1)
entry:
  br label %loop

loop:
  %base = phi ptr addrspace(1) [ %obj, %entry ], [ %next, %cont ]
  br i1 %which, label %left, label %right

left:
  %left.fr = freeze ptr addrspace(1) %base
  br label %join

right:
  %right.fr = freeze ptr addrspace(1) %base
  br label %join

join:
  %same.base = phi ptr addrspace(1) [ %left.fr, %left ], [ %right.fr, %right ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %field = load ptr addrspace(1), ptr addrspace(1) %same.base, align 8
  %next = load ptr addrspace(1), ptr addrspace(1) %base, align 8
  br i1 %more, label %cont, label %exit

cont:
  br label %loop

exit:
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
