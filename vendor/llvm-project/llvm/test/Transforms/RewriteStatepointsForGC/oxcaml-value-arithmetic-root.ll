; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(i64, i64)
declare void @use_val(ptr addrspace(1)) "gc-leaf-function"

define oxcaml_nofpcc { { i64, i64 }, {} } @loop_carried_value_arithmetic(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %start0,
    i1 %more) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @loop_carried_value_arithmetic(
; CHECK-NOT: needs_remat
; CHECK: %[[BASE:.*]] = phi ptr addrspace(1) [ %start0, %entry ], [ %start0.relocated, %loop ]
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %[[BASE]], ptr addrspace(1) %start) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %start0.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: %start.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 1, i32 1)
; CHECK-NEXT: call void @use_val(ptr addrspace(1) %start.relocated)
entry:
  br label %loop

loop:
  %ds.cur = phi i64 [ %ds, %entry ], [ %ds.next, %loop ]
  %alloc.cur = phi i64 [ %alloc, %entry ], [ %alloc.next, %loop ]
  %start = phi ptr addrspace(1) [ %start0, %entry ], [ %start.next, %loop ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds.cur, i64 %alloc.cur) "statepoint-id"="0" [ "deopt"() ]
  call void @use_val(ptr addrspace(1) %start)
  %start.i = ptrtoint ptr addrspace(1) %start to i64
  %start.next.i = add i64 %start.i, -2
  %start.next = inttoptr i64 %start.next.i to ptr addrspace(1)
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds.next = extractvalue { i64, i64 } %pair, 0
  %alloc.next = extractvalue { i64, i64 } %pair, 1
  br i1 %more, label %loop, label %exit

exit:
  %ret0 = insertvalue { { i64, i64 }, {} } poison, i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, {} } %ret0, i64 %alloc.next, 0, 1
  ret { { i64, i64 }, {} } %ret1
}
