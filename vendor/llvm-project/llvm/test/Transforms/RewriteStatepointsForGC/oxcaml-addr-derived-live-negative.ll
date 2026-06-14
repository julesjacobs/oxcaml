; RUN: not --crash opt -S -passes=rewrite-statepoints-for-gc,verify < %s 2>&1 | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(i64, i64)
declare void @use(ptr addrspace(1)) "gc-leaf-function"

define oxcaml_nofpcc { { i64, i64 }, {} } @loop_carried_addr(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i64 %n) gc "oxcaml" {
; CHECK: OxCaml statepoint would relocate a derived addrspace(1) pointer independently
; CHECK: unrematerialized OxCaml derived pointer across statepoint
entry:
  %begin = getelementptr i8, ptr addrspace(1) %obj, i64 16
  br label %loop

loop:
  %ds.cur = phi i64 [ %ds, %entry ], [ %ds2, %loop ]
  %alloc.cur = phi i64 [ %alloc, %entry ], [ %alloc2, %loop ]
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  %addr = phi ptr addrspace(1) [ %begin, %entry ], [ %addr.next, %loop ]
  %field = load ptr addrspace(1), ptr addrspace(1) %addr, align 8
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds.cur, i64 %alloc.cur) "statepoint-id"="0" [ "deopt"() ]
  call void @use(ptr addrspace(1) %field)
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %addr.next = getelementptr i8, ptr addrspace(1) %addr, i64 8
  %i.next = add i64 %i, 1
  %more = icmp ult i64 %i.next, %n
  br i1 %more, label %loop, label %exit

exit:
  %ret0 = insertvalue { { i64, i64 }, {} } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, {} } %ret0, i64 %alloc2, 0, 1
  ret { { i64, i64 }, {} } %ret1
}
