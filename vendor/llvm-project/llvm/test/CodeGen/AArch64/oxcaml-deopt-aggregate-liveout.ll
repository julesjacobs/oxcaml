; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-before=simple-register-coalescing < %s | FileCheck %s

@callee_global = external global ptr

declare oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @callee(i64, i64, i64, ptr addrspace(1), i64, i64)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @deopt_aggregate_liveout(i64 %ds, i64 %alloc, ptr addrspace(1) %p, i64 %cond_int) gc "oxcaml" {
entry:
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %call, label %other

call:
  %r = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @callee(i64 %ds, i64 %alloc, i64 ptrtoint (ptr @callee_global to i64), ptr addrspace(1) %p, i64 1, i64 %cond_int) "statepoint-id"="0" [ "deopt"(i64 1) ]
  %rds = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %r, 0, 0
  %addr = inttoptr i64 %rds to ptr
  store i64 %ds, ptr %addr
  br label %join

other:
  %o0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds, 0, 0
  %o1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %o0, i64 %alloc, 0, 1
  %o2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %o1, ptr addrspace(1) %p, 1, 0
  br label %join

join:
  %phi = phi { { i64, i64 }, { ptr addrspace(1) } } [ %r, %call ], [ %o2, %other ]
  ret { { i64, i64 }, { ptr addrspace(1) } } %phi
}

; CHECK-LABEL: name: deopt_aggregate_liveout
; CHECK: STATEPOINT
