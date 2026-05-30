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
; CHECK-NEXT: %same.base.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 1, i32 1)
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

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @derived_phi_from_base_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj1,
    ptr addrspace(1) %obj2,
    i1 %which) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_phi_from_base_phi(
; CHECK: %derived.phi.base = phi ptr addrspace(1) [ %obj1, %left ], [ %obj2, %right ], !is_base_value !0
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %derived.phi.base) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %derived.phi.base.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: %[[FIELD_ADDR:.*]] = getelementptr i8, ptr addrspace(1) %derived.phi.base.relocated, i64 24
; CHECK-NEXT: %field = load ptr addrspace(1), ptr addrspace(1) %[[FIELD_ADDR]], align 8
entry:
  br i1 %which, label %left, label %right

left:
  %left.derived = getelementptr i8, ptr addrspace(1) %obj1, i64 24
  br label %join

right:
  %right.derived = getelementptr i8, ptr addrspace(1) %obj2, i64 24
  br label %join

join:
  %derived.phi = phi ptr addrspace(1) [ %left.derived, %left ], [ %right.derived, %right ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %field = load ptr addrspace(1), ptr addrspace(1) %derived.phi, align 8
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @derived_or_immediate_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %is_immediate) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_or_immediate_phi(
; CHECK: %derived.phi.base = phi ptr addrspace(1) [ %obj, %object ], [ inttoptr (i64 1 to ptr addrspace(1)), %immediate ], !is_base_value !0
; CHECK: %derived.phi.needs_remat = icmp ne ptr addrspace(1) %derived.phi, %derived.phi.base
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %derived.phi.base) ]
; CHECK: %derived.phi.base.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: %[[BASE_INT:.*]] = ptrtoint ptr addrspace(1) %derived.phi.base.relocated to i64
; CHECK-NEXT: %[[ADJ_INT:.*]] = add i64 %[[BASE_INT]], -2
; CHECK-NEXT: %[[ADJ_PTR:.*]] = inttoptr i64 %[[ADJ_INT]] to ptr addrspace(1)
; CHECK-NEXT: %[[SELECTED:.*]] = select i1 %derived.phi.needs_remat, ptr addrspace(1) %[[ADJ_PTR]], ptr addrspace(1) %derived.phi.base.relocated
; CHECK: insertvalue {{.*}} ptr addrspace(1) %[[SELECTED]], 1, 0
entry:
  br i1 %is_immediate, label %immediate, label %object

object:
  %obj.i = ptrtoint ptr addrspace(1) %obj to i64
  %obj.adj.i = add i64 %obj.i, -2
  %obj.adj = inttoptr i64 %obj.adj.i to ptr addrspace(1)
  br label %join

immediate:
  br label %join

join:
  %derived.phi = phi ptr addrspace(1) [ %obj.adj, %object ], [ inttoptr (i64 1 to ptr addrspace(1)), %immediate ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %derived.phi, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @derived_phi_from_int_alloc_base(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %which) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_phi_from_int_alloc_base(
; CHECK: %derived.phi.base = phi ptr addrspace(1) [ %base, %left ], [ %obj, %right ], !is_base_value !0
; CHECK: %derived.phi.needs_remat = icmp ne ptr addrspace(1) %derived.phi, %derived.phi.base
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %derived.phi.base) ]
; CHECK: %derived.phi.base.relocated = call {{.*}} @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: %[[REMAT:.*]] = getelementptr i8, ptr addrspace(1) %derived.phi.base.relocated, i64 32
; CHECK-NEXT: %[[SELECTED:.*]] = select i1 %derived.phi.needs_remat, ptr addrspace(1) %[[REMAT]], ptr addrspace(1) %derived.phi.base.relocated
; CHECK: insertvalue {{.*}} ptr addrspace(1) %[[SELECTED]], 1, 0
entry:
  br i1 %which, label %left, label %right

left:
  %base.i = add i64 %alloc, 8
  %base = inttoptr i64 %base.i to ptr addrspace(1)
  %derived = getelementptr i8, ptr addrspace(1) %base, i64 32
  br label %join

right:
  br label %join

join:
  %derived.phi = phi ptr addrspace(1) [ %derived, %left ], [ %obj, %right ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %derived.phi, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
