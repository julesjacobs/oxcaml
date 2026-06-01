; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(i64, i64)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @select_base_or_derived(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %use_base) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @select_base_or_derived(
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: [[DERIVED:%.*]] = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 16
; CHECK-NEXT: %sel.remat.select = select i1 %use_base, ptr addrspace(1) %obj.relocated, ptr addrspace(1) [[DERIVED]]
entry:
  %derived = getelementptr i8, ptr addrspace(1) %obj, i64 16
  %sel = select i1 %use_base, ptr addrspace(1) %obj, ptr addrspace(1) %derived
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %field = load ptr addrspace(1), ptr addrspace(1) %sel, align 8
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @select_derived_or_base(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %use_derived) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @select_derived_or_base(
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: [[DERIVED:%.*]] = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 16
; CHECK-NEXT: %sel.remat.select = select i1 %use_derived, ptr addrspace(1) [[DERIVED]], ptr addrspace(1) %obj.relocated
entry:
  %derived = getelementptr i8, ptr addrspace(1) %obj, i64 16
  %sel = select i1 %use_derived, ptr addrspace(1) %derived, ptr addrspace(1) %obj
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %field = load ptr addrspace(1), ptr addrspace(1) %sel, align 8
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @select_between_derived_values(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %use_right) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @select_between_derived_values(
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: [[LEFT:%.*]] = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 16
; CHECK-NEXT: [[RIGHT:%.*]] = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 24
; CHECK-NEXT: [[TAG:%.*]] = icmp eq i1 %use_right, true
; CHECK-NEXT: [[SELECTED:%.*]] = select i1 [[TAG]], ptr addrspace(1) [[RIGHT]], ptr addrspace(1) [[LEFT]]
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) [[SELECTED]], align 8
entry:
  %left = getelementptr i8, ptr addrspace(1) %obj, i64 16
  %right = getelementptr i8, ptr addrspace(1) %obj, i64 24
  %sel = select i1 %use_right, ptr addrspace(1) %right, ptr addrspace(1) %left
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %field = load ptr addrspace(1), ptr addrspace(1) %sel, align 8
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @phi_base_or_derived_from_rematerializable_base(
    i64 %ds,
    i64 %alloc,
    i1 %use_derived) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @phi_base_or_derived_from_rematerializable_base(
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %base) ]
; CHECK-NEXT: %res1 = call {{.*}} @llvm.experimental.gc.result
; CHECK-NEXT: %base.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK-NEXT: [[DERIVED:%.*]] = getelementptr i8, ptr addrspace(1) %base.relocated, i64 32
; CHECK-NEXT: [[SELECTED:%.*]] = select i1 %derived.phi.needs_remat, ptr addrspace(1) [[DERIVED]], ptr addrspace(1) %base.relocated
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) [[SELECTED]], align 8
entry:
  %base.i = add i64 %alloc, 8
  %base = inttoptr i64 %base.i to ptr addrspace(1)
  %derived = getelementptr i8, ptr addrspace(1) %base, i64 32
  br i1 %use_derived, label %left, label %right

left:
  br label %join

right:
  br label %join

join:
  %derived.phi = phi ptr addrspace(1) [ %derived, %left ], [ %base, %right ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %pair = extractvalue { { i64, i64 }, {} } %res, 0
  %ds2 = extractvalue { i64, i64 } %pair, 0
  %alloc2 = extractvalue { i64, i64 } %pair, 1
  %field = load ptr addrspace(1), ptr addrspace(1) %derived.phi, align 8
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %field, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @nested_phi_base_or_derived_from_base_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj,
    i1 %use_fresh,
    i1 %again) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @nested_phi_base_or_derived_from_base_phi(
; CHECK: %outer.derived.base = phi ptr addrspace(1) [ %fresh.base, %fresh ], [ %inner.derived.base, %loop ], !is_base_value
; CHECK: %statepoint_token = call oxcaml_alloccc {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %outer.derived.base) ]
; CHECK: %outer.derived.base.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: getelementptr i8, ptr addrspace(1) %outer.derived.base.relocated, i64 24
entry:
  br label %loop

loop:
  %inner.derived = phi ptr addrspace(1) [ %obj, %entry ], [ %back.derived, %back ]
  %inner.base = phi ptr addrspace(1) [ %obj, %entry ], [ %back.base, %back ]
  br i1 %use_fresh, label %fresh, label %join

fresh:
  %fresh.base.i = add i64 %alloc, 8
  %fresh.base = inttoptr i64 %fresh.base.i to ptr addrspace(1)
  %fresh.derived = getelementptr i8, ptr addrspace(1) %fresh.base, i64 24
  br label %join

join:
  %outer.derived = phi ptr addrspace(1) [ %fresh.derived, %fresh ], [ %inner.derived, %loop ]
  %res = call oxcaml_alloccc { { i64, i64 }, {} } @caml_call_gc(
      i64 %ds, i64 %alloc) "statepoint-id"="0" [ "deopt"() ]
  %field = load ptr addrspace(1), ptr addrspace(1) %outer.derived, align 8
  br i1 %again, label %back, label %exit

back:
  %back.base.i = add i64 %alloc, 64
  %back.base = inttoptr i64 %back.base.i to ptr addrspace(1)
  %back.derived = getelementptr i8, ptr addrspace(1) %back.base, i64 24
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
