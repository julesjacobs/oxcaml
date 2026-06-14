; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s \
; RUN:   | FileCheck %s --check-prefixes=CHECK,DEFAULT
; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify \
; RUN:   -rs4gc-oxcaml-root-dead-c-call-args < %s \
; RUN:   | FileCheck %s --check-prefixes=CHECK,ROOTED

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(i64, i64, ptr addrspace(1))

declare oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @c_call(
    i64, i64, i64, ptr addrspace(1))

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @managed_call_arg_not_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %arg) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @managed_call_arg_not_root(
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @callee
; CHECK-NOT: gc-live
; CHECK-SAME: [ "deopt"() ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %arg)
      "statepoint-id"="0" [ "deopt"() ]
  ret { { i64, i64 }, { i64 } } %res
}

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @managed_derived_call_arg_not_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %base) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @managed_derived_call_arg_not_root(
; CHECK: %[[DERIVED:.*]] = getelementptr i8, ptr addrspace(1) %base, i64 16
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @callee
; CHECK-SAME: ptr addrspace(1) %[[DERIVED]]
; CHECK-NOT: gc-live
; CHECK-SAME: [ "deopt"() ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %derived = getelementptr i8, ptr addrspace(1) %base, i64 16
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %derived)
      "statepoint-id"="0" [ "deopt"() ]
  ret { { i64, i64 }, { i64 } } %res
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @c_wrapper_call_arg_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %arg) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @c_wrapper_call_arg_root(
; CHECK: %statepoint_token = call oxcaml_ccc {{.*}} @c_call
; DEFAULT-SAME: [ "deopt"() ]
; ROOTED-SAME: [ "deopt"(), "gc-live"(ptr addrspace(1) %arg) ]
; CHECK-NOT: @llvm.experimental.gc.relocate
; CHECK: ret
entry:
  %res = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @c_call(
      i64 %ds, i64 %alloc, i64 1, ptr addrspace(1) %arg)
      "statepoint-id"="0" [ "deopt"() ]
  ret { { i64, i64 }, { ptr addrspace(1) } } %res
}

declare oxcaml_nofpcc { { i64, i64 }, {} } @raise_like(
    i64, i64, ptr addrspace(1))

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee5(
    i64, i64, ptr addrspace(1), ptr addrspace(1), ptr addrspace(1))

define oxcaml_nofpcc { { i64, i64 }, {} } @managed_unreachable_call_arg_not_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %arg) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @managed_unreachable_call_arg_not_root(
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @raise_like
; CHECK-NOT: gc-live
; CHECK-SAME: [ "deopt"() ]
entry:
  %res = call oxcaml_nofpcc { { i64, i64 }, {} } @raise_like(
      i64 %ds, i64 %alloc, ptr addrspace(1) %arg)
      "statepoint-id"="0" [ "deopt"() ]
  unreachable
}

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @loop_carried_derived_call_arg(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %base,
    ptr addrspace(1) %x,
    ptr addrspace(1) %y) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @loop_carried_derived_call_arg(
; CHECK: %[[DERIVED_LOOP:.*]] = phi ptr addrspace(1) [ %derived, %entry ], [ %derived.remat, %again_block ]
; CHECK: %[[BASE_LOOP:.*]] = phi ptr addrspace(1) [ %base, %entry ], [ %base.relocated, %again_block ]
; CHECK: %statepoint_token = call oxcaml_nofpcc {{.*}} @callee5
; CHECK-SAME: ptr addrspace(1) %[[DERIVED_LOOP]]
; CHECK-SAME: [ "deopt"(), "gc-live"({{.*}}ptr addrspace(1) %[[BASE_LOOP]]{{.*}}) ]
; CHECK: %base.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 {{[0-9]+}}, i32 {{[0-9]+}})
; CHECK: %[[BASE_BITS:.*]] = ptrtoint ptr addrspace(1) %base.relocated to i64
; CHECK: %[[DERIVED_BITS:.*]] = add i64 %[[BASE_BITS]], 64
; CHECK: %derived.remat = inttoptr i64 %[[DERIVED_BITS]] to ptr addrspace(1)
; CHECK: br i1 %again, label %again_block, label %done
entry:
  %base.bits = ptrtoint ptr addrspace(1) %base to i64
  %derived.bits = add i64 %base.bits, 64
  %derived = inttoptr i64 %derived.bits to ptr addrspace(1)
  br label %loop

loop:
  %ds.loop = phi i64 [ %ds, %entry ], [ %ds.next, %again_block ]
  %alloc.loop = phi i64 [ %alloc, %entry ], [ %alloc.next, %again_block ]
  %x.loop = phi ptr addrspace(1) [ %x, %entry ], [ %x.next, %again_block ]
  %y.loop = phi ptr addrspace(1) [ %y, %entry ], [ %y.next, %again_block ]
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee5(
      i64 %ds.loop, i64 %alloc.loop, ptr addrspace(1) %x.loop,
      ptr addrspace(1) %y.loop, ptr addrspace(1) %derived)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  %tagged = extractvalue { { i64, i64 }, { i64 } } %res, 1, 0
  %again = icmp ugt i64 %tagged, 1
  br i1 %again, label %again_block, label %done

again_block:
  %x.next.slot = getelementptr i8, ptr addrspace(1) %x.loop, i64 8
  %x.next = load ptr addrspace(1), ptr addrspace(1) %x.next.slot
  %y.next.slot = getelementptr i8, ptr addrspace(1) %y.loop, i64 8
  %y.next = load ptr addrspace(1), ptr addrspace(1) %y.next.slot
  br label %loop

done:
  %ret0 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { i64 } } %ret0, i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { i64 } } %ret1, i64 1, 1, 0
  ret { { i64, i64 }, { i64 } } %ret2
}
