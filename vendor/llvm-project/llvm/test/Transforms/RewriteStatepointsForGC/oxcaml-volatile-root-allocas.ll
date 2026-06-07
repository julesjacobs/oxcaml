; RUN: opt -S -passes=rewrite-statepoints-for-gc -rs4gc-oxcaml-volatile-root-allocas < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
    i64, i64, ptr addrspace(1))
declare ptr addrspace(1) @plain_callee(ptr addrspace(1))

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare void @llvm.aarch64.oxcaml.raise.notrace(i64)
declare i32 @personality(...)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @arg_live_after_call(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @arg_live_after_call(
; CHECK: %x.root = alloca ptr addrspace(1), align 8, !oxcaml.statepoint.root.slot
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %[[X_FINAL_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: insertvalue {{.*}} ptr addrspace(1) %[[X_FINAL_LOAD]]
entry:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @loop_invariant_arg(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x,
    i64 %n) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @loop_invariant_arg(
; CHECK: entry:
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: loop:
; CHECK-NOT: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %[[X_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: ptr addrspace(1) %[[X_LOAD]]
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: done:
; CHECK: %x.root.load = load volatile ptr addrspace(1), ptr %x.root, align 8
entry:
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  %ds.loop = phi i64 [ %ds, %entry ], [ %ds.next, %loop ]
  %alloc.loop = phi i64 [ %alloc, %entry ], [ %alloc.next, %loop ]
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds.loop, i64 %alloc.loop, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  %i.next = add i64 %i, 1
  %again = icmp ult i64 %i.next, %n
  br i1 %again, label %loop, label %done

done:
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @statepoint_on_one_join_predecessor(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x,
    i1 %take_call) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @statepoint_on_one_join_predecessor(
; CHECK: %x.root = alloca ptr addrspace(1), align 8, !oxcaml.statepoint.root.slot
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: called:
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: join:
; CHECK: %[[X_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: insertvalue {{.*}} ptr addrspace(1) %[[X_LOAD]]
entry:
  br i1 %take_call, label %called, label %uncalled

called:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.called = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.called = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  br label %join

uncalled:
  br label %join

join:
  %ds.join = phi i64 [ %ds.called, %called ], [ %ds, %uncalled ]
  %alloc.join = phi i64 [ %alloc.called, %called ], [ %alloc, %uncalled ]
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.join, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.join, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @invoke_handler_live(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_handler_live(
; CHECK: %x.root = alloca ptr addrspace(1), align 8
; CHECK-NOT: exnroot
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %statepoint_token = invoke oxcaml_nofpcc token
; CHECK-SAME: ptr addrspace(1) %x
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: exnroot
; CHECK: normal:
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %{{.*}} = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: ret { { i64, i64 }, { ptr addrspace(1) } }
entry:
  call void @llvm.aarch64.oxcaml.push.trap(
      ptr blockaddress(@invoke_handler_live, %lpad))
  br label %try

try:
  %res = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %lpad

normal:
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  call void @llvm.aarch64.oxcaml.pop.trap()
  br label %join

lpad:
  %lp = landingpad token cleanup
  %recover = call { ptr addrspace(1), i64, i64, i64 }
      @llvm.aarch64.oxcaml.trap.recover()
  %exn = extractvalue { ptr addrspace(1), i64, i64, i64 } %recover, 0
  %ds.exn = extractvalue { ptr addrspace(1), i64, i64, i64 } %recover, 3
  %alloc.exn = extractvalue { ptr addrspace(1), i64, i64, i64 } %recover, 2
  %is_miss = icmp eq ptr addrspace(1) %exn, %x
  br i1 %is_miss, label %join, label %reraise

reraise:
  %exn.int = ptrtoint ptr addrspace(1) %exn to i64
  call void @llvm.aarch64.oxcaml.raise.notrace(i64 %exn.int)
  unreachable

join:
  %ds.join = phi i64 [ %ds.next, %normal ], [ %ds.exn, %lpad ]
  %alloc.join = phi i64 [ %alloc.next, %normal ], [ %alloc.exn, %lpad ]
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.join, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.join, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @invoke_lpad_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_lpad_phi(
; CHECK: %x.root = alloca ptr addrspace(1), align 8
; CHECK: %statepoint_token = invoke oxcaml_nofpcc token
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK: normal:
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %[[NORMAL_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: br label %join
; CHECK: lpad:
; CHECK: landingpad token
; CHECK: %[[LPAD_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: br label %join
; CHECK: join:
; CHECK: %q = phi ptr addrspace(1) [ %[[NORMAL_LOAD]], %normal ], [ %[[LPAD_LOAD]], %lpad ]
; CHECK-NOT: %q.root
; CHECK: insertvalue {{.*}} ptr addrspace(1) %q
entry:
  call void @llvm.aarch64.oxcaml.push.trap(
      ptr blockaddress(@invoke_lpad_phi, %lpad))
  br label %try

try:
  %res = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %lpad

normal:
  br label %join

lpad:
  %p = phi ptr addrspace(1) [ %x, %try ]
  %lp = landingpad token cleanup
  %recover = call { ptr addrspace(1), i64, i64, i64 }
      @llvm.aarch64.oxcaml.trap.recover()
  br label %join

join:
  %q = phi ptr addrspace(1) [ %x, %normal ], [ %p, %lpad ]
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %q, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @shared_lpad_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x,
    ptr addrspace(1) %y,
    i1 %which) gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @shared_lpad_phi(
; CHECK: %p.recoverphi = alloca ptr addrspace(1), align 8, !oxcaml.statepoint.root.slot
; CHECK: %x.root = alloca ptr addrspace(1), align 8
; CHECK: %y.root = alloca ptr addrspace(1), align 8
; CHECK: try_x:
; CHECK: invoke oxcaml_nofpcc token
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root, ptr %p.recoverphi) ]
; CHECK: try_y:
; CHECK: invoke oxcaml_nofpcc token
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %y.root, ptr %p.recoverphi) ]
; CHECK: lpad:
; CHECK: landingpad token
; CHECK: %[[P_RECOVER_LOAD:.*]] = load volatile ptr addrspace(1), ptr %p.recoverphi, align 8
; CHECK: join:
; CHECK: %q = phi ptr addrspace(1) [ %{{.*}}, %normal_x ], [ %{{.*}}, %normal_y ], [ %[[P_RECOVER_LOAD]], %lpad ]
; CHECK-NOT: %q.root
; CHECK: insertvalue {{.*}} ptr addrspace(1) %q
entry:
  call void @llvm.aarch64.oxcaml.push.trap(
      ptr blockaddress(@shared_lpad_phi, %lpad))
  br i1 %which, label %try_x, label %try_y

try_x:
  %res.x = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal_x unwind label %lpad

try_y:
  %res.y = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %y)
      "statepoint-id"="1" [ "deopt"() ]
      to label %normal_y unwind label %lpad

normal_x:
  br label %join

normal_y:
  br label %join

lpad:
  %p = phi ptr addrspace(1) [ %x, %try_x ], [ %y, %try_y ]
  %lp = landingpad token cleanup
  %recover = call { ptr addrspace(1), i64, i64, i64 }
      @llvm.aarch64.oxcaml.trap.recover()
  br label %join

join:
  %q = phi ptr addrspace(1) [ %x, %normal_x ], [ %y, %normal_y ], [ %p, %lpad ]
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %q, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @mixed_rooted_unrooted_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x,
    ptr addrspace(1) %y,
    i1 %which) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @mixed_rooted_unrooted_phi(
; CHECK: %[[X_ROOT:.*]] = alloca ptr addrspace(1), align 8
; CHECK-NOT: edge.root
; CHECK: called:
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %[[X_ROOT]]) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %[[X_LOAD:.*]] = load volatile ptr addrspace(1), ptr %[[X_ROOT]], align 8
; CHECK: join:
; CHECK: %p = phi ptr addrspace(1) [ %[[X_LOAD]], %called ], [ %y, %uncalled ]
; CHECK-NOT: %p.root
; CHECK: insertvalue {{.*}} ptr addrspace(1) %p
entry:
  br i1 %which, label %called, label %uncalled

called:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.called = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.called = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  br label %join

uncalled:
  br label %join

join:
  %ds.join = phi i64 [ %ds.called, %called ], [ %ds, %uncalled ]
  %alloc.join = phi i64 [ %alloc.called, %called ], [ %alloc, %uncalled ]
  %p = phi ptr addrspace(1) [ %x, %called ], [ %y, %uncalled ]
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.join, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.join, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %p, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @instruction_root_two_statepoints(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %p) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @instruction_root_two_statepoints(
; CHECK: %x.root = alloca ptr addrspace(1), align 8
; CHECK: %x = load ptr addrspace(1), ptr addrspace(1) %p, align 8
; CHECK-NEXT: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK-NOT: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %statepoint_token{{.*}} = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %{{.*}} = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: insertvalue {{.*}} ptr addrspace(1) %{{.*}}
entry:
  %x = load ptr addrspace(1), ptr addrspace(1) %p, align 8
  %res1 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds1 = extractvalue { { i64, i64 }, { i64 } } %res1, 0, 0
  %alloc1 = extractvalue { { i64, i64 }, { i64 } } %res1, 0, 1
  %res2 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds1, i64 %alloc1, ptr addrspace(1) %x)
      "statepoint-id"="1" [ "deopt"() ]
  %ds2 = extractvalue { { i64, i64 }, { i64 } } %res2, 0, 0
  %alloc2 = extractvalue { { i64, i64 }, { i64 } } %res2, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds2, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc2, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @post_statepoint_traversal_phi(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %root) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @post_statepoint_traversal_phi(
; CHECK: %[[ROOT_SLOT:.*]] = alloca ptr addrspace(1), align 8, !oxcaml.statepoint.root.slot
; CHECK: store volatile ptr addrspace(1) %root, ptr %[[ROOT_SLOT]], align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %[[ROOT_SLOT]]) ]
; CHECK: %[[ROOT_LOAD:.*]] = load volatile ptr addrspace(1), ptr %[[ROOT_SLOT]], align 8
; CHECK: br label %walk
; CHECK: walk:
; CHECK: %cur = phi ptr addrspace(1) [ %[[ROOT_LOAD]], %entry ], [ %next, %walk ]
; CHECK: %next = load ptr addrspace(1), ptr addrspace(1) %cur, align 8
; CHECK: br i1 %again, label %walk, label %done
; CHECK: done:
; CHECK-NOT: %cur.root
; CHECK: insertvalue {{.*}} ptr addrspace(1) %cur
entry:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %root)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  br label %walk

walk:
  %cur = phi ptr addrspace(1) [ %root, %entry ], [ %next, %walk ]
  %next = load ptr addrspace(1), ptr addrspace(1) %cur, align 8
  %again = icmp ne ptr addrspace(1) %next, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %again, label %walk, label %done

done:
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %cur, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @address_phi_loaded_after_statepoint(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x,
    ptr addrspace(1) %y,
    i1 %which) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @address_phi_loaded_after_statepoint(
; CHECK: %[[BASE_ROOT:.*]] = alloca ptr addrspace(1), align 8, !oxcaml.statepoint.root.slot
; CHECK: join:
; CHECK: %[[BASE_PHI:.*]] = phi ptr addrspace(1) [ %x, %left ], [ %y, %right ], !is_base_value
; CHECK: store volatile ptr addrspace(1) %[[BASE_PHI]], ptr %[[BASE_ROOT]], align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %[[BASE_ROOT]]) ]
; CHECK: %[[BASE_LOAD:.*]] = load volatile ptr addrspace(1), ptr %[[BASE_ROOT]], align 8
; CHECK: %[[FIELD_REMAT:.*]] = getelementptr i8, ptr addrspace(1) %[[BASE_LOAD]], i64 8
; CHECK: %[[LOADED:.*]] = load ptr addrspace(1), ptr addrspace(1) %[[FIELD_REMAT]], align 8
; CHECK-NOT: load ptr addrspace(1), ptr addrspace(1) %[[BASE_LOAD]], align 8
; CHECK: insertvalue {{.*}} ptr addrspace(1) %[[LOADED]]
entry:
  br i1 %which, label %left, label %right

left:
  %field.x = getelementptr i8, ptr addrspace(1) %x, i64 8
  br label %join

right:
  %field.y = getelementptr i8, ptr addrspace(1) %y, i64 8
  br label %join

join:
  %base = phi ptr addrspace(1) [ %x, %left ], [ %y, %right ]
  %field = phi ptr addrspace(1) [ %field.x, %left ], [ %field.y, %right ]
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %base)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  %loaded = load ptr addrspace(1), ptr addrspace(1) %field, align 8
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %loaded, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @default_cc_statepoint(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @default_cc_statepoint(
; CHECK: %statepoint_token = call token
; CHECK-SAME: @llvm.experimental.gc.statepoint
; CHECK-SAME: @plain_callee
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr addrspace(1) %x) ]
; CHECK: %x.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate
entry:
  %y = call ptr addrspace(1) @plain_callee(ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @derived_chain_after_statepoint(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @derived_chain_after_statepoint(
; CHECK: %x.root = alloca ptr addrspace(1), align 8, !oxcaml.statepoint.root.slot
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %[[X_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: %same = icmp eq ptr addrspace(1) %derived, %[[X_LOAD]]
; CHECK-NOT: %derived.remat
; CHECK: %ret2 = insertvalue {{.*}} ptr addrspace(1) %derived
entry:
  %x.int = ptrtoint ptr addrspace(1) %x to i64
  %derived.int = add i64 %x.int, -2
  %derived = inttoptr i64 %derived.int to ptr addrspace(1)
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  %same = icmp eq ptr addrspace(1) %derived, %x
  br i1 %same, label %same_path, label %done

same_path:
  br label %done

done:
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %derived, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
