; RUN: opt -S -passes=rewrite-statepoints-for-gc -rs4gc-oxcaml-volatile-root-allocas < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
    i64, i64, ptr addrspace(1))

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare void @llvm.aarch64.oxcaml.raise.notrace(i64, i64, i64)
declare i32 @personality(...)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @arg_live_after_call(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @arg_live_after_call(
; CHECK: %x.root = alloca ptr addrspace(1), align 8
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %x.root.load = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: insertvalue {{.*}} ptr addrspace(1) %x.root.load
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
; CHECK: %x.root = alloca ptr addrspace(1), align 8
; CHECK: store volatile ptr addrspace(1) %x, ptr %x.root, align 8
; CHECK: called:
; CHECK: %statepoint_token = call oxcaml_nofpcc
; CHECK-SAME: [ "deopt"(), "gc-live"(ptr %x.root) ]
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: join:
; CHECK: %x.root.load = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: insertvalue {{.*}} ptr addrspace(1) %x.root.load
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
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK-NOT: exnroot
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
  call void @llvm.aarch64.oxcaml.raise.notrace(i64 %exn.int, i64 %ds.exn, i64 %alloc.exn)
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
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: normal:
; CHECK: %[[NORMAL_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: br label %join
; CHECK: lpad:
; CHECK: landingpad token
; CHECK: %[[LPAD_LOAD:.*]] = load volatile ptr addrspace(1), ptr %x.root, align 8
; CHECK: br label %join
; CHECK: join:
; CHECK: phi ptr addrspace(1) [ %[[NORMAL_LOAD]], %normal ], [ %[[LPAD_LOAD]], %lpad ]
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
