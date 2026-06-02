; RUN: opt -S -O2 < %s | opt -S -passes=rewrite-statepoints-for-gc,verify | FileCheck %s

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee_a(i64, i64, ptr addrspace(1))
declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee_b(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @shared_lpad_handler_live_gc(
    i64 %cond_int,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a,
    ptr addrspace(1) %b)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@shared_lpad_handler_live_gc(
; CHECK: %handler_value.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@shared_lpad_handler_live_gc, %recover))
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %left, label %right

left:
; CHECK: left:
; CHECK-NEXT: store volatile ptr addrspace(1) %a, ptr %handler_value.exnroot, align 8
; CHECK-NEXT: %statepoint_token = invoke {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "gc-live"({{.*}}ptr %handler_value.exnroot{{.*}}) ]
; CHECK-NEXT: to label %{{.*}} unwind label %recover
  %left_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %recover

normal_left:
  ret { i64, i64, ptr addrspace(1) } %left_call

right:
; CHECK: right:
; CHECK-NEXT: store volatile ptr addrspace(1) %b, ptr %handler_value.exnroot, align 8
; CHECK-NEXT: %statepoint_token{{[0-9]*}} = invoke {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "gc-live"({{.*}}ptr %handler_value.exnroot{{.*}}) ]
; CHECK-NEXT: to label %{{.*}} unwind label %recover
  %right_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_b(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %recover

normal_right:
  ret { i64, i64, ptr addrspace(1) } %right_call

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK-NEXT: %handler_value.exnroot.load = load volatile ptr addrspace(1), ptr %handler_value.exnroot, align 8
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: br label %common.ret
  %handler_value = phi ptr addrspace(1) [ %a, %left ], [ %b, %right ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %handler_value, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @same_value_two_statepoints(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@same_value_two_statepoints(
; CHECK: %a.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@same_value_two_statepoints, %recover))
; CHECK: store volatile ptr addrspace(1) %a, ptr %a.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "gc-live"({{.*}}ptr %a.exnroot{{.*}}) ]
  %call1 = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %after1 unwind label %recover

after1:
; CHECK: after1:
; CHECK: %a.normal = load volatile ptr addrspace(1), ptr %a.exnroot, align 8
; CHECK: store volatile ptr addrspace(1) %a.normal, ptr %a.exnroot, align 8
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"({{.*}}ptr %a.exnroot{{.*}}) ]
  %call2 = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_b(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover

normal:
  ret { i64, i64, ptr addrspace(1) } %call2

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK-NEXT: %a.exnroot.load = load volatile ptr addrspace(1), ptr %a.exnroot, align 8
  %handler_value = phi ptr addrspace(1) [ %a, %entry ], [ %a, %after1 ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %handler_value, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @split_lpad_handler_live_gc_phi(
    i64 %cond_int,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a,
    ptr addrspace(1) %b)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@split_lpad_handler_live_gc_phi(
; CHECK: %handler_value.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@split_lpad_handler_live_gc_phi, %recover))
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %left, label %right

left:
; CHECK: left:
; CHECK-NEXT: store volatile ptr addrspace(1) %a, ptr %handler_value.exnroot, align 8
; CHECK-NEXT: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"({{.*}}ptr %handler_value.exnroot{{.*}}) ]
; CHECK-NEXT: to label %{{.*}} unwind label %left_lpad
  %left_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %left_lpad

normal_left:
  ret { i64, i64, ptr addrspace(1) } %left_call

right:
; CHECK: right:
; CHECK-NEXT: store volatile ptr addrspace(1) %b, ptr %handler_value.exnroot, align 8
; CHECK-NEXT: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"({{.*}}ptr %handler_value.exnroot{{.*}}) ]
; CHECK-NEXT: to label %{{.*}} unwind label %right_lpad
  %right_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_b(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %right_lpad

normal_right:
  ret { i64, i64, ptr addrspace(1) } %right_call

left_lpad:
; CHECK: left_lpad:
; CHECK-NEXT: %left_lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: br label %recover
  %left_lp = landingpad token cleanup
  br label %recover

right_lpad:
; CHECK: right_lpad:
; CHECK-NEXT: %right_lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: br label %recover
  %right_lp = landingpad token cleanup
  br label %recover

recover:
; CHECK: recover:
; CHECK-NOT: phi ptr addrspace(1)
; CHECK: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK-NEXT: %handler_value.exnroot.load = load volatile ptr addrspace(1), ptr %handler_value.exnroot, align 8
  %handler_value = phi ptr addrspace(1) [ %a, %left_lpad ], [ %b, %right_lpad ]
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %handler_value, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @late_handler_use_two_statepoints(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@late_handler_use_two_statepoints(
; CHECK: %a.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@late_handler_use_two_statepoints, %recover))
; CHECK: store volatile ptr addrspace(1) %a, ptr %a.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "gc-live"({{.*}}ptr %a.exnroot{{.*}}) ]
  %call1 = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %after1 unwind label %recover

after1:
; CHECK: after1:
; CHECK: %a.normal = load volatile ptr addrspace(1), ptr %a.exnroot, align 8
; CHECK: store volatile ptr addrspace(1) %a.normal, ptr %a.exnroot, align 8
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"({{.*}}ptr %a.exnroot{{.*}}) ]
  %call2 = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_b(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover

normal:
  ret { i64, i64, ptr addrspace(1) } %call2

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK-NEXT: %a.exnroot.load = load volatile ptr addrspace(1), ptr %a.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %a, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @post_recovery_join_phi_gc(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@post_recovery_join_phi_gc(
; CHECK: %a.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@post_recovery_join_phi_gc, %recover))
; CHECK: store volatile ptr addrspace(1) %a, ptr %a.exnroot, align 8
; CHECK-NEXT: %statepoint_token = invoke {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "gc-live"({{.*}}ptr %a.exnroot{{.*}}) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover

normal:
  %normal_value = extractvalue { i64, i64, ptr addrspace(1) } %call, 2
  br label %join

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK: %a.exnroot.load = load volatile ptr addrspace(1), ptr %a.exnroot, align 8
; CHECK-NEXT: br label %join
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br label %caught

caught:
  br label %join

join:
; CHECK: join:
; CHECK-NEXT: %joined = phi ptr addrspace(1) [ %{{.*}}, %normal ], [ %a.exnroot.load, %recover ]
  %joined = phi ptr addrspace(1) [ %normal_value, %normal ], [ %a, %caught ]
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %joined, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @post_recovery_join_phi_gc_normal_relocate(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a,
    ptr addrspace(1) %b)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@post_recovery_join_phi_gc_normal_relocate(
; CHECK: %b.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@post_recovery_join_phi_gc_normal_relocate, %recover))
; CHECK: store volatile ptr addrspace(1) %b, ptr %b.exnroot, align 8
; CHECK-NEXT: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"(ptr addrspace(1) %a, ptr %b.exnroot) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover

normal:
  br label %join

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK: %b.exnroot.load = load volatile ptr addrspace(1), ptr %b.exnroot, align 8
; CHECK-NEXT: br label %join
; CHECK: join{{.*}}:
; CHECK: %a.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token{{[0-9]*}}, i32 0, i32 0)
; CHECK: br label %join
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br label %join

join:
; CHECK: join:
; CHECK-NEXT: %joined = phi ptr addrspace(1) [ %b.exnroot.load, %recover ], [ %a.relocated, %join{{.*}} ]
  %joined = phi ptr addrspace(1) [ %a, %normal ], [ %b, %recover ]
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %joined, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @post_recovery_raw_cast_use(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %ref)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define {{.*}}@post_recovery_raw_cast_use(
; CHECK: %ref.exnroot = alloca ptr addrspace(1), align 8
entry:
  %ref.raw = addrspacecast ptr addrspace(1) %ref to ptr
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@post_recovery_raw_cast_use, %recover))
; CHECK: store volatile ptr addrspace(1) %ref, ptr %ref.exnroot, align 8
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"({{.*}}ptr %ref.exnroot{{.*}}) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_a(i64 %ds, i64 %alloc, ptr addrspace(1) %ref)
      to label %normal unwind label %recover

normal:
  ret { i64, i64, ptr addrspace(1) } %call

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NOT: llvm.experimental.gc.relocate
; CHECK: %rec = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK: %ref.exnroot.load = load volatile ptr addrspace(1), ptr %ref.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br label %handler

handler:
; CHECK: %[[REF_RAW:.*]] = addrspacecast ptr addrspace(1) %ref.exnroot.load to ptr
; CHECK: %field = load ptr addrspace(1), ptr %[[REF_RAW]], align 8
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "gc-live"({{.*}}ptr %ref.exnroot{{.*}}) ]
  %field = load ptr addrspace(1), ptr %ref.raw, align 8
  %handler_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee_b(i64 %ds, i64 %alloc, ptr addrspace(1) %field)
      to label %handler_normal unwind label %outer_recover

handler_normal:
  ret { i64, i64, ptr addrspace(1) } %handler_call

outer_recover:
  %outer_lp = landingpad token cleanup
  %outer_rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %outer_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %outer_rec, 2
  %outer_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %outer_rec, 3
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %outer_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %outer_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %ref, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"ExplicitExceptionRoots"}
