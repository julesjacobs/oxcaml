; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-remat-addrspace1-derived-from-base-at-alloc < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_gep_rejoin(
    i1 %skip,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_gep_rejoin(
; CHECK: %obj.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_gep_rejoin, %recover))
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 96
  br i1 %skip, label %join, label %try

try:
; CHECK: store volatile ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj, ptr %obj.exnroot) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
; CHECK: normal:
; CHECK: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
; CHECK: recover:
; CHECK: %obj.exnroot.load = load volatile ptr addrspace(1), ptr %obj.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  br label %join

join:
; CHECK: join:
; CHECK: %obj.exnroot.select = phi ptr addrspace(1) [ %obj.exnroot.load, %recover ], [ %obj.relocated, %normal ], [ %obj, %entry ]
; CHECK: %field.addr.remat = getelementptr i8, ptr addrspace(1) %obj.exnroot.select, i64 96
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) %field.addr.remat, align 8
; CHECK-NOT: gc.relocate.p1(token %statepoint_token, i32 0, i32 1)
  %join_ds = phi i64 [ %ds, %entry ], [ %normal_ds, %normal ], [ %recovered_ds, %recover ]
  %join_alloc = phi i64 [ %alloc, %entry ], [ %normal_alloc, %normal ], [ %recovered_alloc, %recover ]
  %field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_base_equiv_rejoin(
    i1 %choose,
    i1 %skip,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_base_equiv_rejoin(
; CHECK: %same.base.exnroot = alloca ptr addrspace(1), align 8
entry:
  br i1 %choose, label %left, label %right

left:
  br label %setup

right:
  br label %setup

setup:
  %same.base = phi ptr addrspace(1) [ %obj, %left ], [ %obj, %right ]
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_base_equiv_rejoin, %recover))
  br i1 %skip, label %join, label %try

try:
; CHECK: store volatile ptr addrspace(1) %same.base, ptr %same.base.exnroot, align 8
; CHECK: store volatile ptr addrspace(1) %obj, ptr %obj.callargroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %same.base, ptr %same.base.exnroot, ptr %obj.callargroot) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
; CHECK: recover:
; CHECK: %same.base.exnroot.load = load volatile ptr addrspace(1), ptr %same.base.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  br label %join

join:
; CHECK: join:
; CHECK: %same.base.exnroot.select = phi ptr addrspace(1) [ %same.base.exnroot.load, %recover ], [ %same.base.relocated, %normal ], [ %same.base, %setup ]
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) %same.base.exnroot.select, align 8
; CHECK-NOT: unrematerializable
  %join_ds = phi i64 [ %ds, %setup ], [ %normal_ds, %normal ], [ %recovered_ds, %recover ]
  %join_alloc = phi i64 [ %alloc, %setup ], [ %normal_alloc, %normal ], [ %recovered_alloc, %recover ]
  %field = load ptr addrspace(1), ptr addrspace(1) %same.base, align 8
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_recovery_duplicate_phi_edges(
    i64 %tag,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_recovery_duplicate_phi_edges(
; CHECK: %obj.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_recovery_duplicate_phi_edges, %recover))
; CHECK: store volatile ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
; CHECK: recover:
; CHECK: %obj.exnroot.load = load volatile ptr addrspace(1), ptr %obj.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  switch i64 %tag, label %other [
    i64 3, label %join
    i64 61, label %join
  ]

other:
  br label %join

join:
; CHECK: join:
; CHECK: %merged = phi ptr addrspace(1) [ %obj.relocated, %normal ], [ %obj.exnroot.load, %recover ], [ %obj.exnroot.load, %recover ], [ %obj.exnroot.load{{[0-9]*}}, %other ]
  %join_ds = phi i64 [ %normal_ds, %normal ], [ %recovered_ds, %recover ], [ %recovered_ds, %recover ], [ %recovered_ds, %other ]
  %join_alloc = phi i64 [ %normal_alloc, %normal ], [ %recovered_alloc, %recover ], [ %recovered_alloc, %recover ], [ %recovered_alloc, %other ]
  %merged = phi ptr addrspace(1) [ %obj, %normal ], [ %obj, %recover ], [ %obj, %recover ], [ %obj, %other ]
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %merged, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_recovery_internal_phi_use(
    i1 %choose,
    i1 %which,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_recovery_internal_phi_use(
; CHECK: %same.base.exnroot = alloca ptr addrspace(1), align 8
entry:
  br i1 %choose, label %left, label %right

left:
  br label %setup

right:
  br label %setup

setup:
  %same.base = phi ptr addrspace(1) [ %obj, %left ], [ %obj, %right ]
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_recovery_internal_phi_use, %recover))
  br label %try

try:
; CHECK: store volatile ptr addrspace(1) %same.base, ptr %same.base.exnroot, align 8
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
; CHECK: recover:
; CHECK: %same.base.exnroot.load = load volatile ptr addrspace(1), ptr %same.base.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  br i1 %which, label %recover.left, label %recover.right

recover.left:
  br label %recover.join

recover.right:
  br label %recover.join

recover.join:
; CHECK: recover.join:
; CHECK: %recovered.ptr = phi ptr addrspace(1) [ %same.base.exnroot.load, %recover.left ], [ %same.base.exnroot.load, %recover.right ]
  %recovered.ptr = phi ptr addrspace(1) [ %same.base, %recover.left ], [ %same.base, %recover.right ]
  br label %join

join:
; CHECK: join:
; CHECK: %merged = phi ptr addrspace(1) [ %same.base.relocated, %normal ], [ %recovered.ptr, %recover.join ]
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) %merged, align 8
  %join_ds = phi i64 [ %normal_ds, %normal ], [ %recovered_ds, %recover.join ]
  %join_alloc = phi i64 [ %normal_alloc, %normal ], [ %recovered_alloc, %recover.join ]
  %merged = phi ptr addrspace(1) [ %same.base, %normal ], [ %recovered.ptr, %recover.join ]
  %field = load ptr addrspace(1), ptr addrspace(1) %merged, align 8
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_base_gep_rejoin(
    i1 %skip,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %chunk)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_base_gep_rejoin(
; CHECK: %obj.exnroot = alloca ptr addrspace(1), align 8
entry:
  %obj = getelementptr i8, ptr addrspace(1) %chunk, i64 40, !is_base_value !0
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_base_gep_rejoin, %recover))
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 8
  br i1 %skip, label %join, label %try

try:
; CHECK: store volatile ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj, ptr %obj.exnroot) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
; CHECK: recover:
; CHECK: %obj.exnroot.load = load volatile ptr addrspace(1), ptr %obj.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  br label %join

join:
; CHECK: join:
; CHECK: %obj.exnroot.select = phi ptr addrspace(1) [ %obj.exnroot.load, %recover ], [ %obj.relocated, %normal ], [ %obj, %entry ]
; CHECK: %field.addr.remat = getelementptr i8, ptr addrspace(1) %obj.exnroot.select, i64 8
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) %field.addr.remat, align 8
  %join_ds = phi i64 [ %ds, %entry ], [ %normal_ds, %normal ], [ %recovered_ds, %recover ]
  %join_alloc = phi i64 [ %alloc, %entry ], [ %normal_alloc, %normal ], [ %recovered_alloc, %recover ]
  %field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_inttoptr_derived_rejoin(
    i1 %skip,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_inttoptr_derived_rejoin(
; CHECK: %obj.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_inttoptr_derived_rejoin, %recover))
  %obj.int = ptrtoint ptr addrspace(1) %obj to i64
  %field.int = add i64 %obj.int, 96
  %field.addr = inttoptr i64 %field.int to ptr addrspace(1)
  br i1 %skip, label %join, label %try

try:
; CHECK: store volatile ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
; CHECK: recover:
; CHECK: %obj.exnroot.load = load volatile ptr addrspace(1), ptr %obj.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %recover_call = call oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %recovered_ds, i64 %recovered_alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
  %recover_ds2 = extractvalue { i64, i64, ptr addrspace(1) } %recover_call, 0
  %recover_alloc2 = extractvalue { i64, i64, ptr addrspace(1) } %recover_call, 1
  br label %join

join:
; CHECK: join:
; CHECK: %obj.exnroot.select = phi ptr addrspace(1)
; CHECK: %field.addr.remat = inttoptr i64 %field.int.remat to ptr addrspace(1)
  %join_ds = phi i64 [ %ds, %entry ], [ %normal_ds, %normal ], [ %recover_ds2, %recover ]
  %join_alloc = phi i64 [ %alloc, %entry ], [ %normal_alloc, %normal ], [ %recover_alloc2, %recover ]
  %post = call oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %join_ds, i64 %join_alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
  %post_ds = extractvalue { i64, i64, ptr addrspace(1) } %post, 0
  %post_alloc = extractvalue { i64, i64, ptr addrspace(1) } %post, 1
  %field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %base.field = load ptr addrspace(1), ptr addrspace(1) %obj, align 8
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %post_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %post_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

!0 = !{}
