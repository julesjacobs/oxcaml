; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-remat-addrspace1-derived-from-base-at-alloc < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
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
; CHECK: store ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
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
; CHECK: %obj.exnroot.load = load ptr addrspace(1), ptr %obj.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
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
