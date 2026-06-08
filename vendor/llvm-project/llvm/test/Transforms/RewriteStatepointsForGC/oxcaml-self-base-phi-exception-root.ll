; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @loop_carried_self_base_phi_exception_root(
    i1 %again,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @loop_carried_self_base_phi_exception_root(
; CHECK-NOT: .base.exnroot
; CHECK-NOT: .base.recoverphi
; CHECK: %root.exnroot = alloca ptr addrspace(1), align 8
; CHECK: store volatile ptr addrspace(1) %root, ptr %root.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr %root.exnroot) ]
; CHECK: %root.exnroot.normal.load = load volatile ptr addrspace(1), ptr %root.exnroot, align 8
; CHECK: %root.exnroot.load = load volatile ptr addrspace(1), ptr %root.exnroot, align 8
; CHECK: %root.exnroot.select = phi ptr addrspace(1) [ %root.exnroot.load, %recover ], [ %root.exnroot.normal.load, %normal ]
; CHECK-NOT: .base.exnroot
; CHECK-NOT: .base.recoverphi
entry:
  br label %loop

loop:
  %root = phi ptr addrspace(1) [ %obj, %entry ], [ %next, %join ]
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@loop_carried_self_base_phi_exception_root, %recover))
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %root)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  br label %join

join:
  %join_ds = phi i64 [ %normal_ds, %normal ], [ %recovered_ds, %recover ]
  %join_alloc = phi i64 [ %normal_alloc, %normal ], [ %recovered_alloc, %recover ]
  %live = load ptr addrspace(1), ptr addrspace(1) %root, align 8
  %next = load ptr addrspace(1), ptr addrspace(1) %live, align 8
  br i1 %again, label %loop, label %exit

exit:
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %next, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}
