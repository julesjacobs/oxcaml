; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @duplicate_recovery_boundary_edge(
    i64 %tag,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @duplicate_recovery_boundary_edge(
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@duplicate_recovery_boundary_edge, %recover))
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_ds = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %normal_alloc = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  br label %join

recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  switch i64 %tag, label %other [
    i64 3, label %join
    i64 61, label %join
  ]

other:
  br label %join

join:
; CHECK: join:
; CHECK: %[[SELECT:.*]] = phi ptr addrspace(1) [ %obj.exnroot.load, %other ], [ %obj.exnroot.load{{[0-9]*}}, %recover ], [ %obj.exnroot.load{{[0-9]*}}, %recover ], [ %obj.relocated, %normal ]
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) %[[SELECT]], align 8
  %join_ds = phi i64 [ %normal_ds, %normal ], [ %recovered_ds, %recover ], [ %recovered_ds, %recover ], [ %recovered_ds, %other ]
  %join_alloc = phi i64 [ %normal_alloc, %normal ], [ %recovered_alloc, %recover ], [ %recovered_alloc, %recover ], [ %recovered_alloc, %other ]
  %field = load ptr addrspace(1), ptr addrspace(1) %obj, align 8
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %join_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %join_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}
