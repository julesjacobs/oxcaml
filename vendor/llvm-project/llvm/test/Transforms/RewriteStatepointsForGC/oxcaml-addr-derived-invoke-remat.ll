; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-remat-addrspace1-derived-from-base-at-alloc < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @consume(ptr addrspace(1)) "gc-leaf-function"
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_gep_normal_only(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_gep_normal_only(
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_gep_normal_only, %recover))
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 24
; CHECK: %statepoint_token = invoke {{.*}} @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj, ptr %obj.exnroot) ]
; CHECK-NEXT: to label %normal unwind label %recover
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
; CHECK: normal:
; CHECK: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: %field.addr.remat = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 24
; CHECK: %field = load ptr addrspace(1), ptr addrspace(1) %field.addr.remat
; CHECK-NOT: gc.relocate.p1(token %statepoint_token, i32 0, i32 1)
  %field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %pair = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %alloc2 = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %pair, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %alloc2, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NOT: gc.relocate
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %ret3 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret4 = insertvalue { i64, i64, ptr addrspace(1) } %ret3, i64 %recovered_alloc, 1
  %ret5 = insertvalue { i64, i64, ptr addrspace(1) } %ret4, ptr addrspace(1) %obj, 2
  ret { i64, i64, ptr addrspace(1) } %ret5
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_gep_repeated_statepoints(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_gep_repeated_statepoints(
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_gep_repeated_statepoints, %recover))
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 32
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj, ptr %obj.exnroot) ]
  %call1 = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %after1 unwind label %recover

after1:
; CHECK: after1:
; CHECK: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: %field.addr.remat{{[0-9]*}} = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 32
; CHECK: %field1 = load ptr addrspace(1), ptr addrspace(1) %field.addr.remat{{[0-9]*}}
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj.relocated, ptr %obj.exnroot) ]
  %field1 = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  call void @consume(ptr addrspace(1) %field1) "gc-leaf-function"
  %call2 = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
; CHECK: normal:
; CHECK: %field.addr.remat{{[0-9]*}} = getelementptr i8, ptr addrspace(1) %obj.relocated{{[0-9]*}}, i64 32
  %field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %pair = extractvalue { i64, i64, ptr addrspace(1) } %call2, 0
  %alloc2 = extractvalue { i64, i64, ptr addrspace(1) } %call2, 1
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %pair, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %alloc2, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2

recover:
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %ret3 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret4 = insertvalue { i64, i64, ptr addrspace(1) } %ret3, i64 %recovered_alloc, 1
  %ret5 = insertvalue { i64, i64, ptr addrspace(1) } %ret4, ptr addrspace(1) %obj, 2
  ret { i64, i64, ptr addrspace(1) } %ret5
}
