; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-remat-addrspace1-derived-from-base-at-alloc < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_gep_handler_live(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_gep_handler_live(
; CHECK: %obj.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_gep_handler_live, %recover))
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 24
; CHECK: store ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr %obj.exnroot{{.*}}) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
; CHECK: normal:
; CHECK-NOT: gc.relocate
; CHECK: %obj.exnroot.normal.load = load ptr addrspace(1), ptr %obj.exnroot, align 8
; CHECK: %field.addr.remat{{[0-9]*}} = getelementptr i8, ptr addrspace(1) %obj.exnroot.normal.load, i64 24
  %normal_field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %pair = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %alloc2 = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %pair, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %alloc2, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %normal_field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2

recover:
; CHECK: recover:
; CHECK: %obj.exnroot.load = load ptr addrspace(1), ptr %obj.exnroot, align 8
; CHECK: %field.addr.remat{{[0-9]*}} = getelementptr i8, ptr addrspace(1) %obj.exnroot.load, i64 24
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %handler_field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %ret3 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret4 = insertvalue { i64, i64, ptr addrspace(1) } %ret3, i64 %recovered_alloc, 1
  %ret5 = insertvalue { i64, i64, ptr addrspace(1) } %ret4, ptr addrspace(1) %handler_field, 2
  ret { i64, i64, ptr addrspace(1) } %ret5
}

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @invoke_gep_nested_recovery_trap(
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @invoke_gep_nested_recovery_trap(
; CHECK: %obj.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@invoke_gep_nested_recovery_trap, %recover))
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 24
; CHECK: store ptr addrspace(1) %obj, ptr %obj.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr %obj.exnroot{{.*}}) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  %normal_field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %pair = extractvalue { i64, i64, ptr addrspace(1) } %call, 0
  %alloc2 = extractvalue { i64, i64, ptr addrspace(1) } %call, 1
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %pair, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %alloc2, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %normal_field, 2
  ret { i64, i64, ptr addrspace(1) } %ret2

recover:
; CHECK: recover:
; CHECK: %obj.exnroot.load = load ptr addrspace(1), ptr %obj.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  br label %nested

nested:
; CHECK: nested:
; CHECK: %obj.exnroot.select = phi ptr addrspace(1) [ %obj.exnroot.load, %recover ]
; The inner protected region reuses the outer value's slot; no second slot
; or store is materialized for the selector.
; CHECK: call void @llvm.aarch64.oxcaml.push.trap
; CHECK-NOT: store volatile
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} [ "deopt"(), "gc-live"(ptr %obj.exnroot) ]
  call void @llvm.aarch64.oxcaml.push.trap(
      ptr blockaddress(@invoke_gep_nested_recovery_trap, %inner_recover))
  %inner_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %recovered_ds, i64 %recovered_alloc,
              ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)))
      "statepoint-id"="0" [ "deopt"() ]
      to label %inner_normal unwind label %inner_recover

inner_normal:
; CHECK: inner_normal:
; CHECK-NOT: gc.relocate
; CHECK: %obj.exnroot.select.exnroot.normal.load = load ptr addrspace(1), ptr %obj.exnroot, align 8
; CHECK: %field.addr.remat{{[0-9]*}} = getelementptr i8, ptr addrspace(1) %obj.exnroot.select.exnroot.normal.load, i64 24
; CHECK: %inner_field = load ptr addrspace(1), ptr addrspace(1) %field.addr.remat{{[0-9]*}}, align 8
  %inner_field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %inner_pair = extractvalue { i64, i64, ptr addrspace(1) } %inner_call, 0
  %inner_alloc = extractvalue { i64, i64, ptr addrspace(1) } %inner_call, 1
  %ret3 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %inner_pair, 0
  %ret4 = insertvalue { i64, i64, ptr addrspace(1) } %ret3, i64 %inner_alloc, 1
  %ret5 = insertvalue { i64, i64, ptr addrspace(1) } %ret4, ptr addrspace(1) %inner_field, 2
  ret { i64, i64, ptr addrspace(1) } %ret5

inner_recover:
  %inner_lp = landingpad token cleanup
  %inner_rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %inner_recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %inner_rec, 2
  %inner_recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %inner_rec, 3
  %ret6 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %inner_recovered_ds, 0
  %ret7 = insertvalue { i64, i64, ptr addrspace(1) } %ret6, i64 %inner_recovered_alloc, 1
  %ret8 = insertvalue { i64, i64, ptr addrspace(1) } %ret7, ptr addrspace(1) %obj, 2
  ret { i64, i64, ptr addrspace(1) } %ret8
}
