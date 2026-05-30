; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-remat-addrspace1-derived-from-base-at-alloc < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
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
; CHECK: %statepoint_token = invoke {{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj, ptr %obj.exnroot) ]
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %obj)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
; CHECK: normal:
; CHECK: %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0)
; CHECK: %field.addr.remat{{[0-9]*}} = getelementptr i8, ptr addrspace(1) %obj.relocated, i64 24
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
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %handler_field = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %ret3 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret4 = insertvalue { i64, i64, ptr addrspace(1) } %ret3, i64 %recovered_alloc, 1
  %ret5 = insertvalue { i64, i64, ptr addrspace(1) } %ret4, ptr addrspace(1) %handler_field, 2
  ret { i64, i64, ptr addrspace(1) } %ret5
}
