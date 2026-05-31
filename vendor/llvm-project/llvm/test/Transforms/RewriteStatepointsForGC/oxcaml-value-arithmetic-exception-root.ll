; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @exception_root_value_arithmetic_phi(
    i64 %cond_int,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    ptr addrspace(1) %a,
    ptr addrspace(1) %b)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @exception_root_value_arithmetic_phi(
; CHECK: %handler_value.exnroot = alloca ptr addrspace(1), align 8
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@exception_root_value_arithmetic_phi, %recover))
  %a.i = ptrtoint ptr addrspace(1) %a to i64
  %adjusted.i = add i64 %a.i, -2
  %adjusted = inttoptr i64 %adjusted.i to ptr addrspace(1)
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %left, label %right

left:
; CHECK: left:
; CHECK-NEXT: store ptr addrspace(1) %adjusted, ptr %handler_value.exnroot, align 8
; CHECK: %statepoint_token = invoke {{.*}} @llvm.experimental.gc.statepoint
; CHECK-SAME: ptr %handler_value.exnroot
  %left_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %recover

normal_left:
  ret { i64, i64, ptr addrspace(1) } %left_call

right:
; CHECK: right:
; CHECK-NEXT: store ptr addrspace(1) %b, ptr %handler_value.exnroot, align 8
; CHECK: %statepoint_token{{[0-9]*}} = invoke {{.*}} @llvm.experimental.gc.statepoint
; CHECK-SAME: ptr %handler_value.exnroot
  %right_call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %recover

normal_right:
  ret { i64, i64, ptr addrspace(1) } %right_call

recover:
; CHECK: recover:
; CHECK-NEXT: %lp = landingpad token
; CHECK-NEXT: cleanup
; CHECK-NEXT: %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK: %handler_value.exnroot.load = load ptr addrspace(1), ptr %handler_value.exnroot, align 8
  %handler_value = phi ptr addrspace(1) [ %adjusted, %left ], [ %b, %right ]
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %handler_value, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}
