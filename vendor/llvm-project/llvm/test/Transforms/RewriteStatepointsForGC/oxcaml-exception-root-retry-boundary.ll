; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @callee(i64, i64, ptr addrspace(1))
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @retrying_boundary_exit_uses_selected_root(
    i1 %take_try,
    i1 %retry,
    i1 %exit_after_check,
    ptr addrspace(1) %f)
    gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define {{.*}} @retrying_boundary_exit_uses_selected_root(
; The argument's root slot is stored once at entry; retries reuse the slot,
; whose content the collector keeps current, so no store precedes the invoke.
; CHECK: entry:
; CHECK: %f.exnroot = alloca ptr addrspace(1), align 8
; CHECK: store ptr addrspace(1) %f, ptr %f.exnroot, align 8
entry:
  br i1 %take_try, label %try.preheader, label %exit

try.preheader:
  br label %try

try:
; CHECK: try:
; CHECK-NEXT: %[[F_SELECT:.*]] = phi ptr addrspace(1) [ %f.exnroot.load{{[0-9]*}}, %check ], [ %f, %try.preheader ]
; CHECK-NOT: store volatile
; CHECK: %statepoint_token = invoke {{.*}} ptr addrspace(1) %[[F_SELECT]], {{.*}} [ "gc-live"(ptr %f.exnroot) ]
  %call = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } }
      @callee(i64 0, i64 0, ptr addrspace(1) %f)
      "statepoint-id"="18"
      to label %normal unwind label %recover

normal:
  ret { { i64, i64 }, { ptr addrspace(1) } } %call

recover:
; CHECK: recover:
; CHECK: %f.exnroot.load = load ptr addrspace(1), ptr %f.exnroot, align 8
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br i1 %retry, label %check, label %exit.recover.direct

check:
  br i1 %exit_after_check, label %exit.recover.checked, label %try

exit.recover.direct:
  br label %exit

exit.recover.checked:
  br label %exit

exit:
; CHECK: exit:
; CHECK-NEXT: %[[F_EXIT:.*]] = phi ptr addrspace(1) [ %f.exnroot.load{{[0-9]*}}, %exit.recover.checked ], [ %f.exnroot.load{{[0-9]*}}, %exit.recover.direct ], [ %f, %entry ]
; CHECK: load i64, ptr addrspace(1) %[[F_EXIT]], align 8
  %code = load i64, ptr addrspace(1) %f, align 8
  %code.ptr = inttoptr i64 %code to ptr
  %tail = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } }
      %code.ptr(i64 0, i64 0, ptr addrspace(1) %f)
  ret { { i64, i64 }, { ptr addrspace(1) } } %tail
}
