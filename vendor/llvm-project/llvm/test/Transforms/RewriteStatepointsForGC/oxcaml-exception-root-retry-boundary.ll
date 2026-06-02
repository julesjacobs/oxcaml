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
entry:
  br i1 %take_try, label %try.preheader, label %exit

try.preheader:
  br label %try

try:
; CHECK: {{^}}try:
; CHECK-NEXT: %[[F_TRY_SELECT:.*]] = phi ptr addrspace(1) [ %[[F_RECOVER:.*]], %check ], [ %f, %try.preheader ]
; CHECK-NEXT: %statepoint_token = invoke
; CHECK-SAME: ptr addrspace(1) %[[F_TRY_SELECT]]
; CHECK-SAME: [ "oxcaml-eh-live"(i32 0, i32 0, ptr addrspace(1) %[[F_TRY_SELECT]]) ]
; CHECK-NOT: store volatile ptr addrspace(1) %f,
  %call = invoke oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } }
      @callee(i64 0, i64 0, ptr addrspace(1) %f)
      "statepoint-id"="18"
      to label %normal unwind label %recover

normal:
  ret { { i64, i64 }, { ptr addrspace(1) } } %call

recover:
; CHECK: {{^}}recover:
; CHECK: %[[F_RECOVER]] = call ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 0, i32 0)
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
; CHECK: {{^}}exit:
; CHECK-NEXT: %[[F_EXIT_SELECT:.*]] = phi ptr addrspace(1) [ %[[F_RECOVER]], %exit.recover.checked ], [ %[[F_RECOVER]], %exit.recover.direct ], [ %f, %entry ]
; CHECK-NEXT: %code = load i64, ptr addrspace(1) %[[F_EXIT_SELECT]], align 8
  %code = load i64, ptr addrspace(1) %f, align 8
  %code.ptr = inttoptr i64 %code to ptr
  %tail = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } }
      %code.ptr(i64 0, i64 0, ptr addrspace(1) %f)
  ret { { i64, i64 }, { ptr addrspace(1) } } %tail
}
