; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()
declare oxcaml_nofpcc void @callee(i64, i64, ptr addrspace(1))
declare void @use(ptr addrspace(1)) "gc-leaf-function"
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

define void @normal_safepoint_before_handler_only_eh_root(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %arg,
    ptr addrspace(1) %handler_root)
    gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define void @normal_safepoint_before_handler_only_eh_root(
; CHECK: entry:
; CHECK: %[[SP:.*]] = call token {{.*}} @llvm.experimental.gc.statepoint{{.*}} @may_gc{{.*}} [ "deopt"(), "gc-live"({{.*}}ptr addrspace(1) %handler_root{{.*}}) ]
; CHECK: %[[HANDLER_ROOT_RELOCATED:.*]] = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %[[SP]], i32 {{[0-9]+}}, i32 {{[0-9]+}}) ; (%handler_root, %handler_root)
; CHECK: %[[INVOKE:.*]] = invoke oxcaml_nofpcc token {{.*}} @llvm.experimental.gc.statepoint{{.*}} @callee
; CHECK-SAME: "oxcaml-eh-live"({{.*}}ptr addrspace(1) %[[HANDLER_ROOT_RELOCATED]]
; CHECK: recover:
; CHECK: landingpad token
; CHECK: %{{.*}} = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK: %[[RECOVERED:.*]] = call ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 0, i32 0)
; CHECK: call void @use(ptr addrspace(1) %[[RECOVERED]])
entry:
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  invoke oxcaml_nofpcc void @callee(
      i64 %ds,
      i64 %alloc,
      ptr addrspace(1) %arg)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  ret void

recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %handler_root)
  ret void
}
