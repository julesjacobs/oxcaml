; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @callee(i64, i64, ptr addrspace(1))
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @boundary_local_root_value(
    i1 %retry,
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define oxcaml_nofpcc {{.*}} @boundary_local_root_value(
entry:
  br label %try

try:
; CHECK: try:
; CHECK: %[[CUR:.*]] = phi ptr addrspace(1)
; CHECK: %[[FIELD_ADDR:.*]] = getelementptr i8, ptr addrspace(1) %[[CUR]], i64 8
; CHECK: %[[FIELD:.*]] = load ptr addrspace(1), ptr addrspace(1) %[[FIELD_ADDR]], align 8
; CHECK: store volatile ptr addrspace(1) %[[FIELD]],
; CHECK: %statepoint_token = invoke
  %cur = phi ptr addrspace(1) [ %obj, %entry ], [ %next, %retry.block ]
  %field.addr = getelementptr i8, ptr addrspace(1) %cur, i64 8
  %next = load ptr addrspace(1), ptr addrspace(1) %field.addr, align 8
  %call = invoke oxcaml_nofpcc { i64, i64, ptr addrspace(1) }
      @callee(i64 %ds, i64 %alloc, ptr addrspace(1) %next)
      "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %recover

normal:
  ret { i64, i64, ptr addrspace(1) } %call

recover:
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %tag = ptrtoint ptr addrspace(1) %next to i64
  br i1 %retry, label %retry.block, label %exit

retry.block:
  br label %try

exit:
  %recover.ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %recover.alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recover.ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recover.alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %next, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}
