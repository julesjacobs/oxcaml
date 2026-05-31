; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { i64, i64, i64 } @callee(i64, i64, i64)
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

define oxcaml_nofpcc i64 @trampoline_phi_i64(
    i1 %cond,
    i64 %ds,
    i64 %alloc,
    ptr %trap_block,
    i64 %left_value,
    i64 %right_value)
    gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define oxcaml_nofpcc i64 @trampoline_phi_i64(
; CHECK: %x.recoverphi = alloca i64, align
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@trampoline_phi_i64, %recover))
  br i1 %cond, label %left, label %right

left:
; CHECK: left:
; CHECK-NEXT: store i64 %left_value, ptr %x.recoverphi, align
; CHECK-NEXT: %statepoint_token = invoke
; CHECK-NEXT: to label %{{.*}} unwind label %lpad
  %left_call = invoke oxcaml_nofpcc { i64, i64, i64 }
      @callee(i64 %ds, i64 %alloc, i64 %left_value)
      "statepoint-id"="0" [ "deopt"() ]
      to label %left_normal unwind label %lpad

left_normal:
  %left_result = extractvalue { i64, i64, i64 } %left_call, 2
  ret i64 %left_result

right:
; CHECK: right:
; CHECK-NEXT: store i64 %right_value, ptr %x.recoverphi, align
; CHECK-NEXT: %statepoint_token{{[0-9]*}} = invoke
; CHECK-NEXT: to label %{{.*}} unwind label %lpad
  %right_call = invoke oxcaml_nofpcc { i64, i64, i64 }
      @callee(i64 %ds, i64 %alloc, i64 %right_value)
      "statepoint-id"="0" [ "deopt"() ]
      to label %right_normal unwind label %lpad

right_normal:
  %right_result = extractvalue { i64, i64, i64 } %right_call, 2
  ret i64 %right_result

lpad:
; CHECK: lpad:
; CHECK-NOT: phi i64
; CHECK-NEXT: br label %recover
  %x = phi i64 [ %left_value, %left ], [ %right_value, %right ]
  %lp = landingpad token cleanup
  br label %recover

recover:
; CHECK: recover:
; CHECK-NEXT: %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
; CHECK-NEXT: %x.recoverphi.load = load i64, ptr %x.recoverphi, align
; CHECK: ret i64 %x.recoverphi.load
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  ret i64 %x
}
