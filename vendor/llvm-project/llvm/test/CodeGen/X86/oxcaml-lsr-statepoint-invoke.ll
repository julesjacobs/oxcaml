; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu -O3 -stop-after=loop-reduce -o - | FileCheck %s

target triple = "x86_64-unknown-linux-gnu"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"LSR_statepoint_test"}

define void @test(i64 %ds, i64 %alloc) gc "oxcaml" personality ptr @caml_llvm_eh_personality {
; CHECK-LABEL: loop:
; CHECK-NOT: %lsr.iv
; CHECK: %i = phi i64 [ 1, %entry ], [ %next, %cont ]
; CHECK: %shift = shl nuw nsw i64 %i, 1
; CHECK: %tagged = or i64 %shift, 1
; CHECK: invoke {{.*}} i64 %tagged
; CHECK: %next = add nuw nsw i64 %i, 1
; CHECK: %done = icmp ugt i64 %i, 40000000
entry:
  br label %loop

loop:
  %i = phi i64 [ 1, %entry ], [ %next, %cont ]
  call void @llvm.x86.oxcaml.push.trap.with.domain(ptr blockaddress(@test, %landing), i64 %ds)
  %shift = shl nuw nsw i64 %i, 1
  %tagged = or i64 %shift, 1
  %tok = invoke oxcaml_fpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype(i64 (i64)) @callee, i32 1, i32 0, i64 %tagged, i32 0, i32 0)
          to label %normal unwind label %landing

normal:
  br label %cont

landing:
  %lp = landingpad token
          cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.x86.oxcaml.trap.recover(i64 0)
  br label %cont

cont:
  %next = add nuw nsw i64 %i, 1
  %done = icmp ugt i64 %i, 40000000
  br i1 %done, label %exit, label %loop

exit:
  ret void
}

declare i64 @callee(i64)
declare i32 @caml_llvm_eh_personality(...)
declare void @llvm.x86.oxcaml.push.trap.with.domain(ptr, i64)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.x86.oxcaml.trap.recover(i64)
declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)
