; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"Test"}

declare oxcaml_nofpcc void @callee_v(i64, i64, ptr addrspace(1))
declare void @use2(ptr addrspace(1), ptr addrspace(1))
declare ptr @personality(...)
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 immarg, i32 immarg)

define oxcaml_nofpcc void @dup_eh_roots(
    i64 %cond_int, i64 %ds, i64 %alloc, ptr addrspace(1) %a,
    ptr addrspace(1) %b, ptr addrspace(1) %c)
    gc "oxcaml" personality ptr @personality {
entry:
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %left, label %right

left:
  %tok1 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 2882400000, i32 0,
          ptr elementtype(void (i64, i64, ptr addrspace(1))) @callee_v,
          i32 3, i32 0, i64 %ds, i64 %alloc, ptr addrspace(1) %a,
          i32 0, i32 0)
      [ "oxcaml-eh-live"(i32 0, i32 0, ptr addrspace(1) %a,
                          i32 0, i32 1, ptr addrspace(1) %a) ]
      to label %normal_left unwind label %recover

normal_left:
  ret void

right:
  %tok2 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 2882400000, i32 0,
          ptr elementtype(void (i64, i64, ptr addrspace(1))) @callee_v,
          i32 3, i32 0, i64 %ds, i64 %alloc, ptr addrspace(1) %b,
          i32 0, i32 0)
      [ "oxcaml-eh-live"(i32 0, i32 0, ptr addrspace(1) %b,
                          i32 0, i32 1, ptr addrspace(1) %c) ]
      to label %normal_right unwind label %recover

normal_right:
  ret void

recover:
  %lp = landingpad token cleanup
  %p = call ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 0, i32 0)
  %q = call ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 0, i32 1)
  call void @use2(ptr addrspace(1) %p, ptr addrspace(1) %q)
  ret void
}

; CHECK-LABEL: name: dup_eh_roots
; CHECK: STATEPOINT 2882400000, 0, 3, @callee_v,
; Two logical EH roots receive %a on the left edge.  They must use separate
; stack homes and both homes must appear in the GC pointer map as self-pairs.
; CHECK-SAME: 2, 2, 1, 8, [[ROOT0:%stack\.[0-9]+]], 0, 1, 8, [[ROOT1:%stack\.[0-9]+]], 0, 2, 0, 2, 2, 0, 0, 1, 1,
