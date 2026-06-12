; The gc-root verifier accepts clean code: every gc value location live
; across these statepoints is listed (registers at the alloc statepoint,
; ISel spill slots at the ordinary call statepoints), so fatal mode must
; not fire.
; RUN: llc -mtriple=arm64-apple-macosx -oxcaml-gc-root-verifier \
; RUN:   -oxcaml-gc-root-verifier-fatal \
; RUN:   -stop-after=oxcaml-gc-root-verifier -o - %s | FileCheck %s

@callee = external global ptr
@caml_call_gc = external global ptr

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr,
                                                  i32 immarg, i32 immarg, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare void @use(ptr addrspace(1))

define void @loop_roots(ptr addrspace(1) %seed, i64 %n, i64 %domain,
                        i64 %alloc) gc "oxcaml" {
entry:
  br label %loop

loop:
  %root = phi ptr addrspace(1) [ %seed, %entry ], [ %root.rel2, %loop ]
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop ]
  %tok1 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0, ptr elementtype(void (i64, i64)) @caml_call_gc,
          i32 2, i32 0,
          i64 %domain, i64 %alloc,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root) ]
  %root.rel = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok1, i32 0, i32 0)
  %tok2 = call token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 2, i32 0, ptr elementtype(void (ptr addrspace(1))) @use,
          i32 1, i32 0,
          ptr addrspace(1) %root.rel,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root.rel) ]
  %root.rel2 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok2, i32 0, i32 0)
  %i.next = add nuw i64 %i, 1
  %done = icmp eq i64 %i.next, %n
  br i1 %done, label %exit, label %loop

exit:
  ret void
}

; CHECK-LABEL: name: loop_roots
; CHECK: STATEPOINT 1
; CHECK: STATEPOINT 2
