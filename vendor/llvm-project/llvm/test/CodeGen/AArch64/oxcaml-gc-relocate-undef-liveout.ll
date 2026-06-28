; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s

declare void @callee()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)

define ptr addrspace(1) @relocate_undef_liveout(i1 %cond) gc "ocaml" {
; CHECK-LABEL: relocate_undef_liveout:
; CHECK: mov {{[wx][0-9]+}}, #1
; CHECK: ret
entry:
  br i1 %cond, label %safepoint, label %other

safepoint:
  %tok = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(
    i64 0, i32 0, ptr elementtype(void ()) @callee, i32 0, i32 0, i32 0, i32 0)
    [ "gc-live"(ptr addrspace(1) undef) ]
  %rel = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok, i32 0, i32 0)
  br label %join

other:
  br label %join

join:
  %phi = phi ptr addrspace(1) [ %rel, %safepoint ], [ null, %other ]
  ret ptr addrspace(1) %phi
}
