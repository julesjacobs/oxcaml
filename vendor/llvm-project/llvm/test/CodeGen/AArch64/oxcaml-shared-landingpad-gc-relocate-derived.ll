; RUN: opt -passes=verify -disable-output < %s
; RUN: (llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s -o /dev/null 2>&1 || true) | FileCheck %s

; CHECK: LLVM ERROR: OxCaml statepoint contains a derived GC pointer in alternative_invokes_derived_handler_live

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"SharedLandingpadGCRelocateDerived"}

declare oxcaml_nofpcc void @callee_a()
declare oxcaml_nofpcc void @callee_b()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc ptr addrspace(1) @alternative_invokes_derived_handler_live(
    i64 %choose_a_word, ptr addrspace(1) %base)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %derived = getelementptr i8, ptr addrspace(1) %base, i64 8
  %choose_a = icmp ne i64 %choose_a_word, 0
  br i1 %choose_a, label %call_a, label %call_b

call_a:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %base, ptr addrspace(1) %derived) ]
      to label %normal_a unwind label %handler

call_b:
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %base, ptr addrspace(1) %derived) ]
      to label %normal_b unwind label %handler

normal_a:
  ret ptr addrspace(1) null

normal_b:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %handler.derived = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 1)
  ret ptr addrspace(1) %handler.derived
}
