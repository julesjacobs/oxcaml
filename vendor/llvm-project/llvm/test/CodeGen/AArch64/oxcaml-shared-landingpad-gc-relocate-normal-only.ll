; RUN: opt -passes=verify -disable-output < %s
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s > /dev/null

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"SharedLandingpadGCRelocateNormalOnly"}

declare oxcaml_nofpcc void @callee_a()
declare oxcaml_nofpcc void @callee_b()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc ptr addrspace(1) @alternative_invokes_normal_live(
    i64 %choose_a_word, ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %choose_a = icmp ne i64 %choose_a_word, 0
  br i1 %choose_a, label %call_a, label %call_b

call_a:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal_a unwind label %handler

call_b:
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal_b unwind label %handler

normal_a:
  %normal.a.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.a, i32 0, i32 0)
  br label %normal_join

normal_b:
  %normal.b.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.b, i32 0, i32 0)
  br label %normal_join

normal_join:
  %normal.relocated = phi ptr addrspace(1)
      [ %normal.a.relocated, %normal_a ], [ %normal.b.relocated, %normal_b ]
  ret ptr addrspace(1) %normal.relocated

handler:
  %lp = landingpad token cleanup
  ret ptr addrspace(1) null
}

define oxcaml_nofpcc ptr addrspace(1) @sequential_invokes_normal_live(
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %after_a unwind label %handler

after_a:
  %after.a.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.a, i32 0, i32 0)
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %after.a.relocated) ]
      to label %normal unwind label %handler

normal:
  %normal.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.b, i32 0, i32 0)
  ret ptr addrspace(1) %normal.relocated

handler:
  %lp = landingpad token cleanup
  ret ptr addrspace(1) null
}
