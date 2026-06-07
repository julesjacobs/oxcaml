; RUN: opt -passes=verify -disable-output < %s
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s > /dev/null

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"LandingpadGCRelocateOneInvoke"}

declare oxcaml_nofpcc void @callee()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc ptr addrspace(1) @one_invoke_handler_live(ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %token = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal unwind label %handler

normal:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %relocated
}

define oxcaml_nofpcc ptr addrspace(1) @one_invoke_normal_live(ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %token = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal unwind label %handler

normal:
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token, i32 0, i32 0)
  ret ptr addrspace(1) %relocated

handler:
  %lp = landingpad token cleanup
  ret ptr addrspace(1) null
}

define oxcaml_nofpcc ptr addrspace(1) @one_invoke_normal_and_handler_live(
    ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %token = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal unwind label %handler

normal:
  %normal.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token, i32 0, i32 0)
  ret ptr addrspace(1) %normal.relocated

handler:
  %lp = landingpad token cleanup
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}
