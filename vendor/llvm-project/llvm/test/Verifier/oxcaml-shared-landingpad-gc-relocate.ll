; RUN: not opt -passes=verify -disable-output < %s 2>&1 | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc void @callee_a()
declare oxcaml_nofpcc void @callee_b()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare i32 @personality(...)

define ptr addrspace(1) @non_oxcaml_shared_landingpad(
    i64 %choose, ptr addrspace(1) %obj)
    gc "statepoint-example" personality ptr @personality {
; CHECK: safepoints should have unique landingpads
entry:
  %take_a = icmp ne i64 %choose, 0
  br i1 %take_a, label %call_a, label %call_b

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
  ret ptr addrspace(1) null

normal_b:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %relocated
}

define oxcaml_nofpcc ptr addrspace(1) @shared_landingpad_bad_index(
    i64 %choose, ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @personality {
; CHECK: gc.relocate: statepoint base index out of bounds
entry:
  %take_a = icmp ne i64 %choose, 0
  br i1 %take_a, label %call_a, label %call_b

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
      [ "gc-live"() ]
      to label %normal_b unwind label %handler

normal_a:
  ret ptr addrspace(1) null

normal_b:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %relocated
}

define oxcaml_nofpcc ptr addrspace(1) @shared_landingpad_bad_addrspace(
    i64 %choose, ptr addrspace(1) %obj1, ptr addrspace(2) %obj2)
    gc "oxcaml" personality ptr @personality {
; CHECK: gc.relocate: relocating a pointer shouldn't change its address space
entry:
  %take_a = icmp ne i64 %choose, 0
  br i1 %take_a, label %call_a, label %call_b

call_a:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj1) ]
      to label %normal_a unwind label %handler

call_b:
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(2) %obj2) ]
      to label %normal_b unwind label %handler

normal_a:
  ret ptr addrspace(1) null

normal_b:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %relocated
}

define oxcaml_nofpcc ptr addrspace(1) @shared_landingpad_non_pointer(
    i64 %choose, ptr addrspace(1) %obj, i64 %not_a_ptr)
    gc "oxcaml" personality ptr @personality {
; CHECK: gc.relocate: relocated value must be a pointer
entry:
  %take_a = icmp ne i64 %choose, 0
  br i1 %take_a, label %call_a, label %call_b

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
      [ "gc-live"(i64 %not_a_ptr) ]
      to label %normal_b unwind label %handler

normal_a:
  ret ptr addrspace(1) null

normal_b:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %relocated
}
