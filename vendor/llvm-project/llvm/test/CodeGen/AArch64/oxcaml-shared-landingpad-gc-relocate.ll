; RUN: opt -passes=verify -disable-output < %s
; RUN: not llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s -o /dev/null 2>&1 | FileCheck %s --check-prefix=TODO

; TODO: LLVM ERROR: shared landingpad gc.relocate Machine PHI lowering not implemented

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"SharedLandingpadGCRelocate"}

declare oxcaml_nofpcc void @callee_a()
declare oxcaml_nofpcc void @callee_b()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc ptr addrspace(1) @alternative_invokes_handler_live(
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
  ret ptr addrspace(1) null

normal_b:
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}

define oxcaml_nofpcc ptr addrspace(1) @alternative_invokes_normal_and_handler_live(
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
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}

define oxcaml_nofpcc ptr addrspace(1) @sequential_invokes_handler_live(
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
  ret ptr addrspace(1) null

handler:
  %lp = landingpad token cleanup
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}

define oxcaml_nofpcc ptr addrspace(1) @sequential_invokes_normal_and_handler_live(
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
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}

define oxcaml_nofpcc { ptr addrspace(1), ptr addrspace(1) } @alternative_invokes_two_handler_live_gc_values(
    i64 %choose_a_word, ptr addrspace(1) %obj0, ptr addrspace(1) %obj1)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %choose_a = icmp ne i64 %choose_a_word, 0
  br i1 %choose_a, label %call_a, label %call_b

call_a:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj0, ptr addrspace(1) %obj1) ]
      to label %normal_a unwind label %handler

call_b:
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj0, ptr addrspace(1) %obj1) ]
      to label %normal_b unwind label %handler

normal_a:
  ret { ptr addrspace(1), ptr addrspace(1) } zeroinitializer

normal_b:
  ret { ptr addrspace(1), ptr addrspace(1) } zeroinitializer

handler:
  %lp = landingpad token cleanup
  %handler.relocated0 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  %handler.relocated1 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 1, i32 1)
  %ret0 = insertvalue { ptr addrspace(1), ptr addrspace(1) } poison,
      ptr addrspace(1) %handler.relocated0, 0
  %ret1 = insertvalue { ptr addrspace(1), ptr addrspace(1) } %ret0,
      ptr addrspace(1) %handler.relocated1, 1
  ret { ptr addrspace(1), ptr addrspace(1) } %ret1
}

define oxcaml_nofpcc ptr addrspace(1) @alternative_invokes_distinct_normal_and_handler_gc_values(
    i64 %choose_a_word, ptr addrspace(1) %handler_obj,
    ptr addrspace(1) %normal_obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %choose_a = icmp ne i64 %choose_a_word, 0
  br i1 %choose_a, label %call_a, label %call_b

call_a:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %handler_obj,
                  ptr addrspace(1) %normal_obj) ]
      to label %normal_a unwind label %handler

call_b:
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %handler_obj,
                  ptr addrspace(1) %normal_obj) ]
      to label %normal_b unwind label %handler

normal_a:
  %normal.a.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.a, i32 1, i32 1)
  br label %normal_join

normal_b:
  %normal.b.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.b, i32 1, i32 1)
  br label %normal_join

normal_join:
  %normal.relocated = phi ptr addrspace(1)
      [ %normal.a.relocated, %normal_a ], [ %normal.b.relocated, %normal_b ]
  ret ptr addrspace(1) %normal.relocated

handler:
  %lp = landingpad token cleanup
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}

define oxcaml_nofpcc i64 @alternative_invokes_gc_and_i64_live_in_normal_and_handler(
    i64 %choose_a_word, ptr addrspace(1) %obj, i64 %tag)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %tag.plus.one = add i64 %tag, 1
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
  %normal.a.raw = ptrtoint ptr addrspace(1) %normal.a.relocated to i64
  br label %normal_join

normal_b:
  %normal.b.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.b, i32 0, i32 0)
  %normal.b.raw = ptrtoint ptr addrspace(1) %normal.b.relocated to i64
  br label %normal_join

normal_join:
  %normal.raw = phi i64 [ %normal.a.raw, %normal_a ], [ %normal.b.raw, %normal_b ]
  %normal.sum = add i64 %normal.raw, %tag.plus.one
  ret i64 %normal.sum

handler:
  %lp = landingpad token cleanup
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  %handler.raw = ptrtoint ptr addrspace(1) %handler.relocated to i64
  %handler.sum = add i64 %handler.raw, %tag.plus.one
  ret i64 %handler.sum
}

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

define oxcaml_nofpcc { ptr addrspace(1), ptr addrspace(1) } @sequential_invokes_two_handler_live_gc_values(
    ptr addrspace(1) %obj0, ptr addrspace(1) %obj1)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj0, ptr addrspace(1) %obj1) ]
      to label %after_a unwind label %handler

after_a:
  %after.a.relocated0 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.a, i32 0, i32 0)
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %after.a.relocated0,
                  ptr addrspace(1) %obj1) ]
      to label %normal unwind label %handler

normal:
  ret { ptr addrspace(1), ptr addrspace(1) } zeroinitializer

handler:
  %lp = landingpad token cleanup
  %handler.relocated0 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  %handler.relocated1 = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 1, i32 1)
  %ret0 = insertvalue { ptr addrspace(1), ptr addrspace(1) } poison,
      ptr addrspace(1) %handler.relocated0, 0
  %ret1 = insertvalue { ptr addrspace(1), ptr addrspace(1) } %ret0,
      ptr addrspace(1) %handler.relocated1, 1
  ret { ptr addrspace(1), ptr addrspace(1) } %ret1
}

define oxcaml_nofpcc ptr addrspace(1) @sequential_invokes_distinct_normal_and_handler_gc_values(
    ptr addrspace(1) %handler_obj, ptr addrspace(1) %normal_obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %handler_obj,
                  ptr addrspace(1) %normal_obj) ]
      to label %after_a unwind label %handler

after_a:
  %after.a.handler = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.a, i32 0, i32 0)
  %after.a.normal = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.a, i32 1, i32 1)
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %after.a.handler,
                  ptr addrspace(1) %after.a.normal) ]
      to label %normal unwind label %handler

normal:
  %normal.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %token.b, i32 1, i32 1)
  ret ptr addrspace(1) %normal.relocated

handler:
  %lp = landingpad token cleanup
  %handler.relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  ret ptr addrspace(1) %handler.relocated
}
