; RUN: opt -passes=verify -disable-output < %s
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s > /dev/null
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s 2>&1 | FileCheck %s --check-prefix=MIR

; MIR-LABEL: name:            shared_trap_recovery
; MIR: bb.{{[0-9]+}}.lpad (landing-pad):
; MIR-NOT: PHI
; MIR: bb.{{[0-9]+}}.recover ({{.*}}runtime-entered{{.*}}):
; MIR: PHI {{.*}}, %bb.{{[0-9]+}}, {{.*}}, %bb.{{[0-9]+}}, {{.*}}, %bb.{{[0-9]+}}

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"SharedTrapLpadRelocate"}

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc void @callee_a()
declare oxcaml_nofpcc void @callee_b()
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc ptr addrspace(1) @shared_trap_recovery(
    i64 %choose_word, ptr addrspace(1) %obj)
    gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@shared_trap_recovery, %recover))
  %choose = icmp ne i64 %choose_word, 0
  br i1 %choose, label %call_a, label %call_b

call_a:
  %token.a = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_a,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal_a unwind label %lpad

call_b:
  %token.b = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0, ptr elementtype(void ()) @callee_b,
          i32 0, i32 0, i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %obj) ]
      to label %normal_b unwind label %lpad

normal_a:
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret ptr addrspace(1) null

normal_b:
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret ptr addrspace(1) null

lpad:
  %lp = landingpad token cleanup
  %relocated = call ptr addrspace(1)
      @llvm.experimental.gc.relocate.p1(token %lp, i32 0, i32 0)
  br label %recover

recover:
  %rec = call { ptr addrspace(1), i64, i64, i64 }
      @llvm.aarch64.oxcaml.trap.recover()
  ret ptr addrspace(1) %relocated
}
