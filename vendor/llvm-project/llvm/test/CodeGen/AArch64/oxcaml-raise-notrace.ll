; RUN: opt -S -O2 < %s | FileCheck %s --check-prefix=O2
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s --check-prefix=ASM

declare void @llvm.aarch64.oxcaml.raise.notrace(i64)
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc void @dynamic_raise_notrace(i64 %exn) gc "oxcaml" {
entry:
  call void @llvm.aarch64.oxcaml.raise.notrace(i64 %exn)
  unreachable
}

define oxcaml_nofpcc void @dynamic_raise_notrace_callee(i64 %exn) noinline gc "oxcaml" {
entry:
  call void @llvm.aarch64.oxcaml.raise.notrace(i64 %exn)
  unreachable
}

define oxcaml_nofpcc i64 @invoke_dynamic_raise_notrace(i64 %ds, i64 %alloc, ptr %trap_block, i64 %exn) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 0,
      ptr blockaddress(@invoke_dynamic_raise_notrace, %recover))
  invoke oxcaml_nofpcc void @dynamic_raise_notrace_callee(i64 %exn)
          to label %normal unwind label %lpad

lpad:
  %lp = landingpad token cleanup
  br label %recover

recover:
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  ret i64 %bucket

normal:
  ret i64 0
}

; O2-LABEL: define oxcaml_nofpcc void @dynamic_raise_notrace_callee
; O2-NOT: nounwind
; O2: call void @llvm.aarch64.oxcaml.raise.notrace

; O2-LABEL: define oxcaml_nofpcc i64 @invoke_dynamic_raise_notrace
; O2: blockaddress(@invoke_dynamic_raise_notrace, %recover)
; O2-NOT: inttoptr (i32 1 to ptr)
; O2: invoke oxcaml_nofpcc void @dynamic_raise_notrace_callee
; O2: unwind label %lpad
; O2: recover:
; O2: call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover

; MIR-LABEL: name: dynamic_raise_notrace
; MIR: OXCAML_RAISE_NOTRACE
; MIR-NOT: BRK

; ASM-LABEL: _dynamic_raise_notrace:
; ASM: mov sp, x26
; ASM-NEXT: ldp x26, x16, [sp], #16
; ASM-NEXT: br x16
; ASM-NOT: brk

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"RaiseNotrace"}
