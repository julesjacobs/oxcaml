; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s --check-prefix=ASM

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.raise.notrace.edge(i64, i64, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc i64 @raise_notrace_edge_intrinsic(i64 %ds, i64 %alloc, i64 %exn) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@raise_notrace_edge_intrinsic, %recover))
  invoke void @llvm.aarch64.oxcaml.raise.notrace.edge(
      i64 %exn, i64 %ds, i64 %alloc,
      ptr blockaddress(@raise_notrace_edge_intrinsic, %recover))
          to label %dead unwind label %recover

dead:
  unreachable

recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 0
  %bucket.raw = ptrtoint ptr addrspace(1) %bucket to i64
  ret i64 %bucket.raw
}

; MIR-LABEL: name: raise_notrace_edge_intrinsic
; MIR: OXCAML_PUSH_TRAP %bb.[[RECOVER:[0-9]+]]
; MIR: OXCAML_RAISE_NOTRACE_EDGE {{%[0-9]+}}, %bb.[[RECOVER]]
; MIR-NOT: BL @llvm.aarch64.oxcaml.raise.notrace.edge
; MIR: bb.[[RECOVER]].recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, landing-pad, runtime-entered):

; ASM-LABEL: _raise_notrace_edge_intrinsic:
; ASM-NOT: bl
; ASM: mov sp, x26
; ASM-NEXT: ldp x26, x16, [sp], #16
; ASM-NEXT: br x16

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"RaiseNotraceEdgeIntrinsic"}
