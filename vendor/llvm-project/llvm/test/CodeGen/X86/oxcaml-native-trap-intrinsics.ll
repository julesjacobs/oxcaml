; RUN: llc -mtriple=x86_64-unknown-linux-gnu -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR
; RUN: llc -mtriple=x86_64-unknown-linux-gnu -verify-machineinstrs < %s | FileCheck %s --check-prefix=ASM

declare void @llvm.x86.oxcaml.push.trap.with.domain(ptr, i64)
declare void @llvm.x86.oxcaml.pop.trap.with.domain(i64)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.x86.oxcaml.trap.recover(i64)
declare oxcaml_nofpcc i64 @callee(i64, i64, i64) gc "oxcaml"
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc i64 @native_trap_intrinsics(i64 %ds, i64 %alloc, i64 %arg) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.x86.oxcaml.push.trap.with.domain(
      ptr blockaddress(@native_trap_intrinsics, %recover), i64 %ds)
  %v = invoke oxcaml_nofpcc i64 @callee(i64 %ds, i64 %alloc, i64 %arg)
          to label %normal unwind label %lpad

normal:
  call void @llvm.x86.oxcaml.pop.trap.with.domain(i64 %ds)
  ret i64 %v

lpad:
  %lp = landingpad token cleanup
  br label %recover

recover:
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.x86.oxcaml.trap.recover(i64 0)
  %bucket = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 0
  %bucket.raw = ptrtoint ptr addrspace(1) %bucket to i64
  ret i64 %bucket.raw
}

; MIR-LABEL: name: native_trap_intrinsics
; MIR: bb.0.entry:
; MIR: successors: %bb.1({{.*}}), %bb.[[RECOVER:[0-9]+]]
; MIR: [[DS:%[0-9]+]]:gr64pltsafe = COPY $r14
; MIR: OXCAML_PUSH_TRAP %bb.[[RECOVER]], [[DS]]
; MIR: OXCAML_POP_TRAP [[DS]]
; MIR: bb.[[RECOVER]].recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, {{(landing-pad, )?}}runtime-entered):

; ASM-LABEL: native_trap_intrinsics:
; ASM: leaq [[RECOVER:.LBB[0-9]+_[0-9]+]](%rip), %r11
; ASM-NEXT: pushq %r11
; ASM-NEXT: pushq {{[0-9]+}}(%r14)
; ASM-NEXT: movq %rsp, {{[0-9]+}}(%r14)
; ASM: callq callee@PLT
; ASM: popq {{[0-9]+}}(%{{r14|rax}})
; ASM-NEXT: popq %r11
; ASM-NEXT: popq %rax
; ASM: [[RECOVER]]:

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"X86NativeTrapIntrinsics"}
