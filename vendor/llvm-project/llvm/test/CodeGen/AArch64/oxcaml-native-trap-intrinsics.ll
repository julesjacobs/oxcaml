; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s --check-prefix=ASM

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc i64 @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc i64 @native_trap_intrinsics(i64 %ds, i64 %alloc, i64 %arg) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@native_trap_intrinsics, %recover))
  %v = invoke oxcaml_nofpcc i64 @callee(i64 %ds, i64 %alloc, i64 %arg)
          to label %normal unwind label %lpad

normal:
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret i64 %v

lpad:
  %lp = landingpad token cleanup
  br label %recover

recover:
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 0
  %bucket.raw = ptrtoint ptr addrspace(1) %bucket to i64
  ret i64 %bucket.raw
}

; MIR-LABEL: name: native_trap_intrinsics
; MIR: OXCAML_PUSH_TRAP %bb.[[RECOVER:[0-9]+]]
; MIR: OXCAML_POP_TRAP
; MIR: bb.[[RECOVER]].recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, {{(landing-pad, )?}}runtime-entered):
; MIR-NEXT: successors:
; MIR-NEXT: liveins: $x0, $x26, $x27, $x28

; ASM-LABEL: _native_trap_intrinsics:
; ASM: adr x16, [[RECOVER:LBB[0-9]+_[0-9]+]]
; ASM-NEXT: stp x26, x16, [sp, #-16]!
; ASM-NEXT: mov x26, sp
; ASM: bl _callee
; ASM: ldp x26, x16, [sp], #16
; ASM-NOT: OxCaml trap recovery stack restore
; ASM: [[RECOVER]]:

define oxcaml_nofpcc i64 @native_trap_deleted_target(i64 %arg) gc "oxcaml" {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(ptr inttoptr (i32 1 to ptr))
  %v = add i64 %arg, 1
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret i64 %v
}

; MIR-LABEL: name: native_trap_deleted_target
; MIR: OXCAML_PUSH_TRAP_DEAD
; MIR: OXCAML_POP_TRAP

; ASM-LABEL: _native_trap_deleted_target:
; ASM: adr x16, [[DEAD:Ltmp[0-9]+]]
; ASM-NEXT: stp x26, x16, [sp, #-16]!
; ASM-NEXT: mov x26, sp
; ASM-NEXT: [[DEAD]]:
; ASM: ldp x26, x16, [sp], #16

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"NativeTrapIntrinsics"}
