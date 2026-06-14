; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc i64 @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc i64 @direct_landingpad_recovery(i64 %ds, i64 %alloc, i64 %arg) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %saved = load i64, ptr inttoptr (i64 4096 to ptr), align 8
  call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@direct_landingpad_recovery, %recover))
  %v = invoke oxcaml_nofpcc i64 @callee(i64 %ds, i64 %alloc, i64 %arg)
          to label %normal unwind label %recover

normal:
  ret i64 %v

recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %sum = add i64 %recovered_ds, %saved
  ret i64 %sum
}

; CHECK-LABEL: name: direct_landingpad_recovery
; CHECK: OXCAML_PUSH_TRAP %bb.[[RECOVER:[0-9]+]]
; CHECK: bb.[[RECOVER]].recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, landing-pad, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28
; CHECK: EH_LABEL
; CHECK-NEXT: {{%[0-9]+}}:gpr64all = COPY $x0
; CHECK-NEXT: {{%[0-9]+}}:gpr64all = COPY $x26
; CHECK-NEXT: {{%[0-9]+}}:gpr64all = COPY $x27
; CHECK-NEXT: {{%[0-9]+}}:gpr64{{(sp)?}} = COPY $x28

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"NativeTrapDirectLandingpad"}
