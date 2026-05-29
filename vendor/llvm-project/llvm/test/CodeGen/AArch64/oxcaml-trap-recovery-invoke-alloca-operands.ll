; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s
; RUN: opt -S -O2 < %s | llc -mtriple=arm64-apple-macosx -verify-machineinstrs > /dev/null

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @recovery_uses_entry_alloca(i64 %ds, i64 %alloc, ptr %trap_block, i64 %value) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %handler_bucket = alloca i64
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %value,
      ptr blockaddress(@recovery_uses_entry_alloca, %exn_entry))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 %value)
          to label %normal unwind label %exn_entry

normal:
  ret { i64, i64, i64 } %call

exn_entry:
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  call void asm sideeffect "; restore stack marker", ""()
  store i64 %bucket, ptr %handler_bucket
  %saved_bucket = load i64, ptr %handler_bucket
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %saved_bucket, 2
  ret { i64, i64, i64 } %ret2
}

; CHECK-LABEL: name: recovery_uses_entry_alloca
; CHECK: OXCAML_TRAP_PUBLISH
; CHECK: bb.{{[0-9]+}}.exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, {{(landing-pad, )?}}runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"TrapRecoveryAlloca"}
