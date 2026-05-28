; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @recovery_before_invoke(i64 %ds, i64 %alloc, ptr %trap_block, i64 %value) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %value,
      ptr blockaddress(@recovery_before_invoke, %exn_entry))
  br label %call_block

exn_entry:
  %lp = landingpad token cleanup
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  %prev_trap = extractvalue { i64, i64, i64, i64 } %rec, 1
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %sum0 = add i64 %bucket, %prev_trap
  %sum1 = add i64 %sum0, %recovered_alloc
  %sum = add i64 %sum1, %recovered_ds
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %sum, 2
  ret { i64, i64, i64 } %ret2

call_block:
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 %value)
          to label %normal unwind label %exn_entry

normal:
  ret { i64, i64, i64 } %call
}

; CHECK-LABEL: name: recovery_before_invoke
; CHECK: OXCAML_TRAP_PUBLISH
; CHECK: bb.{{[0-9]+}}.exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"TrapRecoveryBeforeSource"}
