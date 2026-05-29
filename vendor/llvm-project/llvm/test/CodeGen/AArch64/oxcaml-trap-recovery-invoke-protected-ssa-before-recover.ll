; RUN: not --crash llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s 2>&1 | FileCheck %s

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @protected_ssa_before_recover(i64 %ds, i64 %alloc, i64 %value) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(
      ptr blockaddress(@protected_ssa_before_recover, %recover))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 %value)
          to label %normal unwind label %lpad

normal:
  ret { i64, i64, i64 } %call

lpad:
  %lp = landingpad token cleanup
  br label %recover

recover:
  %bad = add i64 %value, 1
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %bad, 2
  ret { i64, i64, i64 } %ret2
}

; CHECK: trap recovery intrinsic must be in a runtime-entered ABI block

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"TrapRecoveryProtectedSSABeforeRecover"}
