; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=aarch64-oxcaml-runtime-entry < %s | FileCheck %s --check-prefix=RUNTIME

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @recover_after_setup(i64 %ds, i64 %alloc, i64 %value) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  %frame = alloca i8, i64 64, align 1
  %frame_int = ptrtoint ptr %frame to i64
  %aligned_plus = add i64 %frame_int, 15
  %trap_int = and i64 %aligned_plus, -16
  %trap_block = inttoptr i64 %trap_int to ptr
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %value,
      ptr blockaddress(@recover_after_setup, %exn_entry))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 %value)
          to label %normal unwind label %exn_entry

exn_entry:
  %lp = landingpad token cleanup
  %slot_int = add i64 %trap_int, 16
  %slot = inttoptr i64 %slot_int to ptr
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  %prev_trap = extractvalue { i64, i64, i64, i64 } %rec, 1
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  store i64 %bucket, ptr %slot
  %sum0 = add i64 %bucket, %prev_trap
  %sum1 = add i64 %sum0, %recovered_alloc
  %sum = add i64 %sum1, %recovered_ds
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %sum, 2
  ret { i64, i64, i64 } %ret2

normal:
  ret { i64, i64, i64 } %call
}

; CHECK-LABEL: name: recover_after_setup
; CHECK: bb.{{[0-9]+}}.exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

; RUNTIME-LABEL: name: recover_after_setup
; RUNTIME: BL @callee
; RUNTIME-SAME: implicit-def dead $x22
; RUNTIME: bb.{{[0-9]+}}.exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, runtime-entered):
; RUNTIME: liveins: $x0, $x26, $x27, $x28
; RUNTIME: $q0 = IMPLICIT_DEF
; RUNTIME: $q31 = IMPLICIT_DEF
; RUNTIME: $x1 = IMPLICIT_DEF
; RUNTIME: $x25 = IMPLICIT_DEF
; RUNTIME-NOT: $x0 = IMPLICIT_DEF
; RUNTIME-NOT: $x26 = IMPLICIT_DEF
; RUNTIME-NOT: $x27 = IMPLICIT_DEF
; RUNTIME-NOT: $x28 = IMPLICIT_DEF
; RUNTIME: COPY $x0

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"TrapRecoveryRecoverAfterSetup"}
