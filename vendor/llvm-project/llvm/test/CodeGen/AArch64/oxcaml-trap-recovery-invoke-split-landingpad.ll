; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee_a(i64, i64, i64)
declare oxcaml_nofpcc { i64, i64, i64 } @callee_b(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @split_landingpad_trampolines(i64 %ds, i64 %alloc, ptr %trap_block, i64 %value, i64 %cond_int) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %value,
      ptr blockaddress(@split_landingpad_trampolines, %exn_entry))
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %call_a, label %call_b

call_a:
  %a = invoke oxcaml_nofpcc { i64, i64, i64 } @callee_a(i64 %ds, i64 %alloc, i64 %value)
       to label %normal unwind label %exn_lpad_a

call_b:
  %b = invoke oxcaml_nofpcc { i64, i64, i64 } @callee_b(i64 %ds, i64 %alloc, i64 %value)
       to label %normal unwind label %exn_lpad_b

exn_lpad_a:
  %lp_a = landingpad token cleanup
  br label %exn_entry

exn_lpad_b:
  %lp_b = landingpad token cleanup
  br label %exn_entry

exn_entry:
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

normal:
  %result = phi { i64, i64, i64 } [ %a, %call_a ], [ %b, %call_b ]
  ret { i64, i64, i64 } %result
}

; CHECK-LABEL: name: split_landingpad_trampolines
; CHECK: bb.{{[0-9]+}}.exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, {{(landing-pad, )?}}runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"TrapRecoverySplitLandingpad"}
