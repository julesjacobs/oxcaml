; RUN: opt -S -O2 < %s | FileCheck %s --check-prefix=O2
; RUN: opt -S -O2 < %s | llc -mtriple=arm64-apple-macosx -verify-machineinstrs > /dev/null
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s --check-prefix=MIR
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s --check-prefix=ASM
; RUN: llc -mtriple=aarch64-linux-gnu -verify-machineinstrs < %s > /dev/null

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64, i64, i64)
declare oxcaml_nofpcc { i64, i64, i64 } @devil_callee_left(i64, i64, i64)
declare oxcaml_nofpcc { i64, i64, i64 } @devil_callee_right(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @single_invoke_trap_recovery(i64 %ds, i64 %alloc, ptr %trap_block, i64 %normal_value) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %normal_value,
      ptr blockaddress(@single_invoke_trap_recovery, %exn_entry))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64 %ds, i64 %alloc, i64 %normal_value)
          to label %normal_cont unwind label %exn_entry

normal_cont:
  ret { i64, i64, i64 } %call

exn_entry:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 0
  %bucket.raw = ptrtoint ptr addrspace(1) %bucket to i64
  %prev_trap = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 1
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %bucket_plus_trap = add i64 %bucket.raw, %prev_trap
  %alloc_plus_domain = add i64 %recovered_alloc, %recovered_ds
  %all_recovered = add i64 %bucket_plus_trap, %alloc_plus_domain
  %exn_ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %exn_ret1 = insertvalue { i64, i64, i64 } %exn_ret0, i64 %recovered_alloc, 1
  %exn_ret2 = insertvalue { i64, i64, i64 } %exn_ret1, i64 %all_recovered, 2
  ret { i64, i64, i64 } %exn_ret2
}

define oxcaml_nofpcc { i64, i64, i64 } @branched_invoke_trap_recovery(i64 %ds, i64 %alloc, ptr %trap_block, i64 %normal_value, i64 %cond_int) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %normal_value,
      ptr blockaddress(@branched_invoke_trap_recovery, %exn_entry))
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %try_left, label %try_right

try_left:
  %left = invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee_left(i64 %ds, i64 %alloc, i64 %normal_value)
          to label %normal_cont unwind label %exn_entry

try_right:
  %right_arg = add i64 %normal_value, 1
  %right = invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee_right(i64 %ds, i64 %alloc, i64 %right_arg)
           to label %normal_cont unwind label %exn_entry

normal_cont:
  %normal_ret = phi { i64, i64, i64 } [ %left, %try_left ], [ %right, %try_right ]
  ret { i64, i64, i64 } %normal_ret

exn_entry:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 0
  %bucket.raw = ptrtoint ptr addrspace(1) %bucket to i64
  %prev_trap = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 1
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %all_recovered0 = add i64 %bucket.raw, %prev_trap
  %all_recovered1 = add i64 %all_recovered0, %recovered_alloc
  %all_recovered = add i64 %all_recovered1, %recovered_ds
  %exn_ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %exn_ret1 = insertvalue { i64, i64, i64 } %exn_ret0, i64 %recovered_alloc, 1
  %exn_ret2 = insertvalue { i64, i64, i64 } %exn_ret1, i64 %all_recovered, 2
  ret { i64, i64, i64 } %exn_ret2
}

; O2-LABEL: define oxcaml_nofpcc { i64, i64, i64 } @single_invoke_trap_recovery
; O2: invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee
; O2: unwind label %exn_entry
; O2: exn_entry:
; O2: landingpad token
; O2: call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover

; MIR-LABEL: name: single_invoke_trap_recovery
; MIR: BL @devil_callee
; MIR: bb.{{[0-9]+}}.exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, {{(landing-pad, )?}}runtime-entered):
; MIR-NEXT: successors:
; MIR-NEXT: liveins: $x0, $x26, $x27, $x28
; MIR: EH_LABEL

; MIR-LABEL: name: branched_invoke_trap_recovery
; MIR: bb.{{[0-9]+}}.try_left:
; MIR: successors: {{.*}}%bb.[[RECOVERY:[0-9]+]]
; MIR: BL @devil_callee
; MIR: bb.{{[0-9]+}}.try_right:
; MIR: successors: {{.*}}%bb.[[RECOVERY]]
; MIR: BL @devil_callee
; MIR: bb.[[RECOVERY]].exn_entry (machine-block-address-taken, ir-block-address-taken %ir-block.exn_entry, {{(landing-pad, )?}}runtime-entered):
; MIR-NEXT: successors:
; MIR-NEXT: liveins: $x0, $x26, $x27, $x28

; ASM-LABEL: _single_invoke_trap_recovery:
; ASM: bl _devil_callee
; ASM: ; Block address taken
; ASM-NEXT: LBB{{[0-9]+}}_{{[0-9]+}}:
; ASM: add x8, x0, x26
; ASM-NEXT: add x9, x27, x28
; ASM-NEXT: add x0, x8, x9
