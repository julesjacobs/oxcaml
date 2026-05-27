; RUN: not llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s 2>&1 | FileCheck %s

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()

define oxcaml_nofpcc { i64, i64, i64 } @callbr_recovery_shape_is_not_classified(i64 %ds, i64 %alloc, ptr %trap_block, i64 %normal_value) nounwind gc "ocaml" {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %normal_value,
      ptr blockaddress(@callbr_recovery_shape_is_not_classified, %exn_entry))
  callbr void asm sideeffect "", "!i,~{memory}"()
          to label %try [label %exn_entry]

try:
  %normal_sum = add i64 %normal_value, 1
  %normal_alloc = add i64 %alloc, 8
  %normal_ret0 = insertvalue { i64, i64, i64 } poison, i64 %ds, 0
  %normal_ret1 = insertvalue { i64, i64, i64 } %normal_ret0, i64 %normal_alloc, 1
  %normal_ret2 = insertvalue { i64, i64, i64 } %normal_ret1, i64 %normal_sum, 2
  ret { i64, i64, i64 } %normal_ret2

exn_entry:
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  %prev_trap = extractvalue { i64, i64, i64, i64 } %rec, 1
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %bucket_plus_trap = add i64 %bucket, %prev_trap
  %alloc_plus_domain = add i64 %recovered_alloc, %recovered_ds
  %all_recovered = add i64 %bucket_plus_trap, %alloc_plus_domain
  %exn_ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %exn_ret1 = insertvalue { i64, i64, i64 } %exn_ret0, i64 %recovered_alloc, 1
  %exn_ret2 = insertvalue { i64, i64, i64 } %exn_ret1, i64 %all_recovered, 2
  ret { i64, i64, i64 } %exn_ret2
}

; CHECK: trap recovery intrinsic must be in a runtime-entered ABI block
