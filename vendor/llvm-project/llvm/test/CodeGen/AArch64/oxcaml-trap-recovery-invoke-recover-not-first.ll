; RUN: not --crash llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s 2>&1 | FileCheck %s

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @recover_not_first(i64 %ds, i64 %alloc, ptr %trap_block, i64 %normal_value) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap_block, i64 %normal_value, ptr blockaddress(@recover_not_first, %exn_entry))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64 %ds, i64 %alloc, i64 %normal_value)
          to label %normal_cont unwind label %exn_entry

normal_cont:
  ret { i64, i64, i64 } %call

exn_entry:
  %lp = landingpad token cleanup
  %bad = add i64 %normal_value, 2
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %bad, 2
  ret { i64, i64, i64 } %ret2
}

; CHECK: trap recovery intrinsic must be in a runtime-entered ABI block
