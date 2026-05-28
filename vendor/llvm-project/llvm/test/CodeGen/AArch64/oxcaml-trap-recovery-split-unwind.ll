; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @split_unwind(i64 %ds, i64 %alloc, ptr %trap_block, i64 %value) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 %value,
      ptr blockaddress(@split_unwind, %recover))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 %value)
          to label %normal unwind label %split_lpad

split_lpad:
  %lp = landingpad token cleanup
  br label %join_a

join_a:
  br label %join_b

join_b:
  br label %recover

recover:
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %bucket, 2
  ret { i64, i64, i64 } %ret2

normal:
  ret { i64, i64, i64 } %call
}

; CHECK-LABEL: name: split_unwind
; CHECK: bb.{{[0-9]+}}.entry:
; CHECK: successors: {{.*}}%bb.[[RECOVER:[0-9]+]]
; CHECK: bb.[[RECOVER]].recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28
