; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s --check-prefix=ASM

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
; CHECK: bb.[[RECOVER]].recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, {{(landing-pad, )?}}runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

; ASM: lCPI0_0:
; ASM-NEXT: .quad Ltmp
; ASM-LABEL: _split_unwind:
; ASM: adrp [[TARGET:x[0-9]+]], lCPI0_0@PAGE
; ASM: ldr [[TARGET]], {{\[}}[[TARGET]], lCPI0_0@PAGEOFF]
; ASM-NEXT: str x1, [x0]
; ASM-NEXT: str [[TARGET]], [x0, #8]
; ASM-NEXT: mov x26, x0

define oxcaml_nofpcc { i64, i64, ptr addrspace(1) } @oxcaml_split_unwind_phi(i64 %ds, i64 %alloc, ptr %trap_block, ptr addrspace(1) %a, ptr addrspace(1) %b, i64 %cond_int) gc "oxcaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_block,
      i64 1,
      ptr blockaddress(@oxcaml_split_unwind_phi, %recover))
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %left, label %right

left:
  %left_call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 1)
          to label %normal_left unwind label %left_lpad

normal_left:
  %left_ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %ds, 0
  %left_ret1 = insertvalue { i64, i64, ptr addrspace(1) } %left_ret0, i64 %alloc, 1
  %left_ret2 = insertvalue { i64, i64, ptr addrspace(1) } %left_ret1, ptr addrspace(1) %a, 2
  ret { i64, i64, ptr addrspace(1) } %left_ret2

right:
  %right_call = invoke oxcaml_nofpcc { i64, i64, i64 } @callee(i64 %ds, i64 %alloc, i64 2)
          to label %normal_right unwind label %right_lpad

normal_right:
  %right_ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %ds, 0
  %right_ret1 = insertvalue { i64, i64, ptr addrspace(1) } %right_ret0, i64 %alloc, 1
  %right_ret2 = insertvalue { i64, i64, ptr addrspace(1) } %right_ret1, ptr addrspace(1) %b, 2
  ret { i64, i64, ptr addrspace(1) } %right_ret2

left_lpad:
  %left_lp = landingpad token cleanup
  br label %recover

right_lpad:
  %right_lp = landingpad token cleanup
  br label %recover

recover:
  %handler_value = phi ptr addrspace(1) [ %a, %left_lpad ], [ %b, %right_lpad ]
  %rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket = extractvalue { i64, i64, i64, i64 } %rec, 0
  %recovered_alloc = extractvalue { i64, i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { i64, i64, i64, i64 } %rec, 3
  %bucket_as_ptr = inttoptr i64 %bucket to ptr
  store ptr addrspace(1) %handler_value, ptr %bucket_as_ptr
  %ret0 = insertvalue { i64, i64, ptr addrspace(1) } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, ptr addrspace(1) } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, ptr addrspace(1) } %ret1, ptr addrspace(1) %handler_value, 2
  ret { i64, i64, ptr addrspace(1) } %ret2
}

; CHECK-LABEL: name: oxcaml_split_unwind_phi
; CHECK: bb.{{[0-9]+}}.recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, {{(landing-pad, )?}}runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"TrapRecoverySplitUnwind"}
