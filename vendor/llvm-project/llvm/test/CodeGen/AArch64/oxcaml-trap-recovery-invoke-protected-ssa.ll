; RUN: opt -S -O2 < %s | FileCheck %s --check-prefix=O2
; RUN: opt -S -O2 < %s | llc -mtriple=arm64-apple-macosx -verify-machineinstrs > /dev/null
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=aarch64-oxcaml-runtime-entry < %s | FileCheck %s --check-prefix=RUNTIME

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @protected_ssa_in_recovery(i64 %ds, i64 %alloc, i64 %normal_value) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@protected_ssa_in_recovery, %recover))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64 %ds, i64 %alloc, i64 %normal_value)
          to label %normal_cont unwind label %lpad

normal_cont:
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret { i64, i64, i64 } %call

lpad:
  %lp = landingpad token cleanup
  br label %recover

recover:
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %bad = add i64 %recovered_ds, %normal_value
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %bad, 2
  ret { i64, i64, i64 } %ret2
}

define oxcaml_nofpcc { i64, i64, i64 } @promoted_local_in_recovery(i64 %ds, i64 %alloc, i64 %normal_value) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  %local = alloca i64
  store i64 %normal_value, ptr %local
  %promoted = load i64, ptr %local
  call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@promoted_local_in_recovery, %recover))
  %call = invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee(i64 %ds, i64 %alloc, i64 %promoted)
          to label %normal_cont unwind label %lpad

normal_cont:
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret { i64, i64, i64 } %call

lpad:
  %lp = landingpad token cleanup
  br label %recover

recover:
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %recovered_alloc = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 2
  %recovered_ds = extractvalue { ptr addrspace(1), i64, i64, i64 } %rec, 3
  %sum = add i64 %recovered_ds, %promoted
  %ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_ds, 0
  %ret1 = insertvalue { i64, i64, i64 } %ret0, i64 %recovered_alloc, 1
  %ret2 = insertvalue { i64, i64, i64 } %ret1, i64 %sum, 2
  ret { i64, i64, i64 } %ret2
}

; O2-LABEL: define oxcaml_nofpcc { i64, i64, i64 } @protected_ssa_in_recovery
; O2: invoke oxcaml_nofpcc { i64, i64, i64 } @devil_callee
; O2: unwind label %lpad
; O2: lpad:
; O2: landingpad token
; O2: recover:
; O2: call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover
; O2: add i64 %{{.*}}, %normal_value

; O2-LABEL: define oxcaml_nofpcc { i64, i64, i64 } @promoted_local_in_recovery
; O2-NOT: alloca
; O2: recover:
; O2: call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover
; O2: add i64 %{{.*}}, %normal_value

; RUNTIME-LABEL: name: protected_ssa_in_recovery
; RUNTIME: OXCAML_PUSH_TRAP
; RUNTIME: BL @devil_callee
; RUNTIME-SAME: implicit-def dead $x1
; RUNTIME: bb.{{[0-9]+}}.recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, {{(landing-pad, )?}}runtime-entered):
; RUNTIME-NEXT: successors:
; RUNTIME-NEXT: liveins: $x0, $x26, $x27, $x28
; RUNTIME: $x1 = IMPLICIT_DEF
; RUNTIME: COPY $x28

; RUNTIME-LABEL: name: promoted_local_in_recovery
; RUNTIME: OXCAML_PUSH_TRAP
; RUNTIME: BL @devil_callee
; RUNTIME-SAME: implicit-def dead $x1
; RUNTIME: bb.{{[0-9]+}}.recover (machine-block-address-taken, ir-block-address-taken %ir-block.recover, {{(landing-pad, )?}}runtime-entered):
; RUNTIME-NEXT: successors:
; RUNTIME-NEXT: liveins: $x0, $x26, $x27, $x28
