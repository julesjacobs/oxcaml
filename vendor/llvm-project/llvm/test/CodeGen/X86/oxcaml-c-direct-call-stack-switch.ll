; RUN: llc -mtriple=x86_64-unknown-linux-gnu -verify-machineinstrs < %s | FileCheck %s

declare { i64, i64, i64 } @c_noalloc(i64, i64, i64, i64) "gc-leaf-function"="true"

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @caller(i64 %domain, i64 %alloc, i64 %arg0, i64 %arg1) #0 gc "oxcaml" {
entry:
  %call = call oxcaml_c_directcc { i64, i64, i64 } @c_noalloc(i64 %domain, i64 %alloc, i64 %arg0, i64 %arg1) "gc-leaf-function"="true"
  %domain.next = extractvalue { i64, i64, i64 } %call, 0
  %alloc.next = extractvalue { i64, i64, i64 } %call, 1
  %value = extractvalue { i64, i64, i64 } %call, 2
  %ret0 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %domain.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { i64 } } %ret0, i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { i64 } } %ret1, i64 %value, 1, 0
  ret { { i64, i64 }, { i64 } } %ret2
}

; CHECK-LABEL: caller:
; CHECK:       movq %rsp, %r13
; CHECK-NEXT:  .cfi_remember_state
; CHECK-NEXT:  .cfi_def_cfa_register %r13
; CHECK-NEXT:  movq 104(%r14), %rsp
; CHECK-NEXT:  callq c_noalloc@PLT
; CHECK-NEXT:  movq %r13, %rsp
; CHECK-NEXT:  .cfi_restore_state

attributes #0 = { "oxcaml-stack-check" }

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"X86_direct_c_call"}
