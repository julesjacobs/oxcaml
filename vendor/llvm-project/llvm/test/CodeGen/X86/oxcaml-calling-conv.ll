; RUN: llc -mtriple=x86_64-unknown-linux-gnu -mattr=+avx -verify-machineinstrs < %s | FileCheck %s

declare oxcaml_nofpcc { { i64, i64 }, { i64 } }
  @camlX86__id(i64, i64, i64) gc "oxcaml"
declare oxcaml_nofpcc { { i64, i64 }, { <4 x i64> } }
  @camlX86__vec(i64, i64, <4 x i64>) gc "oxcaml"
declare { i64, i64, i64 } @c_leaf(i64, i64) "gc-leaf-function"="true"

define void @call_oxcaml(i64 %ds, i64 %alloc, i64 %arg) {
; CHECK-LABEL: call_oxcaml:
; CHECK: movq %rdi, %r14
; CHECK: movq %rsi, %r15
; CHECK: movq %rdx, %rax
; CHECK: callq camlX86__id
  %r = call oxcaml_nofpcc { { i64, i64 }, { i64 } }
    @camlX86__id(i64 %ds, i64 %alloc, i64 %arg)
  ret void
}

define void @call_oxcaml_vec(i64 %ds, i64 %alloc, <4 x i64> %arg) {
; CHECK-LABEL: call_oxcaml_vec:
; CHECK: movq %rdi, %r14
; CHECK: movq %rsi, %r15
; CHECK: callq camlX86__vec
  %r = call oxcaml_nofpcc { { i64, i64 }, { <4 x i64> } }
    @camlX86__vec(i64 %ds, i64 %alloc, <4 x i64> %arg)
  ret void
}

define oxcaml_nofpcc { { i64, i64 }, { i64 } }
  @direct_c_call(i64 %ds, i64 %alloc, i64 %a, i64 %b) gc "oxcaml" {
; CHECK-LABEL: direct_c_call:
; CHECK: movq %rax, %rdi
; CHECK: movq %rbx, %rsi
; CHECK: callq c_leaf
entry:
  %direct = call oxcaml_c_directcc { i64, i64, i64 }
    @c_leaf(i64 %ds, i64 %alloc, i64 %a, i64 %b)
    "gc-leaf-function"="true"
  %ds.out = extractvalue { i64, i64, i64 } %direct, 0
  %alloc.out = extractvalue { i64, i64, i64 } %direct, 1
  %r = extractvalue { i64, i64, i64 } %direct, 2
  %ret0 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.out, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { i64 } } %ret0, i64 %alloc.out, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { i64 } } %ret1, i64 %r, 1, 0
  ret { { i64, i64 }, { i64 } } %ret2
}
