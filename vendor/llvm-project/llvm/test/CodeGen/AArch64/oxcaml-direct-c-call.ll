; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs < %s | FileCheck %s

declare { i64, i64, i64 } @c_leaf(i64, i64, i64, i64) "gc-leaf-function"="true"
declare { i64, i64, i64 } @c_leaf_struct(i64, i64, i64, i64) "gc-leaf-function"="true"

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @caller(i64 %ds, i64 %alloc, i64 %a, i64 %b) {
; CHECK-LABEL: _caller:
; CHECK: mov x29, sp
; CHECK-NEXT: .cfi_remember_state
; CHECK-NEXT: .cfi_def_cfa_register w29
; CHECK-NEXT: ldr x16, [x28, #104]
; CHECK-NEXT: mov sp, x16
; CHECK-NEXT: bl _c_leaf
; CHECK-NEXT: mov sp, x29
; CHECK-NEXT: .cfi_restore_state
entry:
  %direct = call oxcaml_c_directcc { i64, i64, i64 } @c_leaf(i64 %ds, i64 %alloc, i64 %a, i64 %b) "gc-leaf-function"="true"
  %ds.out = extractvalue { i64, i64, i64 } %direct, 0
  %alloc.out = extractvalue { i64, i64, i64 } %direct, 1
  %r = extractvalue { i64, i64, i64 } %direct, 2
  %ret0 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.out, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { i64 } } %ret0, i64 %alloc.out, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { i64 } } %ret1, i64 %r, 1, 0
  ret { { i64, i64 }, { i64 } } %ret2
}

define oxcaml_nofpcc { { i64, i64 }, { i64 } } @caller_struct(i64 %ds, i64 %alloc, i64 %a, i64 %b) {
; CHECK-LABEL: _caller_struct:
; CHECK: mov x29, sp
; CHECK-NEXT: .cfi_remember_state
; CHECK-NEXT: .cfi_def_cfa_register w29
; CHECK-NEXT: ldr x16, [x28, #104]
; CHECK-NEXT: mov sp, x16
; CHECK-NEXT: bl _c_leaf_struct
; CHECK-NEXT: mov sp, x29
; CHECK-NEXT: .cfi_restore_state
entry:
  %direct = call oxcaml_c_directcc { i64, i64, i64 } @c_leaf_struct(i64 %ds, i64 %alloc, i64 %a, i64 %b) "gc-leaf-function"="true"
  %ds.out = extractvalue { i64, i64, i64 } %direct, 0
  %alloc.out = extractvalue { i64, i64, i64 } %direct, 1
  %r = extractvalue { i64, i64, i64 } %direct, 2
  %ret0 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.out, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { i64 } } %ret0, i64 %alloc.out, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { i64 } } %ret1, i64 %r, 1, 0
  ret { { i64, i64 }, { i64 } } %ret2
}
