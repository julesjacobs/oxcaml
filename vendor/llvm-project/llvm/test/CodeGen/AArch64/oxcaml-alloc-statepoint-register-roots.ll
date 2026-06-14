; Register-root lowering is opt-in until statepoints have in-place root
; semantics (the tied-def form creates unlisted secondary locations).
; RUN: llc -mtriple=aarch64-apple-macosx -oxcaml-statepoint-inplace=0 -oxcaml-statepoint-inplace-calls=0 -max-registers-for-gc-values=1000 -stop-after=fixup-statepoint-caller-saved -o - %s | FileCheck %s
;
; In-place lowering (-oxcaml-statepoint-inplace): at alloc-family
; statepoints gc values are plain untied operands (no spill, no reload,
; no tied def); relocates are identity. Ordinary calls still spill.
; RUN: llc -mtriple=aarch64-apple-macosx -oxcaml-statepoint-inplace -oxcaml-statepoint-inplace-calls=0 -verify-machineinstrs -stop-after=fixup-statepoint-caller-saved -o - %s | FileCheck --check-prefix=INPLACE %s

@caml_call_gc = external global ptr

declare void @caml_regular(ptr addrspace(1))

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr,
                                                  i32 immarg, i32 immarg, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token)

define oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } } @alloc_root_registers(i64 %domain, i64 %alloc, ptr addrspace(1) %root) gc "oxcaml" {
entry:
  %tok = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0,
          ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @caml_call_gc,
          i32 2, i32 0,
          i64 %domain, i64 %alloc,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root) ]
  %rel = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok, i32 0, i32 0)
  %result = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %tok)
  %r0 = extractvalue { { i64, i64 }, {} } %result, 0, 0
  %r1 = extractvalue { { i64, i64 }, {} } %result, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %r0, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0, i64 %r1, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1, ptr addrspace(1) %rel, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

define ptr addrspace(1) @ordinary_call_spills_root(ptr addrspace(1) %root) gc "oxcaml" {
entry:
  %tok = call token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 1, i32 0,
          ptr elementtype(void (ptr addrspace(1))) @caml_regular,
          i32 1, i32 0,
          ptr addrspace(1) %root,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root) ]
  %rel = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %tok, i32 0, i32 0)
  ret ptr addrspace(1) %rel
}

; CHECK-LABEL: name: alloc_root_registers
; CHECK: STATEPOINT {{.*}} @caml_call_gc
; CHECK-SAME: renamable $x
; CHECK-SAME: csr_aarch64_oxcaml_alloc
; CHECK-NOT: STATEPOINT {{.*}} @caml_call_gc{{.*}}%stack

; CHECK-LABEL: name: ordinary_call_spills_root
; CHECK: STATEPOINT {{.*}} @caml_regular
; CHECK-SAME: %stack.

; INPLACE-LABEL: name: alloc_root_registers
; INPLACE-NOT: STRXui
; INPLACE: STATEPOINT {{.*}} @caml_call_gc
; INPLACE-SAME: renamable $x
; INPLACE-SAME: csr_aarch64_oxcaml_alloc
; INPLACE-NOT: LDRXui
; INPLACE-NOT: tied-def

; INPLACE-LABEL: name: ordinary_call_spills_root
; INPLACE: STATEPOINT {{.*}} @caml_regular
; INPLACE-SAME: %stack.
