; RUN: llc < %s -mtriple=aarch64-apple-darwin | FileCheck %s --check-prefix=DARWIN
; RUN: llc < %s -mtriple=aarch64-linux-gnu | FileCheck %s --check-prefix=LINUX

@caml_call_gc = external global ptr
declare void @use(ptr)

define oxcaml_fpcc { { i64, i64 }, { i64 } } @camlAarch64__id(i64 %domain, i64 %alloc, i64 %arg) gc "oxcaml" {
  %ret0 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %domain, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { i64 } } %ret0, i64 %alloc, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { i64 } } %ret1, i64 %arg, 1, 0
  ret { { i64, i64 }, { i64 } } %ret2
}

define void @call_oxcaml(i64 %domain, i64 %alloc, i64 %arg) {
  %r = call oxcaml_fpcc { { i64, i64 }, { i64 } } @camlAarch64__id(i64 %domain, i64 %alloc, i64 %arg)
  ret void
}

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @camlAarch64__many_args(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) gc "oxcaml"

define void @call_oxcaml_many(i64 %domain, i64 %alloc, i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, i64 %a6, i64 %a7, i64 %a8) {
  %r = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @camlAarch64__many_args(i64 %domain, i64 %alloc, i64 %a0, i64 %a1, i64 %a2, i64 %a3, i64 %a4, i64 %a5, i64 %a6, i64 %a7, i64 %a8)
  ret void
}

define oxcaml_fpcc { { i64, i64 }, { i64 } } @tail_call_oxcaml(i64 %domain, i64 %alloc, i64 %arg) gc "oxcaml" {
  %r = musttail call oxcaml_fpcc { { i64, i64 }, { i64 } } @camlAarch64__id(i64 %domain, i64 %alloc, i64 %arg)
  ret { { i64, i64 }, { i64 } } %r
}

define oxcaml_fpcc { { i64, i64 }, {} } @camlAarch64__poll(i64 %domain, i64 %alloc) gc "oxcaml" {
  %token = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 1, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @caml_call_gc, i32 2, i32 0, i64 %domain, i64 %alloc, i32 0, i32 0)
  %result = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %token)
  ret { { i64, i64 }, {} } %result
}

define oxcaml_nofpcc { { i64, i64 }, {} } @nofp_with_dynamic_alloca(i64 %domain, i64 %alloc, i64 %n) gc "oxcaml" {
  %p = alloca i8, i64 %n, align 16
  call void @use(ptr %p)
  %ret0 = insertvalue { { i64, i64 }, {} } poison, i64 %domain, 0, 0
  %ret1 = insertvalue { { i64, i64 }, {} } %ret0, i64 %alloc, 0, 1
  ret { { i64, i64 }, {} } %ret1
}

define oxcaml_nofpcc { { i64, i64 }, {} } @nofp_large_frame_dynamic_alloca(i64 %domain, i64 %alloc, i64 %n) gc "oxcaml" {
  %fixed = alloca [512 x i8], align 16
  %dyn = alloca i8, i64 %n, align 16
  call void @use(ptr %fixed)
  call void @use(ptr %dyn)
  %ret0 = insertvalue { { i64, i64 }, {} } poison, i64 %domain, 0, 0
  %ret1 = insertvalue { { i64, i64 }, {} } %ret0, i64 %alloc, 0, 1
  ret { { i64, i64 }, {} } %ret1
}

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)
declare { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token)

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"Aarch64"}

; DARWIN-LABEL: _call_oxcaml:
; DARWIN:       mov x28, x0
; DARWIN:       mov x27, x1
; DARWIN:       mov x0, x2
; DARWIN:       bl _camlAarch64__id
; DARWIN-LABEL: _call_oxcaml_many:
; DARWIN:       bl _camlAarch64__many_args
; DARWIN-LABEL: _tail_call_oxcaml:
; DARWIN:       b _camlAarch64__id
; DARWIN-LABEL: _camlAarch64__poll:
; DARWIN-NOT:   mov x29, sp
; DARWIN:       bl _caml_call_gc
; DARWIN:       ret
; DARWIN-LABEL: _nofp_with_dynamic_alloca:
; DARWIN:       str x30, [sp, #8]
; DARWIN:       mov x29, sp
; DARWIN:       ldr x30, [sp, #8]
; DARWIN-LABEL: _nofp_large_frame_dynamic_alloca:
; DARWIN-NOT:   mov x19, sp
; DARWIN:       bl _use
; DARWIN:       bl _use
; DARWIN-LABEL: _camlAarch64__frametable:
; DARWIN-NEXT:  .quad 4
; DARWIN:       .byte 0

; LINUX-LABEL: call_oxcaml:
; LINUX:       mov x28, x0
; LINUX:       mov x27, x1
; LINUX:       mov x0, x2
; LINUX:       bl camlAarch64__id
; LINUX-LABEL: call_oxcaml_many:
; LINUX:       bl camlAarch64__many_args
; LINUX-LABEL: tail_call_oxcaml:
; LINUX:       b camlAarch64__id
; LINUX-LABEL: camlAarch64__poll:
; LINUX-NOT:   mov x29, sp
; LINUX:       bl caml_call_gc
; LINUX:       ret
; LINUX-LABEL: nofp_with_dynamic_alloca:
; LINUX:       str x30, [sp, #8]
; LINUX:       mov x29, sp
; LINUX:       ldr x30, [sp, #8]
; LINUX-LABEL: nofp_large_frame_dynamic_alloca:
; LINUX-NOT:   mov x19, sp
; LINUX:       bl use
; LINUX:       bl use
; LINUX-LABEL: camlAarch64__frametable:
; LINUX-NEXT:  .xword 4
; LINUX:       .byte 0
