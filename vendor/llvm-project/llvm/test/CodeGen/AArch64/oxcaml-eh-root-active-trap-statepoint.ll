; RUN: llc -mtriple=arm64-apple-macosx -stop-after=prologepilog < %s | FileCheck %s --check-prefix=PEI
; RUN: llc -mtriple=arm64-apple-macosx < %s | FileCheck %s --check-prefix=ASM

target triple = "arm64-apple-macosx"

!llvm.module.flags = !{!0}
!0 = !{i32 1, !"oxcaml_module", !"Test"}

declare void @llvm.aarch64.oxcaml.push.trap(ptr)
declare void @llvm.aarch64.oxcaml.pop.trap()
declare oxcaml_nofpcc void @callee_i64(i64, i64, i64)
declare ptr @personality(...)
declare token @llvm.experimental.gc.statepoint.p0(i64, i32, ptr, i32, i32, ...)
declare ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 immarg, i32 immarg)
declare void @use(ptr addrspace(1), ptr addrspace(1))

define oxcaml_nofpcc void @eh_roots_under_active_trap(
    i64 %ds, i64 %alloc, i64 %arg,
    ptr addrspace(1) %root0, ptr addrspace(1) %root1)
    gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.push.trap(
          ptr blockaddress(@eh_roots_under_active_trap, %recover))
  %tok = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 18, i32 0,
          ptr elementtype(void (i64, i64, i64)) @callee_i64,
          i32 3, i32 0, i64 %ds, i64 %alloc, i64 %arg,
          i32 0, i32 0)
      [ "oxcaml-eh-live"(i32 0, i32 0, ptr addrspace(1) %root0,
                          i32 0, i32 1, ptr addrspace(1) %root1) ]
      to label %normal unwind label %recover

normal:
  call void @llvm.aarch64.oxcaml.pop.trap()
  ret void

recover:
  %lp = landingpad token cleanup
  %p0 = call ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 0, i32 0)
  %p1 = call ptr addrspace(1) @llvm.oxcaml.gc.eh.recover(i32 0, i32 1)
  call void @use(ptr addrspace(1) %p0, ptr addrspace(1) %p1)
  ret void
}

; PEI-LABEL: name: eh_roots_under_active_trap
; PEI: OXCAML_PUSH_TRAP
; PEI: STRXui {{.*}}, $sp, 2
; PEI-NEXT: STRXui {{.*}}, $sp, 3
; PEI: STATEPOINT 18{{.*}}2, 2, 1, 8, {{.*}}, 24, 1, 8, {{.*}}, 16
;
; The stackmap operands are byte offsets from the SP at the statepoint.  The
; stores use scaled 64-bit load/store offsets and run while the native trap has
; pushed SP down by 16 bytes, so the statepoint must report slot * 8.
;
; The final OxCaml frame table must preserve those same SP-relative root
; offsets.  The statepoint ID encodes the active trap separately; that metadata
; is part of the frame size but must not be added to already-final statepoint
; root locations a second time.
; ASM-LABEL: _camlTest__frametable:
; ASM:      .long Ltmp{{[0-9]+}}-Ltmp{{[0-9]+}}
; ASM-NEXT: .short 48
; ASM-NEXT: .short 2
; ASM-NEXT: .short 24
; ASM-NEXT: .short 16
