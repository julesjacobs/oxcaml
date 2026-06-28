; RUN: llc -mtriple=x86_64-unknown-linux-gnu -stop-after=fixup-statepoint-caller-saved -o - %s | FileCheck --check-prefix=DEFAULT %s
; RUN: llc -mtriple=x86_64-unknown-linux-gnu -oxcaml-x86-statepoint-inplace-max-gc-roots=0 -stop-after=fixup-statepoint-caller-saved -o - %s | FileCheck --check-prefix=BUDGET0 %s
; RUN: llc -mtriple=i386-unknown-linux-gnu -verify-machineinstrs -stop-after=finalize-isel -o - %s | FileCheck --check-prefix=I386 %s

declare oxcaml_fpcc { { i64, i64 }, {} } @callee(i64, i64, ptr addrspace(1)) gc "oxcaml"
declare oxcaml_fpcc { { i64, i64 }, {} } @callee3(i64, i64, ptr addrspace(1), ptr addrspace(1), ptr addrspace(1)) gc "oxcaml"

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr,
                                                  i32 immarg, i32 immarg, ...)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32, i32)
declare { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token)

define oxcaml_fpcc { { i64, i64 }, { ptr addrspace(1) } }
@small_ordinary_call(i64 %ds, i64 %alloc, ptr addrspace(1) %root) gc "oxcaml" {
entry:
  %tok = call oxcaml_fpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0,
          ptr elementtype({ { i64, i64 }, {} } (i64, i64, ptr addrspace(1))) @callee,
          i32 3, i32 0,
          i64 %ds, i64 %alloc, ptr addrspace(1) %root,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root) ]
  %rel = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(
      token %tok, i32 0, i32 0)
  %result = call { { i64, i64 }, {} }
      @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %tok)
  %ds.out = extractvalue { { i64, i64 }, {} } %result, 0, 0
  %alloc.out = extractvalue { { i64, i64 }, {} } %result, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.out, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.out, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %rel, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}

; DEFAULT-LABEL: name: small_ordinary_call
; DEFAULT: STATEPOINT
; DEFAULT-SAME: %stack.
; DEFAULT-SAME: :: (load store

; BUDGET0-LABEL: name: small_ordinary_call
; BUDGET0: STATEPOINT
; BUDGET0-SAME: %stack.
; BUDGET0-SAME: :: (volatile load store

; I386-LABEL: name: small_ordinary_call
; I386: STATEPOINT
; I386-SAME: %stack.
; I386-SAME: :: (volatile load store

define oxcaml_fpcc { { i64, i64 },
                     { ptr addrspace(1), ptr addrspace(1), ptr addrspace(1) } }
@large_ordinary_call(i64 %ds, i64 %alloc, ptr addrspace(1) %root0,
                     ptr addrspace(1) %root1, ptr addrspace(1) %root2)
    gc "oxcaml" {
entry:
  %tok = call oxcaml_fpcc token (i64, i32, ptr, i32, i32, ...)
      @llvm.experimental.gc.statepoint.p0(
          i64 0, i32 0,
          ptr elementtype({ { i64, i64 }, {} } (i64, i64, ptr addrspace(1),
                                                ptr addrspace(1),
                                                ptr addrspace(1))) @callee3,
          i32 5, i32 0,
          i64 %ds, i64 %alloc, ptr addrspace(1) %root0,
          ptr addrspace(1) %root1, ptr addrspace(1) %root2,
          i32 0, i32 0)
      [ "gc-live"(ptr addrspace(1) %root0, ptr addrspace(1) %root1,
                  ptr addrspace(1) %root2) ]
  %rel0 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(
      token %tok, i32 0, i32 0)
  %rel1 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(
      token %tok, i32 1, i32 1)
  %rel2 = call ptr addrspace(1) @llvm.experimental.gc.relocate.p1(
      token %tok, i32 2, i32 2)
  %result = call { { i64, i64 }, {} }
      @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %tok)
  %ds.out = extractvalue { { i64, i64 }, {} } %result, 0, 0
  %alloc.out = extractvalue { { i64, i64 }, {} } %result, 0, 1
  %ret0 = insertvalue { { i64, i64 },
                        { ptr addrspace(1), ptr addrspace(1),
                          ptr addrspace(1) } } poison, i64 %ds.out, 0, 0
  %ret1 = insertvalue { { i64, i64 },
                        { ptr addrspace(1), ptr addrspace(1),
                          ptr addrspace(1) } } %ret0, i64 %alloc.out, 0, 1
  %ret2 = insertvalue { { i64, i64 },
                        { ptr addrspace(1), ptr addrspace(1),
                          ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %rel0, 1, 0
  %ret3 = insertvalue { { i64, i64 },
                        { ptr addrspace(1), ptr addrspace(1),
                          ptr addrspace(1) } } %ret2,
      ptr addrspace(1) %rel1, 1, 1
  %ret4 = insertvalue { { i64, i64 },
                        { ptr addrspace(1), ptr addrspace(1),
                          ptr addrspace(1) } } %ret3,
      ptr addrspace(1) %rel2, 1, 2
  ret { { i64, i64 },
        { ptr addrspace(1), ptr addrspace(1), ptr addrspace(1) } } %ret4
}

; DEFAULT-LABEL: name: large_ordinary_call
; DEFAULT: STATEPOINT
; DEFAULT-SAME: %stack.
; DEFAULT-SAME: :: (volatile load store
