; RUN: not --crash opt -S -passes=rewrite-statepoints-for-gc,verify < %s 2>&1 | FileCheck %s --check-prefix=ERR

target triple = "arm64-apple-macosx"

declare void @may_gc()
declare void @escape(ptr) "gc-leaf-function"

define ptr addrspace(1) @ordinary_unpromotable_slot(
    ptr addrspace(1) %obj) gc "oxcaml" {
; ERR: OxCaml GC pointer alloca is not promotable before late root discovery in ordinary_unpromotable_slot
; ERR: alloca:   %slot = alloca ptr addrspace(1)
; ERR: unpromotable ordinary OxCaml GC pointer alloca before RS4GC
entry:
  %slot = alloca ptr addrspace(1)
  store ptr addrspace(1) %obj, ptr %slot
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  call void @escape(ptr %slot) "gc-leaf-function"
  %after = load ptr addrspace(1), ptr %slot
  ret ptr addrspace(1) %after
}
