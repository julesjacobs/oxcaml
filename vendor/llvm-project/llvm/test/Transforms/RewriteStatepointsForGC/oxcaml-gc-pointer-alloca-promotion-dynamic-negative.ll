; RUN: not --crash opt -S -passes=rewrite-statepoints-for-gc,verify < %s 2>&1 | FileCheck %s --check-prefix=ERR

target triple = "arm64-apple-macosx"

declare void @may_gc()

define ptr addrspace(1) @ordinary_dynamic_slot(
    ptr addrspace(1) %obj, i64 %n) gc "oxcaml" {
; ERR: OxCaml GC pointer alloca is not promotable before late root discovery in ordinary_dynamic_slot
; ERR: alloca:   %slot = alloca ptr addrspace(1), i64 %n
; ERR: unpromotable ordinary OxCaml GC pointer alloca before RS4GC
entry:
  %slot = alloca ptr addrspace(1), i64 %n
  store ptr addrspace(1) %obj, ptr %slot
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %after = load ptr addrspace(1), ptr %slot
  ret ptr addrspace(1) %after
}
