; RUN: not --crash opt -S -passes=rewrite-statepoints-for-gc,verify < %s 2>&1 | FileCheck %s
; XFAIL: *

target triple = "arm64-apple-macosx"

declare void @may_gc()

define ptr addrspace(1) @hidden_interior_through_i64_load(
    ptr addrspace(1) %obj,
    ptr %slot) gc "oxcaml" {
; CHECK: OxCaml statepoint cannot treat inttoptr from an unknown integer producer as an ordinary root
; CHECK: unsupported integer-boundary root
entry:
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 8
  %field.raw = ptrtoint ptr addrspace(1) %field.addr to i64
  store i64 %field.raw, ptr %slot, align 8
  %loaded = load i64, ptr %slot, align 8
  %roundtrip = inttoptr i64 %loaded to ptr addrspace(1)
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  ret ptr addrspace(1) %roundtrip
}
