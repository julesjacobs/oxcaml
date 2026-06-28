; RUN: not --crash opt -S -passes=rewrite-statepoints-for-gc,verify < %s 2>&1 | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()

define ptr addrspace(1) @hidden_interior_through_raw_ptr(
    ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK: OxCaml statepoint cannot treat an integer round-trip from a raw pointer derived from an addrspace(1) interior address as an ordinary root
; CHECK: invalid hidden interior address through raw ptr
entry:
  %field.addr = getelementptr i8, ptr addrspace(1) %obj, i64 8
  %raw.ptr = addrspacecast ptr addrspace(1) %field.addr to ptr
  %field.raw = ptrtoint ptr %raw.ptr to i64
  %roundtrip = inttoptr i64 %field.raw to ptr addrspace(1)
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  ret ptr addrspace(1) %roundtrip
}
