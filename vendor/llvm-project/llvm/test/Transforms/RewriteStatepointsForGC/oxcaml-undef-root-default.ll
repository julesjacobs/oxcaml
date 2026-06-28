; RUN: opt -S -passes='default<O3>,rewrite-statepoints-for-gc' < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()
declare void @may_gc_with_arg(ptr addrspace(1))

define void @undef_phi_root(i1 %cond, ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define void @undef_phi_root(
; CHECK-NOT: undef
; CHECK: llvm.experimental.gc.statepoint
; CHECK-NOT: undef
; CHECK: ret void
entry:
  br i1 %cond, label %left, label %join

left:
  call void @may_gc() [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
  br label %join

join:
  %p = phi ptr addrspace(1) [ %obj, %left ], [ undef, %entry ]
  call void @may_gc_with_arg(ptr addrspace(1) %p)
      [ "deopt"(), "gc-live"(ptr addrspace(1) %p) ]
  ret void
}

define void @poison_bundle_root(i1 %cond) gc "oxcaml" {
; CHECK-LABEL: define void @poison_bundle_root(
; CHECK-NOT: poison
; CHECK: "gc-live"(ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)))
; CHECK-NOT: poison
; CHECK: ret void
entry:
  br i1 %cond, label %left, label %join

left:
  call void @may_gc() [ "deopt"(), "gc-live"(ptr addrspace(1) poison) ]
  br label %join

join:
  call void @may_gc() [ "deopt"(), "gc-live"(ptr addrspace(1) poison) ]
  ret void
}
