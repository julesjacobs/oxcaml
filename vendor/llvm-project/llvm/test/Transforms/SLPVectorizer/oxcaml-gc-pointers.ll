; RUN: opt -passes=slp-vectorizer -slp-threshold=-100 -S < %s | FileCheck %s

target triple = "aarch64-apple-darwin"

define void @copy_oxcaml(ptr addrspace(1) %dst, ptr addrspace(1) %src) gc "oxcaml" {
; CHECK-LABEL: define void @copy_oxcaml
; CHECK-NOT: <2 x ptr addrspace(1)>
; CHECK: ret void
entry:
  %s0 = getelementptr i8, ptr addrspace(1) %src, i64 0
  %s1 = getelementptr i8, ptr addrspace(1) %src, i64 8
  %d0 = getelementptr i8, ptr addrspace(1) %dst, i64 0
  %d1 = getelementptr i8, ptr addrspace(1) %dst, i64 8
  %v0 = load ptr addrspace(1), ptr addrspace(1) %s0, align 8
  %v1 = load ptr addrspace(1), ptr addrspace(1) %s1, align 8
  store ptr addrspace(1) %v0, ptr addrspace(1) %d0, align 8
  store ptr addrspace(1) %v1, ptr addrspace(1) %d1, align 8
  ret void
}

define void @copy_non_gc(ptr addrspace(1) %dst, ptr addrspace(1) %src) {
; CHECK-LABEL: define void @copy_non_gc
; CHECK: load <2 x ptr addrspace(1)>
; CHECK: store <2 x ptr addrspace(1)>
; CHECK: ret void
entry:
  %s0 = getelementptr i8, ptr addrspace(1) %src, i64 0
  %s1 = getelementptr i8, ptr addrspace(1) %src, i64 8
  %d0 = getelementptr i8, ptr addrspace(1) %dst, i64 0
  %d1 = getelementptr i8, ptr addrspace(1) %dst, i64 8
  %v0 = load ptr addrspace(1), ptr addrspace(1) %s0, align 8
  %v1 = load ptr addrspace(1), ptr addrspace(1) %s1, align 8
  store ptr addrspace(1) %v0, ptr addrspace(1) %d0, align 8
  store ptr addrspace(1) %v1, ptr addrspace(1) %d1, align 8
  ret void
}
