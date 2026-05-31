; RUN: opt -S -passes=rewrite-statepoints-for-gc < %s | FileCheck %s

target datalayout = "e-ni:1:6"

declare void @foo()

define void @test() gc "statepoint-example" {
; CHECK-LABEL: @test
; CHECK: gc.statepoint
; CHECK: extractelement <2 x ptr addrspace(1)> <ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) poison>
entry:
  call void @foo() [ "gc-live"(ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1))) ]
  %v = extractelement <2 x ptr addrspace(1)> <ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) poison>, i32 0
  %keep = icmp eq ptr addrspace(1) %v, inttoptr (i64 1 to ptr addrspace(1))
  ret void
}
