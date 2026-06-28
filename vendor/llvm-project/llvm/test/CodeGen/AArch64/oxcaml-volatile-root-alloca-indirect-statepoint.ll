; RUN: opt -S -passes=rewrite-statepoints-for-gc < %s | llc -mtriple=arm64-apple-macosx -stop-after=prologepilog -o - | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
    i64, i64, ptr addrspace(1))

define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @arg_live_after_call(
    i64 %ds,
    i64 %alloc,
    ptr addrspace(1) %x) gc "oxcaml" {
; CHECK-LABEL: name: arg_live_after_call
; CHECK: STATEPOINT
; The final STATEPOINT operands encode stack-map operands as:
;   ConstantOp(2), num-gc-pointers(1),
;   IndirectMemRefOp(1), size(8), base register, offset.
; The volatile root alloca stores the GC pointer, so it must be indirect.  A
; direct alloca stack-map operand would encode the alloca address as the root.
; CHECK-SAME: 2, 1, 1, 8, $sp,
entry:
  %res = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @callee(
      i64 %ds, i64 %alloc, ptr addrspace(1) %x)
      "statepoint-id"="0" [ "deopt"() ]
  %ds.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 0
  %alloc.next = extractvalue { { i64, i64 }, { i64 } } %res, 0, 1
  %ret0 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison,
      i64 %ds.next, 0, 0
  %ret1 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret0,
      i64 %alloc.next, 0, 1
  %ret2 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %ret1,
      ptr addrspace(1) %x, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %ret2
}
