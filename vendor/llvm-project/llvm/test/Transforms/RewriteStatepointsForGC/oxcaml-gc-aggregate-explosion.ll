; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s
; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-fail-on-unhandled-gc-aggregate < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare void @may_gc()
declare { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1)) "gc-leaf-function"
declare { ptr addrspace(1), i64, ptr addrspace(1) } @make_two(ptr addrspace(1), ptr addrspace(1)) "gc-leaf-function"
declare { i64, { ptr addrspace(1), i64 } } @make_nested(ptr addrspace(1)) "gc-leaf-function"
declare { i64, ptr addrspace(1) } @may_throw_pair(ptr addrspace(1))
declare i1 @choose() "gc-leaf-function"
declare i32 @personality(...)

define ptr addrspace(1) @call_result_crosses_safepoint(ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @call_result_crosses_safepoint(
; CHECK: %agg.gcagg1 = extractvalue { i64, ptr addrspace(1) } %agg, 1
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %agg.gcagg1) ]
; CHECK: %agg.gcagg1.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1
; CHECK: ret ptr addrspace(1) %agg.gcagg1.relocated
entry:
  %agg = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %obj)
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { i64, ptr addrspace(1) } %agg, 1
  ret ptr addrspace(1) %p
}

define ptr addrspace(1) @two_safepoints(ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @two_safepoints(
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %agg.gcagg1) ]
; CHECK: %agg.gcagg1.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %agg.gcagg1.relocated) ]
; CHECK: %agg.gcagg1.relocated{{[0-9]+}} = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1
entry:
  %agg = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %obj)
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { i64, ptr addrspace(1) } %agg, 1
  ret ptr addrspace(1) %p
}

define ptr addrspace(1) @two_gc_fields(ptr addrspace(1) %a, ptr addrspace(1) %b) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @two_gc_fields(
; CHECK: %agg.gcagg = extractvalue { ptr addrspace(1), i64, ptr addrspace(1) } %agg, 0
; CHECK: %agg.gcagg2 = extractvalue { ptr addrspace(1), i64, ptr addrspace(1) } %agg, 2
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %agg.gcagg, ptr addrspace(1) %agg.gcagg2) ]
entry:
  %agg = call { ptr addrspace(1), i64, ptr addrspace(1) } @make_two(ptr addrspace(1) %a, ptr addrspace(1) %b)
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { ptr addrspace(1), i64, ptr addrspace(1) } %agg, 0
  %q = extractvalue { ptr addrspace(1), i64, ptr addrspace(1) } %agg, 2
  %c = call i1 @choose() "gc-leaf-function"
  %r = select i1 %c, ptr addrspace(1) %p, ptr addrspace(1) %q
  ret ptr addrspace(1) %r
}

define ptr addrspace(1) @nested_gc_field(ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @nested_gc_field(
; CHECK: %agg.gcagg1 = extractvalue { i64, { ptr addrspace(1), i64 } } %agg, 1, 0
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %agg.gcagg1) ]
entry:
  %agg = call { i64, { ptr addrspace(1), i64 } } @make_nested(ptr addrspace(1) %obj)
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %inner = extractvalue { i64, { ptr addrspace(1), i64 } } %agg, 1
  %p = extractvalue { ptr addrspace(1), i64 } %inner, 0
  ret ptr addrspace(1) %p
}

define ptr addrspace(1) @phi_aggregate(ptr addrspace(1) %a, ptr addrspace(1) %b, i1 %c) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @phi_aggregate(
; CHECK: %m.gcagg{{[0-9]*}} = phi ptr addrspace(1)
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %m.gcagg{{[0-9]*}}) ]
entry:
  br i1 %c, label %left, label %right
left:
  %la = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %a)
  br label %join
right:
  %ra = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %b)
  br label %join
join:
  %m = phi { i64, ptr addrspace(1) } [ %la, %left ], [ %ra, %right ]
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { i64, ptr addrspace(1) } %m, 1
  ret ptr addrspace(1) %p
}

define ptr addrspace(1) @select_aggregate(ptr addrspace(1) %a, ptr addrspace(1) %b, i1 %c) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @select_aggregate(
; CHECK: %sel.gcagg{{[0-9]*}} = select i1 %c, ptr addrspace(1) %la.gcagg{{[0-9]*}}, ptr addrspace(1) %ra.gcagg{{[0-9]*}}
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %sel.gcagg{{[0-9]*}}) ]
entry:
  %la = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %a)
  %ra = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %b)
  %sel = select i1 %c, { i64, ptr addrspace(1) } %la, { i64, ptr addrspace(1) } %ra
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { i64, ptr addrspace(1) } %sel, 1
  ret ptr addrspace(1) %p
}

define ptr addrspace(1) @insertvalue_chain(ptr addrspace(1) %obj) gc "oxcaml" {
; CHECK-LABEL: define ptr addrspace(1) @insertvalue_chain(
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
entry:
  %agg0 = insertvalue { i64, ptr addrspace(1) } poison, i64 7, 0
  %agg1 = insertvalue { i64, ptr addrspace(1) } %agg0, ptr addrspace(1) %obj, 1
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { i64, ptr addrspace(1) } %agg1, 1
  ret ptr addrspace(1) %p
}

define ptr addrspace(1) @invoke_normal_result(ptr addrspace(1) %obj) gc "oxcaml" personality ptr @personality {
; CHECK-LABEL: define ptr addrspace(1) @invoke_normal_result(
; CHECK: normal:
; CHECK: %agg.gcagg1 = extractvalue { i64, ptr addrspace(1) } %agg{{[0-9]*}}, 1
; CHECK: @llvm.experimental.gc.statepoint{{.*}} [ "deopt"(), "gc-live"(ptr addrspace(1) %agg.gcagg1) ]
entry:
  %agg = invoke { i64, ptr addrspace(1) } @may_throw_pair(ptr addrspace(1) %obj) "statepoint-id"="0" [ "deopt"() ]
      to label %normal unwind label %lpad
normal:
  call void @may_gc() "statepoint-id"="0" [ "deopt"() ]
  %p = extractvalue { i64, ptr addrspace(1) } %agg, 1
  ret ptr addrspace(1) %p
lpad:
  %lp = landingpad token cleanup
  ret ptr addrspace(1) null
}
