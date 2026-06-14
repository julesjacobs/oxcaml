; RUN: not --crash opt -S -passes=rewrite-statepoints-for-gc,verify -rs4gc-fail-on-unhandled-gc-aggregate < %s 2>&1 | FileCheck %s --check-prefix=ERR

target triple = "arm64-apple-macosx"

declare { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1)) "gc-leaf-function"
declare void @consume_pair({ i64, ptr addrspace(1) })

define void @unsupported_aggregate_statepoint_arg(ptr addrspace(1) %obj) gc "oxcaml" {
; ERR: Unhandled GC pointer-containing aggregate use after aggregate explosion:
; ERR: user:   call void @consume_pair({ i64, ptr addrspace(1) } %agg)
entry:
  %agg = call { i64, ptr addrspace(1) } @make_pair(ptr addrspace(1) %obj)
  call void @consume_pair({ i64, ptr addrspace(1) } %agg) "statepoint-id"="0" [ "deopt"() ]
  ret void
}
