; A live typed GC pointer should be found by RS4GC without any frontend alloca
; root bundle.

declare void @safepoint()

define ptr addrspace(1) @ssa_live_value(ptr addrspace(1) %obj) gc "statepoint-example" {
entry:
  call void @safepoint() [ "deopt"() ]
  ret ptr addrspace(1) %obj
}
