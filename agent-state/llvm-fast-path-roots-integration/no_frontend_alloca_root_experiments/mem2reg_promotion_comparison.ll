; Compare a plain load/store alloca with the same alloca after its address is
; used in a gc-live operand bundle.

declare void @safepoint()

define ptr addrspace(1) @plain_alloca_promotes(ptr addrspace(1) %obj) {
entry:
  %slot = alloca ptr addrspace(1)
  store ptr addrspace(1) %obj, ptr %slot
  %loaded = load ptr addrspace(1), ptr %slot
  ret ptr addrspace(1) %loaded
}

define ptr addrspace(1) @gc_live_alloca_does_not_promote(ptr addrspace(1) %obj) {
entry:
  %slot = alloca ptr addrspace(1)
  store ptr addrspace(1) %obj, ptr %slot
  call void @safepoint() [ "gc-live"(ptr %slot) ]
  %loaded = load ptr addrspace(1), ptr %slot
  ret ptr addrspace(1) %loaded
}
