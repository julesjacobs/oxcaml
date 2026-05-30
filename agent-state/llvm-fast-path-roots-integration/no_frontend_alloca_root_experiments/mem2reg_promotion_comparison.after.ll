; ModuleID = 'agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/mem2reg_promotion_comparison.ll'
source_filename = "agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/mem2reg_promotion_comparison.ll"

declare void @safepoint()

define ptr addrspace(1) @plain_alloca_promotes(ptr addrspace(1) %obj) {
entry:
  ret ptr addrspace(1) %obj
}

define ptr addrspace(1) @gc_live_alloca_does_not_promote(ptr addrspace(1) %obj) {
entry:
  %slot = alloca ptr addrspace(1), align 8
  store ptr addrspace(1) %obj, ptr %slot, align 8
  call void @safepoint() [ "gc-live"(ptr %slot) ]
  %loaded = load ptr addrspace(1), ptr %slot, align 8
  ret ptr addrspace(1) %loaded
}
