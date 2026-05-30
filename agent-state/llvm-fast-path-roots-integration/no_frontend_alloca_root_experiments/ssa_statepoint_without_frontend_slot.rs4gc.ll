; ModuleID = 'agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/ssa_statepoint_without_frontend_slot.ll'
source_filename = "agent-state/llvm-fast-path-roots-integration/no_frontend_alloca_root_experiments/ssa_statepoint_without_frontend_slot.ll"

declare void @safepoint()

define ptr addrspace(1) @ssa_live_value(ptr addrspace(1) %obj) gc "statepoint-example" {
entry:
  %statepoint_token = call token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 2882400000, i32 0, ptr elementtype(void ()) @safepoint, i32 0, i32 0, i32 0, i32 0) [ "deopt"(), "gc-live"(ptr addrspace(1) %obj) ]
  %obj.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%obj, %obj)
  ret ptr addrspace(1) %obj.relocated
}

declare void @__tmp_use(...)

declare token @llvm.experimental.gc.statepoint.p0(i64 immarg, i32 immarg, ptr, i32 immarg, i32 immarg, ...)

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token, i32 immarg, i32 immarg) #0

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }
