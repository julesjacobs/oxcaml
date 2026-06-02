; RUN: opt -S -passes=rewrite-statepoints-for-gc,verify < %s | FileCheck %s

target triple = "arm64-apple-macosx"

declare oxcaml_nofpcc void @callee_v(i64, i64, ptr addrspace(1))
declare oxcaml_nofpcc void @callee_w(i64, i64, ptr addrspace(1))
declare void @use(ptr addrspace(1))
declare void @use2(ptr addrspace(1), ptr addrspace(1))
declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare ptr @personality(...)

; CASE 01: case01_handler_live_direct
; CHECK-LABEL: define {{.*}}@case01_handler_live_direct(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case01_handler_live_direct(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case01_handler_live_direct, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 02: case02_handler_phi_two_invokes_diff
; CHECK-LABEL: define {{.*}}@case02_handler_phi_two_invokes_diff(
; CHECK-COUNT-3: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case02_handler_phi_two_invokes_diff(i1 %cond, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a, ptr addrspace(1) %b) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case02_handler_phi_two_invokes_diff, %recover))
  br i1 %cond, label %left, label %right
left:
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %recover
normal_left:
  ret void
right:
  invoke oxcaml_nofpcc void @callee_w(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %recover
normal_right:
  ret void
recover:
  %handler = phi ptr addrspace(1) [ %a, %left ], [ %b, %right ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %handler)
  ret void
}

; CASE 03: case03_handler_phi_two_invokes_same
; CHECK-LABEL: define {{.*}}@case03_handler_phi_two_invokes_same(
; CHECK-COUNT-3: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case03_handler_phi_two_invokes_same(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case03_handler_phi_two_invokes_same, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %after1 unwind label %recover
after1:
  invoke oxcaml_nofpcc void @callee_w(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %handler = phi ptr addrspace(1) [ %a, %entry ], [ %a, %after1 ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %handler)
  ret void
}

; CASE 04: case04_handler_phi_three_invokes
; CHECK-LABEL: define {{.*}}@case04_handler_phi_three_invokes(
; CHECK-COUNT-4: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case04_handler_phi_three_invokes(i64 %tag, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a, ptr addrspace(1) %b, ptr addrspace(1) %c) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case04_handler_phi_three_invokes, %recover))
  switch i64 %tag, label %left [ i64 1, label %middle i64 2, label %right ]
left:
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %recover
normal_left:
  ret void
middle:
  invoke oxcaml_nofpcc void @callee_w(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_middle unwind label %recover
normal_middle:
  ret void
right:
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %c)
      to label %normal_right unwind label %recover
normal_right:
  ret void
recover:
  %handler = phi ptr addrspace(1) [ %a, %left ], [ %b, %middle ], [ %c, %right ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %handler)
  ret void
}

; CASE 05: case05_split_lpad_phi
; CHECK-LABEL: define {{.*}}@case05_split_lpad_phi(
; CHECK-COUNT-3: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case05_split_lpad_phi(i1 %cond, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a, ptr addrspace(1) %b) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case05_split_lpad_phi, %recover))
  br i1 %cond, label %left, label %right
left:
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %left_lpad
normal_left:
  ret void
right:
  invoke oxcaml_nofpcc void @callee_w(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %right_lpad
normal_right:
  ret void
left_lpad:
  %left_lp = landingpad token cleanup
  br label %recover
right_lpad:
  %right_lp = landingpad token cleanup
  br label %recover
recover:
  %handler = phi ptr addrspace(1) [ %a, %left_lpad ], [ %b, %right_lpad ]
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %handler)
  ret void
}

; CASE 06: case06_handler_live_derived_gep
; CHECK-LABEL: define {{.*}}@case06_handler_live_derived_gep(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case06_handler_live_derived_gep(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case06_handler_live_derived_gep, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %field = getelementptr i8, ptr addrspace(1) %a, i64 8
  call void @use(ptr addrspace(1) %field)
  ret void
}

; CASE 07: case07_handler_live_freeze
; CHECK-LABEL: define {{.*}}@case07_handler_live_freeze(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case07_handler_live_freeze(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case07_handler_live_freeze, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %f = freeze ptr addrspace(1) %a
  call void @use(ptr addrspace(1) %f)
  ret void
}

; CASE 08: case08_boundary_switch_two_edges
; CHECK-LABEL: define {{.*}}@case08_boundary_switch_two_edges(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case08_boundary_switch_two_edges(i64 %tag, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case08_boundary_switch_two_edges, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  br label %join
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  switch i64 %tag, label %other [ i64 1, label %join i64 2, label %join ]
other:
  br label %join
join:
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 09: case09_boundary_switch_three_edges
; CHECK-LABEL: define {{.*}}@case09_boundary_switch_three_edges(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case09_boundary_switch_three_edges(i64 %tag, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case09_boundary_switch_three_edges, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  br label %join
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  switch i64 %tag, label %join [ i64 1, label %join i64 2, label %join i64 3, label %join ]
join:
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 10: case10_boundary_cond_same_successor
; CHECK-LABEL: define {{.*}}@case10_boundary_cond_same_successor(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case10_boundary_cond_same_successor(i1 %cond, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case10_boundary_cond_same_successor, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  br label %join
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br i1 %cond, label %join, label %join
join:
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 11: case11_boundary_via_trampoline
; CHECK-LABEL: define {{.*}}@case11_boundary_via_trampoline(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case11_boundary_via_trampoline(i1 %cond, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case11_boundary_via_trampoline, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  br label %join
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br i1 %cond, label %tramp1, label %tramp2
tramp1:
  br label %join
tramp2:
  br label %join
join:
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 12: case12_boundary_local_loaded_value
; CHECK-LABEL: define {{.*}}@case12_boundary_local_loaded_value(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case12_boundary_local_loaded_value(i1 %retry, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case12_boundary_local_loaded_value, %recover))
  br label %try
try:
  %cur = phi ptr addrspace(1) [ %a, %entry ], [ %next, %retry_block ]
  %addr = getelementptr i8, ptr addrspace(1) %cur, i64 8
  %next = load ptr addrspace(1), ptr addrspace(1) %addr, align 8
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %next)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br i1 %retry, label %retry_block, label %exit
retry_block:
  br label %try
exit:
  call void @use(ptr addrspace(1) %next)
  ret void
}

; CASE 13: case13_two_roots_same_handler
; CHECK-LABEL: define {{.*}}@case13_two_roots_same_handler(
; CHECK-COUNT-4: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case13_two_roots_same_handler(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a, ptr addrspace(1) %b) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case13_two_roots_same_handler, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use2(ptr addrspace(1) %a, ptr addrspace(1) %b)
  ret void
}

; CASE 14: case14_same_root_two_uses
; CHECK-LABEL: define {{.*}}@case14_same_root_two_uses(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case14_same_root_two_uses(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case14_same_root_two_uses, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %a)
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 15: case15_base_equiv_ptrtoint_roundtrip
; CHECK-LABEL: define {{.*}}@case15_base_equiv_ptrtoint_roundtrip(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case15_base_equiv_ptrtoint_roundtrip(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case15_base_equiv_ptrtoint_roundtrip, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %i = ptrtoint ptr addrspace(1) %a to i64
  %round = inttoptr i64 %i to ptr addrspace(1)
  call void @use(ptr addrspace(1) %round)
  ret void
}

; CASE 16: case16_int_arithmetic_derived
; CHECK-LABEL: define {{.*}}@case16_int_arithmetic_derived(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case16_int_arithmetic_derived(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case16_int_arithmetic_derived, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %i = ptrtoint ptr addrspace(1) %a to i64
  %j = add i64 %i, 8
  %derived = inttoptr i64 %j to ptr addrspace(1)
  call void @use(ptr addrspace(1) %derived)
  ret void
}

; CASE 17: case17_two_independent_recover_uses
; CHECK-LABEL: define {{.*}}@case17_two_independent_recover_uses(
; CHECK-COUNT-4: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case17_two_independent_recover_uses(i1 %cond, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a, ptr addrspace(1) %b) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case17_two_independent_recover_uses, %recover_left))
  br i1 %cond, label %left, label %right
left:
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %recover_left
normal_left:
  ret void
right:
  invoke oxcaml_nofpcc void @callee_w(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %recover_right
normal_right:
  ret void
recover_left:
  %lp_left = landingpad token cleanup
  %rec_left = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %a)
  ret void
recover_right:
  %lp_right = landingpad token cleanup
  %rec_right = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %b)
  ret void
}

; CASE 18: case18_nested_boundary_two_joins
; CHECK-LABEL: define {{.*}}@case18_nested_boundary_two_joins(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case18_nested_boundary_two_joins(i64 %tag, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case18_nested_boundary_two_joins, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  br label %join2
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  switch i64 %tag, label %join1 [ i64 1, label %join1 i64 2, label %join1 ]
join1:
  br label %join2
join2:
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 19: case19_recover_phi_to_boundary
; CHECK-LABEL: define {{.*}}@case19_recover_phi_to_boundary(
; CHECK-COUNT-3: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case19_recover_phi_to_boundary(i1 %cond, i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a, ptr addrspace(1) %b) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case19_recover_phi_to_boundary, %recover))
  br i1 %cond, label %left, label %right
left:
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal_left unwind label %recover
normal_left:
  ret void
right:
  invoke oxcaml_nofpcc void @callee_w(i64 %ds, i64 %alloc, ptr addrspace(1) %b)
      to label %normal_right unwind label %recover
normal_right:
  ret void
recover:
  %handler = phi ptr addrspace(1) [ %a, %left ], [ %b, %right ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  br i1 %cond, label %join, label %join
join:
  call void @use(ptr addrspace(1) %handler)
  ret void
}

; CASE 20: case20_call_after_recover_keeps_slot_live
; CHECK-LABEL: define {{.*}}@case20_call_after_recover_keeps_slot_live(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case20_call_after_recover_keeps_slot_live(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case20_call_after_recover_keeps_slot_live, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %a)
  call void @use(ptr addrspace(1) %a)
  ret void
}

; CASE 21: case21_same_store_program_different_logical_roots
; CHECK-LABEL: define {{.*}}@case21_same_store_program_different_logical_roots(
; CHECK-COUNT-2: store volatile ptr addrspace(1)
define oxcaml_nofpcc void @case21_same_store_program_different_logical_roots(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case21_same_store_program_different_logical_roots, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  ret void
recover:
  %handler = phi ptr addrspace(1) [ %a, %entry ]
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use2(ptr addrspace(1) %handler, ptr addrspace(1) %a)
  ret void
}

; CASE 22: case22_normal_and_exception_use_share_root_slot
; CHECK-LABEL: define {{.*}}@case22_normal_and_exception_use_share_root_slot(
; CHECK: %a.exnroot = alloca ptr addrspace(1)
; CHECK: store volatile ptr addrspace(1) %a, ptr %a.exnroot
; CHECK: invoke {{.*}}@llvm.experimental.gc.statepoint
; CHECK-SAME: [ "gc-live"(ptr %a.exnroot) ]
; CHECK: load volatile ptr addrspace(1), ptr %a.exnroot
; CHECK-NOT: gc.relocate{{.*}} ; (%a, %a)
define oxcaml_nofpcc void @case22_normal_and_exception_use_share_root_slot(i64 %ds, i64 %alloc, ptr %trap, ptr addrspace(1) %a) gc "oxcaml" personality ptr @personality {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(ptr %trap, i64 1, ptr blockaddress(@case22_normal_and_exception_use_share_root_slot, %recover))
  invoke oxcaml_nofpcc void @callee_v(i64 %ds, i64 %alloc, ptr addrspace(1) %a)
      to label %normal unwind label %recover
normal:
  call void @use(ptr addrspace(1) %a)
  ret void
recover:
  %lp = landingpad token cleanup
  %rec = call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  call void @use(ptr addrspace(1) %a)
  ret void
}
