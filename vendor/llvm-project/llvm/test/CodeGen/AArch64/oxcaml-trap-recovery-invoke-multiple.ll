; RUN: opt -S -O2 < %s | llc -mtriple=arm64-apple-macosx -verify-machineinstrs > /dev/null
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=finalize-isel < %s | FileCheck %s
; RUN: llc -mtriple=arm64-apple-macosx -verify-machineinstrs -stop-after=branch-folder < %s | FileCheck %s
; RUN: llc -mtriple=aarch64-linux-gnu -verify-machineinstrs < %s > /dev/null

declare void @llvm.aarch64.oxcaml.trap.publish(ptr, i64, ptr)
declare { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
declare oxcaml_nofpcc { i64, i64, i64 } @callee_a(i64, i64, i64)
declare oxcaml_nofpcc { i64, i64, i64 } @callee_b(i64, i64, i64)
declare oxcaml_nofpcc { i64, i64, i64 } @callee_outer(i64, i64, i64)
declare oxcaml_nofpcc { i64, i64, i64 } @callee_inner(i64, i64, i64)
declare i32 @__gxx_personality_v0(...)

define oxcaml_nofpcc { i64, i64, i64 } @two_independent_trap_regions(i64 %ds, i64 %alloc, ptr %trap_a, ptr %trap_b, i64 %value, i64 %cond_int) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  %cond = icmp ne i64 %cond_int, 0
  br i1 %cond, label %try_a, label %try_b

try_a:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_a,
      i64 %value,
      ptr blockaddress(@two_independent_trap_regions, %exn_a))
  %a = invoke oxcaml_nofpcc { i64, i64, i64 } @callee_a(i64 %ds, i64 %alloc, i64 %value)
       to label %normal_a unwind label %exn_a

try_b:
  %value_b = add i64 %value, 1
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %trap_b,
      i64 %value_b,
      ptr blockaddress(@two_independent_trap_regions, %exn_b))
  %b = invoke oxcaml_nofpcc { i64, i64, i64 } @callee_b(i64 %ds, i64 %alloc, i64 %value_b)
       to label %normal_b unwind label %exn_b

normal_a:
  br label %normal_join

normal_b:
  br label %normal_join

normal_join:
  %normal_ret = phi { i64, i64, i64 } [ %a, %normal_a ], [ %b, %normal_b ]
  ret { i64, i64, i64 } %normal_ret

exn_a:
  %lp_a = landingpad token cleanup
  %rec_a = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket_a = extractvalue { i64, i64, i64, i64 } %rec_a, 0
  %alloc_a = extractvalue { i64, i64, i64, i64 } %rec_a, 2
  %domain_a = extractvalue { i64, i64, i64, i64 } %rec_a, 3
  br label %shared_handler

exn_b:
  %lp_b = landingpad token cleanup
  %rec_b = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %bucket_b = extractvalue { i64, i64, i64, i64 } %rec_b, 0
  %alloc_b = extractvalue { i64, i64, i64, i64 } %rec_b, 2
  %domain_b = extractvalue { i64, i64, i64, i64 } %rec_b, 3
  br label %shared_handler

shared_handler:
  %handler_bucket = phi i64 [ %bucket_a, %exn_a ], [ %bucket_b, %exn_b ]
  %handler_alloc = phi i64 [ %alloc_a, %exn_a ], [ %alloc_b, %exn_b ]
  %handler_domain = phi i64 [ %domain_a, %exn_a ], [ %domain_b, %exn_b ]
  %exn_ret0 = insertvalue { i64, i64, i64 } poison, i64 %handler_domain, 0
  %exn_ret1 = insertvalue { i64, i64, i64 } %exn_ret0, i64 %handler_alloc, 1
  %exn_ret2 = insertvalue { i64, i64, i64 } %exn_ret1, i64 %handler_bucket, 2
  ret { i64, i64, i64 } %exn_ret2
}

define oxcaml_nofpcc { i64, i64, i64 } @nested_trap_regions_same_handler(i64 %ds, i64 %alloc, ptr %outer_trap, ptr %inner_trap, i64 %value, i64 %use_inner_int) gc "ocaml" personality ptr @__gxx_personality_v0 {
entry:
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %outer_trap,
      i64 %value,
      ptr blockaddress(@nested_trap_regions_same_handler, %outer_exn))
  %use_inner = icmp ne i64 %use_inner_int, 0
  br i1 %use_inner, label %inner_setup, label %outer_call

outer_call:
  %outer = invoke oxcaml_nofpcc { i64, i64, i64 } @callee_outer(i64 %ds, i64 %alloc, i64 %value)
           to label %outer_normal unwind label %outer_exn

inner_setup:
  %inner_value = add i64 %value, 10
  call void @llvm.aarch64.oxcaml.trap.publish(
      ptr %inner_trap,
      i64 %inner_value,
      ptr blockaddress(@nested_trap_regions_same_handler, %inner_exn))
  %inner = invoke oxcaml_nofpcc { i64, i64, i64 } @callee_inner(i64 %ds, i64 %alloc, i64 %inner_value)
           to label %inner_normal unwind label %inner_exn

outer_normal:
  br label %normal_join

inner_normal:
  br label %normal_join

normal_join:
  %normal_ret = phi { i64, i64, i64 } [ %outer, %outer_normal ], [ %inner, %inner_normal ]
  ret { i64, i64, i64 } %normal_ret

outer_exn:
  %outer_lp = landingpad token cleanup
  %outer_rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %outer_bucket = extractvalue { i64, i64, i64, i64 } %outer_rec, 0
  %outer_alloc = extractvalue { i64, i64, i64, i64 } %outer_rec, 2
  %outer_domain = extractvalue { i64, i64, i64, i64 } %outer_rec, 3
  br label %same_handler

inner_exn:
  %inner_lp = landingpad token cleanup
  %inner_rec = call { i64, i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %inner_bucket = extractvalue { i64, i64, i64, i64 } %inner_rec, 0
  %inner_alloc = extractvalue { i64, i64, i64, i64 } %inner_rec, 2
  %inner_domain = extractvalue { i64, i64, i64, i64 } %inner_rec, 3
  br label %same_handler

same_handler:
  %bucket = phi i64 [ %outer_bucket, %outer_exn ], [ %inner_bucket, %inner_exn ]
  %recovered_alloc = phi i64 [ %outer_alloc, %outer_exn ], [ %inner_alloc, %inner_exn ]
  %recovered_domain = phi i64 [ %outer_domain, %outer_exn ], [ %inner_domain, %inner_exn ]
  %exn_ret0 = insertvalue { i64, i64, i64 } poison, i64 %recovered_domain, 0
  %exn_ret1 = insertvalue { i64, i64, i64 } %exn_ret0, i64 %recovered_alloc, 1
  %exn_ret2 = insertvalue { i64, i64, i64 } %exn_ret1, i64 %bucket, 2
  ret { i64, i64, i64 } %exn_ret2
}

; CHECK-LABEL: name: two_independent_trap_regions
; CHECK: bb.{{[0-9]+}}.exn_a (machine-block-address-taken, ir-block-address-taken %ir-block.exn_a, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28
; CHECK: bb.{{[0-9]+}}.exn_b (machine-block-address-taken, ir-block-address-taken %ir-block.exn_b, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28

; CHECK-LABEL: name: nested_trap_regions_same_handler
; CHECK: bb.{{[0-9]+}}.outer_call:
; CHECK: successors: {{.*}}%bb.[[OUTER:[0-9]+]]
; CHECK: bb.{{[0-9]+}}.inner_setup:
; CHECK: successors: {{.*}}%bb.[[INNER:[0-9]+]]
; CHECK: bb.[[OUTER]].outer_exn (machine-block-address-taken, ir-block-address-taken %ir-block.outer_exn, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28
; CHECK: bb.[[INNER]].inner_exn (machine-block-address-taken, ir-block-address-taken %ir-block.inner_exn, runtime-entered):
; CHECK-NEXT: successors:
; CHECK-NEXT: liveins: $x0, $x26, $x27, $x28
