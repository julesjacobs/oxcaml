define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLayered_try_raise_hit_only__find_5_13_code"(i64 %0, i64 %1, ptr addrspace(1) nocapture %2) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %.040.exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %.040.exnroot, align 8
  %3 = add i64 %0, 40
  %4 = inttoptr i64 %3 to ptr
  %5 = load i64, ptr %4, align 4
  %6 = add i64 %5, 408
  %7 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %7, %6
  br i1 %.not, label %L167, label %L136.preheader, !prof !1

L167:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 38, i32 0, i32 0) #7 [ "gc-live"(ptr addrspace(1) %2, ptr %.040.exnroot) ]
  %8 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %9 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%2, %2)
  %10 = extractvalue { { i64, i64 }, {} } %8, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %8, 0, 1
  br label %L136.preheader

L136.preheader:                                   ; preds = %L167, %L1
  %.062 = phi ptr addrspace(1) [ %9, %L167 ], [ %2, %L1 ]
  %alloc.1.ph = phi i64 [ %1, %L1 ], [ %11, %L167 ]
  %ds.1.ph = phi i64 [ %0, %L1 ], [ %10, %L167 ]
  br label %L136

L136:                                             ; preds = %L136.preheader, %L156
  %.0 = phi ptr addrspace(1) [ %22, %L156 ], [ %.062, %L136.preheader ]
  %alloc.1 = phi i64 [ %gcagg58, %L156 ], [ %alloc.1.ph, %L136.preheader ]
  %ds.1 = phi i64 [ %gcagg59, %L156 ], [ %ds.1.ph, %L136.preheader ]
  %.040.in.remat = getelementptr i8, ptr addrspace(1) %.0, i64 8
  %.040 = load ptr addrspace(1), ptr addrspace(1) %.040.in.remat, align 8
  %12 = add i64 %ds.1, 64
  %13 = inttoptr i64 %12 to ptr
  %14 = load i64, ptr %13, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlLayered_try_raise_hit_only__find_5_13_code", %L170))
  store volatile ptr addrspace(1) %.040, ptr %.040.exnroot, align 8
  %statepoint_token61 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1))) @"\01_camlLayered_try_raise_hit_only__probe_4_12_code", i32 3, i32 0, i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %.0, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 23, i64 0, i64 6, i64 15, i64 0, i64 15, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 31, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 7235942, i64 100), "gc-live"(ptr %.040.exnroot) ]
          to label %L171 unwind label %L170

L170:                                             ; preds = %L136
  %15 = landingpad token
          cleanup
  %16 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %.040.exnroot.load = load volatile ptr addrspace(1), ptr %.040.exnroot, align 8
  %gcagg = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 0
  %gcagg57 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 1
  %gcagg58 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 2
  %gcagg59 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 3
  %17 = add i64 %gcagg59, 64
  %18 = inttoptr i64 %17 to ptr
  store i64 %14, ptr %18, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %gcagg, inttoptr (i64 ptrtoint (ptr @"\01_camlLayered_try_raise_hit_only__Not_found_same275" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L152, label %L162

L171:                                             ; preds = %L136
  %19 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token61)
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  ret { { i64, i64 }, { i64 } } %19

L152:                                             ; preds = %L170
  %20 = ptrtoint ptr addrspace(1) %.040.exnroot.load to i64
  %21 = and i64 %20, 1
  %.not42 = icmp eq i64 %21, 0
  br i1 %.not42, label %L156, label %L154

L154:                                             ; preds = %L152
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlLayered_try_raise_hit_only__Not_found_same275" to i64))
  unreachable

L156:                                             ; preds = %L152
  %22 = load ptr addrspace(1), ptr addrspace(1) %.040.exnroot.load, align 8
  br label %L136

L162:                                             ; preds = %L170
  %23 = ptrtoint ptr addrspace(1) %gcagg to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %23)
  unreachable
}
