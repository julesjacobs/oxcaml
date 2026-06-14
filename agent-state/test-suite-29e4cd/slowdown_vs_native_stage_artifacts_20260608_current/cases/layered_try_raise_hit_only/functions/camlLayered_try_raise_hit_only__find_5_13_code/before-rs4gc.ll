define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLayered_try_raise_hit_only__find_5_13_code"(i64 %0, i64 %1, ptr addrspace(1) nocapture readonly %2) #3 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %3 = add i64 %0, 40
  %4 = inttoptr i64 %3 to ptr
  %5 = load i64, ptr %4, align 4
  %6 = add i64 %5, 408
  %7 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %7, %6
  br i1 %.not, label %L167, label %L136.preheader, !prof !1

L167:                                             ; preds = %L1
  %8 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 38) #7
  %9 = extractvalue { { i64, i64 }, {} } %8, 0, 0
  %10 = extractvalue { { i64, i64 }, {} } %8, 0, 1
  br label %L136.preheader

L136.preheader:                                   ; preds = %L167, %L1
  %alloc.1.ph = phi i64 [ %1, %L1 ], [ %10, %L167 ]
  %ds.1.ph = phi i64 [ %0, %L1 ], [ %9, %L167 ]
  br label %L136

L136:                                             ; preds = %L136.preheader, %L156
  %.0 = phi ptr addrspace(1) [ %24, %L156 ], [ %2, %L136.preheader ]
  %alloc.1 = phi i64 [ %18, %L156 ], [ %alloc.1.ph, %L136.preheader ]
  %ds.1 = phi i64 [ %19, %L156 ], [ %ds.1.ph, %L136.preheader ]
  %.040.in = getelementptr i8, ptr addrspace(1) %.0, i64 8
  %.040 = load ptr addrspace(1), ptr addrspace(1) %.040.in, align 8
  %11 = add i64 %ds.1, 64
  %12 = inttoptr i64 %11 to ptr
  %13 = load i64, ptr %12, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlLayered_try_raise_hit_only__find_5_13_code", %L170))
  %14 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLayered_try_raise_hit_only__probe_4_12_code"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %.0) #9 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 23, i64 0, i64 6, i64 15, i64 0, i64 15, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 31, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 7235942, i64 100) ]
          to label %L171 unwind label %L170

L170:                                             ; preds = %L136
  %15 = landingpad token
          cleanup
  %16 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %17 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 0
  %18 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 2
  %19 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 3
  %20 = add i64 %19, 64
  %21 = inttoptr i64 %20 to ptr
  store i64 %13, ptr %21, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %17, inttoptr (i64 ptrtoint (ptr @"\01_camlLayered_try_raise_hit_only__Not_found_same275" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L152, label %L162

L171:                                             ; preds = %L136
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  ret { { i64, i64 }, { i64 } } %14

L152:                                             ; preds = %L170
  %22 = ptrtoint ptr addrspace(1) %.040 to i64
  %23 = and i64 %22, 1
  %.not42 = icmp eq i64 %23, 0
  br i1 %.not42, label %L156, label %L154

L154:                                             ; preds = %L152
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlLayered_try_raise_hit_only__Not_found_same275" to i64))
  unreachable

L156:                                             ; preds = %L152
  %24 = load ptr addrspace(1), ptr addrspace(1) %.040, align 8
  br label %L136

L162:                                             ; preds = %L170
  %25 = extractvalue { ptr addrspace(1), i64, i64, i64 } %16, 0
  %26 = ptrtoint ptr addrspace(1) %25 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %26)
  unreachable
}
