define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_with_lemmas_13_33_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %.3.exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %.3.exnroot, align 8
  %.07182.exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %.07182.exnroot, align 8
  %exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %exnroot, align 8
  %exnroot124 = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %exnroot124, align 8
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 408
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L670, label %L671, !prof !1

L670:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 38, i32 0, i32 0) #7 [ "gc-live"(ptr addrspace(1) %3, ptr addrspace(1) %2, ptr %.07182.exnroot, ptr %exnroot, ptr %exnroot124, ptr %.3.exnroot) ]
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%3, %3)
  %11 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 1, i32 1) ; (%2, %2)
  %12 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %13 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L671

L671:                                             ; preds = %L670, %L1
  %.0136 = phi ptr addrspace(1) [ %11, %L670 ], [ %2, %L1 ]
  %.0 = phi ptr addrspace(1) [ %10, %L670 ], [ %3, %L1 ]
  %alloc.0 = phi i64 [ %1, %L1 ], [ %13, %L670 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %12, %L670 ]
  %14 = ptrtoint ptr addrspace(1) %.0 to i64
  %15 = and i64 %14, 1
  %.not7381 = icmp eq i64 %15, 0
  br i1 %.not7381, label %L637, label %L635

common.ret:                                       ; preds = %L678, %L635
  %common.ret.op.gcagg = phi i64 [ %ds.1.lcssa, %L635 ], [ %gcagg115, %L678 ]
  %common.ret.op.gcagg104 = phi i64 [ %alloc.1.lcssa, %L635 ], [ %gcagg116, %L678 ]
  %common.ret.op.gcagg105 = phi ptr addrspace(1) [ %.1, %L635 ], [ %gcagg117, %L678 ]
  %common.ret.op = phi { { i64, i64 }, { ptr addrspace(1) } } [ %18, %L635 ], [ %43, %L678 ]
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L635.loopexit:                                    ; preds = %L660
  %exnroot.load125 = load volatile ptr addrspace(1), ptr %exnroot124, align 8
  br label %L635

L635:                                             ; preds = %L635.loopexit, %L671
  %.1 = phi ptr addrspace(1) [ %.3.exnroot.load, %L635.loopexit ], [ %.0136, %L671 ]
  %alloc.1.lcssa = phi i64 [ %alloc.0, %L671 ], [ %gcagg110, %L635.loopexit ]
  %ds.1.lcssa = phi i64 [ %ds.0, %L671 ], [ %gcagg111, %L635.loopexit ]
  %exnroot.select123 = phi ptr addrspace(1) [ %exnroot.load125, %L635.loopexit ], [ %.0136, %L671 ]
  %16 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds.1.lcssa, 0, 0
  %17 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %16, i64 %alloc.1.lcssa, 0, 1
  %18 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %17, ptr addrspace(1) %exnroot.select123, 1, 0
  br label %common.ret

L637:                                             ; preds = %L671, %L660
  %.2 = phi ptr addrspace(1) [ %.0136, %L671 ], [ %.3.exnroot.load, %L660 ]
  %ds.184 = phi i64 [ %gcagg111, %L660 ], [ %ds.0, %L671 ]
  %alloc.183 = phi i64 [ %gcagg110, %L660 ], [ %alloc.0, %L671 ]
  %.07182 = phi ptr addrspace(1) [ %44, %L660 ], [ %.0, %L671 ]
  %exnroot.select = phi ptr addrspace(1) [ %.0136, %L671 ], [ %exnroot.load, %L660 ]
  %19 = load ptr addrspace(1), ptr addrspace(1) %.07182, align 8
  %20 = add i64 %ds.184, 64
  %21 = inttoptr i64 %20 to ptr
  %22 = load i64, ptr %21, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlBoyer__rewrite_with_lemmas_13_33_code", %L673))
  %remat = getelementptr i8, ptr addrspace(1) %19, i64 8
  %23 = load ptr addrspace(1), ptr addrspace(1) %remat, align 8
  %24 = load ptr addrspace(1), ptr addrspace(1) %19, align 8
  store volatile ptr addrspace(1) %.07182, ptr %.07182.exnroot, align 8
  store volatile ptr addrspace(1) %exnroot.select, ptr %exnroot, align 8
  store volatile ptr addrspace(1) %exnroot.select, ptr %exnroot124, align 8
  store volatile ptr addrspace(1) %.2, ptr %.3.exnroot, align 8
  %statepoint_token127 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlBoyer__unify_9_29_code", i32 4, i32 0, i64 %ds.184, i64 %alloc.183, ptr addrspace(1) %exnroot.select, ptr addrspace(1) %24, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 107, i64 0, i64 31, i64 46, i64 0, i64 46, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115), "gc-live"(ptr addrspace(1) %23, ptr addrspace(1) %.2, ptr %.07182.exnroot, ptr %exnroot, ptr %exnroot124, ptr %.3.exnroot) ]
          to label %L674 unwind label %L673

L673:                                             ; preds = %L676, %L637, %L677
  %25 = landingpad token
          cleanup
  %26 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %.3.exnroot.load = load volatile ptr addrspace(1), ptr %.3.exnroot, align 8
  %.07182.exnroot.load = load volatile ptr addrspace(1), ptr %.07182.exnroot, align 8
  %gcagg108 = extractvalue { ptr addrspace(1), i64, i64, i64 } %26, 0
  %gcagg109 = extractvalue { ptr addrspace(1), i64, i64, i64 } %26, 1
  %gcagg110 = extractvalue { ptr addrspace(1), i64, i64, i64 } %26, 2
  %gcagg111 = extractvalue { ptr addrspace(1), i64, i64, i64 } %26, 3
  %27 = add i64 %gcagg111, 64
  %28 = inttoptr i64 %27 to ptr
  store i64 %22, ptr %28, align 4
  %or.cond.not = icmp eq ptr addrspace(1) %gcagg108, inttoptr (i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64) to ptr addrspace(1))
  br i1 %or.cond.not, label %L660, label %L665

L674:                                             ; preds = %L637
  %29 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token127)
  %30 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token127, i32 0, i32 0) ; (%23, %23)
  %.07182.exnroot.normal.load = load volatile ptr addrspace(1), ptr %.07182.exnroot, align 8
  %exnroot.select.exnroot.normal.load = load volatile ptr addrspace(1), ptr %exnroot, align 8
  %31 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token127, i32 1, i32 1) ; (%.2, %.2)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %29, 0, 0
  %gcagg106 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %29, 0, 1
  %gcagg107 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %29, 1, 0
  %32 = add i64 %gcagg106, -40
  %33 = inttoptr i64 %gcagg to ptr
  %34 = load i64, ptr %33, align 4
  %.not74 = icmp ugt i64 %34, %32
  br i1 %.not74, label %L675, label %L676, !prof !1

L675:                                             ; preds = %L674
  %statepoint_token128 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 327699, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg, i64 %32, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 5, i64 2, i64 71, i64 2, i64 19, i64 60, i64 93, i64 153, i64 8, i64 7958370, i64 3043941, i64 27757, i64 24, i64 7958338, i64 3043941, i64 7368801, i64 6257004, i64 6452595, i64 3044467, i64 6255457, i64 6514034, i64 107, i64 0, i64 18, i64 50, i64 0, i64 50, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115), "gc-live"(ptr addrspace(1) %30, ptr addrspace(1) %31, ptr addrspace(1) %exnroot.select.exnroot.normal.load, ptr addrspace(1) %.07182.exnroot.normal.load, ptr addrspace(1) %gcagg107, ptr %.07182.exnroot, ptr %exnroot, ptr %exnroot124, ptr %.3.exnroot) ]
  %35 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token128)
  %36 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token128, i32 0, i32 0) ; (%30, %30)
  %37 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token128, i32 1, i32 1) ; (%31, %31)
  %exnroot.select.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token128, i32 2, i32 2) ; (%exnroot.select.exnroot.normal.load, %exnroot.select.exnroot.normal.load)
  %.07182.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token128, i32 3, i32 3) ; (%.07182.exnroot.normal.load, %.07182.exnroot.normal.load)
  %gcagg107.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token128, i32 4, i32 4) ; (%gcagg107, %gcagg107)
  %38 = extractvalue { { i64, i64 }, {} } %35, 0, 0
  %39 = extractvalue { { i64, i64 }, {} } %35, 0, 1
  br label %L676

L676:                                             ; preds = %L675, %L674
  %.0140 = phi ptr addrspace(1) [ %gcagg107.relocated, %L675 ], [ %gcagg107, %L674 ]
  %.0139 = phi ptr addrspace(1) [ %exnroot.select.relocated, %L675 ], [ %exnroot.select.exnroot.normal.load, %L674 ]
  %.0138 = phi ptr addrspace(1) [ %.07182.relocated, %L675 ], [ %.07182.exnroot.normal.load, %L674 ]
  %.0137 = phi ptr addrspace(1) [ %36, %L675 ], [ %30, %L674 ]
  %.4 = phi ptr addrspace(1) [ %37, %L675 ], [ %31, %L674 ]
  %alloc.2 = phi i64 [ %32, %L674 ], [ %39, %L675 ]
  %ds.2 = phi i64 [ %gcagg, %L674 ], [ %38, %L675 ]
  %40 = add i64 %alloc.2, 8
  %41 = inttoptr i64 %40 to ptr addrspace(1)
  %remat118 = getelementptr i8, ptr addrspace(1) %41, i64 -8
  store volatile i64 4343, ptr addrspace(1) %remat118, align 4
  store volatile i64 ptrtoint (ptr @"\01_camlBoyer__as_rec_8_34_code" to i64), ptr addrspace(1) %41, align 4
  %remat119 = getelementptr i8, ptr addrspace(1) %41, i64 8
  store volatile i64 108086391056891909, ptr addrspace(1) %remat119, align 4
  %remat120 = getelementptr i8, ptr addrspace(1) %41, i64 16
  store ptr addrspace(1) %.0137, ptr addrspace(1) %remat120, align 8
  %remat121 = getelementptr i8, ptr addrspace(1) %41, i64 24
  store ptr addrspace(1) %.0140, ptr addrspace(1) %remat121, align 8
  store volatile ptr addrspace(1) %.0138, ptr %.07182.exnroot, align 8
  store volatile ptr addrspace(1) %.0139, ptr %exnroot, align 8
  store volatile ptr addrspace(1) %.4, ptr %exnroot124, align 8
  store volatile ptr addrspace(1) %.4, ptr %.3.exnroot, align 8
  %statepoint_token130 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlBoyer__as_rec_8_34_code", i32 4, i32 0, i64 %ds.2, i64 %alloc.2, ptr addrspace(1) %.0137, ptr addrspace(1) %41, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 75, i64 0, i64 2, i64 13, i64 0, i64 13, i64 8, i64 7958370, i64 3043941, i64 27757, i64 17, i64 7958338, i64 3043941, i64 7368801, i64 6257004, i64 6452595, i64 29811, i64 107, i64 0, i64 18, i64 50, i64 0, i64 50, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115), "gc-live"(ptr addrspace(1) %.0139, ptr %.07182.exnroot, ptr %exnroot, ptr %exnroot124, ptr %.3.exnroot) ]
          to label %L677 unwind label %L673

L677:                                             ; preds = %L676
  %42 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token130)
  %exnroot.select.relocated131 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token130, i32 0, i32 0) ; (%.0139, %.0139)
  %.07182.exnroot.normal.load134 = load volatile ptr addrspace(1), ptr %.07182.exnroot, align 8
  %exnroot.normal.load = load volatile ptr addrspace(1), ptr %exnroot, align 8
  %gcagg112 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %42, 0, 0
  %gcagg113 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %42, 0, 1
  %gcagg114 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %42, 1, 0
  store volatile ptr addrspace(1) %.07182.exnroot.normal.load134, ptr %.07182.exnroot, align 8
  store volatile ptr addrspace(1) %exnroot.select.relocated131, ptr %exnroot, align 8
  store volatile ptr addrspace(1) %exnroot.normal.load, ptr %exnroot124, align 8
  store volatile ptr addrspace(1) %exnroot.normal.load, ptr %.3.exnroot, align 8
  %statepoint_token133 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, ptr addrspace(1))) @"\01_camlBoyer__rewrite_12_32_code", i32 3, i32 0, i64 %gcagg112, i64 %gcagg113, ptr addrspace(1) %gcagg114, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 107, i64 0, i64 10, i64 50, i64 0, i64 50, i64 8, i64 7958370, i64 3043941, i64 27757, i64 25, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 7823205, i64 6845545, i64 6646879, i64 6385005, i64 115), "gc-live"(ptr %.07182.exnroot, ptr %exnroot, ptr %exnroot124, ptr %.3.exnroot) ]
          to label %L678 unwind label %L673

L678:                                             ; preds = %L677
  %43 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token133)
  %exnroot.normal.load135 = load volatile ptr addrspace(1), ptr %exnroot, align 8
  %gcagg115 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %43, 0, 0
  %gcagg116 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %43, 0, 1
  %gcagg117 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %43, 1, 0
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %common.ret

L660:                                             ; preds = %L673
  %remat122 = getelementptr i8, ptr addrspace(1) %.07182.exnroot.load, i64 8
  %44 = load ptr addrspace(1), ptr addrspace(1) %remat122, align 8
  %45 = ptrtoint ptr addrspace(1) %44 to i64
  %46 = and i64 %45, 1
  %.not73 = icmp eq i64 %46, 0
  %exnroot.load = load volatile ptr addrspace(1), ptr %exnroot, align 8
  br i1 %.not73, label %L637, label %L635.loopexit

L665:                                             ; preds = %L673
  %47 = ptrtoint ptr addrspace(1) %gcagg108 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %47)
  unreachable
}
