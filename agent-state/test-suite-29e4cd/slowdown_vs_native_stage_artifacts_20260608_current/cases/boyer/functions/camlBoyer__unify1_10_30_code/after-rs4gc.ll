define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__unify1_10_30_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture %3, ptr addrspace(1) %4) #1 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %exnroot, align 8
  %exnroot143 = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %exnroot143, align 8
  %exnroot145 = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %exnroot145, align 8
  %remat = getelementptr i8, ptr addrspace(1) %3, i64 -8
  %5 = load i8, ptr addrspace(1) %remat, align 1
  %.not = icmp eq i8 %5, 0
  br i1 %.not, label %L511, label %L490

L490:                                             ; preds = %L1
  %remat134 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %6 = load i8, ptr addrspace(1) %remat134, align 1
  %.not116 = icmp eq i8 %6, 0
  br i1 %.not116, label %L508, label %L494

L494:                                             ; preds = %L490
  %7 = load ptr addrspace(1), ptr addrspace(1) %3, align 8
  %8 = load ptr addrspace(1), ptr addrspace(1) %2, align 8
  %or.cond.not119 = icmp eq ptr addrspace(1) %8, %7
  br i1 %or.cond.not119, label %L498, label %L505

L498:                                             ; preds = %L494
  %remat135 = getelementptr i8, ptr addrspace(1) %3, i64 8
  %9 = load ptr addrspace(1), ptr addrspace(1) %remat135, align 8
  %remat136 = getelementptr i8, ptr addrspace(1) %2, i64 8
  %10 = load ptr addrspace(1), ptr addrspace(1) %remat136, align 8
  %11 = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__unify1_lst_11_31_code"(i64 %0, i64 %1, ptr addrspace(1) %10, ptr addrspace(1) %9, ptr addrspace(1) %4) #9
  ret { { i64, i64 }, { ptr addrspace(1) } } %11

L505:                                             ; preds = %L494
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64))
  unreachable

L508:                                             ; preds = %L490
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64))
  unreachable

L511:                                             ; preds = %L1
  %12 = load ptr addrspace(1), ptr addrspace(1) %3, align 8
  %13 = add i64 %0, 64
  %14 = inttoptr i64 %13 to ptr
  %15 = load i64, ptr %14, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlBoyer__unify1_10_30_code", %L573))
  %16 = ptrtoint ptr addrspace(1) %4 to i64
  %17 = and i64 %16, 1
  %.not112 = icmp eq i64 %17, 0
  br i1 %.not112, label %L533, label %L531

L573:                                             ; preds = %L537, %L551, %L531
  %18 = landingpad token
          cleanup
  %19 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %exnroot.load = load volatile ptr addrspace(1), ptr %exnroot, align 8
  %exnroot.load144 = load volatile ptr addrspace(1), ptr %exnroot143, align 8
  %exnroot.load146 = load volatile ptr addrspace(1), ptr %exnroot145, align 8
  %gcagg = extractvalue { ptr addrspace(1), i64, i64, i64 } %19, 0
  %gcagg126 = extractvalue { ptr addrspace(1), i64, i64, i64 } %19, 1
  %gcagg127 = extractvalue { ptr addrspace(1), i64, i64, i64 } %19, 2
  %gcagg128 = extractvalue { ptr addrspace(1), i64, i64, i64 } %19, 3
  %20 = add i64 %gcagg128, 64
  %21 = inttoptr i64 %20 to ptr
  store i64 %15, ptr %21, align 4
  %22 = load ptr addrspace(1), ptr addrspace(1) %gcagg, align 8
  %or.cond118.not = icmp eq ptr addrspace(1) %22, inttoptr (i64 ptrtoint (ptr @"\01_caml_exn_Failure" to i64) to ptr addrspace(1))
  br i1 %or.cond118.not, label %L562, label %L566

L531:                                             ; preds = %L541, %L511
  store volatile ptr addrspace(1) %12, ptr %exnroot, align 8
  store volatile ptr addrspace(1) %2, ptr %exnroot143, align 8
  store volatile ptr addrspace(1) %4, ptr %exnroot145, align 8
  invoke void @llvm.aarch64.oxcaml.raise.notrace.edge(i64 ptrtoint (ptr @"\01_camlBoyer__Pmakeblock3333" to i64), ptr blockaddress(@"\01_camlBoyer__unify1_10_30_code", %L573))
          to label %L574 unwind label %L573

L574:                                             ; preds = %L531
  unreachable

L533:                                             ; preds = %L511, %L541
  %.0 = phi ptr addrspace(1) [ %27, %L541 ], [ %4, %L511 ]
  %23 = load ptr addrspace(1), ptr addrspace(1) %.0, align 8
  %24 = load ptr addrspace(1), ptr addrspace(1) %23, align 8
  %or.cond117.not = icmp eq ptr addrspace(1) %12, %24
  br i1 %or.cond117.not, label %L537, label %L541

L537:                                             ; preds = %L533
  %remat137 = getelementptr i8, ptr addrspace(1) %23, i64 8
  %25 = load ptr addrspace(1), ptr addrspace(1) %remat137, align 8
  store volatile ptr addrspace(1) %12, ptr %exnroot, align 8
  store volatile ptr addrspace(1) %2, ptr %exnroot143, align 8
  store volatile ptr addrspace(1) %4, ptr %exnroot145, align 8
  %statepoint_token = invoke oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_equal" to i64), ptr addrspace(1) %25, ptr addrspace(1) %2, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 84, i64 0, i64 13, i64 46, i64 0, i64 46, i64 8, i64 7958370, i64 3043941, i64 27757, i64 12, i64 7958338, i64 3043941, i64 6909557, i64 3242342), "gc-live"(ptr addrspace(1) %25, ptr addrspace(1) %2, ptr %exnroot, ptr %exnroot143, ptr %exnroot145) ]
          to label %L576 unwind label %L573

L576:                                             ; preds = %L537
  %26 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token)
  %exnroot.normal.load = load volatile ptr addrspace(1), ptr %exnroot, align 8
  %exnroot.normal.load148 = load volatile ptr addrspace(1), ptr %exnroot143, align 8
  %exnroot.normal.load149 = load volatile ptr addrspace(1), ptr %exnroot145, align 8
  %gcagg129 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %26, 0, 0
  %gcagg130 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %26, 0, 1
  %gcagg131 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %26, 1, 0
  %or.cond.not = icmp eq ptr addrspace(1) %gcagg131, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L551, label %L553

L541:                                             ; preds = %L533
  %remat138 = getelementptr i8, ptr addrspace(1) %.0, i64 8
  %27 = load ptr addrspace(1), ptr addrspace(1) %remat138, align 8
  %28 = ptrtoint ptr addrspace(1) %27 to i64
  %29 = and i64 %28, 1
  %.not115 = icmp eq i64 %29, 0
  br i1 %.not115, label %L533, label %L531

L551:                                             ; preds = %L576
  store volatile ptr addrspace(1) %exnroot.normal.load, ptr %exnroot, align 8
  store volatile ptr addrspace(1) %exnroot.normal.load148, ptr %exnroot143, align 8
  store volatile ptr addrspace(1) %exnroot.normal.load149, ptr %exnroot145, align 8
  invoke void @llvm.aarch64.oxcaml.raise.notrace.edge(i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64), ptr blockaddress(@"\01_camlBoyer__unify1_10_30_code", %L573))
          to label %L578 unwind label %L573

L578:                                             ; preds = %L551
  unreachable

common.ret:                                       ; preds = %L581, %L553
  %.pn.gcagg = phi i64 [ %gcagg129, %L553 ], [ %ds.0, %L581 ]
  %.pn.gcagg132 = phi i64 [ %gcagg130, %L553 ], [ %alloc.0, %L581 ]
  %.pn.gcagg133 = phi ptr addrspace(1) [ %gcagg131, %L553 ], [ poison, %L581 ]
  %.pn = phi { { i64, i64 }, { ptr addrspace(1) } } [ %26, %L553 ], [ %40, %L581 ]
  %.pn114 = phi ptr addrspace(1) [ %exnroot.normal.load149, %L553 ], [ %37, %L581 ]
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn114, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L553:                                             ; preds = %L576
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %common.ret

L562:                                             ; preds = %L573
  %30 = add i64 %gcagg127, -48
  %31 = inttoptr i64 %gcagg128 to ptr
  %32 = load i64, ptr %31, align 4
  %.not113 = icmp ugt i64 %32, %30
  br i1 %.not113, label %L580, label %L581, !prof !1

L580:                                             ; preds = %L562
  %statepoint_token147 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 393217, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg128, i64 %30, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 2, i64 3, i64 1, i64 85, i64 0, i64 24, i64 54, i64 0, i64 54, i64 8, i64 7958370, i64 3043941, i64 27757, i64 12, i64 7958338, i64 3043941, i64 6909557, i64 3242342, i64 3, i64 1, i64 85, i64 0, i64 24, i64 39, i64 0, i64 39, i64 8, i64 7958370, i64 3043941, i64 27757, i64 12, i64 7958338, i64 3043941, i64 6909557, i64 3242342), "gc-live"(ptr addrspace(1) %exnroot.load146, ptr addrspace(1) %exnroot.load144, ptr addrspace(1) %exnroot.load, ptr %exnroot, ptr %exnroot143, ptr %exnroot145) ]
  %33 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token147)
  %exnroot.load146.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token147, i32 0, i32 0) ; (%exnroot.load146, %exnroot.load146)
  %exnroot.load144.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token147, i32 1, i32 1) ; (%exnroot.load144, %exnroot.load144)
  %exnroot.load.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token147, i32 2, i32 2) ; (%exnroot.load, %exnroot.load)
  %34 = extractvalue { { i64, i64 }, {} } %33, 0, 0
  %35 = extractvalue { { i64, i64 }, {} } %33, 0, 1
  br label %L581

L581:                                             ; preds = %L580, %L562
  %.0152 = phi ptr addrspace(1) [ %exnroot.load.relocated, %L580 ], [ %exnroot.load, %L562 ]
  %.0151 = phi ptr addrspace(1) [ %exnroot.load144.relocated, %L580 ], [ %exnroot.load144, %L562 ]
  %.0150 = phi ptr addrspace(1) [ %exnroot.load146.relocated, %L580 ], [ %exnroot.load146, %L562 ]
  %alloc.0 = phi i64 [ %30, %L562 ], [ %35, %L580 ]
  %ds.0 = phi i64 [ %gcagg128, %L562 ], [ %34, %L580 ]
  %36 = add i64 %alloc.0, 8
  %37 = inttoptr i64 %36 to ptr addrspace(1)
  %38 = getelementptr i8, ptr addrspace(1) %37, i64 24, !is_base_value !2
  %remat139 = getelementptr i8, ptr addrspace(1) %38, i64 -8
  store volatile i64 2048, ptr addrspace(1) %remat139, align 4
  store ptr addrspace(1) %.0152, ptr addrspace(1) %38, align 8
  %remat140 = getelementptr i8, ptr addrspace(1) %38, i64 8
  store ptr addrspace(1) %.0151, ptr addrspace(1) %remat140, align 8
  %remat141 = getelementptr i8, ptr addrspace(1) %37, i64 -8
  store volatile i64 2048, ptr addrspace(1) %remat141, align 4
  store ptr addrspace(1) %38, ptr addrspace(1) %37, align 8
  %remat142 = getelementptr i8, ptr addrspace(1) %37, i64 8
  store ptr addrspace(1) %.0150, ptr addrspace(1) %remat142, align 8
  %39 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds.0, 0, 0
  %40 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %39, i64 %alloc.0, 0, 1
  br label %common.ret

L566:                                             ; preds = %L573
  %41 = ptrtoint ptr addrspace(1) %gcagg to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %41)
  unreachable
}
