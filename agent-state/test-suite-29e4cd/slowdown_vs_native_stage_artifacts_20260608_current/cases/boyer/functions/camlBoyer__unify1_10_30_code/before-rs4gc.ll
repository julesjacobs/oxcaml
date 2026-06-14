define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__unify1_10_30_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture readonly %3, ptr addrspace(1) %4) #1 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %5 = getelementptr i8, ptr addrspace(1) %3, i64 -8
  %6 = load i8, ptr addrspace(1) %5, align 1
  %.not = icmp eq i8 %6, 0
  br i1 %.not, label %L511, label %L490

L490:                                             ; preds = %L1
  %7 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %8 = load i8, ptr addrspace(1) %7, align 1
  %.not116 = icmp eq i8 %8, 0
  br i1 %.not116, label %L508, label %L494

L494:                                             ; preds = %L490
  %9 = load ptr addrspace(1), ptr addrspace(1) %3, align 8
  %10 = load ptr addrspace(1), ptr addrspace(1) %2, align 8
  %or.cond.not119 = icmp eq ptr addrspace(1) %10, %9
  br i1 %or.cond.not119, label %L498, label %L505

L498:                                             ; preds = %L494
  %11 = getelementptr i8, ptr addrspace(1) %3, i64 8
  %12 = load ptr addrspace(1), ptr addrspace(1) %11, align 8
  %13 = getelementptr i8, ptr addrspace(1) %2, i64 8
  %14 = load ptr addrspace(1), ptr addrspace(1) %13, align 8
  %15 = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__unify1_lst_11_31_code"(i64 %0, i64 %1, ptr addrspace(1) %14, ptr addrspace(1) %12, ptr addrspace(1) %4) #6
  ret { { i64, i64 }, { ptr addrspace(1) } } %15

L505:                                             ; preds = %L494
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64))
  unreachable

L508:                                             ; preds = %L490
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64))
  unreachable

L511:                                             ; preds = %L1
  %16 = load ptr addrspace(1), ptr addrspace(1) %3, align 8
  %17 = add i64 %0, 64
  %18 = inttoptr i64 %17 to ptr
  %19 = load i64, ptr %18, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlBoyer__unify1_10_30_code", %L573))
  %20 = ptrtoint ptr addrspace(1) %4 to i64
  %21 = and i64 %20, 1
  %.not112 = icmp eq i64 %21, 0
  br i1 %.not112, label %L533, label %L531

L573:                                             ; preds = %L551, %L537, %L531
  %22 = landingpad token
          cleanup
  %23 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %24 = extractvalue { ptr addrspace(1), i64, i64, i64 } %23, 0
  %25 = extractvalue { ptr addrspace(1), i64, i64, i64 } %23, 3
  %26 = add i64 %25, 64
  %27 = inttoptr i64 %26 to ptr
  store i64 %19, ptr %27, align 4
  %28 = load ptr addrspace(1), ptr addrspace(1) %24, align 8
  %or.cond118.not = icmp eq ptr addrspace(1) %28, inttoptr (i64 ptrtoint (ptr @"\01_caml_exn_Failure" to i64) to ptr addrspace(1))
  br i1 %or.cond118.not, label %L562, label %L566

L531:                                             ; preds = %L541, %L511
  invoke void @llvm.aarch64.oxcaml.raise.notrace.edge(i64 ptrtoint (ptr @"\01_camlBoyer__Pmakeblock3333" to i64), ptr blockaddress(@"\01_camlBoyer__unify1_10_30_code", %L573))
          to label %L574 unwind label %L573

L574:                                             ; preds = %L531
  unreachable

L533:                                             ; preds = %L511, %L541
  %.0 = phi ptr addrspace(1) [ %36, %L541 ], [ %4, %L511 ]
  %29 = load ptr addrspace(1), ptr addrspace(1) %.0, align 8
  %30 = load ptr addrspace(1), ptr addrspace(1) %29, align 8
  %or.cond117.not = icmp eq ptr addrspace(1) %16, %30
  br i1 %or.cond117.not, label %L537, label %L541

L537:                                             ; preds = %L533
  %31 = getelementptr i8, ptr addrspace(1) %29, i64 8
  %32 = load ptr addrspace(1), ptr addrspace(1) %31, align 8
  %33 = invoke oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_equal" to i64), ptr addrspace(1) %32, ptr addrspace(1) %2) #13 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 84, i64 0, i64 13, i64 46, i64 0, i64 46, i64 8, i64 7958370, i64 3043941, i64 27757, i64 12, i64 7958338, i64 3043941, i64 6909557, i64 3242342) ]
          to label %L576 unwind label %L573

L576:                                             ; preds = %L537
  %34 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %33, 1, 0
  %or.cond.not = icmp eq ptr addrspace(1) %34, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L551, label %L553

L541:                                             ; preds = %L533
  %35 = getelementptr i8, ptr addrspace(1) %.0, i64 8
  %36 = load ptr addrspace(1), ptr addrspace(1) %35, align 8
  %37 = ptrtoint ptr addrspace(1) %36 to i64
  %38 = and i64 %37, 1
  %.not115 = icmp eq i64 %38, 0
  br i1 %.not115, label %L533, label %L531

L551:                                             ; preds = %L576
  invoke void @llvm.aarch64.oxcaml.raise.notrace.edge(i64 ptrtoint (ptr @"\01_camlBoyer__Unify3423" to i64), ptr blockaddress(@"\01_camlBoyer__unify1_10_30_code", %L573))
          to label %L578 unwind label %L573

L578:                                             ; preds = %L551
  unreachable

common.ret:                                       ; preds = %L581, %L553
  %.pn = phi { { i64, i64 }, { ptr addrspace(1) } } [ %33, %L553 ], [ %54, %L581 ]
  %.pn114 = phi ptr addrspace(1) [ %4, %L553 ], [ %47, %L581 ]
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn114, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L553:                                             ; preds = %L576
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %common.ret

L562:                                             ; preds = %L573
  %39 = extractvalue { ptr addrspace(1), i64, i64, i64 } %23, 2
  %40 = add i64 %39, -48
  %41 = inttoptr i64 %25 to ptr
  %42 = load i64, ptr %41, align 4
  %.not113 = icmp ugt i64 %42, %40
  br i1 %.not113, label %L580, label %L581, !prof !1

L580:                                             ; preds = %L562
  %43 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %25, i64 %40) #9 [ "deopt"(i64 1870160737, i64 1, i64 2, i64 3, i64 1, i64 85, i64 0, i64 24, i64 54, i64 0, i64 54, i64 8, i64 7958370, i64 3043941, i64 27757, i64 12, i64 7958338, i64 3043941, i64 6909557, i64 3242342, i64 3, i64 1, i64 85, i64 0, i64 24, i64 39, i64 0, i64 39, i64 8, i64 7958370, i64 3043941, i64 27757, i64 12, i64 7958338, i64 3043941, i64 6909557, i64 3242342) ]
  %44 = extractvalue { { i64, i64 }, {} } %43, 0, 0
  %45 = extractvalue { { i64, i64 }, {} } %43, 0, 1
  br label %L581

L581:                                             ; preds = %L580, %L562
  %alloc.0 = phi i64 [ %40, %L562 ], [ %45, %L580 ]
  %ds.0 = phi i64 [ %25, %L562 ], [ %44, %L580 ]
  %46 = add i64 %alloc.0, 8
  %47 = inttoptr i64 %46 to ptr addrspace(1)
  %48 = getelementptr i8, ptr addrspace(1) %47, i64 24, !is_base_value !2
  %49 = getelementptr i8, ptr addrspace(1) %48, i64 -8
  store volatile i64 2048, ptr addrspace(1) %49, align 4
  store ptr addrspace(1) %16, ptr addrspace(1) %48, align 8
  %50 = getelementptr i8, ptr addrspace(1) %48, i64 8
  store ptr addrspace(1) %2, ptr addrspace(1) %50, align 8
  %51 = getelementptr i8, ptr addrspace(1) %47, i64 -8
  store volatile i64 2048, ptr addrspace(1) %51, align 4
  store ptr addrspace(1) %48, ptr addrspace(1) %47, align 8
  %52 = getelementptr i8, ptr addrspace(1) %47, i64 8
  store ptr addrspace(1) %4, ptr addrspace(1) %52, align 8
  %53 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds.0, 0, 0
  %54 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %53, i64 %alloc.0, 0, 1
  br label %common.ret

L566:                                             ; preds = %L573
  %55 = ptrtoint ptr addrspace(1) %24 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %55)
  unreachable
}
