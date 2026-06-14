define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_12_32_code"(i64 %0, i64 %1, ptr addrspace(1) %2) #0 gc "oxcaml" {
L1:
  %remat = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %3 = load i8, ptr addrspace(1) %remat, align 1
  %.not = icmp eq i8 %3, 0
  br i1 %.not, label %L625, label %L615

L615:                                             ; preds = %L1
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not40 = icmp ult i64 %8, %7
  br i1 %.not40, label %L628, label %L629, !prof !1

L628:                                             ; preds = %L615
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #7 [ "gc-live"(ptr addrspace(1) %2) ]
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%2, %2)
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %12 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L629

L629:                                             ; preds = %L628, %L615
  %.0 = phi ptr addrspace(1) [ %10, %L628 ], [ %2, %L615 ]
  %alloc.0 = phi i64 [ %1, %L615 ], [ %12, %L628 ]
  %ds.0 = phi i64 [ %0, %L615 ], [ %11, %L628 ]
  %13 = load ptr addrspace(1), ptr addrspace(1) %.0, align 8
  %remat44 = getelementptr i8, ptr addrspace(1) %13, i64 8
  %14 = load ptr addrspace(1), ptr addrspace(1) %remat44, align 8
  %remat45 = getelementptr i8, ptr addrspace(1) %.0, i64 8
  %15 = load ptr addrspace(1), ptr addrspace(1) %remat45, align 8
  %statepoint_token48 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlStdlib__List__map_15_113_code", i32 4, i32 0, i64 %ds.0, i64 %alloc.0, i64 ptrtoint (ptr @"\01_camlBoyer__rewrite_28" to i64), ptr addrspace(1) %15, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 101, i64 0, i64 39, i64 60, i64 0, i64 60, i64 8, i64 7958370, i64 3043941, i64 27757, i64 13, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 101), "gc-live"(ptr addrspace(1) %14, ptr addrspace(1) %13) ]
  %16 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token48)
  %17 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token48, i32 0, i32 0) ; (%14, %14)
  %18 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token48, i32 1, i32 1) ; (%13, %13)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 0, 0
  %gcagg42 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 0, 1
  %gcagg43 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 1, 0
  %19 = add i64 %gcagg42, -24
  %20 = inttoptr i64 %gcagg to ptr
  %21 = load i64, ptr %20, align 4
  %.not41 = icmp ugt i64 %21, %19
  br i1 %.not41, label %L630, label %L631, !prof !1

L630:                                             ; preds = %L629
  %statepoint_token49 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 196609, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg, i64 %19, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 101, i64 0, i64 26, i64 62, i64 0, i64 62, i64 8, i64 7958370, i64 3043941, i64 27757, i64 13, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 101), "gc-live"(ptr addrspace(1) %17, ptr addrspace(1) %gcagg43, ptr addrspace(1) %18) ]
  %22 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token49)
  %23 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token49, i32 0, i32 0) ; (%17, %17)
  %gcagg43.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token49, i32 1, i32 1) ; (%gcagg43, %gcagg43)
  %24 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token49, i32 2, i32 2) ; (%18, %18)
  %25 = extractvalue { { i64, i64 }, {} } %22, 0, 0
  %26 = extractvalue { { i64, i64 }, {} } %22, 0, 1
  br label %L631

L631:                                             ; preds = %L630, %L629
  %.052 = phi ptr addrspace(1) [ %gcagg43.relocated, %L630 ], [ %gcagg43, %L629 ]
  %.051 = phi ptr addrspace(1) [ %24, %L630 ], [ %18, %L629 ]
  %.050 = phi ptr addrspace(1) [ %23, %L630 ], [ %17, %L629 ]
  %alloc.1 = phi i64 [ %19, %L629 ], [ %26, %L630 ]
  %ds.1 = phi i64 [ %gcagg, %L629 ], [ %25, %L630 ]
  %27 = add i64 %alloc.1, 8
  %28 = inttoptr i64 %27 to ptr addrspace(1)
  %remat46 = getelementptr i8, ptr addrspace(1) %28, i64 -8
  store volatile i64 2049, ptr addrspace(1) %remat46, align 4
  store ptr addrspace(1) %.051, ptr addrspace(1) %28, align 8
  %remat47 = getelementptr i8, ptr addrspace(1) %28, i64 8
  store ptr addrspace(1) %.052, ptr addrspace(1) %remat47, align 8
  %29 = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_with_lemmas_13_33_code"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %28, ptr addrspace(1) %.050) #9
  ret { { i64, i64 }, { ptr addrspace(1) } } %29

L625:                                             ; preds = %L1
  %30 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %0, 0, 0
  %31 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %30, i64 %1, 0, 1
  %32 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %31, ptr addrspace(1) %2, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %32
}
