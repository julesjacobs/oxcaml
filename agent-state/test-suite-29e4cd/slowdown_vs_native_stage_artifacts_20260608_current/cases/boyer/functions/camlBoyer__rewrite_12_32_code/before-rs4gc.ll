define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_12_32_code"(i64 %0, i64 %1, ptr addrspace(1) %2) #0 gc "oxcaml" {
L1:
  %3 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %4 = load i8, ptr addrspace(1) %3, align 1
  %.not = icmp eq i8 %4, 0
  br i1 %.not, label %L625, label %L615

L615:                                             ; preds = %L1
  %5 = add i64 %0, 40
  %6 = inttoptr i64 %5 to ptr
  %7 = load i64, ptr %6, align 4
  %8 = add i64 %7, 376
  %9 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #7
  %.not40 = icmp ult i64 %9, %8
  br i1 %.not40, label %L628, label %L629, !prof !1

L628:                                             ; preds = %L615
  %10 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #8
  %11 = extractvalue { { i64, i64 }, {} } %10, 0, 0
  %12 = extractvalue { { i64, i64 }, {} } %10, 0, 1
  br label %L629

L629:                                             ; preds = %L628, %L615
  %alloc.0 = phi i64 [ %1, %L615 ], [ %12, %L628 ]
  %ds.0 = phi i64 [ %0, %L615 ], [ %11, %L628 ]
  %13 = load ptr addrspace(1), ptr addrspace(1) %2, align 8
  %14 = getelementptr i8, ptr addrspace(1) %13, i64 8
  %15 = load ptr addrspace(1), ptr addrspace(1) %14, align 8
  %16 = getelementptr i8, ptr addrspace(1) %2, i64 8
  %17 = load ptr addrspace(1), ptr addrspace(1) %16, align 8
  %18 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlStdlib__List__map_15_113_code"(i64 %ds.0, i64 %alloc.0, i64 ptrtoint (ptr @"\01_camlBoyer__rewrite_28" to i64), ptr addrspace(1) %17) #6 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 101, i64 0, i64 39, i64 60, i64 0, i64 60, i64 8, i64 7958370, i64 3043941, i64 27757, i64 13, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 101) ]
  %19 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %18, 0, 0
  %20 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %18, 0, 1
  %21 = add i64 %20, -24
  %22 = inttoptr i64 %19 to ptr
  %23 = load i64, ptr %22, align 4
  %.not41 = icmp ugt i64 %23, %21
  br i1 %.not41, label %L630, label %L631, !prof !1

L630:                                             ; preds = %L629
  %24 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %19, i64 %21) #12 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 101, i64 0, i64 26, i64 62, i64 0, i64 62, i64 8, i64 7958370, i64 3043941, i64 27757, i64 13, i64 7958338, i64 3043941, i64 7824754, i64 7629170, i64 101) ]
  %25 = extractvalue { { i64, i64 }, {} } %24, 0, 0
  %26 = extractvalue { { i64, i64 }, {} } %24, 0, 1
  br label %L631

L631:                                             ; preds = %L630, %L629
  %alloc.1 = phi i64 [ %21, %L629 ], [ %26, %L630 ]
  %ds.1 = phi i64 [ %19, %L629 ], [ %25, %L630 ]
  %27 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %18, 1, 0
  %28 = add i64 %alloc.1, 8
  %29 = inttoptr i64 %28 to ptr addrspace(1)
  %30 = getelementptr i8, ptr addrspace(1) %29, i64 -8
  store volatile i64 2049, ptr addrspace(1) %30, align 4
  store ptr addrspace(1) %13, ptr addrspace(1) %29, align 8
  %31 = getelementptr i8, ptr addrspace(1) %29, i64 8
  store ptr addrspace(1) %27, ptr addrspace(1) %31, align 8
  %32 = musttail call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlBoyer__rewrite_with_lemmas_13_33_code"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %29, ptr addrspace(1) %15) #6
  ret { { i64, i64 }, { ptr addrspace(1) } } %32

L625:                                             ; preds = %L1
  %33 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %0, 0, 0
  %34 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %33, i64 %1, 0, 1
  %35 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %34, ptr addrspace(1) %2, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %35
}
