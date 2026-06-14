define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture readonly %3, i64 %4) #0 gc "oxcaml" {
L1:
  %5 = getelementptr i8, ptr addrspace(1) %3, i64 -8
  %6 = load i64, ptr addrspace(1) %5, align 4
  %7 = lshr i64 %6, 9
  %8 = and i64 %7, 140737488355326
  %9 = or i64 %8, 1
  %or.cond.not6468 = icmp eq i64 %9, %4
  br i1 %or.cond.not6468, label %L131, label %L136.preheader

L136.preheader:                                   ; preds = %L1
  %10 = getelementptr i8, ptr addrspace(1) %3, i64 -4
  %11 = getelementptr i8, ptr addrspace(1) %3, i64 -4
  br label %L136

L131:                                             ; preds = %L157, %L1
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64))
  unreachable

L136:                                             ; preds = %L136.preheader, %L157
  %.in = phi i64 [ %33, %L157 ], [ %6, %L136.preheader ]
  %ds.071 = phi i64 [ %31, %L157 ], [ %0, %L136.preheader ]
  %alloc.070 = phi i64 [ %30, %L157 ], [ %1, %L136.preheader ]
  %.069 = phi i64 [ %32, %L157 ], [ %4, %L136.preheader ]
  %12 = and i64 %.in, 255
  %or.cond.not = icmp eq i64 %12, 254
  br i1 %or.cond.not, label %L140, label %L147

L140:                                             ; preds = %L136
  %13 = add i64 %alloc.070, -16
  %14 = inttoptr i64 %ds.071 to ptr
  %15 = load i64, ptr %14, align 4
  %.not = icmp ugt i64 %15, %13
  br i1 %.not, label %L168, label %L169, !prof !1

L168:                                             ; preds = %L140
  %16 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %ds.071, i64 %13) #8 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 18, i64 0, i64 10, i64 30, i64 0, i64 30, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257) ]
  %17 = extractvalue { { i64, i64 }, {} } %16, 0, 0
  %18 = extractvalue { { i64, i64 }, {} } %16, 0, 1
  br label %L169

L169:                                             ; preds = %L168, %L140
  %alloc.1 = phi i64 [ %13, %L140 ], [ %18, %L168 ]
  %ds.1 = phi i64 [ %ds.071, %L140 ], [ %17, %L168 ]
  %19 = add i64 %alloc.1, 8
  %20 = inttoptr i64 %19 to ptr addrspace(1)
  %21 = getelementptr i8, ptr addrspace(1) %20, i64 -8
  store volatile i64 1277, ptr addrspace(1) %21, align 4
  %22 = shl i64 %.069, 2
  %23 = getelementptr i8, ptr addrspace(1) %11, i64 %22
  %24 = load double, ptr addrspace(1) %23, align 8
  store double %24, ptr addrspace(1) %20, align 8
  br label %L154

L147:                                             ; preds = %L136
  %25 = shl i64 %.069, 2
  %26 = getelementptr i8, ptr addrspace(1) %10, i64 %25
  %27 = load ptr addrspace(1), ptr addrspace(1) %26, align 8
  br label %L154

L154:                                             ; preds = %L147, %L169
  %.061 = phi ptr addrspace(1) [ %27, %L147 ], [ %20, %L169 ]
  %alloc.2 = phi i64 [ %alloc.070, %L147 ], [ %alloc.1, %L169 ]
  %ds.2 = phi i64 [ %ds.071, %L147 ], [ %ds.1, %L169 ]
  %28 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %ds.2, i64 %alloc.2, i64 ptrtoint (ptr @"\01_caml_equal" to i64), ptr addrspace(1) %.061, ptr addrspace(1) %2) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 18, i64 0, i64 10, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257) ]
  %29 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 1, 0
  %or.cond4.not = icmp eq ptr addrspace(1) %29, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond4.not, label %L157, label %L160

L157:                                             ; preds = %L154
  %30 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  %31 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %32 = add i64 %.069, 2
  %33 = load i64, ptr addrspace(1) %5, align 4
  %34 = lshr i64 %33, 9
  %35 = and i64 %34, 140737488355326
  %36 = or i64 %35, 1
  %or.cond.not64 = icmp eq i64 %32, %36
  br i1 %or.cond.not64, label %L131, label %L136

L160:                                             ; preds = %L154
  %37 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %38 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  %39 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %37, 0, 0
  %40 = insertvalue { { i64, i64 }, { i64 } } %39, i64 %38, 0, 1
  %41 = insertvalue { { i64, i64 }, { i64 } } %40, i64 %.069, 1, 0
  ret { { i64, i64 }, { i64 } } %41
}
