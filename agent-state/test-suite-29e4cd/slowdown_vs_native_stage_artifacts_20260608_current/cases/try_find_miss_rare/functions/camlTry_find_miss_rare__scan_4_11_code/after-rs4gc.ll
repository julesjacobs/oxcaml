define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture %3, i64 %4) #0 gc "oxcaml" {
L1:
  %remat = getelementptr i8, ptr addrspace(1) %3, i64 -8
  %5 = load i64, ptr addrspace(1) %remat, align 4
  %6 = lshr i64 %5, 9
  %7 = and i64 %6, 140737488355326
  %8 = or i64 %7, 1
  %or.cond.not6468 = icmp eq i64 %8, %4
  br i1 %or.cond.not6468, label %L131, label %L136.preheader

L136.preheader:                                   ; preds = %L1
  br label %L136

L131:                                             ; preds = %L157, %L1
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64))
  unreachable

L136:                                             ; preds = %L136.preheader, %L157
  %.091 = phi ptr addrspace(1) [ %2, %L136.preheader ], [ %26, %L157 ]
  %.0 = phi ptr addrspace(1) [ %3, %L136.preheader ], [ %25, %L157 ]
  %.in = phi i64 [ %28, %L157 ], [ %5, %L136.preheader ]
  %ds.071 = phi i64 [ %gcagg, %L157 ], [ %0, %L136.preheader ]
  %alloc.070 = phi i64 [ %gcagg82, %L157 ], [ %1, %L136.preheader ]
  %.069 = phi i64 [ %27, %L157 ], [ %4, %L136.preheader ]
  %9 = and i64 %.in, 255
  %or.cond.not = icmp eq i64 %9, 254
  br i1 %or.cond.not, label %L140, label %L147

L140:                                             ; preds = %L136
  %10 = add i64 %alloc.070, -16
  %11 = inttoptr i64 %ds.071 to ptr
  %12 = load i64, ptr %11, align 4
  %.not = icmp ugt i64 %12, %10
  br i1 %.not, label %L168, label %L169, !prof !1

L168:                                             ; preds = %L140
  %statepoint_token = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 131073, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %ds.071, i64 %10, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 18, i64 0, i64 10, i64 30, i64 0, i64 30, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257), "gc-live"(ptr addrspace(1) %.0, ptr addrspace(1) %.091) ]
  %13 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %14 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%.0, %.0)
  %15 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 1, i32 1) ; (%.091, %.091)
  %16 = extractvalue { { i64, i64 }, {} } %13, 0, 0
  %17 = extractvalue { { i64, i64 }, {} } %13, 0, 1
  br label %L169

L169:                                             ; preds = %L168, %L140
  %.192 = phi ptr addrspace(1) [ %15, %L168 ], [ %.091, %L140 ]
  %.1 = phi ptr addrspace(1) [ %14, %L168 ], [ %.0, %L140 ]
  %alloc.1 = phi i64 [ %10, %L140 ], [ %17, %L168 ]
  %ds.1 = phi i64 [ %ds.071, %L140 ], [ %16, %L168 ]
  %18 = add i64 %alloc.1, 8
  %19 = inttoptr i64 %18 to ptr addrspace(1)
  %remat87 = getelementptr i8, ptr addrspace(1) %19, i64 -8
  store volatile i64 1277, ptr addrspace(1) %remat87, align 4
  %20 = shl i64 %.069, 2
  %remat86 = getelementptr i8, ptr addrspace(1) %.1, i64 -4
  %remat88 = getelementptr i8, ptr addrspace(1) %remat86, i64 %20
  %21 = load double, ptr addrspace(1) %remat88, align 8
  store double %21, ptr addrspace(1) %19, align 8
  br label %L154

L147:                                             ; preds = %L136
  %22 = shl i64 %.069, 2
  %remat85 = getelementptr i8, ptr addrspace(1) %.0, i64 -4
  %remat89 = getelementptr i8, ptr addrspace(1) %remat85, i64 %22
  %23 = load ptr addrspace(1), ptr addrspace(1) %remat89, align 8
  br label %L154

L154:                                             ; preds = %L147, %L169
  %.293 = phi ptr addrspace(1) [ %.192, %L169 ], [ %.091, %L147 ]
  %.2 = phi ptr addrspace(1) [ %.1, %L169 ], [ %.0, %L147 ]
  %.061 = phi ptr addrspace(1) [ %23, %L147 ], [ %19, %L169 ]
  %alloc.2 = phi i64 [ %alloc.070, %L147 ], [ %alloc.1, %L169 ]
  %ds.2 = phi i64 [ %ds.071, %L147 ], [ %ds.1, %L169 ]
  %statepoint_token90 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %ds.2, i64 %alloc.2, i64 ptrtoint (ptr @"\01_caml_equal" to i64), ptr addrspace(1) %.061, ptr addrspace(1) %.293, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 1, i64 18, i64 0, i64 10, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 23, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 6517550, i64 28257), "gc-live"(ptr addrspace(1) %.2, ptr addrspace(1) %.293, ptr addrspace(1) %.061) ]
  %24 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token90)
  %25 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token90, i32 0, i32 0) ; (%.2, %.2)
  %26 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token90, i32 1, i32 1) ; (%.293, %.293)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 0
  %gcagg82 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 1
  %gcagg83 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 1, 0
  %or.cond4.not = icmp eq ptr addrspace(1) %gcagg83, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond4.not, label %L157, label %L160

L157:                                             ; preds = %L154
  %27 = add i64 %.069, 2
  %remat84 = getelementptr i8, ptr addrspace(1) %25, i64 -8
  %28 = load i64, ptr addrspace(1) %remat84, align 4
  %29 = lshr i64 %28, 9
  %30 = and i64 %29, 140737488355326
  %31 = or i64 %30, 1
  %or.cond.not64 = icmp eq i64 %27, %31
  br i1 %or.cond.not64, label %L131, label %L136

L160:                                             ; preds = %L154
  %32 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %gcagg, 0, 0
  %33 = insertvalue { { i64, i64 }, { i64 } } %32, i64 %gcagg82, 0, 1
  %34 = insertvalue { { i64, i64 }, { i64 } } %33, i64 %.069, 1, 0
  ret { { i64, i64 }, { i64 } } %34
}
