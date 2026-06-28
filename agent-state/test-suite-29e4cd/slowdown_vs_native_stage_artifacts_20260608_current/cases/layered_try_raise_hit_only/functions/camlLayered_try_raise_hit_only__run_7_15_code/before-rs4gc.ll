define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLayered_try_raise_hit_only__run_7_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L259, label %L260, !prof !1

L259:                                             ; preds = %L1
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #7
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L260

L260:                                             ; preds = %L259, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L259 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L259 ]
  %12 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlLayered_try_raise_hit_only__open_layers_6_14_code"(i64 %ds.0, i64 %alloc.0, i64 13, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlLayered_try_raise_hit_only__const_block139" to i64) to ptr addrspace(1))) #8 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 40, i64 0, i64 12, i64 57, i64 0, i64 57, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 30, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 7239026) ]
  %13 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %14 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %15 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %16 = icmp slt i64 %3, 3
  br i1 %16, label %common.ret, label %L214

L214:                                             ; preds = %L260
  %17 = lshr i64 %3, 1
  %18 = icmp slt i64 %2, 3
  br i1 %18, label %L246.preheader, label %L225

L225:                                             ; preds = %L249, %L214
  %.062 = phi i64 [ 1, %L214 ], [ %.064.ph, %L249 ]
  %.0 = phi i64 [ 1, %L214 ], [ %30, %L249 ]
  %alloc.1 = phi i64 [ %14, %L214 ], [ %alloc.4.ph, %L249 ]
  %ds.1 = phi i64 [ %13, %L214 ], [ %ds.4.ph, %L249 ]
  %19 = lshr i64 %2, 1
  br label %L231

L231:                                             ; preds = %L231, %L225
  %.066 = phi i64 [ %.062, %L225 ], [ %25, %L231 ]
  %.065 = phi i64 [ 1, %L225 ], [ %26, %L231 ]
  %alloc.2 = phi i64 [ %alloc.1, %L225 ], [ %22, %L231 ]
  %ds.2 = phi i64 [ %ds.1, %L225 ], [ %21, %L231 ]
  %20 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlLayered_try_raise_hit_only__find_5_13_code"(i64 %ds.2, i64 %alloc.2, ptr addrspace(1) %15) #8 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 44, i64 0, i64 20, i64 28, i64 0, i64 28, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 30, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 7239026) ]
  %21 = extractvalue { { i64, i64 }, { i64 } } %20, 0, 0
  %22 = extractvalue { { i64, i64 }, { i64 } } %20, 0, 1
  %23 = extractvalue { { i64, i64 }, { i64 } } %20, 1, 0
  %24 = add i64 %.066, -1
  %25 = add i64 %24, %23
  %26 = add i64 %.065, 1
  %27 = icmp sgt i64 %26, %19
  br i1 %27, label %L246.loopexit, label %L231

L246.loopexit:                                    ; preds = %L231
  %28 = extractvalue { { i64, i64 }, { i64 } } %20, 0, 0
  %29 = extractvalue { { i64, i64 }, { i64 } } %20, 0, 1
  br label %L246.preheader

L246.preheader:                                   ; preds = %L246.loopexit, %L214
  %.064.ph = phi i64 [ 1, %L214 ], [ %25, %L246.loopexit ]
  %.2.ph = phi i64 [ 1, %L214 ], [ %.0, %L246.loopexit ]
  %alloc.4.ph = phi i64 [ %14, %L214 ], [ %29, %L246.loopexit ]
  %ds.4.ph = phi i64 [ %13, %L214 ], [ %28, %L246.loopexit ]
  br label %L246

L246:                                             ; preds = %L246.preheader, %L249
  %.2 = phi i64 [ %30, %L249 ], [ %.2.ph, %L246.preheader ]
  %30 = add i64 %.2, 1
  %31 = icmp sgt i64 %30, %17
  br i1 %31, label %common.ret, label %L249

L249:                                             ; preds = %L246
  br i1 %18, label %L246, label %L225

common.ret:                                       ; preds = %L246, %L260
  %ds.4.pn = phi i64 [ %13, %L260 ], [ %ds.4.ph, %L246 ]
  %alloc.4.pn = phi i64 [ %14, %L260 ], [ %alloc.4.ph, %L246 ]
  %.064.pn = phi i64 [ 1, %L260 ], [ %.064.ph, %L246 ]
  %.pn72 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.4.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn72, i64 %alloc.4.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.064.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
