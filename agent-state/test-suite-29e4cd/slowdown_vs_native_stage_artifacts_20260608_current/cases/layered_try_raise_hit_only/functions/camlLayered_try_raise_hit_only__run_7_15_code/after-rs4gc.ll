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
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #7
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L260

L260:                                             ; preds = %L259, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L259 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L259 ]
  %statepoint_token93 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlLayered_try_raise_hit_only__open_layers_6_14_code", i32 4, i32 0, i64 %ds.0, i64 %alloc.0, i64 13, ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlLayered_try_raise_hit_only__const_block139" to i64) to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 40, i64 0, i64 12, i64 57, i64 0, i64 57, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 30, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 7239026) ]
  %12 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token93)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %gcagg91 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %gcagg92 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %13 = icmp slt i64 %3, 3
  br i1 %13, label %common.ret, label %L214

L214:                                             ; preds = %L260
  %14 = lshr i64 %3, 1
  %15 = icmp slt i64 %2, 3
  br i1 %15, label %L246.preheader, label %L225

L225:                                             ; preds = %L249, %L214
  %.095 = phi ptr addrspace(1) [ %.296, %L249 ], [ %gcagg92, %L214 ]
  %.062 = phi i64 [ 1, %L214 ], [ %.064.ph, %L249 ]
  %.0 = phi i64 [ 1, %L214 ], [ %27, %L249 ]
  %alloc.1 = phi i64 [ %gcagg91, %L214 ], [ %alloc.4.ph, %L249 ]
  %ds.1 = phi i64 [ %gcagg, %L214 ], [ %ds.4.ph, %L249 ]
  %16 = lshr i64 %2, 1
  br label %L231

L231:                                             ; preds = %L231, %L225
  %.1 = phi ptr addrspace(1) [ %.095, %L225 ], [ %gcagg92.relocated, %L231 ]
  %.066 = phi i64 [ %.062, %L225 ], [ %22, %L231 ]
  %.065 = phi i64 [ 1, %L225 ], [ %23, %L231 ]
  %alloc.2 = phi i64 [ %alloc.1, %L225 ], [ %19, %L231 ]
  %ds.2 = phi i64 [ %ds.1, %L225 ], [ %18, %L231 ]
  %statepoint_token94 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1))) @"\01_camlLayered_try_raise_hit_only__find_5_13_code", i32 3, i32 0, i64 %ds.2, i64 %alloc.2, ptr addrspace(1) %.1, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 44, i64 0, i64 20, i64 28, i64 0, i64 28, i64 29, i64 7954796, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 27757, i64 30, i64 7954764, i64 6648421, i64 7626596, i64 6257010, i64 6906226, i64 6251891, i64 7629160, i64 7237471, i64 3045740, i64 7239026), "gc-live"(ptr addrspace(1) %.1) ]
  %17 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token94)
  %gcagg92.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token94, i32 0, i32 0) ; (%.1, %.1)
  %18 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 0
  %19 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 1
  %20 = extractvalue { { i64, i64 }, { i64 } } %17, 1, 0
  %21 = add i64 %.066, -1
  %22 = add i64 %21, %20
  %23 = add i64 %.065, 1
  %24 = icmp sgt i64 %23, %16
  br i1 %24, label %L246.loopexit, label %L231

L246.loopexit:                                    ; preds = %L231
  %25 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 0
  %26 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 1
  br label %L246.preheader

L246.preheader:                                   ; preds = %L246.loopexit, %L214
  %.296 = phi ptr addrspace(1) [ %gcagg92, %L214 ], [ %gcagg92.relocated, %L246.loopexit ]
  %.064.ph = phi i64 [ 1, %L214 ], [ %22, %L246.loopexit ]
  %.2.ph = phi i64 [ 1, %L214 ], [ %.0, %L246.loopexit ]
  %alloc.4.ph = phi i64 [ %gcagg91, %L214 ], [ %26, %L246.loopexit ]
  %ds.4.ph = phi i64 [ %gcagg, %L214 ], [ %25, %L246.loopexit ]
  br label %L246

L246:                                             ; preds = %L246.preheader, %L249
  %.2 = phi i64 [ %27, %L249 ], [ %.2.ph, %L246.preheader ]
  %27 = add i64 %.2, 1
  %28 = icmp sgt i64 %27, %14
  br i1 %28, label %common.ret, label %L249

L249:                                             ; preds = %L246
  br i1 %15, label %L246, label %L225

common.ret:                                       ; preds = %L246, %L260
  %ds.4.pn = phi i64 [ %gcagg, %L260 ], [ %ds.4.ph, %L246 ]
  %alloc.4.pn = phi i64 [ %gcagg91, %L260 ], [ %alloc.4.ph, %L246 ]
  %.064.pn = phi i64 [ 1, %L260 ], [ %.064.ph, %L246 ]
  %.pn72 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.4.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn72, i64 %alloc.4.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.064.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
