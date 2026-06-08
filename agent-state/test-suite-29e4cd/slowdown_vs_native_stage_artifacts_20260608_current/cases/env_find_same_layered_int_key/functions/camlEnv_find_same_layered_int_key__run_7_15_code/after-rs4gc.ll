define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlEnv_find_same_layered_int_key__run_7_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L271, label %L272, !prof !1

L271:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #7
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L272

L272:                                             ; preds = %L271, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L271 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L271 ]
  %statepoint_token95 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code", i32 4, i32 0, i64 %ds.0, i64 %alloc.0, i64 13, ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlEnv_find_same_layered_int_key__const_block153" to i64) to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 47, i64 0, i64 12, i64 32, i64 0, i64 32, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 33, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7239026) ]
  %12 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token95)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %gcagg93 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %gcagg94 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %13 = icmp slt i64 %3, 3
  br i1 %13, label %common.ret, label %L226

L226:                                             ; preds = %L272
  %14 = lshr i64 %3, 1
  %15 = icmp slt i64 %2, 3
  br i1 %15, label %L258.preheader, label %L237

L237:                                             ; preds = %L261, %L226
  %.097 = phi ptr addrspace(1) [ %.298, %L261 ], [ %gcagg94, %L226 ]
  %.064 = phi i64 [ 1, %L226 ], [ %.066.ph, %L261 ]
  %.0 = phi i64 [ 1, %L226 ], [ %27, %L261 ]
  %alloc.1 = phi i64 [ %gcagg93, %L226 ], [ %alloc.4.ph, %L261 ]
  %ds.1 = phi i64 [ %gcagg, %L226 ], [ %ds.4.ph, %L261 ]
  %16 = lshr i64 %2, 1
  br label %L243

L243:                                             ; preds = %L243, %L237
  %.1 = phi ptr addrspace(1) [ %.097, %L237 ], [ %gcagg94.relocated, %L243 ]
  %.068 = phi i64 [ %.064, %L237 ], [ %22, %L243 ]
  %.067 = phi i64 [ 1, %L237 ], [ %23, %L243 ]
  %alloc.2 = phi i64 [ %alloc.1, %L237 ], [ %19, %L243 ]
  %ds.2 = phi i64 [ %ds.1, %L237 ], [ %18, %L243 ]
  %statepoint_token96 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlEnv_find_same_layered_int_key__find_same_without_locks_5_13_code", i32 4, i32 0, i64 %ds.2, i64 %alloc.2, ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlEnv_find_same_layered_int_key__const_block149" to i64) to ptr addrspace(1)), ptr addrspace(1) %.1, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 52, i64 0, i64 20, i64 50, i64 0, i64 50, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 33, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 7239026), "gc-live"(ptr addrspace(1) %.1) ]
  %17 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token96)
  %gcagg94.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token96, i32 0, i32 0) ; (%.1, %.1)
  %18 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 0
  %19 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 1
  %20 = extractvalue { { i64, i64 }, { i64 } } %17, 1, 0
  %21 = add i64 %.068, -1
  %22 = add i64 %21, %20
  %23 = add i64 %.067, 1
  %24 = icmp sgt i64 %23, %16
  br i1 %24, label %L258.loopexit, label %L243

L258.loopexit:                                    ; preds = %L243
  %25 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 0
  %26 = extractvalue { { i64, i64 }, { i64 } } %17, 0, 1
  br label %L258.preheader

L258.preheader:                                   ; preds = %L258.loopexit, %L226
  %.298 = phi ptr addrspace(1) [ %gcagg94, %L226 ], [ %gcagg94.relocated, %L258.loopexit ]
  %.066.ph = phi i64 [ 1, %L226 ], [ %22, %L258.loopexit ]
  %.2.ph = phi i64 [ 1, %L226 ], [ %.0, %L258.loopexit ]
  %alloc.4.ph = phi i64 [ %gcagg93, %L226 ], [ %26, %L258.loopexit ]
  %ds.4.ph = phi i64 [ %gcagg, %L226 ], [ %25, %L258.loopexit ]
  br label %L258

L258:                                             ; preds = %L258.preheader, %L261
  %.2 = phi i64 [ %27, %L261 ], [ %.2.ph, %L258.preheader ]
  %27 = add i64 %.2, 1
  %28 = icmp sgt i64 %27, %14
  br i1 %28, label %common.ret, label %L261

L261:                                             ; preds = %L258
  br i1 %15, label %L258, label %L237

common.ret:                                       ; preds = %L258, %L272
  %ds.4.pn = phi i64 [ %gcagg, %L272 ], [ %ds.4.ph, %L258 ]
  %alloc.4.pn = phi i64 [ %gcagg93, %L272 ], [ %alloc.4.ph, %L258 ]
  %.066.pn = phi i64 [ 1, %L272 ], [ %.066.ph, %L258 ]
  %.pn74 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.4.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn74, i64 %alloc.4.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.066.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
