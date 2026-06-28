define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code"(i64 %0, i64 %1, i64 %2, ptr addrspace(1) %3) #1 gc "oxcaml" {
L1:
  %or.cond.not = icmp eq i64 %2, 1
  br i1 %or.cond.not, label %common.ret, label %L193

common.ret:                                       ; preds = %L1, %L221
  %.pn76 = phi i64 [ %ds.4, %L221 ], [ %0, %L1 ]
  %.pn74 = phi i64 [ %alloc.4, %L221 ], [ %1, %L1 ]
  %.pn72 = phi ptr addrspace(1) [ %40, %L221 ], [ %3, %L1 ]
  %.pn73 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } undef, i64 %.pn76, 0, 0
  %.pn = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn73, i64 %.pn74, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn72, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L193:                                             ; preds = %L1
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L213, label %L214, !prof !1

L213:                                             ; preds = %L193
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #7 [ "gc-live"(ptr addrspace(1) %3) ]
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%3, %3)
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %12 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L214

L214:                                             ; preds = %L213, %L193
  %.093 = phi ptr addrspace(1) [ %10, %L213 ], [ %3, %L193 ]
  %alloc.0 = phi i64 [ %1, %L193 ], [ %12, %L213 ]
  %ds.0 = phi i64 [ %0, %L193 ], [ %11, %L213 ]
  %13 = and i64 %2, 3
  %14 = add i64 %2, -2
  %or.cond3.not = icmp eq i64 %13, 1
  br i1 %or.cond3.not, label %L196, label %L201

L196:                                             ; preds = %L214
  %statepoint_token88 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code", i32 4, i32 0, i64 %ds.0, i64 %alloc.0, i64 %14, ptr addrspace(1) %.093, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 40, i64 0, i64 18, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %15 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token88)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %15, 0, 0
  %gcagg80 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %15, 0, 1
  %gcagg81 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %15, 1, 0
  %16 = add i64 %gcagg80, -16
  %17 = inttoptr i64 %gcagg to ptr
  %18 = load i64, ptr %17, align 4
  %.not77 = icmp ugt i64 %18, %16
  br i1 %.not77, label %L216, label %L217, !prof !1

L216:                                             ; preds = %L196
  %statepoint_token89 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 131073, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg, i64 %16, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 40, i64 0, i64 13, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554), "gc-live"(ptr addrspace(1) %gcagg81) ]
  %19 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token89)
  %gcagg81.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token89, i32 0, i32 0) ; (%gcagg81, %gcagg81)
  %20 = extractvalue { { i64, i64 }, {} } %19, 0, 0
  %21 = extractvalue { { i64, i64 }, {} } %19, 0, 1
  br label %L217

L217:                                             ; preds = %L216, %L196
  %.094 = phi ptr addrspace(1) [ %gcagg81.relocated, %L216 ], [ %gcagg81, %L196 ]
  %alloc.1 = phi i64 [ %16, %L196 ], [ %21, %L216 ]
  %ds.1 = phi i64 [ %gcagg, %L196 ], [ %20, %L216 ]
  %22 = add i64 %alloc.1, 8
  %23 = inttoptr i64 %22 to ptr addrspace(1)
  %remat = getelementptr i8, ptr addrspace(1) %23, i64 -8
  store volatile i64 1024, ptr addrspace(1) %remat, align 4
  store ptr addrspace(1) %.094, ptr addrspace(1) %23, align 8
  br label %L207

L201:                                             ; preds = %L214
  %statepoint_token90 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlEnv_find_same_layered_int_key__open_layers_6_14_code", i32 4, i32 0, i64 %ds.0, i64 %alloc.0, i64 %14, ptr addrspace(1) %.093, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 41, i64 0, i64 18, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554) ]
  %24 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token90)
  %gcagg82 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 0
  %gcagg83 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 0, 1
  %gcagg84 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %24, 1, 0
  %25 = add i64 %gcagg83, -16
  %26 = inttoptr i64 %gcagg82 to ptr
  %27 = load i64, ptr %26, align 4
  %.not79 = icmp ugt i64 %27, %25
  br i1 %.not79, label %L218, label %L219, !prof !1

L218:                                             ; preds = %L201
  %statepoint_token91 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 131073, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg82, i64 %25, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 41, i64 0, i64 13, i64 44, i64 0, i64 44, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554), "gc-live"(ptr addrspace(1) %gcagg84) ]
  %28 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token91)
  %gcagg84.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token91, i32 0, i32 0) ; (%gcagg84, %gcagg84)
  %29 = extractvalue { { i64, i64 }, {} } %28, 0, 0
  %30 = extractvalue { { i64, i64 }, {} } %28, 0, 1
  br label %L219

L219:                                             ; preds = %L218, %L201
  %.095 = phi ptr addrspace(1) [ %gcagg84.relocated, %L218 ], [ %gcagg84, %L201 ]
  %alloc.2 = phi i64 [ %25, %L201 ], [ %30, %L218 ]
  %ds.2 = phi i64 [ %gcagg82, %L201 ], [ %29, %L218 ]
  %31 = add i64 %alloc.2, 8
  %32 = inttoptr i64 %31 to ptr addrspace(1)
  %remat85 = getelementptr i8, ptr addrspace(1) %32, i64 -8
  store volatile i64 1025, ptr addrspace(1) %remat85, align 4
  store ptr addrspace(1) %.095, ptr addrspace(1) %32, align 8
  br label %L207

L207:                                             ; preds = %L219, %L217
  %.0 = phi ptr addrspace(1) [ %32, %L219 ], [ %23, %L217 ]
  %alloc.3 = phi i64 [ %alloc.2, %L219 ], [ %alloc.1, %L217 ]
  %ds.3 = phi i64 [ %ds.2, %L219 ], [ %ds.1, %L217 ]
  %33 = add i64 %alloc.3, -24
  %34 = inttoptr i64 %ds.3 to ptr
  %35 = load i64, ptr %34, align 4
  %.not78 = icmp ugt i64 %35, %33
  br i1 %.not78, label %L220, label %L221, !prof !1

L220:                                             ; preds = %L207
  %statepoint_token92 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 196609, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %ds.3, i64 %33, i32 0, i32 0) #7 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 1, i64 37, i64 4, i64 4, i64 46, i64 106, i64 152, i64 32, i64 7761509, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 27757, i64 41, i64 7761477, i64 6907487, i64 6251630, i64 7168371, i64 7102309, i64 6650209, i64 6579570, i64 7235935, i64 7036788, i64 3045733, i64 6647919, i64 7102318, i64 6650209, i64 29554), "gc-live"(ptr addrspace(1) %.0) ]
  %36 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token92)
  %.0.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token92, i32 0, i32 0) ; (%.0, %.0)
  %37 = extractvalue { { i64, i64 }, {} } %36, 0, 0
  %38 = extractvalue { { i64, i64 }, {} } %36, 0, 1
  br label %L221

L221:                                             ; preds = %L220, %L207
  %.096 = phi ptr addrspace(1) [ %.0.relocated, %L220 ], [ %.0, %L207 ]
  %alloc.4 = phi i64 [ %33, %L207 ], [ %38, %L220 ]
  %ds.4 = phi i64 [ %ds.3, %L207 ], [ %37, %L220 ]
  %39 = add i64 %alloc.4, 8
  %40 = inttoptr i64 %39 to ptr addrspace(1)
  %remat86 = getelementptr i8, ptr addrspace(1) %40, i64 -8
  store volatile i64 2048, ptr addrspace(1) %remat86, align 4
  store volatile i64 1, ptr addrspace(1) %40, align 4
  %remat87 = getelementptr i8, ptr addrspace(1) %40, i64 8
  store ptr addrspace(1) %.096, ptr addrspace(1) %remat87, align 8
  br label %common.ret
}
