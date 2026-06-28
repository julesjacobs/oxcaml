define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlHash_lookup_string_equal__run_6_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L279, label %L280, !prof !1

L279:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #6
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L280

L280:                                             ; preds = %L279, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L279 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L279 ]
  %statepoint_token222 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, i64, i64)) @"\01_camlStdlib__List__init_11_109_code", i32 5, i32 0, i64 %ds.0, i64 %alloc.0, i64 1, i64 63, i64 ptrtoint (ptr @"\01_camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a24$2c17$2d$2d70$5d_15" to i64), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 77, i64 0, i64 2, i64 20, i64 0, i64 20, i64 7, i64 7563628, i64 7155316, i64 108, i64 17, i64 6583379, i64 6449516, i64 5005151, i64 7631721, i64 7235886, i64 29801, i64 24, i64 0, i64 4, i64 70, i64 0, i64 70, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110) ]
  %12 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token222)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %gcagg204 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %gcagg205 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %13 = add i64 %gcagg204, -16
  %14 = inttoptr i64 %gcagg to ptr
  %15 = load i64, ptr %14, align 4
  %.not175 = icmp ugt i64 %15, %13
  br i1 %.not175, label %L281, label %L282, !prof !1

L281:                                             ; preds = %L280
  %statepoint_token223 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 131073, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg, i64 %13, i32 0, i32 0) #6 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 26, i64 0, i64 16, i64 29, i64 0, i64 29, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg205) ]
  %16 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token223)
  %gcagg205.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token223, i32 0, i32 0) ; (%gcagg205, %gcagg205)
  %17 = extractvalue { { i64, i64 }, {} } %16, 0, 0
  %18 = extractvalue { { i64, i64 }, {} } %16, 0, 1
  br label %L282

L282:                                             ; preds = %L281, %L280
  %.0227 = phi ptr addrspace(1) [ %gcagg205.relocated, %L281 ], [ %gcagg205, %L280 ]
  %alloc.1 = phi i64 [ %13, %L280 ], [ %18, %L281 ]
  %ds.1 = phi i64 [ %gcagg, %L280 ], [ %17, %L281 ]
  %19 = add i64 %alloc.1, 8
  %20 = inttoptr i64 %19 to ptr addrspace(1)
  %remat = getelementptr i8, ptr addrspace(1) %20, i64 -8
  store volatile i64 1024, ptr addrspace(1) %remat, align 4
  store ptr addrspace(1) %.0227, ptr addrspace(1) %20, align 8
  %statepoint_token224 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlStdlib__List__map_15_113_code", i32 4, i32 0, i64 %ds.1, i64 %alloc.1, i64 ptrtoint (ptr @"\01_camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a27$2c37$2d$2d40$5d_16" to i64), ptr addrspace(1) %.0227, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 27, i64 49, i64 0, i64 49, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %20) ]
  %21 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token224)
  %22 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token224, i32 0, i32 0) ; (%20, %20)
  %gcagg206 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 0, 0
  %gcagg207 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 0, 1
  %gcagg208 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 1, 0
  %23 = ptrtoint ptr addrspace(1) %gcagg208 to i64
  %24 = and i64 %23, 1
  %.not176 = icmp eq i64 %24, 0
  br i1 %.not176, label %L172, label %L158

L158:                                             ; preds = %L282
  %25 = icmp slt i64 %3, 3
  br i1 %25, label %common.ret, label %L226

L170:                                             ; preds = %L172
  %26 = inttoptr i64 %34 to ptr addrspace(1)
  %27 = load ptr addrspace(1), ptr addrspace(1) %gcagg208, align 8
  %statepoint_token225 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %gcagg206, i64 %gcagg207, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) %26, ptr addrspace(1) %27, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 213, i64 0, i64 14, i64 41, i64 0, i64 41, i64 8, i64 7500385, i64 3045729, i64 27757, i64 21, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 7286393, i64 7102310, i64 7631721, i64 27, i64 0, i64 13, i64 49, i64 0, i64 49, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %22, ptr addrspace(1) %gcagg208, ptr addrspace(1) %26, ptr addrspace(1) %27) ]
  %28 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token225)
  %29 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token225, i32 0, i32 0) ; (%22, %22)
  %gcagg208.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token225, i32 1, i32 1) ; (%gcagg208, %gcagg208)
  %gcagg209 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %gcagg210 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  %gcagg211 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 1, 0
  %remat212 = getelementptr i8, ptr addrspace(1) %gcagg208.relocated, i64 8
  %30 = load ptr addrspace(1), ptr addrspace(1) %remat212, align 8
  %31 = ptrtoint ptr addrspace(1) %30 to i64
  %32 = and i64 %31, 1
  %.not178 = icmp eq i64 %32, 0
  br i1 %.not178, label %L192.preheader, label %L190

L192.preheader:                                   ; preds = %L170
  br label %L192

L172:                                             ; preds = %L282, %L172
  %.0160 = phi ptr addrspace(1) [ %33, %L172 ], [ %gcagg208, %L282 ]
  %.1 = phi i64 [ %34, %L172 ], [ 1, %L282 ]
  %remat216 = getelementptr i8, ptr addrspace(1) %.0160, i64 8
  %33 = load ptr addrspace(1), ptr addrspace(1) %remat216, align 8
  %34 = add i64 %.1, 2
  %35 = ptrtoint ptr addrspace(1) %33 to i64
  %36 = and i64 %35, 1
  %.not177 = icmp eq i64 %36, 0
  br i1 %.not177, label %L172, label %L170

L190:                                             ; preds = %L214, %L170
  %alloc.2 = phi i64 [ %gcagg210, %L170 ], [ %alloc.4, %L214 ]
  %ds.2 = phi i64 [ %gcagg209, %L170 ], [ %ds.4, %L214 ]
  %37 = icmp slt i64 %3, 3
  br i1 %37, label %common.ret, label %L226

L192:                                             ; preds = %L192.preheader, %L214
  %.0162 = phi ptr addrspace(1) [ %45, %L214 ], [ %30, %L192.preheader ]
  %.0161 = phi i64 [ %46, %L214 ], [ 3, %L192.preheader ]
  %alloc.3 = phi i64 [ %alloc.4, %L214 ], [ %gcagg210, %L192.preheader ]
  %ds.3 = phi i64 [ %ds.4, %L214 ], [ %gcagg209, %L192.preheader ]
  %38 = load ptr addrspace(1), ptr addrspace(1) %.0162, align 8
  %remat213 = getelementptr i8, ptr addrspace(1) %gcagg211, i64 -8
  %39 = load i8, ptr addrspace(1) %remat213, align 1
  %40 = shl i64 %.0161, 2
  %or.cond.not = icmp eq i8 %39, -2
  br i1 %or.cond.not, label %L200, label %L207

L200:                                             ; preds = %L192
  %remat215 = getelementptr i8, ptr addrspace(1) %gcagg211, i64 -4
  %41 = load double, ptr addrspace(1) %38, align 8
  %remat217 = getelementptr i8, ptr addrspace(1) %remat215, i64 %40
  store double %41, ptr addrspace(1) %remat217, align 8
  br label %L214

L207:                                             ; preds = %L192
  %remat214 = getelementptr i8, ptr addrspace(1) %gcagg211, i64 -4
  %remat218 = getelementptr i8, ptr addrspace(1) %remat214, i64 %40
  %42 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %remat218, ptr addrspace(1) %38) #8
  %43 = extractvalue { i64, i64 } %42, 0
  %44 = extractvalue { i64, i64 } %42, 1
  br label %L214

L214:                                             ; preds = %L207, %L200
  %alloc.4 = phi i64 [ %44, %L207 ], [ %alloc.3, %L200 ]
  %ds.4 = phi i64 [ %43, %L207 ], [ %ds.3, %L200 ]
  %remat219 = getelementptr i8, ptr addrspace(1) %.0162, i64 8
  %45 = load ptr addrspace(1), ptr addrspace(1) %remat219, align 8
  %46 = add i64 %.0161, 2
  %47 = ptrtoint ptr addrspace(1) %45 to i64
  %48 = and i64 %47, 1
  %.not179 = icmp eq i64 %48, 0
  br i1 %.not179, label %L192, label %L190

L226:                                             ; preds = %L190, %L158
  %.0228 = phi ptr addrspace(1) [ %29, %L190 ], [ %22, %L158 ]
  %.0 = phi ptr addrspace(1) [ inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__Array__empty_array50" to i64) to ptr addrspace(1)), %L158 ], [ %gcagg211, %L190 ]
  %alloc.5 = phi i64 [ %gcagg207, %L158 ], [ %alloc.2, %L190 ]
  %ds.5 = phi i64 [ %gcagg206, %L158 ], [ %ds.2, %L190 ]
  %49 = lshr i64 %3, 1
  %50 = lshr i64 %2, 1
  %51 = icmp slt i64 %2, 3
  br i1 %51, label %L232.us, label %L232.preheader

L232.preheader:                                   ; preds = %L226
  br label %L232

L232.us:                                          ; preds = %L226, %L232.us
  %.0163.us = phi i64 [ %52, %L232.us ], [ 1, %L226 ]
  %52 = add i64 %.0163.us, 1
  %53 = icmp sgt i64 %52, %49
  br i1 %53, label %common.ret, label %L232.us

L232:                                             ; preds = %L232.preheader, %L265.loopexit
  %.0230 = phi ptr addrspace(1) [ %.0, %L232.preheader ], [ %.0.relocated, %L265.loopexit ]
  %.1229 = phi ptr addrspace(1) [ %.0228, %L232.preheader ], [ %62, %L265.loopexit ]
  %.0164 = phi i64 [ %67, %L265.loopexit ], [ 1, %L232.preheader ]
  %.0163 = phi i64 [ %72, %L265.loopexit ], [ 1, %L232.preheader ]
  %alloc.6 = phi i64 [ %71, %L265.loopexit ], [ %alloc.5, %L232.preheader ]
  %ds.6 = phi i64 [ %70, %L265.loopexit ], [ %ds.5, %L232.preheader ]
  %54 = shl i64 %.0163, 1
  %55 = or i64 %54, 1
  br label %L244

L244:                                             ; preds = %L244, %L232
  %.1231 = phi ptr addrspace(1) [ %.0230, %L232 ], [ %.0.relocated, %L244 ]
  %.2 = phi ptr addrspace(1) [ %.1229, %L232 ], [ %62, %L244 ]
  %.0167 = phi i64 [ %.0164, %L232 ], [ %67, %L244 ]
  %.0166 = phi i64 [ 1, %L232 ], [ %68, %L244 ]
  %alloc.7 = phi i64 [ %alloc.6, %L232 ], [ %64, %L244 ]
  %ds.7 = phi i64 [ %ds.6, %L232 ], [ %63, %L244 ]
  %56 = shl i64 %.0166, 1
  %57 = add i64 %55, %56
  %58 = shl i64 %57, 2
  %59 = and i64 %58, 252
  %remat220 = getelementptr i8, ptr addrspace(1) %.1231, i64 -4
  %remat221 = getelementptr i8, ptr addrspace(1) %remat220, i64 %59
  %60 = load ptr addrspace(1), ptr addrspace(1) %remat221, align 8
  %statepoint_token226 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlHash_lookup_string_equal__find_4_13_code", i32 4, i32 0, i64 %ds.7, i64 %alloc.7, ptr addrspace(1) %60, ptr addrspace(1) %.2, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 31, i64 0, i64 20, i64 74, i64 0, i64 74, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.2, ptr addrspace(1) %.1231) ]
  %61 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token226)
  %62 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token226, i32 0, i32 0) ; (%.2, %.2)
  %.0.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token226, i32 1, i32 1) ; (%.1231, %.1231)
  %63 = extractvalue { { i64, i64 }, { i64 } } %61, 0, 0
  %64 = extractvalue { { i64, i64 }, { i64 } } %61, 0, 1
  %65 = extractvalue { { i64, i64 }, { i64 } } %61, 1, 0
  %66 = add i64 %.0167, -1
  %67 = add i64 %66, %65
  %68 = add i64 %.0166, 1
  %69 = icmp sgt i64 %68, %50
  br i1 %69, label %L265.loopexit, label %L244

L265.loopexit:                                    ; preds = %L244
  %70 = extractvalue { { i64, i64 }, { i64 } } %61, 0, 0
  %71 = extractvalue { { i64, i64 }, { i64 } } %61, 0, 1
  %72 = add i64 %.0163, 1
  %73 = icmp sgt i64 %72, %49
  br i1 %73, label %common.ret.loopexit192, label %L232

common.ret.loopexit192:                           ; preds = %L265.loopexit
  %74 = extractvalue { { i64, i64 }, { i64 } } %61, 0, 0
  %75 = extractvalue { { i64, i64 }, { i64 } } %61, 0, 1
  br label %common.ret

common.ret:                                       ; preds = %L232.us, %common.ret.loopexit192, %L158, %L190
  %ds.8.pn = phi i64 [ %gcagg206, %L158 ], [ %ds.2, %L190 ], [ %74, %common.ret.loopexit192 ], [ %ds.5, %L232.us ]
  %alloc.8.pn = phi i64 [ %gcagg207, %L158 ], [ %alloc.2, %L190 ], [ %75, %common.ret.loopexit192 ], [ %alloc.5, %L232.us ]
  %.0165.pn = phi i64 [ 1, %L158 ], [ 1, %L190 ], [ %67, %common.ret.loopexit192 ], [ 1, %L232.us ]
  %.pn180 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.8.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn180, i64 %alloc.8.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0165.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
