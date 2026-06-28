define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlHash_lookup_string_equal__run_6_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #4
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L279, label %L280, !prof !1

L279:                                             ; preds = %L1
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #5
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L280

L280:                                             ; preds = %L279, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L279 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L279 ]
  %12 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlStdlib__List__init_11_109_code"(i64 %ds.0, i64 %alloc.0, i64 1, i64 63, i64 ptrtoint (ptr @"\01_camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a24$2c17$2d$2d70$5d_15" to i64)) #6 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 77, i64 0, i64 2, i64 20, i64 0, i64 20, i64 7, i64 7563628, i64 7155316, i64 108, i64 17, i64 6583379, i64 6449516, i64 5005151, i64 7631721, i64 7235886, i64 29801, i64 24, i64 0, i64 4, i64 70, i64 0, i64 70, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110) ]
  %13 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %14 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %15 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %16 = add i64 %14, -16
  %17 = inttoptr i64 %13 to ptr
  %18 = load i64, ptr %17, align 4
  %.not175 = icmp ugt i64 %18, %16
  br i1 %.not175, label %L281, label %L282, !prof !1

L281:                                             ; preds = %L280
  %19 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %13, i64 %16) #8 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 26, i64 0, i64 16, i64 29, i64 0, i64 29, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110) ]
  %20 = extractvalue { { i64, i64 }, {} } %19, 0, 0
  %21 = extractvalue { { i64, i64 }, {} } %19, 0, 1
  br label %L282

L282:                                             ; preds = %L281, %L280
  %alloc.1 = phi i64 [ %16, %L280 ], [ %21, %L281 ]
  %ds.1 = phi i64 [ %13, %L280 ], [ %20, %L281 ]
  %22 = add i64 %alloc.1, 8
  %23 = inttoptr i64 %22 to ptr addrspace(1)
  %24 = getelementptr i8, ptr addrspace(1) %23, i64 -8
  store volatile i64 1024, ptr addrspace(1) %24, align 4
  store ptr addrspace(1) %15, ptr addrspace(1) %23, align 8
  %25 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlStdlib__List__map_15_113_code"(i64 %ds.1, i64 %alloc.1, i64 ptrtoint (ptr @"\01_camlHash_lookup_string_equal__fn$5bhash_lookup_string_equal.ml$3a27$2c37$2d$2d40$5d_16" to i64), ptr addrspace(1) %15) #6 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 27, i64 49, i64 0, i64 49, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110) ]
  %26 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %25, 0, 0
  %27 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %25, 0, 1
  %28 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %25, 1, 0
  %29 = ptrtoint ptr addrspace(1) %28 to i64
  %30 = and i64 %29, 1
  %.not176 = icmp eq i64 %30, 0
  br i1 %.not176, label %L172, label %L158

L158:                                             ; preds = %L282
  %31 = icmp slt i64 %3, 3
  br i1 %31, label %common.ret, label %L226

L170:                                             ; preds = %L172
  %32 = inttoptr i64 %47 to ptr addrspace(1)
  %33 = load ptr addrspace(1), ptr addrspace(1) %28, align 8
  %34 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %26, i64 %27, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) %32, ptr addrspace(1) %33) #6 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 213, i64 0, i64 14, i64 41, i64 0, i64 41, i64 8, i64 7500385, i64 3045729, i64 27757, i64 21, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 7286393, i64 7102310, i64 7631721, i64 27, i64 0, i64 13, i64 49, i64 0, i64 49, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110) ]
  %35 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %34, 0, 0
  %36 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %34, 0, 1
  %37 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %34, 1, 0
  %38 = getelementptr i8, ptr addrspace(1) %28, i64 8
  %39 = load ptr addrspace(1), ptr addrspace(1) %38, align 8
  %40 = ptrtoint ptr addrspace(1) %39 to i64
  %41 = and i64 %40, 1
  %.not178 = icmp eq i64 %41, 0
  br i1 %.not178, label %L192.preheader, label %L190

L192.preheader:                                   ; preds = %L170
  %42 = getelementptr i8, ptr addrspace(1) %37, i64 -8
  %43 = getelementptr i8, ptr addrspace(1) %37, i64 -4
  %44 = getelementptr i8, ptr addrspace(1) %37, i64 -4
  br label %L192

L172:                                             ; preds = %L282, %L172
  %.0160 = phi ptr addrspace(1) [ %46, %L172 ], [ %28, %L282 ]
  %.1 = phi i64 [ %47, %L172 ], [ 1, %L282 ]
  %45 = getelementptr i8, ptr addrspace(1) %.0160, i64 8
  %46 = load ptr addrspace(1), ptr addrspace(1) %45, align 8
  %47 = add i64 %.1, 2
  %48 = ptrtoint ptr addrspace(1) %46 to i64
  %49 = and i64 %48, 1
  %.not177 = icmp eq i64 %49, 0
  br i1 %.not177, label %L172, label %L170

L190:                                             ; preds = %L214, %L170
  %alloc.2 = phi i64 [ %36, %L170 ], [ %alloc.4, %L214 ]
  %ds.2 = phi i64 [ %35, %L170 ], [ %ds.4, %L214 ]
  %50 = icmp slt i64 %3, 3
  br i1 %50, label %common.ret, label %L226

L192:                                             ; preds = %L192.preheader, %L214
  %.0162 = phi ptr addrspace(1) [ %61, %L214 ], [ %39, %L192.preheader ]
  %.0161 = phi i64 [ %62, %L214 ], [ 3, %L192.preheader ]
  %alloc.3 = phi i64 [ %alloc.4, %L214 ], [ %36, %L192.preheader ]
  %ds.3 = phi i64 [ %ds.4, %L214 ], [ %35, %L192.preheader ]
  %51 = load ptr addrspace(1), ptr addrspace(1) %.0162, align 8
  %52 = load i8, ptr addrspace(1) %42, align 1
  %or.cond.not = icmp eq i8 %52, -2
  %53 = shl i64 %.0161, 2
  br i1 %or.cond.not, label %L200, label %L207

L200:                                             ; preds = %L192
  %54 = getelementptr i8, ptr addrspace(1) %44, i64 %53
  %55 = load double, ptr addrspace(1) %51, align 8
  store double %55, ptr addrspace(1) %54, align 8
  br label %L214

L207:                                             ; preds = %L192
  %56 = getelementptr i8, ptr addrspace(1) %43, i64 %53
  %57 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %56, ptr addrspace(1) %51) #7
  %58 = extractvalue { i64, i64 } %57, 0
  %59 = extractvalue { i64, i64 } %57, 1
  br label %L214

L214:                                             ; preds = %L207, %L200
  %alloc.4 = phi i64 [ %59, %L207 ], [ %alloc.3, %L200 ]
  %ds.4 = phi i64 [ %58, %L207 ], [ %ds.3, %L200 ]
  %60 = getelementptr i8, ptr addrspace(1) %.0162, i64 8
  %61 = load ptr addrspace(1), ptr addrspace(1) %60, align 8
  %62 = add i64 %.0161, 2
  %63 = ptrtoint ptr addrspace(1) %61 to i64
  %64 = and i64 %63, 1
  %.not179 = icmp eq i64 %64, 0
  br i1 %.not179, label %L192, label %L190

L226:                                             ; preds = %L190, %L158
  %.0 = phi ptr addrspace(1) [ inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__Array__empty_array50" to i64) to ptr addrspace(1)), %L158 ], [ %37, %L190 ]
  %alloc.5 = phi i64 [ %27, %L158 ], [ %alloc.2, %L190 ]
  %ds.5 = phi i64 [ %26, %L158 ], [ %ds.2, %L190 ]
  %65 = lshr i64 %3, 1
  %66 = icmp slt i64 %2, 3
  %67 = lshr i64 %2, 1
  br i1 %66, label %L232.us, label %L232.preheader

L232.preheader:                                   ; preds = %L226
  %68 = getelementptr i8, ptr addrspace(1) %.0, i64 -4
  br label %L232

L232.us:                                          ; preds = %L226, %L232.us
  %.0163.us = phi i64 [ %69, %L232.us ], [ 1, %L226 ]
  %69 = add i64 %.0163.us, 1
  %70 = icmp sgt i64 %69, %65
  br i1 %70, label %common.ret, label %L232.us

L232:                                             ; preds = %L232.preheader, %L265.loopexit
  %.0164 = phi i64 [ %84, %L265.loopexit ], [ 1, %L232.preheader ]
  %.0163 = phi i64 [ %89, %L265.loopexit ], [ 1, %L232.preheader ]
  %alloc.6 = phi i64 [ %88, %L265.loopexit ], [ %alloc.5, %L232.preheader ]
  %ds.6 = phi i64 [ %87, %L265.loopexit ], [ %ds.5, %L232.preheader ]
  %71 = shl i64 %.0163, 1
  %72 = or i64 %71, 1
  br label %L244

L244:                                             ; preds = %L244, %L232
  %.0167 = phi i64 [ %.0164, %L232 ], [ %84, %L244 ]
  %.0166 = phi i64 [ 1, %L232 ], [ %85, %L244 ]
  %alloc.7 = phi i64 [ %alloc.6, %L232 ], [ %81, %L244 ]
  %ds.7 = phi i64 [ %ds.6, %L232 ], [ %80, %L244 ]
  %73 = shl i64 %.0166, 1
  %74 = add i64 %72, %73
  %75 = shl i64 %74, 2
  %76 = and i64 %75, 252
  %77 = getelementptr i8, ptr addrspace(1) %68, i64 %76
  %78 = load ptr addrspace(1), ptr addrspace(1) %77, align 8
  %79 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlHash_lookup_string_equal__find_4_13_code"(i64 %ds.7, i64 %alloc.7, ptr addrspace(1) %78, ptr addrspace(1) %23) #6 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 31, i64 0, i64 20, i64 74, i64 0, i64 74, i64 27, i64 7561576, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7105838, i64 28, i64 7561544, i64 7102312, i64 7040879, i64 6254709, i64 7500915, i64 6778473, i64 7431519, i64 7102837, i64 7696942, i64 110) ]
  %80 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 0
  %81 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 1
  %82 = extractvalue { { i64, i64 }, { i64 } } %79, 1, 0
  %83 = add i64 %.0167, -1
  %84 = add i64 %83, %82
  %85 = add i64 %.0166, 1
  %86 = icmp sgt i64 %85, %67
  br i1 %86, label %L265.loopexit, label %L244

L265.loopexit:                                    ; preds = %L244
  %87 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 0
  %88 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 1
  %89 = add i64 %.0163, 1
  %90 = icmp sgt i64 %89, %65
  br i1 %90, label %common.ret.loopexit192, label %L232

common.ret.loopexit192:                           ; preds = %L265.loopexit
  %91 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 0
  %92 = extractvalue { { i64, i64 }, { i64 } } %79, 0, 1
  br label %common.ret

common.ret:                                       ; preds = %L232.us, %common.ret.loopexit192, %L158, %L190
  %ds.8.pn = phi i64 [ %26, %L158 ], [ %ds.2, %L190 ], [ %91, %common.ret.loopexit192 ], [ %ds.5, %L232.us ]
  %alloc.8.pn = phi i64 [ %27, %L158 ], [ %alloc.2, %L190 ], [ %92, %common.ret.loopexit192 ], [ %alloc.5, %L232.us ]
  %.0165.pn = phi i64 [ 1, %L158 ], [ 1, %L190 ], [ %84, %common.ret.loopexit192 ], [ 1, %L232.us ]
  %.pn180 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.8.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn180, i64 %alloc.8.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0165.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
