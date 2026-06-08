define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__find_4_13_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) nocapture %3) #0 gc "oxcaml" {
L1:
  %4 = getelementptr i8, ptr addrspace(1) %3, i64 -8
  %5 = load i64, ptr addrspace(1) %4, align 4
  %6 = lshr i64 %5, 9
  %7 = and i64 %6, 140737488355326
  %8 = add nsw i64 %7, -1
  %9 = icmp ugt i64 %7, 2
  br i1 %9, label %L134.preheader, label %L162

L162:                                             ; preds = %L1
  %.not = icmp eq i64 %7, 2
  br i1 %.not, label %L134.preheader, label %common.ret

L134.preheader:                                   ; preds = %L1, %L162
  %.069.ph = phi i64 [ 1, %L162 ], [ %8, %L1 ]
  %10 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %11 = getelementptr i8, ptr addrspace(1) %2, i64 8
  %12 = getelementptr i8, ptr addrspace(1) %3, i64 -4
  br label %L134.outer

common.ret:                                       ; preds = %L152, %L155, %L162, %L146
  %ds.0.pn = phi i64 [ %ds.287, %L146 ], [ %0, %L162 ], [ %ds.2, %L155 ], [ %ds.2, %L152 ]
  %alloc.0.pn = phi i64 [ %alloc.286, %L146 ], [ %1, %L162 ], [ %alloc.2, %L155 ], [ %alloc.2, %L152 ]
  %.pn77 = phi i64 [ %78, %L146 ], [ -1, %L162 ], [ -1, %L155 ], [ -1, %L152 ]
  %.pn78 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.0.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn78, i64 %alloc.0.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.pn77, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op

L134:                                             ; preds = %L134.outer, %L155
  %.068 = phi i64 [ %83, %L155 ], [ %.068.ph, %L134.outer ]
  %alloc.1 = phi i64 [ %alloc.2, %L155 ], [ %alloc.1.ph, %L134.outer ]
  %ds.1 = phi i64 [ %ds.2, %L155 ], [ %ds.1.ph, %L134.outer ]
  %13 = add i64 %82, %.068
  %14 = lshr i64 %13, 1
  %15 = or i64 %14, 1
  %16 = shl i64 %15, 2
  %17 = getelementptr i8, ptr addrspace(1) %12, i64 %16
  %18 = load ptr addrspace(1), ptr addrspace(1) %17, align 8
  %19 = load ptr addrspace(1), ptr addrspace(1) %18, align 8
  %20 = icmp eq ptr addrspace(1) %19, %2
  br i1 %20, label %L146, label %L165

L165:                                             ; preds = %L134
  %21 = load atomic i64, ptr addrspace(1) %10 monotonic, align 8
  %22 = lshr i64 %21, 7
  %23 = and i64 %22, 562949953421304
  %24 = add nsw i64 %23, -1
  %25 = getelementptr i8, ptr addrspace(1) %2, i64 %24
  %26 = load i8, ptr addrspace(1) %25, align 1
  %27 = zext i8 %26 to i64
  %28 = sub nsw i64 %24, %27
  %29 = getelementptr i8, ptr addrspace(1) %19, i64 -8
  %30 = load atomic i64, ptr addrspace(1) %29 monotonic, align 8
  %31 = lshr i64 %30, 7
  %32 = and i64 %31, 562949953421304
  %33 = add nsw i64 %32, -1
  %34 = getelementptr i8, ptr addrspace(1) %19, i64 %33
  %35 = load i8, ptr addrspace(1) %34, align 1
  %36 = zext i8 %35 to i64
  %37 = sub nsw i64 %33, %36
  %38 = icmp ult i64 %28, %37
  %39 = tail call i64 @llvm.umin.i64(i64 %28, i64 %37)
  %40 = icmp ugt i64 %39, 15
  br i1 %40, label %L166, label %L167

L166:                                             ; preds = %L165
  %41 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.1, i64 %alloc.1, ptr addrspace(1) %2, ptr addrspace(1) %19) #8
  %42 = extractvalue { i64, i64, ptr addrspace(1) } %41, 0
  %43 = extractvalue { i64, i64, ptr addrspace(1) } %41, 1
  %44 = extractvalue { i64, i64, ptr addrspace(1) } %41, 2
  br label %L144

L167:                                             ; preds = %L165
  %45 = icmp eq i64 %39, 0
  br i1 %45, label %L168, label %L169

L169:                                             ; preds = %L167
  %46 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %39)
  %47 = shl nuw nsw i64 %46, 3
  %48 = shl nsw i64 -1, %47
  %49 = load i64, ptr addrspace(1) %2, align 8
  %50 = tail call i64 @llvm.bswap.i64(i64 %49)
  %51 = load i64, ptr addrspace(1) %19, align 8
  %52 = tail call i64 @llvm.bswap.i64(i64 %51)
  %53 = and i64 %50, %48
  %54 = and i64 %52, %48
  %.not76 = icmp eq i64 %53, %54
  br i1 %.not76, label %L171, label %L170

L170:                                             ; preds = %L169
  %55 = icmp ult i64 %53, %54
  %56 = select i1 %55, i64 -1, i64 3
  %57 = inttoptr i64 %56 to ptr addrspace(1)
  br label %L144

L171:                                             ; preds = %L169
  %58 = icmp ugt i64 %39, 8
  br i1 %58, label %L172, label %L168

L172:                                             ; preds = %L171
  %59 = shl nuw nsw i64 %39, 3
  %60 = sub nuw nsw i64 128, %59
  %61 = shl nsw i64 -1, %60
  %62 = load i64, ptr addrspace(1) %11, align 8
  %63 = tail call i64 @llvm.bswap.i64(i64 %62)
  %64 = getelementptr i8, ptr addrspace(1) %19, i64 8
  %65 = load i64, ptr addrspace(1) %64, align 8
  %66 = tail call i64 @llvm.bswap.i64(i64 %65)
  %67 = and i64 %63, %61
  %68 = and i64 %66, %61
  %.not80 = icmp eq i64 %67, %68
  br i1 %.not80, label %L168, label %L173

L173:                                             ; preds = %L172
  %69 = icmp ult i64 %67, %68
  %70 = select i1 %69, i64 -1, i64 3
  %71 = inttoptr i64 %70 to ptr addrspace(1)
  br label %L144

L168:                                             ; preds = %L172, %L171, %L167
  %72 = icmp ugt i64 %28, %37
  %73 = select i1 %72, i64 3, i64 1
  %74 = select i1 %38, i64 -1, i64 %73
  %75 = inttoptr i64 %74 to ptr addrspace(1)
  br label %L144

L144:                                             ; preds = %L166, %L170, %L173, %L168
  %.0 = phi ptr addrspace(1) [ %44, %L166 ], [ %75, %L168 ], [ %57, %L170 ], [ %71, %L173 ]
  %alloc.2 = phi i64 [ %43, %L166 ], [ %alloc.1, %L168 ], [ %alloc.1, %L170 ], [ %alloc.1, %L173 ]
  %ds.2 = phi i64 [ %42, %L166 ], [ %ds.1, %L168 ], [ %ds.1, %L170 ], [ %ds.1, %L173 ]
  %or.cond.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L146, label %L150

L146:                                             ; preds = %L134, %L144
  %ds.287 = phi i64 [ %ds.2, %L144 ], [ %ds.1, %L134 ]
  %alloc.286 = phi i64 [ %alloc.2, %L144 ], [ %alloc.1, %L134 ]
  %76 = getelementptr i8, ptr addrspace(1) %18, i64 8
  %77 = load ptr addrspace(1), ptr addrspace(1) %76, align 8
  %78 = ptrtoint ptr addrspace(1) %77 to i64
  br label %common.ret

L150:                                             ; preds = %L144
  %79 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %79, label %L152, label %L155

L152:                                             ; preds = %L150
  %80 = add nsw i64 %15, -2
  %81 = icmp sgt i64 %.068, %80
  br i1 %81, label %common.ret, label %L134.outer

L134.outer:                                       ; preds = %L134.preheader, %L152
  %.069.ph88 = phi i64 [ %.069.ph, %L134.preheader ], [ %80, %L152 ]
  %.068.ph = phi i64 [ 1, %L134.preheader ], [ %.068, %L152 ]
  %alloc.1.ph = phi i64 [ %1, %L134.preheader ], [ %alloc.2, %L152 ]
  %ds.1.ph = phi i64 [ %0, %L134.preheader ], [ %ds.2, %L152 ]
  %82 = add nsw i64 %.069.ph88, -1
  br label %L134

L155:                                             ; preds = %L150
  %83 = add nuw i64 %15, 2
  %84 = icmp sgt i64 %83, %.069.ph88
  br i1 %84, label %common.ret, label %L134
}
