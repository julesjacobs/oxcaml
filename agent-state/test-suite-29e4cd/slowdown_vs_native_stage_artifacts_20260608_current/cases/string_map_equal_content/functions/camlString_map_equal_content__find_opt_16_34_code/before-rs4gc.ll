define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_map_equal_content__find_opt_16_34_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) #1 gc "oxcaml" {
L1:
  %4 = ptrtoint ptr addrspace(1) %3 to i64
  %5 = and i64 %4, 1
  %.not = icmp eq i64 %5, 0
  br i1 %.not, label %L226.preheader, label %common.ret

L226.preheader:                                   ; preds = %L1
  %6 = getelementptr i8, ptr addrspace(1) %3, i64 8
  %7 = load ptr addrspace(1), ptr addrspace(1) %6, align 8
  %8 = icmp eq ptr addrspace(1) %7, %2
  br i1 %8, label %L231, label %L251.lr.ph

L251.lr.ph:                                       ; preds = %L226.preheader
  %9 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %10 = getelementptr i8, ptr addrspace(1) %2, i64 8
  br label %L251

common.ret:                                       ; preds = %L238, %L241, %L1, %L262
  %ds.0.pn = phi i64 [ %ds.3, %L262 ], [ %0, %L1 ], [ %ds.2, %L241 ], [ %ds.2, %L238 ]
  %alloc.0.pn = phi i64 [ %alloc.3, %L262 ], [ %1, %L1 ], [ %alloc.2, %L241 ], [ %alloc.2, %L238 ]
  %.pn50 = phi ptr addrspace(1) [ %74, %L262 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L1 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L241 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L238 ]
  %.pn51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } undef, i64 %ds.0.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn51, i64 %alloc.0.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn50, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L251:                                             ; preds = %L251.lr.ph, %L226.backedge
  %11 = phi ptr addrspace(1) [ %7, %L251.lr.ph ], [ %83, %L226.backedge ]
  %ds.165 = phi i64 [ %0, %L251.lr.ph ], [ %ds.2, %L226.backedge ]
  %alloc.164 = phi i64 [ %1, %L251.lr.ph ], [ %alloc.2, %L226.backedge ]
  %.04563 = phi ptr addrspace(1) [ %3, %L251.lr.ph ], [ %.045.be, %L226.backedge ]
  %12 = load atomic i64, ptr addrspace(1) %9 monotonic, align 8
  %13 = lshr i64 %12, 7
  %14 = and i64 %13, 562949953421304
  %15 = add nsw i64 %14, -1
  %16 = getelementptr i8, ptr addrspace(1) %2, i64 %15
  %17 = load i8, ptr addrspace(1) %16, align 1
  %18 = zext i8 %17 to i64
  %19 = sub nsw i64 %15, %18
  %20 = getelementptr i8, ptr addrspace(1) %11, i64 -8
  %21 = load atomic i64, ptr addrspace(1) %20 monotonic, align 8
  %22 = lshr i64 %21, 7
  %23 = and i64 %22, 562949953421304
  %24 = add nsw i64 %23, -1
  %25 = getelementptr i8, ptr addrspace(1) %11, i64 %24
  %26 = load i8, ptr addrspace(1) %25, align 1
  %27 = zext i8 %26 to i64
  %28 = sub nsw i64 %24, %27
  %29 = icmp ult i64 %19, %28
  %30 = tail call i64 @llvm.umin.i64(i64 %19, i64 %28)
  %31 = icmp ugt i64 %30, 15
  br i1 %31, label %L252, label %L253

L252:                                             ; preds = %L251
  %32 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.165, i64 %alloc.164, ptr addrspace(1) %2, ptr addrspace(1) %11) #6
  %33 = extractvalue { i64, i64, ptr addrspace(1) } %32, 0
  %34 = extractvalue { i64, i64, ptr addrspace(1) } %32, 1
  %35 = extractvalue { i64, i64, ptr addrspace(1) } %32, 2
  br label %L228

L253:                                             ; preds = %L251
  %36 = icmp eq i64 %30, 0
  br i1 %36, label %L254, label %L255

L255:                                             ; preds = %L253
  %37 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %30)
  %38 = shl nuw nsw i64 %37, 3
  %39 = shl nsw i64 -1, %38
  %40 = load i64, ptr addrspace(1) %2, align 8
  %41 = tail call i64 @llvm.bswap.i64(i64 %40)
  %42 = load i64, ptr addrspace(1) %11, align 8
  %43 = tail call i64 @llvm.bswap.i64(i64 %42)
  %44 = and i64 %41, %39
  %45 = and i64 %43, %39
  %.not48 = icmp eq i64 %44, %45
  br i1 %.not48, label %L257, label %L256

L256:                                             ; preds = %L255
  %46 = icmp ult i64 %44, %45
  %47 = select i1 %46, i64 -1, i64 3
  %48 = inttoptr i64 %47 to ptr addrspace(1)
  br label %L228

L257:                                             ; preds = %L255
  %49 = icmp ugt i64 %30, 8
  br i1 %49, label %L258, label %L254

L258:                                             ; preds = %L257
  %50 = shl nuw nsw i64 %30, 3
  %51 = sub nuw nsw i64 128, %50
  %52 = shl nsw i64 -1, %51
  %53 = load i64, ptr addrspace(1) %10, align 8
  %54 = tail call i64 @llvm.bswap.i64(i64 %53)
  %55 = getelementptr i8, ptr addrspace(1) %11, i64 8
  %56 = load i64, ptr addrspace(1) %55, align 8
  %57 = tail call i64 @llvm.bswap.i64(i64 %56)
  %58 = and i64 %54, %52
  %59 = and i64 %57, %52
  %.not55 = icmp eq i64 %58, %59
  br i1 %.not55, label %L254, label %L259

L259:                                             ; preds = %L258
  %60 = icmp ult i64 %58, %59
  %61 = select i1 %60, i64 -1, i64 3
  %62 = inttoptr i64 %61 to ptr addrspace(1)
  br label %L228

L254:                                             ; preds = %L258, %L257, %L253
  %63 = icmp ugt i64 %19, %28
  %64 = select i1 %63, i64 3, i64 1
  %65 = select i1 %29, i64 -1, i64 %64
  %66 = inttoptr i64 %65 to ptr addrspace(1)
  br label %L228

L228:                                             ; preds = %L252, %L256, %L259, %L254
  %.0 = phi ptr addrspace(1) [ %35, %L252 ], [ %66, %L254 ], [ %48, %L256 ], [ %62, %L259 ]
  %alloc.2 = phi i64 [ %34, %L252 ], [ %alloc.164, %L254 ], [ %alloc.164, %L256 ], [ %alloc.164, %L259 ]
  %ds.2 = phi i64 [ %33, %L252 ], [ %ds.165, %L254 ], [ %ds.165, %L256 ], [ %ds.165, %L259 ]
  %or.cond.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L231, label %L236

L231:                                             ; preds = %L228, %L226.backedge, %L226.preheader
  %.045.lcssa = phi ptr addrspace(1) [ %3, %L226.preheader ], [ %.045.be, %L226.backedge ], [ %.04563, %L228 ]
  %ds.261 = phi i64 [ %0, %L226.preheader ], [ %ds.2, %L226.backedge ], [ %ds.2, %L228 ]
  %alloc.260 = phi i64 [ %1, %L226.preheader ], [ %alloc.2, %L226.backedge ], [ %alloc.2, %L228 ]
  %67 = add i64 %alloc.260, -16
  %68 = inttoptr i64 %ds.261 to ptr
  %69 = load i64, ptr %68, align 4
  %.not49 = icmp ugt i64 %69, %67
  br i1 %.not49, label %L261, label %L262, !prof !1

L261:                                             ; preds = %L231
  %70 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %ds.261, i64 %67) #10 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 229, i64 0, i64 24, i64 30, i64 0, i64 30, i64 6, i64 7364973, i64 7105838, i64 25, i64 6583379, i64 6449516, i64 5070687, i64 3043425, i64 7037261, i64 6696549, i64 6581865, i64 7368543, i64 116) ]
  %71 = extractvalue { { i64, i64 }, {} } %70, 0, 0
  %72 = extractvalue { { i64, i64 }, {} } %70, 0, 1
  br label %L262

L262:                                             ; preds = %L261, %L231
  %alloc.3 = phi i64 [ %67, %L231 ], [ %72, %L261 ]
  %ds.3 = phi i64 [ %ds.261, %L231 ], [ %71, %L261 ]
  %73 = add i64 %alloc.3, 8
  %74 = inttoptr i64 %73 to ptr addrspace(1)
  %75 = getelementptr i8, ptr addrspace(1) %74, i64 -8
  store volatile i64 1024, ptr addrspace(1) %75, align 4
  %76 = getelementptr i8, ptr addrspace(1) %.045.lcssa, i64 16
  %77 = load ptr addrspace(1), ptr addrspace(1) %76, align 8
  store ptr addrspace(1) %77, ptr addrspace(1) %74, align 8
  br label %common.ret

L236:                                             ; preds = %L228
  %78 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %78, label %L238, label %L241

L238:                                             ; preds = %L236
  %79 = load ptr addrspace(1), ptr addrspace(1) %.04563, align 8
  %80 = ptrtoint ptr addrspace(1) %79 to i64
  %81 = and i64 %80, 1
  %.not54 = icmp eq i64 %81, 0
  br i1 %.not54, label %L226.backedge, label %common.ret

L226.backedge:                                    ; preds = %L238, %L241
  %.045.be = phi ptr addrspace(1) [ %79, %L238 ], [ %86, %L241 ]
  %82 = getelementptr i8, ptr addrspace(1) %.045.be, i64 8
  %83 = load ptr addrspace(1), ptr addrspace(1) %82, align 8
  %84 = icmp eq ptr addrspace(1) %83, %2
  br i1 %84, label %L231, label %L251

L241:                                             ; preds = %L236
  %85 = getelementptr i8, ptr addrspace(1) %.04563, i64 24
  %86 = load ptr addrspace(1), ptr addrspace(1) %85, align 8
  %87 = ptrtoint ptr addrspace(1) %86 to i64
  %88 = and i64 %87, 1
  %.not53 = icmp eq i64 %88, 0
  br i1 %.not53, label %L226.backedge, label %common.ret
}
