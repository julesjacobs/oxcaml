define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_map_equal_content__find_opt_16_34_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) #1 gc "oxcaml" {
L1:
  %4 = ptrtoint ptr addrspace(1) %3 to i64
  %5 = and i64 %4, 1
  %.not = icmp eq i64 %5, 0
  br i1 %.not, label %L226.preheader, label %common.ret

L226.preheader:                                   ; preds = %L1
  %remat = getelementptr i8, ptr addrspace(1) %3, i64 8
  %6 = load ptr addrspace(1), ptr addrspace(1) %remat, align 8
  %7 = icmp eq ptr addrspace(1) %6, %2
  br i1 %7, label %L231, label %L251.lr.ph

L251.lr.ph:                                       ; preds = %L226.preheader
  br label %L251

common.ret:                                       ; preds = %L238, %L241, %L1, %L262
  %ds.0.pn = phi i64 [ %ds.3, %L262 ], [ %0, %L1 ], [ %ds.2, %L241 ], [ %ds.2, %L238 ]
  %alloc.0.pn = phi i64 [ %alloc.3, %L262 ], [ %1, %L1 ], [ %alloc.2, %L241 ], [ %alloc.2, %L238 ]
  %.pn50 = phi ptr addrspace(1) [ %64, %L262 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L1 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L241 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L238 ]
  %.pn51 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } undef, i64 %ds.0.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn51, i64 %alloc.0.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn, ptr addrspace(1) %.pn50, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %common.ret.op

L251:                                             ; preds = %L251.lr.ph, %L226.backedge
  %8 = phi ptr addrspace(1) [ %6, %L251.lr.ph ], [ %70, %L226.backedge ]
  %ds.165 = phi i64 [ %0, %L251.lr.ph ], [ %ds.2, %L226.backedge ]
  %alloc.164 = phi i64 [ %1, %L251.lr.ph ], [ %alloc.2, %L226.backedge ]
  %.04563 = phi ptr addrspace(1) [ %3, %L251.lr.ph ], [ %.045.be, %L226.backedge ]
  %remat83 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %9 = load atomic i64, ptr addrspace(1) %remat83 monotonic, align 8
  %10 = lshr i64 %9, 7
  %11 = and i64 %10, 562949953421304
  %12 = add nsw i64 %11, -1
  %remat85 = getelementptr i8, ptr addrspace(1) %2, i64 %12
  %13 = load i8, ptr addrspace(1) %remat85, align 1
  %14 = zext i8 %13 to i64
  %15 = sub nsw i64 %12, %14
  %remat86 = getelementptr i8, ptr addrspace(1) %8, i64 -8
  %16 = load atomic i64, ptr addrspace(1) %remat86 monotonic, align 8
  %17 = lshr i64 %16, 7
  %18 = and i64 %17, 562949953421304
  %19 = add nsw i64 %18, -1
  %remat87 = getelementptr i8, ptr addrspace(1) %8, i64 %19
  %20 = load i8, ptr addrspace(1) %remat87, align 1
  %21 = zext i8 %20 to i64
  %22 = sub nsw i64 %19, %21
  %23 = icmp ult i64 %15, %22
  %24 = tail call i64 @llvm.umin.i64(i64 %15, i64 %22)
  %25 = icmp ugt i64 %24, 15
  br i1 %25, label %L252, label %L253

L252:                                             ; preds = %L251
  %26 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.165, i64 %alloc.164, ptr addrspace(1) %2, ptr addrspace(1) %8) #6
  %gcagg = extractvalue { i64, i64, ptr addrspace(1) } %26, 0
  %gcagg81 = extractvalue { i64, i64, ptr addrspace(1) } %26, 1
  %gcagg82 = extractvalue { i64, i64, ptr addrspace(1) } %26, 2
  br label %L228

L253:                                             ; preds = %L251
  %27 = icmp eq i64 %24, 0
  br i1 %27, label %L254, label %L255

L255:                                             ; preds = %L253
  %28 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %24)
  %29 = shl nuw nsw i64 %28, 3
  %30 = shl nsw i64 -1, %29
  %31 = load i64, ptr addrspace(1) %2, align 8
  %32 = tail call i64 @llvm.bswap.i64(i64 %31)
  %33 = load i64, ptr addrspace(1) %8, align 8
  %34 = tail call i64 @llvm.bswap.i64(i64 %33)
  %35 = and i64 %32, %30
  %36 = and i64 %34, %30
  %.not48 = icmp eq i64 %35, %36
  br i1 %.not48, label %L257, label %L256

L256:                                             ; preds = %L255
  %37 = icmp ult i64 %35, %36
  %38 = select i1 %37, i64 -1, i64 3
  %39 = inttoptr i64 %38 to ptr addrspace(1)
  br label %L228

L257:                                             ; preds = %L255
  %40 = icmp ugt i64 %24, 8
  br i1 %40, label %L258, label %L254

L258:                                             ; preds = %L257
  %41 = shl nuw nsw i64 %24, 3
  %42 = sub nuw nsw i64 128, %41
  %43 = shl nsw i64 -1, %42
  %remat84 = getelementptr i8, ptr addrspace(1) %2, i64 8
  %44 = load i64, ptr addrspace(1) %remat84, align 8
  %45 = tail call i64 @llvm.bswap.i64(i64 %44)
  %remat88 = getelementptr i8, ptr addrspace(1) %8, i64 8
  %46 = load i64, ptr addrspace(1) %remat88, align 8
  %47 = tail call i64 @llvm.bswap.i64(i64 %46)
  %48 = and i64 %45, %43
  %49 = and i64 %47, %43
  %.not55 = icmp eq i64 %48, %49
  br i1 %.not55, label %L254, label %L259

L259:                                             ; preds = %L258
  %50 = icmp ult i64 %48, %49
  %51 = select i1 %50, i64 -1, i64 3
  %52 = inttoptr i64 %51 to ptr addrspace(1)
  br label %L228

L254:                                             ; preds = %L258, %L257, %L253
  %53 = icmp ugt i64 %15, %22
  %54 = select i1 %53, i64 3, i64 1
  %55 = select i1 %23, i64 -1, i64 %54
  %56 = inttoptr i64 %55 to ptr addrspace(1)
  br label %L228

L228:                                             ; preds = %L252, %L256, %L259, %L254
  %.0 = phi ptr addrspace(1) [ %gcagg82, %L252 ], [ %56, %L254 ], [ %39, %L256 ], [ %52, %L259 ]
  %alloc.2 = phi i64 [ %gcagg81, %L252 ], [ %alloc.164, %L254 ], [ %alloc.164, %L256 ], [ %alloc.164, %L259 ]
  %ds.2 = phi i64 [ %gcagg, %L252 ], [ %ds.165, %L254 ], [ %ds.165, %L256 ], [ %ds.165, %L259 ]
  %or.cond.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L231, label %L236

L231:                                             ; preds = %L228, %L226.backedge, %L226.preheader
  %.045.lcssa = phi ptr addrspace(1) [ %3, %L226.preheader ], [ %.045.be, %L226.backedge ], [ %.04563, %L228 ]
  %ds.261 = phi i64 [ %0, %L226.preheader ], [ %ds.2, %L226.backedge ], [ %ds.2, %L228 ]
  %alloc.260 = phi i64 [ %1, %L226.preheader ], [ %alloc.2, %L226.backedge ], [ %alloc.2, %L228 ]
  %57 = add i64 %alloc.260, -16
  %58 = inttoptr i64 %ds.261 to ptr
  %59 = load i64, ptr %58, align 4
  %.not49 = icmp ugt i64 %59, %57
  br i1 %.not49, label %L261, label %L262, !prof !1

L261:                                             ; preds = %L231
  %statepoint_token = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 131073, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %ds.261, i64 %57, i32 0, i32 0) #5 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 1, i64 229, i64 0, i64 24, i64 30, i64 0, i64 30, i64 6, i64 7364973, i64 7105838, i64 25, i64 6583379, i64 6449516, i64 5070687, i64 3043425, i64 7037261, i64 6696549, i64 6581865, i64 7368543, i64 116), "gc-live"(ptr addrspace(1) %.045.lcssa) ]
  %60 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %.045.lcssa.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token, i32 0, i32 0) ; (%.045.lcssa, %.045.lcssa)
  %61 = extractvalue { { i64, i64 }, {} } %60, 0, 0
  %62 = extractvalue { { i64, i64 }, {} } %60, 0, 1
  br label %L262

L262:                                             ; preds = %L261, %L231
  %.093 = phi ptr addrspace(1) [ %.045.lcssa.relocated, %L261 ], [ %.045.lcssa, %L231 ]
  %alloc.3 = phi i64 [ %57, %L231 ], [ %62, %L261 ]
  %ds.3 = phi i64 [ %ds.261, %L231 ], [ %61, %L261 ]
  %63 = add i64 %alloc.3, 8
  %64 = inttoptr i64 %63 to ptr addrspace(1)
  %remat89 = getelementptr i8, ptr addrspace(1) %64, i64 -8
  store volatile i64 1024, ptr addrspace(1) %remat89, align 4
  %remat90 = getelementptr i8, ptr addrspace(1) %.093, i64 16
  %65 = load ptr addrspace(1), ptr addrspace(1) %remat90, align 8
  store ptr addrspace(1) %65, ptr addrspace(1) %64, align 8
  br label %common.ret

L236:                                             ; preds = %L228
  %66 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %66, label %L238, label %L241

L238:                                             ; preds = %L236
  %67 = load ptr addrspace(1), ptr addrspace(1) %.04563, align 8
  %68 = ptrtoint ptr addrspace(1) %67 to i64
  %69 = and i64 %68, 1
  %.not54 = icmp eq i64 %69, 0
  br i1 %.not54, label %L226.backedge, label %common.ret

L226.backedge:                                    ; preds = %L238, %L241
  %.045.be = phi ptr addrspace(1) [ %67, %L238 ], [ %72, %L241 ]
  %remat91 = getelementptr i8, ptr addrspace(1) %.045.be, i64 8
  %70 = load ptr addrspace(1), ptr addrspace(1) %remat91, align 8
  %71 = icmp eq ptr addrspace(1) %70, %2
  br i1 %71, label %L231, label %L251

L241:                                             ; preds = %L236
  %remat92 = getelementptr i8, ptr addrspace(1) %.04563, i64 24
  %72 = load ptr addrspace(1), ptr addrspace(1) %remat92, align 8
  %73 = ptrtoint ptr addrspace(1) %72 to i64
  %74 = and i64 %73, 1
  %.not53 = icmp eq i64 %74, 0
  br i1 %.not53, label %L226.backedge, label %common.ret
}
