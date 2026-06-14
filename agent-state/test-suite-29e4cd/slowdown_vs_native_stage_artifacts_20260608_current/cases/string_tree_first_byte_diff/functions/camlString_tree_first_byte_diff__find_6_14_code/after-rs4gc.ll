define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__find_6_14_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) #0 gc "oxcaml" {
L1:
  %4 = ptrtoint ptr addrspace(1) %3 to i64
  %5 = and i64 %4, 1
  %.not = icmp eq i64 %5, 0
  br i1 %.not, label %L213.preheader, label %common.ret

L213.preheader:                                   ; preds = %L1
  %6 = getelementptr i8, ptr addrspace(1) %3, i64 8
  %7 = load ptr addrspace(1), ptr addrspace(1) %6, align 8
  %8 = icmp eq ptr addrspace(1) %7, %2
  br i1 %8, label %L218, label %L237.lr.ph

L237.lr.ph:                                       ; preds = %L213.preheader
  %9 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %10 = getelementptr i8, ptr addrspace(1) %2, i64 8
  br label %L237

common.ret:                                       ; preds = %L224, %L227, %L1, %L218
  %ds.0.pn = phi i64 [ %ds.251, %L218 ], [ %0, %L1 ], [ %ds.2, %L227 ], [ %ds.2, %L224 ]
  %alloc.0.pn = phi i64 [ %alloc.250, %L218 ], [ %1, %L1 ], [ %alloc.2, %L227 ], [ %alloc.2, %L224 ]
  %.pn40 = phi i64 [ %69, %L218 ], [ -1, %L1 ], [ -1, %L227 ], [ -1, %L224 ]
  %.pn41 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.0.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn41, i64 %alloc.0.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.pn40, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op

L237:                                             ; preds = %L237.lr.ph, %L213.backedge
  %11 = phi ptr addrspace(1) [ %7, %L237.lr.ph ], [ %75, %L213.backedge ]
  %ds.155 = phi i64 [ %0, %L237.lr.ph ], [ %ds.2, %L213.backedge ]
  %alloc.154 = phi i64 [ %1, %L237.lr.ph ], [ %alloc.2, %L213.backedge ]
  %.03753 = phi ptr addrspace(1) [ %3, %L237.lr.ph ], [ %.037.be, %L213.backedge ]
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
  br i1 %31, label %L238, label %L239

L238:                                             ; preds = %L237
  %32 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.155, i64 %alloc.154, ptr addrspace(1) %2, ptr addrspace(1) %11) #8
  %33 = extractvalue { i64, i64, ptr addrspace(1) } %32, 0
  %34 = extractvalue { i64, i64, ptr addrspace(1) } %32, 1
  %35 = extractvalue { i64, i64, ptr addrspace(1) } %32, 2
  br label %L215

L239:                                             ; preds = %L237
  %36 = icmp eq i64 %30, 0
  br i1 %36, label %L240, label %L241

L241:                                             ; preds = %L239
  %37 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %30)
  %38 = shl nuw nsw i64 %37, 3
  %39 = shl nsw i64 -1, %38
  %40 = load i64, ptr addrspace(1) %2, align 8
  %41 = tail call i64 @llvm.bswap.i64(i64 %40)
  %42 = load i64, ptr addrspace(1) %11, align 8
  %43 = tail call i64 @llvm.bswap.i64(i64 %42)
  %44 = and i64 %41, %39
  %45 = and i64 %43, %39
  %.not39 = icmp eq i64 %44, %45
  br i1 %.not39, label %L243, label %L242

L242:                                             ; preds = %L241
  %46 = icmp ult i64 %44, %45
  %47 = select i1 %46, i64 -1, i64 3
  %48 = inttoptr i64 %47 to ptr addrspace(1)
  br label %L215

L243:                                             ; preds = %L241
  %49 = icmp ugt i64 %30, 8
  br i1 %49, label %L244, label %L240

L244:                                             ; preds = %L243
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
  %.not45 = icmp eq i64 %58, %59
  br i1 %.not45, label %L240, label %L245

L245:                                             ; preds = %L244
  %60 = icmp ult i64 %58, %59
  %61 = select i1 %60, i64 -1, i64 3
  %62 = inttoptr i64 %61 to ptr addrspace(1)
  br label %L215

L240:                                             ; preds = %L244, %L243, %L239
  %63 = icmp ugt i64 %19, %28
  %64 = select i1 %63, i64 3, i64 1
  %65 = select i1 %29, i64 -1, i64 %64
  %66 = inttoptr i64 %65 to ptr addrspace(1)
  br label %L215

L215:                                             ; preds = %L238, %L242, %L245, %L240
  %.0 = phi ptr addrspace(1) [ %35, %L238 ], [ %66, %L240 ], [ %48, %L242 ], [ %62, %L245 ]
  %alloc.2 = phi i64 [ %34, %L238 ], [ %alloc.154, %L240 ], [ %alloc.154, %L242 ], [ %alloc.154, %L245 ]
  %ds.2 = phi i64 [ %33, %L238 ], [ %ds.155, %L240 ], [ %ds.155, %L242 ], [ %ds.155, %L245 ]
  %or.cond.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L218, label %L222

L218:                                             ; preds = %L215, %L213.backedge, %L213.preheader
  %.037.lcssa = phi ptr addrspace(1) [ %3, %L213.preheader ], [ %.037.be, %L213.backedge ], [ %.03753, %L215 ]
  %ds.251 = phi i64 [ %0, %L213.preheader ], [ %ds.2, %L213.backedge ], [ %ds.2, %L215 ]
  %alloc.250 = phi i64 [ %1, %L213.preheader ], [ %alloc.2, %L213.backedge ], [ %alloc.2, %L215 ]
  %67 = getelementptr i8, ptr addrspace(1) %.037.lcssa, i64 16
  %68 = load ptr addrspace(1), ptr addrspace(1) %67, align 8
  %69 = ptrtoint ptr addrspace(1) %68 to i64
  br label %common.ret

L222:                                             ; preds = %L215
  %70 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %70, label %L224, label %L227

L224:                                             ; preds = %L222
  %71 = load ptr addrspace(1), ptr addrspace(1) %.03753, align 8
  %72 = ptrtoint ptr addrspace(1) %71 to i64
  %73 = and i64 %72, 1
  %.not44 = icmp eq i64 %73, 0
  br i1 %.not44, label %L213.backedge, label %common.ret

L213.backedge:                                    ; preds = %L224, %L227
  %.037.be = phi ptr addrspace(1) [ %71, %L224 ], [ %78, %L227 ]
  %74 = getelementptr i8, ptr addrspace(1) %.037.be, i64 8
  %75 = load ptr addrspace(1), ptr addrspace(1) %74, align 8
  %76 = icmp eq ptr addrspace(1) %75, %2
  br i1 %76, label %L218, label %L237

L227:                                             ; preds = %L222
  %77 = getelementptr i8, ptr addrspace(1) %.03753, i64 24
  %78 = load ptr addrspace(1), ptr addrspace(1) %77, align 8
  %79 = ptrtoint ptr addrspace(1) %78 to i64
  %80 = and i64 %79, 1
  %.not43 = icmp eq i64 %80, 0
  br i1 %.not43, label %L213.backedge, label %common.ret
}
