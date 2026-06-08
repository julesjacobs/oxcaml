define oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_map_equal_content__find_7_33_code"(i64 %0, i64 %1, ptr addrspace(1) %2, ptr addrspace(1) %3) #1 gc "oxcaml" {
L1:
  %4 = ptrtoint ptr addrspace(1) %3 to i64
  %5 = and i64 %4, 1
  %.not = icmp eq i64 %5, 0
  br i1 %.not, label %L181.preheader, label %L179

L181.preheader:                                   ; preds = %L1
  %6 = getelementptr i8, ptr addrspace(1) %3, i64 8
  %7 = load ptr addrspace(1), ptr addrspace(1) %6, align 8
  %8 = icmp eq ptr addrspace(1) %7, %2
  br i1 %8, label %L186, label %L205.lr.ph

L205.lr.ph:                                       ; preds = %L181.preheader
  %9 = getelementptr i8, ptr addrspace(1) %2, i64 -8
  %10 = getelementptr i8, ptr addrspace(1) %2, i64 8
  br label %L205

L179:                                             ; preds = %L195, %L192, %L1
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_caml_exn_Not_found" to i64))
  unreachable

L205:                                             ; preds = %L205.lr.ph, %L181.backedge
  %11 = phi ptr addrspace(1) [ %7, %L205.lr.ph ], [ %77, %L181.backedge ]
  %ds.051 = phi i64 [ %0, %L205.lr.ph ], [ %ds.1, %L181.backedge ]
  %alloc.050 = phi i64 [ %1, %L205.lr.ph ], [ %alloc.1, %L181.backedge ]
  %.03649 = phi ptr addrspace(1) [ %3, %L205.lr.ph ], [ %.036.be, %L181.backedge ]
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
  br i1 %31, label %L206, label %L207

L206:                                             ; preds = %L205
  %32 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.051, i64 %alloc.050, ptr addrspace(1) %2, ptr addrspace(1) %11) #6
  %33 = extractvalue { i64, i64, ptr addrspace(1) } %32, 0
  %34 = extractvalue { i64, i64, ptr addrspace(1) } %32, 1
  %35 = extractvalue { i64, i64, ptr addrspace(1) } %32, 2
  br label %L183

L207:                                             ; preds = %L205
  %36 = icmp eq i64 %30, 0
  br i1 %36, label %L208, label %L209

L209:                                             ; preds = %L207
  %37 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %30)
  %38 = shl nuw nsw i64 %37, 3
  %39 = shl nsw i64 -1, %38
  %40 = load i64, ptr addrspace(1) %2, align 8
  %41 = tail call i64 @llvm.bswap.i64(i64 %40)
  %42 = load i64, ptr addrspace(1) %11, align 8
  %43 = tail call i64 @llvm.bswap.i64(i64 %42)
  %44 = and i64 %41, %39
  %45 = and i64 %43, %39
  %.not38 = icmp eq i64 %44, %45
  br i1 %.not38, label %L211, label %L210

L210:                                             ; preds = %L209
  %46 = icmp ult i64 %44, %45
  %47 = select i1 %46, i64 -1, i64 3
  %48 = inttoptr i64 %47 to ptr addrspace(1)
  br label %L183

L211:                                             ; preds = %L209
  %49 = icmp ugt i64 %30, 8
  br i1 %49, label %L212, label %L208

L212:                                             ; preds = %L211
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
  %.not41 = icmp eq i64 %58, %59
  br i1 %.not41, label %L208, label %L213

L213:                                             ; preds = %L212
  %60 = icmp ult i64 %58, %59
  %61 = select i1 %60, i64 -1, i64 3
  %62 = inttoptr i64 %61 to ptr addrspace(1)
  br label %L183

L208:                                             ; preds = %L212, %L211, %L207
  %63 = icmp ugt i64 %19, %28
  %64 = select i1 %63, i64 3, i64 1
  %65 = select i1 %29, i64 -1, i64 %64
  %66 = inttoptr i64 %65 to ptr addrspace(1)
  br label %L183

L183:                                             ; preds = %L206, %L210, %L213, %L208
  %.0 = phi ptr addrspace(1) [ %35, %L206 ], [ %66, %L208 ], [ %48, %L210 ], [ %62, %L213 ]
  %alloc.1 = phi i64 [ %34, %L206 ], [ %alloc.050, %L208 ], [ %alloc.050, %L210 ], [ %alloc.050, %L213 ]
  %ds.1 = phi i64 [ %33, %L206 ], [ %ds.051, %L208 ], [ %ds.051, %L210 ], [ %ds.051, %L213 ]
  %or.cond.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond.not, label %L186, label %L190

L186:                                             ; preds = %L183, %L181.backedge, %L181.preheader
  %.036.lcssa = phi ptr addrspace(1) [ %3, %L181.preheader ], [ %.036.be, %L181.backedge ], [ %.03649, %L183 ]
  %ds.147 = phi i64 [ %0, %L181.preheader ], [ %ds.1, %L181.backedge ], [ %ds.1, %L183 ]
  %alloc.146 = phi i64 [ %1, %L181.preheader ], [ %alloc.1, %L181.backedge ], [ %alloc.1, %L183 ]
  %67 = getelementptr i8, ptr addrspace(1) %.036.lcssa, i64 16
  %68 = load ptr addrspace(1), ptr addrspace(1) %67, align 8
  %69 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } poison, i64 %ds.147, 0, 0
  %70 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %69, i64 %alloc.146, 0, 1
  %71 = insertvalue { { i64, i64 }, { ptr addrspace(1) } } %70, ptr addrspace(1) %68, 1, 0
  ret { { i64, i64 }, { ptr addrspace(1) } } %71

L190:                                             ; preds = %L183
  %72 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %72, label %L192, label %L195

L192:                                             ; preds = %L190
  %73 = load ptr addrspace(1), ptr addrspace(1) %.03649, align 8
  %74 = ptrtoint ptr addrspace(1) %73 to i64
  %75 = and i64 %74, 1
  %.not40 = icmp eq i64 %75, 0
  br i1 %.not40, label %L181.backedge, label %L179

L181.backedge:                                    ; preds = %L192, %L195
  %.036.be = phi ptr addrspace(1) [ %73, %L192 ], [ %80, %L195 ]
  %76 = getelementptr i8, ptr addrspace(1) %.036.be, i64 8
  %77 = load ptr addrspace(1), ptr addrspace(1) %76, align 8
  %78 = icmp eq ptr addrspace(1) %77, %2
  br i1 %78, label %L186, label %L205

L195:                                             ; preds = %L190
  %79 = getelementptr i8, ptr addrspace(1) %.03649, i64 24
  %80 = load ptr addrspace(1), ptr addrspace(1) %79, align 8
  %81 = ptrtoint ptr addrspace(1) %80 to i64
  %82 = and i64 %81, 1
  %.not39 = icmp eq i64 %82, 0
  br i1 %.not39, label %L181.backedge, label %L179
}
