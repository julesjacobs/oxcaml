define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__run_6_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 27, i64 0, i64 50, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %5 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 0, 0
  %6 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 0, 1
  %7 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 1, 0
  %8 = getelementptr i8, ptr addrspace(1) %7, i64 -8
  %9 = load i64, ptr addrspace(1) %8, align 4
  %10 = lshr i64 %9, 7
  %11 = and i64 %10, 562949953421304
  %12 = add nsw i64 %11, -1
  %13 = getelementptr i8, ptr addrspace(1) %7, i64 %12
  %14 = load i8, ptr addrspace(1) %13, align 1
  %15 = zext i8 %14 to i64
  %16 = sub nsw i64 %12, %15
  %17 = shl nsw i64 %16, 1
  %18 = or i64 %17, 1
  %19 = add nsw i64 %17, 35
  %20 = inttoptr i64 %19 to ptr addrspace(1)
  %21 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %5, i64 %6, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull %20) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 27, i64 0, i64 28, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %22 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 0, 0
  %23 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 0, 1
  %24 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 1, 0
  %25 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %22, i64 %23, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlArray_binary_search_string__immstring129" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %24, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1))) #8
  %26 = extractvalue { i64, i64, ptr addrspace(1) } %25, 0
  %27 = extractvalue { i64, i64, ptr addrspace(1) } %25, 1
  %28 = inttoptr i64 %18 to ptr addrspace(1)
  %29 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %26, i64 %27, ptr addrspace(1) %7, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %24, ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1)), ptr addrspace(1) nonnull %28) #8
  %30 = extractvalue { i64, i64, ptr addrspace(1) } %29, 0
  %31 = extractvalue { i64, i64, ptr addrspace(1) } %29, 1
  %32 = add i64 %31, -24
  %33 = inttoptr i64 %30 to ptr
  %34 = load i64, ptr %33, align 4
  %.not = icmp ugt i64 %34, %32
  br i1 %.not, label %L345, label %L346, !prof !1

L345:                                             ; preds = %L1
  %35 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %30, i64 %32) #9 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 3, i64 27, i64 0, i64 28, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %36 = extractvalue { { i64, i64 }, {} } %35, 0, 0
  %37 = extractvalue { { i64, i64 }, {} } %35, 0, 1
  br label %L346

L346:                                             ; preds = %L345, %L1
  %alloc.0 = phi i64 [ %32, %L1 ], [ %37, %L345 ]
  %ds.0 = phi i64 [ %30, %L1 ], [ %36, %L345 ]
  %38 = add i64 %alloc.0, 8
  %39 = inttoptr i64 %38 to ptr addrspace(1)
  %40 = getelementptr i8, ptr addrspace(1) %39, i64 -8
  store volatile i64 2048, ptr addrspace(1) %40, align 4
  store ptr addrspace(1) %24, ptr addrspace(1) %39, align 8
  %41 = getelementptr i8, ptr addrspace(1) %39, i64 8
  store volatile i64 1, ptr addrspace(1) %41, align 4
  %42 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %ds.0, i64 %alloc.0, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) nonnull inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %39) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %43 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %42, 0, 0
  %44 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %42, 0, 1
  %45 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %42, 1, 0
  %46 = add i64 %43, 40
  %47 = inttoptr i64 %46 to ptr
  %48 = load i64, ptr %47, align 4
  %49 = add i64 %48, 376
  %50 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not296 = icmp ult i64 %50, %49
  br i1 %.not296, label %L347, label %L348, !prof !1

L347:                                             ; preds = %L346
  %51 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %43, i64 %44, i64 34) #6
  %52 = extractvalue { { i64, i64 }, {} } %51, 0, 0
  %53 = extractvalue { { i64, i64 }, {} } %51, 0, 1
  br label %L348

L348:                                             ; preds = %L347, %L346
  %alloc.1 = phi i64 [ %44, %L346 ], [ %53, %L347 ]
  %ds.1 = phi i64 [ %43, %L346 ], [ %52, %L347 ]
  %54 = getelementptr i8, ptr addrspace(1) %45, i64 -8
  %55 = getelementptr i8, ptr addrspace(1) %45, i64 -4
  br label %L204

L204:                                             ; preds = %L351, %L348
  %.0 = phi i64 [ 1, %L348 ], [ %106, %L351 ]
  %alloc.2 = phi i64 [ %alloc.1, %L348 ], [ %105, %L351 ]
  %ds.2 = phi i64 [ %ds.1, %L348 ], [ %104, %L351 ]
  %56 = shl nuw i64 %.0, 1
  %57 = or i64 %56, 1
  %58 = inttoptr i64 %57 to ptr addrspace(1)
  %59 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %ds.2, i64 %alloc.2, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull %58) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 27, i64 0, i64 50, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %60 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 0, 0
  %61 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 0, 1
  %62 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 1, 0
  %63 = getelementptr i8, ptr addrspace(1) %62, i64 -8
  %64 = load i64, ptr addrspace(1) %63, align 4
  %65 = lshr i64 %64, 7
  %66 = and i64 %65, 562949953421304
  %67 = add nsw i64 %66, -1
  %68 = getelementptr i8, ptr addrspace(1) %62, i64 %67
  %69 = load i8, ptr addrspace(1) %68, align 1
  %70 = zext i8 %69 to i64
  %71 = sub nsw i64 %67, %70
  %72 = shl nsw i64 %71, 1
  %73 = or i64 %72, 1
  %74 = add nsw i64 %72, 35
  %75 = inttoptr i64 %74 to ptr addrspace(1)
  %76 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %60, i64 %61, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull %75) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 27, i64 0, i64 28, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %77 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %76, 0, 0
  %78 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %76, 0, 1
  %79 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %76, 1, 0
  %80 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %77, i64 %78, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlArray_binary_search_string__immstring129" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %79, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1))) #8
  %81 = extractvalue { i64, i64, ptr addrspace(1) } %80, 0
  %82 = extractvalue { i64, i64, ptr addrspace(1) } %80, 1
  %83 = inttoptr i64 %73 to ptr addrspace(1)
  %84 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %81, i64 %82, ptr addrspace(1) %62, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %79, ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1)), ptr addrspace(1) nonnull %83) #8
  %85 = extractvalue { i64, i64, ptr addrspace(1) } %84, 0
  %86 = load i8, ptr addrspace(1) %54, align 1
  %or.cond.not = icmp eq i8 %86, -2
  br i1 %or.cond.not, label %L224, label %L226

L224:                                             ; preds = %L204
  %87 = extractvalue { i64, i64, ptr addrspace(1) } %84, 0
  %88 = extractvalue { i64, i64, ptr addrspace(1) } %84, 1
  %89 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %87, i64 %88, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlArray_binary_search_string__invalid776" to i64) to ptr addrspace(1))) #8
  unreachable

L226:                                             ; preds = %L204
  %90 = extractvalue { i64, i64, ptr addrspace(1) } %84, 1
  %91 = add i64 %90, -24
  %92 = inttoptr i64 %85 to ptr
  %93 = load i64, ptr %92, align 4
  %.not297 = icmp ugt i64 %93, %91
  br i1 %.not297, label %L350, label %L351, !prof !1

L350:                                             ; preds = %L226
  %94 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %85, i64 %91) #9 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 3, i64 27, i64 0, i64 28, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %95 = extractvalue { { i64, i64 }, {} } %94, 0, 0
  %96 = extractvalue { { i64, i64 }, {} } %94, 0, 1
  br label %L351

L351:                                             ; preds = %L350, %L226
  %alloc.3 = phi i64 [ %91, %L226 ], [ %96, %L350 ]
  %ds.3 = phi i64 [ %85, %L226 ], [ %95, %L350 ]
  %97 = add i64 %alloc.3, 8
  %98 = inttoptr i64 %97 to ptr addrspace(1)
  %99 = getelementptr i8, ptr addrspace(1) %98, i64 -8
  store volatile i64 2048, ptr addrspace(1) %99, align 4
  store ptr addrspace(1) %79, ptr addrspace(1) %98, align 8
  %100 = getelementptr i8, ptr addrspace(1) %98, i64 8
  store volatile i64 %57, ptr addrspace(1) %100, align 4
  %101 = shl i64 %57, 2
  %102 = getelementptr i8, ptr addrspace(1) %55, i64 %101
  %103 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %102, ptr addrspace(1) %98) #8
  %104 = extractvalue { i64, i64 } %103, 0
  %105 = extractvalue { i64, i64 } %103, 1
  %106 = add nuw nsw i64 %.0, 1
  %exitcond = icmp eq i64 %106, 64
  br i1 %exitcond, label %L236, label %L204

L236:                                             ; preds = %L351
  %107 = extractvalue { i64, i64 } %103, 0
  %108 = extractvalue { i64, i64 } %103, 1
  %109 = load ptr addrspace(1), ptr addrspace(1) %45, align 8
  %110 = load ptr addrspace(1), ptr addrspace(1) %109, align 8
  %111 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %107, i64 %108, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) nonnull inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %110) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 155, i64 0, i64 12, i64 40, i64 0, i64 40, i64 8, i64 7500385, i64 3045729, i64 27757, i64 17, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 7155321, i64 28769, i64 29, i64 0, i64 13, i64 28, i64 0, i64 28, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %112 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %111, 0, 0
  %113 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %111, 0, 1
  %114 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %111, 1, 0
  %115 = getelementptr i8, ptr addrspace(1) %114, i64 -8
  %116 = getelementptr i8, ptr addrspace(1) %45, i64 -4
  %117 = getelementptr i8, ptr addrspace(1) %114, i64 -4
  %118 = getelementptr i8, ptr addrspace(1) %114, i64 -4
  br label %L252

L252:                                             ; preds = %L279, %L236
  %.0281 = phi i64 [ 1, %L236 ], [ %131, %L279 ]
  %alloc.4 = phi i64 [ %113, %L236 ], [ %alloc.5, %L279 ]
  %ds.4 = phi i64 [ %112, %L236 ], [ %ds.5, %L279 ]
  %119 = shl i64 %.0281, 3
  %120 = or i64 %119, 4
  %121 = getelementptr i8, ptr addrspace(1) %116, i64 %120
  %122 = load ptr addrspace(1), ptr addrspace(1) %121, align 8
  %123 = load ptr addrspace(1), ptr addrspace(1) %122, align 8
  %124 = load i8, ptr addrspace(1) %115, align 1
  %or.cond5.not = icmp eq i8 %124, -2
  br i1 %or.cond5.not, label %L265, label %L272

L265:                                             ; preds = %L252
  %125 = getelementptr i8, ptr addrspace(1) %118, i64 %120
  %126 = load double, ptr addrspace(1) %123, align 8
  store double %126, ptr addrspace(1) %125, align 8
  br label %L279

L272:                                             ; preds = %L252
  %127 = getelementptr i8, ptr addrspace(1) %117, i64 %120
  %128 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.4, i64 %alloc.4, ptr addrspace(1) %127, ptr addrspace(1) %123) #8
  %129 = extractvalue { i64, i64 } %128, 0
  %130 = extractvalue { i64, i64 } %128, 1
  br label %L279

L279:                                             ; preds = %L272, %L265
  %alloc.5 = phi i64 [ %130, %L272 ], [ %alloc.4, %L265 ]
  %ds.5 = phi i64 [ %129, %L272 ], [ %ds.4, %L265 ]
  %131 = add nuw nsw i64 %.0281, 1
  %exitcond321 = icmp eq i64 %131, 64
  br i1 %exitcond321, label %L284, label %L252

L284:                                             ; preds = %L279
  %132 = icmp slt i64 %3, 3
  br i1 %132, label %common.ret, label %L291

L291:                                             ; preds = %L284
  %133 = lshr i64 %3, 1
  %134 = icmp slt i64 %2, 3
  %135 = lshr i64 %2, 1
  br i1 %134, label %L297.us, label %L297.preheader

L297.preheader:                                   ; preds = %L291
  %136 = getelementptr i8, ptr addrspace(1) %114, i64 -4
  br label %L297

L297.us:                                          ; preds = %L291, %L297.us
  %.0282.us = phi i64 [ %137, %L297.us ], [ 1, %L291 ]
  %137 = add i64 %.0282.us, 1
  %138 = icmp sgt i64 %137, %133
  br i1 %138, label %common.ret, label %L297.us

L297:                                             ; preds = %L297.preheader, %L330.loopexit
  %.0283 = phi i64 [ %152, %L330.loopexit ], [ 1, %L297.preheader ]
  %.0282 = phi i64 [ %157, %L330.loopexit ], [ 1, %L297.preheader ]
  %alloc.6 = phi i64 [ %156, %L330.loopexit ], [ %alloc.5, %L297.preheader ]
  %ds.6 = phi i64 [ %155, %L330.loopexit ], [ %ds.5, %L297.preheader ]
  %139 = shl i64 %.0282, 1
  %140 = or i64 %139, 1
  br label %L309

L309:                                             ; preds = %L309, %L297
  %.0286 = phi i64 [ %.0283, %L297 ], [ %152, %L309 ]
  %.0285 = phi i64 [ 1, %L297 ], [ %153, %L309 ]
  %alloc.7 = phi i64 [ %alloc.6, %L297 ], [ %149, %L309 ]
  %ds.7 = phi i64 [ %ds.6, %L297 ], [ %148, %L309 ]
  %141 = shl i64 %.0285, 1
  %142 = add i64 %140, %141
  %143 = shl i64 %142, 2
  %144 = and i64 %143, 508
  %145 = getelementptr i8, ptr addrspace(1) %136, i64 %144
  %146 = load ptr addrspace(1), ptr addrspace(1) %145, align 8
  %147 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__find_4_13_code"(i64 %ds.7, i64 %alloc.7, ptr addrspace(1) %146, ptr addrspace(1) %45) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 20, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %148 = extractvalue { { i64, i64 }, { i64 } } %147, 0, 0
  %149 = extractvalue { { i64, i64 }, { i64 } } %147, 0, 1
  %150 = extractvalue { { i64, i64 }, { i64 } } %147, 1, 0
  %151 = add i64 %.0286, -1
  %152 = add i64 %151, %150
  %153 = add i64 %.0285, 1
  %154 = icmp sgt i64 %153, %135
  br i1 %154, label %L330.loopexit, label %L309

L330.loopexit:                                    ; preds = %L309
  %155 = extractvalue { { i64, i64 }, { i64 } } %147, 0, 0
  %156 = extractvalue { { i64, i64 }, { i64 } } %147, 0, 1
  %157 = add i64 %.0282, 1
  %158 = icmp sgt i64 %157, %133
  br i1 %158, label %common.ret.loopexit322, label %L297

common.ret.loopexit322:                           ; preds = %L330.loopexit
  %159 = extractvalue { { i64, i64 }, { i64 } } %147, 0, 0
  %160 = extractvalue { { i64, i64 }, { i64 } } %147, 0, 1
  br label %common.ret

common.ret:                                       ; preds = %L297.us, %common.ret.loopexit322, %L284
  %ds.8.pn = phi i64 [ %ds.5, %L284 ], [ %159, %common.ret.loopexit322 ], [ %ds.5, %L297.us ]
  %alloc.8.pn = phi i64 [ %alloc.5, %L284 ], [ %160, %common.ret.loopexit322 ], [ %alloc.5, %L297.us ]
  %.0284.pn = phi i64 [ 1, %L284 ], [ %152, %common.ret.loopexit322 ], [ 1, %L297.us ]
  %.pn298 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.8.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn298, i64 %alloc.8.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0284.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
