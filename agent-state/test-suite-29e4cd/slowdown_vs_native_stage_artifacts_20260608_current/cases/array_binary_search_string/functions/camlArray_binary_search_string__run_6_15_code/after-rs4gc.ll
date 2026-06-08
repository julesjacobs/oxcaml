define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlArray_binary_search_string__run_6_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %statepoint_token = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 27, i64 0, i64 50, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026) ]
  %4 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 0, 0
  %gcagg348 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 0, 1
  %gcagg349 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 1, 0
  %remat = getelementptr i8, ptr addrspace(1) %gcagg349, i64 -8
  %5 = load i64, ptr addrspace(1) %remat, align 4
  %6 = lshr i64 %5, 7
  %7 = and i64 %6, 562949953421304
  %8 = add nsw i64 %7, -1
  %remat377 = getelementptr i8, ptr addrspace(1) %gcagg349, i64 %8
  %9 = load i8, ptr addrspace(1) %remat377, align 1
  %10 = zext i8 %9 to i64
  %11 = sub nsw i64 %8, %10
  %12 = shl nsw i64 %11, 1
  %13 = or i64 %12, 1
  %14 = add nsw i64 %12, 35
  %15 = inttoptr i64 %14 to ptr addrspace(1)
  %statepoint_token396 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg, i64 %gcagg348, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) %15, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 27, i64 0, i64 28, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %gcagg349, ptr addrspace(1) %15) ]
  %16 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token396)
  %gcagg349.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token396, i32 0, i32 0) ; (%gcagg349, %gcagg349)
  %gcagg350 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 0, 0
  %gcagg351 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 0, 1
  %gcagg352 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 1, 0
  %17 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg350, i64 %gcagg351, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlArray_binary_search_string__immstring129" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg352, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1))) #8
  %gcagg353 = extractvalue { i64, i64, ptr addrspace(1) } %17, 0
  %gcagg354 = extractvalue { i64, i64, ptr addrspace(1) } %17, 1
  %gcagg355 = extractvalue { i64, i64, ptr addrspace(1) } %17, 2
  %18 = inttoptr i64 %13 to ptr addrspace(1)
  %19 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg353, i64 %gcagg354, ptr addrspace(1) %gcagg349.relocated, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg352, ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1)), ptr addrspace(1) nonnull %18) #8
  %gcagg356 = extractvalue { i64, i64, ptr addrspace(1) } %19, 0
  %gcagg357 = extractvalue { i64, i64, ptr addrspace(1) } %19, 1
  %gcagg358 = extractvalue { i64, i64, ptr addrspace(1) } %19, 2
  %20 = add i64 %gcagg357, -24
  %21 = inttoptr i64 %gcagg356 to ptr
  %22 = load i64, ptr %21, align 4
  %.not = icmp ugt i64 %22, %20
  br i1 %.not, label %L345, label %L346, !prof !1

L345:                                             ; preds = %L1
  %statepoint_token397 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 196609, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg356, i64 %20, i32 0, i32 0) #6 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 3, i64 27, i64 0, i64 28, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %gcagg352) ]
  %23 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token397)
  %gcagg352.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token397, i32 0, i32 0) ; (%gcagg352, %gcagg352)
  %24 = extractvalue { { i64, i64 }, {} } %23, 0, 0
  %25 = extractvalue { { i64, i64 }, {} } %23, 0, 1
  br label %L346

L346:                                             ; preds = %L345, %L1
  %.0411 = phi ptr addrspace(1) [ %gcagg352.relocated, %L345 ], [ %gcagg352, %L1 ]
  %alloc.0 = phi i64 [ %20, %L1 ], [ %25, %L345 ]
  %ds.0 = phi i64 [ %gcagg356, %L1 ], [ %24, %L345 ]
  %26 = add i64 %alloc.0, 8
  %27 = inttoptr i64 %26 to ptr addrspace(1)
  %remat378 = getelementptr i8, ptr addrspace(1) %27, i64 -8
  store volatile i64 2048, ptr addrspace(1) %remat378, align 4
  store ptr addrspace(1) %.0411, ptr addrspace(1) %27, align 8
  %remat379 = getelementptr i8, ptr addrspace(1) %27, i64 8
  store volatile i64 1, ptr addrspace(1) %remat379, align 4
  %statepoint_token398 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %ds.0, i64 %alloc.0, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %27, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %27) ]
  %28 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token398)
  %gcagg359 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 0
  %gcagg360 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 0, 1
  %gcagg361 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %28, 1, 0
  %29 = add i64 %gcagg359, 40
  %30 = inttoptr i64 %29 to ptr
  %31 = load i64, ptr %30, align 4
  %32 = add i64 %31, 376
  %33 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not296 = icmp ult i64 %33, %32
  br i1 %.not296, label %L347, label %L348, !prof !1

L347:                                             ; preds = %L346
  %statepoint_token399 = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %gcagg359, i64 %gcagg360, i64 34, i32 0, i32 0) #6 [ "gc-live"(ptr addrspace(1) %gcagg361) ]
  %34 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token399)
  %gcagg361.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token399, i32 0, i32 0) ; (%gcagg361, %gcagg361)
  %35 = extractvalue { { i64, i64 }, {} } %34, 0, 0
  %36 = extractvalue { { i64, i64 }, {} } %34, 0, 1
  br label %L348

L348:                                             ; preds = %L347, %L346
  %.0412 = phi ptr addrspace(1) [ %gcagg361.relocated, %L347 ], [ %gcagg361, %L346 ]
  %alloc.1 = phi i64 [ %gcagg360, %L346 ], [ %36, %L347 ]
  %ds.1 = phi i64 [ %gcagg359, %L346 ], [ %35, %L347 ]
  br label %L204

L204:                                             ; preds = %L351, %L348
  %.1413 = phi ptr addrspace(1) [ %.0412, %L348 ], [ %.2, %L351 ]
  %.0 = phi i64 [ 1, %L348 ], [ %70, %L351 ]
  %alloc.2 = phi i64 [ %alloc.1, %L348 ], [ %69, %L351 ]
  %ds.2 = phi i64 [ %ds.1, %L348 ], [ %68, %L351 ]
  %37 = shl nuw i64 %.0, 1
  %38 = or i64 %37, 1
  %39 = inttoptr i64 %38 to ptr addrspace(1)
  %statepoint_token400 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %ds.2, i64 %alloc.2, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) %39, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 27, i64 0, i64 50, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %.1413, ptr addrspace(1) %39) ]
  %40 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token400)
  %gcagg361.relocated401 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token400, i32 0, i32 0) ; (%.1413, %.1413)
  %gcagg362 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %40, 0, 0
  %gcagg363 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %40, 0, 1
  %gcagg364 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %40, 1, 0
  %remat382 = getelementptr i8, ptr addrspace(1) %gcagg364, i64 -8
  %41 = load i64, ptr addrspace(1) %remat382, align 4
  %42 = lshr i64 %41, 7
  %43 = and i64 %42, 562949953421304
  %44 = add nsw i64 %43, -1
  %remat383 = getelementptr i8, ptr addrspace(1) %gcagg364, i64 %44
  %45 = load i8, ptr addrspace(1) %remat383, align 1
  %46 = zext i8 %45 to i64
  %47 = sub nsw i64 %44, %46
  %48 = shl nsw i64 %47, 1
  %49 = or i64 %48, 1
  %50 = add nsw i64 %48, 35
  %51 = inttoptr i64 %50 to ptr addrspace(1)
  %statepoint_token402 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg362, i64 %gcagg363, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) %51, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 27, i64 0, i64 28, i64 65, i64 0, i64 65, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %gcagg361.relocated401, ptr addrspace(1) %gcagg364, ptr addrspace(1) %51) ]
  %52 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token402)
  %gcagg361.relocated403 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token402, i32 0, i32 0) ; (%gcagg361.relocated401, %gcagg361.relocated401)
  %gcagg364.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token402, i32 1, i32 1) ; (%gcagg364, %gcagg364)
  %gcagg365 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 0, 0
  %gcagg366 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 0, 1
  %gcagg367 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %52, 1, 0
  %53 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg365, i64 %gcagg366, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlArray_binary_search_string__immstring129" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg367, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1))) #8
  %gcagg368 = extractvalue { i64, i64, ptr addrspace(1) } %53, 0
  %gcagg369 = extractvalue { i64, i64, ptr addrspace(1) } %53, 1
  %gcagg370 = extractvalue { i64, i64, ptr addrspace(1) } %53, 2
  %54 = inttoptr i64 %49 to ptr addrspace(1)
  %55 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg368, i64 %gcagg369, ptr addrspace(1) %gcagg364.relocated, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg367, ptr addrspace(1) nonnull inttoptr (i64 35 to ptr addrspace(1)), ptr addrspace(1) nonnull %54) #8
  %gcagg371 = extractvalue { i64, i64, ptr addrspace(1) } %55, 0
  %gcagg372 = extractvalue { i64, i64, ptr addrspace(1) } %55, 1
  %gcagg373 = extractvalue { i64, i64, ptr addrspace(1) } %55, 2
  %remat380 = getelementptr i8, ptr addrspace(1) %gcagg361.relocated403, i64 -8
  %56 = load i8, ptr addrspace(1) %remat380, align 1
  %or.cond.not = icmp eq i8 %56, -2
  br i1 %or.cond.not, label %L224, label %L226

L224:                                             ; preds = %L204
  %57 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %gcagg371, i64 %gcagg372, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlArray_binary_search_string__invalid776" to i64) to ptr addrspace(1))) #8
  unreachable

L226:                                             ; preds = %L204
  %58 = add i64 %gcagg372, -24
  %59 = inttoptr i64 %gcagg371 to ptr
  %60 = load i64, ptr %59, align 4
  %.not297 = icmp ugt i64 %60, %58
  br i1 %.not297, label %L350, label %L351, !prof !1

L350:                                             ; preds = %L226
  %statepoint_token404 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 196609, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %gcagg371, i64 %58, i32 0, i32 0) #6 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 3, i64 3, i64 27, i64 0, i64 28, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 36, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026, i64 6694958, i64 2715253, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 27, i64 0, i64 4, i64 69, i64 0, i64 69, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %gcagg361.relocated403, ptr addrspace(1) %gcagg367) ]
  %61 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token404)
  %gcagg361.relocated405 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token404, i32 0, i32 0) ; (%gcagg361.relocated403, %gcagg361.relocated403)
  %gcagg367.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token404, i32 1, i32 1) ; (%gcagg367, %gcagg367)
  %62 = extractvalue { { i64, i64 }, {} } %61, 0, 0
  %63 = extractvalue { { i64, i64 }, {} } %61, 0, 1
  br label %L351

L351:                                             ; preds = %L350, %L226
  %.0414 = phi ptr addrspace(1) [ %gcagg367.relocated, %L350 ], [ %gcagg367, %L226 ]
  %.2 = phi ptr addrspace(1) [ %gcagg361.relocated405, %L350 ], [ %gcagg361.relocated403, %L226 ]
  %alloc.3 = phi i64 [ %58, %L226 ], [ %63, %L350 ]
  %ds.3 = phi i64 [ %gcagg371, %L226 ], [ %62, %L350 ]
  %64 = add i64 %alloc.3, 8
  %65 = inttoptr i64 %64 to ptr addrspace(1)
  %remat384 = getelementptr i8, ptr addrspace(1) %65, i64 -8
  store volatile i64 2048, ptr addrspace(1) %remat384, align 4
  store ptr addrspace(1) %.0414, ptr addrspace(1) %65, align 8
  %remat385 = getelementptr i8, ptr addrspace(1) %65, i64 8
  store volatile i64 %38, ptr addrspace(1) %remat385, align 4
  %66 = shl i64 %38, 2
  %remat381 = getelementptr i8, ptr addrspace(1) %.2, i64 -4
  %remat386 = getelementptr i8, ptr addrspace(1) %remat381, i64 %66
  %67 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %remat386, ptr addrspace(1) %65) #8
  %68 = extractvalue { i64, i64 } %67, 0
  %69 = extractvalue { i64, i64 } %67, 1
  %70 = add nuw nsw i64 %.0, 1
  %exitcond = icmp eq i64 %70, 64
  br i1 %exitcond, label %L236, label %L204

L236:                                             ; preds = %L351
  %71 = extractvalue { i64, i64 } %67, 0
  %72 = extractvalue { i64, i64 } %67, 1
  %73 = load ptr addrspace(1), ptr addrspace(1) %.2, align 8
  %74 = load ptr addrspace(1), ptr addrspace(1) %73, align 8
  %statepoint_token406 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %71, i64 %72, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %74, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 155, i64 0, i64 12, i64 40, i64 0, i64 40, i64 8, i64 7500385, i64 3045729, i64 27757, i64 17, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 7155321, i64 28769, i64 29, i64 0, i64 13, i64 28, i64 0, i64 28, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %.2, ptr addrspace(1) %74) ]
  %75 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token406)
  %gcagg361.relocated407 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token406, i32 0, i32 0) ; (%.2, %.2)
  %gcagg374 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %75, 0, 0
  %gcagg375 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %75, 0, 1
  %gcagg376 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %75, 1, 0
  br label %L252

L252:                                             ; preds = %L279, %L236
  %.0281 = phi i64 [ 1, %L236 ], [ %85, %L279 ]
  %alloc.4 = phi i64 [ %gcagg375, %L236 ], [ %alloc.5, %L279 ]
  %ds.4 = phi i64 [ %gcagg374, %L236 ], [ %ds.5, %L279 ]
  %76 = shl i64 %.0281, 3
  %77 = or i64 %76, 4
  %remat388 = getelementptr i8, ptr addrspace(1) %gcagg361.relocated407, i64 -4
  %remat391 = getelementptr i8, ptr addrspace(1) %remat388, i64 %77
  %78 = load ptr addrspace(1), ptr addrspace(1) %remat391, align 8
  %79 = load ptr addrspace(1), ptr addrspace(1) %78, align 8
  %remat387 = getelementptr i8, ptr addrspace(1) %gcagg376, i64 -8
  %80 = load i8, ptr addrspace(1) %remat387, align 1
  %or.cond5.not = icmp eq i8 %80, -2
  br i1 %or.cond5.not, label %L265, label %L272

L265:                                             ; preds = %L252
  %remat390 = getelementptr i8, ptr addrspace(1) %gcagg376, i64 -4
  %81 = load double, ptr addrspace(1) %79, align 8
  %remat392 = getelementptr i8, ptr addrspace(1) %remat390, i64 %77
  store double %81, ptr addrspace(1) %remat392, align 8
  br label %L279

L272:                                             ; preds = %L252
  %remat389 = getelementptr i8, ptr addrspace(1) %gcagg376, i64 -4
  %remat393 = getelementptr i8, ptr addrspace(1) %remat389, i64 %77
  %82 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.4, i64 %alloc.4, ptr addrspace(1) %remat393, ptr addrspace(1) %79) #8
  %83 = extractvalue { i64, i64 } %82, 0
  %84 = extractvalue { i64, i64 } %82, 1
  br label %L279

L279:                                             ; preds = %L272, %L265
  %alloc.5 = phi i64 [ %84, %L272 ], [ %alloc.4, %L265 ]
  %ds.5 = phi i64 [ %83, %L272 ], [ %ds.4, %L265 ]
  %85 = add nuw nsw i64 %.0281, 1
  %exitcond321 = icmp eq i64 %85, 64
  br i1 %exitcond321, label %L284, label %L252

L284:                                             ; preds = %L279
  %86 = icmp slt i64 %3, 3
  br i1 %86, label %common.ret, label %L291

L291:                                             ; preds = %L284
  %87 = lshr i64 %3, 1
  %88 = lshr i64 %2, 1
  %89 = icmp slt i64 %2, 3
  br i1 %89, label %L297.us, label %L297.preheader

L297.preheader:                                   ; preds = %L291
  br label %L297

L297.us:                                          ; preds = %L291, %L297.us
  %.0282.us = phi i64 [ %90, %L297.us ], [ 1, %L291 ]
  %90 = add i64 %.0282.us, 1
  %91 = icmp sgt i64 %90, %87
  br i1 %91, label %common.ret, label %L297.us

L297:                                             ; preds = %L297.preheader, %L330.loopexit
  %.3 = phi ptr addrspace(1) [ %gcagg361.relocated407, %L297.preheader ], [ %gcagg361.relocated409, %L330.loopexit ]
  %.0410 = phi ptr addrspace(1) [ %gcagg376, %L297.preheader ], [ %gcagg376.relocated, %L330.loopexit ]
  %.0283 = phi i64 [ %104, %L330.loopexit ], [ 1, %L297.preheader ]
  %.0282 = phi i64 [ %109, %L330.loopexit ], [ 1, %L297.preheader ]
  %alloc.6 = phi i64 [ %108, %L330.loopexit ], [ %alloc.5, %L297.preheader ]
  %ds.6 = phi i64 [ %107, %L330.loopexit ], [ %ds.5, %L297.preheader ]
  %92 = shl i64 %.0282, 1
  %93 = or i64 %92, 1
  br label %L309

L309:                                             ; preds = %L309, %L297
  %.4 = phi ptr addrspace(1) [ %.3, %L297 ], [ %gcagg361.relocated409, %L309 ]
  %.1 = phi ptr addrspace(1) [ %.0410, %L297 ], [ %gcagg376.relocated, %L309 ]
  %.0286 = phi i64 [ %.0283, %L297 ], [ %104, %L309 ]
  %.0285 = phi i64 [ 1, %L297 ], [ %105, %L309 ]
  %alloc.7 = phi i64 [ %alloc.6, %L297 ], [ %101, %L309 ]
  %ds.7 = phi i64 [ %ds.6, %L297 ], [ %100, %L309 ]
  %94 = shl i64 %.0285, 1
  %95 = add i64 %93, %94
  %96 = shl i64 %95, 2
  %97 = and i64 %96, 508
  %remat394 = getelementptr i8, ptr addrspace(1) %.1, i64 -4
  %remat395 = getelementptr i8, ptr addrspace(1) %remat394, i64 %97
  %98 = load ptr addrspace(1), ptr addrspace(1) %remat395, align 8
  %statepoint_token408 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlArray_binary_search_string__find_4_13_code", i32 4, i32 0, i64 %ds.7, i64 %alloc.7, ptr addrspace(1) %98, ptr addrspace(1) %.4, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 33, i64 0, i64 20, i64 68, i64 0, i64 68, i64 29, i64 7500385, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 27757, i64 30, i64 7500353, i64 6256993, i64 7235938, i64 7959137, i64 6648671, i64 6517345, i64 7561064, i64 6910580, i64 3041134, i64 7239026), "gc-live"(ptr addrspace(1) %.4, ptr addrspace(1) %.1) ]
  %99 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token408)
  %gcagg361.relocated409 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token408, i32 0, i32 0) ; (%.4, %.4)
  %gcagg376.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token408, i32 1, i32 1) ; (%.1, %.1)
  %100 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 0
  %101 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 1
  %102 = extractvalue { { i64, i64 }, { i64 } } %99, 1, 0
  %103 = add i64 %.0286, -1
  %104 = add i64 %103, %102
  %105 = add i64 %.0285, 1
  %106 = icmp sgt i64 %105, %88
  br i1 %106, label %L330.loopexit, label %L309

L330.loopexit:                                    ; preds = %L309
  %107 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 0
  %108 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 1
  %109 = add i64 %.0282, 1
  %110 = icmp sgt i64 %109, %87
  br i1 %110, label %common.ret.loopexit322, label %L297

common.ret.loopexit322:                           ; preds = %L330.loopexit
  %111 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 0
  %112 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 1
  br label %common.ret

common.ret:                                       ; preds = %L297.us, %common.ret.loopexit322, %L284
  %ds.8.pn = phi i64 [ %ds.5, %L284 ], [ %111, %common.ret.loopexit322 ], [ %ds.5, %L297.us ]
  %alloc.8.pn = phi i64 [ %alloc.5, %L284 ], [ %112, %common.ret.loopexit322 ], [ %alloc.5, %L297.us ]
  %.0284.pn = phi i64 [ 1, %L284 ], [ %104, %common.ret.loopexit322 ], [ 1, %L297.us ]
  %.pn298 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.8.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn298, i64 %alloc.8.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0284.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
