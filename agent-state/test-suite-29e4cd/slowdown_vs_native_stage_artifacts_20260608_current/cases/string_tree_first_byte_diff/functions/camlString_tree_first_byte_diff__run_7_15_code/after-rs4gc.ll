define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__run_7_15_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = add i64 %0, 40
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = add i64 %6, 376
  %8 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not = icmp ult i64 %8, %7
  br i1 %.not, label %L389, label %L390, !prof !1

L389:                                             ; preds = %L1
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #6
  %9 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L390

L390:                                             ; preds = %L389, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L389 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L389 ]
  %statepoint_token323 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, i64)) @"\01_camlString_tree_first_byte_diff__build_5_13_code", i32 4, i32 0, i64 %ds.0, i64 %alloc.0, i64 1, i64 127, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 32, i64 0, i64 13, i64 23, i64 0, i64 23, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %12 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token323)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %gcagg276 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %gcagg277 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %statepoint_token324 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %gcagg, i64 %gcagg276, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 17, i64 0, i64 67, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg277) ]
  %13 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token324)
  %gcagg277.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token324, i32 0, i32 0) ; (%gcagg277, %gcagg277)
  %gcagg278 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %13, 0, 0
  %gcagg279 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %13, 0, 1
  %gcagg280 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %13, 1, 0
  %remat = getelementptr i8, ptr addrspace(1) %gcagg280, i64 -8
  %14 = load i64, ptr addrspace(1) %remat, align 4
  %15 = lshr i64 %14, 7
  %16 = and i64 %15, 562949953421304
  %17 = add nsw i64 %16, -1
  %remat311 = getelementptr i8, ptr addrspace(1) %gcagg280, i64 %17
  %18 = load i8, ptr addrspace(1) %remat311, align 1
  %19 = zext i8 %18 to i64
  %20 = sub nsw i64 %17, %19
  %21 = shl nsw i64 %20, 1
  %22 = or i64 %21, 1
  %23 = add nsw i64 %21, 29
  %24 = inttoptr i64 %23 to ptr addrspace(1)
  %statepoint_token325 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg278, i64 %gcagg279, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) %24, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 48, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg277.relocated, ptr addrspace(1) %gcagg280, ptr addrspace(1) %24) ]
  %25 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token325)
  %gcagg277.relocated326 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token325, i32 0, i32 0) ; (%gcagg277.relocated, %gcagg277.relocated)
  %gcagg280.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token325, i32 1, i32 1) ; (%gcagg280, %gcagg280)
  %gcagg281 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %25, 0, 0
  %gcagg282 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %25, 0, 1
  %gcagg283 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %25, 1, 0
  %26 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg281, i64 %gcagg282, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlString_tree_first_byte_diff__immstring83" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg283, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1))) #8
  %gcagg284 = extractvalue { i64, i64, ptr addrspace(1) } %26, 0
  %gcagg285 = extractvalue { i64, i64, ptr addrspace(1) } %26, 1
  %gcagg286 = extractvalue { i64, i64, ptr addrspace(1) } %26, 2
  %27 = inttoptr i64 %22 to ptr addrspace(1)
  %28 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg284, i64 %gcagg285, ptr addrspace(1) %gcagg280.relocated, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg283, ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1)), ptr addrspace(1) nonnull %27) #8
  %gcagg287 = extractvalue { i64, i64, ptr addrspace(1) } %28, 0
  %gcagg288 = extractvalue { i64, i64, ptr addrspace(1) } %28, 1
  %gcagg289 = extractvalue { i64, i64, ptr addrspace(1) } %28, 2
  %statepoint_token327 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg287, i64 %gcagg288, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) inttoptr (i64 3 to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 5, i64 60, i64 0, i64 10, i64 18, i64 0, i64 18, i64 8, i64 7633250, i64 3044197, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4349791, i64 6648953, i64 7155315, i64 6646625, i64 44, i64 0, i64 2, i64 12, i64 0, i64 12, i64 9, i64 7500915, i64 6778473, i64 7105838, i64 19, i64 6583379, i64 6449516, i64 5463903, i64 6910580, i64 3041134, i64 7037293, i64 101, i64 17, i64 0, i64 2, i64 45, i64 0, i64 45, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg277.relocated326, ptr addrspace(1) %gcagg283) ]
  %29 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token327)
  %gcagg277.relocated328 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token327, i32 0, i32 0) ; (%gcagg277.relocated326, %gcagg277.relocated326)
  %gcagg283.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token327, i32 1, i32 1) ; (%gcagg283, %gcagg283)
  %gcagg290 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %29, 0, 0
  %gcagg291 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %29, 0, 1
  %gcagg292 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %29, 1, 0
  %30 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_fill_bytes"(i64 %gcagg290, i64 %gcagg291, ptr addrspace(1) %gcagg292, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 3 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 131 to ptr addrspace(1))) #8
  %gcagg293 = extractvalue { i64, i64, ptr addrspace(1) } %30, 0
  %gcagg294 = extractvalue { i64, i64, ptr addrspace(1) } %30, 1
  %gcagg295 = extractvalue { i64, i64, ptr addrspace(1) } %30, 2
  %remat312 = getelementptr i8, ptr addrspace(1) %gcagg292, i64 -8
  %31 = load i64, ptr addrspace(1) %remat312, align 4
  %32 = lshr i64 %31, 7
  %33 = and i64 %32, 562949953421304
  %34 = add nsw i64 %33, -1
  %remat313 = getelementptr i8, ptr addrspace(1) %gcagg292, i64 %34
  %35 = load i8, ptr addrspace(1) %remat313, align 1
  %36 = zext i8 %35 to i64
  %37 = sub nsw i64 %34, %36
  %38 = shl nsw i64 %37, 1
  %39 = or i64 %38, 1
  %remat314 = getelementptr i8, ptr addrspace(1) %gcagg283.relocated, i64 -8
  %40 = load i64, ptr addrspace(1) %remat314, align 4
  %41 = lshr i64 %40, 7
  %42 = and i64 %41, 562949953421304
  %43 = add nsw i64 %42, -1
  %remat315 = getelementptr i8, ptr addrspace(1) %gcagg283.relocated, i64 %43
  %44 = load i8, ptr addrspace(1) %remat315, align 1
  %45 = zext i8 %44 to i64
  %46 = sub nsw i64 %43, %45
  %47 = shl nsw i64 %46, 1
  %48 = or i64 %47, 1
  %49 = add nsw i64 %38, %48
  %50 = inttoptr i64 %49 to ptr addrspace(1)
  %statepoint_token329 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg293, i64 %gcagg294, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) %50, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 2, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg277.relocated328, ptr addrspace(1) %gcagg283.relocated, ptr addrspace(1) %gcagg292, ptr addrspace(1) %50) ]
  %51 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token329)
  %gcagg277.relocated330 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token329, i32 0, i32 0) ; (%gcagg277.relocated328, %gcagg277.relocated328)
  %gcagg283.relocated331 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token329, i32 1, i32 1) ; (%gcagg283.relocated, %gcagg283.relocated)
  %gcagg292.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token329, i32 2, i32 2) ; (%gcagg292, %gcagg292)
  %gcagg296 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %51, 0, 0
  %gcagg297 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %51, 0, 1
  %gcagg298 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %51, 1, 0
  %52 = inttoptr i64 %39 to ptr addrspace(1)
  %53 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg296, i64 %gcagg297, ptr addrspace(1) %gcagg292.relocated, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg298, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull %52) #8
  %gcagg299 = extractvalue { i64, i64, ptr addrspace(1) } %53, 0
  %gcagg300 = extractvalue { i64, i64, ptr addrspace(1) } %53, 1
  %gcagg301 = extractvalue { i64, i64, ptr addrspace(1) } %53, 2
  %54 = inttoptr i64 %48 to ptr addrspace(1)
  %55 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg299, i64 %gcagg300, ptr addrspace(1) %gcagg283.relocated331, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg298, ptr addrspace(1) nonnull %52, ptr addrspace(1) nonnull %54) #8
  %gcagg302 = extractvalue { i64, i64, ptr addrspace(1) } %55, 0
  %gcagg303 = extractvalue { i64, i64, ptr addrspace(1) } %55, 1
  %gcagg304 = extractvalue { i64, i64, ptr addrspace(1) } %55, 2
  %statepoint_token332 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %gcagg302, i64 %gcagg303, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %gcagg298, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg277.relocated330, ptr addrspace(1) %gcagg298) ]
  %56 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token332)
  %gcagg277.relocated333 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token332, i32 0, i32 0) ; (%gcagg277.relocated330, %gcagg277.relocated330)
  %gcagg305 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %56, 0, 0
  %gcagg306 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %56, 0, 1
  %gcagg307 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %56, 1, 0
  br label %L301

L301:                                             ; preds = %L324, %L390
  %.0340 = phi ptr addrspace(1) [ %gcagg307, %L390 ], [ %gcagg307.relocated, %L324 ]
  %.0339 = phi ptr addrspace(1) [ %gcagg277.relocated333, %L390 ], [ %gcagg277.relocated335, %L324 ]
  %.0 = phi i64 [ 1, %L390 ], [ %66, %L324 ]
  %alloc.1 = phi i64 [ %gcagg306, %L390 ], [ %alloc.2, %L324 ]
  %ds.1 = phi i64 [ %gcagg305, %L390 ], [ %ds.2, %L324 ]
  %57 = shl nuw i64 %.0, 1
  %58 = or i64 %57, 1
  %statepoint_token334 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64)) @"\01_camlString_tree_first_byte_diff__key_4_12_code", i32 3, i32 0, i64 %ds.1, i64 %alloc.1, i64 %58, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.0340, ptr addrspace(1) %.0339) ]
  %59 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token334)
  %gcagg307.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token334, i32 0, i32 0) ; (%.0340, %.0340)
  %gcagg277.relocated335 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token334, i32 1, i32 1) ; (%.0339, %.0339)
  %gcagg308 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 0, 0
  %gcagg309 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 0, 1
  %gcagg310 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 1, 0
  %remat316 = getelementptr i8, ptr addrspace(1) %gcagg307.relocated, i64 -8
  %60 = load i8, ptr addrspace(1) %remat316, align 1
  %61 = shl i64 %58, 2
  %or.cond.not = icmp eq i8 %60, -2
  br i1 %or.cond.not, label %L310, label %L317

L310:                                             ; preds = %L301
  %remat318 = getelementptr i8, ptr addrspace(1) %gcagg307.relocated, i64 -4
  %62 = load double, ptr addrspace(1) %gcagg310, align 8
  %remat319 = getelementptr i8, ptr addrspace(1) %remat318, i64 %61
  store double %62, ptr addrspace(1) %remat319, align 8
  br label %L324

L317:                                             ; preds = %L301
  %remat317 = getelementptr i8, ptr addrspace(1) %gcagg307.relocated, i64 -4
  %remat320 = getelementptr i8, ptr addrspace(1) %remat317, i64 %61
  %63 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %gcagg308, i64 %gcagg309, ptr addrspace(1) %remat320, ptr addrspace(1) %gcagg310) #8
  %64 = extractvalue { i64, i64 } %63, 0
  %65 = extractvalue { i64, i64 } %63, 1
  br label %L324

L324:                                             ; preds = %L317, %L310
  %alloc.2 = phi i64 [ %65, %L317 ], [ %gcagg309, %L310 ]
  %ds.2 = phi i64 [ %64, %L317 ], [ %gcagg308, %L310 ]
  %66 = add nuw nsw i64 %.0, 1
  %exitcond = icmp eq i64 %66, 64
  br i1 %exitcond, label %L329, label %L301

L329:                                             ; preds = %L324
  %67 = icmp slt i64 %3, 3
  br i1 %67, label %common.ret, label %L336

L336:                                             ; preds = %L329
  %68 = lshr i64 %3, 1
  %69 = lshr i64 %2, 1
  %70 = icmp slt i64 %2, 3
  br i1 %70, label %L342.us, label %L342.preheader

L342.preheader:                                   ; preds = %L336
  br label %L342

L342.us:                                          ; preds = %L336, %L342.us
  %.0247.us = phi i64 [ %71, %L342.us ], [ 1, %L336 ]
  %71 = add i64 %.0247.us, 1
  %72 = icmp sgt i64 %71, %68
  br i1 %72, label %common.ret, label %L342.us

L342:                                             ; preds = %L342.preheader, %L375.loopexit
  %.1341 = phi ptr addrspace(1) [ %gcagg307.relocated, %L342.preheader ], [ %gcagg307.relocated338, %L375.loopexit ]
  %.1 = phi ptr addrspace(1) [ %gcagg277.relocated335, %L342.preheader ], [ %gcagg277.relocated337, %L375.loopexit ]
  %.0248 = phi i64 [ %85, %L375.loopexit ], [ 1, %L342.preheader ]
  %.0247 = phi i64 [ %90, %L375.loopexit ], [ 1, %L342.preheader ]
  %alloc.3 = phi i64 [ %89, %L375.loopexit ], [ %alloc.2, %L342.preheader ]
  %ds.3 = phi i64 [ %88, %L375.loopexit ], [ %ds.2, %L342.preheader ]
  %73 = shl i64 %.0247, 1
  %74 = or i64 %73, 1
  br label %L354

L354:                                             ; preds = %L354, %L342
  %.2342 = phi ptr addrspace(1) [ %.1341, %L342 ], [ %gcagg307.relocated338, %L354 ]
  %.2 = phi ptr addrspace(1) [ %.1, %L342 ], [ %gcagg277.relocated337, %L354 ]
  %.0251 = phi i64 [ %.0248, %L342 ], [ %85, %L354 ]
  %.0250 = phi i64 [ 1, %L342 ], [ %86, %L354 ]
  %alloc.4 = phi i64 [ %alloc.3, %L342 ], [ %82, %L354 ]
  %ds.4 = phi i64 [ %ds.3, %L342 ], [ %81, %L354 ]
  %75 = shl i64 %.0250, 1
  %76 = add i64 %74, %75
  %77 = shl i64 %76, 2
  %78 = and i64 %77, 508
  %remat321 = getelementptr i8, ptr addrspace(1) %.2342, i64 -4
  %remat322 = getelementptr i8, ptr addrspace(1) %remat321, i64 %78
  %79 = load ptr addrspace(1), ptr addrspace(1) %remat322, align 8
  %statepoint_token336 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_camlString_tree_first_byte_diff__find_6_14_code", i32 4, i32 0, i64 %ds.4, i64 %alloc.4, ptr addrspace(1) %79, ptr addrspace(1) %.2, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 37, i64 0, i64 20, i64 71, i64 0, i64 71, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.2, ptr addrspace(1) %.2342) ]
  %80 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token336)
  %gcagg277.relocated337 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token336, i32 0, i32 0) ; (%.2, %.2)
  %gcagg307.relocated338 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token336, i32 1, i32 1) ; (%.2342, %.2342)
  %81 = extractvalue { { i64, i64 }, { i64 } } %80, 0, 0
  %82 = extractvalue { { i64, i64 }, { i64 } } %80, 0, 1
  %83 = extractvalue { { i64, i64 }, { i64 } } %80, 1, 0
  %84 = add i64 %.0251, -1
  %85 = add i64 %84, %83
  %86 = add i64 %.0250, 1
  %87 = icmp sgt i64 %86, %69
  br i1 %87, label %L375.loopexit, label %L354

L375.loopexit:                                    ; preds = %L354
  %88 = extractvalue { { i64, i64 }, { i64 } } %80, 0, 0
  %89 = extractvalue { { i64, i64 }, { i64 } } %80, 0, 1
  %90 = add i64 %.0247, 1
  %91 = icmp sgt i64 %90, %68
  br i1 %91, label %common.ret.loopexit268, label %L342

common.ret.loopexit268:                           ; preds = %L375.loopexit
  %92 = extractvalue { { i64, i64 }, { i64 } } %80, 0, 0
  %93 = extractvalue { { i64, i64 }, { i64 } } %80, 0, 1
  br label %common.ret

common.ret:                                       ; preds = %L342.us, %common.ret.loopexit268, %L329
  %ds.5.pn = phi i64 [ %ds.2, %L329 ], [ %92, %common.ret.loopexit268 ], [ %ds.2, %L342.us ]
  %alloc.5.pn = phi i64 [ %alloc.2, %L329 ], [ %93, %common.ret.loopexit268 ], [ %alloc.2, %L342.us ]
  %.0249.pn = phi i64 [ 1, %L329 ], [ %85, %common.ret.loopexit268 ], [ 1, %L342.us ]
  %.pn258 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.5.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn258, i64 %alloc.5.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0249.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
