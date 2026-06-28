define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_map_equal_content__run_6_52_code"(i64 %0, i64 %1, i64 %2, i64 %3) #0 gc "oxcaml" {
L1:
  %statepoint_token = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 19, i64 0, i64 63, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %4 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 0, 0
  %gcagg446 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 0, 1
  %gcagg447 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %4, 1, 0
  %remat = getelementptr i8, ptr addrspace(1) %gcagg447, i64 -8
  %5 = load i64, ptr addrspace(1) %remat, align 4
  %6 = lshr i64 %5, 7
  %7 = and i64 %6, 562949953421304
  %8 = add nsw i64 %7, -1
  %remat495 = getelementptr i8, ptr addrspace(1) %gcagg447, i64 %8
  %9 = load i8, ptr addrspace(1) %remat495, align 1
  %10 = zext i8 %9 to i64
  %11 = sub nsw i64 %8, %10
  %12 = shl nsw i64 %11, 1
  %13 = or i64 %12, 1
  %14 = add nsw i64 %12, 39
  %15 = inttoptr i64 %14 to ptr addrspace(1)
  %statepoint_token522 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg, i64 %gcagg446, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) %15, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 19, i64 0, i64 39, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg447, ptr addrspace(1) %15) ]
  %16 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token522)
  %gcagg447.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token522, i32 0, i32 0) ; (%gcagg447, %gcagg447)
  %gcagg448 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 0, 0
  %gcagg449 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 0, 1
  %gcagg450 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %16, 1, 0
  %17 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg448, i64 %gcagg449, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlString_map_equal_content__immstring119" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg450, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1))) #6
  %gcagg451 = extractvalue { i64, i64, ptr addrspace(1) } %17, 0
  %gcagg452 = extractvalue { i64, i64, ptr addrspace(1) } %17, 1
  %gcagg453 = extractvalue { i64, i64, ptr addrspace(1) } %17, 2
  %18 = inttoptr i64 %13 to ptr addrspace(1)
  %19 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg451, i64 %gcagg452, ptr addrspace(1) %gcagg447.relocated, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg450, ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1)), ptr addrspace(1) nonnull %18) #6
  %gcagg454 = extractvalue { i64, i64, ptr addrspace(1) } %19, 0
  %gcagg455 = extractvalue { i64, i64, ptr addrspace(1) } %19, 1
  %gcagg456 = extractvalue { i64, i64, ptr addrspace(1) } %19, 2
  %statepoint_token523 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %gcagg454, i64 %gcagg455, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %gcagg450, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg450) ]
  %20 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token523)
  %gcagg457 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %20, 0, 0
  %gcagg458 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %20, 0, 1
  %gcagg459 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %20, 1, 0
  %21 = add i64 %gcagg457, 40
  %22 = inttoptr i64 %21 to ptr
  %23 = load i64, ptr %22, align 4
  %24 = add i64 %23, 376
  %25 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #7
  %.not = icmp ult i64 %25, %24
  br i1 %.not, label %L1731, label %L1732, !prof !1

L1731:                                            ; preds = %L1
  %statepoint_token524 = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %gcagg457, i64 %gcagg458, i64 34, i32 0, i32 0) #5 [ "gc-live"(ptr addrspace(1) %gcagg459) ]
  %26 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token524)
  %gcagg459.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token524, i32 0, i32 0) ; (%gcagg459, %gcagg459)
  %27 = extractvalue { { i64, i64 }, {} } %26, 0, 0
  %28 = extractvalue { { i64, i64 }, {} } %26, 0, 1
  br label %L1732

L1732:                                            ; preds = %L1731, %L1
  %.0543 = phi ptr addrspace(1) [ %gcagg459.relocated, %L1731 ], [ %gcagg459, %L1 ]
  %alloc.0 = phi i64 [ %gcagg458, %L1 ], [ %28, %L1731 ]
  %ds.0 = phi i64 [ %gcagg457, %L1 ], [ %27, %L1731 ]
  br label %L1544

L1544:                                            ; preds = %L1544.backedge, %L1732
  %.1544 = phi ptr addrspace(1) [ %.0543, %L1732 ], [ %gcagg459.relocated528, %L1544.backedge ]
  %.0315 = phi i64 [ 1, %L1732 ], [ %.0315.be, %L1544.backedge ]
  %alloc.1 = phi i64 [ %alloc.0, %L1732 ], [ %alloc.1.be, %L1544.backedge ]
  %ds.1 = phi i64 [ %ds.0, %L1732 ], [ %ds.1.be, %L1544.backedge ]
  %29 = shl nuw i64 %.0315, 1
  %30 = or i64 %29, 1
  %31 = inttoptr i64 %30 to ptr addrspace(1)
  %statepoint_token525 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %ds.1, i64 %alloc.1, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) %31, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 19, i64 0, i64 63, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.1544, ptr addrspace(1) %31) ]
  %32 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token525)
  %gcagg459.relocated526 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token525, i32 0, i32 0) ; (%.1544, %.1544)
  %gcagg460 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 0
  %gcagg461 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 1
  %gcagg462 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 1, 0
  %remat499 = getelementptr i8, ptr addrspace(1) %gcagg462, i64 -8
  %33 = load i64, ptr addrspace(1) %remat499, align 4
  %34 = lshr i64 %33, 7
  %35 = and i64 %34, 562949953421304
  %36 = add nsw i64 %35, -1
  %remat500 = getelementptr i8, ptr addrspace(1) %gcagg462, i64 %36
  %37 = load i8, ptr addrspace(1) %remat500, align 1
  %38 = zext i8 %37 to i64
  %39 = sub nsw i64 %36, %38
  %40 = shl nsw i64 %39, 1
  %41 = or i64 %40, 1
  %42 = add nsw i64 %40, 39
  %43 = inttoptr i64 %42 to ptr addrspace(1)
  %statepoint_token527 = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_caml_c_call", i32 4, i32 0, i64 %gcagg460, i64 %gcagg461, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) %43, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 19, i64 0, i64 39, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg459.relocated526, ptr addrspace(1) %gcagg462, ptr addrspace(1) %43) ]
  %44 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token527)
  %gcagg459.relocated528 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token527, i32 0, i32 0) ; (%gcagg459.relocated526, %gcagg459.relocated526)
  %gcagg462.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token527, i32 1, i32 1) ; (%gcagg462, %gcagg462)
  %gcagg463 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %44, 0, 0
  %gcagg464 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %44, 0, 1
  %gcagg465 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %44, 1, 0
  %45 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg463, i64 %gcagg464, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlString_map_equal_content__immstring119" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg465, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1))) #6
  %gcagg466 = extractvalue { i64, i64, ptr addrspace(1) } %45, 0
  %gcagg467 = extractvalue { i64, i64, ptr addrspace(1) } %45, 1
  %gcagg468 = extractvalue { i64, i64, ptr addrspace(1) } %45, 2
  %46 = inttoptr i64 %41 to ptr addrspace(1)
  %47 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %gcagg466, i64 %gcagg467, ptr addrspace(1) %gcagg462.relocated, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %gcagg465, ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1)), ptr addrspace(1) nonnull %46) #6
  %gcagg469 = extractvalue { i64, i64, ptr addrspace(1) } %47, 0
  %gcagg470 = extractvalue { i64, i64, ptr addrspace(1) } %47, 1
  %gcagg471 = extractvalue { i64, i64, ptr addrspace(1) } %47, 2
  %remat496 = getelementptr i8, ptr addrspace(1) %gcagg459.relocated528, i64 -8
  %48 = load i8, ptr addrspace(1) %remat496, align 1
  %49 = shl i64 %30, 2
  %or.cond.not.not = icmp eq i8 %48, -2
  br i1 %or.cond.not.not, label %L1581, label %L1581.thread

L1581:                                            ; preds = %L1544
  %remat498 = getelementptr i8, ptr addrspace(1) %gcagg459.relocated528, i64 -4
  %50 = load double, ptr addrspace(1) %gcagg465, align 8
  %remat501 = getelementptr i8, ptr addrspace(1) %remat498, i64 %49
  store double %50, ptr addrspace(1) %remat501, align 8
  %51 = add nuw nsw i64 %.0315, 1
  %exitcond = icmp eq i64 %51, 64
  br i1 %exitcond, label %L1600.us.preheader, label %L1544.backedge

L1544.backedge:                                   ; preds = %L1581, %L1581.thread
  %.0315.be = phi i64 [ %51, %L1581 ], [ %55, %L1581.thread ]
  %alloc.1.be = phi i64 [ %gcagg470, %L1581 ], [ %54, %L1581.thread ]
  %ds.1.be = phi i64 [ %gcagg469, %L1581 ], [ %53, %L1581.thread ]
  br label %L1544

L1581.thread:                                     ; preds = %L1544
  %remat497 = getelementptr i8, ptr addrspace(1) %gcagg459.relocated528, i64 -4
  %remat502 = getelementptr i8, ptr addrspace(1) %remat497, i64 %49
  %52 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %gcagg469, i64 %gcagg470, ptr addrspace(1) %remat502, ptr addrspace(1) %gcagg465) #6
  %53 = extractvalue { i64, i64 } %52, 0
  %54 = extractvalue { i64, i64 } %52, 1
  %55 = add nuw nsw i64 %.0315, 1
  %exitcond383 = icmp eq i64 %55, 64
  br i1 %exitcond383, label %L1600.preheader, label %L1544.backedge

L1600.preheader:                                  ; preds = %L1581.thread
  %56 = extractvalue { i64, i64 } %52, 0
  %57 = extractvalue { i64, i64 } %52, 1
  %statepoint_token529 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlStdlib__Array__map_10_80_code", i32 4, i32 0, i64 %56, i64 %57, i64 ptrtoint (ptr @"\01_camlString_map_equal_content__fresh_46" to i64), ptr addrspace(1) %gcagg459.relocated528, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 20, i64 0, i64 15, i64 37, i64 0, i64 37, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg459.relocated528) ]
  %58 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token529)
  %gcagg459.relocated530 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token529, i32 0, i32 0) ; (%gcagg459.relocated528, %gcagg459.relocated528)
  %gcagg472 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %58, 0, 0
  %gcagg473 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %58, 0, 1
  %gcagg474 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %58, 1, 0
  br label %L1600

L1600.us.preheader:                               ; preds = %L1581
  %statepoint_token531 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1))) @"\01_camlStdlib__Array__map_10_80_code", i32 4, i32 0, i64 %gcagg469, i64 %gcagg470, i64 ptrtoint (ptr @"\01_camlString_map_equal_content__fresh_46" to i64), ptr addrspace(1) %gcagg459.relocated528, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 20, i64 0, i64 15, i64 37, i64 0, i64 37, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %gcagg459.relocated528) ]
  %59 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token531)
  %gcagg459.relocated532 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token531, i32 0, i32 0) ; (%gcagg459.relocated528, %gcagg459.relocated528)
  %gcagg475 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 0, 0
  %gcagg476 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 0, 1
  %gcagg477 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %59, 1, 0
  br label %L1600.us

L1600.us:                                         ; preds = %L1600.us.preheader, %L1736.us
  %.2 = phi ptr addrspace(1) [ %gcagg459.relocated532, %L1600.us.preheader ], [ %gcagg459.relocated537, %L1736.us ]
  %.0542 = phi ptr addrspace(1) [ %gcagg477, %L1600.us.preheader ], [ %gcagg477.relocated536, %L1736.us ]
  %.0318.us = phi ptr addrspace(1) [ %gcagg482, %L1736.us ], [ inttoptr (i64 1 to ptr addrspace(1)), %L1600.us.preheader ]
  %.0317.us = phi i64 [ %77, %L1736.us ], [ 0, %L1600.us.preheader ]
  %.pn419.gcagg = phi i64 [ %gcagg480, %L1736.us ], [ %gcagg475, %L1600.us.preheader ]
  %.pn419.gcagg478 = phi i64 [ %gcagg481, %L1736.us ], [ %gcagg476, %L1600.us.preheader ]
  %.pn419.gcagg479 = phi ptr addrspace(1) [ %gcagg482, %L1736.us ], [ %gcagg477, %L1600.us.preheader ]
  %60 = shl i64 %.0317.us, 3
  %61 = or i64 %60, 4
  %62 = add i64 %.pn419.gcagg478, -16
  %63 = inttoptr i64 %.pn419.gcagg to ptr
  %64 = load i64, ptr %63, align 4
  %.not342.us = icmp ugt i64 %64, %62
  br i1 %.not342.us, label %L1735.us, label %L1736.us, !prof !1

L1735.us:                                         ; preds = %L1600.us
  %statepoint_token533 = call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 131073, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64)) @"\01_caml_call_gc", i32 2, i32 0, i64 %.pn419.gcagg, i64 %62, i32 0, i32 0) #5 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 2, i64 222, i64 0, i64 14, i64 30, i64 0, i64 30, i64 8, i64 7500385, i64 3045729, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6696569, i64 6581359, i64 6646879, i64 29798, i64 22, i64 0, i64 4, i64 75, i64 0, i64 75, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.0542, ptr addrspace(1) %.2, ptr addrspace(1) %.0318.us) ]
  %65 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token533)
  %gcagg477.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token533, i32 0, i32 0) ; (%.0542, %.0542)
  %gcagg459.relocated534 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token533, i32 1, i32 1) ; (%.2, %.2)
  %.0318.us.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token533, i32 2, i32 2) ; (%.0318.us, %.0318.us)
  %66 = extractvalue { { i64, i64 }, {} } %65, 0, 0
  %67 = extractvalue { { i64, i64 }, {} } %65, 0, 1
  br label %L1736.us

L1736.us:                                         ; preds = %L1735.us, %L1600.us
  %.3 = phi ptr addrspace(1) [ %gcagg459.relocated534, %L1735.us ], [ %.2, %L1600.us ]
  %.1 = phi ptr addrspace(1) [ %gcagg477.relocated, %L1735.us ], [ %.0542, %L1600.us ]
  %.0541 = phi ptr addrspace(1) [ %.0318.us.relocated, %L1735.us ], [ %.0318.us, %L1600.us ]
  %alloc.4.us = phi i64 [ %62, %L1600.us ], [ %67, %L1735.us ]
  %ds.4.us = phi i64 [ %.pn419.gcagg, %L1600.us ], [ %66, %L1735.us ]
  %68 = add i64 %alloc.4.us, 8
  %69 = inttoptr i64 %68 to ptr addrspace(1)
  %remat505 = getelementptr i8, ptr addrspace(1) %69, i64 -8
  store volatile i64 1277, ptr addrspace(1) %remat505, align 4
  %remat504 = getelementptr i8, ptr addrspace(1) %.3, i64 -4
  %remat506 = getelementptr i8, ptr addrspace(1) %remat504, i64 %61
  %70 = load double, ptr addrspace(1) %remat506, align 8
  store double %70, ptr addrspace(1) %69, align 8
  %71 = bitcast double %70 to i64
  %72 = lshr i64 %71, 55
  %73 = and i64 %72, 510
  %74 = sub nsw i64 15, %73
  %75 = inttoptr i64 %74 to ptr addrspace(1)
  %statepoint_token535 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, ptr addrspace(1), ptr addrspace(1), ptr addrspace(1))) @"\01_camlString_map_equal_content__add_6_32_code", i32 5, i32 0, i64 %ds.4.us, i64 %alloc.4.us, ptr addrspace(1) %69, ptr addrspace(1) %75, ptr addrspace(1) %.0541, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 3, i64 22, i64 0, i64 32, i64 59, i64 0, i64 59, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 222, i64 0, i64 9, i64 30, i64 0, i64 30, i64 8, i64 7500385, i64 3045729, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6696569, i64 6581359, i64 6646879, i64 29798, i64 22, i64 0, i64 4, i64 75, i64 0, i64 75, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.1, ptr addrspace(1) %.3) ]
  %76 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token535)
  %gcagg477.relocated536 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token535, i32 0, i32 0) ; (%.1, %.1)
  %gcagg459.relocated537 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token535, i32 1, i32 1) ; (%.3, %.3)
  %gcagg480 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %76, 0, 0
  %gcagg481 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %76, 0, 1
  %gcagg482 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %76, 1, 0
  %77 = add nuw nsw i64 %.0317.us, 1
  %exitcond378 = icmp eq i64 %77, 64
  br i1 %exitcond378, label %L1636, label %L1600.us

L1600:                                            ; preds = %L1600.preheader, %L1600
  %.4 = phi ptr addrspace(1) [ %gcagg459.relocated530, %L1600.preheader ], [ %gcagg459.relocated539, %L1600 ]
  %.0540 = phi ptr addrspace(1) [ %gcagg474, %L1600.preheader ], [ %gcagg474.relocated, %L1600 ]
  %.0318 = phi ptr addrspace(1) [ %gcagg487, %L1600 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L1600.preheader ]
  %.0317 = phi i64 [ %92, %L1600 ], [ 0, %L1600.preheader ]
  %.pn416.gcagg = phi i64 [ %gcagg485, %L1600 ], [ %gcagg472, %L1600.preheader ]
  %.pn416.gcagg483 = phi i64 [ %gcagg486, %L1600 ], [ %gcagg473, %L1600.preheader ]
  %.pn416.gcagg484 = phi ptr addrspace(1) [ %gcagg487, %L1600 ], [ %gcagg474, %L1600.preheader ]
  %78 = shl i64 %.0317, 3
  %79 = or i64 %78, 4
  %remat503 = getelementptr i8, ptr addrspace(1) %.4, i64 -4
  %remat507 = getelementptr i8, ptr addrspace(1) %remat503, i64 %79
  %80 = load ptr addrspace(1), ptr addrspace(1) %remat507, align 8
  %remat508 = getelementptr i8, ptr addrspace(1) %80, i64 -8
  %81 = load i64, ptr addrspace(1) %remat508, align 4
  %82 = lshr i64 %81, 7
  %83 = and i64 %82, 562949953421304
  %84 = add nsw i64 %83, -1
  %remat509 = getelementptr i8, ptr addrspace(1) %80, i64 %84
  %85 = load i8, ptr addrspace(1) %remat509, align 1
  %86 = zext i8 %85 to i64
  %87 = sub nsw i64 %84, %86
  %88 = shl nsw i64 %87, 1
  %89 = or i64 %88, 1
  %90 = inttoptr i64 %89 to ptr addrspace(1)
  %statepoint_token538 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, ptr addrspace(1), ptr addrspace(1), ptr addrspace(1))) @"\01_camlString_map_equal_content__add_6_32_code", i32 5, i32 0, i64 %.pn416.gcagg, i64 %.pn416.gcagg483, ptr addrspace(1) %80, ptr addrspace(1) %90, ptr addrspace(1) %.0318, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 3, i64 22, i64 0, i64 32, i64 59, i64 0, i64 59, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 222, i64 0, i64 9, i64 30, i64 0, i64 30, i64 8, i64 7500385, i64 3045729, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6696569, i64 6581359, i64 6646879, i64 29798, i64 22, i64 0, i64 4, i64 75, i64 0, i64 75, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110), "gc-live"(ptr addrspace(1) %.0540, ptr addrspace(1) %.4) ]
  %91 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token538)
  %gcagg474.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token538, i32 0, i32 0) ; (%.0540, %.0540)
  %gcagg459.relocated539 = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token538, i32 1, i32 1) ; (%.4, %.4)
  %gcagg485 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 0
  %gcagg486 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 0, 1
  %gcagg487 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %91, 1, 0
  %92 = add nuw nsw i64 %.0317, 1
  %exitcond377 = icmp eq i64 %92, 64
  br i1 %exitcond377, label %L1636, label %L1600

L1636:                                            ; preds = %L1600, %L1736.us
  %.lcssa410.sink415.gcagg = phi i64 [ %gcagg480, %L1736.us ], [ %gcagg485, %L1600 ]
  %.lcssa410.sink415.gcagg488 = phi i64 [ %gcagg481, %L1736.us ], [ %gcagg486, %L1600 ]
  %.lcssa410.sink415.gcagg489 = phi ptr addrspace(1) [ %gcagg482, %L1736.us ], [ %gcagg487, %L1600 ]
  %.pn418.gcagg = phi i64 [ %gcagg475, %L1736.us ], [ %gcagg472, %L1600 ]
  %.pn418.gcagg490 = phi i64 [ %gcagg476, %L1736.us ], [ %gcagg473, %L1600 ]
  %.pn418.gcagg491 = phi ptr addrspace(1) [ %gcagg477.relocated536, %L1736.us ], [ %gcagg474.relocated, %L1600 ]
  %93 = icmp slt i64 %3, 3
  br i1 %93, label %common.ret, label %L1643

L1643:                                            ; preds = %L1636
  %94 = lshr i64 %3, 1
  %95 = lshr i64 %2, 1
  %96 = icmp slt i64 %2, 3
  br i1 %96, label %L1649.us, label %L1643.split

L1649.us:                                         ; preds = %L1643, %L1649.us
  %.0320.us = phi i64 [ %97, %L1649.us ], [ 1, %L1643 ]
  %97 = add i64 %.0320.us, 1
  %98 = icmp sgt i64 %97, %94
  br i1 %98, label %common.ret, label %L1649.us

L1643.split:                                      ; preds = %L1643
  %99 = ptrtoint ptr addrspace(1) %.lcssa410.sink415.gcagg489 to i64
  %100 = and i64 %99, 1
  %.not335 = icmp eq i64 %100, 0
  br i1 %.not335, label %L1649.preheader, label %L1677

L1649.preheader:                                  ; preds = %L1643.split
  br label %L1649

L1649:                                            ; preds = %L1649.preheader, %L1716.loopexit
  %.0321 = phi i64 [ %162, %L1716.loopexit ], [ 1, %L1649.preheader ]
  %.0320 = phi i64 [ %174, %L1716.loopexit ], [ 1, %L1649.preheader ]
  %alloc.6 = phi i64 [ %alloc.9348, %L1716.loopexit ], [ %.lcssa410.sink415.gcagg488, %L1649.preheader ]
  %ds.6 = phi i64 [ %ds.9349, %L1716.loopexit ], [ %.lcssa410.sink415.gcagg, %L1649.preheader ]
  %101 = shl i64 %.0320, 1
  %102 = or i64 %101, 1
  br label %L1661

L1661:                                            ; preds = %L1684, %L1649
  %.0324 = phi i64 [ %.0321, %L1649 ], [ %162, %L1684 ]
  %.0323 = phi i64 [ 1, %L1649 ], [ %163, %L1684 ]
  %alloc.7 = phi i64 [ %alloc.6, %L1649 ], [ %alloc.9348, %L1684 ]
  %ds.7 = phi i64 [ %ds.6, %L1649 ], [ %ds.9349, %L1684 ]
  %103 = shl i64 %.0323, 1
  %104 = add i64 %102, %103
  %105 = shl i64 %104, 2
  %106 = and i64 %105, 508
  %remat511 = getelementptr i8, ptr addrspace(1) %.pn418.gcagg491, i64 -4
  %remat512 = getelementptr i8, ptr addrspace(1) %remat511, i64 %106
  %107 = load ptr addrspace(1), ptr addrspace(1) %remat512, align 8
  %remat510 = getelementptr i8, ptr addrspace(1) %.lcssa410.sink415.gcagg489, i64 8
  %108 = load ptr addrspace(1), ptr addrspace(1) %remat510, align 8
  %109 = icmp eq ptr addrspace(1) %107, %108
  br i1 %109, label %L1684, label %L1742.lr.ph

L1742.lr.ph:                                      ; preds = %L1661
  br label %L1742

L1677:                                            ; preds = %L1693, %L1690, %L1643.split
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_caml_exn_Not_found" to i64))
  unreachable

L1742:                                            ; preds = %L1742.lr.ph, %L1679.backedge
  %110 = phi ptr addrspace(1) [ %108, %L1742.lr.ph ], [ %169, %L1679.backedge ]
  %ds.8362 = phi i64 [ %ds.7, %L1742.lr.ph ], [ %ds.9, %L1679.backedge ]
  %alloc.8361 = phi i64 [ %alloc.7, %L1742.lr.ph ], [ %alloc.9, %L1679.backedge ]
  %.0325360 = phi ptr addrspace(1) [ %.lcssa410.sink415.gcagg489, %L1742.lr.ph ], [ %.0325.be, %L1679.backedge ]
  %remat513 = getelementptr i8, ptr addrspace(1) %107, i64 -8
  %111 = load atomic i64, ptr addrspace(1) %remat513 monotonic, align 8
  %112 = lshr i64 %111, 7
  %113 = and i64 %112, 562949953421304
  %114 = add nsw i64 %113, -1
  %remat515 = getelementptr i8, ptr addrspace(1) %107, i64 %114
  %115 = load i8, ptr addrspace(1) %remat515, align 1
  %116 = zext i8 %115 to i64
  %117 = sub nsw i64 %114, %116
  %remat516 = getelementptr i8, ptr addrspace(1) %110, i64 -8
  %118 = load atomic i64, ptr addrspace(1) %remat516 monotonic, align 8
  %119 = lshr i64 %118, 7
  %120 = and i64 %119, 562949953421304
  %121 = add nsw i64 %120, -1
  %remat517 = getelementptr i8, ptr addrspace(1) %110, i64 %121
  %122 = load i8, ptr addrspace(1) %remat517, align 1
  %123 = zext i8 %122 to i64
  %124 = sub nsw i64 %121, %123
  %125 = icmp ult i64 %117, %124
  %126 = tail call i64 @llvm.umin.i64(i64 %117, i64 %124)
  %127 = icmp ugt i64 %126, 15
  br i1 %127, label %L1743, label %L1744

L1743:                                            ; preds = %L1742
  %128 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.8362, i64 %alloc.8361, ptr addrspace(1) %107, ptr addrspace(1) %110) #6
  %gcagg492 = extractvalue { i64, i64, ptr addrspace(1) } %128, 0
  %gcagg493 = extractvalue { i64, i64, ptr addrspace(1) } %128, 1
  %gcagg494 = extractvalue { i64, i64, ptr addrspace(1) } %128, 2
  br label %L1681

L1744:                                            ; preds = %L1742
  %129 = icmp eq i64 %126, 0
  br i1 %129, label %L1745, label %L1746

L1746:                                            ; preds = %L1744
  %130 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %126)
  %131 = shl nuw nsw i64 %130, 3
  %132 = shl nsw i64 -1, %131
  %133 = load i64, ptr addrspace(1) %107, align 8
  %134 = tail call i64 @llvm.bswap.i64(i64 %133)
  %135 = load i64, ptr addrspace(1) %110, align 8
  %136 = tail call i64 @llvm.bswap.i64(i64 %135)
  %137 = and i64 %134, %132
  %138 = and i64 %136, %132
  %.not336 = icmp eq i64 %137, %138
  br i1 %.not336, label %L1748, label %L1747

L1747:                                            ; preds = %L1746
  %139 = icmp ult i64 %137, %138
  %140 = select i1 %139, i64 -1, i64 3
  %141 = inttoptr i64 %140 to ptr addrspace(1)
  br label %L1681

L1748:                                            ; preds = %L1746
  %142 = icmp ugt i64 %126, 8
  br i1 %142, label %L1749, label %L1745

L1749:                                            ; preds = %L1748
  %143 = shl nuw nsw i64 %126, 3
  %144 = sub nuw nsw i64 128, %143
  %145 = shl nsw i64 -1, %144
  %remat514 = getelementptr i8, ptr addrspace(1) %107, i64 8
  %146 = load i64, ptr addrspace(1) %remat514, align 8
  %147 = tail call i64 @llvm.bswap.i64(i64 %146)
  %remat518 = getelementptr i8, ptr addrspace(1) %110, i64 8
  %148 = load i64, ptr addrspace(1) %remat518, align 8
  %149 = tail call i64 @llvm.bswap.i64(i64 %148)
  %150 = and i64 %147, %145
  %151 = and i64 %149, %145
  %.not341 = icmp eq i64 %150, %151
  br i1 %.not341, label %L1745, label %L1750

L1750:                                            ; preds = %L1749
  %152 = icmp ult i64 %150, %151
  %153 = select i1 %152, i64 -1, i64 3
  %154 = inttoptr i64 %153 to ptr addrspace(1)
  br label %L1681

L1745:                                            ; preds = %L1749, %L1748, %L1744
  %155 = icmp ugt i64 %117, %124
  %156 = select i1 %155, i64 3, i64 1
  %157 = select i1 %125, i64 -1, i64 %156
  %158 = inttoptr i64 %157 to ptr addrspace(1)
  br label %L1681

L1681:                                            ; preds = %L1743, %L1747, %L1750, %L1745
  %.0 = phi ptr addrspace(1) [ %gcagg494, %L1743 ], [ %158, %L1745 ], [ %141, %L1747 ], [ %154, %L1750 ]
  %alloc.9 = phi i64 [ %gcagg493, %L1743 ], [ %alloc.8361, %L1745 ], [ %alloc.8361, %L1747 ], [ %alloc.8361, %L1750 ]
  %ds.9 = phi i64 [ %gcagg492, %L1743 ], [ %ds.8362, %L1745 ], [ %ds.8362, %L1747 ], [ %ds.8362, %L1750 ]
  %or.cond8.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond8.not, label %L1684, label %L1688

L1684:                                            ; preds = %L1681, %L1679.backedge, %L1661
  %.0325.lcssa = phi ptr addrspace(1) [ %.lcssa410.sink415.gcagg489, %L1661 ], [ %.0325.be, %L1679.backedge ], [ %.0325360, %L1681 ]
  %ds.9349 = phi i64 [ %ds.7, %L1661 ], [ %ds.9, %L1679.backedge ], [ %ds.9, %L1681 ]
  %alloc.9348 = phi i64 [ %alloc.7, %L1661 ], [ %alloc.9, %L1679.backedge ], [ %alloc.9, %L1681 ]
  %remat519 = getelementptr i8, ptr addrspace(1) %.0325.lcssa, i64 16
  %159 = load ptr addrspace(1), ptr addrspace(1) %remat519, align 8
  %160 = ptrtoint ptr addrspace(1) %159 to i64
  %161 = add i64 %.0324, -1
  %162 = add i64 %161, %160
  %163 = add i64 %.0323, 1
  %164 = icmp sgt i64 %163, %95
  br i1 %164, label %L1716.loopexit, label %L1661

L1688:                                            ; preds = %L1681
  %165 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %165, label %L1690, label %L1693

L1690:                                            ; preds = %L1688
  %166 = load ptr addrspace(1), ptr addrspace(1) %.0325360, align 8
  %167 = ptrtoint ptr addrspace(1) %166 to i64
  %168 = and i64 %167, 1
  %.not340 = icmp eq i64 %168, 0
  br i1 %.not340, label %L1679.backedge, label %L1677

L1679.backedge:                                   ; preds = %L1690, %L1693
  %.0325.be = phi ptr addrspace(1) [ %166, %L1690 ], [ %171, %L1693 ]
  %remat520 = getelementptr i8, ptr addrspace(1) %.0325.be, i64 8
  %169 = load ptr addrspace(1), ptr addrspace(1) %remat520, align 8
  %170 = icmp eq ptr addrspace(1) %107, %169
  br i1 %170, label %L1684, label %L1742

L1693:                                            ; preds = %L1688
  %remat521 = getelementptr i8, ptr addrspace(1) %.0325360, i64 24
  %171 = load ptr addrspace(1), ptr addrspace(1) %remat521, align 8
  %172 = ptrtoint ptr addrspace(1) %171 to i64
  %173 = and i64 %172, 1
  %.not339 = icmp eq i64 %173, 0
  br i1 %.not339, label %L1679.backedge, label %L1677

L1716.loopexit:                                   ; preds = %L1684
  %174 = add i64 %.0320, 1
  %175 = icmp sgt i64 %174, %94
  br i1 %175, label %common.ret, label %L1649

common.ret:                                       ; preds = %L1716.loopexit, %L1649.us, %L1636
  %ds.10.pn = phi i64 [ %.lcssa410.sink415.gcagg, %L1636 ], [ %.lcssa410.sink415.gcagg, %L1649.us ], [ %ds.9349, %L1716.loopexit ]
  %alloc.10.pn = phi i64 [ %.lcssa410.sink415.gcagg488, %L1636 ], [ %.lcssa410.sink415.gcagg488, %L1649.us ], [ %alloc.9348, %L1716.loopexit ]
  %.0322.pn = phi i64 [ 1, %L1636 ], [ 1, %L1649.us ], [ %162, %L1716.loopexit ]
  %.pn337 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.10.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn337, i64 %alloc.10.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0322.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
