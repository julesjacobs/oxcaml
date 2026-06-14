define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_map_equal_content__run_6_52_code"(i64 %0, i64 %1, i64 %2, i64 %3) #0 gc "oxcaml" {
L1:
  %4 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #9 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 19, i64 0, i64 63, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
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
  %19 = add nsw i64 %17, 39
  %20 = inttoptr i64 %19 to ptr addrspace(1)
  %21 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %5, i64 %6, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull %20) #9 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 19, i64 0, i64 39, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %22 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 0, 0
  %23 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 0, 1
  %24 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %21, 1, 0
  %25 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %22, i64 %23, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlString_map_equal_content__immstring119" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %24, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1))) #6
  %26 = extractvalue { i64, i64, ptr addrspace(1) } %25, 0
  %27 = extractvalue { i64, i64, ptr addrspace(1) } %25, 1
  %28 = inttoptr i64 %18 to ptr addrspace(1)
  %29 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %26, i64 %27, ptr addrspace(1) %7, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %24, ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1)), ptr addrspace(1) nonnull %28) #6
  %30 = extractvalue { i64, i64, ptr addrspace(1) } %29, 0
  %31 = extractvalue { i64, i64, ptr addrspace(1) } %29, 1
  %32 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %30, i64 %31, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) nonnull inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %24) #9 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %33 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 1
  %35 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 1, 0
  %36 = add i64 %33, 40
  %37 = inttoptr i64 %36 to ptr
  %38 = load i64, ptr %37, align 4
  %39 = add i64 %38, 376
  %40 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #7
  %.not = icmp ult i64 %40, %39
  br i1 %.not, label %L1731, label %L1732, !prof !1

L1731:                                            ; preds = %L1
  %41 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %33, i64 %34, i64 34) #8
  %42 = extractvalue { { i64, i64 }, {} } %41, 0, 0
  %43 = extractvalue { { i64, i64 }, {} } %41, 0, 1
  br label %L1732

L1732:                                            ; preds = %L1731, %L1
  %alloc.0 = phi i64 [ %34, %L1 ], [ %43, %L1731 ]
  %ds.0 = phi i64 [ %33, %L1 ], [ %42, %L1731 ]
  %44 = getelementptr i8, ptr addrspace(1) %35, i64 -8
  %45 = getelementptr i8, ptr addrspace(1) %35, i64 -4
  %46 = getelementptr i8, ptr addrspace(1) %35, i64 -4
  br label %L1544

L1544:                                            ; preds = %L1544.backedge, %L1732
  %.0315 = phi i64 [ 1, %L1732 ], [ %.0315.be, %L1544.backedge ]
  %alloc.1 = phi i64 [ %alloc.0, %L1732 ], [ %alloc.1.be, %L1544.backedge ]
  %ds.1 = phi i64 [ %ds.0, %L1732 ], [ %ds.1.be, %L1544.backedge ]
  %47 = shl nuw i64 %.0315, 1
  %48 = or i64 %47, 1
  %49 = inttoptr i64 %48 to ptr addrspace(1)
  %50 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %ds.1, i64 %alloc.1, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull %49) #9 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 19, i64 0, i64 63, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %51 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %50, 0, 0
  %52 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %50, 0, 1
  %53 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %50, 1, 0
  %54 = getelementptr i8, ptr addrspace(1) %53, i64 -8
  %55 = load i64, ptr addrspace(1) %54, align 4
  %56 = lshr i64 %55, 7
  %57 = and i64 %56, 562949953421304
  %58 = add nsw i64 %57, -1
  %59 = getelementptr i8, ptr addrspace(1) %53, i64 %58
  %60 = load i8, ptr addrspace(1) %59, align 1
  %61 = zext i8 %60 to i64
  %62 = sub nsw i64 %58, %61
  %63 = shl nsw i64 %62, 1
  %64 = or i64 %63, 1
  %65 = add nsw i64 %63, 39
  %66 = inttoptr i64 %65 to ptr addrspace(1)
  %67 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %51, i64 %52, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull %66) #9 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 19, i64 0, i64 39, i64 78, i64 0, i64 78, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 19, i64 0, i64 15, i64 79, i64 0, i64 79, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %68 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %67, 0, 0
  %69 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %67, 0, 1
  %70 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %67, 1, 0
  %71 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %68, i64 %69, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlString_map_equal_content__immstring119" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %70, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1))) #6
  %72 = extractvalue { i64, i64, ptr addrspace(1) } %71, 0
  %73 = extractvalue { i64, i64, ptr addrspace(1) } %71, 1
  %74 = inttoptr i64 %64 to ptr addrspace(1)
  %75 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %72, i64 %73, ptr addrspace(1) %53, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %70, ptr addrspace(1) nonnull inttoptr (i64 39 to ptr addrspace(1)), ptr addrspace(1) nonnull %74) #6
  %76 = extractvalue { i64, i64, ptr addrspace(1) } %75, 0
  %77 = extractvalue { i64, i64, ptr addrspace(1) } %75, 1
  %78 = load i8, ptr addrspace(1) %44, align 1
  %or.cond.not.not = icmp eq i8 %78, -2
  %79 = shl i64 %48, 2
  br i1 %or.cond.not.not, label %L1581, label %L1581.thread

L1581:                                            ; preds = %L1544
  %80 = getelementptr i8, ptr addrspace(1) %46, i64 %79
  %81 = load double, ptr addrspace(1) %70, align 8
  store double %81, ptr addrspace(1) %80, align 8
  %82 = add nuw nsw i64 %.0315, 1
  %exitcond = icmp eq i64 %82, 64
  br i1 %exitcond, label %L1600.us.preheader, label %L1544.backedge

L1544.backedge:                                   ; preds = %L1581, %L1581.thread
  %.0315.be = phi i64 [ %82, %L1581 ], [ %87, %L1581.thread ]
  %alloc.1.be = phi i64 [ %77, %L1581 ], [ %86, %L1581.thread ]
  %ds.1.be = phi i64 [ %76, %L1581 ], [ %85, %L1581.thread ]
  br label %L1544

L1581.thread:                                     ; preds = %L1544
  %83 = getelementptr i8, ptr addrspace(1) %45, i64 %79
  %84 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %76, i64 %77, ptr addrspace(1) %83, ptr addrspace(1) %70) #6
  %85 = extractvalue { i64, i64 } %84, 0
  %86 = extractvalue { i64, i64 } %84, 1
  %87 = add nuw nsw i64 %.0315, 1
  %exitcond383 = icmp eq i64 %87, 64
  br i1 %exitcond383, label %L1600.preheader, label %L1544.backedge

L1600.preheader:                                  ; preds = %L1581.thread
  %88 = extractvalue { i64, i64 } %84, 0
  %89 = extractvalue { i64, i64 } %84, 1
  %90 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlStdlib__Array__map_10_80_code"(i64 %88, i64 %89, i64 ptrtoint (ptr @"\01_camlString_map_equal_content__fresh_46" to i64), ptr addrspace(1) %35) #9 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 20, i64 0, i64 15, i64 37, i64 0, i64 37, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %91 = getelementptr i8, ptr addrspace(1) %35, i64 -4
  br label %L1600

L1600.us.preheader:                               ; preds = %L1581
  %92 = extractvalue { i64, i64, ptr addrspace(1) } %75, 0
  %93 = extractvalue { i64, i64, ptr addrspace(1) } %75, 1
  %94 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlStdlib__Array__map_10_80_code"(i64 %92, i64 %93, i64 ptrtoint (ptr @"\01_camlString_map_equal_content__fresh_46" to i64), ptr addrspace(1) %35) #9 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 20, i64 0, i64 15, i64 37, i64 0, i64 37, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %95 = getelementptr i8, ptr addrspace(1) %35, i64 -4
  br label %L1600.us

L1600.us:                                         ; preds = %L1600.us.preheader, %L1736.us
  %.0318.us = phi ptr addrspace(1) [ %115, %L1736.us ], [ inttoptr (i64 1 to ptr addrspace(1)), %L1600.us.preheader ]
  %.0317.us = phi i64 [ %116, %L1736.us ], [ 0, %L1600.us.preheader ]
  %.pn419 = phi { { i64, i64 }, { ptr addrspace(1) } } [ %114, %L1736.us ], [ %94, %L1600.us.preheader ]
  %ds.3.us = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn419, 0, 0
  %alloc.3.us = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn419, 0, 1
  %96 = shl i64 %.0317.us, 3
  %97 = or i64 %96, 4
  %98 = add i64 %alloc.3.us, -16
  %99 = inttoptr i64 %ds.3.us to ptr
  %100 = load i64, ptr %99, align 4
  %.not342.us = icmp ugt i64 %100, %98
  br i1 %.not342.us, label %L1735.us, label %L1736.us, !prof !1

L1735.us:                                         ; preds = %L1600.us
  %101 = call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_call_gc"(i64 %ds.3.us, i64 %98) #10 [ "deopt"(i64 1870160737, i64 1, i64 1, i64 2, i64 2, i64 222, i64 0, i64 14, i64 30, i64 0, i64 30, i64 8, i64 7500385, i64 3045729, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6696569, i64 6581359, i64 6646879, i64 29798, i64 22, i64 0, i64 4, i64 75, i64 0, i64 75, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %102 = extractvalue { { i64, i64 }, {} } %101, 0, 0
  %103 = extractvalue { { i64, i64 }, {} } %101, 0, 1
  br label %L1736.us

L1736.us:                                         ; preds = %L1735.us, %L1600.us
  %alloc.4.us = phi i64 [ %98, %L1600.us ], [ %103, %L1735.us ]
  %ds.4.us = phi i64 [ %ds.3.us, %L1600.us ], [ %102, %L1735.us ]
  %104 = add i64 %alloc.4.us, 8
  %105 = inttoptr i64 %104 to ptr addrspace(1)
  %106 = getelementptr i8, ptr addrspace(1) %105, i64 -8
  store volatile i64 1277, ptr addrspace(1) %106, align 4
  %107 = getelementptr i8, ptr addrspace(1) %95, i64 %97
  %108 = load double, ptr addrspace(1) %107, align 8
  store double %108, ptr addrspace(1) %105, align 8
  %109 = bitcast double %108 to i64
  %110 = lshr i64 %109, 55
  %111 = and i64 %110, 510
  %112 = sub nsw i64 15, %111
  %113 = inttoptr i64 %112 to ptr addrspace(1)
  %114 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_map_equal_content__add_6_32_code"(i64 %ds.4.us, i64 %alloc.4.us, ptr addrspace(1) %105, ptr addrspace(1) nonnull %113, ptr addrspace(1) %.0318.us) #9 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 3, i64 22, i64 0, i64 32, i64 59, i64 0, i64 59, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 222, i64 0, i64 9, i64 30, i64 0, i64 30, i64 8, i64 7500385, i64 3045729, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6696569, i64 6581359, i64 6646879, i64 29798, i64 22, i64 0, i64 4, i64 75, i64 0, i64 75, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %115 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %114, 1, 0
  %116 = add nuw nsw i64 %.0317.us, 1
  %exitcond378 = icmp eq i64 %116, 64
  br i1 %exitcond378, label %L1636, label %L1600.us

L1600:                                            ; preds = %L1600.preheader, %L1600
  %.0318 = phi ptr addrspace(1) [ %134, %L1600 ], [ inttoptr (i64 1 to ptr addrspace(1)), %L1600.preheader ]
  %.0317 = phi i64 [ %135, %L1600 ], [ 0, %L1600.preheader ]
  %.pn416 = phi { { i64, i64 }, { ptr addrspace(1) } } [ %133, %L1600 ], [ %90, %L1600.preheader ]
  %ds.3 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn416, 0, 0
  %alloc.3 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn416, 0, 1
  %117 = shl i64 %.0317, 3
  %118 = or i64 %117, 4
  %119 = getelementptr i8, ptr addrspace(1) %91, i64 %118
  %120 = load ptr addrspace(1), ptr addrspace(1) %119, align 8
  %121 = getelementptr i8, ptr addrspace(1) %120, i64 -8
  %122 = load i64, ptr addrspace(1) %121, align 4
  %123 = lshr i64 %122, 7
  %124 = and i64 %123, 562949953421304
  %125 = add nsw i64 %124, -1
  %126 = getelementptr i8, ptr addrspace(1) %120, i64 %125
  %127 = load i8, ptr addrspace(1) %126, align 1
  %128 = zext i8 %127 to i64
  %129 = sub nsw i64 %125, %128
  %130 = shl nsw i64 %129, 1
  %131 = or i64 %130, 1
  %132 = inttoptr i64 %131 to ptr addrspace(1)
  %133 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_map_equal_content__add_6_32_code"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %120, ptr addrspace(1) nonnull %132, ptr addrspace(1) %.0318) #9 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 3, i64 22, i64 0, i64 32, i64 59, i64 0, i64 59, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 34, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 2633326, i64 7239014, i64 41, i64 222, i64 0, i64 9, i64 30, i64 0, i64 30, i64 8, i64 7500385, i64 3045729, i64 27757, i64 23, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6696569, i64 6581359, i64 6646879, i64 29798, i64 22, i64 0, i64 4, i64 75, i64 0, i64 75, i64 27, i64 7500915, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7105838, i64 28, i64 7500883, i64 6778473, i64 6384991, i64 6643568, i64 6387057, i64 6512492, i64 7630447, i64 7630437, i64 7696942, i64 110) ]
  %134 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %133, 1, 0
  %135 = add nuw nsw i64 %.0317, 1
  %exitcond377 = icmp eq i64 %135, 64
  br i1 %exitcond377, label %L1636, label %L1600

L1636:                                            ; preds = %L1600, %L1736.us
  %.lcssa410.sink415 = phi { { i64, i64 }, { ptr addrspace(1) } } [ %114, %L1736.us ], [ %133, %L1600 ]
  %.pn418 = phi { { i64, i64 }, { ptr addrspace(1) } } [ %94, %L1736.us ], [ %90, %L1600 ]
  %136 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.pn418, 1, 0
  %137 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.lcssa410.sink415, 0, 0
  %138 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.lcssa410.sink415, 0, 1
  %139 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %.lcssa410.sink415, 1, 0
  %140 = icmp slt i64 %3, 3
  br i1 %140, label %common.ret, label %L1643

L1643:                                            ; preds = %L1636
  %141 = lshr i64 %3, 1
  %142 = icmp slt i64 %2, 3
  %143 = lshr i64 %2, 1
  %144 = getelementptr i8, ptr addrspace(1) %139, i64 8
  br i1 %142, label %L1649.us, label %L1643.split

L1649.us:                                         ; preds = %L1643, %L1649.us
  %.0320.us = phi i64 [ %145, %L1649.us ], [ 1, %L1643 ]
  %145 = add i64 %.0320.us, 1
  %146 = icmp sgt i64 %145, %141
  br i1 %146, label %common.ret, label %L1649.us

L1643.split:                                      ; preds = %L1643
  %147 = ptrtoint ptr addrspace(1) %139 to i64
  %148 = and i64 %147, 1
  %.not335 = icmp eq i64 %148, 0
  br i1 %.not335, label %L1649.preheader, label %L1677

L1649.preheader:                                  ; preds = %L1643.split
  %149 = getelementptr i8, ptr addrspace(1) %136, i64 -4
  br label %L1649

L1649:                                            ; preds = %L1649.preheader, %L1716.loopexit
  %.0321 = phi i64 [ %222, %L1716.loopexit ], [ 1, %L1649.preheader ]
  %.0320 = phi i64 [ %236, %L1716.loopexit ], [ 1, %L1649.preheader ]
  %alloc.6 = phi i64 [ %alloc.9348, %L1716.loopexit ], [ %138, %L1649.preheader ]
  %ds.6 = phi i64 [ %ds.9349, %L1716.loopexit ], [ %137, %L1649.preheader ]
  %150 = shl i64 %.0320, 1
  %151 = or i64 %150, 1
  br label %L1661

L1661:                                            ; preds = %L1684, %L1649
  %.0324 = phi i64 [ %.0321, %L1649 ], [ %222, %L1684 ]
  %.0323 = phi i64 [ 1, %L1649 ], [ %223, %L1684 ]
  %alloc.7 = phi i64 [ %alloc.6, %L1649 ], [ %alloc.9348, %L1684 ]
  %ds.7 = phi i64 [ %ds.6, %L1649 ], [ %ds.9349, %L1684 ]
  %152 = shl i64 %.0323, 1
  %153 = add i64 %151, %152
  %154 = shl i64 %153, 2
  %155 = and i64 %154, 508
  %156 = getelementptr i8, ptr addrspace(1) %149, i64 %155
  %157 = load ptr addrspace(1), ptr addrspace(1) %156, align 8
  %158 = load ptr addrspace(1), ptr addrspace(1) %144, align 8
  %159 = icmp eq ptr addrspace(1) %157, %158
  br i1 %159, label %L1684, label %L1742.lr.ph

L1742.lr.ph:                                      ; preds = %L1661
  %160 = getelementptr i8, ptr addrspace(1) %157, i64 -8
  %161 = getelementptr i8, ptr addrspace(1) %157, i64 8
  br label %L1742

L1677:                                            ; preds = %L1693, %L1690, %L1643.split
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 ptrtoint (ptr @"\01_caml_exn_Not_found" to i64))
  unreachable

L1742:                                            ; preds = %L1742.lr.ph, %L1679.backedge
  %162 = phi ptr addrspace(1) [ %158, %L1742.lr.ph ], [ %230, %L1679.backedge ]
  %ds.8362 = phi i64 [ %ds.7, %L1742.lr.ph ], [ %ds.9, %L1679.backedge ]
  %alloc.8361 = phi i64 [ %alloc.7, %L1742.lr.ph ], [ %alloc.9, %L1679.backedge ]
  %.0325360 = phi ptr addrspace(1) [ %139, %L1742.lr.ph ], [ %.0325.be, %L1679.backedge ]
  %163 = load atomic i64, ptr addrspace(1) %160 monotonic, align 8
  %164 = lshr i64 %163, 7
  %165 = and i64 %164, 562949953421304
  %166 = add nsw i64 %165, -1
  %167 = getelementptr i8, ptr addrspace(1) %157, i64 %166
  %168 = load i8, ptr addrspace(1) %167, align 1
  %169 = zext i8 %168 to i64
  %170 = sub nsw i64 %166, %169
  %171 = getelementptr i8, ptr addrspace(1) %162, i64 -8
  %172 = load atomic i64, ptr addrspace(1) %171 monotonic, align 8
  %173 = lshr i64 %172, 7
  %174 = and i64 %173, 562949953421304
  %175 = add nsw i64 %174, -1
  %176 = getelementptr i8, ptr addrspace(1) %162, i64 %175
  %177 = load i8, ptr addrspace(1) %176, align 1
  %178 = zext i8 %177 to i64
  %179 = sub nsw i64 %175, %178
  %180 = icmp ult i64 %170, %179
  %181 = tail call i64 @llvm.umin.i64(i64 %170, i64 %179)
  %182 = icmp ugt i64 %181, 15
  br i1 %182, label %L1743, label %L1744

L1743:                                            ; preds = %L1742
  %183 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_string_compare"(i64 %ds.8362, i64 %alloc.8361, ptr addrspace(1) %157, ptr addrspace(1) %162) #6
  %184 = extractvalue { i64, i64, ptr addrspace(1) } %183, 0
  %185 = extractvalue { i64, i64, ptr addrspace(1) } %183, 1
  %186 = extractvalue { i64, i64, ptr addrspace(1) } %183, 2
  br label %L1681

L1744:                                            ; preds = %L1742
  %187 = icmp eq i64 %181, 0
  br i1 %187, label %L1745, label %L1746

L1746:                                            ; preds = %L1744
  %188 = tail call i64 @llvm.usub.sat.i64(i64 8, i64 %181)
  %189 = shl nuw nsw i64 %188, 3
  %190 = shl nsw i64 -1, %189
  %191 = load i64, ptr addrspace(1) %157, align 8
  %192 = tail call i64 @llvm.bswap.i64(i64 %191)
  %193 = load i64, ptr addrspace(1) %162, align 8
  %194 = tail call i64 @llvm.bswap.i64(i64 %193)
  %195 = and i64 %192, %190
  %196 = and i64 %194, %190
  %.not336 = icmp eq i64 %195, %196
  br i1 %.not336, label %L1748, label %L1747

L1747:                                            ; preds = %L1746
  %197 = icmp ult i64 %195, %196
  %198 = select i1 %197, i64 -1, i64 3
  %199 = inttoptr i64 %198 to ptr addrspace(1)
  br label %L1681

L1748:                                            ; preds = %L1746
  %200 = icmp ugt i64 %181, 8
  br i1 %200, label %L1749, label %L1745

L1749:                                            ; preds = %L1748
  %201 = shl nuw nsw i64 %181, 3
  %202 = sub nuw nsw i64 128, %201
  %203 = shl nsw i64 -1, %202
  %204 = load i64, ptr addrspace(1) %161, align 8
  %205 = tail call i64 @llvm.bswap.i64(i64 %204)
  %206 = getelementptr i8, ptr addrspace(1) %162, i64 8
  %207 = load i64, ptr addrspace(1) %206, align 8
  %208 = tail call i64 @llvm.bswap.i64(i64 %207)
  %209 = and i64 %205, %203
  %210 = and i64 %208, %203
  %.not341 = icmp eq i64 %209, %210
  br i1 %.not341, label %L1745, label %L1750

L1750:                                            ; preds = %L1749
  %211 = icmp ult i64 %209, %210
  %212 = select i1 %211, i64 -1, i64 3
  %213 = inttoptr i64 %212 to ptr addrspace(1)
  br label %L1681

L1745:                                            ; preds = %L1749, %L1748, %L1744
  %214 = icmp ugt i64 %170, %179
  %215 = select i1 %214, i64 3, i64 1
  %216 = select i1 %180, i64 -1, i64 %215
  %217 = inttoptr i64 %216 to ptr addrspace(1)
  br label %L1681

L1681:                                            ; preds = %L1743, %L1747, %L1750, %L1745
  %.0 = phi ptr addrspace(1) [ %186, %L1743 ], [ %217, %L1745 ], [ %199, %L1747 ], [ %213, %L1750 ]
  %alloc.9 = phi i64 [ %185, %L1743 ], [ %alloc.8361, %L1745 ], [ %alloc.8361, %L1747 ], [ %alloc.8361, %L1750 ]
  %ds.9 = phi i64 [ %184, %L1743 ], [ %ds.8362, %L1745 ], [ %ds.8362, %L1747 ], [ %ds.8362, %L1750 ]
  %or.cond8.not = icmp eq ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %or.cond8.not, label %L1684, label %L1688

L1684:                                            ; preds = %L1681, %L1679.backedge, %L1661
  %.0325.lcssa = phi ptr addrspace(1) [ %139, %L1661 ], [ %.0325.be, %L1679.backedge ], [ %.0325360, %L1681 ]
  %ds.9349 = phi i64 [ %ds.7, %L1661 ], [ %ds.9, %L1679.backedge ], [ %ds.9, %L1681 ]
  %alloc.9348 = phi i64 [ %alloc.7, %L1661 ], [ %alloc.9, %L1679.backedge ], [ %alloc.9, %L1681 ]
  %218 = getelementptr i8, ptr addrspace(1) %.0325.lcssa, i64 16
  %219 = load ptr addrspace(1), ptr addrspace(1) %218, align 8
  %220 = ptrtoint ptr addrspace(1) %219 to i64
  %221 = add i64 %.0324, -1
  %222 = add i64 %221, %220
  %223 = add i64 %.0323, 1
  %224 = icmp sgt i64 %223, %143
  br i1 %224, label %L1716.loopexit, label %L1661

L1688:                                            ; preds = %L1681
  %225 = icmp slt ptr addrspace(1) %.0, inttoptr (i64 1 to ptr addrspace(1))
  br i1 %225, label %L1690, label %L1693

L1690:                                            ; preds = %L1688
  %226 = load ptr addrspace(1), ptr addrspace(1) %.0325360, align 8
  %227 = ptrtoint ptr addrspace(1) %226 to i64
  %228 = and i64 %227, 1
  %.not340 = icmp eq i64 %228, 0
  br i1 %.not340, label %L1679.backedge, label %L1677

L1679.backedge:                                   ; preds = %L1690, %L1693
  %.0325.be = phi ptr addrspace(1) [ %226, %L1690 ], [ %233, %L1693 ]
  %229 = getelementptr i8, ptr addrspace(1) %.0325.be, i64 8
  %230 = load ptr addrspace(1), ptr addrspace(1) %229, align 8
  %231 = icmp eq ptr addrspace(1) %157, %230
  br i1 %231, label %L1684, label %L1742

L1693:                                            ; preds = %L1688
  %232 = getelementptr i8, ptr addrspace(1) %.0325360, i64 24
  %233 = load ptr addrspace(1), ptr addrspace(1) %232, align 8
  %234 = ptrtoint ptr addrspace(1) %233 to i64
  %235 = and i64 %234, 1
  %.not339 = icmp eq i64 %235, 0
  br i1 %.not339, label %L1679.backedge, label %L1677

L1716.loopexit:                                   ; preds = %L1684
  %236 = add i64 %.0320, 1
  %237 = icmp sgt i64 %236, %141
  br i1 %237, label %common.ret, label %L1649

common.ret:                                       ; preds = %L1716.loopexit, %L1649.us, %L1636
  %ds.10.pn = phi i64 [ %137, %L1636 ], [ %137, %L1649.us ], [ %ds.9349, %L1716.loopexit ]
  %alloc.10.pn = phi i64 [ %138, %L1636 ], [ %138, %L1649.us ], [ %alloc.9348, %L1716.loopexit ]
  %.0322.pn = phi i64 [ 1, %L1636 ], [ 1, %L1649.us ], [ %222, %L1716.loopexit ]
  %.pn337 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.10.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn337, i64 %alloc.10.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0322.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
