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
  %9 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %0, i64 %1, i64 34) #6
  %10 = extractvalue { { i64, i64 }, {} } %9, 0, 0
  %11 = extractvalue { { i64, i64 }, {} } %9, 0, 1
  br label %L390

L390:                                             ; preds = %L389, %L1
  %alloc.0 = phi i64 [ %1, %L1 ], [ %11, %L389 ]
  %ds.0 = phi i64 [ %0, %L1 ], [ %10, %L389 ]
  %12 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__build_5_13_code"(i64 %ds.0, i64 %alloc.0, i64 1, i64 127) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 32, i64 0, i64 13, i64 23, i64 0, i64 23, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %13 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 0
  %14 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 0, 1
  %15 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %13, i64 %14, i64 ptrtoint (ptr @"\01_caml_format_int" to i64), ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlStdlib__immstring191" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 280, i64 0, i64 2, i64 19, i64 0, i64 19, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 20, i64 6583379, i64 6449516, i64 7631662, i64 7235954, i64 7298919, i64 6905702, i64 29806, i64 17, i64 0, i64 67, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %16 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %15, 0, 0
  %17 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %15, 0, 1
  %18 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %15, 1, 0
  %19 = getelementptr i8, ptr addrspace(1) %18, i64 -8
  %20 = load i64, ptr addrspace(1) %19, align 4
  %21 = lshr i64 %20, 7
  %22 = and i64 %21, 562949953421304
  %23 = add nsw i64 %22, -1
  %24 = getelementptr i8, ptr addrspace(1) %18, i64 %23
  %25 = load i8, ptr addrspace(1) %24, align 1
  %26 = zext i8 %25 to i64
  %27 = sub nsw i64 %23, %26
  %28 = shl nsw i64 %27, 1
  %29 = or i64 %28, 1
  %30 = add nsw i64 %28, 29
  %31 = inttoptr i64 %30 to ptr addrspace(1)
  %32 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %16, i64 %17, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull %31) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 48, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %33 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 0
  %34 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 0, 1
  %35 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %32, 1, 0
  %36 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %33, i64 %34, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlString_tree_first_byte_diff__immstring83" to i64) to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %35, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1))) #8
  %37 = extractvalue { i64, i64, ptr addrspace(1) } %36, 0
  %38 = extractvalue { i64, i64, ptr addrspace(1) } %36, 1
  %39 = inttoptr i64 %29 to ptr addrspace(1)
  %40 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %37, i64 %38, ptr addrspace(1) %18, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %35, ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1)), ptr addrspace(1) nonnull %39) #8
  %41 = extractvalue { i64, i64, ptr addrspace(1) } %40, 0
  %42 = extractvalue { i64, i64, ptr addrspace(1) } %40, 1
  %43 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %41, i64 %42, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull inttoptr (i64 3 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 5, i64 60, i64 0, i64 10, i64 18, i64 0, i64 18, i64 8, i64 7633250, i64 3044197, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4349791, i64 6648953, i64 7155315, i64 6646625, i64 44, i64 0, i64 2, i64 12, i64 0, i64 12, i64 9, i64 7500915, i64 6778473, i64 7105838, i64 19, i64 6583379, i64 6449516, i64 5463903, i64 6910580, i64 3041134, i64 7037293, i64 101, i64 17, i64 0, i64 2, i64 45, i64 0, i64 45, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %44 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %43, 0, 0
  %45 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %43, 0, 1
  %46 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %43, 1, 0
  %47 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_fill_bytes"(i64 %44, i64 %45, ptr addrspace(1) %46, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 3 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 131 to ptr addrspace(1))) #8
  %48 = extractvalue { i64, i64, ptr addrspace(1) } %47, 0
  %49 = extractvalue { i64, i64, ptr addrspace(1) } %47, 1
  %50 = getelementptr i8, ptr addrspace(1) %46, i64 -8
  %51 = load i64, ptr addrspace(1) %50, align 4
  %52 = lshr i64 %51, 7
  %53 = and i64 %52, 562949953421304
  %54 = add nsw i64 %53, -1
  %55 = getelementptr i8, ptr addrspace(1) %46, i64 %54
  %56 = load i8, ptr addrspace(1) %55, align 1
  %57 = zext i8 %56 to i64
  %58 = sub nsw i64 %54, %57
  %59 = shl nsw i64 %58, 1
  %60 = or i64 %59, 1
  %61 = getelementptr i8, ptr addrspace(1) %35, i64 -8
  %62 = load i64, ptr addrspace(1) %61, align 4
  %63 = lshr i64 %62, 7
  %64 = and i64 %63, 562949953421304
  %65 = add nsw i64 %64, -1
  %66 = getelementptr i8, ptr addrspace(1) %35, i64 %65
  %67 = load i8, ptr addrspace(1) %66, align 1
  %68 = zext i8 %67 to i64
  %69 = sub nsw i64 %65, %68
  %70 = shl nsw i64 %69, 1
  %71 = or i64 %70, 1
  %72 = add nsw i64 %59, %71
  %73 = inttoptr i64 %72 to ptr addrspace(1)
  %74 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %48, i64 %49, i64 ptrtoint (ptr @"\01_caml_create_bytes" to i64), ptr addrspace(1) nonnull %73) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 4, i64 226, i64 0, i64 10, i64 32, i64 0, i64 32, i64 9, i64 6583411, i64 6449516, i64 7105838, i64 10, i64 6583379, i64 6449516, i64 6170670, i64 41, i64 17, i64 0, i64 2, i64 82, i64 0, i64 82, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 6646574, i64 121, i64 85, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %75 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %74, 0, 0
  %76 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %74, 0, 1
  %77 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %74, 1, 0
  %78 = inttoptr i64 %60 to ptr addrspace(1)
  %79 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %75, i64 %76, ptr addrspace(1) %46, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %77, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) nonnull %78) #8
  %80 = extractvalue { i64, i64, ptr addrspace(1) } %79, 0
  %81 = extractvalue { i64, i64, ptr addrspace(1) } %79, 1
  %82 = inttoptr i64 %71 to ptr addrspace(1)
  %83 = tail call oxcaml_c_directcc { i64, i64, ptr addrspace(1) } @"\01_caml_blit_string"(i64 %80, i64 %81, ptr addrspace(1) %35, ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1)), ptr addrspace(1) %77, ptr addrspace(1) nonnull %78, ptr addrspace(1) nonnull %82) #8
  %84 = extractvalue { i64, i64, ptr addrspace(1) } %83, 0
  %85 = extractvalue { i64, i64, ptr addrspace(1) } %83, 1
  %86 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %84, i64 %85, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) nonnull inttoptr (i64 129 to ptr addrspace(1)), ptr addrspace(1) %77) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %87 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %86, 0, 0
  %88 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %86, 0, 1
  %89 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %86, 1, 0
  %90 = getelementptr i8, ptr addrspace(1) %89, i64 -8
  %91 = getelementptr i8, ptr addrspace(1) %89, i64 -4
  %92 = getelementptr i8, ptr addrspace(1) %89, i64 -4
  br label %L301

L301:                                             ; preds = %L324, %L390
  %.0 = phi i64 [ 1, %L390 ], [ %107, %L324 ]
  %alloc.1 = phi i64 [ %88, %L390 ], [ %alloc.2, %L324 ]
  %ds.1 = phi i64 [ %87, %L390 ], [ %ds.2, %L324 ]
  %93 = shl nuw i64 %.0, 1
  %94 = or i64 %93, 1
  %95 = call oxcaml_nofpcc { { i64, i64 }, { ptr addrspace(1) } } @"\01_camlString_tree_first_byte_diff__key_4_12_code"(i64 %ds.1, i64 %alloc.1, i64 %94) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 2, i64 87, i64 0, i64 22, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 33, i64 0, i64 13, i64 30, i64 0, i64 30, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %96 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %95, 0, 0
  %97 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %95, 0, 1
  %98 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %95, 1, 0
  %99 = load i8, ptr addrspace(1) %90, align 1
  %or.cond.not = icmp eq i8 %99, -2
  %100 = shl i64 %94, 2
  br i1 %or.cond.not, label %L310, label %L317

L310:                                             ; preds = %L301
  %101 = getelementptr i8, ptr addrspace(1) %92, i64 %100
  %102 = load double, ptr addrspace(1) %98, align 8
  store double %102, ptr addrspace(1) %101, align 8
  br label %L324

L317:                                             ; preds = %L301
  %103 = getelementptr i8, ptr addrspace(1) %91, i64 %100
  %104 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %96, i64 %97, ptr addrspace(1) %103, ptr addrspace(1) %98) #8
  %105 = extractvalue { i64, i64 } %104, 0
  %106 = extractvalue { i64, i64 } %104, 1
  br label %L324

L324:                                             ; preds = %L317, %L310
  %alloc.2 = phi i64 [ %106, %L317 ], [ %97, %L310 ]
  %ds.2 = phi i64 [ %105, %L317 ], [ %96, %L310 ]
  %107 = add nuw nsw i64 %.0, 1
  %exitcond = icmp eq i64 %107, 64
  br i1 %exitcond, label %L329, label %L301

L329:                                             ; preds = %L324
  %108 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %12, 1, 0
  %109 = icmp slt i64 %3, 3
  br i1 %109, label %common.ret, label %L336

L336:                                             ; preds = %L329
  %110 = lshr i64 %3, 1
  %111 = icmp slt i64 %2, 3
  %112 = lshr i64 %2, 1
  br i1 %111, label %L342.us, label %L342.preheader

L342.preheader:                                   ; preds = %L336
  %113 = getelementptr i8, ptr addrspace(1) %89, i64 -4
  br label %L342

L342.us:                                          ; preds = %L336, %L342.us
  %.0247.us = phi i64 [ %114, %L342.us ], [ 1, %L336 ]
  %114 = add i64 %.0247.us, 1
  %115 = icmp sgt i64 %114, %110
  br i1 %115, label %common.ret, label %L342.us

L342:                                             ; preds = %L342.preheader, %L375.loopexit
  %.0248 = phi i64 [ %129, %L375.loopexit ], [ 1, %L342.preheader ]
  %.0247 = phi i64 [ %134, %L375.loopexit ], [ 1, %L342.preheader ]
  %alloc.3 = phi i64 [ %133, %L375.loopexit ], [ %alloc.2, %L342.preheader ]
  %ds.3 = phi i64 [ %132, %L375.loopexit ], [ %ds.2, %L342.preheader ]
  %116 = shl i64 %.0247, 1
  %117 = or i64 %116, 1
  br label %L354

L354:                                             ; preds = %L354, %L342
  %.0251 = phi i64 [ %.0248, %L342 ], [ %129, %L354 ]
  %.0250 = phi i64 [ 1, %L342 ], [ %130, %L354 ]
  %alloc.4 = phi i64 [ %alloc.3, %L342 ], [ %126, %L354 ]
  %ds.4 = phi i64 [ %ds.3, %L342 ], [ %125, %L354 ]
  %118 = shl i64 %.0250, 1
  %119 = add i64 %117, %118
  %120 = shl i64 %119, 2
  %121 = and i64 %120, 508
  %122 = getelementptr i8, ptr addrspace(1) %113, i64 %121
  %123 = load ptr addrspace(1), ptr addrspace(1) %122, align 8
  %124 = call oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlString_tree_first_byte_diff__find_6_14_code"(i64 %ds.4, i64 %alloc.4, ptr addrspace(1) %123, ptr addrspace(1) %108) #7 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 37, i64 0, i64 20, i64 71, i64 0, i64 71, i64 30, i64 7500915, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7105838, i64 31, i64 7500883, i64 6778473, i64 7500895, i64 6251877, i64 7498086, i64 6255731, i64 7633250, i64 6578021, i64 6710889, i64 7696942, i64 110) ]
  %125 = extractvalue { { i64, i64 }, { i64 } } %124, 0, 0
  %126 = extractvalue { { i64, i64 }, { i64 } } %124, 0, 1
  %127 = extractvalue { { i64, i64 }, { i64 } } %124, 1, 0
  %128 = add i64 %.0251, -1
  %129 = add i64 %128, %127
  %130 = add i64 %.0250, 1
  %131 = icmp sgt i64 %130, %112
  br i1 %131, label %L375.loopexit, label %L354

L375.loopexit:                                    ; preds = %L354
  %132 = extractvalue { { i64, i64 }, { i64 } } %124, 0, 0
  %133 = extractvalue { { i64, i64 }, { i64 } } %124, 0, 1
  %134 = add i64 %.0247, 1
  %135 = icmp sgt i64 %134, %110
  br i1 %135, label %common.ret.loopexit268, label %L342

common.ret.loopexit268:                           ; preds = %L375.loopexit
  %136 = extractvalue { { i64, i64 }, { i64 } } %124, 0, 0
  %137 = extractvalue { { i64, i64 }, { i64 } } %124, 0, 1
  br label %common.ret

common.ret:                                       ; preds = %L342.us, %common.ret.loopexit268, %L329
  %ds.5.pn = phi i64 [ %ds.2, %L329 ], [ %136, %common.ret.loopexit268 ], [ %ds.2, %L342.us ]
  %alloc.5.pn = phi i64 [ %alloc.2, %L329 ], [ %137, %common.ret.loopexit268 ], [ %alloc.2, %L342.us ]
  %.0249.pn = phi i64 [ 1, %L329 ], [ %129, %common.ret.loopexit268 ], [ 1, %L342.us ]
  %.pn258 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.5.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn258, i64 %alloc.5.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.0249.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
