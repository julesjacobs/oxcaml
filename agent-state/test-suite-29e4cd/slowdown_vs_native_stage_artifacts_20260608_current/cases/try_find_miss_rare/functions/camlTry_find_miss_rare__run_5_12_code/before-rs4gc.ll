define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__run_5_12_code"(i64 %0, i64 %1, i64 %2, i64 %3) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %4 = add i64 %0, 64
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %7 = call oxcaml_ccc { { i64, i64 }, { ptr addrspace(1) } } @"\01_caml_c_call"(i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) nonnull inttoptr (i64 33 to ptr addrspace(1)), ptr addrspace(1) nonnull inttoptr (i64 1 to ptr addrspace(1))) #7 [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 22, i64 0, i64 10, i64 40, i64 0, i64 40, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110) ]
  %8 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 0, 0
  %9 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 0, 1
  %10 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 1, 0
  %11 = add i64 %8, 40
  %12 = inttoptr i64 %11 to ptr
  %13 = load i64, ptr %12, align 4
  %14 = add i64 %13, 408
  %15 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #5
  %.not = icmp ult i64 %15, %14
  br i1 %.not, label %L305, label %L306, !prof !1

L305:                                             ; preds = %L1
  %16 = tail call oxcaml_alloccc { { i64, i64 }, {} } @"\01_caml_llvm_call_realloc_stack"(i64 %8, i64 %9, i64 38) #6
  %17 = extractvalue { { i64, i64 }, {} } %16, 0, 0
  %18 = extractvalue { { i64, i64 }, {} } %16, 0, 1
  br label %L306

L306:                                             ; preds = %L305, %L1
  %alloc.0 = phi i64 [ %9, %L1 ], [ %18, %L305 ]
  %ds.0 = phi i64 [ %8, %L1 ], [ %17, %L305 ]
  %19 = getelementptr i8, ptr addrspace(1) %10, i64 -8
  %20 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not = icmp eq i8 %20, -2
  br i1 %or.cond.not, label %L187, label %L189

L187:                                             ; preds = %L189.13, %L189.12, %L189.11, %L189.10, %L189.9, %L189.8, %L189.7, %L189.6, %L189.5, %L189.4, %L189.3, %L189.2, %L189.1, %L189, %L306
  %alloc.1.lcssa = phi i64 [ %alloc.0, %L306 ], [ %25, %L189 ], [ %30, %L189.1 ], [ %35, %L189.2 ], [ %40, %L189.3 ], [ %45, %L189.4 ], [ %50, %L189.5 ], [ %55, %L189.6 ], [ %60, %L189.7 ], [ %65, %L189.8 ], [ %70, %L189.9 ], [ %75, %L189.10 ], [ %80, %L189.11 ], [ %85, %L189.12 ], [ %90, %L189.13 ]
  %ds.1.lcssa = phi i64 [ %ds.0, %L306 ], [ %24, %L189 ], [ %29, %L189.1 ], [ %34, %L189.2 ], [ %39, %L189.3 ], [ %44, %L189.4 ], [ %49, %L189.5 ], [ %54, %L189.6 ], [ %59, %L189.7 ], [ %64, %L189.8 ], [ %69, %L189.9 ], [ %74, %L189.10 ], [ %79, %L189.11 ], [ %84, %L189.12 ], [ %89, %L189.13 ]
  %21 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %ds.1.lcssa, i64 %alloc.1.lcssa, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__invalid605" to i64) to ptr addrspace(1))) #9
  unreachable

L189:                                             ; preds = %L306
  %22 = getelementptr i8, ptr addrspace(1) %10, i64 8
  %23 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.0, i64 %alloc.0, ptr addrspace(1) %22, ptr addrspace(1) nonnull inttoptr (i64 5 to ptr addrspace(1))) #9
  %24 = extractvalue { i64, i64 } %23, 0
  %25 = extractvalue { i64, i64 } %23, 1
  %26 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.1 = icmp eq i8 %26, -2
  br i1 %or.cond.not.1, label %L187, label %L189.1

L189.1:                                           ; preds = %L189
  %27 = getelementptr i8, ptr addrspace(1) %10, i64 16
  %28 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %24, i64 %25, ptr addrspace(1) %27, ptr addrspace(1) nonnull inttoptr (i64 9 to ptr addrspace(1))) #9
  %29 = extractvalue { i64, i64 } %28, 0
  %30 = extractvalue { i64, i64 } %28, 1
  %31 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.2 = icmp eq i8 %31, -2
  br i1 %or.cond.not.2, label %L187, label %L189.2

L189.2:                                           ; preds = %L189.1
  %32 = getelementptr i8, ptr addrspace(1) %10, i64 24
  %33 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %29, i64 %30, ptr addrspace(1) %32, ptr addrspace(1) nonnull inttoptr (i64 13 to ptr addrspace(1))) #9
  %34 = extractvalue { i64, i64 } %33, 0
  %35 = extractvalue { i64, i64 } %33, 1
  %36 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.3 = icmp eq i8 %36, -2
  br i1 %or.cond.not.3, label %L187, label %L189.3

L189.3:                                           ; preds = %L189.2
  %37 = getelementptr i8, ptr addrspace(1) %10, i64 32
  %38 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %34, i64 %35, ptr addrspace(1) %37, ptr addrspace(1) nonnull inttoptr (i64 17 to ptr addrspace(1))) #9
  %39 = extractvalue { i64, i64 } %38, 0
  %40 = extractvalue { i64, i64 } %38, 1
  %41 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.4 = icmp eq i8 %41, -2
  br i1 %or.cond.not.4, label %L187, label %L189.4

L189.4:                                           ; preds = %L189.3
  %42 = getelementptr i8, ptr addrspace(1) %10, i64 40
  %43 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %39, i64 %40, ptr addrspace(1) %42, ptr addrspace(1) nonnull inttoptr (i64 21 to ptr addrspace(1))) #9
  %44 = extractvalue { i64, i64 } %43, 0
  %45 = extractvalue { i64, i64 } %43, 1
  %46 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.5 = icmp eq i8 %46, -2
  br i1 %or.cond.not.5, label %L187, label %L189.5

L189.5:                                           ; preds = %L189.4
  %47 = getelementptr i8, ptr addrspace(1) %10, i64 48
  %48 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %44, i64 %45, ptr addrspace(1) %47, ptr addrspace(1) nonnull inttoptr (i64 25 to ptr addrspace(1))) #9
  %49 = extractvalue { i64, i64 } %48, 0
  %50 = extractvalue { i64, i64 } %48, 1
  %51 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.6 = icmp eq i8 %51, -2
  br i1 %or.cond.not.6, label %L187, label %L189.6

L189.6:                                           ; preds = %L189.5
  %52 = getelementptr i8, ptr addrspace(1) %10, i64 56
  %53 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %49, i64 %50, ptr addrspace(1) %52, ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1))) #9
  %54 = extractvalue { i64, i64 } %53, 0
  %55 = extractvalue { i64, i64 } %53, 1
  %56 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.7 = icmp eq i8 %56, -2
  br i1 %or.cond.not.7, label %L187, label %L189.7

L189.7:                                           ; preds = %L189.6
  %57 = getelementptr i8, ptr addrspace(1) %10, i64 64
  %58 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %54, i64 %55, ptr addrspace(1) %57, ptr addrspace(1) nonnull inttoptr (i64 33 to ptr addrspace(1))) #9
  %59 = extractvalue { i64, i64 } %58, 0
  %60 = extractvalue { i64, i64 } %58, 1
  %61 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.8 = icmp eq i8 %61, -2
  br i1 %or.cond.not.8, label %L187, label %L189.8

L189.8:                                           ; preds = %L189.7
  %62 = getelementptr i8, ptr addrspace(1) %10, i64 72
  %63 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %59, i64 %60, ptr addrspace(1) %62, ptr addrspace(1) nonnull inttoptr (i64 37 to ptr addrspace(1))) #9
  %64 = extractvalue { i64, i64 } %63, 0
  %65 = extractvalue { i64, i64 } %63, 1
  %66 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.9 = icmp eq i8 %66, -2
  br i1 %or.cond.not.9, label %L187, label %L189.9

L189.9:                                           ; preds = %L189.8
  %67 = getelementptr i8, ptr addrspace(1) %10, i64 80
  %68 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %64, i64 %65, ptr addrspace(1) %67, ptr addrspace(1) nonnull inttoptr (i64 41 to ptr addrspace(1))) #9
  %69 = extractvalue { i64, i64 } %68, 0
  %70 = extractvalue { i64, i64 } %68, 1
  %71 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.10 = icmp eq i8 %71, -2
  br i1 %or.cond.not.10, label %L187, label %L189.10

L189.10:                                          ; preds = %L189.9
  %72 = getelementptr i8, ptr addrspace(1) %10, i64 88
  %73 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %69, i64 %70, ptr addrspace(1) %72, ptr addrspace(1) nonnull inttoptr (i64 45 to ptr addrspace(1))) #9
  %74 = extractvalue { i64, i64 } %73, 0
  %75 = extractvalue { i64, i64 } %73, 1
  %76 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.11 = icmp eq i8 %76, -2
  br i1 %or.cond.not.11, label %L187, label %L189.11

L189.11:                                          ; preds = %L189.10
  %77 = getelementptr i8, ptr addrspace(1) %10, i64 96
  %78 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %74, i64 %75, ptr addrspace(1) %77, ptr addrspace(1) nonnull inttoptr (i64 49 to ptr addrspace(1))) #9
  %79 = extractvalue { i64, i64 } %78, 0
  %80 = extractvalue { i64, i64 } %78, 1
  %81 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.12 = icmp eq i8 %81, -2
  br i1 %or.cond.not.12, label %L187, label %L189.12

L189.12:                                          ; preds = %L189.11
  %82 = getelementptr i8, ptr addrspace(1) %10, i64 104
  %83 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %79, i64 %80, ptr addrspace(1) %82, ptr addrspace(1) nonnull inttoptr (i64 53 to ptr addrspace(1))) #9
  %84 = extractvalue { i64, i64 } %83, 0
  %85 = extractvalue { i64, i64 } %83, 1
  %86 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.13 = icmp eq i8 %86, -2
  br i1 %or.cond.not.13, label %L187, label %L189.13

L189.13:                                          ; preds = %L189.12
  %87 = getelementptr i8, ptr addrspace(1) %10, i64 112
  %88 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %84, i64 %85, ptr addrspace(1) %87, ptr addrspace(1) nonnull inttoptr (i64 57 to ptr addrspace(1))) #9
  %89 = extractvalue { i64, i64 } %88, 0
  %90 = extractvalue { i64, i64 } %88, 1
  %91 = load i8, ptr addrspace(1) %19, align 1
  %or.cond.not.14 = icmp eq i8 %91, -2
  br i1 %or.cond.not.14, label %L187, label %L189.14

L189.14:                                          ; preds = %L189.13
  %92 = getelementptr i8, ptr addrspace(1) %10, i64 120
  %93 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %89, i64 %90, ptr addrspace(1) %92, ptr addrspace(1) nonnull inttoptr (i64 61 to ptr addrspace(1))) #9
  %94 = extractvalue { i64, i64 } %93, 0
  %95 = extractvalue { i64, i64 } %93, 1
  %96 = icmp slt i64 %3, 3
  br i1 %96, label %L300, label %L209

L209:                                             ; preds = %L189.14
  %97 = lshr i64 %3, 1
  %98 = icmp slt i64 %2, 3
  %99 = lshr i64 %2, 1
  br label %L215

L215:                                             ; preds = %L287, %L209
  %.0138 = phi i64 [ 1, %L209 ], [ %.0139, %L287 ]
  %.0137 = phi i64 [ 1, %L209 ], [ %134, %L287 ]
  %alloc.2 = phi i64 [ %95, %L209 ], [ %alloc.5, %L287 ]
  %ds.2 = phi i64 [ %94, %L209 ], [ %ds.5, %L287 ]
  %100 = shl i64 %.0137, 1
  %101 = or i64 %100, 1
  br i1 %98, label %L287, label %L227

L227:                                             ; preds = %L215, %L273
  %.0141 = phi i64 [ %.0143, %L273 ], [ %.0138, %L215 ]
  %.0140 = phi i64 [ %132, %L273 ], [ 1, %L215 ]
  %alloc.3 = phi i64 [ %alloc.4, %L273 ], [ %alloc.2, %L215 ]
  %ds.3 = phi i64 [ %ds.4, %L273 ], [ %ds.2, %L215 ]
  %102 = shl i64 %.0140, 1
  %103 = or i64 %102, 1
  %104 = and i64 %103, 511
  %.not151 = icmp eq i64 %104, 1
  br i1 %.not151, label %L243, label %L236

L236:                                             ; preds = %L227
  %105 = add i64 %101, %103
  %106 = shl i64 %105, 1
  %107 = add i64 %106, 62
  %108 = and i64 %107, 62
  %109 = add nsw i64 %108, -1
  br label %L243

L243:                                             ; preds = %L227, %L236
  %.0142 = phi i64 [ %109, %L236 ], [ -1, %L227 ]
  %110 = add i64 %ds.3, 64
  %111 = inttoptr i64 %110 to ptr
  %112 = load i64, ptr %111, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTry_find_miss_rare__run_5_12_code", %L313))
  %113 = inttoptr i64 %.0142 to ptr addrspace(1)
  %114 = invoke oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__scan_4_11_code"(i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %113, ptr addrspace(1) %10, i64 1) #10 [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 24, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110) ]
          to label %L314 unwind label %L313

L313:                                             ; preds = %L243
  %115 = landingpad token
          cleanup
  %116 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %117 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 0
  %118 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 3
  %119 = add i64 %118, 64
  %120 = inttoptr i64 %119 to ptr
  store i64 %112, ptr %120, align 4
  %or.cond.not154 = icmp eq ptr addrspace(1) %117, inttoptr (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64) to ptr addrspace(1))
  br i1 %or.cond.not154, label %L264, label %L269

L314:                                             ; preds = %L243
  %121 = extractvalue { { i64, i64 }, { i64 } } %114, 0, 0
  %122 = extractvalue { { i64, i64 }, { i64 } } %114, 0, 1
  %123 = extractvalue { { i64, i64 }, { i64 } } %114, 1, 0
  %124 = add i64 %.0141, -1
  %125 = add i64 %124, %123
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %L273

L264:                                             ; preds = %L313
  %126 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 2
  %127 = add i64 %.0141, 2147483647
  %128 = add i64 %127, %103
  %129 = and i64 %128, 2147483647
  br label %L273

L269:                                             ; preds = %L313
  %130 = extractvalue { ptr addrspace(1), i64, i64, i64 } %116, 0
  %131 = ptrtoint ptr addrspace(1) %130 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %131)
  unreachable

L273:                                             ; preds = %L264, %L314
  %.0143 = phi i64 [ %125, %L314 ], [ %129, %L264 ]
  %alloc.4 = phi i64 [ %122, %L314 ], [ %126, %L264 ]
  %ds.4 = phi i64 [ %121, %L314 ], [ %118, %L264 ]
  %132 = add i64 %.0140, 1
  %133 = icmp sgt i64 %132, %99
  br i1 %133, label %L287, label %L227

L287:                                             ; preds = %L273, %L215
  %.0139 = phi i64 [ %.0138, %L215 ], [ %.0143, %L273 ]
  %alloc.5 = phi i64 [ %alloc.2, %L215 ], [ %alloc.4, %L273 ]
  %ds.5 = phi i64 [ %ds.2, %L215 ], [ %ds.4, %L273 ]
  %134 = add i64 %.0137, 1
  %135 = icmp sgt i64 %134, %97
  br i1 %135, label %L300, label %L215

L300:                                             ; preds = %L287, %L189.14
  %.0136 = phi i64 [ 1, %L189.14 ], [ %.0139, %L287 ]
  %alloc.6 = phi i64 [ %95, %L189.14 ], [ %alloc.5, %L287 ]
  %ds.6 = phi i64 [ %94, %L189.14 ], [ %ds.5, %L287 ]
  %136 = add i64 %ds.6, 64
  %137 = inttoptr i64 %136 to ptr
  store i64 %6, ptr %137, align 4
  %138 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.6, 0, 0
  %139 = insertvalue { { i64, i64 }, { i64 } } %138, i64 %alloc.6, 0, 1
  %140 = insertvalue { { i64, i64 }, { i64 } } %139, i64 %.0136, 1, 0
  ret { { i64, i64 }, { i64 } } %140
}
