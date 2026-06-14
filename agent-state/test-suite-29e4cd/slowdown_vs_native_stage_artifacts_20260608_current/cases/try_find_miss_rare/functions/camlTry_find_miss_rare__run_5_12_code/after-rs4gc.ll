define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_find_miss_rare__run_5_12_code"(i64 %0, i64 %1, i64 %2, i64 %3) #2 gc "oxcaml" personality ptr @"\01_caml_llvm_eh_personality" {
L1:
  %gcagg167.exnroot = alloca ptr addrspace(1), align 8
  store volatile ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), ptr %gcagg167.exnroot, align 8
  %4 = add i64 %0, 64
  %5 = inttoptr i64 %4 to ptr
  %6 = load i64, ptr %5, align 4
  %statepoint_token = call oxcaml_ccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { ptr addrspace(1) } } (i64, i64, i64, ptr addrspace(1), ptr addrspace(1))) @"\01_caml_c_call", i32 5, i32 0, i64 %0, i64 %1, i64 ptrtoint (ptr @"\01_caml_array_make" to i64), ptr addrspace(1) inttoptr (i64 33 to ptr addrspace(1)), ptr addrspace(1) inttoptr (i64 1 to ptr addrspace(1)), i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 1, i64 2, i64 85, i64 0, i64 13, i64 27, i64 0, i64 27, i64 8, i64 7500385, i64 3045729, i64 27757, i64 18, i64 6583379, i64 6449516, i64 4284255, i64 6386290, i64 6893177, i64 7629166, i64 22, i64 0, i64 10, i64 40, i64 0, i64 40, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110), "gc-live"(ptr %gcagg167.exnroot) ]
  %7 = call { { i64, i64 }, { ptr addrspace(1) } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_p1ss(token %statepoint_token)
  %gcagg = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 0, 0
  %gcagg166 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 0, 1
  %gcagg167 = extractvalue { { i64, i64 }, { ptr addrspace(1) } } %7, 1, 0
  %8 = add i64 %gcagg, 40
  %9 = inttoptr i64 %8 to ptr
  %10 = load i64, ptr %9, align 4
  %11 = add i64 %10, 408
  %12 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %12, %11
  br i1 %.not, label %L305, label %L306, !prof !1

L305:                                             ; preds = %L1
  %statepoint_token201 = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %gcagg, i64 %gcagg166, i64 38, i32 0, i32 0) #7 [ "gc-live"(ptr addrspace(1) %gcagg167, ptr %gcagg167.exnroot) ]
  %13 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token201)
  %gcagg167.relocated = call coldcc ptr addrspace(1) @llvm.experimental.gc.relocate.p1(token %statepoint_token201, i32 0, i32 0) ; (%gcagg167, %gcagg167)
  %14 = extractvalue { { i64, i64 }, {} } %13, 0, 0
  %15 = extractvalue { { i64, i64 }, {} } %13, 0, 1
  br label %L306

L306:                                             ; preds = %L305, %L1
  %.0 = phi ptr addrspace(1) [ %gcagg167.relocated, %L305 ], [ %gcagg167, %L1 ]
  %alloc.0 = phi i64 [ %gcagg166, %L1 ], [ %15, %L305 ]
  %ds.0 = phi i64 [ %gcagg, %L1 ], [ %14, %L305 ]
  %remat185 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %16 = load i8, ptr addrspace(1) %remat185, align 1
  %or.cond.not = icmp eq i8 %16, -2
  br i1 %or.cond.not, label %L187, label %L189

L187:                                             ; preds = %L189.13, %L189.12, %L189.11, %L189.10, %L189.9, %L189.8, %L189.7, %L189.6, %L189.5, %L189.4, %L189.3, %L189.2, %L189.1, %L189, %L306
  %alloc.1.lcssa = phi i64 [ %alloc.0, %L306 ], [ %20, %L189 ], [ %24, %L189.1 ], [ %28, %L189.2 ], [ %32, %L189.3 ], [ %36, %L189.4 ], [ %40, %L189.5 ], [ %44, %L189.6 ], [ %48, %L189.7 ], [ %52, %L189.8 ], [ %56, %L189.9 ], [ %60, %L189.10 ], [ %64, %L189.11 ], [ %68, %L189.12 ], [ %72, %L189.13 ]
  %ds.1.lcssa = phi i64 [ %ds.0, %L306 ], [ %19, %L189 ], [ %23, %L189.1 ], [ %27, %L189.2 ], [ %31, %L189.3 ], [ %35, %L189.4 ], [ %39, %L189.5 ], [ %43, %L189.6 ], [ %47, %L189.7 ], [ %51, %L189.8 ], [ %55, %L189.9 ], [ %59, %L189.10 ], [ %63, %L189.11 ], [ %67, %L189.12 ], [ %71, %L189.13 ]
  %17 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_flambda2_invalid"(i64 %ds.1.lcssa, i64 %alloc.1.lcssa, ptr addrspace(1) nonnull inttoptr (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__invalid605" to i64) to ptr addrspace(1))) #9
  unreachable

L189:                                             ; preds = %L306
  %remat186 = getelementptr i8, ptr addrspace(1) %.0, i64 8
  %18 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %ds.0, i64 %alloc.0, ptr addrspace(1) %remat186, ptr addrspace(1) nonnull inttoptr (i64 5 to ptr addrspace(1))) #9
  %19 = extractvalue { i64, i64 } %18, 0
  %20 = extractvalue { i64, i64 } %18, 1
  %remat184 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %21 = load i8, ptr addrspace(1) %remat184, align 1
  %or.cond.not.1 = icmp eq i8 %21, -2
  br i1 %or.cond.not.1, label %L187, label %L189.1

L189.1:                                           ; preds = %L189
  %remat187 = getelementptr i8, ptr addrspace(1) %.0, i64 16
  %22 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %19, i64 %20, ptr addrspace(1) %remat187, ptr addrspace(1) nonnull inttoptr (i64 9 to ptr addrspace(1))) #9
  %23 = extractvalue { i64, i64 } %22, 0
  %24 = extractvalue { i64, i64 } %22, 1
  %remat183 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %25 = load i8, ptr addrspace(1) %remat183, align 1
  %or.cond.not.2 = icmp eq i8 %25, -2
  br i1 %or.cond.not.2, label %L187, label %L189.2

L189.2:                                           ; preds = %L189.1
  %remat188 = getelementptr i8, ptr addrspace(1) %.0, i64 24
  %26 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %23, i64 %24, ptr addrspace(1) %remat188, ptr addrspace(1) nonnull inttoptr (i64 13 to ptr addrspace(1))) #9
  %27 = extractvalue { i64, i64 } %26, 0
  %28 = extractvalue { i64, i64 } %26, 1
  %remat182 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %29 = load i8, ptr addrspace(1) %remat182, align 1
  %or.cond.not.3 = icmp eq i8 %29, -2
  br i1 %or.cond.not.3, label %L187, label %L189.3

L189.3:                                           ; preds = %L189.2
  %remat189 = getelementptr i8, ptr addrspace(1) %.0, i64 32
  %30 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %27, i64 %28, ptr addrspace(1) %remat189, ptr addrspace(1) nonnull inttoptr (i64 17 to ptr addrspace(1))) #9
  %31 = extractvalue { i64, i64 } %30, 0
  %32 = extractvalue { i64, i64 } %30, 1
  %remat181 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %33 = load i8, ptr addrspace(1) %remat181, align 1
  %or.cond.not.4 = icmp eq i8 %33, -2
  br i1 %or.cond.not.4, label %L187, label %L189.4

L189.4:                                           ; preds = %L189.3
  %remat190 = getelementptr i8, ptr addrspace(1) %.0, i64 40
  %34 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %31, i64 %32, ptr addrspace(1) %remat190, ptr addrspace(1) nonnull inttoptr (i64 21 to ptr addrspace(1))) #9
  %35 = extractvalue { i64, i64 } %34, 0
  %36 = extractvalue { i64, i64 } %34, 1
  %remat180 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %37 = load i8, ptr addrspace(1) %remat180, align 1
  %or.cond.not.5 = icmp eq i8 %37, -2
  br i1 %or.cond.not.5, label %L187, label %L189.5

L189.5:                                           ; preds = %L189.4
  %remat191 = getelementptr i8, ptr addrspace(1) %.0, i64 48
  %38 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %35, i64 %36, ptr addrspace(1) %remat191, ptr addrspace(1) nonnull inttoptr (i64 25 to ptr addrspace(1))) #9
  %39 = extractvalue { i64, i64 } %38, 0
  %40 = extractvalue { i64, i64 } %38, 1
  %remat179 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %41 = load i8, ptr addrspace(1) %remat179, align 1
  %or.cond.not.6 = icmp eq i8 %41, -2
  br i1 %or.cond.not.6, label %L187, label %L189.6

L189.6:                                           ; preds = %L189.5
  %remat192 = getelementptr i8, ptr addrspace(1) %.0, i64 56
  %42 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %39, i64 %40, ptr addrspace(1) %remat192, ptr addrspace(1) nonnull inttoptr (i64 29 to ptr addrspace(1))) #9
  %43 = extractvalue { i64, i64 } %42, 0
  %44 = extractvalue { i64, i64 } %42, 1
  %remat178 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %45 = load i8, ptr addrspace(1) %remat178, align 1
  %or.cond.not.7 = icmp eq i8 %45, -2
  br i1 %or.cond.not.7, label %L187, label %L189.7

L189.7:                                           ; preds = %L189.6
  %remat193 = getelementptr i8, ptr addrspace(1) %.0, i64 64
  %46 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %43, i64 %44, ptr addrspace(1) %remat193, ptr addrspace(1) nonnull inttoptr (i64 33 to ptr addrspace(1))) #9
  %47 = extractvalue { i64, i64 } %46, 0
  %48 = extractvalue { i64, i64 } %46, 1
  %remat177 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %49 = load i8, ptr addrspace(1) %remat177, align 1
  %or.cond.not.8 = icmp eq i8 %49, -2
  br i1 %or.cond.not.8, label %L187, label %L189.8

L189.8:                                           ; preds = %L189.7
  %remat194 = getelementptr i8, ptr addrspace(1) %.0, i64 72
  %50 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %47, i64 %48, ptr addrspace(1) %remat194, ptr addrspace(1) nonnull inttoptr (i64 37 to ptr addrspace(1))) #9
  %51 = extractvalue { i64, i64 } %50, 0
  %52 = extractvalue { i64, i64 } %50, 1
  %remat176 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %53 = load i8, ptr addrspace(1) %remat176, align 1
  %or.cond.not.9 = icmp eq i8 %53, -2
  br i1 %or.cond.not.9, label %L187, label %L189.9

L189.9:                                           ; preds = %L189.8
  %remat195 = getelementptr i8, ptr addrspace(1) %.0, i64 80
  %54 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %51, i64 %52, ptr addrspace(1) %remat195, ptr addrspace(1) nonnull inttoptr (i64 41 to ptr addrspace(1))) #9
  %55 = extractvalue { i64, i64 } %54, 0
  %56 = extractvalue { i64, i64 } %54, 1
  %remat175 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %57 = load i8, ptr addrspace(1) %remat175, align 1
  %or.cond.not.10 = icmp eq i8 %57, -2
  br i1 %or.cond.not.10, label %L187, label %L189.10

L189.10:                                          ; preds = %L189.9
  %remat196 = getelementptr i8, ptr addrspace(1) %.0, i64 88
  %58 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %55, i64 %56, ptr addrspace(1) %remat196, ptr addrspace(1) nonnull inttoptr (i64 45 to ptr addrspace(1))) #9
  %59 = extractvalue { i64, i64 } %58, 0
  %60 = extractvalue { i64, i64 } %58, 1
  %remat174 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %61 = load i8, ptr addrspace(1) %remat174, align 1
  %or.cond.not.11 = icmp eq i8 %61, -2
  br i1 %or.cond.not.11, label %L187, label %L189.11

L189.11:                                          ; preds = %L189.10
  %remat197 = getelementptr i8, ptr addrspace(1) %.0, i64 96
  %62 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %59, i64 %60, ptr addrspace(1) %remat197, ptr addrspace(1) nonnull inttoptr (i64 49 to ptr addrspace(1))) #9
  %63 = extractvalue { i64, i64 } %62, 0
  %64 = extractvalue { i64, i64 } %62, 1
  %remat173 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %65 = load i8, ptr addrspace(1) %remat173, align 1
  %or.cond.not.12 = icmp eq i8 %65, -2
  br i1 %or.cond.not.12, label %L187, label %L189.12

L189.12:                                          ; preds = %L189.11
  %remat198 = getelementptr i8, ptr addrspace(1) %.0, i64 104
  %66 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %63, i64 %64, ptr addrspace(1) %remat198, ptr addrspace(1) nonnull inttoptr (i64 53 to ptr addrspace(1))) #9
  %67 = extractvalue { i64, i64 } %66, 0
  %68 = extractvalue { i64, i64 } %66, 1
  %remat172 = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %69 = load i8, ptr addrspace(1) %remat172, align 1
  %or.cond.not.13 = icmp eq i8 %69, -2
  br i1 %or.cond.not.13, label %L187, label %L189.13

L189.13:                                          ; preds = %L189.12
  %remat199 = getelementptr i8, ptr addrspace(1) %.0, i64 112
  %70 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %67, i64 %68, ptr addrspace(1) %remat199, ptr addrspace(1) nonnull inttoptr (i64 57 to ptr addrspace(1))) #9
  %71 = extractvalue { i64, i64 } %70, 0
  %72 = extractvalue { i64, i64 } %70, 1
  %remat = getelementptr i8, ptr addrspace(1) %.0, i64 -8
  %73 = load i8, ptr addrspace(1) %remat, align 1
  %or.cond.not.14 = icmp eq i8 %73, -2
  br i1 %or.cond.not.14, label %L187, label %L189.14

L189.14:                                          ; preds = %L189.13
  %remat200 = getelementptr i8, ptr addrspace(1) %.0, i64 120
  %74 = tail call oxcaml_c_directcc { i64, i64 } @"\01_caml_modify"(i64 %71, i64 %72, ptr addrspace(1) %remat200, ptr addrspace(1) nonnull inttoptr (i64 61 to ptr addrspace(1))) #9
  %75 = extractvalue { i64, i64 } %74, 0
  %76 = extractvalue { i64, i64 } %74, 1
  %77 = icmp slt i64 %3, 3
  br i1 %77, label %L300, label %L209

L209:                                             ; preds = %L189.14
  %78 = lshr i64 %3, 1
  %79 = lshr i64 %2, 1
  br label %L215

L215:                                             ; preds = %L287, %L209
  %.1 = phi ptr addrspace(1) [ %.0, %L209 ], [ %.4, %L287 ]
  %.0138 = phi i64 [ 1, %L209 ], [ %.0139, %L287 ]
  %.0137 = phi i64 [ 1, %L209 ], [ %111, %L287 ]
  %alloc.2 = phi i64 [ %76, %L209 ], [ %alloc.5, %L287 ]
  %ds.2 = phi i64 [ %75, %L209 ], [ %ds.5, %L287 ]
  %80 = shl i64 %.0137, 1
  %81 = or i64 %80, 1
  %82 = icmp slt i64 %2, 3
  br i1 %82, label %L287, label %L227

L227:                                             ; preds = %L215, %L273
  %.2 = phi ptr addrspace(1) [ %.1, %L215 ], [ %.3, %L273 ]
  %.0141 = phi i64 [ %.0143, %L273 ], [ %.0138, %L215 ]
  %.0140 = phi i64 [ %109, %L273 ], [ 1, %L215 ]
  %alloc.3 = phi i64 [ %alloc.4, %L273 ], [ %alloc.2, %L215 ]
  %ds.3 = phi i64 [ %ds.4, %L273 ], [ %ds.2, %L215 ]
  %83 = shl i64 %.0140, 1
  %84 = or i64 %83, 1
  %85 = and i64 %84, 511
  %.not151 = icmp eq i64 %85, 1
  br i1 %.not151, label %L243, label %L236

L236:                                             ; preds = %L227
  %86 = add i64 %81, %84
  %87 = shl i64 %86, 1
  %88 = add i64 %87, 62
  %89 = and i64 %88, 62
  %90 = add nsw i64 %89, -1
  br label %L243

L243:                                             ; preds = %L227, %L236
  %.0142 = phi i64 [ %90, %L236 ], [ -1, %L227 ]
  %91 = add i64 %ds.3, 64
  %92 = inttoptr i64 %91 to ptr
  %93 = load i64, ptr %92, align 4
  tail call void @llvm.aarch64.oxcaml.push.trap(ptr blockaddress(@"\01_camlTry_find_miss_rare__run_5_12_code", %L313))
  %94 = inttoptr i64 %.0142 to ptr addrspace(1)
  store volatile ptr addrspace(1) %.2, ptr %gcagg167.exnroot, align 8
  %statepoint_token203 = invoke oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 18, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, ptr addrspace(1), ptr addrspace(1), i64)) @"\01_camlTry_find_miss_rare__scan_4_11_code", i32 5, i32 0, i64 %ds.3, i64 %alloc.3, ptr addrspace(1) %94, ptr addrspace(1) %.2, i64 1, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 27, i64 0, i64 24, i64 36, i64 0, i64 36, i64 21, i64 7959156, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7105838, i64 22, i64 7959124, i64 6907487, i64 6251630, i64 7563629, i64 7495539, i64 6648417, i64 7696942, i64 110), "gc-live"(ptr %gcagg167.exnroot) ]
          to label %L314 unwind label %L313

L313:                                             ; preds = %L243
  %95 = landingpad token
          cleanup
  %96 = tail call { ptr addrspace(1), i64, i64, i64 } @llvm.aarch64.oxcaml.trap.recover()
  %gcagg168 = extractvalue { ptr addrspace(1), i64, i64, i64 } %96, 0
  %gcagg169 = extractvalue { ptr addrspace(1), i64, i64, i64 } %96, 1
  %gcagg170 = extractvalue { ptr addrspace(1), i64, i64, i64 } %96, 2
  %gcagg171 = extractvalue { ptr addrspace(1), i64, i64, i64 } %96, 3
  %97 = add i64 %gcagg171, 64
  %98 = inttoptr i64 %97 to ptr
  store i64 %93, ptr %98, align 4
  %or.cond.not154 = icmp eq ptr addrspace(1) %gcagg168, inttoptr (i64 ptrtoint (ptr @"\01_camlTry_find_miss_rare__Miss281" to i64) to ptr addrspace(1))
  br i1 %or.cond.not154, label %L264, label %L269

L314:                                             ; preds = %L243
  %99 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token203)
  %gcagg167.exnroot.normal.load = load volatile ptr addrspace(1), ptr %gcagg167.exnroot, align 8
  %100 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 0
  %101 = extractvalue { { i64, i64 }, { i64 } } %99, 0, 1
  %102 = extractvalue { { i64, i64 }, { i64 } } %99, 1, 0
  %103 = add i64 %.0141, -1
  %104 = add i64 %103, %102
  tail call void @llvm.aarch64.oxcaml.pop.trap()
  br label %L273

L264:                                             ; preds = %L313
  %105 = add i64 %.0141, 2147483647
  %106 = add i64 %105, %84
  %107 = and i64 %106, 2147483647
  %.2.exnroot.load = load volatile ptr addrspace(1), ptr %gcagg167.exnroot, align 8
  br label %L273

L269:                                             ; preds = %L313
  %108 = ptrtoint ptr addrspace(1) %gcagg168 to i64
  tail call void @llvm.aarch64.oxcaml.raise.notrace(i64 %108)
  unreachable

L273:                                             ; preds = %L264, %L314
  %.3 = phi ptr addrspace(1) [ %gcagg167.exnroot.normal.load, %L314 ], [ %.2.exnroot.load, %L264 ]
  %.0143 = phi i64 [ %104, %L314 ], [ %107, %L264 ]
  %alloc.4 = phi i64 [ %101, %L314 ], [ %gcagg170, %L264 ]
  %ds.4 = phi i64 [ %100, %L314 ], [ %gcagg171, %L264 ]
  %109 = add i64 %.0140, 1
  %110 = icmp sgt i64 %109, %79
  br i1 %110, label %L287, label %L227

L287:                                             ; preds = %L273, %L215
  %.4 = phi ptr addrspace(1) [ %.1, %L215 ], [ %.3, %L273 ]
  %.0139 = phi i64 [ %.0138, %L215 ], [ %.0143, %L273 ]
  %alloc.5 = phi i64 [ %alloc.2, %L215 ], [ %alloc.4, %L273 ]
  %ds.5 = phi i64 [ %ds.2, %L215 ], [ %ds.4, %L273 ]
  %111 = add i64 %.0137, 1
  %112 = icmp sgt i64 %111, %78
  br i1 %112, label %L300, label %L215

L300:                                             ; preds = %L287, %L189.14
  %.0136 = phi i64 [ 1, %L189.14 ], [ %.0139, %L287 ]
  %alloc.6 = phi i64 [ %76, %L189.14 ], [ %alloc.5, %L287 ]
  %ds.6 = phi i64 [ %75, %L189.14 ], [ %ds.5, %L287 ]
  %113 = add i64 %ds.6, 64
  %114 = inttoptr i64 %113 to ptr
  store i64 %6, ptr %114, align 4
  %115 = insertvalue { { i64, i64 }, { i64 } } poison, i64 %ds.6, 0, 0
  %116 = insertvalue { { i64, i64 }, { i64 } } %115, i64 %alloc.6, 0, 1
  %117 = insertvalue { { i64, i64 }, { i64 } } %116, i64 %.0136, 1, 0
  ret { { i64, i64 }, { i64 } } %117
}
