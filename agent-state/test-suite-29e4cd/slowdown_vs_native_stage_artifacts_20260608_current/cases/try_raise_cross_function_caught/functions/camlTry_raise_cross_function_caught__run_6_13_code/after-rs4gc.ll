define oxcaml_nofpcc { { i64, i64 }, { i64 } } @"\01_camlTry_raise_cross_function_caught__run_6_13_code"(i64 %0, i64 %1, i64 %2, i64 %3) #1 gc "oxcaml" {
L1:
  %4 = icmp slt i64 %3, 3
  br i1 %4, label %common.ret, label %L159

L159:                                             ; preds = %L1
  %5 = add i64 %0, 40
  %6 = inttoptr i64 %5 to ptr
  %7 = load i64, ptr %6, align 4
  %8 = add i64 %7, 376
  %9 = tail call i64 asm sideeffect "mov $0, sp", "=r"() #6
  %.not = icmp ult i64 %9, %8
  br i1 %.not, label %L205, label %L206, !prof !1

L205:                                             ; preds = %L159
  %statepoint_token = tail call oxcaml_alloccc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, {} } (i64, i64, i64)) @"\01_caml_llvm_call_realloc_stack", i32 3, i32 0, i64 %0, i64 %1, i64 34, i32 0, i32 0) #7
  %10 = call { { i64, i64 }, {} } @llvm.experimental.gc.result.sl_sl_i64i64ssl_ss(token %statepoint_token)
  %11 = extractvalue { { i64, i64 }, {} } %10, 0, 0
  %12 = extractvalue { { i64, i64 }, {} } %10, 0, 1
  br label %L206

L206:                                             ; preds = %L205, %L159
  %alloc.0 = phi i64 [ %1, %L159 ], [ %12, %L205 ]
  %ds.0 = phi i64 [ %0, %L159 ], [ %11, %L205 ]
  %13 = lshr i64 %3, 1
  %14 = icmp slt i64 %2, 3
  br i1 %14, label %L191.preheader, label %L170

L170:                                             ; preds = %L194, %L206
  %.055 = phi i64 [ 1, %L206 ], [ %.057.ph, %L194 ]
  %.0 = phi i64 [ 1, %L206 ], [ %26, %L194 ]
  %alloc.1 = phi i64 [ %alloc.0, %L206 ], [ %alloc.4.ph, %L194 ]
  %ds.1 = phi i64 [ %ds.0, %L206 ], [ %ds.4.ph, %L194 ]
  %15 = lshr i64 %2, 1
  br label %L176

L176:                                             ; preds = %L176, %L170
  %.059 = phi i64 [ %.055, %L170 ], [ %21, %L176 ]
  %.058 = phi i64 [ 1, %L170 ], [ %22, %L176 ]
  %alloc.2 = phi i64 [ %alloc.1, %L170 ], [ %18, %L176 ]
  %ds.2 = phi i64 [ %ds.1, %L170 ], [ %17, %L176 ]
  %statepoint_token84 = call oxcaml_nofpcc token (i64, i32, ptr, i32, i32, ...) @llvm.experimental.gc.statepoint.p0(i64 0, i32 0, ptr elementtype({ { i64, i64 }, { i64 } } (i64, i64, i64)) @"\01_camlTry_raise_cross_function_caught__find_5_12_code", i32 3, i32 0, i64 %ds.2, i64 %alloc.2, i64 1, i32 0, i32 0) [ "deopt"(i64 1870160740, i64 1, i64 0, i64 1, i64 26, i64 0, i64 20, i64 26, i64 0, i64 26, i64 34, i64 7959156, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7155316, i64 108, i64 35, i64 7959124, i64 6386271, i64 6648681, i64 7496543, i64 7566191, i64 7693919, i64 7627630, i64 7237481, i64 6382431, i64 6842229, i64 7482996, i64 28277) ]
  %16 = call { { i64, i64 }, { i64 } } @llvm.experimental.gc.result.sl_sl_i64i64ssl_i64ss(token %statepoint_token84)
  %17 = extractvalue { { i64, i64 }, { i64 } } %16, 0, 0
  %18 = extractvalue { { i64, i64 }, { i64 } } %16, 0, 1
  %19 = extractvalue { { i64, i64 }, { i64 } } %16, 1, 0
  %20 = add i64 %.059, -1
  %21 = add i64 %20, %19
  %22 = add i64 %.058, 1
  %23 = icmp sgt i64 %22, %15
  br i1 %23, label %L191.loopexit, label %L176

L191.loopexit:                                    ; preds = %L176
  %24 = extractvalue { { i64, i64 }, { i64 } } %16, 0, 0
  %25 = extractvalue { { i64, i64 }, { i64 } } %16, 0, 1
  br label %L191.preheader

L191.preheader:                                   ; preds = %L191.loopexit, %L206
  %.057.ph = phi i64 [ 1, %L206 ], [ %21, %L191.loopexit ]
  %.2.ph = phi i64 [ 1, %L206 ], [ %.0, %L191.loopexit ]
  %alloc.4.ph = phi i64 [ %alloc.0, %L206 ], [ %25, %L191.loopexit ]
  %ds.4.ph = phi i64 [ %ds.0, %L206 ], [ %24, %L191.loopexit ]
  br label %L191

L191:                                             ; preds = %L191.preheader, %L194
  %.2 = phi i64 [ %26, %L194 ], [ %.2.ph, %L191.preheader ]
  %26 = add i64 %.2, 1
  %27 = icmp sgt i64 %26, %13
  br i1 %27, label %common.ret, label %L194

L194:                                             ; preds = %L191
  br i1 %14, label %L191, label %L170

common.ret:                                       ; preds = %L191, %L1
  %ds.4.pn = phi i64 [ %0, %L1 ], [ %ds.4.ph, %L191 ]
  %alloc.4.pn = phi i64 [ %1, %L1 ], [ %alloc.4.ph, %L191 ]
  %.057.pn = phi i64 [ 1, %L1 ], [ %.057.ph, %L191 ]
  %.pn65 = insertvalue { { i64, i64 }, { i64 } } undef, i64 %ds.4.pn, 0, 0
  %.pn = insertvalue { { i64, i64 }, { i64 } } %.pn65, i64 %alloc.4.pn, 0, 1
  %common.ret.op = insertvalue { { i64, i64 }, { i64 } } %.pn, i64 %.057.pn, 1, 0
  ret { { i64, i64 }, { i64 } } %common.ret.op
}
